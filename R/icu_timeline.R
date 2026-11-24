# Validate the portable identifier subset accepted by the public SQL builders.
.ehr_validate_identifier <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)) {
    stop(label, " must be one portable SQL identifier.", call. = FALSE)
  }
  x
}

.ehr_quote_identifier <- function(con, x) {
  as.character(DBI::dbQuoteIdentifier(con, x))
}

.ehr_quote_relation <- function(con, schema, table) {
  as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = table)))
}

.ehr_relation_exists <- function(con, schema, table) {
  lookup <- paste0(
    .ehr_validate_identifier(schema, "schema"), ".",
    .ehr_validate_identifier(table, "table")
  )
  isTRUE(DBI::dbGetQuery(
    con,
    "SELECT to_regclass($1) IS NOT NULL AS ok",
    params = list(lookup)
  )$ok)
}

.ehr_icu_id_sql_type <- function(x) {
  x <- tolower(trimws(x %||% "bigint"))
  switch(x,
    integer =, int =, int4 = "integer",
    bigint =, int8 = "bigint",
    text =, varchar =, `character varying` = "text",
    stop("icu_id_type must map to integer, bigint, or text.", call. = FALSE)
  )
}

# Drop a relation by its actual type. Relation lookup is parameterized and the
# drop target is quoted, including table/view transitions.
.ehr_drop_relation <- function(con, schema, table) {
  schema <- .ehr_validate_identifier(schema, "schema")
  table <- .ehr_validate_identifier(table, "table")
  lookup <- paste0(schema, ".", table)
  relkind <- DBI::dbGetQuery(
    con,
    "SELECT relkind FROM pg_class WHERE oid = to_regclass($1)",
    params = list(lookup)
  )$relkind
  if (length(relkind) == 0 || is.na(relkind)) return(invisible())
  kind <- switch(relkind,
    r = "TABLE", p = "TABLE",
    v = "VIEW",  m = "MATERIALIZED VIEW",
    f = "FOREIGN TABLE",
    "TABLE")
  fqtn <- .ehr_quote_relation(con, schema, table)
  DBI::dbExecute(
    con, glue::glue("DROP {kind} IF EXISTS {fqtn} CASCADE;"),
    immediate = TRUE
  )
}

# Pure SQL builder for the windowed ICU-timeline body. Kept free of any database
# access so the generated query can be unit-tested without Postgres.
#
# The matching runs on a common hospital-admission clock: every ICU stay's
# interval (intime_min/outtime_min) and every event's time_since_hadm_min are
# expressed in minutes since hospital admission. MIMIC icu_windows store
# timestamps, so the bounds are EXTRACTed relative to admittime; eICU icu_windows
# are already numeric minute offsets on that clock (admittime = 0), so the bounds
# are used as-is. The admission grouping key differs by family (hadm_id vs
# patienthealthsystemstayid); everything downstream is identical.
.build_icu_timeline_body <- function(fam, src_fqtn, raw, icu_id_col, icu_id_type,
                                     keep_cols, window_before_hours,
                                     window_after_hours, include_other_icu_events,
                                     overlap, include_date_only_events,
                                     date_time_since_icu) {
  keep_s    <- paste0("s.", keep_cols, collapse = ", ")
  keep_bare <- paste(keep_cols, collapse = ", ")

  if (fam == "eicu") {
    adm_key       <- "patienthealthsystemstayid"
    stays_ids     <- "uniquepid, patienthealthsystemstayid"
    intime_expr   <- "icu_intime"
    outtime_expr  <- "icu_outtime"
    hadm_end_expr <- "dischtime"
  } else {
    adm_key       <- "hadm_id"
    stays_ids     <- "subject_id, hadm_id"
    # Rounded to whole minutes to match the atomic builders, so the reassigned
    # time_since_icu_min (time_since_hadm_min - intime_min) stays integer-valued.
    intime_expr   <- "ROUND((EXTRACT(EPOCH FROM (icu_intime  - admittime)) / 60.0)::numeric)"
    outtime_expr  <- "ROUND((EXTRACT(EPOCH FROM (icu_outtime - admittime)) / 60.0)::numeric)"
    hadm_end_expr <- "ROUND((EXTRACT(EPOCH FROM (dischtime   - admittime)) / 60.0)::numeric)"
  }

  start_off_min <- -window_before_hours * 60
  end_off_min   <- window_after_hours * 60
  other_icu_condition <- if (isTRUE(include_other_icu_events)) {
    "TRUE"
  } else {
    glue::glue("s.{icu_id_col} IS NULL OR s.{icu_id_col} IS NOT DISTINCT FROM b.assigned_icustay_id")
  }
  date_condition <- if (isTRUE(include_date_only_events)) {
    glue::glue(
      "s.time_precision = 'date'\n",
      "    AND s.time_since_hadm_min IS NOT NULL\n",
      "    AND ({other_icu_condition})\n",
      "    AND b.outtime_min >= FLOOR(s.time_since_hadm_min / 1440.0) * 1440.0\n",
      "    AND b.intime_min <  (FLOOR(s.time_since_hadm_min / 1440.0) + 1) * 1440.0"
    )
  } else {
    "FALSE"
  }
  date_tsi_expr <- switch(date_time_since_icu,
    null = "NULL::numeric",
    zero = "0::numeric",
    event_anchor = "(s.time_since_hadm_min - b.intime_min)"
  )

  ctes <- glue::glue(
"WITH src AS (
  SELECT *, ROW_NUMBER() OVER () AS src_row_id FROM {src_fqtn}
),
stays AS (
  SELECT {stays_ids}, {icu_id_col} AS assigned_icustay_id,
         admittime, dischtime,
         {intime_expr}  AS intime_min,
         {outtime_expr} AS outtime_min,
         {hadm_end_expr} AS hadm_end_min
  FROM {raw}.icu_windows
  WHERE {icu_id_col} IS NOT NULL
),
winb AS (
  SELECT *,
    intime_min + {start_off_min} AS win_start_min,
    outtime_min + {end_off_min} AS win_end_min
  FROM stays
),
matched AS (
  SELECT s.*,
         s.{icu_id_col} AS source_icustay_id,
         b.assigned_icustay_id,
         b.intime_min AS w_intime,
         (s.time_since_hadm_min - b.intime_min) AS assigned_time_since_icu_min,
         CASE WHEN s.time_since_hadm_min BETWEEN b.intime_min AND b.outtime_min THEN 'in_icu'
              WHEN s.time_since_hadm_min <  b.intime_min THEN 'before_icu'
              ELSE 'after_icu' END AS icu_time_relation,
         CASE WHEN s.{icu_id_col} IS NULL THEN 'hospital'
              WHEN s.{icu_id_col} IS NOT DISTINCT FROM b.assigned_icustay_id THEN 'same_icu'
              ELSE 'other_icu' END AS event_origin,
         CASE WHEN s.{icu_id_col} IS NOT DISTINCT FROM b.assigned_icustay_id THEN 0
              WHEN s.{icu_id_col} IS NULL THEN 1
              ELSE 2 END AS projection_rank,
         CASE WHEN s.time_since_hadm_min BETWEEN b.intime_min AND b.outtime_min THEN 0
              WHEN s.time_since_hadm_min <  b.intime_min THEN b.intime_min - s.time_since_hadm_min
              ELSE s.time_since_hadm_min - b.outtime_min END AS dist
  FROM src s
  JOIN winb b ON b.{adm_key} = s.{adm_key}
  WHERE s.time_precision = 'datetime'
    AND ({other_icu_condition})
    AND s.time_since_hadm_min BETWEEN b.win_start_min AND b.win_end_min
),
date_matched AS (
  SELECT s.*,
         s.{icu_id_col} AS source_icustay_id,
         b.assigned_icustay_id,
         b.intime_min AS w_intime,
         {date_tsi_expr} AS assigned_time_since_icu_min,
         'date_only' AS icu_time_relation,
         CASE WHEN s.{icu_id_col} IS NULL THEN 'hospital'
              WHEN s.{icu_id_col} IS NOT DISTINCT FROM b.assigned_icustay_id THEN 'same_icu'
              ELSE 'other_icu' END AS event_origin
  FROM src s
  JOIN winb b ON b.{adm_key} = s.{adm_key}
  WHERE {date_condition}
)")

  assigned_select <- glue::glue(
    "{keep_bare}, source_icustay_id, assigned_icustay_id, ",
    "event_origin, icu_time_relation, ",
    "assigned_time_since_icu_min AS time_since_icu_min"
  )

  if (overlap == "duplicate") {
    arm_matched_datetime <- glue::glue("SELECT {assigned_select}\nFROM matched")
  } else {
    pref <- if (overlap == "first") {
      glue::glue("projection_rank ASC, w_intime ASC, assigned_icustay_id ASC")
    } else {
      glue::glue(
        "projection_rank ASC, dist ASC, w_intime ASC, ",
        "assigned_icustay_id ASC"
      )
    }
    arm_matched_datetime <- glue::glue(
      "(SELECT DISTINCT ON (src_row_id) {assigned_select}\n",
      " FROM matched\n ORDER BY src_row_id, {pref})"
    )
  }

  arm_unmatched_datetime <- glue::glue(
    "SELECT {keep_s},\n",
    "  s.{icu_id_col} AS source_icustay_id,\n",
    "  NULL::{icu_id_type} AS assigned_icustay_id,\n",
    "  CASE WHEN s.{icu_id_col} IS NULL THEN 'hospital' ELSE 'unassigned_icu' END AS event_origin,\n",
    "  'not_in_icu_window' AS icu_time_relation,\n",
    "  NULL::numeric AS time_since_icu_min\n",
    "FROM src s\n",
    "WHERE s.time_precision = 'datetime'\n",
    "  AND NOT EXISTS (SELECT 1 FROM winb b WHERE b.{adm_key} = s.{adm_key}\n",
    "    AND ({other_icu_condition})\n",
    "    AND s.time_since_hadm_min BETWEEN b.win_start_min AND b.win_end_min)"
  )

  arm_matched_date <- glue::glue("SELECT {assigned_select}\nFROM date_matched")

  arm_unmatched_nondatetime <- glue::glue(
    "SELECT {keep_s},\n",
    "  s.{icu_id_col} AS source_icustay_id,\n",
    "  NULL::{icu_id_type} AS assigned_icustay_id,\n",
    "  CASE WHEN s.{icu_id_col} IS NULL THEN 'hospital' ELSE 'unassigned_icu' END AS event_origin,\n",
    "  CASE WHEN s.time_precision = 'date' THEN 'date_only' ELSE 'no_datetime' END AS icu_time_relation,\n",
    "  NULL::numeric AS time_since_icu_min\n",
    "FROM src s\n",
    "WHERE s.time_precision IS DISTINCT FROM 'datetime'\n",
    "  AND NOT EXISTS (SELECT 1 FROM date_matched dm WHERE dm.src_row_id = s.src_row_id)"
  )

  glue::glue(
    "{ctes}\n{arm_matched_datetime}\nUNION ALL\n{arm_unmatched_datetime}\nUNION ALL\n",
    "{arm_matched_date}\nUNION ALL\n{arm_unmatched_nondatetime}"
  )
}

# Admission grouping key used for the window join, by source family.
.icu_timeline_adm_key <- function(fam) {
  if (identical(fam, "eicu")) return("patienthealthsystemstayid")
  if (fam %in% c("mimic_iii", "mimic_iv")) return("hadm_id")
  stop("ICU timeline generation currently supports only eICU, MIMIC-III, ",
       "and MIMIC-IV source families.", call. = FALSE)
}

#' Build an ICU-assigned timeline from a native-grain harmonized table
#'
#' Projects a native-grain time-series table (usually `ts_tab`) onto ICU stays
#' without modifying the canonical store. Source rows keep their native
#' identifiers, including the source ICU stay ID when present. The derived table
#' adds an assigned ICU stay ID, event-origin labels, ICU-time-relation labels,
#' and a recomputed `time_since_icu_min` relative to the assigned ICU stay.
#'
#' This is intended for ICU-stay analysis and machine-learning datasets. For
#' example, a hospital-level lab with no source ICU stay ID can be assigned to
#' an ICU stay whose analysis window contains the lab time. If
#' `include_other_icu_events = TRUE`, ICU-native events from neighboring ICU
#' stays in the same admission can also be assigned when they fall inside the
#' analysis window; the original source ICU stay ID is retained.
#'
#' Date-only events are conservative by default. If `include_date_only_events =
#' TRUE`, a date-only row is assigned to every ICU stay in the same admission
#' whose ICU interval overlaps that 24-hour admission-relative date. Date-only
#' assignment intentionally uses the raw ICU interval, not the padded
#' `window_before_hours`/`window_after_hours` interval, and date-only rows are
#' not deduplicated by `overlap`.
#'
#' `event_origin` is one of `"hospital"`, `"same_icu"`, `"other_icu"`, or
#' `"unassigned_icu"`. The last value means the source row had an ICU stay ID,
#' but it was not assigned to an ICU timeline by the selected rules.
#'
#' eICU is supported with the same semantics: its ICU windows are numeric minute
#' offsets on the hospital-admission clock (`admittime = 0`) and its `ts_tab`
#' carries a hospital-admission-relative `time_since_hadm_min`, so the same
#' windowing and cross-stay assignment run, grouped on
#' `patienthealthsystemstayid` instead of `hadm_id`.
#'
#' @param src_name Source name (e.g. `"mimic_iv_demo"`).
#' @param db_name Optional Postgres database name; resolved from config if NULL.
#' @param base_dir Project root directory.
#' @param source_table Native-grain source table in the harmonized schema
#'   (default `"ts_tab"`).
#' @param out_table Output table/view name in the harmonized schema
#'   (default `"ts_icu"`).
#' @param icustay_id ICU stay identifier column. Defaults to the source ICU ID
#'   column from the dataset config (e.g. `"stay_id"` for MIMIC-IV).
#' @param target_id Deprecated alias for `icustay_id`.
#' @param window_before_hours Hours before ICU admission to include in each ICU
#'   analysis window.
#' @param window_after_hours Hours after ICU discharge to include in each ICU
#'   analysis window.
#' @param include_other_icu_events If `TRUE`, ICU-native events from other ICU
#'   stays in the same admission may be assigned to an ICU when they fall inside
#'   its analysis window. If `FALSE`, ICU-native events are only assigned to
#'   their own source ICU stay.
#' @param include_other_icu_stays Deprecated alias for
#'   `include_other_icu_events`.
#' @param overlap How to resolve a datetime event that lands in multiple ICU
#'   analysis windows: `"first"`, `"nearest"`, or `"duplicate"`. If omitted,
#'   defaults to `"duplicate"` when `include_other_icu_events = TRUE`, otherwise
#'   `"first"`. For `"first"` and `"nearest"`, an eligible source ICU is
#'   preferred before temporal ordering. `"first"` then orders by ICU-window
#'   start, while `"nearest"` orders by distance to the ICU interval. ICU stay
#'   ID is the final deterministic tie-break.
#' @param include_date_only_events If `TRUE`, date-only events are assigned to
#'   every ICU stay overlapping the same 24-hour admission-relative date.
#' @param date_time_since_icu How to populate `time_since_icu_min` for assigned
#'   date-only events: `"null"` (default), `"zero"`, or `"event_anchor"`.
#' @param materialize `TRUE` (default) builds a TABLE; `FALSE` builds a VIEW.
#' @return Invisibly, the output object name (`{schema}.{out_table}`).
#' @examples
#' \dontrun{
#' make_icu_timeline("mimic_iv_demo", db_name = "rendehr", window_before_hours = 6)
#' }
#' @export
make_icu_timeline <- function(src_name, db_name = NULL, base_dir = NULL,
                              source_table = "ts_tab",
                              out_table = "ts_icu",
                              icustay_id = NULL,
                              target_id = NULL,
                              window_before_hours = 0,
                              window_after_hours = 0,
                              include_other_icu_events = FALSE,
                              include_other_icu_stays = NULL,
                              overlap = NULL,
                              include_date_only_events = FALSE,
                              date_time_since_icu = c("null", "zero", "event_anchor"),
                              materialize = TRUE) {
  icustay_id_supplied <- !missing(icustay_id)
  include_other_supplied <- !missing(include_other_icu_events)

  if (!is.null(target_id)) {
    warning("target_id is deprecated; use icustay_id instead.", call. = FALSE)
    if (icustay_id_supplied && !identical(icustay_id, target_id)) {
      stop("target_id and icustay_id must agree when both are supplied.",
           call. = FALSE)
    }
    if (!icustay_id_supplied) icustay_id <- target_id
  }
  if (!is.null(include_other_icu_stays)) {
    warning("include_other_icu_stays is deprecated; use include_other_icu_events instead.",
            call. = FALSE)
    if (include_other_supplied &&
        !identical(include_other_icu_events, include_other_icu_stays)) {
      stop("include_other_icu_stays and include_other_icu_events must agree ",
           "when both are supplied.", call. = FALSE)
    }
    if (!include_other_supplied) {
      include_other_icu_events <- include_other_icu_stays
    }
  }
  if (is.null(overlap)) {
    overlap <- if (isTRUE(include_other_icu_events)) "duplicate" else "first"
  }
  overlap <- match.arg(overlap, c("first", "nearest", "duplicate"))
  date_time_since_icu <- match.arg(date_time_since_icu)

  if (!is.numeric(window_before_hours) || length(window_before_hours) != 1L ||
      is.na(window_before_hours) || !is.finite(window_before_hours) ||
      window_before_hours < 0) {
    stop("window_before_hours must be one finite non-negative number.")
  }
  if (!is.numeric(window_after_hours) || length(window_after_hours) != 1L ||
      is.na(window_after_hours) || !is.finite(window_after_hours) ||
      window_after_hours < 0) {
    stop("window_after_hours must be one finite non-negative number.")
  }
  if (!is.logical(include_other_icu_events) || length(include_other_icu_events) != 1L ||
      is.na(include_other_icu_events)) {
    stop("include_other_icu_events must be TRUE or FALSE.")
  }
  if (!is.logical(include_date_only_events) || length(include_date_only_events) != 1L ||
      is.na(include_date_only_events)) {
    stop("include_date_only_events must be TRUE or FALSE.")
  }
  if (!is.logical(materialize) || length(materialize) != 1L ||
      is.na(materialize)) {
    stop("materialize must be TRUE or FALSE.")
  }
  source_table <- .ehr_validate_identifier(source_table, "source_table")
  out_table <- .ehr_validate_identifier(out_table, "out_table")
  if (!is.null(icustay_id)) {
    icustay_id <- .ehr_validate_identifier(icustay_id, "icustay_id")
  }

  fam <- src_family(src_name)
  adm_key <- .icu_timeline_adm_key(fam)

  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name      <- .ehr_resolve_db_name(db_name)
  schema_names <- src_schema_names(src_name)
  cfg            <- get_dataset_config(src_name)
  icu_id_col     <- .ehr_validate_identifier(
    utils::tail(cfg$select_id_cols, 1), "configured ICU identifier")
  icu_id_type    <- .ehr_icu_id_sql_type(cfg$icu_id_type %||% "bigint")
  raw_schema <- .ehr_validate_identifier(schema_names$raw, "raw schema")
  harm_schema <- .ehr_validate_identifier(schema_names$harmonized,
                                          "harmonized schema")
  icustay_id <- icustay_id %||% icu_id_col
  if (!identical(icustay_id, icu_id_col)) {
    stop("Only icustay_id = '", icu_id_col, "' is currently supported for ",
         src_name, ".")
  }

  creds        <- load_credentials(paths$credentials_json)
  con          <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  raw <- .ehr_quote_identifier(con, raw_schema)
  harm <- .ehr_quote_identifier(con, harm_schema)
  src_fqtn <- .ehr_quote_relation(con, harm_schema, source_table)
  out_fqtn <- .ehr_quote_relation(con, harm_schema, out_table)

  if (!.ehr_relation_exists(con, harm_schema, source_table)) {
    stop(source_table, " not found in schema '", harm,
         "'. Run run_harmonized_tables() first.")
  }

  src_cols <- DBI::dbListFields(
    con, DBI::Id(schema = harm_schema, table = source_table))
  assignment_cols <- c(
    "source_icustay_id", "assigned_icustay_id", "event_origin", "icu_time_relation"
  )
  existing_assignment_cols <- intersect(assignment_cols, src_cols)
  if (length(existing_assignment_cols)) {
    stop(source_table, " appears to already be ICU-assigned because it contains: ",
         paste(existing_assignment_cols, collapse = ", "),
         ". Use a native-grain source table such as ts_tab.")
  }
  # time_since_icu_min is recomputed relative to the assigned ICU stay, and the
  # native ICU ID is exposed with the clearer source_icustay_id name.
  keep_cols <- vapply(
    setdiff(src_cols, c("time_since_icu_min", icu_id_col)),
    function(x) .ehr_quote_identifier(
      con, .ehr_validate_identifier(x, "source column")),
    character(1)
  )

  obj     <- if (materialize) "TABLE" else "VIEW"

  req_cols <- c(adm_key, icu_id_col, "time_since_hadm_min", "time_precision")
  missing_cols <- setdiff(req_cols, src_cols)
  if (length(missing_cols)) {
    stop(source_table, " is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  if (!.ehr_relation_exists(con, raw_schema, "icu_windows")) {
    stop("icu_windows not found in '", raw, "'. Run build_window_tables() first.")
  }

  body <- .build_icu_timeline_body(
    fam = fam, src_fqtn = src_fqtn, raw = raw,
    icu_id_col = .ehr_quote_identifier(con, icu_id_col),
    icu_id_type = icu_id_type, keep_cols = keep_cols,
    window_before_hours = window_before_hours, window_after_hours = window_after_hours,
    include_other_icu_events = include_other_icu_events, overlap = overlap,
    include_date_only_events = include_date_only_events,
    date_time_since_icu = date_time_since_icu
  )

  tx_open <- FALSE
  DBI::dbBegin(con)
  tx_open <- TRUE
  tryCatch({
    .ehr_drop_relation(con, harm_schema, out_table)
    DBI::dbExecute(
      con, glue::glue("CREATE {obj} {out_fqtn} AS\n{body};"),
      immediate = TRUE
    )
    out_cols <- DBI::dbListFields(
      con, DBI::Id(schema = harm_schema, table = out_table))
    required_out <- c(
      "source_icustay_id", "assigned_icustay_id", "event_origin",
      "icu_time_relation", "time_since_icu_min"
    )
    missing_out <- setdiff(required_out, out_cols)
    if (length(missing_out)) {
      stop("ICU timeline output is missing required column(s): ",
           paste(missing_out, collapse = ", "), call. = FALSE)
    }
    DBI::dbCommit(con)
    tx_open <- FALSE
  }, error = function(e) {
    if (tx_open && DBI::dbIsValid(con)) DBI::dbRollback(con)
    stop(conditionMessage(e), call. = FALSE)
  })
  message("ICU timeline built: ", out_fqtn,
          "  (src_family=", fam,
          ", icustay_id=", icustay_id,
          ", before=", window_before_hours, "h",
          ", after=", window_after_hours, "h",
          ", include_other_icu_events=", include_other_icu_events,
          ", overlap=", overlap,
          ", include_date_only_events=", include_date_only_events,
          ", date_time_since_icu=", date_time_since_icu, ")")
  invisible(as.character(out_fqtn))
}

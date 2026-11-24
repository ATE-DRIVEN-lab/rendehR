# Internal: map generic alias -> original column name per source family
.src_id_map <- function(src_name) {
  switch(src_family(src_name),
    mimic_iv  = c(src_patient_id   = "subject_id",
                  src_admission_id = "hadm_id",
                  src_icu_id       = "stay_id"),
    mimic_iii = c(src_patient_id   = "subject_id",
                  src_admission_id = "hadm_id",
                  src_icu_id       = "icustay_id"),
    eicu      = c(src_patient_id   = "uniquepid",
                  src_admission_id = "patienthealthsystemstayid",
                  src_icu_id       = "patientunitstayid"),
    stop("Unknown src_family for ID mapping: ", src_name)
  )
}

# Cast a column/expression for safe UNION ALL across heterogeneous sources.
.sql_cast <- function(expr, as_type = "text", alias = NULL) {
  out <- paste0("CAST(", expr, " AS ", as_type, ")")
  if (!is.null(alias)) out <- paste0(out, " AS ", alias)
  out
}

# Output type of each merged flat / cohort_info column, from the packaged
# variable dictionary (rendehr_flat_concepts_config.csv). Only four types are
# used: numeric, date, logical and character. A column not in the dictionary
# is merged as character.
.flat_concept_types <- function() {
  cfg <- utils::read.csv(ehr_config_path("rendehr_flat_concepts_config.csv"),
                         stringsAsFactors = FALSE, na.strings = "")
  bad <- setdiff(cfg$output_type, c("numeric", "date", "logical", "character"))
  if (length(bad))
    stop("rendehr_flat_concepts_config.csv: unknown output_type ",
         paste(bad, collapse = ", "), call. = FALSE)
  stats::setNames(cfg$output_type, cfg$concept_name)
}

# Cast one merged flat / cohort_info value to its dictionary type. A logical is
# stored as a 0/1 integer or a boolean in the source tables, so it goes through
# integer first; that also works for NULL.
.merge_typed_expr <- function(expr, col, types) {
  type <- if (col %in% names(types)) types[[col]] else "character"
  out <- switch(type,
    numeric   = paste0("CAST(", expr, " AS numeric)"),
    date      = paste0("CAST(", expr, " AS date)"),
    logical   = paste0("CAST(CAST(", expr, " AS integer) <> 0 AS boolean)"),
    paste0("CAST(", expr, " AS text)"))
  paste0(out, " AS ", col)
}

# Inspect the actual left input; source-wide ID assumptions are insufficient.
# Inputs are materialized pipeline tables and must remain stable during build.
.non_null_id_cols <- function(con, schema, table, id_cols) {
  target <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table))
  quoted <- DBI::dbQuoteIdentifier(con, id_cols)
  counts <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM %s",
    paste(sprintf("COUNT(*) - COUNT(%s) AS %s", quoted, quoted), collapse = ", "),
    target
  ))
  result <- id_cols[vapply(id_cols, function(id) {
    isTRUE(counts[[id]][1] == 0)
  }, logical(1))]
  if (!length(result))
    message("No non-NULL ID column in ", schema, ".", table,
            "; retaining NULL-safe joins (may be slow).")
  result
}

# Build a NULL-safe equality join across a source's configured ID columns.
.null_safe_id_join <- function(left_alias, right_alias, id_cols,
                               non_null_cols = character()) {
  paste(
    sprintf("%s.%s %s %s.%s",
            left_alias, id_cols,
            ifelse(id_cols %in% non_null_cols, "=", "IS NOT DISTINCT FROM"),
            right_alias, id_cols),
    collapse = "\n  AND "
  )
}


# Resolve the source-level time-series table for a merged analysis level.
.merge_ts_table_name <- function(analysis_level) {
  analysis_level <- match.arg(analysis_level, c("admission", "icu"))
  if (analysis_level == "icu") "ts_icu" else "ts_tab"
}

# Column expression helpers for heterogeneous source UNIONs.
.merge_col_expr <- function(cols, col, alias = col, table_alias = "ts",
                            as_type = "text", required = FALSE,
                            reason = NULL) {
  if (col %in% cols)
    return(.sql_cast(paste0(table_alias, ".", col), as_type = as_type, alias = alias))
  if (isTRUE(required)) {
    msg <- paste0("Required column '", col, "' is missing")
    if (!is.null(reason)) msg <- paste0(msg, " ", reason)
    stop(msg, call. = FALSE)
  }
  .sql_cast("NULL", as_type = as_type, alias = alias)
}

.merge_raw_expr <- function(cols, col, table_alias = "ts", required = FALSE,
                            reason = NULL) {
  if (col %in% cols) return(paste0(table_alias, ".", col))
  if (isTRUE(required)) {
    msg <- paste0("Required column '", col, "' is missing")
    if (!is.null(reason)) msg <- paste0(msg, " ", reason)
    stop(msg, call. = FALSE)
  }
  "NULL"
}

.merge_ehr_id_expr <- function(parts) {
  paste0(
    "concat_ws('_', ",
    paste0("NULLIF(CAST(", parts, " AS text), '')", collapse = ", "),
    ")"
  )
}

# Select the id components that compose ehr_id at a given analysis grain.
# `parts` are ordered patient -> admission -> icu. Admission-level analysis keys
# on patient+admission; ICU-level analysis additionally appends the icu id.
.ehr_id_grain_parts <- function(parts, analysis_level) {
  if (identical(analysis_level, "icu")) parts else parts[-length(parts)]
}

# One cohort_info row per admission for the admission-grain flat/cohort_info
# merge: the first ICU stay (earliest icu_intime, then lowest ICU id), or the
# ICU-less row when the admission has no ICU stay. Returns a JOIN clause that
# keeps only that row of `alias` (a cohort_info alias) and exposes
# fi.adm_cohort_index, the admission-level membership (any of its rows).
.merge_first_icu_join <- function(raw_schema, id_map, alias, has_icu_windows,
                                  has_cohort_index) {
  adm <- id_map[["src_admission_id"]]
  icu <- id_map[["src_icu_id"]]
  windows_join <- if (has_icu_windows) {
    paste0(" LEFT JOIN ", raw_schema, ".icu_windows w ON w.", icu, " = c1.", icu)
  } else ""
  intime_order <- if (has_icu_windows) "w.icu_intime NULLS LAST, " else ""
  member <- if (has_cohort_index) {
    "bool_or(CAST(c1.cohort_index AS boolean))"
  } else "CAST(NULL AS boolean)"
  paste0(
    "JOIN (SELECT c1.", adm, " AS first_adm, (array_agg(c1.", icu,
    " ORDER BY (c1.", icu, " IS NULL), ", intime_order, "c1.", icu,
    "))[1] AS first_icu, ", member, " AS adm_cohort_index FROM ",
    raw_schema, ".cohort_info c1", windows_join, " GROUP BY c1.", adm,
    ") fi ON fi.first_adm = ", alias, ".", adm,
    " AND fi.first_icu IS NOT DISTINCT FROM ", alias, ".", icu)
}

#' Merge harmonized source tables across sources into one schema.
#'
#' Each source has different patient ID columns (e.g. stay_id vs
#' patientunitstayid). This function UNION ALLs harmonized source outputs into a
#' single target schema (default: "rendehr"). For time series,
#' `analysis_level = "admission"` reads the native-grain `ts_tab`, and
#' `analysis_level = "icu"` reads the derived ICU projection `ts_icu` created by
#' `make_icu_timeline()`. In the merged time-series table, `src_icu_id` is always
#' the native/source ICU ID. The projection columns `assigned_icu_id`,
#' `event_origin`, and `icu_time_relation` are emitted only for
#' `analysis_level = "icu"` (omitted in admission mode, where they would be all
#' NULL). The merged `ehr_id` is grain-specific: `analysis_level = "admission"`
#' keys on `src_patient_id + src_admission_id`, while `analysis_level = "icu"`
#' additionally appends the (assigned) ICU id. It is recomputed from the source
#' id columns at merge time, so it always matches the requested grain regardless
#' of how each source `cohort_info` was built. Use `event_id` to track the same
#' physical event across admission and ICU projections.
#'
#' @param src_names Character vector of source names (e.g.
#'   c("mimic_iv_demo", "mimic_iii_demo", "eicu_demo")).
#' @param db_name Postgres database name. Defaults to the active project
#'   config, then "rendehr".
#' @param base_dir Project root directory. Defaults to the active project
#'   config, then current working directory.
#' @param target_schema Target schema for merged tables. Default "rendehr".
#' @param merge_ts Build merged ts table (rendehr.ts). Default TRUE. Its
#'   `value` column keeps the source type, NUMERIC(18,3), as does dur's.
#' @param merge_dur Build merged dur table (rendehr.dur) from each source's
#'   dur_tab (medication exposures, infusions, ventilation episodes). Default
#'   TRUE. Uses the same ehr_id/append-mode contract as the other merged
#'   tables; unlike ts, dur has no separate ICU projection variant, so the
#'   same source dur_tab is read regardless of `analysis_level`.
#' @param merge_flat Build merged flat table (rendehr.flat). Default TRUE.
#'   Flat and cohort_info columns get the type listed in the packaged variable
#'   dictionary `rendehr_flat_concepts_config.csv` (numeric, date, logical or
#'   character); identifiers and columns not in the dictionary are character.
#'   Every flat column of any source is kept; it is NULL for sources that do
#'   not provide it.
#' @param merge_cohort Build merged cohort_info table (rendehr.cohort_info). Default TRUE.
#' @param append If FALSE (default), rebuild merged tables from exactly
#'   `src_names`. If TRUE, keep existing merged tables and replace rows for
#'   `src_names` before inserting their current data. Append mode requires the
#'   selected target tables to already exist and uses their existing columns as
#'   the schema contract.
#'
#'   Both modes run in a single transaction that is rolled back on error, so
#'   a failed merge leaves the previous merged tables unchanged.
#' @param analysis_level Time-series grain to merge: `"admission"` merges native
#'   `ts_tab`; `"icu"` merges `ts_icu` and requires `make_icu_timeline()` first.
#'   At `"admission"` the merged `flat` and `cohort_info` hold one row per
#'   admission: the row of its first ICU stay (earliest ICU admission time,
#'   then lowest ICU id), or the ICU-less row when there is no ICU stay. The
#'   number of later-ICU rows left out is reported per source.
#' @return Invisibly TRUE.
#'
#' @details
#' An admission does not mean exactly the same thing in every source. In
#' MIMIC-III and MIMIC-IV, `src_admission_id` is the hospital admission
#' (`hadm_id`) and cohorts are selected per admission. In eICU it is the
#' hospital stay (`patienthealthsystemstayid`), but cohorts are selected per
#' ICU unit stay (`patientunitstayid`), so a merged eICU admission holds only
#' the unit stays that were selected, not necessarily the whole hospital stay.
#' eICU also has no admission timestamps: its window tables store offsets in
#' minutes (`admittime` is 0), while MIMIC stores timestamps. Times in
#' `time_since_hadm_min` count from the hospital admission in every source.
#'
#' The merged `ehr_id` is computed as
#' `concat_ws('_', NULLIF(CAST(part AS text), ''), ...)` over each row's
#' grain-defining source ID parts (patient, admission, and — at ICU grain —
#' ICU). `concat_ws()` silently omits any part that is NULL or the empty
#' string rather than raising, on the assumption that every source row
#' always has non-NULL values for its grain-defining ID columns. If that
#' assumption is ever violated for a given row, its `ehr_id` is computed
#' from only the remaining non-NULL parts, which could coincide with another
#' row's `ehr_id`; this is a documented, currently unvalidated assumption,
#' not a runtime-checked invariant.
#' @examples
#' \dontrun{
#' merge_sources(c("mimic_iii_demo", "mimic_iv_demo", "eicu_demo"),
#'               db_name = "rendehr", target_schema = "rendehr_admission")
#' }
#' @export
merge_sources <- function(src_names, db_name = NULL, base_dir = NULL,
                          target_schema = "rendehr",
                          merge_ts      = TRUE,
                          merge_dur     = TRUE,
                          merge_flat    = TRUE,
                          merge_cohort  = TRUE,
                          append        = FALSE,
                          analysis_level = c("admission", "icu")) {
  if (!is.character(src_names) || length(src_names) == 0L || any(!nzchar(src_names))) {
    stop("src_names must contain at least one source name.", call. = FALSE)
  }
  if (!is.logical(append) || length(append) != 1L || is.na(append)) {
    stop("append must be TRUE or FALSE.", call. = FALSE)
  }
  analysis_level <- match.arg(analysis_level)
  flat_types <- .flat_concept_types()
  ts_table_name <- .merge_ts_table_name(analysis_level)
  target_schema_valid <- .ehr_validate_db_identifier(target_schema, "schema name")
  .merge_target_id <- function(table) {
    DBI::dbQuoteIdentifier(con, DBI::Id(schema = target_schema_valid, table = table))
  }

  paths <- ehr_paths(src_name = src_names[[1]], base_dir = base_dir)
  base_dir <- paths$base_dir
  db_name <- .ehr_resolve_db_name(db_name)

  creds <- load_credentials(paths$credentials_json)
  ensure_database_exists(db_name, creds)
  con      <- connect_postgres(db_name, creds)
  tx_open <- FALSE
  on.exit({
    if (isTRUE(tx_open) && DBI::dbIsValid(con)) DBI::dbRollback(con)
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  }, add = TRUE)

  DBI::dbExecute(con,
    sprintf("CREATE SCHEMA IF NOT EXISTS %s",
            DBI::dbQuoteIdentifier(con, target_schema_valid)),
    immediate = TRUE
  )

  # helper: get all column names for a table
  .cols <- function(schema, table) {
    DBI::dbGetQuery(con, sprintf(
      "SELECT column_name FROM information_schema.columns
       WHERE table_schema = '%s' AND table_name = '%s'
       ORDER BY ordinal_position",
      schema, table
    ))$column_name
  }

  .table_exists <- function(schema, table) {
    nrow(DBI::dbGetQuery(con, sprintf(
      "SELECT 1 FROM information_schema.tables
       WHERE table_schema = '%s' AND table_name = '%s'",
      schema, table
    ))) > 0
  }

  .require_append_target <- function(table) {
    if (!.table_exists(target_schema, table)) {
      stop(target_schema, ".", table,
           " does not exist. Run merge_sources(..., append = FALSE) first.",
           call. = FALSE)
    }
    target_cols <- .cols(target_schema, table)
    if (!("source_data" %in% target_cols)) {
      stop(target_schema, ".", table,
           " is missing source_data and cannot be appended safely.",
           call. = FALSE)
    }
    target_cols
  }

  .source_delete_sql <- function(table) {
    src_sql <- paste(
      vapply(src_names, function(src) as.character(DBI::dbQuoteLiteral(con, src)),
             character(1)),
      collapse = ", "
    )
    sprintf("DELETE FROM %s WHERE source_data IN (%s);",
            .merge_target_id(table), src_sql)
  }

  .insert_sql <- function(table, cols, parts) {
    paste0(
      .source_delete_sql(table), "\n",
      "INSERT INTO ", .merge_target_id(table), " (",
      paste(cols, collapse = ", "), ")\n",
      paste(parts, collapse = "\nUNION ALL\n"), ";"
    )
  }

  # Admission grain: keep the first-ICU row of each admission (see
  # .merge_first_icu_join()) and report the later-ICU rows left out.
  .first_icu_from <- function(from_sql, sn, id_map, label, src) {
    first_from <- paste0(from_sql, "\n", .merge_first_icu_join(
      sn$raw, id_map, "ci", .table_exists(sn$raw, "icu_windows"),
      "cohort_index" %in% .cols(sn$raw, "cohort_info")))
    n <- DBI::dbGetQuery(con, paste0(
      "SELECT (SELECT count(*) ", from_sql, ") - (SELECT count(*) ", first_from,
      ") AS n"))$n
    if (isTRUE(as.numeric(n) > 0))
      message("merge_sources(): ", format(as.numeric(n), big.mark = ","), " ",
              label, " row(s) of ", src, " belong to a later ICU stay of the ",
              "same admission; the admission-level merge keeps the first ICU stay.")
    first_from
  }

  DBI::dbBegin(con)
  tx_open <- TRUE

  # ---------------------------------------------------------------------------
  # Merge source-level time series -> target_schema.ts
  # admission: native ts_tab, one row per source event.
  # icu: explicit ts_icu projection built by make_icu_timeline().
  # ---------------------------------------------------------------------------
  if (merge_ts) {
    ts_target_cols <- if (append) .require_append_target("ts") else NULL

    ts_parts <- vapply(src_names, function(src) {
      sn     <- src_schema_names(src)
      id_map <- .src_id_map(src)
      ts_cols <- .cols(sn$harmonized, ts_table_name)

      if (length(ts_cols) == 0L) {
        msg <- paste0(sn$harmonized, ".", ts_table_name, " does not exist")
        if (analysis_level == "icu")
          msg <- paste0(msg, ". Run make_icu_timeline(\"", src,
                        "\") before merge_sources(analysis_level = 'icu').")
        else
          msg <- paste0(msg, ". Run run_harmonized_tables(\"", src, "\") first.")
        stop(msg, call. = FALSE)
      }
      if (analysis_level == "icu" && !("assigned_icustay_id" %in% ts_cols)) {
        stop(sn$harmonized, ".", ts_table_name,
             " is missing assigned_icustay_id. Rebuild it with make_icu_timeline() ",
             "before merge_sources(analysis_level = 'icu').",
             call. = FALSE)
      }

      patient_col <- unname(id_map["src_patient_id"])
      adm_col     <- unname(id_map["src_admission_id"])
      icu_col     <- unname(id_map["src_icu_id"])

      patient_expr <- .merge_raw_expr(ts_cols, patient_col, required = TRUE,
                                      reason = paste0("in ", sn$harmonized, ".", ts_table_name, "."))
      adm_expr     <- .merge_raw_expr(ts_cols, adm_col, required = TRUE,
                                      reason = paste0("in ", sn$harmonized, ".", ts_table_name, "."))
      native_icu_expr <- if ("source_icustay_id" %in% ts_cols) {
        "ts.source_icustay_id"
      } else {
        .merge_raw_expr(ts_cols, icu_col)
      }
      assigned_icu_expr <- if ("assigned_icustay_id" %in% ts_cols) {
        "ts.assigned_icustay_id"
      } else {
        "NULL"
      }
      active_icu_expr <- if (analysis_level == "icu") assigned_icu_expr else native_icu_expr
      ehr_expr <- .merge_ehr_id_expr(
        .ehr_id_grain_parts(c(patient_expr, adm_expr, active_icu_expr), analysis_level))

      # ICU-projection columns are emitted only for analysis_level = "icu"; in
      # admission mode they would be all-NULL, so they are omitted entirely.
      include_icu  <- analysis_level == "icu"
      assigned_seg <- if (include_icu)
        paste0(.sql_cast(assigned_icu_expr, alias = "assigned_icu_id"), ",\n  ") else ""
      relation_seg <- if (include_icu)
        paste0(.merge_col_expr(ts_cols, "event_origin"), ",\n  ",
               .merge_col_expr(ts_cols, "icu_time_relation"), ",\n  ") else ""

      ts_exprs <- c(
        ehr_id              = .sql_cast(ehr_expr, alias = "ehr_id"),
        source_data         = .sql_cast(paste0("'", src, "'"), alias = "source_data"),
        src_patient_id      = .sql_cast(patient_expr, alias = "src_patient_id"),
        src_admission_id    = .sql_cast(adm_expr, alias = "src_admission_id"),
        src_icu_id          = .sql_cast(native_icu_expr, alias = "src_icu_id"),
        assigned_icu_id     = .sql_cast(assigned_icu_expr, alias = "assigned_icu_id"),
        event_id            = .merge_col_expr(ts_cols, "event_id"),
        event_level         = .merge_col_expr(ts_cols, "event_level"),
        native_id_level     = .merge_col_expr(ts_cols, "native_id_level"),
        time_precision      = .merge_col_expr(ts_cols, "time_precision"),
        source_table        = .merge_col_expr(ts_cols, "source_table"),
        source_dataset      = .merge_col_expr(ts_cols, "source_dataset"),
        event_origin        = .merge_col_expr(ts_cols, "event_origin"),
        icu_time_relation   = .merge_col_expr(ts_cols, "icu_time_relation"),
        time_since_hadm_min = .merge_col_expr(ts_cols, "time_since_hadm_min",
                                              as_type = "double precision"),
        time_since_icu_min  = .merge_col_expr(ts_cols, "time_since_icu_min",
                                              as_type = "double precision"),
        concept_name        = .merge_col_expr(ts_cols, "concept_name"),
        value               = .merge_col_expr(ts_cols, "value", as_type = "numeric(18,3)"),
        unit                = .merge_col_expr(ts_cols, "unit")
      )

      if (append) {
        missing_cols <- setdiff(ts_target_cols, names(ts_exprs))
        if (length(missing_cols) > 0) {
          stop(target_schema, ".ts has unsupported columns for append: ",
               paste(missing_cols, collapse = ", "), call. = FALSE)
        }
        return(paste0(
          "SELECT ",
          paste(ts_exprs[ts_target_cols], collapse = ",\n  "), "\n",
          "FROM ", sn$harmonized, ".", ts_table_name, " ts"
        ))
      }

      paste0(
        "SELECT ",
        .sql_cast(ehr_expr, alias = "ehr_id"), ", ",
        .sql_cast(paste0("'", src, "'"), alias = "source_data"), ",\n  ",
        .sql_cast(patient_expr, alias = "src_patient_id"), ",\n  ",
        .sql_cast(adm_expr, alias = "src_admission_id"), ",\n  ",
        .sql_cast(native_icu_expr, alias = "src_icu_id"), ",\n  ",
        assigned_seg,
        .merge_col_expr(ts_cols, "event_id"), ",\n  ",
        .merge_col_expr(ts_cols, "event_level"), ",\n  ",
        .merge_col_expr(ts_cols, "native_id_level"), ",\n  ",
        .merge_col_expr(ts_cols, "time_precision"), ",\n  ",
        .merge_col_expr(ts_cols, "source_table"), ",\n  ",
        .merge_col_expr(ts_cols, "source_dataset"), ",\n  ",
        relation_seg,
        .merge_col_expr(ts_cols, "time_since_hadm_min", as_type = "double precision"), ",\n  ",
        .merge_col_expr(ts_cols, "time_since_icu_min", as_type = "double precision"), ",\n  ",
        .merge_col_expr(ts_cols, "concept_name"), ",\n  ",
        .merge_col_expr(ts_cols, "value", as_type = "numeric(18,3)"), ",\n  ",
        .merge_col_expr(ts_cols, "unit"), "\n",
        "FROM ", sn$harmonized, ".", ts_table_name, " ts"
      )
    }, character(1))

    ts_sql <- if (append) {
      .insert_sql("ts", ts_target_cols, ts_parts)
    } else {
      paste0(
        "DROP TABLE IF EXISTS ", .merge_target_id("ts"), ";\n",
        "CREATE TABLE ", .merge_target_id("ts"), " AS\n",
        paste(ts_parts, collapse = "\nUNION ALL\n"), ";"
      )
    }
    DBI::dbExecute(con, ts_sql, immediate = TRUE)
    if (append) {
      message("Merged ts table appended: ", target_schema, ".ts from ", ts_table_name,
              " (analysis_level = ", analysis_level, ")")
    } else {
      message("Merged ts table created: ", target_schema, ".ts from ", ts_table_name,
              " (analysis_level = ", analysis_level, ")")
    }
  }

  # ---------------------------------------------------------------------------
  # Merge dur_tab -> rendehr.dur
  # Duration/medication-exposure concepts (start_time/end_time/duration_min).
  # dur_tab has no separate ICU-projection variant (unlike ts_tab/ts_icu), so
  # the same source dur_tab is read regardless of analysis_level; ehr_id is
  # still computed at the requested grain.
  # ---------------------------------------------------------------------------
  if (merge_dur) {
    dur_target_cols <- if (append) .require_append_target("dur") else NULL

    dur_parts <- vapply(src_names, function(src) {
      sn      <- src_schema_names(src)
      id_map  <- .src_id_map(src)
      dur_cols <- .cols(sn$harmonized, "dur_tab")

      if (length(dur_cols) == 0L) {
        message("  No dur_tab for '", src, "' - skipping in merged dur table.")
        return(NA_character_)
      }

      patient_col <- unname(id_map["src_patient_id"])
      adm_col     <- unname(id_map["src_admission_id"])
      icu_col     <- unname(id_map["src_icu_id"])

      patient_expr <- .merge_raw_expr(dur_cols, patient_col, required = TRUE,
                                      reason = paste0("in ", sn$harmonized, ".dur_tab."))
      adm_expr     <- .merge_raw_expr(dur_cols, adm_col, required = TRUE,
                                      reason = paste0("in ", sn$harmonized, ".dur_tab."))
      icu_expr     <- .merge_raw_expr(dur_cols, icu_col)
      ehr_expr     <- .merge_ehr_id_expr(
        .ehr_id_grain_parts(c(patient_expr, adm_expr, icu_expr), analysis_level))

      dur_exprs <- c(
        ehr_id              = .sql_cast(ehr_expr, alias = "ehr_id"),
        source_data         = .sql_cast(paste0("'", src, "'"), alias = "source_data"),
        src_patient_id      = .sql_cast(patient_expr, alias = "src_patient_id"),
        src_admission_id    = .sql_cast(adm_expr, alias = "src_admission_id"),
        src_icu_id          = .sql_cast(icu_expr, alias = "src_icu_id"),
        # start_time/end_time are cast to the default "text" type (not a
        # specific numeric/timestamp type) because their underlying column
        # type is genuinely heterogeneous across sources - e.g. an integer
        # minute-offset for eicu_demo vs. an actual `timestamp without time
        # zone` for mimic_iii_demo, as seen in the real per-source schemas.
        # This matches how every other cross-source-heterogeneous column in
        # this function (event_id, event_level, concept_name, unit, etc.)
        # already defaults to a text cast for UNION safety.
        start_time          = .merge_col_expr(dur_cols, "start_time"),
        end_time            = .merge_col_expr(dur_cols, "end_time"),
        end_offset_min      = .merge_col_expr(dur_cols, "end_offset_min",
                                              as_type = "double precision"),
        duration_min        = .merge_col_expr(dur_cols, "duration_min",
                                              as_type = "double precision"),
        event_id            = .merge_col_expr(dur_cols, "event_id"),
        event_level         = .merge_col_expr(dur_cols, "event_level"),
        native_id_level     = .merge_col_expr(dur_cols, "native_id_level"),
        time_precision      = .merge_col_expr(dur_cols, "time_precision"),
        source_table        = .merge_col_expr(dur_cols, "source_table"),
        source_dataset      = .merge_col_expr(dur_cols, "source_dataset"),
        concept_name        = .merge_col_expr(dur_cols, "concept_name"),
        value               = .merge_col_expr(dur_cols, "value", as_type = "numeric(18,3)"),
        unit                = .merge_col_expr(dur_cols, "unit")
      )

      if (append) {
        missing_cols <- setdiff(dur_target_cols, names(dur_exprs))
        if (length(missing_cols) > 0) {
          stop(target_schema, ".dur has unsupported columns for append: ",
               paste(missing_cols, collapse = ", "), call. = FALSE)
        }
        return(paste0(
          "SELECT ",
          paste(dur_exprs[dur_target_cols], collapse = ",\n  "), "\n",
          "FROM ", sn$harmonized, ".dur_tab ts"
        ))
      }

      paste0(
        "SELECT ",
        paste(dur_exprs, collapse = ",\n  "), "\n",
        "FROM ", sn$harmonized, ".dur_tab ts"
      )
    }, character(1))

    dur_parts <- dur_parts[!is.na(dur_parts)]

    if (length(dur_parts) == 0) {
      message("No source dur_tab found - skipping merged dur table.")
    } else {
      dur_sql <- if (append) {
        .insert_sql("dur", dur_target_cols, dur_parts)
      } else {
        paste0(
          "DROP TABLE IF EXISTS ", .merge_target_id("dur"), ";\n",
          "CREATE TABLE ", .merge_target_id("dur"), " AS\n",
          paste(dur_parts, collapse = "\nUNION ALL\n"), ";"
        )
      }
      DBI::dbExecute(con, dur_sql, immediate = TRUE)
      if (append) {
        message("Merged dur table appended: ", target_schema, ".dur")
      } else {
        message("Merged dur table created: ", target_schema, ".dur")
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Merge flat_tab -> rendehr.flat
  # Schema: ehr_id, source_data, ids, every concept column of any source (NULL
  # for a source that lacks it)
  # ---------------------------------------------------------------------------
  if (merge_flat) {
    flat_target_cols <- if (append) .require_append_target("flat") else NULL

    id_cols_per_src <- lapply(src_names, function(src) {
      get_dataset_config(src)$select_id_cols
    })

    flat_concept_cols <- lapply(seq_along(src_names), function(i) {
      src <- src_names[i]
      sn  <- src_schema_names(src)
      all_cols <- .cols(sn$harmonized, "flat_tab")
      setdiff(all_cols, c("source_data", id_cols_per_src[[i]]))
    })

    all_flat <- unique(unlist(flat_concept_cols))
    if (!append) {
      for (i in seq_along(src_names)) {
        missing <- setdiff(all_flat, flat_concept_cols[[i]])
        if (length(missing))
          message("merge_sources(): ", length(missing), " flat column(s) are NULL ",
                  "for ", src_names[i], ", which does not provide them: ",
                  paste(missing, collapse = ", "))
      }
    }

    if (length(all_flat) == 0 && !append) {
      message("No flat concept columns in any source - skipping flat merge.")
    } else {
      flat_parts <- vapply(src_names, function(src) {
        cfg       <- get_dataset_config(src)
        sn        <- src_schema_names(src)
        id_map    <- .src_id_map(src)
        flat_cols <- .cols(sn$harmonized, "flat_tab")
        non_null_cols <- .non_null_id_cols(con, sn$harmonized, "flat_tab", cfg$select_id_cols)
        join_cond <- .null_safe_id_join("ft", "ci", cfg$select_id_cols, non_null_cols)
        # The merge INNER JOINs flat_tab to cohort_info; say how many rows that
        # leaves out instead of dropping them silently.
        unmatched <- DBI::dbGetQuery(con, paste0(
          "SELECT count(*) AS n FROM ", sn$harmonized, ".flat_tab ft ",
          "WHERE NOT EXISTS (SELECT 1 FROM ", sn$raw, ".cohort_info ci WHERE ",
          join_cond, ")"))$n
        if (isTRUE(as.numeric(unmatched) > 0))
          message("merge_sources(): ", format(as.numeric(unmatched), big.mark = ","),
                  " flat_tab row(s) of ", src, " have no cohort_info row and are ",
                  "not merged.")
        flat_from <- paste0("FROM ", sn$harmonized, ".flat_tab ft\n",
                            "JOIN ", sn$raw, ".cohort_info ci ON ", join_cond)
        if (analysis_level == "admission") {
          flat_from <- .first_icu_from(flat_from, sn, id_map, "flat_tab", src)
        }
        ehr_expr  <- .merge_ehr_id_expr(.ehr_id_grain_parts(c(
          paste0("ci.", id_map["src_patient_id"]),
          paste0("ci.", id_map["src_admission_id"]),
          paste0("ft.", id_map["src_icu_id"])), analysis_level))

        if (append) {
          flat_exprs <- c(
            ehr_id           = .sql_cast(ehr_expr, alias = "ehr_id"),
            source_data      = .sql_cast(paste0("'", src, "'"), alias = "source_data"),
            src_patient_id   = .sql_cast(paste0("ci.", id_map["src_patient_id"]),
                                         alias = "src_patient_id"),
            src_admission_id = .sql_cast(paste0("ci.", id_map["src_admission_id"]),
                                         alias = "src_admission_id"),
            src_icu_id       = .sql_cast(paste0("ft.", id_map["src_icu_id"]),
                                         alias = "src_icu_id")
          )
          extra_cols <- setdiff(flat_target_cols, names(flat_exprs))
          extra_exprs <- vapply(extra_cols, function(col) {
            expr <- if (col %in% flat_cols) paste0("ft.", col) else "NULL"
            .merge_typed_expr(expr, col, flat_types)
          }, character(1))
          flat_exprs <- c(flat_exprs, extra_exprs)

          return(paste0(
            "SELECT ",
            paste(flat_exprs[flat_target_cols], collapse = ",\n  "), "\n",
            flat_from
          ))
        }

        paste0(
          "SELECT ",
          .sql_cast(ehr_expr, alias = "ehr_id"), ", ",
          .sql_cast(paste0("'", src, "'"), alias = "source_data"), ",\n  ",
          .sql_cast(paste0("ci.", id_map["src_patient_id"]), alias = "src_patient_id"), ",\n  ",
          .sql_cast(paste0("ci.", id_map["src_admission_id"]), alias = "src_admission_id"), ",\n  ",
          .sql_cast(paste0("ft.", id_map["src_icu_id"]), alias = "src_icu_id"), ",\n  ",
          paste(vapply(all_flat, function(col) .merge_typed_expr(
            if (col %in% flat_cols) paste0("ft.", col) else "NULL", col, flat_types),
            character(1)), collapse = ",\n  "), "\n",
          flat_from
        )
      }, character(1))

      flat_sql <- if (append) {
        .insert_sql("flat", flat_target_cols, flat_parts)
      } else {
        paste0(
          "DROP TABLE IF EXISTS ", .merge_target_id("flat"), ";\n",
          "CREATE TABLE ", .merge_target_id("flat"), " AS\n",
          paste(flat_parts, collapse = "\nUNION ALL\n"), ";"
        )
      }
      DBI::dbExecute(con, flat_sql, immediate = TRUE)
      if (append) {
        message("Merged flat table appended: ", target_schema, ".flat")
      } else {
        message("Merged flat table created: ", target_schema, ".flat",
                " (", paste(all_flat, collapse = ", "), ")")
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Merge cohort_info -> rendehr.cohort_info
  # Finds common non-ID columns across all sources.
  # ---------------------------------------------------------------------------
  if (merge_cohort) {
    cohort_target_cols <- if (append) .require_append_target("cohort_info") else NULL

    id_cols_per_src <- lapply(src_names, function(src) {
      get_dataset_config(src)$select_id_cols
    })

    cohort_concept_cols <- lapply(seq_along(src_names), function(i) {
      src <- src_names[i]
      sn  <- src_schema_names(src)
      all_cols <- .cols(sn$raw, "cohort_info")
      setdiff(all_cols, c("ehr_id", "source_data", id_cols_per_src[[i]]))
    })

    common_cohort <- Reduce(intersect, cohort_concept_cols)

    cohort_parts <- vapply(src_names, function(src) {
      sn     <- src_schema_names(src)
      id_map <- .src_id_map(src)
      cohort_cols <- .cols(sn$raw, "cohort_info")
      cohort_from <- paste0("FROM ", sn$raw, ".cohort_info ci")
      col_src <- function(col) col
      if (analysis_level == "admission") {
        cohort_from <- .first_icu_from(cohort_from, sn, id_map, "cohort_info", src)
        col_src <- function(col) {
          if (identical(col, "cohort_index")) "fi.adm_cohort_index" else col
        }
      }
      ehr_expr <- .merge_ehr_id_expr(.ehr_id_grain_parts(c(
        id_map[["src_patient_id"]],
        id_map[["src_admission_id"]],
        id_map[["src_icu_id"]]), analysis_level))
      id_sel <- paste(
        vapply(seq_along(id_map), function(i) {
          .sql_cast(id_map[[i]], alias = names(id_map)[i])
        }, character(1)),
        collapse = ",\n  "
      )
      col_sel <- if (length(common_cohort) > 0) {
        paste0(",\n  ", paste(
          vapply(common_cohort, function(col) {
            .merge_typed_expr(col_src(col), col, flat_types)
          }, character(1)),
          collapse = ",\n  "
        ))
      } else {
        ""
      }

      if (append) {
        cohort_exprs <- c(
          ehr_id           = .sql_cast(ehr_expr, alias = "ehr_id"),
          source_data      = .sql_cast("source_data", alias = "source_data"),
          src_patient_id   = .sql_cast(id_map[["src_patient_id"]],
                                       alias = "src_patient_id"),
          src_admission_id = .sql_cast(id_map[["src_admission_id"]],
                                       alias = "src_admission_id"),
          src_icu_id       = .sql_cast(id_map[["src_icu_id"]],
                                       alias = "src_icu_id")
        )
        extra_cols <- setdiff(cohort_target_cols, names(cohort_exprs))
        extra_exprs <- vapply(extra_cols, function(col) {
          expr <- if (col %in% cohort_cols) col_src(col) else "NULL"
          .merge_typed_expr(expr, col, flat_types)
        }, character(1))
        cohort_exprs <- c(cohort_exprs, extra_exprs)

        return(paste0(
          "SELECT ",
          paste(cohort_exprs[cohort_target_cols], collapse = ",\n  "), "\n",
          cohort_from
        ))
      }

      paste0(
        "SELECT ",
        .sql_cast(ehr_expr, alias = "ehr_id"), ", ",
        .sql_cast("source_data", alias = "source_data"), ",\n  ",
        id_sel, col_sel, "\n",
        cohort_from
      )
    }, character(1))

    cohort_sql <- if (append) {
      .insert_sql("cohort_info", cohort_target_cols, cohort_parts)
    } else {
      paste0(
        "DROP TABLE IF EXISTS ", .merge_target_id("cohort_info"), ";\n",
        "CREATE TABLE ", .merge_target_id("cohort_info"), " AS\n",
        paste(cohort_parts, collapse = "\nUNION ALL\n"), ";"
      )
    }
    DBI::dbExecute(con, cohort_sql, immediate = TRUE)
    if (append) {
      message("Merged cohort_info appended: ", target_schema, ".cohort_info")
    } else {
      message("Merged cohort_info created: ", target_schema, ".cohort_info")
    }

    # Internal membership-grain metadata (one row per source), read by ICU-level
    # accessors so they never filter on admission-grain cohort_index. A source
    # without it (cohort_info built before cohort_index) gets no row, and ICU-level
    # membership filtering then refuses that source until it is rebuilt.
    meta_parts <- vapply(src_names, function(src) {
      sn <- src_schema_names(src)
      if (!.table_exists(sn$raw, "cohort_info_meta")) return(NA_character_)
      paste0("SELECT CAST(source_data AS text) AS source_data, ",
             "CAST(analysis_level AS text) AS analysis_level FROM ",
             sn$raw, ".cohort_info_meta")
    }, character(1))
    if (anyNA(meta_parts)) {
      message("  No cohort_info_meta for: ",
              paste(src_names[is.na(meta_parts)], collapse = ", "),
              " - rebuild their cohort_info before ICU-level membership filtering.")
    }
    meta_parts <- meta_parts[!is.na(meta_parts)]
    meta_id <- .merge_target_id("cohort_info_meta")
    meta_sql <- paste0(
      if (append) paste0(
        "CREATE TABLE IF NOT EXISTS ", meta_id,
        " (source_data text, analysis_level text);\n",
        .source_delete_sql("cohort_info_meta"), "\n")
      else paste0("DROP TABLE IF EXISTS ", meta_id, ";\nCREATE TABLE ", meta_id,
                  " (source_data text, analysis_level text);\n"),
      if (length(meta_parts)) paste0(
        "INSERT INTO ", meta_id, " (source_data, analysis_level)\n",
        paste(meta_parts, collapse = "\nUNION ALL\n"), ";"))
    DBI::dbExecute(con, meta_sql, immediate = TRUE)
  }

  DBI::dbCommit(con)
  tx_open <- FALSE

  invisible(TRUE)
}

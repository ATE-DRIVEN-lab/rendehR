# Atomic concept SQL builders for the rendehR pipeline.
#
# These functions generate CREATE TABLE ... AS SELECT SQL for each concept
# in rendehr_concepts_config.csv / rendehr_duration_concepts_config.csv, then execute
# them against Postgres.

# ---------------------------------------------------------------------------
# Dataset configs (schema + ID columns per source)
# ---------------------------------------------------------------------------
#
# Per-family config (select_id_cols, time_origin, icu_id_type, id_cols,
# time_diff strategy, cohort key/seed tables) lives in the source registry,
# seeded in .register_builtin_sources() (R/env_config.R) and read via
# .source_entry(). The getters below reconstruct the config list callers expect.
#
# icu_id_type: SQL type used when emitting the ICU/unit-stay id as NULL for
# hospital-grain (admission-spine) concepts. Keep it matching the native id type
# so the ts_tab UNION stays type-consistent; a future text-id source would set
# this to "text".

#' Get source dataset configuration
#'
#' Returns the configuration list for a supported source, resolved from the
#' source registry. This is most useful when checking source-specific
#' identifier columns or schema defaults. The `schema` is the raw cohort schema
#' for the exact source name (e.g. `"mimic_iv_demo_coh"`), and `row_limit` is
#' the demo cap when `dataset` is a demo variant, else `NULL`.
#'
#' @param dataset Source name, such as `"mimic_iv_demo"`, `"mimic_iii_demo"`,
#'   or `"eicu_demo"`.
#' @return A named list containing source configuration values.
#' @examples
#' str(get_dataset_config("eicu"))
#' @export
get_dataset_config <- function(dataset) {
  entry <- .source_entry(dataset)
  if (is.null(entry)) stop("Unknown dataset: ", dataset)
  is_demo <- grepl("(^|_)demo($|_)", dataset)
  list(
    select_id_cols = entry$select_id_cols,
    time_origin    = entry$time_origin,
    icu_id_type    = entry$icu_id_type,
    row_limit      = if (is_demo) entry$demo_row_limit else NULL,
    schema         = paste0(dataset, "_coh")
  )
}

#' Get identifier columns for a source table
#'
#' Returns the source-specific identifier columns used to join a raw table to
#' the appropriate rendehR window table.
#'
#' @param dataset Source name, such as `"mimic_iv_demo"`, `"mimic_iii_demo"`,
#'   or `"eicu_demo"`.
#' @param table_name Raw source table name.
#' @return Character vector of identifier column names.
#' @examples
#' get_id_cols("mimic_iii", "chartevents")
#' get_id_cols("mimic_iii", "prescriptions")
#' @export
get_id_cols <- function(dataset, table_name) {
  entry <- .source_entry(dataset)
  if (is.null(entry)) stop("Unknown dataset: ", dataset)
  cfg <- entry$id_cols
  if (is.null(cfg$tables_with_icu)) return(cfg$default)
  if (table_name %in% cfg$tables_with_icu) return(cfg$icu)
  cfg$default
}

# ---------------------------------------------------------------------------
# SQL clause builders
# ---------------------------------------------------------------------------

# All time-since columns are emitted as whole-minute `numeric` values so every
# source shares the same granularity (eICU is natively integer minutes) and the
# `double precision` float noise from EXTRACT(EPOCH ...)/60 is removed. Kept as
# numeric (not int) to stay union-compatible with the NULL::numeric placeholders
# emitted for hospital-grain rows.
build_time_diff <- function(dataset, index_var, time_origin) {
  entry <- .source_entry(dataset)
  if (is.null(entry)) stop("Unknown dataset: ", dataset)
  if (entry$time_diff == "offset_minutes") {
    glue::glue("(src.{index_var} + {time_origin})::numeric")
  } else {
    glue::glue("ROUND((EXTRACT(EPOCH FROM (src.{index_var} - {time_origin})) / 60.0)::numeric)")
  }
}

build_icu_time_diff <- function(dataset, index_var) {
  entry <- .source_entry(dataset)
  if (is.null(entry)) stop("Unknown dataset: ", dataset)
  if (entry$time_diff == "offset_minutes") {
    glue::glue("src.{index_var}::numeric")
  } else {
    glue::glue("ROUND((EXTRACT(EPOCH FROM (src.{index_var} - cw.icu_intime)) / 60.0)::numeric)")
  }
}

build_match_clause <- function(itm_class, sub_var, match_value, dict,
                               engine = "postgres") {
  # Only col_itm reads a whole column; for any other class an empty
  # match_value would select every row of the source table.
  if (is.na(match_value) || !nzchar(trimws(match_value))) {
    if (identical(itm_class, "col_itm")) return("TRUE")
    stop("Empty match_value for itm_class '", itm_class, "': only col_itm may ",
         "leave it empty (see validate_concept_config()).", call. = FALSE)
  }

  if (itm_class == "itm_itm") {
    ids <- gsub("\\|", ", ", match_value)
    return(glue::glue("src.{sub_var} IN ({ids})"))

  } else if (itm_class == "rgx_itm") {
    # match_value is a literal (pipe-delimited) list of exact source values,
    # not a regex pattern -- upstream (ricu's sel_itm, BlendedICU's dict
    # replace) treat these as exact-value membership, not substring search.
    # Using IN() here instead of ~ avoids both regex-metacharacter escaping
    # (e.g. parens in drug names) and substring collisions (e.g. "ph" vs.
    # "phosphate" under unanchored `~`).
    target <- if (!is.na(dict) && nzchar(dict)) "LOWER(TRIM(dict.label))"
              else glue::glue("LOWER(TRIM(src.{sub_var}))")
    values <- tolower(trimws(strsplit(match_value, "|", fixed = TRUE)[[1]]))
    values <- gsub("'", "''", values, fixed = TRUE)
    ids <- paste(sprintf("'%s'", values), collapse = ", ")
    return(glue::glue("{target} IN ({ids})"))

  } else if (itm_class == "rgx_exp_itm") {
    target <- if (!is.na(dict) && nzchar(dict)) "LOWER(dict.label)"
              else glue::glue("LOWER(src.{sub_var})")
    escaped_value <- gsub("'", "''", match_value, fixed = TRUE)
    # `target` is already lower-cased above but match_value is not, so the
    # comparison still needs to be case-insensitive. Postgres's `~*` operator
    # has no DuckDB equivalent; DuckDB's regexp_matches() takes the case-
    # insensitive flag as its 3rd argument instead and returns BOOLEAN either
    # way (unlike Postgres's own regexp_matches(), which returns a capture
    # array and can't be dropped in here).
    if (identical(engine, "duckdb"))
      return(glue::glue("regexp_matches({target}, '{escaped_value}', 'i')"))
    return(glue::glue("{target} ~* '{escaped_value}'"))

  } else if (itm_class == "col_itm") {
    return("TRUE")

  } else {
    stop("Unknown class: ", itm_class)
  }
}

build_id_select <- function(select_id_cols) {
  paste(glue::glue("cw.{select_id_cols}"), collapse = ",\n    ")
}

build_join_condition <- function(join_id_cols) {
  if (length(join_id_cols) == 0) stop("No join keys supplied")
  paste(glue::glue("cw.{join_id_cols} = src.{join_id_cols}"),
        collapse = "\n  AND ")
}

# ---------------------------------------------------------------------------
# Unit conversion
# ---------------------------------------------------------------------------

# Loads rendehr_concepts_unit_conversion_map.csv from package extdata.
# Called only when conversion == "map"; not cached (file is small).
.load_unit_conversion_map <- function() {
  path <- system.file("extdata", "rendehr_concepts_unit_conversion_map.csv",
                      package = "rendehR")
  if (!nzchar(path))
    stop("rendehr_concepts_unit_conversion_map.csv not found in rendehR package extdata")
  map <- utils::read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  # Units are matched case-insensitively, so two rows that differ only in
  # the case of source_unit are the same key.
  dup <- duplicated(paste(map$concept_name, tolower(map$source_unit)))
  if (any(dup))
    stop("Duplicate (concept_name, source_unit) keys in ",
         "rendehr_concepts_unit_conversion_map.csv: ",
         paste(unique(paste(map$concept_name, map$source_unit)[dup]), collapse = ", "),
         call. = FALSE)
  map
}

# TRUE for a unit-map factor left empty, which marks the unit as excluded.
.unit_map_excluded <- function(factor) {
  is.na(factor) | !nzchar(trimws(as.character(factor)))
}

# Fahrenheit labels accepted by f_to_c, lower-cased and trimmed like the source
# unit. "?f" is how MIMIC-III stores the degree sign.
.fahrenheit_units <- c("f", "\u00b0f", "?f", "deg. f", "deg.f", "deg f", "degf",
                       "degrees f", "fahrenheit")

# Ordinal value of each urine dipstick grade, keyed by the upper-cased,
# trimmed source text.
.dipstick_grades <- list(
  "0" = c("NEG", "NEGATIVE"),
  "1" = c("SM", "SMALL", "1+"),
  "2" = c("MOD", "MODERATE", "2+"),
  "3" = c("LG", "LARGE", "3+")
)

#' Build SQL for a concept's unit conversion
#'
#' Returns a SQL expression (string) that casts the source value to NUMERIC
#' and optionally applies a unit conversion when the source unit does not
#' match the target unit.
#'
#' The value is parsed as a number when it is plain or comma-grouped digits
#' with an optional sign, decimal point, exponent (\code{1e3}) or trailing
#' \code{\%}. Anything else, including censored values such as \code{<0.5},
#' becomes \code{NULL}; the original text stays in \code{value_raw}.
#'
#' @param conversion Conversion type string from the concept config.
#'   Supported formats:
#'   \itemize{
#'     \item \code{"none"} - no conversion, cast to NUMERIC as-is.
#'     \item \code{"*N"} or \code{"/N"} - multiply or divide every value by
#'       N. Only for items without a unit column; with a unit column this is
#'       an error, because unrecognised units would be converted too. Use
#'       \code{"map"} instead.
#'     \item \code{"f_to_c"} - Fahrenheit to Celsius conversion. With a unit
#'       column, only recognised Fahrenheit labels (\code{F}, \code{Deg. F},
#'       ...) are converted; values already in \code{target_unit} or with no
#'       unit are kept, and any other unit becomes \code{NULL}.
#'     \item \code{"cond_f_to_c"} - for items without a unit: values above 70
#'       are taken as Fahrenheit and converted.
#'     \item \code{"fio2_fraction"} - for FiO2 items that mix fractions and
#'       percents: values in (0, 1] are multiplied by 100, values in (1, 20)
#'       become \code{NULL}, and all others are kept as percents.
#'     \item \code{"ca_ionized_by_value"} - for ionized calcium reported as
#'       mmol/L or mg/dL under one unit label (eICU): values up to 2 are taken
#'       as mmol/L, values of 3.5 or more as mg/dL and multiplied by 0.2495,
#'       and values in between become \code{NULL}. The unit column is ignored.
#'     \item \code{"dipstick_grade"} - for qualitative urine dipstick
#'       results: \code{NEG} is 0, \code{SM}/\code{1+} is 1, \code{MOD}/\code{2+}
#'       is 2 and \code{LG}/\code{3+} is 3 (full words are accepted too). The
#'       unit column is ignored and any other text becomes \code{NULL}.
#'     \item \code{"exposure"} - exposure-only concepts (antibiotics): every
#'       row's value is 1, whatever amount or unit the source recorded; the
#'       original text stays in \code{value_raw} and \code{unit_raw}.
#'     \item \code{"map"} - multi-unit lookup. Factors are read from
#'       \code{rendehr_concepts_unit_conversion_map.csv}, keyed by \code{concept_name}
#'       and \code{source_unit}. The target unit is always taken from the
#'       \code{unit} column of the concept config (not the map file).
#'       Any source unit not listed in the map that also does not match
#'       the target unit is set to \code{NULL}, and so is a row without a
#'       unit. A map row with an empty \code{factor} records a unit that is
#'       excluded on purpose (for example an absolute count reported under a
#'       percentage concept); its values are \code{NULL} too. Use this when a source
#'       column contains more than one unit across rows (e.g. CRP in
#'       mimic_iv stored as both mg/dL and ug/mL).
#'   }
#' @param val_var Source column name holding the raw value.
#' @param unit_var Source column name holding the unit label (may be NA).
#' @param target_unit Target unit string (from the \code{unit} column in the
#'   concept config); used as the reference for conditional conversion.
#' @param concept_name Concept name. Required when `conversion = "map"` so the
#'   unit conversion map can be filtered to the correct concept.
#' @return SQL expression string.
#' @examples
#' cat(concept_unit_conversion_func("f_to_c", "valuenum", "valueuom", "C", "temperature"))
#' @export
concept_unit_conversion_func <- function(conversion, val_var, unit_var,
                                         target_unit,
                                         concept_name = NA_character_) {
  v          <- paste0("src.", val_var)
  is_percent <- !is.na(conversion) && conversion %in% c("percent", "pct")
  # Exposure-only concepts record that the drug was given, not how much: every
  # row is 1, whatever amount or unit the source wrote.
  if (!is.na(conversion) && conversion == "exposure")
    return("CAST(1 AS NUMERIC(38,9))")

  raw_txt <- sprintf("TRIM(%s::TEXT)", v)
  clean_numeric_txt <- sprintf("REPLACE(REPLACE(%s, '%%', ''), ',', '')", raw_txt)
  # NUMERIC(38,9): explicit precision/scale, not bare NUMERIC. DuckDB defaults
  # an unqualified NUMERIC/DECIMAL cast to (18,3) - 3 decimal places - which
  # would truncate the raw value before any unit conversion (*N, /N, f_to_c,
  # map) is even applied, silently compounding rounding error. Postgres's
  # bare NUMERIC is arbitrary-precision so this only changes DuckDB behavior.
  # The final 3-decimal rounding happens once, at the very end, in
  # build_atomic_concept_unified_sql()'s wrap_with_schema().
  # Accepted: optional sign, plain or comma-grouped digits, a leading or
  # trailing decimal point, scientific notation and a trailing %. Censored
  # values (<0.5, >100) stay NULL. Magnitudes of 1e28 or more would overflow
  # NUMERIC(38,9) and abort the whole query, so they are NULL as well; the
  # nested CASE keeps the DOUBLE cast away from non-numeric text.
  base <- sprintf(
    "CASE
     WHEN %s ~ '^[+-]?((([0-9]+)|([0-9]{1,3}(,[0-9]{3})+))(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?%%?$'
     THEN CASE
       WHEN ABS(CAST(%s AS DOUBLE PRECISION)) < 1e28
       THEN CAST(%s AS NUMERIC(38,9))
       ELSE NULL
     END
     ELSE NULL
   END",
    raw_txt, clean_numeric_txt, clean_numeric_txt
  )

  no_unit_col <- is.na(unit_var) || !nzchar(unit_var)

  if (no_unit_col) {
    if (is.na(conversion) || conversion == "" ||
        conversion == "none" || is_percent)
      return(base)
  } else {
    u <- paste0("src.", unit_var)
    # Unit strings are matched case- and padding-insensitively, like the
    # match values in build_match_clause(): a space-padded source unit must
    # not fall through to ELSE NULL.
    u_key <- sprintf("LOWER(TRIM(CAST(%s AS TEXT)))", u)
  }
  unit_lit <- function(x) sprintf("LOWER(TRIM('%s'))", gsub("'", "''", x, fixed = TRUE))

  # With no unit column there is nothing to condition on, so f_to_c, *N and
  # /N apply unconditionally (paste0("src.", NA) would name a column "src.NA").

  if (is.na(conversion) || conversion == "" || conversion == "none")
    return(base)
  if (is_percent)
    return(base)
  if (conversion == "f_to_c") {
    f_expr <- sprintf("(%s - 32) * 5.0/9.0", base)
    if (no_unit_col) return(f_expr)
    return(sprintf(
      "CASE
         WHEN %s IS NULL THEN %s
         WHEN %s = %s THEN %s
         WHEN %s IN (%s) THEN %s
         ELSE NULL
       END",
      u, base, u_key, unit_lit(target_unit), base,
      u_key, paste(sprintf("'%s'", .fahrenheit_units), collapse = ", "), f_expr
    ))
  }
  if (conversion == "dipstick_grade") {
    # The unit label of a dipstick result (mg/dL, EU/dL) does not change what
    # the grade means, so it is not consulted.
    grade_txt <- sprintf("UPPER(%s)", raw_txt)
    branches <- vapply(names(.dipstick_grades), function(g) sprintf(
      "WHEN %s IN (%s) THEN CAST(%s AS NUMERIC(38,9))", grade_txt,
      paste(sprintf("'%s'", .dipstick_grades[[g]]), collapse = ", "), g), character(1L))
    return(sprintf("CASE\n         %s\n         ELSE NULL\n       END",
                   paste(branches, collapse = "\n         ")))
  }
  if (!no_unit_col && grepl("^[*/]\\s*[0-9.]+$", conversion))
    stop("Conversion '", conversion, "' for concept '", concept_name,
         "' would apply to every source unit other than '", target_unit,
         "', including unrecognised ones. List the source units and factors ",
         "in rendehr_concepts_unit_conversion_map.csv and use conversion = 'map'.",
         call. = FALSE)
  if (conversion == "cond_f_to_c") {
    # Some eicu columns (e.g. vitalPeriodic.temperature) carry no unit_var at
    # all, so a unit-keyed conversion (f_to_c's conditional_wrap, map) can't
    # apply. Threshold on the raw value instead: no real body temperature
    # exceeds 50C or falls below 50F, so >70 unambiguously means Fahrenheit
    # (matches MIT's vitals_first_day.sql convention: >70 => F, <50 => C).
    return(sprintf(
      "CASE
         WHEN %s IS NULL THEN NULL
         WHEN %s > 70 THEN (%s - 32) * 5.0/9.0
         ELSE %s
       END",
      base, base, base, base
    ))
  }
  if (conversion == "fio2_fraction") {
    # Some fio2 sources mix fraction-form (0.21-1.0) and percent-form
    # (21-100) values under the same itemid/column, with no unit_var to
    # distinguish them -- threshold on the raw value instead. Every value in
    # (0, 1] is a fraction and is scaled, so the column holds percents only;
    # implausibly low results are left to the concept's range check. Values
    # between 1 and 20 are implausible in either representation (FiO2 can't
    # be 1-19%, nor a fraction above 1) and are nulled out rather than guessed.
    return(sprintf(
      "CASE
         WHEN %s IS NULL THEN NULL
         WHEN %s > 0 AND %s <= 1.0 THEN %s * 100
         WHEN %s > 1 AND %s < 20 THEN NULL
         ELSE %s
       END",
      base, base, base, base, base, base, base
    ))
  }
  if (conversion == "ca_ionized_by_value") {
    # eICU labels every ionized calcium "mg/dL", but some hospitals report
    # mmol/L under that label (raw values ~1.1 next to ~4.5). Living values are
    # about 0.5-2 mmol/L or 2-8 mg/dL, so the raw value tells the unit apart;
    # 2-3.5 fits neither scale cleanly and is not guessed.
    return(sprintf(
      "CASE
         WHEN %s IS NULL THEN NULL
         WHEN %s <= 2 THEN %s
         WHEN %s >= 3.5 THEN %s * 0.2495
         ELSE NULL
       END",
      base, base, base, base, base
    ))
  }
  if (grepl("^\\*\\s*[0-9.]+$", conversion)) {
    num <- sub("^\\*\\s*", "", conversion)
    return(sprintf("%s * %s", base, num))
  }
  if (grepl("^/\\s*[0-9.]+$", conversion)) {
    num <- sub("^/\\s*", "", conversion)
    return(sprintf("%s / %s", base, num))
  }

  if (conversion == "map") {
    if (is.na(concept_name) || !nzchar(concept_name))
      stop("conversion='map' requires concept_name to be supplied")
    if (no_unit_col)
      stop("conversion='map' requires a unit_var (source unit column) for ",
           "concept '", concept_name, "'; none was configured")
    map_df  <- .load_unit_conversion_map()
    entries <- map_df[map_df$concept_name == concept_name, , drop = FALSE]
    if (nrow(entries) == 0)
      stop("conversion='map': no entries in rendehr_concepts_unit_conversion_map.csv for concept '",
           concept_name, "'")
    branches <- vapply(seq_len(nrow(entries)), function(i) {
      src    <- entries$source_unit[i]
      if (.unit_map_excluded(entries$factor[i]))
        return(sprintf("WHEN %s = %s THEN NULL", u_key, unit_lit(src)))
      factor <- suppressWarnings(as.numeric(entries$factor[i]))
      if (is.na(factor))
        stop("Non-numeric factor in rendehr_concepts_unit_conversion_map.csv for concept '",
             concept_name, "', source_unit '", entries$source_unit[i], "'")
      expr <- sprintf("%s * %s", base, factor)
      sprintf("WHEN %s = %s THEN %s", u_key, unit_lit(src), expr)
    }, character(1L))
    return(sprintf(
      "CASE\n         WHEN %s IS NULL                    THEN NULL\n         WHEN %s = %s THEN %s\n         %s\n         ELSE NULL\n       END",
      u, u_key, unit_lit(target_unit), base,
      paste(branches, collapse = "\n         ")
    ))
  }

  stop("Unknown conversion: ", conversion)
}


# ---------------------------------------------------------------------------
# Per-item SELECT builder
# ---------------------------------------------------------------------------

build_select_unified <- function(table_name, sub_var, match_value,
                                 val_var, unit_var, index_var,
                                 end_var = NULL, conversion, source,
                                 dict, unit, itm_class, raw_schema,
                                 concept_name = NA_character_,
                                 typ = "ts",
                                 time_precision = "datetime",
                                 required_dedup = FALSE,
                                 engine = "postgres",
                                 dedup_cols = FALSE) {
  cfg            <- get_dataset_config(source)
  raw_schema     <- raw_schema %||% cfg$schema
  select_id_cols <- cfg$select_id_cols
  join_id_cols   <- get_id_cols(source, table_name)
  time_origin    <- cfg$time_origin

  join_cond  <- build_join_condition(join_id_cols)
  conv_sql   <- concept_unit_conversion_func(conversion, val_var, unit_var, unit, concept_name)
  # A concept without a unit (exposures, flags) stores NULL, not the text 'NA'.
  unit_sql   <- if (length(unit) != 1L || is.na(unit) || trimws(unit) %in% c("", "NA")) "NULL::text"
                else as.character(DBI::dbQuoteString(DBI::ANSI(), unit))

  # Grain metadata, derived from which IDs the source table is joined on.
  # The ICU/unit-stay column is the last entry of select_id_cols by convention
  # (e.g. stay_id, icustay_id, patientunitstayid). A table joined on it is
  # ICU-grain; otherwise it is hospital-grain (joined on subject/hadm only).
  icu_id_col      <- utils::tail(select_id_cols, 1)
  is_icu_grain    <- icu_id_col %in% join_id_cols
  event_level     <- if (is_icu_grain) "icu" else "hospital"
  native_id_level <- if (is_icu_grain) "icu_stay" else "hadm"
  # time_precision: per-concept value from the config CSV (datetime/date/none).
  # Missing/blank falls back to datetime so older configs without the column
  # keep working unchanged.
  time_precision  <- if (is.na(time_precision) || !nzchar(time_precision))
                       "datetime" else time_precision

  # Native-grain spine. ICU-grain tables fan across ICU stays (icu_windows).
  # Hospital-grain tables are stored once per admission (admission_windows),
  # with the ICU stay id and ICU-relative time left NULL - no fan-out. eICU is
  # always ICU-grain, so only MIMIC sources take the hospital branch (and they
  # provide admission_windows).
  if (is_icu_grain) {
    spine_table <- "icu_windows"
    id_select   <- build_id_select(select_id_cols)
  } else {
    spine_table <- "admission_windows"
    hosp_cols   <- setdiff(select_id_cols, icu_id_col)
    icu_id_type <- cfg$icu_id_type %||% "bigint"
    id_select   <- paste0(
      paste(glue::glue("cw.{hosp_cols}"), collapse = ",\n    "),
      glue::glue(",\n    NULL::{icu_id_type} AS {icu_id_col}")
    )
  }

  # event_id: deterministic per-event surrogate so the same canonical event is
  # never double-counted even after a derived ICU view fans it across stays
  # (COUNT(DISTINCT event_id)). Hash of source table + join ids + time + item +
  # value; concat_ws skips NULLs.
  evt_components <- c(
    glue::glue("src.{join_id_cols}"),
    glue::glue("src.{index_var}"),
    if (!is.na(sub_var) && nzchar(sub_var)) glue::glue("src.{sub_var}") else NULL,
    if (!is.na(val_var) && nzchar(val_var)) glue::glue("src.{val_var}") else NULL
  )
  event_id_expr <- glue::glue(
    "md5(concat_ws('|', '{table_name}', {paste(evt_components, collapse = ', ')}))"
  )

  dict_join <- if (!is.na(dict) && nzchar(dict))
    glue::glue("LEFT JOIN {raw_schema}.{dict} dict ON src.{sub_var} = dict.{sub_var}")
  else ""

  match_clause <- build_match_clause(itm_class, sub_var, match_value, dict, engine)
  # A col_itm concept is one column of a wide table (eICU vitalperiodic), so a
  # row is an event of the concept only when that column is filled.
  if (identical(itm_class, "col_itm") && !is.na(val_var) && nzchar(val_var))
    match_clause <- glue::glue("({match_clause}) AND NULLIF(TRIM(CAST(src.{val_var} AS TEXT)), '') IS NOT NULL")

  has_itemid <- itm_class %in% c("itm_itm", "rgx_itm", "rgx_exp_itm")
  has_label  <- has_itemid && !is.na(dict) && nzchar(dict)

  source_itemid_col <- if (has_itemid) glue::glue("src.{sub_var} AS source_itemid,")
                       else "NULL AS source_itemid,"
  unit_col          <- if (!is.na(unit_var) && nzchar(unit_var))
                         glue::glue("src.{unit_var} AS unit_raw,")
                       else "NULL AS unit_raw,"
  label_col         <- if (has_label) "dict.label AS source_label,"
                       else "NULL AS source_label,"

  if (typ == "ts") {
    time_diff     <- build_time_diff(source, index_var, time_origin)
    icu_time_diff <- if (is_icu_grain) build_icu_time_diff(source, index_var) else "NULL::numeric"
    icu_los_expr  <- if (is_icu_grain) "cw.icu_los_min" else "NULL::numeric"
    time_block    <- glue::glue(
      "src.{index_var},\n",
      "    {time_diff} AS time_since_hadm_min,\n",
      "    {icu_time_diff} AS time_since_icu_min,\n",
      "    {icu_los_expr} AS icu_los_min"
    )

  } else if (typ == "dur") {
    if (is.null(end_var) || !nzchar(end_var))
      stop("end_var must be provided for duration concepts")
    start_diff     <- build_time_diff(source, index_var, time_origin)
    end_diff       <- build_time_diff(source, end_var,   time_origin)
    icu_start_diff <- if (is_icu_grain) build_icu_time_diff(source, index_var) else "NULL::numeric"
    icu_los_expr   <- if (is_icu_grain) "cw.icu_los_min" else "NULL::numeric"
    time_block <- glue::glue(
      "src.{index_var} AS start_time,\n    src.{end_var} AS end_time,\n",
      "    {start_diff} AS time_since_hadm_min,\n",
      "    {icu_start_diff} AS time_since_icu_min,\n",
      "    {icu_los_expr} AS icu_los_min,\n",
      "    {end_diff}   AS end_offset_min,\n",
      "    ({end_diff} - {start_diff}) AS duration_min"
    )
  } else {
    stop("Unknown typ: ", typ)
  }

  # Internal columns used only by chart-copy deduplication: the source's
  # original timestamp (matching never uses the whole-minute time columns) and
  # the canonical unit of the stored value. Both are dropped before the
  # concept table is written.
  dedup_select <- if (isTRUE(dedup_cols)) {
    unit_key <- .dedup_unit_key_sql(conversion, unit_var, unit, concept_name)
    glue::glue(",\n    src.{index_var} AS event_time_raw,\n    {unit_key} AS unit_key")
  } else ""

  glue::glue("
SELECT
    {id_select},
    {time_block},
    {conv_sql} AS value,
    {unit_sql} AS unit,
    src.{val_var} AS value_raw,
    {unit_col}
    {source_itemid_col}
    {label_col}
    {event_id_expr} AS event_id,
    '{event_level}' AS event_level,
    '{native_id_level}' AS native_id_level,
    '{time_precision}' AS time_precision,
    {tolower(isTRUE(required_dedup))} AS dedup_candidate,
    '{table_name}' AS source_table,
    '{raw_schema}' AS source_dataset{dedup_select}
FROM {raw_schema}.{spine_table} cw
INNER JOIN {raw_schema}.{table_name} src
  ON {join_cond}
{dict_join}
WHERE {match_clause}
")
}

# ---------------------------------------------------------------------------
# Concept table builder (UNION ALL + CREATE TABLE)
# ---------------------------------------------------------------------------

build_atomic_concept_unified_sql <- function(df, raw_schema, concepts_schema,
                                             src_name, engine = "postgres",
                                             rm_duplicates = TRUE, dedup = NULL) {
  wrap_with_schema <- function(sql, typ, extra = "") {
    cfg       <- get_dataset_config(src_name)
    id_cols   <- cfg$select_id_cols
    id_select <- paste(id_cols, collapse = ",\n        ")

    time_cols <- if (typ == "ts") {
      "time_since_hadm_min,\n          time_since_icu_min,\n          icu_los_min,"
    } else {
      paste0(
        "start_time,\n          end_time,\n",
        "          time_since_hadm_min,\n",
        "          time_since_icu_min,\n",
        "          icu_los_min,\n",
        "          end_offset_min,\n",
        "          duration_min,"
      )
    }

    glue::glue("
      SELECT
          {id_select},
          {time_cols}
          CAST(ROUND(CAST(value AS NUMERIC(38,9)), 3) AS NUMERIC(18,3)) AS value,
          CAST(unit AS TEXT)           AS unit,
          CAST(value_raw AS TEXT)      AS value_raw,
          CAST(unit_raw AS TEXT)       AS unit_raw,
          TRIM(CAST(source_itemid AS TEXT)) AS source_itemid,
          TRIM(CAST(source_label AS TEXT))  AS source_label,
          CAST(event_id AS TEXT)         AS event_id,
          CAST(event_level AS TEXT)      AS event_level,
          CAST(native_id_level AS TEXT)  AS native_id_level,
          CAST(time_precision AS TEXT)   AS time_precision,
          CAST(dedup_candidate AS BOOLEAN) AS dedup_candidate,
          CAST(source_table AS TEXT)   AS source_table,
          CAST(source_dataset AS TEXT) AS source_dataset{extra}
      FROM (
          {sql}
      ) t
    ")
  }

  # Older configs may lack time_precision / required_dedup; row_selects()
  # defaults them so dplyr::select succeeds. NA (blank config cell) means
  # "not flagged", same as FALSE.
  row_selects <- function(df, dedup_cols = FALSE, reference = FALSE) {
    df <- .ehr_normalize_concept_cols(df)
    if (!"time_precision" %in% names(df)) df$time_precision <- "datetime"
    if (!"required_dedup" %in% names(df)) df$required_dedup <- FALSE
    df$required_dedup <- !is.na(df$required_dedup) & as.logical(df$required_dedup)
    extra <- if (dedup_cols) paste0(",\n          event_time_raw,\n          unit_key,\n          ",
                                    if (reference) "TRUE" else "FALSE", " AS is_dedup_reference") else ""
    df_small <- dplyr::select(df,
      table_name, class, sub_var, match_value,
      val_var, unit_var, index_var, end_var,
      conversion, dict, unit, typ, time_precision, concept_name,
      required_dedup
    )

    selects <- purrr::pmap_chr(
      df_small,
      function(table_name, class, sub_var, match_value,
               val_var, unit_var, index_var, end_var,
               conversion, dict, unit, typ, time_precision, concept_name,
               required_dedup) {
        raw_sql <- build_select_unified(
          table_name     = table_name,
          sub_var        = sub_var,
          match_value    = match_value,
          val_var        = val_var,
          unit_var       = unit_var,
          index_var      = index_var,
          end_var        = end_var,
          conversion     = conversion,
          dict           = dict,
          unit           = unit,
          itm_class      = class,
          source         = src_name,
          raw_schema     = raw_schema,
          typ            = typ,
          time_precision = time_precision,
          concept_name   = concept_name,
          required_dedup  = required_dedup,
          engine         = engine,
          dedup_cols     = dedup_cols
        )
        wrap_with_schema(raw_sql, typ, extra)
      }
    )
  }

  cfg      <- get_dataset_config(src_name)
  is_ts    <- !identical(df$typ[1], "dur")
  time_cols <- if (is_ts)
    c("time_since_hadm_min", "time_since_icu_min", "icu_los_min") else
    c("start_time", "end_time", "time_since_hadm_min", "time_since_icu_min",
      "icu_los_min", "end_offset_min", "duration_min")
  out_cols <- c(cfg$select_id_cols, time_cols, "value", "unit", "value_raw",
                "unit_raw", "source_itemid", "source_label", "event_id",
                "event_level", "native_id_level", "time_precision",
                "dedup_candidate", "source_table", "source_dataset")
  create <- function(body) glue::glue("
  DROP TABLE IF EXISTS {concepts_schema}.{df$concept_name[1]};
  CREATE TABLE {concepts_schema}.{df$concept_name[1]} AS
  {body};
  ")
  # rm_duplicates also keeps one of the chart rows of a ts concept that agree
  # on ids, minute, value and unit (e.g. two CareVue items charting the same
  # glucose). Drug tables are left alone: two drugs of one concept can share a
  # dose and a start time.
  # Only chartevents rows can be exact copies, so a concept that reads no
  # chartevents skips this step, and the window runs over the chart rows only.
  exact <- function(body, ctes = NULL) {
    with_ctes <- function(b) if (is.null(ctes)) b else paste0("WITH ", ctes, "\n", b)
    if (!isTRUE(rm_duplicates) || !is_ts || !any(df$table_name == "chartevents"))
      return(with_ctes(body))
    key <- paste(c(cfg$select_id_cols, "time_since_hadm_min", "value", "unit"),
                 collapse = ", ")
    cols <- paste(out_cols, collapse = ", ")
    glue::glue("
  WITH {if (is.null(ctes)) '' else paste0(ctes, ',\n  ')}exact_u AS (
  {body}
  )
  SELECT {cols} FROM exact_u
  WHERE NOT (source_table = 'chartevents' AND value IS NOT NULL)
  UNION ALL
  SELECT {cols} FROM (
  SELECT e.*, ROW_NUMBER() OVER (PARTITION BY {key}
                                 ORDER BY LENGTH(e.source_itemid), e.source_itemid, e.event_id) AS exact_rn
  FROM exact_u e
  WHERE e.source_table = 'chartevents' AND e.value IS NOT NULL
  ) x
  WHERE x.exact_rn = 1")
  }

  apply_dedup <- isTRUE(rm_duplicates) && !is.null(dedup) && NROW(dedup$pairs) > 0L
  if (!apply_dedup) {
    selects   <- row_selects(df)
    union_sql <- paste(selects, collapse = "\nUNION ALL\n")
    return(create(exact(union_sql)))
  }

  # rm_duplicates: the same rows plus reference-only rows of other concepts,
  # then chart copies matching an allowed reference lab row are filtered out.
  selects <- c(row_selects(df, dedup_cols = TRUE),
               if (NROW(dedup$reference_rows))
                 row_selects(dedup$reference_rows, dedup_cols = TRUE, reference = TRUE))
  union_sql <- paste(selects, collapse = "\nUNION ALL\n")
  # MIMIC copies pair on the admission (chart and lab times are timestamps);
  # eICU offsets are minutes from the unit stay, so they pair on the stay.
  hosp_ids <- if (identical(src_family(src_name), "eicu")) cfg$select_id_cols else
    setdiff(cfg$select_id_cols, utils::tail(cfg$select_id_cols, 1))
  filter <- .dedup_filter_sql(dedup, hosp_ids)

  # PostgreSQL badly underestimates the rows of the dedup_u CTE and would join
  # the chart and reference keys with a nested loop (millions x millions on the
  # eICU vital tables); the key join must be a hash join, so nested loops are
  # switched off for this statement. DuckDB hash-joins equality keys anyway.
  pg_hash <- identical(engine, "postgres")
  ctes <- glue::glue("
  dedup_u AS (
  SELECT t.*, ROW_NUMBER() OVER () AS dedup_rid FROM (
  {union_sql}
  ) t
  ),
  {filter$ctes}")
  sql <- create(exact(glue::glue("
  SELECT {paste0('c.', out_cols, collapse = ', ')}
  FROM dedup_u c
  WHERE NOT c.is_dedup_reference
    AND {filter$anti_join}"), ctes = ctes))
  if (pg_hash) sql <- paste0("SET enable_nestloop = off;\n", sql, "\nRESET enable_nestloop;")
  sql
}

# ---------------------------------------------------------------------------
# Chart-copy deduplication (rm_duplicates)
# ---------------------------------------------------------------------------
# Everything comes from the concept config. A MIMIC chart row (MetaVision or
# CareVue) with required_dedup = TRUE is a lab copy. It is removed when one of
# its concept's extracted labevents rows (plus any items named in its
# dedup_cross_reference) has the same subject and admission, the same stored
# value, chart time - lab time exactly equal to an allowed offset, and the same
# canonical unit. A blank chart unit is treated as the concept's target unit,
# because the stored value is already in that unit. Any other unresolved unit
# is kept.

# Offsets (chart time - lab time, minutes) validated per source family.
.dedup_allowed_offsets <- list(mimic_iv = 0L, mimic_iii = c(0L, 60L), eicu = 0L)
# eICU: a nurse-charted vital (nursecharting) flagged required_dedup is a copy
# of the monitor value when the same concept has a vitalperiodic or
# vitalaperiodic row for the same unit stay, minute offset and value. Neither
# side carries a unit column; both store the concept's unit.
.dedup_eicu_reference_tables <- c("vitalperiodic", "vitalaperiodic")
# Reviewed unit-label aliases (normalised labels): alias -> canonical label.
# gm/dl: MIMIC-III CareVue hemoglobin (item 814) next to labevents g/dL.
.dedup_unit_aliases <- c(seconds = "sec", "gm/dl" = "g/dl")

.dedup_ids <- function(x) {
  x <- as.character(x)
  if (is.na(x) || !nzchar(trimws(x))) return(integer(0))
  suppressWarnings(as.integer(trimws(strsplit(x, "|", fixed = TRUE)[[1]])))
}

.dedup_norm_unit <- function(x) {
  x <- gsub("[ .]", "", tolower(trimws(x)))
  ifelse(x %in% names(.dedup_unit_aliases), .dedup_unit_aliases[x], x)
}

# SQL for the canonical unit of a row's stored value, mirroring
# concept_unit_conversion_func(): conversion none keeps the (normalised) raw
# label; map/multiply/divide/f_to_c store values in the target unit when the
# source unit is recognised. A missing or blank unit uses the concept's
# target unit (the stored value is already in that unit), or '' for unitless
# concepts (no target unit, "ratio", "inr"). An unrecognised label is NULL,
# so such rows are never removed. Any other conversion is NULL.
.dedup_unit_key_sql <- function(conversion, unit_var, unit, concept_name) {
  if (is.na(unit_var) || !nzchar(unit_var)) return("CAST(NULL AS TEXT)")
  u <- glue::glue("CAST(src.{unit_var} AS TEXT)")
  q <- function(x) paste0("'", gsub("'", "''", x), "'")
  unitless <- is.na(unit) || !nzchar(unit) || tolower(unit) %in% c("ratio", "inr")
  # Blank source unit: the stored value is already in the concept unit, so
  # pair it with that label (or '' when the concept itself is unitless).
  blank <- if (unitless) "''" else q(.dedup_norm_unit(unit))
  norm <- glue::glue("REPLACE(REPLACE(LOWER(TRIM({u})), ' ', ''), '.', '')")
  alias <- if (length(.dedup_unit_aliases))
    paste0("CASE ", paste(sprintf("WHEN %s = %s THEN %s", norm, q(names(.dedup_unit_aliases)), q(.dedup_unit_aliases)),
                          collapse = " "), " ELSE ", norm, " END") else norm
  target <- if (unitless) "''" else q(.dedup_norm_unit(unit))
  conv <- if (is.na(conversion)) "" else conversion
  if (conv %in% c("", "none", "percent", "pct"))
    return(glue::glue("CASE WHEN {u} IS NULL OR TRIM({u}) = '' THEN {blank} ELSE {alias} END"))
  if (conv == "map") {
    map <- .load_unit_conversion_map()
    used <- map$concept_name == concept_name & !.unit_map_excluded(map$factor)
    known <- unique(tolower(c(unit, map$source_unit[used])))
    return(glue::glue("CASE WHEN LOWER({u}) IN ({paste(q(known), collapse = ', ')}) THEN {target} ELSE CAST(NULL AS TEXT) END"))
  }
  if (conv == "f_to_c") {
    known <- c(q(tolower(unit)), q(.fahrenheit_units))
    return(glue::glue("CASE WHEN {u} IS NULL THEN {blank} WHEN LOWER(TRIM({u})) IN ({paste(known, collapse = ', ')}) THEN {target} ELSE CAST(NULL AS TEXT) END"))
  }
  "CAST(NULL AS TEXT)"
}

# Dedup plan for one source, derived from its effective config. A concept
# replaced through user_concepts loses its dedup; a cross-concept reference to
# a replaced concept is dropped while the concept's own references remain.
.dedup_plan <- function(src_cfg, family, overridden = character(0)) {
  src_cfg <- .ehr_normalize_concept_cols(src_cfg)
  plan <- list(); notes <- character(0)
  offs <- .dedup_allowed_offsets[[family]]
  if (is.null(offs) || !"required_dedup" %in% names(src_cfg)) return(structure(plan, notes = notes))
  cross_col <- if ("dedup_cross_reference" %in% names(src_cfg)) as.character(src_cfg$dedup_cross_reference) else rep(NA_character_, nrow(src_cfg))
  flagged <- !is.na(src_cfg$required_dedup) & toupper(trimws(as.character(src_cfg$required_dedup))) == "TRUE"
  items_of <- function(concept, table)
    unique(unlist(lapply(src_cfg$match_value[src_cfg$concept_name == concept & src_cfg$table_name == table], .dedup_ids)))
  if (identical(family, "eicu")) {
    for (i in which(flagged & src_cfg$table_name %in% "nursecharting")) {
      cn <- src_cfg$concept_name[i]
      if (cn %in% overridden) {
        notes <- c(notes, sprintf("dedup for '%s' disabled: concept replaced by user_concepts", cn)); next
      }
      labels <- trimws(strsplit(as.character(src_cfg$match_value[i]), "|", fixed = TRUE)[[1]])
      refs <- intersect(.dedup_eicu_reference_tables, src_cfg$table_name[src_cfg$concept_name == cn])
      if (!length(labels) || !length(refs)) next
      pairs <- merge(data.frame(chart_table = "nursecharting", chart_itemid = labels,
                                lab_table = refs, lab_itemid = NA_character_, cross = FALSE),
                     data.frame(offset_min = offs))
      e <- plan[[cn]] %||% list(pairs = NULL, cross_items = list())
      e$pairs <- unique(rbind(e$pairs, pairs))
      plan[[cn]] <- e
    }
    attr(plan, "notes") <- unique(notes)
    return(plan)
  }
  for (i in which(flagged & src_cfg$table_name %in% "chartevents")) {
    cn <- src_cfg$concept_name[i]
    ci <- .dedup_ids(src_cfg$match_value[i]); ci <- ci[!is.na(ci)]
    if (!length(ci)) next                      # non-numeric ids: flag only
    if (cn %in% overridden) {
      notes <- c(notes, sprintf("dedup for '%s' disabled: concept replaced by user_concepts", cn)); next
    }
    own <- items_of(cn, "labevents")
    if (!length(own)) next                     # nothing to compare against
    cross <- integer(0); owner <- NA_character_
    ref <- cross_col[i]
    if (!is.na(ref) && nzchar(trimws(ref))) {
      if (!grepl("^[A-Za-z0-9_]+:[0-9]+(\\|[0-9]+)*$", trimws(ref)))
        stop("dedup_cross_reference for '", cn, "' must be 'concept:itemid[|itemid]', got '", ref, "'", call. = FALSE)
      owner <- sub(":.*", "", trimws(ref)); cross <- .dedup_ids(sub("^[^:]+:", "", trimws(ref)))
      if (owner %in% overridden) {
        notes <- c(notes, sprintf("dedup %s/%s: cross-concept reference to '%s' disabled: '%s' replaced by user_concepts",
                                  cn, paste(ci, collapse = "|"), owner, owner))
        cross <- integer(0)
      } else if (!all(cross %in% items_of(owner, "labevents"))) {
        stop("dedup_cross_reference for '", cn, "': item(s) ", paste(setdiff(cross, items_of(owner, "labevents")), collapse = ", "),
             " are not labevents items of '", owner, "'", call. = FALSE)
      }
    }
    labs <- data.frame(lab_itemid = c(own, cross), cross = c(rep(FALSE, length(own)), rep(TRUE, length(cross))))
    pairs <- merge(data.frame(chart_table = "chartevents", chart_itemid = as.character(ci), lab_table = "labevents"),
                   merge(labs, data.frame(offset_min = offs)))
    pairs$lab_itemid <- as.character(pairs$lab_itemid)
    e <- plan[[cn]] %||% list(pairs = NULL, cross_items = list())
    e$pairs <- unique(rbind(e$pairs, pairs))
    if (length(cross)) e$cross_items[[owner]] <- unique(c(e$cross_items[[owner]], as.character(cross)))
    plan[[cn]] <- e
  }
  attr(plan, "notes") <- unique(notes)
  plan
}

# The builder's dedup argument for one concept. Cross-concept references are
# kept only for owning concepts in `built` (built successfully in this call).
.dedup_spec <- function(entry, src_cfg, built, concept) {
  if (is.null(entry)) return(NULL)
  pairs <- entry$pairs; ref_rows <- NULL
  for (owner in names(entry$cross_items)) {
    items <- entry$cross_items[[owner]]
    if (owner %in% built) {
      rows <- src_cfg[src_cfg$concept_name == owner & src_cfg$table_name == "labevents", , drop = FALSE]
      rows <- rows[vapply(rows$match_value, function(m) any(.dedup_ids(m) %in% items), TRUE), , drop = FALSE]
      rows$match_value <- vapply(rows$match_value, function(m) paste(intersect(.dedup_ids(m), items), collapse = "|"), "")
      ref_rows <- rbind(ref_rows, rows)
    } else {
      message("dedup: cross-concept reference from '", concept, "' to '", owner,
              "' skipped because '", owner, "' is not built in this call; ",
              "its chart copies are retained with dedup_candidate = TRUE")
      pairs <- pairs[!(pairs$cross & pairs$lab_itemid %in% as.character(items)), , drop = FALSE]
    }
  }
  if (!nrow(pairs)) return(NULL)
  list(pairs = pairs, reference_rows = ref_rows)
}

.dedup_filter_sql <- function(dedup, id_cols) {
  q <- function(x) ifelse(is.na(x), "CAST(NULL AS TEXT)", paste0("'", gsub("'", "''", x), "'"))
  p <- dedup$pairs
  if (is.null(p$chart_table)) p$chart_table <- "chartevents"
  if (is.null(p$lab_table)) p$lab_table <- "labevents"
  # an eICU reference table stores the concept unit without a unit column
  unitless <- p$lab_table %in% .dedup_eicu_reference_tables
  pair_vals <- paste(sprintf("(%s, %s, %s, %s, %d, %s)", q(p$chart_table), q(p$chart_itemid), q(p$lab_table),
                             q(p$lab_itemid), as.integer(p$offset_min), ifelse(unitless, "TRUE", "FALSE")),
                     collapse = ",\n    ")
  offs <- sort(unique(as.integer(p$offset_min)))
  # chart time shifted back by the rule's offset, i.e. the lab time it must equal
  aligned <- paste0("CASE p.offset_min ", paste(vapply(offs, function(o) if (o == 0L)
    "WHEN 0 THEN c.event_time_raw" else sprintf("WHEN %d THEN c.event_time_raw - INTERVAL '%d minutes'", o, o), ""),
    collapse = " "), " END")
  # Hash matching: every covered chart row gets one key per (reference table
  # and item, offset), every reference row its own key, and a chart row whose
  # key equals a reference key is a copy. Each key part is compared as text with
  # NULL mapped to a marker, so two NULLs match (as rows of one group would) and
  # the join stays a hash join instead of sorting every row. Rows without a
  # resolved unit have no key and are never removed, except on the unitless
  # eICU vital tables.
  nk <- function(x) sprintf("COALESCE(CAST(%s AS TEXT), '\\N')", x)
  key <- function(a, time, item, unit) paste(c(
    nk(paste0(a, ".", id_cols)), sprintf("CAST(%s.value AS TEXT)", a),
    nk(time), item, nk(unit)), collapse = " || '|' || ")
  list(
    ctes = glue::glue(
      "dedup_pair (chart_table, chart_itemid, lab_table, lab_itemid, offset_min, unitless) AS (VALUES\n    {pair_vals}\n  ),\n",
      "  dedup_lab_keys AS (\n",
      "    SELECT {key('l', 'l.event_time_raw', \"l.source_table || ':' || COALESCE(p.lab_itemid, '')\", \"CASE WHEN p.unitless THEN COALESCE(l.unit_key, '') ELSE l.unit_key END\")} AS k\n",
      "    FROM dedup_u l\n",
      "    JOIN (SELECT DISTINCT lab_table, lab_itemid, unitless FROM dedup_pair) p\n",
      "      ON p.lab_table = l.source_table AND (p.lab_itemid IS NULL OR p.lab_itemid = l.source_itemid)\n",
      "    WHERE l.value IS NOT NULL AND (p.unitless OR l.unit_key IS NOT NULL)\n",
      "  ),\n",
      "  dedup_removed AS (\n",
      "    SELECT DISTINCT c.dedup_rid\n",
      "    FROM dedup_u c\n",
      "    JOIN dedup_pair p ON p.chart_table = c.source_table AND p.chart_itemid = c.source_itemid\n",
      "    JOIN dedup_lab_keys lk\n",
      "      ON lk.k = {key('c', aligned, \"p.lab_table || ':' || COALESCE(p.lab_itemid, '')\", \"CASE WHEN p.unitless THEN COALESCE(c.unit_key, '') ELSE c.unit_key END\")}\n",
      "    WHERE c.value IS NOT NULL AND (p.unitless OR c.unit_key IS NOT NULL)\n",
      "  )"),
    anti_join = "c.dedup_rid NOT IN (SELECT dedup_rid FROM dedup_removed)"
  )
}

# ---------------------------------------------------------------------------
# Main execution function
# ---------------------------------------------------------------------------

# Concept config columns that shipped misspelled. Configs written with the
# old spellings keep working: they are renamed on read unless the corrected
# column is also present, in which case the corrected one wins.
.ehr_concept_col_renames <- c(requred_dedup = "required_dedup",
                              basos_asbolute = "basos_absolute")

.ehr_normalize_concept_cols <- function(df) {
  for (old in names(.ehr_concept_col_renames)) {
    new <- .ehr_concept_col_renames[[old]]
    if (!old %in% names(df)) next
    if (new %in% names(df)) {
      df[[old]] <- NULL
    } else {
      names(df)[names(df) == old] <- new
    }
  }
  df
}

# readr guesses column types per file, so a one-row user concept file whose
# match_value is a single item ID reads as numeric while the shipped config
# reads it as text, and bind_rows() refuses to combine them. Reconcile each
# shared column first: an all-NA column takes the other side's type, a
# text/number mismatch becomes text on both sides (no value is coerced to
# NA), and integer/double becomes double.
.ehr_align_concept_cols <- function(base, extra) {
  for (col in intersect(names(base), names(extra))) {
    a <- base[[col]]; b <- extra[[col]]
    if (identical(class(a), class(b))) next
    # Indexing with NA keeps the vector's type, so this yields typed NAs.
    if (all(is.na(b))) {
      extra[[col]] <- a[rep(NA_integer_, length(b))]
    } else if (all(is.na(a))) {
      base[[col]] <- b[rep(NA_integer_, length(a))]
    } else if (is.numeric(a) && is.numeric(b)) {
      base[[col]] <- as.double(a); extra[[col]] <- as.double(b)
    } else {
      base[[col]] <- as.character(a); extra[[col]] <- as.character(b)
    }
  }
  list(base = base, extra = extra)
}

# Item classes build_match_clause() understands.
.ehr_itm_classes <- c("itm_itm", "rgx_itm", "rgx_exp_itm", "col_itm")

#' Check a concept configuration for invalid rows
#'
#' Applies the concept configuration rules to every row and lists the
#' problems found. [run_atomic_concepts_unified()] runs the same check before
#' it builds any SQL: rows with a `"skip"` problem are left out, rows with a
#' `"warn"` problem are built, and both are reported in one warning.
#'
#' Rules:
#' * `unknown_itm_class` (skip): `itm_class` is missing or not one of
#'   `itm_itm`, `rgx_itm`, `rgx_exp_itm` or `col_itm` (case and surrounding
#'   spaces are ignored).
#' * `empty_match_value` (skip): `match_value` is empty for a class other
#'   than `col_itm`. Only `col_itm` reads a whole column; the other classes
#'   need item ids, labels or a pattern, and without one would select every
#'   row of the source table.
#'
#' @param config Path to a concept configuration CSV, or a data frame of its
#'   rows.
#' @param src_name Optional source name, such as `"mimic_iv_demo"`. When
#'   given, only rows of its source family are checked.
#' @return A data frame with one row per problem: `row` (position of the
#'   config row in `config`, header excluded), `source`, `concept_name`,
#'   `table_name`, `rule`, `severity` (`"skip"` or `"warn"`) and `message`.
#'   It has zero rows when the configuration is valid.
#' @examples
#' validate_concept_config(
#'   system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"))
#' @export
validate_concept_config <- function(config, src_name = NULL) {
  if (is.character(config) && length(config) == 1L)
    config <- .ehr_normalize_concept_cols(readr::read_csv(config, show_col_types = FALSE))
  config <- as.data.frame(config)
  if (!"itm_class" %in% names(config))
    stop("The concept config has no itm_class column.", call. = FALSE)
  row <- seq_len(nrow(config))
  if (!is.null(src_name)) row <- row[config$source %in% src_family(src_name)]
  field <- function(x) if (x %in% names(config)) as.character(config[[x]][row]) else rep(NA_character_, length(row))
  blank <- function(x) is.na(x) | !nzchar(trimws(x))
  cls   <- trimws(tolower(field("itm_class")))
  known <- !blank(cls) & cls %in% .ehr_itm_classes

  # One entry per rule: its severity, the rows it flags and their message.
  # A new rule is one more entry here.
  rules <- list(
    unknown_itm_class = list(
      severity = "skip", hit = !known,
      message  = paste0("unknown itm_class ",
                        ifelse(blank(field("itm_class")), "<missing>", paste0("'", field("itm_class"), "'")),
                        " (use one of: ", paste(.ehr_itm_classes, collapse = ", "), ")")),
    empty_match_value = list(
      severity = "skip", hit = known & cls != "col_itm" & blank(field("match_value")),
      message  = paste0("empty match_value (", cls, " needs item ids, labels or a pattern; ",
                        "only col_itm may leave it empty)"))
  )
  out <- do.call(rbind, lapply(names(rules), function(r) {
    h <- rules[[r]]$hit
    data.frame(row = row[h], source = field("source")[h], concept_name = field("concept_name")[h],
               table_name = field("table_name")[h], rule = rep(r, sum(h)),
               severity = rep(rules[[r]]$severity, sum(h)), message = rules[[r]]$message[h],
               stringsAsFactors = FALSE)
  }))
  out <- out[order(out$row), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Execute atomic concept SQL for all concepts matching a source and type
#'
#' Reads the concept config CSV, builds CREATE TABLE SQL for each concept,
#' writes it to sql_out_dir, and executes it against `con`.
#'
#' @param con DBI connection. Required unless `dry_run = TRUE`. Its dialect
#'   must match `engine`.
#' @param concept_config_path Path to rendehr_concepts_config.csv or
#'   rendehr_duration_concepts_config.csv.
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param schema_names List from src_schema_names().
#' @param typ Concept type filter: "ts", "dur", or NULL for all.
#' @param sql_out_dir Directory to write generated SQL files.
#' @param user_concepts Optional path to a supplemental concept CSV. A concept
#'   defined here (same source + concept_name) replaces all package rows for
#'   that concept with all of its rows in this file; other package concepts
#'   are kept unchanged. Lets users add or redefine concepts without editing
#'   package files. An error if the file does not exist.
#' @param concept_name Optional character vector to restrict execution to
#'   specific concept name(s), instead of every concept matching `src_name`/
#'   `typ`. Useful while iterating on one concept without rebuilding the rest.
#' @param dry_run If `TRUE`, build (and still write to `sql_out_dir`) the SQL
#'   for each matching concept without executing it. `con` is not required in
#'   this mode. Default `FALSE`.
#' @param engine SQL dialect to generate: `"postgres"` (default) or
#'   `"duckdb"`. Only affects the small handful of clauses that differ
#'   between the two (currently: the case-insensitive regex match used by
#'   `rgx_exp_itm` concepts). Must match the dialect `con` actually speaks.
#' @param rm_duplicates Logical. If `TRUE` (default), remove chart rows that
#'   are copies of lab results. A MIMIC chart row (MetaVision or CareVue)
#'   flagged `required_dedup = TRUE` in the concept config is dropped only
#'   when one of the concept's extracted lab rows (or an item named in its
#'   `dedup_cross_reference`, as `concept:itemid`) has the same subject and
#'   admission, the same stored value, an exact allowed offset between the
#'   original timestamps (MIMIC-IV 0, MIMIC-III 0 or +60 minutes), and the same
#'   canonical unit after each row's configured conversion. A blank chart unit
#'   is treated as the concept's target unit; any other unresolved unit is
#'   kept. An eICU nurse-charted vital (`nursecharting`) flagged
#'   `required_dedup = TRUE` is dropped when the same concept has a
#'   `vitalperiodic` or `vitalaperiodic` row for the same unit stay, minute
#'   offset and value; the monitor row is kept. In addition, chart rows of a ts concept
#'   that agree on all ID columns, minute, value and unit (for example two
#'   CareVue items charting the same glucose) are kept once, as the row with
#'   the lowest item id; rows with a NULL value and rows from other tables
#'   (labs, drugs) are never removed by this rule.
#'   `FALSE` extracts exactly the same rows and removes none. Rules for a
#'   concept replaced through `user_concepts` are disabled, as are
#'   cross-concept references to a replaced concept. A cross-concept reference
#'   is used only when its owning concept is built successfully in the same
#'   call; otherwise the chart copies are retained.
#' @details The configuration is checked with [validate_concept_config()]
#'   before any SQL is built. Its problems are reported in one immediate
#'   warning; rows with a `"skip"` problem (an unknown `itm_class`, or an empty
#'   `match_value` for a class other than `col_itm`) are left out and the
#'   remaining rows are built. A concept built without some of its rows is
#'   reported as `OK: <concept> (<n> config row(s) skipped: <reasons>)`.
#' @return Invisibly returns a named logical vector, one entry per attempted
#'   concept, `TRUE` if its table was built successfully and `FALSE` if its
#'   `CREATE TABLE` execution failed. Returns `invisible(logical(0))` when no
#'   concept matches the source/type filter. A failed concept does not stop
#'   the remaining concepts from being attempted; the caller is responsible
#'   for inspecting the returned vector to detect partial or total failure.
#'   When `dry_run = TRUE`, instead invisibly returns a named character vector
#'   of `concept_name -> generated SQL text`, and nothing is executed.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' run_atomic_concepts_unified(con,
#'   concept_config_path = system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"),
#'   src_name = "mimic_iv_demo", schema_names = src_schema_names("mimic_iv_demo"),
#'   typ = "ts", sql_out_dir = "sql/generated/mimic_iv_demo/ts", concept_name = "heart_rate")
#' }
#' @export
run_atomic_concepts_unified <- function(con = NULL, concept_config_path, src_name,
                                        schema_names, typ = NULL, sql_out_dir,
                                        user_concepts = NULL, concept_name = NULL,
                                        dry_run = FALSE,
                                        engine = c("postgres", "duckdb"),
                                        rm_duplicates = TRUE) {
  engine   <- match.arg(engine)
  if (!is.logical(rm_duplicates) || length(rm_duplicates) != 1L || is.na(rm_duplicates))
    stop("`rm_duplicates` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(user_concepts) && !file.exists(user_concepts))
    stop("`user_concepts` file not found: ", user_concepts, call. = FALSE)
  concepts <- .ehr_normalize_concept_cols(
    readr::read_csv(concept_config_path, show_col_types = FALSE))

  overridden <- character(0)
  if (!is.null(user_concepts)) {
    # A user concept replaces every base row for the same source + name, and
    # keeps all of its own rows. Deduplicating the merged table instead would
    # also collapse the base config's multi-row concepts (several itemids or
    # tables per concept) down to their last row.
    extra     <- .ehr_normalize_concept_cols(
      readr::read_csv(user_concepts, show_col_types = FALSE))
    user_keys <- paste(extra$source, extra$concept_name)
    overridden <- unique(extra$concept_name[extra$source == src_family(src_name)])
    aligned   <- .ehr_align_concept_cols(concepts, extra)
    concepts  <- aligned$base
    extra     <- aligned$extra
    concepts  <- dplyr::bind_rows(
      concepts[!paste(concepts$source, concepts$concept_name) %in% user_keys, ,
               drop = FALSE],
      extra
    )
  }

  concepts <- concepts[concepts$source == src_family(src_name), , drop = FALSE]

  concepts <- dplyr::mutate(concepts, class = trimws(tolower(itm_class)))
  # Invalid rows (see validate_concept_config()) are reported in one warning,
  # printed in place in the run log; rows with a "skip" problem are left out.
  problems <- validate_concept_config(concepts)
  if (nrow(problems)) {
    shown <- utils::head(seq_len(nrow(problems)), 10L)
    lines <- sprintf("  %s / %s: %s", problems$concept_name[shown],
                     problems$table_name[shown], problems$message[shown])
    if (nrow(problems) > length(shown))
      lines <- c(lines, sprintf("  ... and %d more", nrow(problems) - length(shown)))
    warning(nrow(problems), " problem(s) in the concept config for source '",
            src_family(src_name), "'; rows with a skip problem are not built:\n",
            paste(lines, collapse = "\n"), call. = FALSE, immediate. = TRUE)
  }
  skip <- unique(problems[problems$severity == "skip", c("row", "concept_name", "rule")])
  # Per concept, for its OK line: "<n> config row(s) skipped: <reasons>"; a
  # rule name reads as "<what>_<column>", e.g. "empty match_value".
  skipped <- lapply(split(skip, skip$concept_name), function(s)
    sprintf("%d config row(s) skipped: %s", length(unique(s$row)),
            paste(unique(sub("_", " ", s$rule, fixed = TRUE)), collapse = ", ")))
  if (nrow(skip)) concepts <- concepts[-unique(skip$row), , drop = FALSE]
  src_cfg <- as.data.frame(concepts)   # every concept of the source, before typ/name filters
  for (col in c("table_name", "match_value", "dedup_cross_reference"))
    src_cfg[[col]] <- if (col %in% names(src_cfg)) as.character(src_cfg[[col]]) else rep(NA_character_, nrow(src_cfg))

  if (!is.null(typ))
    concepts <- concepts[concepts$typ %in% typ, , drop = FALSE]

  if (!is.null(concept_name))
    concepts <- concepts[concepts$concept_name %in% concept_name, , drop = FALSE]

  if (nrow(concepts) == 0L) {
    message("No concepts found for source '", src_name, "'.")
    return(invisible(logical(0L)))
  }

  plan <- NULL
  if (isTRUE(rm_duplicates)) {
    plan <- .dedup_plan(src_cfg, src_family(src_name), overridden)
    for (note in attr(plan, "notes")) message("dedup: ", note)
  }

  grouped       <- dplyr::group_split(dplyr::group_by(concepts, concept_name))
  concept_names <- vapply(grouped, function(x) x$concept_name[1L], character(1L))
  # Owning concepts of cross-concept references are built before dependents.
  owners  <- unique(unlist(lapply(plan, function(e) names(e$cross_items))))
  ord     <- order(!concept_names %in% owners)
  grouped <- stats::setNames(grouped[ord], concept_names[ord])
  concept_names <- concept_names[ord]

  build_sql <- function(name, built) {
    build_atomic_concept_unified_sql(
      grouped[[name]],
      raw_schema      = schema_names$raw,
      concepts_schema = schema_names$concepts,
      src_name        = src_name,
      engine          = engine,
      rm_duplicates   = rm_duplicates,
      dedup           = if (isTRUE(rm_duplicates)) .dedup_spec(plan[[name]], src_cfg, built, name)
    )
  }
  # Generated assuming every selected concept builds; a dependent is rebuilt
  # without its cross-concept reference if its owning concept then fails.
  sqls <- vapply(concept_names, build_sql, character(1L), built = concept_names)
  names(sqls) <- concept_names

  dir.create(sql_out_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in concept_names)
    writeLines(sqls[[name]], file.path(sql_out_dir, paste0(name, ".sql")))

  if (isTRUE(dry_run)) {
    message("Dry run: ", length(concept_names), " concept(s) built, not executed: ",
            paste(concept_names, collapse = ", "))
    return(invisible(sqls))
  }

  if (is.null(con))
    stop("run_atomic_concepts_unified(): `con` is required unless dry_run = TRUE.",
         call. = FALSE)

  # Each generated string holds several statements (DROP TABLE IF EXISTS ...;
  # CREATE TABLE ... AS ...;). RPostgres can run them in one dbExecute() call
  # via immediate = TRUE, but that argument is Postgres-specific and duckdb's
  # dbExecute() doesn't accept multi-statement strings the same way - split
  # them with the quote-aware .ehr_split_sql() and execute each in turn,
  # which both backends support identically.
  results <- setNames(logical(length(concept_names)), concept_names)
  for (name in concept_names) {
    results[name] <- tryCatch(
      {
        full_sql  <- sqls[[name]]
        failed    <- intersect(names(plan[[name]]$cross_items), concept_names)
        failed    <- failed[!results[failed]]
        if (length(failed)) {
          full_sql <- build_sql(name, built = setdiff(concept_names, failed))
          writeLines(full_sql, file.path(sql_out_dir, paste0(name, ".sql")))
        }
        t0 <- Sys.time()
        for (statement in .ehr_split_sql(full_sql))
          DBI::dbExecute(con, statement)
        secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        message("OK: ", name, sprintf(" (%.1f s)", secs),
                if (name %in% names(skipped)) paste0(" (", skipped[[name]], ")"))
        TRUE
      },
      error = function(e) {
        message("FAIL: ", name, " | ", e$message)
        if (identical(engine, "postgres"))
          try(DBI::dbExecute(con, "RESET enable_nestloop"), silent = TRUE)
        FALSE
      }
    )
  }

  invisible(results)
}

# %||% is defined in R/download.R

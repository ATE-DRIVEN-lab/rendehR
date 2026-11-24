
#' Cohort-building and parquet subsetting functions.
#' @noRd

# ___________________________________________________________________________
# Direct-ID cohort helpers
# ___________________________________________________________________________

.ehr_read_direct_ids <- function(path, key, src_name = NULL,
                                 arg_name = "direct_ids_path") {
  if (is.null(path) || !nzchar(path))
    stop(arg_name, " must be a non-empty path.", call. = FALSE)
  if (!file.exists(path))
    stop(arg_name, " file not found: ", path, call. = FALSE)
  if (is.null(key) || length(key) != 1L || !nzchar(key))
    stop("A non-empty cohort key is required to read ", arg_name, ".",
         call. = FALSE)

  lower_path <- tolower(path)
  df <- if (grepl("\\.rds$", lower_path)) {
    obj <- readRDS(path)
    if (is.data.frame(obj)) {
      as.data.frame(obj)
    } else if (is.atomic(obj) || is.factor(obj)) {
      stats::setNames(data.frame(as.character(obj), stringsAsFactors = FALSE), key)
    } else {
      stop(arg_name, " .rds must contain a data frame or vector.", call. = FALSE)
    }
  } else if (grepl("\\.csv(\\.gz)?$", lower_path)) {
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      name_repair = "unique_quiet"
    )
  } else if (grepl("\\.parquet$", lower_path)) {
    as.data.frame(arrow::read_parquet(path))
  } else {
    stop(arg_name, " must be a .csv, .csv.gz, .parquet, or .rds file.",
         call. = FALSE)
  }

  if (!key %in% names(df)) {
    src_msg <- if (!is.null(src_name)) paste0(" for source ", src_name) else ""
    stop(arg_name, src_msg, " must contain the column ", key, ".",
         call. = FALSE)
  }

  ignored <- max(0L, ncol(df) - 1L)
  values <- .ehr_normalize_id_values(df[[key]], arg_name)
  out <- stats::setNames(
    data.frame(values, stringsAsFactors = FALSE), key
  )
  attr(out, "ignored_column_count") <- ignored
  out
}

# ___________________________________________________________________________
# Parquet helpers
# ___________________________________________________________________________

# Test whether a parquet file exists and contains at least one row. A 0-row or
# unreadable parquet (e.g. from a crashed run) is treated as not present, so
# callers rebuild rather than trust a broken file.
parquet_has_rows <- function(path) {
  file.exists(path) && tryCatch(
    # Read the row count from the parquet footer metadata only - no data scan.
    # NB: ReadTable(col_indices = integer(0)) is WRONG here: reading zero
    # columns yields a 0-column table whose num_rows is always 0, so every
    # file looks empty.
    nrow(arrow::open_dataset(path, format = "parquet")) > 0L,
    error = function(e) FALSE)
}

#' Safely subset one parquet file to rows matching key values
#'
#' This bounded utility handles exactly one input file and one output file. The
#' input must contain `key`; it is never copied unfiltered when the key is
#' absent. Identifiers use the same strict normalization as cohort selection,
#' and publication is atomic.
#'
#' @param input_path Input parquet file.
#' @param output_path Output parquet file.
#' @param key Column name to filter on (e.g. "subject_id").
#' @param key_values Vector of values to keep.
#' @param allow_unmatched_ids Permit a partially unmatched list when at least
#'   one identifier matches? Default `FALSE`.
#' @param overwrite Replace an existing output atomically?
#' @return Invisibly returns aggregate requested, matched, unmatched, and row
#'   counts. Identifier values are never printed.
#' @examples
#' \dontrun{
#' subset_parquet_by_key("raw/labevents.parquet", "cohort/labevents.parquet",
#'                       key = "subject_id", key_values = c(10000032, 10001217))
#' }
#' @export
subset_parquet_by_key <- function(input_path, output_path, key,
                                  key_values,
                                  allow_unmatched_ids = FALSE,
                                  overwrite = FALSE) {
  .duckdb_require()
  if (!file.exists(input_path) || dir.exists(input_path) ||
      !grepl("\\.parquet$", input_path, ignore.case = TRUE)) {
    stop("`input_path` must be one existing parquet file.", call. = FALSE)
  }
  if (normalizePath(input_path, mustWork = TRUE) ==
      normalizePath(output_path, mustWork = FALSE)) {
    stop("Input and output parquet paths must differ.", call. = FALSE)
  }
  if (file.exists(output_path) && !isTRUE(overwrite)) {
    stop("Output parquet already exists; use `overwrite = TRUE` to replace it.",
         call. = FALSE)
  }
  if (is.null(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    stop("`key` must be one non-empty column name.", call. = FALSE)
  }
  values <- .ehr_normalize_id_values(key_values, "`key_values`")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(
    pattern = paste0(".", basename(output_path), ".stage-"),
    tmpdir = dirname(output_path)
  )
  on.exit(if (file.exists(stage)) unlink(stage, force = TRUE), add = TRUE)

  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "ehr_subset_ids",
    data.frame(value = values, stringsAsFactors = FALSE),
    temporary = TRUE, overwrite = TRUE
  )
  source <- sprintf(
    "read_parquet(%s)", DBI::dbQuoteString(con, input_path)
  )
  columns <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", source))
  if (!key %in% columns$column_name) {
    stop("Input parquet does not contain the required key column.",
         call. = FALSE)
  }
  keyq <- as.character(DBI::dbQuoteIdentifier(con, key))
  matched <- as.numeric(DBI::dbGetQuery(con, sprintf(
    paste0(
      "SELECT count(DISTINCT ids.value) AS n FROM ehr_subset_ids ids ",
      "INNER JOIN %s src ON CAST(src.%s AS VARCHAR) = ids.value"
    ),
    source, keyq
  ))$n)
  unmatched <- length(values) - matched
  if (unmatched > 0L && !isTRUE(allow_unmatched_ids)) {
    stop(unmatched, " of ", length(values),
         " identifier value(s) did not match; values are not shown.",
         call. = FALSE)
  }
  if (matched == 0L) {
    stop("No identifier matched the input parquet.", call. = FALSE)
  }
  DBI::dbExecute(con, sprintf(
    paste0(
      "COPY (SELECT src.* FROM %s src ",
      "WHERE CAST(src.%s AS VARCHAR) IN (SELECT value FROM ehr_subset_ids)) ",
      "TO %s (FORMAT PARQUET)"
    ),
    source, keyq, DBI::dbQuoteString(con, stage)
  ))
  rows <- as.numeric(DBI::dbGetQuery(
    con, sprintf("SELECT count(*) AS n FROM read_parquet(%s)",
                 DBI::dbQuoteString(con, stage))
  )$n)
  if (file.exists(output_path) && !file.remove(output_path)) {
    stop("Could not replace the existing output parquet.", call. = FALSE)
  }
  if (!file.rename(stage, output_path)) {
    stop("Could not publish the subset parquet atomically.", call. = FALSE)
  }
  invisible(data.frame(
    requested = length(values),
    matched = matched,
    unmatched = unmatched,
    rows = rows,
    stringsAsFactors = FALSE
  ))
}

# ___________________________________________________________________________
# Registry lookups
# ___________________________________________________________________________

#' Tables needed to resolve ICD cohort IDs for a source
#'
#' These are the minimal raw tables an ICD cohort reads before cohort IDs can
#' be resolved; [cache_raw_parquet_duckdb()] caches them for named ICD cohorts.
#'
#' @param src_name Source name.
#' @return Character vector of table names.
#' @keywords internal
cohort_seed_tables <- function(src_name) {
  entry <- .source_entry(src_name)
  if (is.null(entry) || is.null(entry$cohort_seed_tables))
    stop("Unknown src_family: ", src_family(src_name))
  entry$cohort_seed_tables
}

#' Return the primary cohort key column for a source
#'
#' Returns the patient-root identifier configured for the source family. The
#' canonical cohort engine uses this as the coarsest hierarchy level and the
#' default effective scope; individual clinical tables are filtered through
#' their catalog-declared linkage keys.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @return Character scalar column name.
#' @examples
#' cohort_key("mimic_iv_demo")
#' cohort_key("eicu_demo")
#' @export
cohort_key <- function(src_name) {
  entry <- .source_entry(src_name)
  if (is.null(entry) || is.null(entry$cohort_key))
    stop("Unknown src_family: ", src_family(src_name))
  entry$cohort_key
}

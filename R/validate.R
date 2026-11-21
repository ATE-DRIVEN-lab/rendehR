# Stage-gate verification functions for the rendehR pipeline.
#
# These are standalone, user-invoked diagnostics: no pipeline stage or
# orchestration wrapper calls them automatically. A user calls one
# directly (e.g. verify_db_load(con, schema) after run_db()) to confirm a
# stage's output has the expected shape.
#
# Each verify_*() returns a list with:
#   ok       - TRUE if all checks passed
#   checks   - named logical vector
#   messages - character vector describing any failures

.make_result <- function(checks, messages) {
  list(ok = all(checks), checks = checks, messages = messages)
}

.table_exists <- function(con, schema, table) {
  n <- DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n FROM information_schema.tables
     WHERE table_schema = '{schema}' AND table_name = '{table}'"
  ))$n
  isTRUE(n > 0)
}

# Source family behind a pipeline schema name ("eicu_demo_coh_concepts" ->
# "eicu"), or NA when the schema does not follow the src_schema_names() layout.
.verify_schema_family <- function(schema) {
  src <- sub("_coh(_concepts|_derived|_harmonized)?$", "", schema)
  if (identical(src, schema)) return(NA_character_)
  tryCatch(src_family(src), error = function(e) NA_character_)
}

.verify_core_tables <- list(
  mimic_iii = c("patients", "admissions"),
  mimic_iv  = c("patients", "admissions"),
  eicu      = "patient"
)

.verify_mimic_handcrafted <- c(
  "sofa", "first_day_sofa",
  "first_day_vitalsign", "first_day_lab",
  "first_day_bg", "first_day_gcs",
  "bg", "chemistry", "complete_blood_count",
  "apsiii", "oasis", "sapsii", "kdigo_stages"
)

.row_count <- function(con, schema, table) {
  target_id <- DBI::dbQuoteIdentifier(con, DBI::Id(
    schema = .ehr_validate_db_identifier(schema, "schema name"),
    table  = .ehr_validate_db_identifier(table, "table name")
  ))
  DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n FROM {target_id}"
  ))$n
}

#' Verify a source's cohort Parquet artifact
#'
#' Checks the cohort Parquet directory written by [import_cohort_general()]
#' (`paths$cohort_parquet_dir`), not the raw downloads; [download_source()]
#' verifies those against the release checksums itself.
#'
#' @param paths List from ehr_paths().
#' @param required_tables Character vector of bare table names to check. A
#'   table is present as a flat `<table>.parquet` artifact (what the cohort
#'   builders write) or as a `<table>/` directory of parquet parts. NULL
#'   checks that any parquet files exist and are non-empty.
#' @return Verification result list (ok, checks, messages).
#' @examples
#' \dontrun{
#' paths <- ehr_paths("mimic_iv_demo")
#' verify_download(paths, required_tables = c("patients", "admissions", "labevents"))
#' }
#' @export
verify_download <- function(paths, required_tables = NULL) {
  parquet_dir <- paths$cohort_parquet_dir
  checks <- c(); msgs <- c()

  dir_exists <- dir.exists(parquet_dir)
  checks["parquet_dir_exists"] <- dir_exists
  if (!dir_exists) {
    msgs <- c(msgs, paste("Parquet directory not found:", parquet_dir))
    return(.make_result(checks, msgs))
  }

  parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$",
                               full.names = TRUE, recursive = TRUE)
  checks["has_parquet_files"] <- length(parquet_files) > 0
  if (!checks["has_parquet_files"]) {
    msgs <- c(msgs, paste("No .parquet files in", parquet_dir))
    return(.make_result(checks, msgs))
  }

  if (!is.null(required_tables)) {
    for (tbl in required_tables) {
      tbl_files <- list.files(file.path(parquet_dir, tbl),
                              pattern = "\\.parquet$", recursive = TRUE)
      flat_file <- file.path(parquet_dir, paste0(tbl, ".parquet"))
      checks[paste0("parquet_", tbl)] <- length(tbl_files) > 0 ||
        isTRUE(file.size(flat_file) > 0)
      if (!checks[paste0("parquet_", tbl)])
        msgs <- c(msgs, paste("Missing parquet table:", tbl))
    }
  } else {
    zero_size <- parquet_files[file.size(parquet_files) == 0]
    checks["no_empty_parquet"] <- length(zero_size) == 0
    if (!checks["no_empty_parquet"])
      msgs <- c(msgs, paste("Zero-size parquet files:",
                            paste(zero_size, collapse = ", ")))
  }

  .make_result(checks, msgs)
}

#' Verify cohort anchor table was built correctly
#' @param con DBI connection.
#' @param schema Raw cohort schema (e.g. "mimic_iv_demo_coh").
#' @return Verification result list.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' verify_cohort(con, "mimic_iv_demo_coh")
#' }
#' @export
verify_cohort <- function(con, schema) {
  checks <- c(); msgs <- c()

  window_tables <- c("admission_windows", "icu_windows", "visit_windows")
  for (tbl in window_tables) {
    exists <- .table_exists(con, schema, tbl)
    checks[paste0(tbl, "_exists")] <- exists
    if (!exists)
      msgs <- c(msgs, paste0(schema, ".", tbl, " does not exist"))
  }

  if (!all(checks[paste0(window_tables, "_exists")]))
    return(.make_result(checks, msgs))

  n_adm <- .row_count(con, schema, "admission_windows")
  checks["admission_windows_has_rows"] <- n_adm > 0
  if (!checks["admission_windows_has_rows"])
    msgs <- c(msgs, paste0(schema, ".admission_windows is empty"))

  n_icu <- .row_count(con, schema, "icu_windows")
  checks["icu_windows_row_count_available"] <- !is.na(n_icu)

  n_visit <- .row_count(con, schema, "visit_windows")
  checks["visit_windows_has_rows"] <- n_visit > 0
  if (!checks["visit_windows_has_rows"])
    msgs <- c(msgs, paste0(schema, ".visit_windows is empty"))

  if (checks["admission_windows_has_rows"] || checks["visit_windows_has_rows"])
    message("window tables: ",
            n_adm, " admission rows | ",
            n_icu, " ICU rows | ",
            n_visit, " visit rows in ", schema)

  .make_result(checks, msgs)
}

#' Verify raw tables were loaded into Postgres
#' @param con DBI connection.
#' @param schema Raw cohort schema.
#' @param required_tables Character vector of table names that must exist and
#'   have rows. NULL (the default) checks the three window tables plus the
#'   core tables of the schema's source family (`patients`/`admissions` for
#'   MIMIC, `patient` for eICU).
#' @return Verification result list.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' verify_db_load(con, "mimic_iv_demo_coh", required_tables = c("patients", "admissions"))
#' }
#' @export
verify_db_load <- function(con, schema, required_tables = NULL) {
  checks <- c(); msgs <- c()
  if (is.null(required_tables)) {
    fam <- .verify_schema_family(schema)
    required_tables <- c("admission_windows", "icu_windows", "visit_windows",
                         if (!is.na(fam)) .verify_core_tables[[fam]])
  }

  for (tbl in required_tables) {
    exists <- .table_exists(con, schema, tbl)
    checks[paste0(tbl, "_exists")] <- exists
    if (!exists) {
      msgs <- c(msgs, paste0(schema, ".", tbl, " does not exist"))
      next
    }
    n <- .row_count(con, schema, tbl)
    checks[paste0(tbl, "_has_rows")] <- n > 0
    if (n == 0) msgs <- c(msgs, paste0(schema, ".", tbl, " is empty"))
  }

  .make_result(checks, msgs)
}

#' Verify atomic concept tables were created
#' @param con DBI connection.
#' @param schema Concepts schema (e.g. "mimic_iv_demo_coh_concepts").
#' @param concept_names Character vector of concept table names. NULL checks
#'   that at least one table exists in the schema.
#' @return Verification result list.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' verify_concepts(con, "mimic_iv_demo_coh_concepts", concept_names = c("heart_rate", "sbp"))
#' }
#' @export
verify_concepts <- function(con, schema, concept_names = NULL) {
  checks <- c(); msgs <- c()

  if (is.null(concept_names)) {
    tbls <- DBI::dbGetQuery(con, glue::glue(
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = '{schema}'"
    ))$table_name
    checks["any_concept_table_exists"] <- length(tbls) > 0
    if (!checks["any_concept_table_exists"])
      msgs <- c(msgs, paste("No tables found in schema:", schema))
    else
      message("Found ", length(tbls), " concept tables in ", schema)
    return(.make_result(checks, msgs))
  }

  for (cn in concept_names) {
    exists <- .table_exists(con, schema, cn)
    checks[paste0(cn, "_exists")] <- exists
    if (!exists) {
      msgs <- c(msgs, paste0(schema, ".", cn, " does not exist"))
      next
    }
    n <- .row_count(con, schema, cn)
    checks[paste0(cn, "_has_rows")] <- n > 0
    if (n == 0) msgs <- c(msgs, paste0(schema, ".", cn, " is empty"))
  }

  .make_result(checks, msgs)
}

#' Verify handcrafted concept tables (SOFA, vitalsign, scores, etc.)
#' @param con DBI connection.
#' @param schema Schema where handcrafted concepts were written.
#' @param expected_tables Character vector of table names to check. NULL
#'   (the default) checks the standard MIMIC-code tables (SOFA, first-day
#'   summaries, scores) for MIMIC schemas, and that at least one handcrafted
#'   table exists for any other source family.
#' @return Verification result list.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' verify_handcrafted(con, "mimic_iv_demo_coh_concepts")
#' }
#' @export
verify_handcrafted <- function(con, schema, expected_tables = NULL) {
  checks <- c(); msgs <- c()
  if (is.null(expected_tables)) {
    fam <- .verify_schema_family(schema)
    if (!is.na(fam) && fam %in% c("mimic_iii", "mimic_iv")) {
      expected_tables <- .verify_mimic_handcrafted
    } else {
      res <- verify_concepts(con, schema)
      names(res$checks) <- "any_handcrafted_table_exists"
      return(res)
    }
  }

  for (tbl in expected_tables) {
    exists <- .table_exists(con, schema, tbl)
    checks[paste0(tbl, "_exists")] <- exists
    if (!exists) {
      msgs <- c(msgs, paste0(schema, ".", tbl, " does not exist"))
      next
    }
    n <- .row_count(con, schema, tbl)
    checks[paste0(tbl, "_has_rows")] <- n > 0
    if (n == 0) msgs <- c(msgs, paste0(schema, ".", tbl, " is empty"))
  }

  n_ok <- sum(checks, na.rm = TRUE)
  message("Handcrafted tables: ", n_ok, "/", length(checks),
          " checks passed in ", schema)

  .make_result(checks, msgs)
}

#' Verify harmonized ts_tab, flat_tab, and dur_tab outputs
#' @param con DBI connection.
#' @param schema Harmonized output schema (e.g. "mimic_iv_demo_coh_harmonized").
#' @param ts_table Name of the time-series table.
#' @param flat_table Name of the flat table.
#' @param dur_table Name of the duration table.
#' @param required_ts_cols Expected columns in ts_tab; NULL skips column check.
#' @return Verification result list.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' verify_harmonized(con, "mimic_iv_demo_coh_harmonized")
#' }
#' @export
verify_harmonized <- function(con, schema,
                              ts_table   = "ts_tab",
                              flat_table = "flat_tab",
                              dur_table  = "dur_tab",
                              required_ts_cols = c(
                                "time_since_hadm_min",
                                "concept_name", "value", "unit"
                              )) {
  checks <- c(); msgs <- c()

  ts_exists <- .table_exists(con, schema, ts_table)
  checks["ts_tab_exists"] <- ts_exists
  if (!ts_exists) {
    msgs <- c(msgs, paste0(schema, ".", ts_table, " does not exist"))
  } else {
    n_ts <- .row_count(con, schema, ts_table)
    checks["ts_tab_has_rows"] <- n_ts > 0
    if (n_ts == 0)
      msgs <- c(msgs, paste0(schema, ".", ts_table, " is empty"))
    else
      message(ts_table, ": ", format(n_ts, big.mark = ","), " rows in ", schema)

    if (!is.null(required_ts_cols)) {
      actual_cols <- DBI::dbGetQuery(con, glue::glue(
        "SELECT column_name FROM information_schema.columns
         WHERE table_schema = '{schema}' AND table_name = '{ts_table}'"
      ))$column_name
      missing_cols <- setdiff(required_ts_cols, actual_cols)
      checks["ts_tab_cols_ok"] <- length(missing_cols) == 0
      if (length(missing_cols) > 0)
        msgs <- c(msgs, paste("ts_tab missing columns:",
                              paste(missing_cols, collapse = ", ")))
    }
  }

  flat_exists <- .table_exists(con, schema, flat_table)
  checks["flat_tab_exists"] <- flat_exists
  if (!flat_exists) {
    msgs <- c(msgs, paste0(schema, ".", flat_table, " does not exist"))
  } else {
    n_flat <- .row_count(con, schema, flat_table)
    checks["flat_tab_has_rows"] <- n_flat > 0
    if (n_flat == 0)
      msgs <- c(msgs, paste0(schema, ".", flat_table, " is empty"))
    else
      message(flat_table, ": ", format(n_flat, big.mark = ","),
              " rows in ", schema)
  }

  dur_exists <- .table_exists(con, schema, dur_table)
  checks["dur_tab_exists"] <- dur_exists
  if (!dur_exists) {
    msgs <- c(msgs, paste0(schema, ".", dur_table, " does not exist"))
  } else {
    n_dur <- .row_count(con, schema, dur_table)
    checks["dur_tab_has_rows"] <- n_dur > 0
    if (n_dur == 0)
      msgs <- c(msgs, paste0(schema, ".", dur_table, " is empty"))
    else
      message(dur_table, ": ", format(n_dur, big.mark = ","),
              " rows in ", schema)
  }

  .make_result(checks, msgs)
}

#' Compare concept config item IDs against ricu's concept dictionary
#'
#' Returns a tibble of mismatches for manual review. Never auto-overwrites.
#'
#' @param concept_config_path Path to rendehr_concepts_config.csv.
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @return Tibble with columns: concept_name, your_match_value,
#'   ricu_match_value, class, mismatch_type. Empty = no mismatches.
#' @examples
#' \dontrun{
#' validate_concepts_against_ricu(
#'   system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"), "mimic_iv")
#' }
#' @export
validate_concepts_against_ricu <- function(concept_config_path, src_name) {
  if (!requireNamespace("ricu", quietly = TRUE))
    stop("ricu required: install.packages('ricu')")

  cfg <- readr::read_csv(concept_config_path, show_col_types = FALSE)
  cfg <- cfg[cfg$source == src_family(src_name), , drop = FALSE]

  ricu_src_key <- switch(src_family(src_name),
    mimic_iv  = "miiv",
    mimic_iii = "mimic",
    eicu      = "eicu",
    stop("Unknown source family: ", src_family(src_name))
  )

  ricu_dict <- tryCatch(
    ricu::concept(ricu_src_key),
    error = function(e)
      stop("Could not load ricu concepts for '", ricu_src_key, "': ",
           conditionMessage(e))
  )

  overlap <- intersect(cfg$concept_name, names(ricu_dict))
  if (length(overlap) == 0) {
    message("No overlapping concept names between config and ricu for ", src_name)
    return(tibble::tibble(
      concept_name     = character(),
      your_match_value = character(),
      ricu_match_value = character(),
      class            = character(),
      mismatch_type    = character()
    ))
  }

  mismatches <- purrr::map_dfr(overlap, function(cn) {
    your_rows  <- cfg[cfg$concept_name == cn, ]
    ricu_items <- tryCatch(ricu_dict[[cn]]$items, error = function(e) NULL)
    if (is.null(ricu_items)) return(NULL)

    ricu_ids <- tryCatch(
      as.character(unlist(lapply(ricu_items, function(x) x$ids))),
      error = function(e) character(0)
    )

    itm_rows <- your_rows[your_rows$itm_class %in% c("itm_itm", "rgx_itm"), ]
    your_ids <- trimws(unlist(strsplit(as.character(itm_rows$match_value), "|",
                                        fixed = TRUE)))

    only_yours <- setdiff(your_ids, ricu_ids)
    only_ricu  <- setdiff(ricu_ids, your_ids)

    result <- tibble::tibble(
      concept_name     = character(), your_match_value = character(),
      ricu_match_value = character(), class = character(),
      mismatch_type    = character()
    )

    if (length(only_yours) > 0)
      result <- dplyr::bind_rows(result, tibble::tibble(
        concept_name     = cn, your_match_value = only_yours,
        ricu_match_value = NA_character_, class = "itm_itm",
        mismatch_type    = "in_yours_not_in_ricu"
      ))
    if (length(only_ricu) > 0)
      result <- dplyr::bind_rows(result, tibble::tibble(
        concept_name     = cn, your_match_value = NA_character_,
        ricu_match_value = only_ricu, class = "itm_itm",
        mismatch_type    = "in_ricu_not_in_yours"
      ))
    result
  })

  n_mis <- nrow(mismatches)
  message("ricu cross-check: ", length(overlap), " concepts compared, ",
          n_mis, " item-level mismatches found")

  mismatches
}

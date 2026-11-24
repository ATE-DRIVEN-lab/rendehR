test_that("verify_download: fails cleanly when parquet dir does not exist", {
  paths <- ehr_paths(src_name = "fake_source_99", base_dir = tempdir())
  result <- verify_download(paths)
  expect_false(result$ok)
  expect_true("parquet_dir_exists" %in% names(result$checks))
  expect_false(result$checks["parquet_dir_exists"])
})

test_that("verify_download: fails when dir exists but has no parquet files", {
  tmp <- file.path(tempdir(), "rendehr_test_empty_parquet")
  dir.create(tmp, showWarnings = FALSE)
  paths <- list(cohort_parquet_dir = tmp)
  result <- verify_download(paths)
  expect_false(result$ok)
  expect_false(result$checks["has_parquet_files"])
  unlink(tmp, recursive = TRUE)
})

test_that("verify_download: passes when parquet files are present", {
  tmp    <- file.path(tempdir(), "rendehr_test_parquet")
  tbl    <- file.path(tmp, "admissions")
  dir.create(tbl, recursive = TRUE, showWarnings = FALSE)
  writeLines("fake", file.path(tbl, "part-0.parquet"))
  paths  <- list(cohort_parquet_dir = tmp)
  result <- verify_download(paths)
  expect_true(result$ok)
  unlink(tmp, recursive = TRUE)
})

test_that("verify_download: checks required_tables when specified", {
  tmp <- file.path(tempdir(), "rendehr_test_required")
  dir.create(file.path(tmp, "admissions"), recursive = TRUE,
             showWarnings = FALSE)
  writeLines("fake", file.path(tmp, "admissions", "p0.parquet"))
  paths <- list(cohort_parquet_dir = tmp)

  result_ok  <- verify_download(paths, required_tables = "admissions")
  result_bad <- verify_download(paths, required_tables = c("admissions", "labevents"))

  expect_true(result_ok$ok)
  expect_false(result_bad$ok)
  expect_false(result_bad$checks["parquet_labevents"])

  unlink(tmp, recursive = TRUE)
})

# .row_count() validate-then-quote: a malformed schema or table name must
# be rejected before the identifier reaches an executed query.
# .table_exists() (a WHERE-clause string comparison, not an identifier
# position, so it needs no identifier validation) is checked first by every
# verify_*() caller and simply
# reports a malformed name as "does not exist" rather than raising, so the
# rejection is tested directly against .row_count() (internal, via :::)
# instead of through a verify_*() wrapper that would never reach it for a
# nonexistent table. DuckDB supports information_schema and DBI::Id()-quoted
# identifiers identically to PostgreSQL for this path, so this is fully
# testable here (not left to the validation harness).
test_that(".row_count: rejects a malformed schema or table name before any query runs", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s1")
  DBI::dbWriteTable(con, DBI::Id(schema = "s1", table = "t1"), data.frame(a = 1:3))

  expect_error(
    rendehR:::.row_count(con, "bad schema; DROP TABLE x; --", "t1"),
    "Invalid schema name"
  )
  expect_error(
    rendehR:::.row_count(con, "s1", "bad;table"),
    "Invalid table name"
  )
  # legitimate identifiers still work unchanged
  expect_equal(rendehR:::.row_count(con, "s1", "t1"), 3)
})

.validate_cohort_fixture <- function() {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, "CREATE SCHEMA raw_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "admission_windows"),
                    data.frame(hadm_id = 1:3))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "icu_windows"),
                    data.frame(stay_id = 1:2))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "visit_windows"),
                    data.frame(hadm_id = 1:3))
  con
}

test_that("verify_cohort: passes when all three window tables exist with rows", {
  con <- .validate_cohort_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  result <- verify_cohort(con, "raw_s")
  expect_true(result$ok)
  expect_true(result$checks[["admission_windows_exists"]])
  expect_true(result$checks[["admission_windows_has_rows"]])
  expect_true(result$checks[["visit_windows_has_rows"]])
})

test_that("verify_cohort: fails when a window table is missing", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA raw_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "admission_windows"),
                    data.frame(hadm_id = 1:3))
  result <- verify_cohort(con, "raw_s")
  expect_false(result$ok)
  expect_true(any(grepl("icu_windows does not exist", result$messages)))
})

test_that("verify_db_load: passes when required tables exist with rows, fails on empty table", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA raw_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "patients"),
                    data.frame(subject_id = 1:3))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "admissions"),
                    data.frame(hadm_id = integer(0)))

  result <- verify_db_load(con, "raw_s",
                           required_tables = c("patients", "admissions"))
  expect_false(result$ok)
  expect_true(result$checks[["patients_has_rows"]])
  expect_false(result$checks[["admissions_has_rows"]])
})

test_that("verify_concepts: any-table mode and named-concept mode both work", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA concepts_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "concepts_s", table = "heart_rate"),
                    data.frame(ehr_id = 1:3, value = c(80, 90, 100)))

  any_result <- verify_concepts(con, "concepts_s")
  expect_true(any_result$ok)

  named_result <- verify_concepts(con, "concepts_s", concept_names = "heart_rate")
  expect_true(named_result$ok)
  expect_true(named_result$checks[["heart_rate_has_rows"]])

  missing_result <- verify_concepts(con, "concepts_s", concept_names = "sofa")
  expect_false(missing_result$ok)
})

test_that("verify_handcrafted: reports per-table pass/fail against a custom expected_tables list", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA raw_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "sofa"),
                    data.frame(ehr_id = 1:3, sofa = c(2, 4, 6)))

  result <- verify_handcrafted(con, "raw_s", expected_tables = c("sofa", "oasis"))
  expect_false(result$ok)
  expect_true(result$checks[["sofa_exists"]])
  expect_true(result$checks[["sofa_has_rows"]])
  expect_true(any(grepl("oasis does not exist", result$messages)))
})

.validate_harmonized_fixture <- function() {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, "CREATE SCHEMA harmonized_s")
  # Deliberately no "ehr_id" column: the real per-source ts_tab never has
  # one (only merge_sources()'s cross-source output does). The fixture uses
  # realistic source-native ID columns so that a default required_ts_cols
  # that wrongly expects ehr_id fails here instead of being masked.
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "ts_tab"),
                    data.frame(subject_id = 1:2, time_since_hadm_min = c(10, 20),
                              concept_name = c("hr", "hr"), value = c(80, 82),
                              unit = c("bpm", "bpm")))
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "flat_tab"),
                    data.frame(subject_id = 1:2, sex = c("M", "F")))
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "dur_tab"),
                    data.frame(subject_id = 1:2, start_time = c(1, 2), end_time = c(3, 4)))
  con
}

test_that("verify_harmonized: passes when ts_tab/flat_tab/dur_tab all exist with rows and expected columns", {
  con <- .validate_harmonized_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  result <- verify_harmonized(con, "harmonized_s")
  expect_true(result$ok)
  expect_true(result$checks[["ts_tab_cols_ok"]])
  expect_true(result$checks[["dur_tab_exists"]])
  expect_true(result$checks[["dur_tab_has_rows"]])
})

test_that("verify_harmonized: default required_ts_cols does not require a nonexistent ehr_id column", {
  con <- .validate_harmonized_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_false("ehr_id" %in% eval(formals(verify_harmonized)$required_ts_cols))
  result <- verify_harmonized(con, "harmonized_s")
  expect_true(result$checks[["ts_tab_cols_ok"]])
})

test_that("verify_harmonized: fails when dur_tab is missing, even though ts_tab/flat_tab are fine", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA harmonized_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "ts_tab"),
                    data.frame(subject_id = 1:2, time_since_hadm_min = c(10, 20),
                              concept_name = c("hr", "hr"), value = c(80, 82),
                              unit = c("bpm", "bpm")))
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "flat_tab"),
                    data.frame(subject_id = 1:2, sex = c("M", "F")))

  result <- verify_harmonized(con, "harmonized_s")
  expect_false(result$ok)
  expect_true(result$checks[["ts_tab_has_rows"]])
  expect_true(result$checks[["flat_tab_has_rows"]])
  expect_false(result$checks[["dur_tab_exists"]])
  expect_true(any(grepl("dur_tab does not exist", result$messages)))
})

test_that("verify_harmonized: fails and reports missing columns when ts_tab lacks a required column", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA harmonized_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "ts_tab"),
                    data.frame(subject_id = 1:2, value = c(80, 82)))
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "flat_tab"),
                    data.frame(subject_id = 1:2))
  DBI::dbWriteTable(con, DBI::Id(schema = "harmonized_s", table = "dur_tab"),
                    data.frame(subject_id = 1:2))

  result <- verify_harmonized(con, "harmonized_s")
  expect_false(result$checks[["ts_tab_cols_ok"]])
  expect_true(any(grepl("ts_tab missing columns", result$messages)))
})

test_that("validate_concepts_against_ricu: errors clearly when ricu is unavailable", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) if (identical(package, "ricu")) FALSE else TRUE,
    .package = "base"
  )
  expect_error(
    validate_concepts_against_ricu("unused.csv", "mimic_iv_demo"),
    "ricu required"
  )
})

test_that("validate_concepts_against_ricu: reports item-level mismatches against a stubbed ricu dictionary", {
  skip_if_not_installed("ricu")
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  readr::write_csv(data.frame(
    source       = "mimic_iv",
    concept_name = c("heart_rate", "heart_rate", "sbp"),
    itm_class    = c("itm_itm", "itm_itm", "itm_itm"),
    match_value  = c("220045", "999999", "220050")
  ), tmp_csv)

  fake_dict <- list(
    heart_rate = list(items = list(list(ids = "220045"), list(ids = "220046"))),
    sbp        = list(items = list(list(ids = "220050")))
  )
  testthat::local_mocked_bindings(
    concept = function(x) fake_dict,
    .package = "ricu"
  )

  result <- validate_concepts_against_ricu(tmp_csv, "mimic_iv_demo")
  expect_true(all(c("concept_name", "your_match_value", "ricu_match_value",
                    "class", "mismatch_type") %in% names(result)))
  expect_true("999999" %in% result$your_match_value)
  expect_true("220046" %in% result$ricu_match_value)
  expect_false("220050" %in% c(result$your_match_value, result$ricu_match_value))
})

test_that("validate_concepts_against_ricu: splits pipe-delimited match_value before diffing", {
  skip_if_not_installed("ricu")
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  readr::write_csv(data.frame(
    source       = "mimic_iv",
    concept_name = "dbp",
    itm_class    = "itm_itm",
    match_value  = "8368|8441|220051"
  ), tmp_csv)

  fake_dict <- list(
    dbp = list(items = list(list(ids = c(8368, 8441, 220051))))
  )
  testthat::local_mocked_bindings(
    concept = function(x) fake_dict,
    .package = "ricu"
  )

  result <- validate_concepts_against_ricu(tmp_csv, "mimic_iv_demo")
  expect_equal(nrow(result), 0)
})

# .assert_ehr_schema is not exported, so we access it via :::
test_that(".assert_ehr_schema accepts valid ehr schema names", {
  valid <- c("mimic_iv_demo_coh", "eicu_coh_concepts", "mimic_iii_coh_harmonized",
             "mimic_iv_coh_harmonized", "eicu_demo_coh_harmonized", "mimic_iii_coh_derived")
  for (s in valid)
    expect_no_error(rendehR:::.assert_ehr_schema(s))
})

test_that(".assert_ehr_schema rejects non-ehr schema names", {
  invalid <- c("public", "pg_catalog", "information_schema",
               "my_random_schema", "mimic_iv_demo")
  for (s in invalid)
    expect_error(rendehR:::.assert_ehr_schema(s))
})

test_that("verify_download: accepts flat <table>.parquet artifacts", {
  tmp <- tempfile("verify-flat-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("fake", file.path(tmp, "admissions.parquet"))
  result <- verify_download(list(cohort_parquet_dir = tmp),
                            required_tables = c("admissions", "labevents"))
  expect_true(result$checks[["parquet_admissions"]])
  expect_false(result$checks[["parquet_labevents"]])
})

test_that("verify_db_load: default required tables follow the source family", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA eicu_demo_coh")
  for (tbl in c("admission_windows", "icu_windows", "visit_windows", "patient")) {
    DBI::dbWriteTable(con, DBI::Id(schema = "eicu_demo_coh", table = tbl),
                      data.frame(patientunitstayid = 1:2))
  }
  result <- verify_db_load(con, "eicu_demo_coh")
  expect_true(result$ok)
  expect_false(any(grepl("admissions", names(result$checks))))
})

test_that("verify_handcrafted: non-MIMIC default needs only some table", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA eicu_demo_coh_derived")
  DBI::dbWriteTable(con, DBI::Id(schema = "eicu_demo_coh_derived",
                                 table = "icustay_detail"),
                    data.frame(patientunitstayid = 1:2))
  expect_true(suppressMessages(verify_handcrafted(con, "eicu_demo_coh_derived"))$ok)
})

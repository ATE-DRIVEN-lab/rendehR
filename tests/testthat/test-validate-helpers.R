# Validation/stage-readiness helpers in R/pipeline.R
# (.ehr_table_exists, .ehr_table_count, .ehr_table_has_columns,
# .ehr_valid_table, .ehr_stage_log_table, .ehr_write_stage_log,
# .ehr_stage_has_success_log, .ehr_parquet_files_valid).
#
# .ehr_validate_stage() opens its own PostgreSQL connection internally (via
# .ehr_with_pipeline_con()/connect_postgres()) and cannot be redirected to a
# test database, like run_harmonized_tables()/merge_sources() in
# test-harmonize.R/test-merge.R; its correctness is covered by the
# validation harness (validation/) instead. The other eight helpers take a
# caller-supplied connection and are fully testable against DuckDB here.

test_that(".ehr_table_exists: reports existence correctly", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s1")
  DBI::dbWriteTable(con, DBI::Id(schema = "s1", table = "t1"), data.frame(a = 1:3))

  expect_true(rendehR:::.ehr_table_exists(con, "s1", "t1"))
  expect_false(rendehR:::.ehr_table_exists(con, "s1", "nope"))
})

test_that(".ehr_table_count: rejects a malformed schema or table name before any query runs", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s1")
  DBI::dbWriteTable(con, DBI::Id(schema = "s1", table = "t1"), data.frame(a = 1:3))

  expect_error(
    rendehR:::.ehr_table_count(con, "bad;schema", "t1"),
    "Invalid schema name"
  )
  expect_error(
    rendehR:::.ehr_table_count(con, "s1", "bad table"),
    "Invalid table name"
  )
  expect_equal(rendehR:::.ehr_table_count(con, "s1", "t1"), 3)
})

test_that(".ehr_table_has_columns: reports column presence correctly", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s1")
  DBI::dbWriteTable(con, DBI::Id(schema = "s1", table = "t1"),
                    data.frame(a = 1:3, b = 4:6))

  expect_true(rendehR:::.ehr_table_has_columns(con, "s1", "t1", c("a", "b")))
  expect_false(rendehR:::.ehr_table_has_columns(con, "s1", "t1", c("a", "z")))
  expect_false(rendehR:::.ehr_table_has_columns(con, "s1", "nope", "a"))
})

test_that(".ehr_valid_table: combines existence/column/row checks into ok/reason", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s1")
  DBI::dbWriteTable(con, DBI::Id(schema = "s1", table = "t1"), data.frame(a = 1:3))
  DBI::dbWriteTable(con, DBI::Id(schema = "s1", table = "empty_t"),
                    data.frame(a = integer(0)))

  missing <- rendehR:::.ehr_valid_table(con, "s1", "nope")
  expect_false(missing$ok)
  expect_match(missing$reason, "does not exist")

  bad_cols <- rendehR:::.ehr_valid_table(con, "s1", "t1", columns = "z")
  expect_false(bad_cols$ok)
  expect_match(bad_cols$reason, "missing columns")

  empty <- rendehR:::.ehr_valid_table(con, "s1", "empty_t")
  expect_false(empty$ok)
  expect_match(empty$reason, "zero rows")

  empty_allowed <- rendehR:::.ehr_valid_table(con, "s1", "empty_t", allow_empty = TRUE)
  expect_true(empty_allowed$ok)

  ok <- rendehR:::.ehr_valid_table(con, "s1", "t1", columns = "a")
  expect_true(ok$ok)
  expect_match(ok$reason, "valid \\(3 rows\\)")
})

test_that(".ehr_stage_log_table: rejects a malformed schema name; creates an idempotent pipeline_log table", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s1")

  expect_error(
    rendehR:::.ehr_stage_log_table(con, "bad;schema"),
    "Invalid schema name"
  )

  rendehR:::.ehr_stage_log_table(con, "s1")
  expect_true(rendehR:::.ehr_table_exists(con, "s1", "pipeline_log"))
  # idempotent: calling again does not error or duplicate the table
  expect_no_error(rendehR:::.ehr_stage_log_table(con, "s1"))
  expect_equal(rendehR:::.ehr_table_count(con, "s1", "pipeline_log"), 0)
})

test_that(".ehr_write_stage_log/.ehr_stage_has_success_log: create-write-read round trip is consistent", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s1")

  expect_false(rendehR:::.ehr_stage_has_success_log(con, "s1", "run_db", "mimic_iv_demo", "my_db"))

  rendehR:::.ehr_write_stage_log(con, "s1", stage = "run_db", status = "success",
                             src_name = "mimic_iv_demo", db_name = "my_db",
                             base_dir = "/tmp/proj")
  expect_true(rendehR:::.ehr_stage_has_success_log(con, "s1", "run_db", "mimic_iv_demo", "my_db"))

  # a different stage/source/db combination is not conflated with the logged one
  expect_false(rendehR:::.ehr_stage_has_success_log(con, "s1", "run_db", "eicu_demo", "my_db"))
  expect_false(rendehR:::.ehr_stage_has_success_log(con, "s1", "run_raw_mods", "mimic_iv_demo", "my_db"))

  rendehR:::.ehr_write_stage_log(con, "s1", stage = "run_db", status = "failed",
                             src_name = "mimic_iv_demo", db_name = "my_db",
                             base_dir = "/tmp/proj")
  # most-recent-row semantics: the newest row for this key is "failed", not "success"
  log <- DBI::dbReadTable(con, DBI::Id(schema = "s1", table = "pipeline_log"))
  expect_equal(nrow(log), 2)
})

test_that(".ehr_parquet_files_valid: reports presence/readability of parquet files", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("arrow")

  missing_dir <- rendehR:::.ehr_parquet_files_valid(list(cohort_parquet_dir = tempfile()))
  expect_false(missing_dir$ok)
  expect_match(missing_dir$reason, "Missing parquet directory")

  tmp <- file.path(tempdir(), "s009_parquet_valid_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  empty_dir <- rendehR:::.ehr_parquet_files_valid(list(cohort_parquet_dir = tmp))
  expect_false(empty_dir$ok)
  expect_match(empty_dir$reason, "No parquet files")

  arrow::write_parquet(data.frame(a = 1:3), file.path(tmp, "t1.parquet"))
  valid <- rendehR:::.ehr_parquet_files_valid(list(cohort_parquet_dir = tmp))
  expect_true(valid$ok)

  writeLines("not a real parquet file", file.path(tmp, "bad.parquet"))
  invalid <- rendehR:::.ehr_parquet_files_valid(list(cohort_parquet_dir = tmp))
  expect_false(invalid$ok)
  expect_match(invalid$reason, "Unreadable parquet files")
})

test_that(".ehr_validate_stage: full execution requires a real PostgreSQL target", {
  skip("`.ehr_validate_stage()` opens its own live PostgreSQL connection via \
.ehr_with_pipeline_con()/connect_postgres() and cannot be redirected to a \
test database; it is covered by the real-data \
validation, not a unit test.")
})

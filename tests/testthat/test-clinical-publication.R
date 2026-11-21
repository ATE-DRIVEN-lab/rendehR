test_that("Transactional publication preserves prior state on failure", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA clinical")
  DBI::dbWriteTable(con, DBI::Id(schema = "clinical", table = "cohort_info"),
                    data.frame(ehr_id = "old", source_data = "old"))

  expect_error(.clinical_publish_table(
    con, "clinical", "cohort_info",
    data.frame(ehr_id = "new", source_data = "new"),
    required_columns = c("ehr_id", "source_data"),
    inject_failure = "after_promote"), "Injected")

  got <- DBI::dbReadTable(
    con, DBI::Id(schema = "clinical", table = "cohort_info"))
  expect_identical(got$ehr_id, "old")
  expect_false(any(grepl("__(stage|backup)_", DBI::dbListTables(con))))
})

test_that("Publication validates a complete frame before replacing anything", {
  expect_error(.clinical_validate_frame(
    data.frame(ehr_id = c("x", "x")), "cohort_info",
    required_columns = c("ehr_id", "source_data")), "missing")
  expect_error(.clinical_validate_frame(
    data.frame(ehr_id = c("x", "x"), source_data = "s"), "cohort_info",
    unique_columns = "ehr_id"), "unique-key")
})

test_that("Publication republishes an indexed table on DuckDB and rolls the index back on failure", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA clinical")
  idx <- list(cohort_info = list(list(name = "idx_cohort_info_ehr_id", columns = "ehr_id")))
  pub <- function(id, ...) .clinical_publish_tables(
    con, "clinical", list(cohort_info = data.frame(ehr_id = id, source_data = "s")),
    indexes = idx, ...)
  n_idx <- function() DBI::dbGetQuery(con,
    "SELECT count(*) AS n FROM duckdb_indexes() WHERE table_name = 'cohort_info'")$n
  pub("first")
  expect_equal(n_idx(), 1)
  # a second publication must succeed although DuckDB cannot rename a table with an index
  pub("second")
  expect_identical(DBI::dbReadTable(con, DBI::Id(schema = "clinical", table = "cohort_info"))$ehr_id, "second")
  expect_equal(n_idx(), 1)
  expect_error(pub("third", inject_failure = "after_promote"), "Injected")
  expect_identical(DBI::dbReadTable(con, DBI::Id(schema = "clinical", table = "cohort_info"))$ehr_id, "second")
  expect_equal(n_idx(), 1)
})

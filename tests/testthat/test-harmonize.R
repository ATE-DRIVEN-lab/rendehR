# build_ts_table()/build_dur_table() query the PostgreSQL-only pg_tables
# catalog, so their full happy-path execution cannot be exercised against
# DuckDB in a unit test; that path is covered by the validation harness
# (validation/) against a disposable PostgreSQL database instead. Their
# identifier validation, however, runs before any catalog query and is fully
# testable here.

test_that("build_ts_table: rejects a malformed table_name before any query runs", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_error(
    build_ts_table(con, "mimic_iv_demo", list(harmonized = "h"), "heart_rate",
                   table_name = "bad; DROP TABLE x; --"),
    "Invalid table name"
  )
})

test_that("build_ts_table: rejects a malformed harmonized schema name", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_error(
    build_ts_table(con, "mimic_iv_demo", list(harmonized = "bad schema"), "heart_rate"),
    "Invalid schema name"
  )
})

test_that("build_dur_table: rejects a malformed table_name before any query runs", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_error(
    build_dur_table(con, "mimic_iv_demo", list(harmonized = "h"), "abx",
                    table_name = "bad name"),
    "Invalid table name"
  )
})

test_that("build_flat_table: rejects a malformed table_name/schema", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_error(
    build_flat_table(con, "mimic_iv_demo", list(harmonized = "h"), character(0),
                     table_name = "bad\"name"),
    "Invalid table name"
  )
  expect_error(
    build_flat_table(con, "mimic_iv_demo", list(harmonized = "bad schema"), character(0)),
    "Invalid schema name"
  )
})

# build_flat_table() only touches dbListFields(), an ANSI-portable probe
# SELECT, and DROP/CREATE TABLE - none of which are PostgreSQL-specific - so
# its full happy path, including the ambiguous-key warning, is
# genuinely testable end-to-end against DuckDB.

.harmonize_flat_fixture <- function() {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, "CREATE SCHEMA raw_s")
  DBI::dbExecute(con, "CREATE SCHEMA concepts_s")
  DBI::dbExecute(con, "CREATE SCHEMA harmonized_s")
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "visit_windows"),
                    data.frame(subject_id = 1:3, hadm_id = 101:103, stay_id = 201:203))
  DBI::dbWriteTable(con, DBI::Id(schema = "concepts_s", table = "sex"),
                    data.frame(subject_id = 1:3, hadm_id = 101:103, stay_id = 201:203,
                              sex = "M"))
  DBI::dbWriteTable(con, DBI::Id(schema = "concepts_s", table = "ambig"),
                    data.frame(subject_id = 1:3, hadm_id = 101:103, stay_id = 201:203,
                              valA = 1:3, valB = 4:6))
  con
}

test_that("build_flat_table: builds the expected wide table from concept tables", {
  con <- .harmonize_flat_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  schema_names <- list(raw = "raw_s", concepts = "concepts_s", harmonized = "harmonized_s")

  suppressWarnings(
    build_flat_table(con, "mimic_iv_demo", schema_names, c("sex", "ambig"),
                     table_name = "flat_tab")
  )
  out <- DBI::dbReadTable(con, DBI::Id(schema = "harmonized_s", table = "flat_tab"))
  expect_equal(nrow(out), 3)
  expect_true(all(c("source_data", "subject_id", "hadm_id", "stay_id", "sex", "ambig")
                  %in% names(out)))
  expect_equal(out$sex, rep("M", 3))
})

test_that("build_flat_table: hybrid joins preserve NULL stays and exact concept values", {
  con <- .harmonize_flat_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ids <- data.frame(subject_id = c(1L, 1L, 2L, 3L),
                    hadm_id = c(101L, 101L, 102L, 103L),
                    stay_id = c(NA_integer_, 201L, NA_integer_, 203L))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw_s", table = "visit_windows"),
                   ids, overwrite = TRUE)
  values <- transform(ids[1:3, ], sex = c("F", "M", "F"))
  DBI::dbWriteTable(con, DBI::Id(schema = "concepts_s", table = "sex"),
                   values, overwrite = TRUE)
  build_flat_table(con, "mimic_iv_demo",
                   list(raw = "raw_s", concepts = "concepts_s", harmonized = "harmonized_s"),
                   "sex")
  out <- DBI::dbGetQuery(con, "SELECT subject_id, hadm_id, stay_id, sex
    FROM harmonized_s.flat_tab ORDER BY subject_id, stay_id NULLS FIRST")
  expect_equal(out, transform(ids, sex = c("F", "M", "F", NA_character_)))

  # Sources/cohorts with no non-NULL key must retain the original semantics.
  DBI::dbExecute(con, "UPDATE raw_s.visit_windows SET subject_id = NULL, hadm_id = NULL")
  DBI::dbExecute(con, "UPDATE concepts_s.sex SET subject_id = NULL, hadm_id = NULL")
  # A flat concept holds one row per key (build_flat_table() skips one that
  # does not), so keep a single all-NULL-key row.
  DBI::dbExecute(con, "DELETE FROM concepts_s.sex WHERE stay_id IS NULL")
  DBI::dbExecute(con, "INSERT INTO concepts_s.sex VALUES (NULL, NULL, NULL, 'F')")
  expected <- DBI::dbGetQuery(con, "SELECT b.stay_id, t.sex
    FROM raw_s.visit_windows b LEFT JOIN concepts_s.sex t
    ON b.subject_id IS NOT DISTINCT FROM t.subject_id
    AND b.hadm_id IS NOT DISTINCT FROM t.hadm_id
    AND b.stay_id IS NOT DISTINCT FROM t.stay_id
    ORDER BY b.stay_id NULLS FIRST, t.sex")
  build_flat_table(con, "mimic_iv_demo",
                   list(raw = "raw_s", concepts = "concepts_s", harmonized = "harmonized_s"),
                   "sex")
  actual <- DBI::dbGetQuery(con, "SELECT stay_id, sex FROM harmonized_s.flat_tab
    ORDER BY stay_id NULLS FIRST, sex")
  expect_equal(actual, expected)
})

test_that("build_flat_table: warns exactly which column it picked on ambiguity", {
  con <- .harmonize_flat_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  schema_names <- list(raw = "raw_s", concepts = "concepts_s", harmonized = "harmonized_s")

  w <- character()
  withCallingHandlers(
    build_flat_table(con, "mimic_iv_demo", schema_names, "ambig", table_name = "flat_tab"),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") }
  )
  # exactly once, although the column is used by both the probe and the build
  expect_length(w, 1)
  expect_match(w, "using 'valA'.*Skipped: valB")
})

test_that("build_flat_table: no warning when the value column matches the concept name", {
  con <- .harmonize_flat_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  schema_names <- list(raw = "raw_s", concepts = "concepts_s", harmonized = "harmonized_s")

  expect_no_warning(
    build_flat_table(con, "mimic_iv_demo", schema_names, "sex", table_name = "flat_tab")
  )
})

# run_harmonized_tables() opens its own real PostgreSQL connection internally
# (via connect_postgres()) and cannot be redirected to a test database; its
# correctness is covered by the validation harness (validation/) instead.
test_that("run_harmonized_tables: full execution requires a real PostgreSQL target", {
  skip("run_harmonized_tables() opens its own live PostgreSQL connection; \
covered by the validation harness, not a unit test.")
})

test_that("build_flat_table: a flat concept with several rows per key is skipped, not fanned out", {
  con <- .harmonize_flat_fixture()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, DBI::Id(schema = "concepts_s", table = "multi"),
                    data.frame(subject_id = c(1L, 1L, 2L), hadm_id = c(101L, 101L, 102L),
                               stay_id = c(201L, 201L, 202L), multi = c(1, 2, 3)))
  expect_warning(
    build_flat_table(con, "mimic_iv_demo",
                     list(raw = "raw_s", concepts = "concepts_s", harmonized = "harmonized_s"),
                     c("sex", "multi")),
    "multi.*skipped")
  out <- DBI::dbReadTable(con, DBI::Id(schema = "harmonized_s", table = "flat_tab"))
  expect_equal(nrow(out), 3)
  expect_true("sex" %in% names(out))
  expect_false("multi" %in% names(out))
})

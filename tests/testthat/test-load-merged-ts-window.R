# load_merged_ts(window_min =): server-side time window on the merged ts.
#
# Fixture: one admission, one concept, readings at -60, 0, 720, 1439, 1440 and
# NULL minutes on the hospital clock; the ICU clock runs 120 min behind.

.window_fixture <- function(con, schema = "win_test") {
  DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", schema))
  t_hadm <- c(-60, 0, 720, 1439, 1440, NA)
  DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "ts"), data.frame(
    ehr_id = "1_10", source_data = "src_a", src_admission_id = "10",
    concept_name = "lactate", value = as.character(seq_along(t_hadm)),
    unit = "mmol/L", time_since_hadm_min = t_hadm,
    time_since_icu_min = t_hadm - 120))
  schema
}

.window_con <- function() {
  skip_if_not_installed("duckdb")
  connect_duckdb(":memory:")
}

test_that("window_min keeps [from, to) on the admission clock", {
  con <- .window_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  schema <- .window_fixture(con)

  ts <- load_merged_ts(schema, cohort_only = FALSE, window_min = c(0, 1440),
                       con = con)
  expect_setequal(ts$time_since_hadm_min, c(0, 720, 1439))
})

test_that("window_min = NULL returns every row, as before", {
  con <- .window_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  schema <- .window_fixture(con)

  expect_identical(nrow(load_merged_ts(schema, cohort_only = FALSE, con = con)), 6L)
})

test_that("window_ref = 'icu' filters on time_since_icu_min", {
  con <- .window_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  schema <- .window_fixture(con)

  ts <- load_merged_ts(schema, cohort_only = FALSE, window_min = c(0, 1440),
                       window_ref = "icu", con = con)
  expect_setequal(ts$time_since_icu_min, c(600, 1319, 1320))
})

test_that("window_min combines with concepts, cols and n", {
  con <- .window_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  schema <- .window_fixture(con)

  ts <- load_merged_ts(schema, concepts = "lactate", cols = "minimal",
                       cohort_only = FALSE, window_min = c(-60, 1), con = con)
  expect_setequal(ts$time_since_hadm_min, c(-60, 0))
  expect_identical(nrow(load_merged_ts(schema, concepts = "sodium",
                                       cohort_only = FALSE,
                                       window_min = c(0, 1440), con = con)), 0L)
  expect_identical(nrow(load_merged_ts(schema, n = 1, cohort_only = FALSE,
                                       window_min = c(0, 1440), con = con)), 1L)
})

test_that("window_min is validated before touching the database", {
  bad <- list("a", 1, c(1, 2, 3), c(10, 5), c(5, 5), c(0, Inf), c(NA, 1))
  for (w in bad)
    expect_error(load_merged_ts("s", window_min = w), "window_min")
  expect_error(load_merged_ts("s", window_min = c(0, 1), window_ref = "x"),
               "should be one of")
})

test_that("window_ref errors when the merged ts lacks that clock", {
  con <- .window_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA no_icu")
  DBI::dbWriteTable(con, DBI::Id(schema = "no_icu", table = "ts"), data.frame(
    ehr_id = "1_10", source_data = "src_a", src_admission_id = "10",
    concept_name = "lactate", value = "1", time_since_hadm_min = 0))

  expect_error(load_merged_ts("no_icu", cohort_only = FALSE,
                              window_min = c(0, 1), window_ref = "icu", con = con),
               "no time_since_icu_min column")
})

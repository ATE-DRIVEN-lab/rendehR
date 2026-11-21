test_that("progress submits identical SQL in one DBI call in either mode", {
  skip_if_not_installed("DBI")
  calls <- list()
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, immediate) {
      calls[[length(calls) + 1L]] <<- list(conn, statement, immediate)
      0L
    }, .package = "DBI"
  )
  sql <- "-- keep comments;\nCREATE TABLE x AS SELECT 'a;b' AS v; SELECT * FROM x;"
  messages <- capture.output(
    .duckdb_execute_setup_sql("connection", sql, "fixture.sql", TRUE),
    type = "message"
  )
  quiet <- capture.output(
    .duckdb_execute_setup_sql("connection", sql, "fixture.sql", FALSE),
    type = "message"
  )
  expect_identical(calls, rep(list(list("connection", sql, TRUE)), 2L))
  expect_match(messages, "setup_sql fixture.sql done \\([0-9.]+s\\)")
  expect_length(quiet, 0L)
})

test_that("file timing preserves dependent SQL execution and results", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sql <- paste(
    "-- header;\nCREATE OR REPLACE TABLE progress_input AS SELECT 'a;b' AS value;",
    "CREATE OR REPLACE TABLE progress_output AS SELECT * FROM progress_input;",
    "DROP TABLE progress_input;"
  )
  for (progress in c(FALSE, TRUE)) {
    suppressMessages(.duckdb_execute_setup_sql(con, sql, "fixture.sql", progress))
    expect_identical(DBI::dbGetQuery(con, "SELECT * FROM progress_output")$value, "a;b")
    expect_false(DBI::dbExistsTable(con, "progress_input"))
  }
})

test_that("file timing preserves the original database error condition", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sql <- "SELECT * FROM missing_progress_table"
  original <- tryCatch(DBI::dbExecute(con, sql, immediate = TRUE), error = identity)
  messages <- capture.output(
    timed <- tryCatch(.duckdb_execute_setup_sql(con, sql, "failure.sql"), error = identity),
    type = "message"
  )
  expect_identical(class(timed), class(original))
  expect_identical(conditionMessage(timed), conditionMessage(original))
  expect_match(messages, "setup_sql failure.sql FAILED \\([0-9.]+s\\)")
})

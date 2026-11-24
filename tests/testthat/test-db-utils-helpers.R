test_that("suggest_match() prefers prefixes, then substrings, then close spellings", {
  ch <- c("heart_rate", "hemoglobin", "creatinine", "sbp")
  expect_identical(suggest_match("he", ch), c("heart_rate", "hemoglobin"))
  expect_identical(suggest_match("he", ch, max_suggestions = 1), "heart_rate")
  expect_identical(suggest_match("RATE", ch), "heart_rate")
  expect_identical(suggest_match("creatinin", ch), "creatinine")
  expect_identical(suggest_match("kreatinine", ch), "creatinine")
  expect_identical(suggest_match("zzzzzz", ch), character(0))
  expect_identical(suggest_match("x", character(0)), character(0))
})

test_that("drop_database() refuses protected targets, needs credentials and never drops unconfirmed", {
  expect_error(drop_database("postgres"), "Protected")
  expect_error(drop_database("template1"), "Protected")
  expect_error(drop_database("bad name;"), "database name")
  prev <- rendehR:::.ehr_env$creds
  on.exit(assign("creds", prev, envir = rendehR:::.ehr_env), add = TRUE)
  assign("creds", NULL, envir = rendehR:::.ehr_env)
  expect_error(drop_database("some_db"), "No credentials")
  creds <- list(postgres = list(host = "127.0.0.1", port = 1L, user = "nobody", password = "x"))
  expect_message(drop_database("some_db", creds), "\\[dry_run\\] Would drop database: some_db")
  # a confirmation that does not match never reaches the server
  expect_message(r <- drop_database("some_db", creds, dry_run = FALSE, confirm = "other_db"), "Aborted")
  expect_false(r)
  # a matching confirmation goes on to connect (and fails here: nothing listens on port 1)
  expect_error(suppressMessages(drop_database("some_db", creds, dry_run = FALSE, confirm = "some_db")))
  # outside an interactive session the console prompt is skipped, so nothing is dropped
  skip_if(interactive())
  expect_message(drop_database("some_db", creds, dry_run = FALSE), "Aborted")
})

test_that("write_lookup_table() replaces or refuses an existing table", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA lk")
  write_lookup_table(con, "lk", "codes", data.frame(code = "a", n = 1L))
  write_lookup_table(con, "lk", "codes", data.frame(code = c("b", "c"), n = 2:3))
  expect_identical(DBI::dbReadTable(con, DBI::Id(schema = "lk", table = "codes"))$code, c("b", "c"))
  expect_error(write_lookup_table(con, "lk", "codes", data.frame(code = "d"), overwrite = FALSE))
  expect_error(write_lookup_table(con, "lk", "bad name", data.frame(code = "d")), "table name")
})

test_that("connect_postgres() asks again for a refused password in an interactive session", {
  prev <- rendehR:::.ehr_env$creds
  on.exit(assign("creds", prev, envir = rendehR:::.ehr_env), add = TRUE)
  creds <- list(postgres = list(host = "db", port = 5432L, user = "me", password = "stale"))
  assign("creds", creds, envir = rendehR:::.ehr_env)
  tried <- character()
  local_mocked_bindings(
    .ehr_pg_dbconnect = function(..., password) {
      tried <<- c(tried, password)
      if (!identical(password, "right")) stop("FATAL:  password authentication failed for user \"me\"")
      structure(list(), class = "fake_con")
    },
    is_interactive = function() TRUE,
    .ehr_readline = function(prompt) "right")
  expect_message(con <- connect_postgres("somedb", creds), "Enter it again")
  expect_s3_class(con, "fake_con")
  expect_equal(tried, c("stale", "right"))
  expect_equal(rendehR:::.ehr_env$creds$postgres$password, "right")   # kept for the session
})

test_that("connect_postgres() explains a refused password outside an interactive session", {
  creds <- list(postgres = list(host = "db", user = "me", password = "stale"))
  local_mocked_bindings(
    .ehr_pg_dbconnect = function(...) stop("FATAL:  password authentication failed for user \"me\""),
    is_interactive = function() FALSE)
  expect_error(connect_postgres("somedb", creds), "Correct it in the credentials file")
  local_mocked_bindings(.ehr_pg_dbconnect = function(...) stop("could not connect to server"))
  expect_error(connect_postgres("somedb", creds), "could not connect to server")
})

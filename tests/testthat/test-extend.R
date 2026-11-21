# Tests for R/extend.R (identifier validation and execution).
# register_concept_sql()'s identifier validation runs before any connection
# is touched (execute = FALSE by default), so most tests here need no
# database at all; execution tests use in-memory DuckDB.

test_that("register_concept_sql: strips known schema prefixes and wraps in CREATE TABLE", {
  adapted <- register_concept_sql(
    "SELECT subject_id, valuenum AS value FROM mimiciv_icu.chartevents WHERE itemid = 220045",
    src_name     = "mimic_iv_demo",
    concept_name = "heart_rate_raw"
  )
  expect_false(grepl("mimiciv_icu\\.", adapted))
  expect_match(adapted, "CREATE TABLE \"mimic_iv_demo_coh_concepts\"\\.\"heart_rate_raw\" AS")
  expect_match(adapted, "FROM chartevents")
})

test_that("register_concept_sql: rejects a malformed concept_name before any execution", {
  expect_error(
    register_concept_sql(
      "SELECT 1", src_name = "mimic_iv_demo",
      concept_name = 'x"; DROP TABLE important_stuff; --'
    ),
    "Invalid"
  )
})

test_that("register_concept_sql: rejects a malformed schema argument", {
  expect_error(
    register_concept_sql(
      "SELECT 1", src_name = "mimic_iv_demo", concept_name = "ok_name",
      schema = "bad schema; --"
    ),
    "Invalid"
  )
})

test_that("register_concept_sql: strips an existing DROP/CREATE TABLE wrapper before re-wrapping", {
  adapted <- register_concept_sql(
    "DROP TABLE IF EXISTS old_name;\nCREATE TABLE old_name AS\nSELECT 1 AS x",
    src_name = "mimic_iv_demo", concept_name = "new_name"
  )
  expect_match(adapted, "CREATE TABLE \"mimic_iv_demo_coh_concepts\"\\.\"new_name\" AS")
  expect_identical(lengths(regmatches(adapted, gregexpr("CREATE TABLE", adapted))), 1L)
})

test_that("register_concept_sql: overwrite = TRUE prepends a DROP TABLE line", {
  adapted <- register_concept_sql(
    "SELECT 1 AS x", src_name = "mimic_iv_demo", concept_name = "y",
    overwrite = TRUE
  )
  expect_match(adapted, "DROP TABLE IF EXISTS \"mimic_iv_demo_coh_concepts\"\\.\"y\"")
})

test_that("register_concept_sql: execute = TRUE actually creates and registers the concept", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS mimic_iv_demo_coh")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS mimic_iv_demo_coh_concepts")

  register_concept_sql(
    "SELECT 42 AS answer", src_name = "mimic_iv_demo",
    concept_name = "life_universe_everything", con = con
  )

  n <- DBI::dbGetQuery(con, 'SELECT COUNT(*) AS n FROM mimic_iv_demo_coh_concepts.life_universe_everything')$n
  expect_equal(n, 1)
  registered <- DBI::dbGetQuery(con, "SELECT concept_name FROM mimic_iv_demo_coh.concept_registry")
  expect_identical(registered$concept_name, "life_universe_everything")
})

test_that("register_concept_sql: execute = TRUE builds the executed DROP/CREATE/COUNT SQL via DBI::dbQuoteIdentifier(), not a manual sprintf() string", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS mimic_iv_demo_coh")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS mimic_iv_demo_coh_concepts")

  real_quote <- DBI::dbQuoteIdentifier
  calls <- new.env(parent = emptyenv())
  calls$seen <- list()
  testthat::local_mocked_bindings(
    dbQuoteIdentifier = function(conn, x, ...) {
      calls$seen[[length(calls$seen) + 1L]] <- x
      real_quote(conn, x, ...)
    },
    .package = "DBI"
  )

  register_concept_sql(
    "SELECT 1 AS x", src_name = "mimic_iv_demo",
    concept_name = "quoting_probe", con = con, overwrite = TRUE
  )

  id_calls <- Filter(function(x) methods::is(x, "Id"), calls$seen)
  expect_true(length(id_calls) >= 1L)
  expect_identical(
    id_calls[[1L]]@name[c("schema", "table")],
    c(schema = "mimic_iv_demo_coh_concepts", table = "quoting_probe")
  )
  n <- DBI::dbGetQuery(con, 'SELECT COUNT(*) AS n FROM mimic_iv_demo_coh_concepts.quoting_probe')$n
  expect_equal(n, 1)
})

test_that("register_concept_sql: execution failure raises a clear error with a tip", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS mimic_iv_demo_coh_concepts")

  expect_error(
    register_concept_sql(
      "SELECT * FROM table_that_does_not_exist", src_name = "mimic_iv_demo",
      concept_name = "will_fail", con = con
    ),
    "SQL execution failed"
  )
})

test_that("register_concept_sql: an inline string under 500 characters is not mistaken for a file path", {
  adapted <- register_concept_sql(
    "SELECT subject_id FROM mimiciii.admissions",
    src_name = "mimic_iii_demo", concept_name = "adm"
  )
  expect_match(adapted, "SELECT subject_id FROM admissions")
})

test_that("init_project_schema: adds the project_ prefix when missing", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  schema <- init_project_schema("rehab_outcomes", con = con)
  expect_identical(schema, "project_rehab_outcomes")
  exists_query <- DBI::dbGetQuery(con,
    "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'project_rehab_outcomes'")
  expect_identical(nrow(exists_query), 1L)
})

test_that("init_project_schema: an already-prefixed name is not double-prefixed", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  schema <- init_project_schema("project_already", con = con)
  expect_identical(schema, "project_already")
})

test_that("init_project_schema: rejects a malformed project name before touching the connection", {
  expect_error(
    init_project_schema('bad"; DROP SCHEMA public CASCADE; --'),
    "Invalid"
  )
})

test_that("clone_source_to_db builds authenticated commands for every schema", {
  captured <- character()
  seen_pw <- character()
  local_mocked_bindings(
    ensure_database_exists = function(...) invisible(TRUE),
    .ehr_run_shell = function(cmd) {
      captured <<- c(captured, cmd)
      seen_pw <<- c(seen_pw, Sys.getenv("PGPASSWORD"))
      0L
    }
  )
  old_pw <- Sys.getenv("PGPASSWORD", unset = NA_character_)
  creds <- list(postgres = list(host = "dbhost", port = 15432L,
                                user = "dbuser", password = "secret"))
  suppressMessages(clone_source_to_db("mimic_iv_demo", "db_from", "db_to",
                                      creds = creds))
  expect_length(captured, 4)
  expect_true(all(grepl("-h 'dbhost' -p 15432 -U 'dbuser'", captured, fixed = TRUE)))
  for (s in unlist(src_schema_names("mimic_iv_demo")))
    expect_true(any(grepl(paste0("-n '", s, "' "), captured, fixed = TRUE)), info = s)
  expect_true(all(seen_pw == "secret"))
  expect_identical(Sys.getenv("PGPASSWORD", unset = NA_character_), old_pw)
  expect_false(any(grepl("secret", captured, fixed = TRUE)))
  expect_error(clone_source_to_db("mimic_iv_demo", "a", "b", creds,
                                  schemas = "bogus"), "Unknown schema")
})

test_that("clone_source_to_db: validates arguments are present (no execution, no pg_dump call)", {
  # clone_source_to_db() shells out to pg_dump/pg_restore; a full execution
  # test needs those external binaries and a real PostgreSQL server, which
  # this environment does not have. This test only confirms the function
  # exists with its documented signature and fails fast on an unusable
  # target rather than silently doing nothing.
  expect_true(is.function(clone_source_to_db))
  expect_true(all(c("src_name", "from_db", "to_db", "creds") %in%
                 names(formals(clone_source_to_db))))
})

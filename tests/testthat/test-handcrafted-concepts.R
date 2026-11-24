# Tests for R/handcrafted_concepts.R. Every execution test uses
# self-authored, deliberately portable SQL against an in-memory DuckDB
# connection rather than the packaged Postgres-flavored mimic-code/eicu-code
# files, so these tests do not depend on PostgreSQL-only syntax.

test_that("run_flat_sql_concepts: substitutes schema placeholders and executes each file", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dir <- file.path(tempdir(), paste0("s007_flat_", as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    "CREATE TABLE flat_ok AS SELECT 1 AS x;",
    file.path(dir, "flat_ok.sql")
  )

  results <- run_flat_sql_concepts(
    con             = con,
    flat_sql_dir    = dir,
    raw_schema      = "main",
    concepts_schema = "main"
  )

  expect_true(results[["flat_ok.sql"]])
  expect_true(DBI::dbExistsTable(con, "flat_ok"))
})

test_that("run_flat_sql_concepts: skip_on_error continues past a failing file and marks it FALSE", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dir <- file.path(tempdir(), paste0("s007_flat_fail_", as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("CREATE TABLE flat_a AS SELECT 1 AS x;", file.path(dir, "flat_a.sql"))
  writeLines("SELECT * FROM nonexistent_table;", file.path(dir, "flat_b.sql"))

  results <- run_flat_sql_concepts(
    con             = con,
    flat_sql_dir    = dir,
    raw_schema      = "main",
    concepts_schema = "main",
    skip_on_error   = TRUE
  )

  expect_true(results[["flat_a.sql"]])
  expect_false(results[["flat_b.sql"]])
})

test_that("run_flat_sql_concepts: skip_on_error = FALSE (default) raises on the first failure", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dir <- file.path(tempdir(), paste0("s007_flat_stop_", as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("SELECT * FROM nonexistent_table;", file.path(dir, "only.sql"))

  expect_error(
    run_flat_sql_concepts(
      con = con, flat_sql_dir = dir, raw_schema = "main", concepts_schema = "main"
    ),
    "only\\.sql"
  )
})

test_that("run_flat_sql_concepts: no directory returns invisible(logical(0))", {
  expect_identical(
    run_flat_sql_concepts(
      con = NULL, flat_sql_dir = file.path(tempdir(), "does_not_exist_ever"),
      raw_schema = "main", concepts_schema = "main"
    ),
    logical(0)
  )
})

test_that(".run_user_sql_dir: missing directory returns invisible(logical(0))", {
  expect_identical(
    .run_user_sql_dir(NULL, file.path(tempdir(), "no_such_user_dir"),
                      raw_schema = "main", target_schema = "main"),
    logical(0)
  )
})

# This test verifies manifest-driven file discovery/dispatch and the
# reserved-file exclusion without a connection (execute = FALSE +
# save_adapted = TRUE); the DuckDB tests below execute end to end.
test_that("run_external_concepts: dispatches the manifest-listed file and excludes reserved control files", {
  root <- file.path(tempdir(), paste0("s007_ext_", as.integer(Sys.time())))
  sql_dir <- file.path(root, "mimic_code")
  save_dir <- file.path(root, "adapted")
  dir.create(sql_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    "CREATE TABLE manifest_tbl AS SELECT 1 AS x;",
    file.path(sql_dir, "manifest_tbl.sql")
  )
  writeLines(
    "\\i manifest_tbl.sql",
    file.path(sql_dir, "postgres-make-concepts.sql")
  )

  paths <- list(external_sql_dir = sql_dir, custom_sql_dir = file.path(root, "custom"))
  schema_names <- list(raw = "main", derived = "main")

  results <- run_external_concepts(
    con           = NULL,
    paths         = paths,
    schema_names  = schema_names,
    src_name      = "mimic_iv_demo",
    execute       = FALSE,
    save_adapted  = TRUE,
    save_to       = save_dir,
    skip_on_error = TRUE
  )

  expect_true(results[["manifest_tbl.sql"]])
  expect_true(file.exists(file.path(save_dir, "manifest_tbl.sql")))
  # The manifest file and functions bootstrap are reserved control files and
  # must never themselves be counted as executed concepts.
  expect_false("postgres-make-concepts.sql" %in% names(results))
})

test_that("run_external_concepts: strips known MIMIC-IV schema prefixes from adapted SQL", {
  root <- file.path(tempdir(), paste0("s007_ext_prefix_", as.integer(Sys.time())))
  sql_dir <- file.path(root, "mimic_code")
  save_dir <- file.path(root, "adapted")
  dir.create(sql_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    "SELECT subject_id FROM mimiciv_hosp.admissions;",
    file.path(sql_dir, "only.sql")
  )

  paths <- list(external_sql_dir = sql_dir, custom_sql_dir = file.path(root, "custom"))
  schema_names <- list(raw = "main", derived = "main")

  results <- run_external_concepts(
    con           = NULL,
    paths         = paths,
    schema_names  = schema_names,
    src_name      = "mimic_iv_demo",
    execute       = FALSE,
    save_adapted  = TRUE,
    save_to       = save_dir
  )

  expect_true(results[["only.sql"]])
  adapted <- paste(readLines(file.path(save_dir, "only.sql"), warn = FALSE), collapse = "\n")
  expect_false(grepl("mimiciv_hosp\\.", adapted, fixed = FALSE))
  expect_match(adapted, "FROM admissions")
})

test_that("run_external_concepts: missing directory returns invisible(logical(0))", {
  expect_identical(
    run_external_concepts(
      con = NULL, paths = list(external_sql_dir = file.path(tempdir(), "nope")),
      schema_names = list(raw = "main", derived = "main"), src_name = "mimic_iv_demo"
    ),
    logical(0)
  )
})

test_that(".ehr_set_search_path uses each engine's syntax", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA drv")
  DBI::dbExecute(con, "CREATE SCHEMA raw")
  DBI::dbExecute(con, "CREATE TABLE raw.src AS SELECT 7 AS x")
  .ehr_set_search_path(con, c("drv", "raw"))
  DBI::dbExecute(con, "CREATE TABLE out_tbl AS SELECT x FROM src")
  got <- DBI::dbGetQuery(con, "SELECT table_schema FROM information_schema.tables
                               WHERE table_name = 'out_tbl'")
  expect_identical(got$table_schema, "drv")
  expect_error(.ehr_set_search_path(con, c("drv", "bad;name")), "Invalid")
})

test_that(".run_user_sql_dir executes user SQL end to end on DuckDB", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA rawsch")
  DBI::dbExecute(con, "CREATE TABLE rawsch.src AS SELECT 1 AS x")
  dir <- tempfile("user-sql-")
  dir.create(dir)
  writeLines("CREATE TABLE user_tbl AS SELECT x FROM src;", file.path(dir, "user_tbl.sql"))
  dir.create(file.path(dir, "vitals"))
  writeLines("CREATE TABLE user_sub AS SELECT x FROM src;", file.path(dir, "vitals", "user_sub.sql"))
  res <- suppressMessages(.run_user_sql_dir(con, dir, raw_schema = "rawsch",
                                            target_schema = "usersch"))
  expect_true(all(res))
  expect_length(res, 2L)
  expect_true(DBI::dbExistsTable(con, DBI::Id(schema = "usersch", table = "user_tbl")))
  # a file in a subfolder runs too, as run_concepts() counts it when enabling the layer
  expect_true(DBI::dbExistsTable(con, DBI::Id(schema = "usersch", table = "user_sub")))
  # the same files run again: each table is replaced, not duplicated or failed
  expect_true(all(suppressMessages(.run_user_sql_dir(
    con, dir, raw_schema = "rawsch", target_schema = "usersch"
  ))))
})

test_that("every packaged DO-guarded flat concept unwraps for DuckDB", {
  files <- list.files(.ehr_extdata("sql"), pattern = "\\.sql$",
                      recursive = TRUE, full.names = TRUE)
  files <- files[grepl("/concepts_sqls/flat/", files)]
  guarded <- files[vapply(files, function(f)
    any(grepl("^DO \\$", readLines(f, warn = FALSE))), logical(1))]
  expect_gt(length(guarded), 0)
  for (f in guarded) {
    sql <- paste(readLines(f, warn = FALSE), collapse = "\n")
    sql <- gsub("{raw_schema}", "src_coh", sql, fixed = TRUE)
    sql <- gsub("{concepts_schema}", "src_coh_concepts", sql, fixed = TRUE)
    u <- .ehr_unwrap_guarded_do(sql)
    expect_false(is.null(u), info = basename(f))
    body <- paste(u$pre, u$if_missing, u$if_present)
    expect_false(grepl("$", body, fixed = TRUE), info = basename(f))
    expect_match(u$if_present, "CREATE TABLE", info = basename(f))
  }
  expect_null(.ehr_unwrap_guarded_do("CREATE TABLE t AS SELECT 1;"))
})

test_that("run_flat_sql_concepts honours DO guards on DuckDB", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA rawsch")
  DBI::dbExecute(con, "CREATE SCHEMA consch")
  dir <- tempfile("flat-guard-")
  dir.create(dir)
  writeLines(c(
    "-- guarded",
    "DO $$",
    "BEGIN",
    "    IF to_regclass('{raw_schema}.iss') IS NULL THEN",
    "        RAISE NOTICE 'Skipping flat concept g: {raw_schema}.iss does not exist (run_iss() did not run).';",
    "        RETURN;",
    "    END IF;",
    "    DROP TABLE IF EXISTS {concepts_schema}.g;",
    "    CREATE TABLE {concepts_schema}.g AS SELECT x FROM {raw_schema}.iss;",
    "END $$;"), file.path(dir, "g.sql"))
  run <- function() suppressMessages(run_flat_sql_concepts(
    con, flat_sql_dir = dir, raw_schema = "rawsch", concepts_schema = "consch"))
  expect_true(run()[["g.sql"]])
  expect_false(DBI::dbExistsTable(con, DBI::Id(schema = "consch", table = "g")))
  DBI::dbExecute(con, "CREATE TABLE rawsch.iss AS SELECT 3 AS x")
  expect_true(run()[["g.sql"]])
  expect_equal(DBI::dbGetQuery(con, "SELECT x FROM consch.g")$x, 3)
})

test_that("the IF/ELSE guard form picks its branch on DuckDB", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA rawsch")
  DBI::dbExecute(con, "CREATE SCHEMA consch")
  dir <- tempfile("flat-branch-")
  dir.create(dir)
  writeLines(c(
    "DROP TABLE IF EXISTS {concepts_schema}.e;",
    "DO $do$",
    "BEGIN",
    "IF to_regclass('{raw_schema}.notes') IS NULL THEN",
    "    CREATE TABLE {concepts_schema}.e (x integer);",
    "ELSE",
    "    EXECUTE $sql$",
    "    CREATE TABLE {concepts_schema}.e AS SELECT x FROM {raw_schema}.notes;",
    "    $sql$;",
    "END IF;",
    "END",
    "$do$;"), file.path(dir, "e.sql"))
  run <- function() suppressMessages(run_flat_sql_concepts(
    con, flat_sql_dir = dir, raw_schema = "rawsch", concepts_schema = "consch"))
  expect_true(run()[["e.sql"]])
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM consch.e")$n, 0)
  DBI::dbExecute(con, "CREATE TABLE rawsch.notes AS SELECT 5 AS x")
  expect_true(run()[["e.sql"]])
  expect_equal(DBI::dbGetQuery(con, "SELECT x FROM consch.e")$x, 5)
})

test_that(".ehr_sql_template_src reads the source above concepts_sqls at any depth", {
  expect_identical(.ehr_sql_template_src(
    "/p/sql/mimic_iv_demo/concepts_sqls/external/mod", "x"), "mimic_iv_demo")
  expect_identical(.ehr_sql_template_src(
    "/p/sql/mimic_iv_demo/concepts_sqls/external", "x"), "mimic_iv_demo")
  expect_identical(.ehr_sql_template_src(
    "C:\\p\\sql\\eicu_demo\\concepts_sqls\\external\\a\\b", "x"), "eicu_demo")
  expect_identical(.ehr_sql_template_src("/somewhere/else/my_sql", "fallback"),
                   "fallback")
})

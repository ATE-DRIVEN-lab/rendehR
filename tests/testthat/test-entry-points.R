# Exported entry points that otherwise run only against a live PostgreSQL
# server, the PhysioNet files or a browser: browsing, dropping and exporting
# run on an in-memory DuckDB database; the pipeline wrappers and the explorer
# launcher are checked for what they pass on, with the heavy steps mocked.
# Tests at the end of the file also run them for real when a test database
# and the demo files are available.

.ep_duckdb <- function(env = parent.frame()) {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

# Functions that open their own connection record it in the package state;
# restore that state when the test ends.
.ep_keep_session <- function(env = parent.frame()) {
  old <- mget(c("db_name", "creds", "config"), envir = .ehr_env, ifnotfound = list(NULL))
  withr::defer(for (n in names(old)) assign(n, old[[n]], envir = .ehr_env), envir = env)
}

.ep_pg_creds <- function() {
  list(postgres = list(host = Sys.getenv("PGHOST", "localhost"),
                       port = as.integer(Sys.getenv("PGPORT", "5432")),
                       user = Sys.getenv("PGUSER", "postgres"),
                       password = Sys.getenv("PGPASSWORD", "")))
}

# ---- Browsing ---------------------------------------------------------------

test_that("browse_source() and open_source() expose every table of a source schema", {
  con <- .ep_duckdb()
  DBI::dbExecute(con, "CREATE SCHEMA demo_coh")
  DBI::dbExecute(con, "CREATE TABLE demo_coh.admissions AS SELECT range AS hadm_id FROM range(5)")
  DBI::dbExecute(con, "CREATE TABLE demo_coh.patients AS SELECT 1 AS subject_id")

  src <- browse_source("demo", con = con, db_name = "testdb")
  expect_s3_class(src, "ehr_source")
  expect_identical(src$tables(), c("admissions", "patients"))
  expect_identical(src$.schema, "demo_coh")
  expect_identical(src$.db_name, "testdb")
  expect_false(src$.owns_connection)
  expect_equal(nrow(dplyr::collect(src$admissions)), 5)
  expect_message(expect_false(src$disconnect()), "supplied by caller")
  expect_true(DBI::dbIsValid(con))

  same <- open_source("demo", con = con, db_name = "testdb")
  expect_identical(same$tables(), src$tables())
  expect_identical(browse_source("demo", schema = "concepts", con = con, db_name = "testdb")$tables(), character(0))
  expect_error(browse_source("demo", schema = "x; DROP SCHEMA demo_coh", con = con, db_name = "testdb"))
})

test_that("browse_ehr_db_flex() groups tables by source and layer", {
  con <- .ep_duckdb()
  for (s in c("demo_coh", "demo_coh_concepts", "rendehr", "misc")) DBI::dbExecute(con, paste("CREATE SCHEMA", s))
  DBI::dbExecute(con, "CREATE TABLE demo_coh.admissions AS SELECT range AS hadm_id FROM range(3)")
  DBI::dbExecute(con, "CREATE VIEW demo_coh.adm_view AS SELECT * FROM demo_coh.admissions")
  DBI::dbExecute(con, "CREATE TABLE demo_coh_concepts.heart_rate AS SELECT 80 AS value")
  DBI::dbExecute(con, "CREATE TABLE rendehr.ts AS SELECT 1 AS x")
  DBI::dbExecute(con, "CREATE TABLE misc.notes AS SELECT 1 AS x")

  db <- browse_ehr_db_flex(con = con, db_name = "testdb")
  expect_s3_class(db, "ehr_db_flex_browser")
  expect_named(db$demo, c("raw", "concepts", "derived", "harmonized"), ignore.order = TRUE)
  expect_setequal(names(db$demo$raw), c("admissions", "adm_view"))
  expect_equal(nrow(dplyr::collect(db$demo$raw$admissions)), 3)
  expect_named(db$demo$concepts, "heart_rate")
  expect_length(db$demo$derived, 0)
  expect_named(db$merged, "rendehr")
  expect_named(db$merged$rendehr, "ts")
  expect_named(db$other$misc, "notes")
  expect_identical(attr(db, "db_name"), "testdb")
  expect_false(attr(db, "owns_connection"))

  flat <- browse_ehr_db_flex(con = con, db_name = "testdb", view = "flattened")
  expect_true(all(c("demo_coh.admissions", "demo_coh_concepts.heart_rate", "rendehr.ts") %in% names(flat)))
  views <- browse_ehr_db_flex(con = con, db_name = "testdb", table_type = "view", view = "flattened")
  expect_identical(names(views), "demo_coh.adm_view")
  base <- browse_ehr_db_flex(con = con, db_name = "testdb", table_type = "base", view = "flattened")
  expect_false("demo_coh.adm_view" %in% names(base))
  both <- browse_ehr_db_flex(con = con, db_name = "testdb", view = "both")
  expect_named(both, c("sources", "flattened", "tables"))
  expect_true(all(c("table_schema", "table_name", "table_type") %in% names(both$tables)))
  sys <- browse_ehr_db_flex(con = con, db_name = "testdb", include_system = TRUE, view = "both")
  expect_gte(nrow(sys$tables), nrow(both$tables))

  expect_message(expect_false(disconnect_ehr_db(db)), "supplied by caller")
  expect_true(DBI::dbIsValid(con))
  expect_error(disconnect_ehr_db(list()), "browse_ehr_db_flex")
})

test_that("a browser that opened its own connection closes it", {
  skip_if_not_installed("duckdb")
  .ep_keep_session()
  own <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(if (DBI::dbIsValid(own)) DBI::dbDisconnect(own, shutdown = TRUE))
  DBI::dbExecute(own, "CREATE SCHEMA demo_coh")
  DBI::dbExecute(own, "CREATE TABLE demo_coh.patients AS SELECT 1 AS subject_id")
  local_mocked_bindings(
    .ehr_load_config_for_browse = function(config_path = NULL) NULL,
    load_credentials = function(path = NULL) list(postgres = list(user = "u")),
    connect_postgres = function(db_name, creds) own)

  db <- browse_ehr_db_flex(db_name = "testdb")
  expect_true(attr(db, "owns_connection"))
  expect_identical(.ehr_env$db_name, "testdb")
  expect_message(expect_true(disconnect_ehr_db(db)), "Disconnected")
  expect_false(DBI::dbIsValid(own))
})

test_that("a source browser that opened its own connection closes it", {
  skip_if_not_installed("duckdb")
  .ep_keep_session()
  own <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(if (DBI::dbIsValid(own)) DBI::dbDisconnect(own, shutdown = TRUE))
  DBI::dbExecute(own, "CREATE SCHEMA demo_coh")
  DBI::dbExecute(own, "CREATE TABLE demo_coh.patients AS SELECT 1 AS subject_id")
  local_mocked_bindings(
    .ehr_load_config_for_browse = function(config_path = NULL) NULL,
    load_credentials = function(path = NULL) list(postgres = list(user = "u")),
    connect_postgres = function(db_name, creds) own)

  src <- open_source("demo", db_name = "testdb")
  expect_true(src$.owns_connection)
  expect_identical(src$tables(), "patients")
  expect_message(expect_true(src$disconnect()), "Disconnected: demo_coh")
  expect_false(DBI::dbIsValid(own))
})

test_that("browse_psql() lists the server's databases with code to browse each", {
  skip_if_not_installed("RPostgres")
  seen <- new.env()
  listing <- data.frame(db_name = c("rendehr", "locked"), allow_connections = c(TRUE, TRUE),
                        can_connect = c(TRUE, FALSE), stringsAsFactors = FALSE)
  local_mocked_bindings(
    dbConnect = function(drv, ...) { seen$args <- list(...); structure(list(), class = "fake_con") },
    dbGetQuery = function(conn, statement, ...) { seen$sql <- statement; listing },
    dbDisconnect = function(conn, ...) { seen$closed <- TRUE; invisible(TRUE) },
    .package = "DBI")
  creds <- list(postgres = list(host = "db.example", port = 5433L, user = "analyst", password = "pw"))

  out <- browse_psql(creds = creds)
  expect_identical(seen$args$dbname, "postgres")
  expect_identical(seen$args$host, "db.example")
  expect_identical(seen$args$port, 5433L)
  expect_identical(seen$args$user, "analyst")
  expect_true(seen$closed)
  expect_match(seen$sql, "d.datname NOT IN ('postgres', 'template0', 'template1')", fixed = TRUE)
  expect_match(seen$sql, "d.datallowconn IS TRUE", fixed = TRUE)
  expect_identical(out$browse_code, c('browse_ehr_db_flex(db_name = "rendehr")', NA))

  browse_psql(creds = creds, maintenance_db = "other", include_system = TRUE, include_unconnectable = TRUE)
  expect_identical(seen$args$dbname, "other")
  expect_no_match(seen$sql, "WHERE", fixed = TRUE)

  listing <- listing[0, ]
  expect_identical(nrow(browse_psql(creds = creds)), 0L)
})

# ---- Dropping ---------------------------------------------------------------

test_that("drop_schema() previews by default and drops only rendehR schemas", {
  con <- .ep_duckdb()
  DBI::dbExecute(con, "CREATE SCHEMA demo_coh_concepts")
  DBI::dbExecute(con, "CREATE TABLE demo_coh_concepts.heart_rate AS SELECT 80 AS value")
  DBI::dbExecute(con, "CREATE TABLE demo_coh_concepts.sbp AS SELECT 120 AS value")
  has_schema <- function() "demo_coh_concepts" %in%
    DBI::dbGetQuery(con, "SELECT schema_name FROM information_schema.schemata")$schema_name

  msgs <- capture_messages(drop_schema("demo", con = con))
  expect_match(paste(msgs, collapse = ""), "DRY RUN.*demo_coh_concepts")
  expect_match(paste(msgs, collapse = ""), "2 table\\(s\\)")
  expect_true(has_schema())

  DBI::dbExecute(con, "CREATE SCHEMA demo_coh_derived")
  expect_message(drop_schema("demo", schema = "derived", con = con), "Schema is empty")

  expect_message(drop_schema("demo", dry_run = FALSE, con = con), "2 tables removed")
  expect_false(has_schema())
  expect_error(drop_schema("demo", schema = "main", con = con), "does not look like an rendehR schema")
})

test_that("drop_subcohort() drops the schemas of the named subcohort", {
  seen <- NULL
  local_mocked_bindings(drop_source_schemas = function(src_name, dry_run = TRUE, con = NULL) {
    seen <<- list(src_name = src_name, dry_run = dry_run, con = con); invisible(NULL)
  })
  drop_subcohort("mimic_iv_demo", "cervical_complete")
  expect_identical(seen, list(src_name = "mimic_iv_demo_cervical_complete", dry_run = TRUE, con = NULL))
  drop_subcohort("mimic_iv_demo", "cervical_complete", dry_run = FALSE, con = "con")
  expect_identical(seen$dry_run, FALSE)
  expect_identical(seen$con, "con")
})

# ---- Export -----------------------------------------------------------------

test_that("export_table_parquet() writes chunked datasets and single files", {
  skip_if_not_installed("arrow")
  con <- .ep_duckdb()
  DBI::dbExecute(con, "CREATE SCHEMA demo_coh")
  DBI::dbExecute(con, "CREATE TABLE demo_coh.labs AS SELECT range AS id, range * 2 AS value FROM range(25)")
  out <- withr::local_tempdir()

  dir <- file.path(out, "labs")
  n <- export_table_parquet("demo", "labs", output_dir = dir, chunk_size = 10, verbose = FALSE, con = con)
  expect_equal(n, 25)
  expect_length(list.files(dir, pattern = "\\.parquet$"), 3)
  back <- dplyr::collect(arrow::open_dataset(dir))
  expect_equal(sort(back$id), 0:24)
  expect_equal(back$value[order(back$id)], (0:24) * 2)

  expect_message(export_table_parquet(query = "SELECT * FROM demo_coh.labs WHERE id < 5", output_dir = dir,
                                      overwrite = TRUE, chunk_size = 10, con = con),
                 "Cleared existing parquet files")
  expect_length(list.files(dir, pattern = "\\.parquet$"), 1)
  expect_equal(nrow(dplyr::collect(arrow::open_dataset(dir))), 5)

  file <- file.path(out, "single", "labs.parquet")
  export_table_parquet("demo", "labs", output_dir = file, chunk_size = 7, verbose = FALSE, con = con)
  expect_true(file.exists(file))
  expect_equal(nrow(arrow::read_parquet(file)), 25)
  export_table_parquet(query = "SELECT * FROM demo_coh.labs WHERE id >= 20", output_dir = file,
                       overwrite = TRUE, verbose = FALSE, con = con)
  expect_equal(sort(arrow::read_parquet(file)$id), 20:24)

  expect_error(export_table_parquet(output_dir = dir, con = con), "src_name \\+ table")
})

# ---- Credentials ------------------------------------------------------------

test_that("set_physionet_env() sets only the PhysioNet variables and needs both values", {
  skip_if(interactive())
  withr::local_envvar(PHYSIONET_USER = NA, PHYSIONET_PASS = NA)
  set_physionet_env(list(physionet = list(username = "reader", password = "secret")))
  expect_identical(Sys.getenv("PHYSIONET_USER"), "reader")
  expect_identical(Sys.getenv("PHYSIONET_PASS"), "secret")

  set_physionet_env(list(physionet = list(user = "alias", password = "secret")))
  expect_identical(Sys.getenv("PHYSIONET_USER"), "alias")

  withr::local_envvar(PHYSIONET_USER = NA, PHYSIONET_PASS = NA)
  err <- expect_error(set_physionet_env(list(physionet = list(username = "reader"))), "PhysioNet credentials are required")
  expect_no_match(conditionMessage(err), "reader")
  expect_error(set_physionet_env(list()), "PhysioNet credentials are required")
  expect_identical(Sys.getenv("PHYSIONET_USER"), "")
})

# ---- Pipeline wrappers ------------------------------------------------------

# A DuckDB stand-in for the Postgres connection build_window_tables() opens:
# it has no pg_class (drop is mocked) and no SET LOCAL (skipped).
.ep_window_mocks <- function(con, sql_dir, window_sql, env = parent.frame()) {
  dbExecute <- DBI::dbExecute
  local_mocked_bindings(
    ehr_paths = function(src_name = NULL, base_dir = NULL, ...)
      list(helper_sql_dir = sql_dir, credentials_json = "creds.json"),
    load_credentials = function(path = NULL) list(postgres = list(user = "u")),
    connect_postgres = function(db_name, creds) con,
    .ehr_drop_relation = function(con, schema, table)
      dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"."%s"', schema, table)),
    run_sql_file = function(con, path) for (s in window_sql) dbExecute(con, s),
    .env = env)
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...)
      if (grepl("^SET LOCAL", statement)) 0L else dbExecute(conn, statement, ...),
    dbDisconnect = function(conn, ...) invisible(TRUE),
    .package = "DBI", .env = env)
}

test_that("build_window_tables() replaces the window tables and validates them", {
  con <- .ep_duckdb()
  DBI::dbExecute(con, "CREATE SCHEMA mimic_iv_demo_coh")
  sql_dir <- withr::local_tempdir()
  writeLines("-- window tables", file.path(sql_dir, "window_tables.sql"))
  good <- c(
    "CREATE TABLE mimic_iv_demo_coh.admission_windows AS SELECT range AS hadm_id, now() AS admittime, now() AS dischtime FROM range(3)",
    "CREATE TABLE mimic_iv_demo_coh.icu_windows AS SELECT range AS hadm_id, range + 10 AS stay_id, now() AS icu_intime, now() AS icu_outtime FROM range(3)",
    "CREATE TABLE mimic_iv_demo_coh.visit_windows AS SELECT hadm_id, stay_id, now() AS admittime, now() AS dischtime FROM mimic_iv_demo_coh.icu_windows")

  local({
    .ep_window_mocks(con, sql_dir, good)
    expect_message(build_window_tables("mimic_iv_demo", db_name = "testdb"), "Window tables built: mimic_iv_demo")
  })
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM mimic_iv_demo_coh.icu_windows")$n, 3)

  # a failed rebuild leaves the previous tables in place
  missing_col <- sub("now\\(\\) AS dischtime FROM range", "1 AS x FROM range", good)
  local({
    .ep_window_mocks(con, sql_dir, missing_col)
    expect_error(build_window_tables("mimic_iv_demo", db_name = "testdb"), "missing required column\\(s\\): dischtime")
  })
  expect_true("dischtime" %in% DBI::dbListFields(con, DBI::Id(schema = "mimic_iv_demo_coh", table = "admission_windows")))

  dup <- sub("range AS hadm_id, now\\(\\) AS admittime", "1 AS hadm_id, now() AS admittime", good)
  local({
    .ep_window_mocks(con, sql_dir, dup)
    expect_error(build_window_tables("mimic_iv_demo", db_name = "testdb"), "failed non-null/unique key validation")
  })
  expect_equal(DBI::dbGetQuery(con, "SELECT count(DISTINCT hadm_id) AS n FROM mimic_iv_demo_coh.admission_windows")$n, 3)

  local({
    .ep_window_mocks(con, withr::local_tempdir(), good)
    expect_error(build_window_tables("mimic_iv_demo", db_name = "testdb"), "window_tables.sql not found")
  })
})

test_that("build_icu_windows() and build_visit_windows() are aliases of build_window_tables()", {
  seen <- list()
  local_mocked_bindings(build_window_tables = function(src_name, db_name = NULL, base_dir = NULL) {
    seen[[length(seen) + 1L]] <<- list(src_name, db_name, base_dir); invisible(NULL)
  })
  expect_null(build_icu_windows("mimic_iv_demo", db_name = "a", base_dir = "b"))
  expect_null(build_visit_windows("eicu_demo", db_name = "c"))
  expect_identical(seen, list(list("mimic_iv_demo", "a", "b"), list("eicu_demo", "c", NULL)))
})

test_that("build_cohort_info() runs the generic cohort_info build on its own connection", {
  seen <- NULL; closed <- FALSE
  local_mocked_bindings(
    ehr_paths = function(src_name = NULL, base_dir = NULL, ...)
      list(credentials_json = "creds.json", icds_dict_path = "dict.csv", icd_adjudication_path = "adj.csv"),
    load_credentials = function(path = NULL) list(postgres = list(user = "u")),
    connect_postgres = function(db_name, creds) structure(list(db = db_name), class = "fake_con"),
    run_cohort_info = function(...) { seen <<- list(...); invisible(data.frame(ehr_id = "1")) })
  local_mocked_bindings(dbDisconnect = function(conn, ...) { closed <<- TRUE; invisible(TRUE) }, .package = "DBI")

  out <- build_cohort_info("mimic_iv_demo", db_name = "testdb", icd_filter_path = "ignored.csv")
  expect_identical(out, data.frame(ehr_id = "1"))
  expect_true(closed)
  expect_identical(seen$con$db, "testdb")
  expect_identical(seen$schema_names, src_schema_names("mimic_iv_demo"))
  expect_identical(seen$src_name, "mimic_iv_demo")
  expect_identical(seen$icds_dict_path, "dict.csv")
  expect_identical(seen$icd_adjudication_path, "adj.csv")
  expect_null(seen$classify_fn)
  expect_identical(seen$analysis_level, "admission")
  build_cohort_info("mimic_iv_demo", db_name = "testdb", analysis_level = "icu")
  expect_identical(seen$analysis_level, "icu")
  expect_error(build_cohort_info("mimic_iv_demo", db_name = "testdb", analysis_level = "patient"))
})

test_that("build_sepsis_cohort() selects the Sepsis-3 stays and imports them", {
  seen <- new.env()
  local_mocked_bindings(
    .ehr_select_cohort_ids_duckdb = function(src_name, cohort_sql, key, setup_sql, out_csv, ...) {
      seen$select <- list(src_name = src_name, key = key, sql = cohort_sql, setup = setup_sql, out_csv = out_csv, dots = list(...))
      ids <- data.frame(icustay_id = c(1L, 2L))
      if (!is.null(out_csv)) utils::write.csv(ids, out_csv, row.names = FALSE)
      ids
    },
    import_cohort_general = function(...) {
      seen$import <- list(...); seen$csv_existed <- file.exists(seen$import$direct_ids_path); invisible(NULL)
    })

  ids <- build_sepsis_cohort("mimic_iii_demo", base_dir = "base", raw_data_root = "raw", show_progress = FALSE)
  expect_identical(ids, data.frame(icustay_id = c(1L, 2L)))
  expect_identical(seen$select$key, "icustay_id")
  expect_match(seen$select$sql, "FROM sepsis3 WHERE sepsis3")
  expect_true(length(seen$select$setup) > 0)
  expect_identical(seen$select$dots$raw_data_root, "raw")
  expect_true(seen$csv_existed)
  expect_identical(seen$import$direct_ids_path, seen$select$out_csv)
  expect_identical(seen$import$key, "icustay_id")
  expect_identical(seen$import$import_scope, "hospital")
  expect_false(file.exists(seen$select$out_csv))     # the temporary id list is removed

  seen$import <- NULL
  keep <- withr::local_tempfile(fileext = ".csv")
  build_sepsis_cohort("mimic_iv_demo", import = FALSE, out_csv = keep, show_progress = FALSE)
  expect_null(seen$import)
  expect_identical(seen$select$key, "stay_id")
  expect_true(file.exists(keep))

  build_sepsis_cohort("mimic_iv_demo", import = FALSE, show_progress = FALSE)
  expect_null(seen$select$out_csv)
  expect_error(build_sepsis_cohort("mimic_iv_demo", bundle = "unknown", import = FALSE))
})

test_that("run_sepsis3_example() builds the cohort, then runs the remaining stages", {
  seen <- new.env()
  local_mocked_bindings(
    build_sepsis_cohort = function(src_name, ...) { seen$cohort <- c(list(src_name = src_name), list(...)); invisible(NULL) },
    run_one_source = function(...) { seen$run <- list(...); invisible(NULL) })
  msgs <- capture_messages(run_sepsis3_example(db_name = "testdb", base_dir = "base", show_progress = FALSE))
  expect_match(paste(msgs, collapse = ""), "Sepsis-3 Reference Example")
  expect_match(paste(msgs, collapse = ""), "Sepsis-3 example complete")
  expect_identical(seen$cohort$src_name, "mimic_iii_demo")
  expect_true(seen$cohort$import)
  expect_identical(seen$run$db_name, "testdb")
  expect_identical(seen$run$src_name, "mimic_iii_demo")
  expect_false("import_cohort" %in% seen$run$stages)
  expect_true(all(c("run_comorbidities", "run_iss", "build_cohort_info") %in% seen$run$stages))
})

test_that("run_scid_example() runs the SCI source with the bundled code list", {
  seen <- NULL
  local_mocked_bindings(run_one_source = function(...) { seen <<- list(...); invisible(NULL) })
  msgs <- capture_messages(run_scid_example(db_name = "testdb", raw_data_root = "raw", overwrite = TRUE))
  expect_match(paste(msgs, collapse = ""), "SCI Reference Example")
  expect_identical(seen$src_name, "mimic_iv_demo")
  expect_identical(seen$db_name, "testdb")
  expect_identical(basename(seen$icd_filter_path), "scid_icds_rendehr.rds")
  expect_true(file.exists(seen$icd_filter_path))
  expect_true(seen$run_cohort_annotations)
  expect_identical(seen$raw_data_root, "raw")
  expect_true(seen$overwrite)
})

test_that("run_sources() runs each source in turn and stops at the first failure", {
  ran <- character(0)
  local_mocked_bindings(run_one_source = function(src_name, ...) {
    if (identical(src_name, "broken")) stop("source failed")
    ran <<- c(ran, paste(src_name, list(...)$db_name)); invisible(NULL)
  })
  done <- suppressMessages(run_sources(c("mimic_iv_demo", "eicu_demo"), db_name = "testdb"))
  expect_identical(done, c(mimic_iv_demo = TRUE, eicu_demo = TRUE))
  expect_identical(ran, c("mimic_iv_demo testdb", "eicu_demo testdb"))
  expect_error(suppressMessages(run_sources(c("eicu_demo", "broken", "mimic_iv_demo"))), "source failed")
  expect_identical(tail(ran, 1), "eicu_demo ")
  expect_error(run_sources(character(0)), "at least one source")
  expect_error(run_sources(c("eicu_demo", "")), "at least one source")
})

test_that("download_sources() downloads each source under its raw-data root", {
  seen <- list()
  local_mocked_bindings(download_source = function(src_name, tables = NULL, raw_data_root = NULL, ...) {
    seen[[src_name]] <<- list(tables = tables, root = raw_data_root); paste0(src_name, ".csv.gz")
  })
  root <- withr::local_tempdir()
  out <- download_sources(c("mimic_iv_demo", "eicu_demo"), tables = "patients", raw_data_root = root)
  expect_identical(out, list(mimic_iv_demo = "mimic_iv_demo.csv.gz", eicu_demo = "eicu_demo.csv.gz"))
  expect_identical(seen$eicu_demo, list(tables = "patients", root = root))

  download_sources("mimic_iv_demo")
  expect_identical(seen$mimic_iv_demo$root, dirname(ehr_paths(src_name = "mimic_iv_demo")$raw_dir))
  expect_null(seen$mimic_iv_demo$tables)
  expect_error(download_sources(character(0)), "at least one source")
})

# ---- SCI classification -----------------------------------------------------

test_that("scid_classify() assigns the SCI categories from the bundled code list", {
  dict <- readRDS(ehr_config_path("scid_icds_rendehr.rds"))
  pick <- function(sci, tr) dict[dict$is_sci == sci & dict$is_tr == tr, c("icd_code", "icd_version")][1, ]
  codes <- rbind(
    cbind(ehr_id = "trauma_sci", pick("1", "1")),
    cbind(ehr_id = "trauma_sci", pick("0", "0")),
    cbind(ehr_id = "nontrauma_sci", pick("1", "0")),
    cbind(ehr_id = "nerve_root", pick("2", "0")),
    cbind(ehr_id = "trauma_control", pick("0", "1")),
    cbind(ehr_id = "other_control", pick("0", "0")))
  diag <- tibble::as_tibble(codes)

  got <- scid_classify(diag, con = NULL, raw_schema = "eicu_demo_coh", src_name = "eicu_demo", group_col = "ehr_id")
  got <- got[match(c("trauma_sci", "nontrauma_sci", "nerve_root", "trauma_control", "other_control"), got$ehr_id), ]
  expect_identical(got$top_level, c("Spinal cord damage", "Spinal cord damage", "Nerve roots injury and disease",
                                    "Spinal control", "Spinal control"))
  expect_identical(got$second_level, c("Traumatic SCI", "Non-traumatic SCI", "Nerve root injury and disease",
                                       "Spinal trauma control", "Spinal non-trauma control"))
  expect_identical(got$scid_trauma_types[1:2], c("traumatic", "non_traumatic"))
  expect_true(all(is.na(got$scid_trauma_types[3:5])))
  # eICU has no ICD procedure table: the procedure block is present but not reported
  proc_cols <- setdiff(names(got), c(names(diag), "top_level", "second_level"))
  expect_true(any(vapply(got[proc_cols], function(x) any(x %in% "not_reported"), logical(1))))
})

test_that("scid_classify() publishes the procedure list for MIMIC with the versions the source uses", {
  published <- NULL; enriched <- NULL
  local_mocked_bindings(
    .clinical_publish_table = function(con, schema, table, data, ...) { published <<- list(schema = schema, table = table, data = data) },
    .sci_enrich_mimic_iii = function(diag_exp, con, raw_schema, group_col, icds_scid, icds_cause, icds_proc, icds_proc_dict) {
      enriched <<- list(proc = icds_proc, n_scid = nrow(icds_scid)); "mimic_iii"
    },
    .sci_enrich_mimic_iv = function(diag_exp, con, raw_schema, group_col, icds_scid, icds_cause, icds_proc, icds_proc_dict) {
      enriched <<- list(proc = icds_proc, n_scid = nrow(icds_scid)); "mimic_iv"
    })
  proc <- ehr_config_path("procedure_classification_icds_rendehr.csv")

  expect_message(out <- scid_classify(data.frame(), con = NULL, raw_schema = "mimic_iii_demo_coh", src_name = "mimic_iii_demo",
                                      group_col = "ehr_id", icds_proc_path = proc), "ICD procedure code")
  expect_identical(out, "mimic_iii")
  expect_identical(published$table, "d_scid_operation_procedures_icds_rendehr")
  expect_identical(unique(as.character(published$data$icd_version)), "9")
  expect_equal(enriched$n_scid, nrow(readRDS(ehr_config_path("scid_icds_rendehr.rds"))))

  published <- NULL
  expect_message(scid_classify(data.frame(), con = NULL, raw_schema = "mimic_iv_demo_coh", src_name = "mimic_iv_demo",
                               group_col = "ehr_id", icds_proc_path = proc), "ICD procedure code")
  expect_setequal(unique(as.character(published$data$icd_version)), c("9", "10"))

  published <- NULL
  expect_identical(scid_classify(data.frame(), con = NULL, raw_schema = "mimic_iv_demo_coh", src_name = "mimic_iv_demo",
                                 group_col = "ehr_id", icds_proc_path = proc, publish = FALSE), "mimic_iv")
  expect_null(published)
  scid_classify(data.frame(), con = NULL, raw_schema = "mimic_iv_demo_coh", src_name = "mimic_iv_demo", group_col = "ehr_id")
  expect_null(enriched$proc)
})

# ---- Explorer ---------------------------------------------------------------

# Prepared SCI files for the admissions of the synthetic explorer bundle.
.ep_sci_files <- function(bundle, dir, with_optional = TRUE) {
  adm <- bundle$adm
  k <- nrow(adm)
  cats <- c("Traumatic SCI", "Non-traumatic SCI", "Spinal trauma control")
  sci_adm <- data.frame(
    source = adm$source, source_label = adm$source_label, patient_id = adm$patient_id, adm_id = adm$adm_id,
    age = adm$age, sex = adm$sex, age_group = adm$age_group, admission_type = adm$admission_type,
    any_icu = adm$any_icu, hospital_los = adm$hospital_los, hospital_mortality = adm$hospital_mortality,
    second_level = rep_len(cats, k), injury_level = rep_len(c("thoracic", "unspecified", "cervical"), k),
    stringsAsFactors = FALSE)
  sci_adm$sci <- ifelse(sci_adm$second_level == "Spinal trauma control", "Not SCI", "SCI")
  sci_adm$level_group <- ifelse(sci_adm$sci == "SCI", rep_len(c("Tetraplegia level (cervical)", "Level unknown"), k), NA)
  sci_adm$completeness3 <- ifelse(sci_adm$sci == "SCI", "Complete", NA)
  sci_adm$cx_pneumonia <- rep_len(c(0L, 2L), k)
  ctx <- data.frame(source = bundle$ctx_adm$source, adm_id = bundle$ctx_adm$adm_id, stringsAsFactors = FALSE)
  ctx$flagged <- ctx$adm_id %in% adm$adm_id
  m <- match(ctx$adm_id, sci_adm$adm_id)
  ctx$second_level <- sci_adm$second_level[m]
  ctx$scid_icd_codes <- ifelse(ctx$flagged, "S14111A", NA)
  ctx$level_group <- sci_adm$level_group[m]
  ctx$completeness <- sci_adm$completeness3[m]
  ctx_icu <- data.frame(source = adm$source[1], patient_id = adm$patient_id[1], adm_id = adm$adm_id[1], icu_id = "9001",
                        icu_in_min = 60, icu_out_min = 600, icu_unit = "sicu", stringsAsFactors = FALSE)
  patients <- bundle$patients
  patients$n_flagged <- 1L
  files <- list(adm = sci_adm, ctx_adm = ctx, ctx_icu = ctx_icu, patients = patients,
                dx = data.frame(), px = data.frame(), codes = data.frame(), bio_daily = data.frame(),
                map_stay = data.frame(), map_values = data.frame(), surgery_timing = data.frame())
  if (with_optional)
    files$dur <- data.frame(source = adm$source[1:2], adm_id = adm$adm_id[1:2], drug = "heparin", stringsAsFactors = FALSE)
  for (n in names(files)) saveRDS(files[[n]], file.path(dir, paste0(n, ".rds")))
  invisible(files)
}

test_that("explorer_sci_data() reads the prepared files and explorer_add_sci() turns on the SCI pages", {
  b <- .xp_fixture_bundle()
  dir <- withr::local_tempdir()
  files <- .ep_sci_files(b, dir)
  out <- withr::local_tempdir()
  dir.create(file.path(out, "tables"))
  readr::write_csv(data.frame(concept = c("heart_rate", "heart_rate", "sodium"), category = c("Vital signs", "Vital signs", "Chemistry")),
                   file.path(out, "tables", "T22a_ts_concept_summary.csv"))

  sci <- explorer_sci_data(dir, out)
  expect_s3_class(sci, "rendehR_explorer_sci")
  expect_identical(nrow(sci$adm), nrow(files$adm))
  expect_identical(nrow(sci$dur), 2L)
  expect_identical(sci$ts_summary$concept, c("heart_rate", "heart_rate", "sodium"))
  expect_null(explorer_sci_data(dir)$ts_summary)

  x <- explorer_add_sci(b, sci)
  expect_identical(x$meta$mode, "sci")
  expect_identical(x$meta$title, "SCI-EHR Explorer")
  expect_true(all(x$adm$in_cohort))
  expect_identical(levels(x$adm$injury_level), c("cervical", "thoracic", "unspecified"))
  expect_true(all(x$adm$cx_pneumonia %in% c(0L, 1L)))      # event counts become 0/1 indicators
  expect_identical(sum(x$ctx_adm$in_cohort), nrow(b$adm))
  expect_true(all(x$ctx_adm$tl_group[!x$ctx_adm$in_cohort] == "Not flagged (context)"))
  expect_identical(x$icu$icu_id, "9001")
  expect_true(all(c("sci", "second_level", ".all") %in% x$groups))
  expect_identical(x$defaults$overview_group, "second_level")
  expect_identical(x$concept_categories, c(heart_rate = "Vital signs", sodium = "Chemistry"))
  expect_identical(sum(x$med_prev$n), 2L)
  expect_null(x$sci$dur)

  unlink(file.path(dir, c("dur.rds", "codes.rds")))
  expect_error(explorer_sci_data(dir), "Missing SCI explorer files.*codes.rds")
  expect_error(explorer_add_sci(list(), sci), "bundle from explorer_data")
  expect_error(explorer_add_sci(b, list()), "must come from explorer_sci_data")
})

test_that("run_explorer() loads the merged schema, adds the SCI pages and starts the app", {
  .ep_keep_session()
  seen <- new.env()
  b <- .xp_fixture_bundle()
  local_mocked_bindings(
    .xp_check_packages = function() invisible(TRUE),
    ehr_connect = function(db_name = NULL, creds = NULL, config_path = NULL, ...) { seen$connect <- db_name; invisible(TRUE) },
    .with_connection = function(con, fn) fn("con"),
    .xp_table_exists = function(con, schema, table) identical(schema, "rendehr_admission"),
    explorer_data = function(schema, raw_dirs = NULL, cache_dir = NULL, ...) { seen$schema <- schema; seen$cache <- cache_dir; b },
    explorer_add_sci = function(data, sci) { seen$sci <- sci; data },
    explorer_app = function(data, live = TRUE) { seen$live <- live; structure(list(data = data), class = "fake_app") })
  local_mocked_bindings(runApp = function(appDir, host, port, launch.browser, ...) {
    seen$run <- list(app = appDir, host = host, port = port, browser = launch.browser); "stopped"
  }, .package = "shiny")

  expect_message(res <- run_explorer(db_name = "testdb", cache_dir = "cache", port = 4321, launch.browser = FALSE),
                 "loading 'rendehr_admission'")
  expect_identical(res, "stopped")
  expect_identical(seen$connect, "testdb")
  expect_identical(seen$schema, "rendehr_admission")
  expect_identical(seen$cache, "cache")
  expect_true(seen$live)
  expect_s3_class(seen$run$app, "fake_app")
  expect_identical(seen$run$host, "127.0.0.1")
  expect_identical(seen$run$port, 4321)
  expect_false(seen$run$browser)
  expect_null(seen$sci)

  seen$schema <- NULL; seen$connect <- NULL
  .ehr_env$creds <- list(postgres = list(user = "u"))
  run_explorer(data = b, sci = "sci", launch.browser = FALSE)
  expect_null(seen$schema)        # a bundle passed in is used as is
  expect_null(seen$connect)       # an existing session is reused
  expect_identical(seen$sci, "sci")
})

# ---- Live checks (test database and demo files) -----------------------------

test_that("browse_psql() and browse_ehr_db_flex() read a live PostgreSQL server", {
  db <- Sys.getenv("EHR_TEST_DB", "")
  skip_if(!nzchar(db), "set EHR_TEST_DB to run the live browsing tests")
  skip_if_not_installed("RPostgres")
  creds <- .ep_pg_creds()
  dbs <- browse_psql(creds = creds)
  expect_true(db %in% dbs$db_name)
  expect_identical(dbs$browse_code[dbs$db_name == db], sprintf('browse_ehr_db_flex(db_name = "%s")', db))
  expect_false(any(dbs$db_name %in% c("postgres", "template0", "template1")))

  con <- connect_postgres(db, creds)
  withr::defer(DBI::dbDisconnect(con))
  b <- browse_ehr_db_flex(con = con, db_name = db, view = "both")
  expect_false(any(b$tables$table_schema %in% c("pg_catalog", "information_schema")))
  expect_identical(nrow(b$tables), length(b$flattened))
})

test_that("build_window_tables() rebuilds the demo window tables unchanged", {
  skip_if(!nzchar(Sys.getenv("EHR_TEST_DB")), "set EHR_TEST_DB to rebuild the demo window tables")
  skip_unless_pipeline_table("mimic_iv_demo_coh", "icu_windows")
  .ep_keep_session()
  db <- Sys.getenv("EHR_TEST_DB")
  creds <- .ep_pg_creds()
  con <- connect_postgres(db, creds)
  withr::defer(DBI::dbDisconnect(con))
  counts <- function() vapply(c("admission_windows", "icu_windows", "visit_windows"), function(t)
    as.numeric(DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM mimic_iv_demo_coh.%s", t))$n), numeric(1))
  before <- counts()
  local_mocked_bindings(load_credentials = function(path = NULL) creds)
  expect_message(build_window_tables("mimic_iv_demo", db_name = db, base_dir = withr::local_tempdir()),
                 "Window tables built")
  expect_identical(counts(), before)
})

test_that("build_sepsis_cohort() selects the Sepsis-3 stays of the MIMIC-III demo", {
  raw <- Sys.getenv("EHR_TEST_E2E_RAW_ROOT", "")
  skip_if(!nzchar(raw) || !dir.exists(file.path(raw, "mimic_iii_demo")),
          "set EHR_TEST_E2E_RAW_ROOT to a folder with mimic_iii_demo to run the Sepsis-3 selection")
  skip_if_not_installed("duckdb")
  out <- withr::local_tempfile(fileext = ".csv")
  ids <- suppressMessages(build_sepsis_cohort("mimic_iii_demo", base_dir = withr::local_tempdir(), raw_data_root = raw,
                                              out_csv = out, import = FALSE, show_progress = FALSE))
  expect_gt(nrow(ids), 0)
  expect_true("icustay_id" %in% names(ids))
  expect_equal(anyDuplicated(ids$icustay_id), 0L)
  expect_identical(nrow(utils::read.csv(out)), nrow(ids))
})

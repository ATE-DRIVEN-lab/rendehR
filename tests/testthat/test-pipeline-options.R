# Isolate orchestration from storage; no source data or live database is needed.
.option_test_function <- function(fn, bindings = list()) {
  env <- list2env(bindings, parent = environment(fn))
  environment(fn) <- env
  fn
}

test_that("option constructors store only explicit overrides, including NULL", {
  expect_length(loading_options(), 0L)
  expect_identical(concept_options(layers = "flat"), list(layers = "flat"))
  expect_identical(cohort_selection_options(cohort = NULL), list(cohort = NULL))
  expect_error(concept_options(layers = "sciehr"), "disabled")
  expect_error(loading_options(ignore_errors = NA), "TRUE or FALSE")
})

test_that("groups and compatibility arguments resolve before project work", {
  expect_error(run_one_source(concepts = list(layres = "flat")), "Unknown concepts")
  expect_error(run_one_source(concepts = list("flat")), "unique, non-empty")
  expect_error(run_one_source(concept_name = "x",
    concepts = concept_options(concept_name = "x")), "not both")
  expect_error(run_one_source(unknown_option = TRUE), "Unknown run_one_source")
  expect_error(run_one_source(concepts = concept_options(layers = "ehr"),
                             engine = "duckdb"), "requires PostgreSQL")
  expect_error(run_one_source(delete_source_csv = TRUE), "retired")
})

test_that("pipeline forwards grouped settings and preserves default stage behavior", {
  calls <- new.env(parent = emptyenv())
  capture <- function(name) function(...) { calls[[name]] <- list(...); invisible(NULL) }
  runner <- .option_test_function(run_one_source, list(
    ehr_paths = function(...) list(src_name = "mimic_iv", base_dir = ".", cohort_profile = "general"),
    ehr_init_project = function(...) NULL,
    .ehr_resolve_db_name = function(...) "test_db",
    import_cohort_general = capture("import"),
    run_db = capture("db"),
    run_raw_mods = capture("raw"),
    .ehr_has_general_window_spine = function(...) TRUE,
    build_cohort_info_general = capture("info"),
    run_concepts = capture("concepts"),
    run_harmonized_tables = capture("harmonization")
  ))
  result <- suppressMessages(runner("mimic_iv"))
  expect_identical(result$stage, c("import_cohort_general", "run_db", "run_raw_mods",
    "build_window_tables", "build_cohort_info_general", "run_concepts", "run_harmonized_tables"))
  expect_null(calls$import$overwrite)
  expect_null(calls$import$store_rejects)
  expect_null(calls$import$table_chunk_rows)
  expect_null(calls$db$create_indexes)
  expect_null(calls$concepts$layers)
  expect_null(calls$harmonization$build_dur)
  expect_null(calls$info$analysis_level)
  suppressMessages(runner("mimic_iv", stages = c("import_cohort", "run_db", "run_concepts"),
    cohort_selection = cohort_selection_options(direct_ids_path = "ids.csv"),
    loading = loading_options(create_indexes = FALSE),
    execution = execution_options(overwrite = TRUE),
    concepts = concept_options(layers = "flat")))
  expect_identical(calls$import$direct_ids_path, "ids.csv")
  expect_true(calls$import$overwrite)
  expect_true(calls$db$overwrite)
  expect_false(calls$db$create_indexes)
  expect_identical(calls$concepts$layers, "flat")
  suppressMessages(runner("mimic_iv", stages = "run_concepts", concept_name = "legacy"))
  expect_identical(calls$concepts$concept_name, "legacy")
})

test_that("original import runner accepts groups and retains its old API", {
  runner <- .option_test_function(import_cohort_general, list(
    .ehr_run_cohort_engine = function(...) list(...)))
  grouped <- runner("mimic_iv",
    cohort_selection = cohort_selection_options(direct_ids_path = "ids.csv", cohort = NULL),
    windows = window_options(window_sql = "windows.sql"),
    loading = loading_options(threads = 2),
    execution = execution_options(overwrite = TRUE))
  legacy <- runner("mimic_iv", direct_ids_path = "ids.csv", cohort = NULL,
                   window_sql = "windows.sql", threads = 2, overwrite = TRUE)
  expect_identical(grouped, legacy)
  expect_error(runner("mimic_iv", threads = 1, loading = loading_options(threads = 2)), "not both")
  expect_identical(runner("mimic_iv", loading = loading_options(create_indexes = FALSE)),
                   runner("mimic_iv"))
})

test_that("direct stage functions resolve groups before doing storage work", {
  cases <- list(
    list(run_db, "loading", loading_options(create_indexes = FALSE), "create_indexes", FALSE),
    list(run_harmonized_tables, "harmonization", harmonization_options(build_dur = FALSE), "build_dur", FALSE),
    list(build_cohort_info_general, "enrichment", enrichment_options(analysis_level = "icu"), "analysis_level", "icu"),
    list(build_cohort_annotations, "enrichment", enrichment_options(classify_args = list(x = 1)), "classify_args", list(x = 1)),
    list(run_comorbidities, "enrichment", enrichment_options(publish = FALSE), "publish", FALSE),
    list(run_iss, "enrichment", enrichment_options(publish = FALSE), "publish", FALSE),
    list(run_raw_mods, "execution", execution_options(skip_on_error = TRUE), "skip_on_error", TRUE),
    list(cache_raw_parquet_duckdb, "loading", loading_options(threads = 3), "threads", 3)
  )
  for (case in cases) {
    fn <- case[[1]]
    # Retain the actual group-resolution expression and inspect its resolved value.
    body(fn) <- as.call(list(as.name("{"), body(fn)[[2]], as.name(case[[4]])))
    args <- list(src_name = "mimic_iv")
    args[case[[2]]] <- list(case[[3]])
    expect_identical(do.call(fn, args), case[[5]])
  }
})

test_that("layer selection preserves order, provenance, and failure aggregation", {
  tmp <- tempfile("layers-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  for (d in c("ehr", "user", "external")) dir.create(file.path(tmp, d))
  writeLines("SELECT 1;", file.path(tmp, "user", "example.sql"))
  writeLines("SELECT 1;", file.path(tmp, "ehr", "example.sql"))
  paths <- list(credentials_json = "unused", helper_sql_dir = file.path(tmp, "missing"),
    pkg_flat_sql_dir = "flat", concept_config_csv_ts = "ts.csv", pkg_ts_sql_dir = "ts",
    concept_config_csv_dur = "dur.csv", pkg_dur_sql_dir = "dur",
    pkg_ehr_sql_dir = file.path(tmp, "ehr"), pkg_rendehr_sql_dir = "rendehr",
    user_sql_dir = file.path(tmp, "user"), external_sql_dir = file.path(tmp, "external"))
  calls <- character()
  provenance <- character()
  fail_ts <- FALSE
  record <- function(layer) { calls <<- c(calls, layer); c(example = TRUE) }
  runner <- .option_test_function(run_concepts, list(
    ehr_paths = function(...) paths,
    src_schema_names = function(...) list(raw = "raw", concepts = "concepts", derived = "derived"),
    src_family = function(...) "mimic_iv",
    .ehr_resolve_db_name = function(...) "test_db",
    load_credentials = function(...) list(), ensure_database_exists = function(...) NULL,
    connect_postgres = function(...) list(), connect_duckdb = function(...) list(),
    create_schemas = function(...) NULL,
    run_flat_sql_concepts = function(...) record("flat"),
    run_atomic_concepts_unified = function(..., typ) {
      result <- record(typ)
      if (typ == "ts" && fail_ts) result[] <- FALSE
      result
    },
    .run_user_sql_dir = function(...) record("user"),
    run_external_concepts = function(..., sql_dir) record(basename(sql_dir)),
    .snapshot_tables = function(...) character(),
    .register_pkg_layer_provenance = function(...) { provenance <<- c(provenance, "flat") },
    auto_register_atomic_provenance = function(...) { provenance <<- c(provenance, "atomic") },
    .register_user_layer_provenance = function(...) { provenance <<- c(provenance, "user") },
    .register_new_tables = function(...) { provenance <<- c(provenance, "external") }
  ))
  local_mocked_bindings(dbExecute = function(...) 0L, dbDisconnect = function(...) TRUE,
                        .package = "DBI")
  result <- suppressMessages(runner("mimic_iv", concepts = concept_options(layers = c("ts", "flat"))))
  expect_identical(calls, c("flat", "ts"))
  expect_named(result, c("flat", "ts"))
  expect_identical(provenance, c("flat", "atomic"))
  calls <- provenance <- character()
  suppressMessages(runner("mimic_iv"))
  expect_identical(calls, c("flat", "ts", "dur", "ehr", "user", "external"))
  calls <- provenance <- character()
  suppressMessages(runner("mimic_iv", layers = "user", dry_run = TRUE))
  expect_identical(calls, "user")
  expect_length(provenance, 0)
  calls <- character()
  suppressMessages(runner("mimic_iv", engine = "duckdb"))
  expect_false("ehr" %in% calls)
  fail_ts <- TRUE
  expect_error(suppressMessages(runner("mimic_iv", layers = "ts")), "ts/example")
})

test_that("validation runner forwards grouped options on rerun", {
  captured <- NULL
  attempts <- 0L
  runner <- .option_test_function(run_one_source_test, list(
    ehr_paths = function(...) list(src_name = "mimic_iv", base_dir = ".", cohort_profile = "general"),
    .ehr_resolve_db_name = function(...) "test_db",
    .ehr_validate_stage = function(...) {
      attempts <<- attempts + 1L
      list(ok = attempts > 1L, reason = "test")
    },
    run_one_source = function(...) { captured <<- list(...); invisible(NULL) }
  ))
  result <- suppressMessages(runner("mimic_iv", stages = "import_cohort",
    cohort_selection = cohort_selection_options(direct_ids_path = "ids.csv"),
    loading = loading_options(threads = 2)))
  expect_identical(captured$direct_ids_path, "ids.csv")
  expect_identical(captured$threads, 2)
  expect_identical(result$status, "success")
  expect_error(runner("mimic_iv", direct_ids_path = "old.csv",
    cohort_selection = cohort_selection_options(direct_ids_path = "new.csv")), "not both")
})

test_that("empty groups preserve direct cache defaults distinct from pipeline defaults", {
  fn <- cache_raw_parquet_duckdb
  body(fn) <- as.call(list(as.name("{"), body(fn)[[2]], as.name("validate_columns")))
  expect_true(fn("mimic_iv", loading = loading_options()))
  expect_false(.ehr_resolve_pipeline_options(list(loading = loading_options()), list())$validate_columns)
  expect_true(.ehr_resolve_pipeline_options(list(), list())$ignore_errors)
})

test_that("all migrated pipeline defaults match the pre-grouping API", {
  original <- dget(test_path("fixtures", "pipeline-option-defaults.R"))
  resolved <- .ehr_resolve_pipeline_options(list(
    cohort_selection = cohort_selection_options(), windows = window_options(),
    loading = loading_options(), enrichment = enrichment_options(),
    concepts = concept_options(), harmonization = harmonization_options(),
    execution = execution_options()), list())
  expect_length(original, 41L)
  expect_identical(resolved[names(original)], original)
  expect_null(resolved$layers)
})

test_that("legacy cohort cannot partially match the new cohort_selection group", {
  captured <- NULL
  runner <- .option_test_function(run_one_source, list(
    ehr_paths = function(...) list(src_name = "mimic_iv", base_dir = ".", cohort_profile = "general"),
    ehr_init_project = function(...) NULL,
    .ehr_resolve_db_name = function(...) "test_db",
    import_cohort_general = function(...) { captured <<- list(...); NULL }
  ))
  old_cohort <- list(ids = c(1L, 2L))
  suppressMessages(runner("mimic_iv", stages = "import_cohort", cohort = old_cohort))
  expect_identical(captured$cohort, old_cohort)
  expect_error(runner("mimic_iv", cohort = old_cohort,
    cohort_selection = cohort_selection_options(cohort = old_cohort)), "not both")
})

# Regression tests for grouped-option forwarding and early validation.
test_that("both entry points validate engine before project work", {
  for (fn in list(run_one_source, run_one_source_test)) {
    runner <- .option_test_function(fn, list(
      ehr_paths = function(...) stop("project work started")))
    expect_error(runner("x", engine = "garbage"), "arg.*should be one of")
    expect_error(runner("x", "y", "z", "/tmp/old-filter.rds"), "arg.*should be one of")
  }
  expect_identical(setdiff(names(formals(run_one_source_test)), "require_log"),
                   names(formals(run_one_source)))
})

test_that("shared groups retain applicable settings and reject invalid ones", {
  imports <- .option_test_function(import_cohort_general, list(
    .ehr_run_cohort_engine = function(...) list(...)))
  out <- imports("mimic_iii",
    loading = loading_options(threads = 4, create_indexes = FALSE),
    execution = execution_options(skip_on_error = TRUE, overwrite = TRUE))
  expect_identical(out$threads, 4)
  expect_true(out$overwrite)
  expect_false("create_indexes" %in% names(out))
  expect_error(imports("mimic_iii", loading = list(threds = 4)), "Unknown loading")
  expect_error(imports("mimic_iii", threads = 2,
    loading = loading_options(threads = 4)), "not both")
  expect_error(.ehr_flatten_options(list(misspelled = list())), "Unknown option group")
  fields <- .ehr_option_fields
  fields$execution <- c(fields$execution, fields$loading[[1]])
  flatten <- .option_test_function(.ehr_flatten_options, list(.ehr_option_fields = fields))
  expect_error(flatten(list()), "exactly one group")
})

test_that("cache and import receive resources without flattening their defaults", {
  calls <- new.env()
  cache <- function(src_name, base_dir, raw_data_root, all_tables, keep,
                    validate_columns = TRUE, memory_limit = NULL, threads = NULL,
                    temp_directory = NULL, show_progress = TRUE) {
    calls$cache <- as.list(environment()); NULL
  }
  importer <- function(src_name, base_dir, raw_data_root,
                       validate_columns = FALSE, memory_limit = NULL, threads = NULL,
                       temp_directory = NULL, show_progress = TRUE,
                       table_chunk_rows = 5e6) {
    calls$import <- as.list(environment()); NULL
  }
  for (fn in list(run_one_source, run_one_source_test)) {
    runner <- .option_test_function(fn, list(
      ehr_paths = function(...) list(src_name = "mimic_iii", base_dir = ".", cohort_profile = "general"),
      ehr_init_project = function(...) NULL,
      .ehr_resolve_db_name = function(...) "test_db",
      cache_raw_parquet_duckdb = cache, import_cohort_general = importer,
      # For resume tests, invoke the production runner with the same stage doubles.
      run_one_source = .option_test_function(run_one_source, list(
        ehr_paths = function(...) list(src_name = "mimic_iii", base_dir = ".", cohort_profile = "general"),
        ehr_init_project = function(...) NULL,
        .ehr_resolve_db_name = function(...) "test_db",
        cache_raw_parquet_duckdb = cache, import_cohort_general = importer)),
      .ehr_validate_stage = function(...) list(ok = TRUE, reason = "valid"),
      .ehr_with_stage_con = function(...) NULL))
    args <- list("mimic_iii", stages = c("cache_raw_parquet", "import_cohort"),
                 execution = execution_options(overwrite = TRUE))
    suppressMessages(do.call(runner, args))
    expect_true(calls$cache$validate_columns)
    expect_false(calls$import$validate_columns)
    args$loading <- loading_options(memory_limit = "32GB", threads = 4,
      temp_directory = "/tmp/duck-spill", validate_columns = FALSE,
      table_chunk_rows = NULL, create_indexes = FALSE)
    args$execution <- execution_options(overwrite = TRUE, show_progress = FALSE)
    suppressMessages(do.call(runner, args))
    for (out in list(calls$cache, calls$import)) {
      expect_identical(out$memory_limit, "32GB")
      expect_identical(out$threads, 4)
      expect_identical(out$temp_directory, "/tmp/duck-spill")
      expect_false(out$show_progress)
      expect_false(out$validate_columns)
    }
    expect_null(calls$import$table_chunk_rows)
  }
})

test_that("constructor reference defaults agree with their primary stages", {
  defaults <- .ehr_pipeline_option_defaults()
  constructors <- c("cohort_selection_options", "window_options", "loading_options",
                    "enrichment_options", "concept_options", "harmonization_options",
                    "execution_options")
  for (name in constructors) {
    fn <- get(name, mode = "function")
    for (key in names(formals(fn)))
      expect_identical(eval(formals(fn)[[key]], environment(fn)), defaults[[key]],
                       info = paste(name, key))
  }
})

test_that("resume validation and logs receive the chosen engine", {
  engines <- character(); forwarded <- NULL
  runner <- .option_test_function(run_one_source_test, list(
    ehr_paths = function(...) list(src_name = "mimic_iii", base_dir = ".", cohort_profile = "general"),
    .ehr_resolve_db_name = function(...) "test_db",
    .ehr_validate_stage = function(..., engine, require_log) {
      engines <<- c(engines, engine)
      list(ok = !require_log, reason = "checked")
    },
    run_one_source = function(...) { forwarded <<- list(...); NULL },
    .ehr_with_stage_con = function(stage, engine, ...) {
      engines <<- c(engines, engine); NULL
    }))
  result <- suppressMessages(runner("mimic_iii", engine = "duckdb",
    stages = "run_concepts", concepts = concept_options(layers = "flat")))
  expect_identical(forwarded$engine, "duckdb")
  expect_identical(forwarded$layers, "flat")
  expect_identical(engines, rep("duckdb", 3))
  expect_identical(result$status, "success")
  expect_error(runner("mimic_iii", engine = "invalid"), "arg.*should be one of")
})

test_that("stage connection routing never inspects PostgreSQL for the DuckDB engine", {
  connector <- .option_test_function(.ehr_with_stage_con, list(
    connect_duckdb = function(path) stop(paste("duck", path)),
    .ehr_with_pipeline_con = function(...) "postgres"))
  for (stage in c("run_concepts", "run_db", "build_cohort_info"))
    expect_error(connector(stage, "duckdb", "db", list(duckdb_path = "cohort.duckdb"), identity),
                 "duck cohort.duckdb", info = stage)
  expect_identical(connector("run_db", "postgres", "db", list(), identity), "postgres")
  expect_identical(connector("run_concepts", "postgres", "db", list(), identity), "postgres")
})


test_that("raw cache resume checks inspect files rather than PostgreSQL", {
  validator <- .option_test_function(.ehr_validate_stage, list(
    .ehr_parquet_files_valid = function(paths) list(ok = TRUE, reason = paths$cohort_parquet_dir),
    .ehr_with_stage_con = function(...) stop("unexpected database check")))
  paths <- list(raw_parquet_cache_dir = "raw-cache", cohort_parquet_dir = "cohort")
  expect_false(validator("cache_raw_parquet", "mimic_iii", "db", paths)$ok)
  expect_identical(validator("cache_raw_parquet", "mimic_iii", "db", paths,
                             require_log = FALSE)$reason, "raw-cache")
})

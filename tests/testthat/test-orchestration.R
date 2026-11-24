# Orchestration and high-level API tests (R/api.R, and the R/pipeline.R
# symbols not already covered by the run_concepts()/
# .ehr_check_concept_layer_results()/.ehr_resolve_pipeline_stages() blocks
# in test-pipeline.R).

# ---------------------------------------------------------------------------
# build_cohort()/extract_concepts()/harmonize_concepts() must read the
# real nested ehr_load_config() shape, not the retired flat fields. Each
# test uses a real config object (not a mock of the config itself) so a
# mismatch with the real shape is caught; only the underlying stage runner
# is mocked, to avoid a live database.
# ---------------------------------------------------------------------------

test_that("build_cohort() forwards the real nested config fields to run_db()", {
  tmp <- tempfile("s010_api_build_cohort_")
  cfg_path <- file.path(tmp, "config", "ehr_config.json")
  ehr_create_config_template(path = cfg_path, src_name = "eicu_demo",
                             db_name = "s010_manual_db", base_dir = tmp,
                             overwrite = TRUE)
  cfg <- ehr_load_config(cfg_path)
  expect_null(cfg$src_name)
  expect_equal(cfg$source$src_name, "eicu_demo")

  captured <- NULL
  local_mocked_bindings(
    run_db = function(src_name, db_name = NULL, base_dir = NULL, ...) {
      captured <<- list(src_name = src_name, db_name = db_name, base_dir = base_dir)
      invisible(NULL)
    }
  )

  build_cohort(cfg)
  expect_equal(captured$src_name, "eicu_demo")
  expect_equal(captured$db_name, "s010_manual_db")
  expect_equal(captured$base_dir, tmp)
})

test_that("extract_concepts() forwards the real nested config fields to run_concepts()", {
  tmp <- tempfile("s010_api_extract_concepts_")
  cfg_path <- file.path(tmp, "config", "ehr_config.json")
  ehr_create_config_template(path = cfg_path, src_name = "mimic_iii_demo",
                             db_name = "s010_manual_db2", base_dir = tmp,
                             overwrite = TRUE)
  cfg <- ehr_load_config(cfg_path)

  captured <- NULL
  local_mocked_bindings(
    run_concepts = function(src_name, db_name = NULL, base_dir = NULL, ...) {
      captured <<- list(src_name = src_name, db_name = db_name, base_dir = base_dir)
      invisible(NULL)
    }
  )

  extract_concepts(cfg)
  expect_equal(captured$src_name, "mimic_iii_demo")
  expect_equal(captured$db_name, "s010_manual_db2")
  expect_equal(captured$base_dir, tmp)
})

test_that("harmonize_concepts() forwards the real nested config fields to run_harmonized_tables()", {
  tmp <- tempfile("s010_api_harmonize_concepts_")
  cfg_path <- file.path(tmp, "config", "ehr_config.json")
  ehr_create_config_template(path = cfg_path, src_name = "mimic_iv_demo",
                             db_name = "s010_manual_db3", base_dir = tmp,
                             overwrite = TRUE)
  cfg <- ehr_load_config(cfg_path)

  captured <- NULL
  local_mocked_bindings(
    run_harmonized_tables = function(base_dir = NULL, src_name, db_name = NULL, ...) {
      captured <<- list(src_name = src_name, db_name = db_name, base_dir = base_dir)
      invisible(NULL)
    }
  )

  harmonize_concepts(cfg)
  expect_equal(captured$src_name, "mimic_iv_demo")
  expect_equal(captured$db_name, "s010_manual_db3")
  expect_equal(captured$base_dir, tmp)
})

# ---------------------------------------------------------------------------
# .ehr_has_general_window_spine()'s identifier validation.
# Mirrors the run_concepts() caller-level integration test's full-mock
# pattern (test-pipeline.R): connect_postgres() is mocked directly rather
# than redirected to a DuckDB connection, because the function opens its
# own connection internally.
# ---------------------------------------------------------------------------

test_that(".ehr_has_general_window_spine(): rejects a malformed schema name before any query runs", {
  fake_paths <- list(src_name = "bad_src")
  local_mocked_bindings(
    ehr_paths          = function(...) fake_paths,
    .ehr_resolve_db_name = function(...) "fake_db",
    src_schema_names   = function(...) list(raw = "bad-schema; drop table x"),
    load_credentials   = function(...) list(),
    connect_postgres   = function(...) structure(list(), class = "fake_con")
  )
  local_mocked_bindings(dbDisconnect = function(...) invisible(TRUE), .package = "DBI")

  expect_false(.ehr_has_general_window_spine("bad_src"))
})

test_that(".ehr_has_general_window_spine(): TRUE only when all four tables and cohort_index columns are present", {
  fake_paths <- list(src_name = "mimic_iv_demo")
  captured_query <- NULL
  local_mocked_bindings(
    ehr_paths          = function(...) fake_paths,
    .ehr_resolve_db_name = function(...) "fake_db",
    src_schema_names   = function(...) list(raw = "mimic_iv_demo_coh"),
    load_credentials   = function(...) list(),
    connect_postgres   = function(...) structure(list(), class = "fake_con")
  )
  local_mocked_bindings(
    dbGetQuery = function(con, query, ...) {
      captured_query <<- query
      data.frame(table_name = c("cohort_meta", "admission_windows",
                                "icu_windows", "visit_windows"))
    },
    dbListFields = function(con, id, ...) c("cohort_index", "other_col"),
    dbQuoteString = function(con, x, ...) paste0("'", x, "'"),
    dbDisconnect  = function(...) invisible(TRUE),
    .package = "DBI"
  )

  expect_true(.ehr_has_general_window_spine("mimic_iv_demo"))
  expect_match(captured_query, "'mimic_iv_demo_coh'", fixed = TRUE)
})

test_that(".ehr_has_general_window_spine(): FALSE when a required table is missing", {
  fake_paths <- list(src_name = "mimic_iv_demo")
  local_mocked_bindings(
    ehr_paths          = function(...) fake_paths,
    .ehr_resolve_db_name = function(...) "fake_db",
    src_schema_names   = function(...) list(raw = "mimic_iv_demo_coh"),
    load_credentials   = function(...) list(),
    connect_postgres   = function(...) structure(list(), class = "fake_con")
  )
  local_mocked_bindings(
    dbGetQuery = function(con, query, ...) {
      data.frame(table_name = c("cohort_meta", "admission_windows"))
    },
    dbQuoteString = function(con, x, ...) paste0("'", x, "'"),
    dbDisconnect  = function(...) invisible(TRUE),
    .package = "DBI"
  )

  expect_false(.ehr_has_general_window_spine("mimic_iv_demo"))
})

test_that(".ehr_has_general_window_spine(): FALSE when a table exists but lacks cohort_index", {
  fake_paths <- list(src_name = "mimic_iv_demo")
  local_mocked_bindings(
    ehr_paths          = function(...) fake_paths,
    .ehr_resolve_db_name = function(...) "fake_db",
    src_schema_names   = function(...) list(raw = "mimic_iv_demo_coh"),
    load_credentials   = function(...) list(),
    connect_postgres   = function(...) structure(list(), class = "fake_con")
  )
  local_mocked_bindings(
    dbGetQuery = function(con, query, ...) {
      data.frame(table_name = c("cohort_meta", "admission_windows",
                                "icu_windows", "visit_windows"))
    },
    dbListFields = function(con, id, ...) c("some_other_col"),
    dbQuoteString = function(con, x, ...) paste0("'", x, "'"),
    dbDisconnect  = function(...) invisible(TRUE),
    .package = "DBI"
  )

  expect_false(.ehr_has_general_window_spine("mimic_iv_demo"))
})

# ---------------------------------------------------------------------------
# ehr_suggest_next()'s multi-source grouping logic.
# ---------------------------------------------------------------------------

test_that("ehr_suggest_next(): sources sharing an identical missing-stage set are grouped into one run_sources() recommendation", {
  local_mocked_bindings(
    .ehr_validate_stage = function(stage, src_name, db_name, base_dir, paths,
                                   run_cohort_annotations, require_log = TRUE) {
      if (src_name %in% c("mimic_iv_demo", "eicu_demo") &&
          stage %in% c("run_iss", "run_concepts")) {
        return(list(ok = FALSE, reason = "not done"))
      }
      if (identical(src_name, "mimic_iii_demo") &&
          identical(stage, "run_harmonized_tables")) {
        return(list(ok = FALSE, reason = "not done"))
      }
      list(ok = TRUE, reason = "done")
    }
  )

  out <- ehr_suggest_next(
    src_names = c("mimic_iv_demo", "eicu_demo", "mimic_iii_demo"),
    db_name = "d", base_dir = ".",
    stages = c("run_iss", "run_concepts", "run_harmonized_tables"),
    suggest_merge = FALSE
  )

  grouped <- out$recommended_code[grepl("^run_sources\\(", out$recommended_code)]
  expect_length(grouped, 1L)
  expect_match(grouped, 'c("mimic_iv_demo", "eicu_demo")', fixed = TRUE)
  expect_match(grouped, 'stages = c("run_iss", "run_concepts")', fixed = TRUE)

  single <- out$recommended_code[grepl("^run_one_source\\(", out$recommended_code)]
  expect_length(single, 1L)
  expect_match(single, 'src_name = "mimic_iii_demo"', fixed = TRUE)
  expect_match(single, 'stages = c("run_harmonized_tables")', fixed = TRUE)
})

test_that("ehr_suggest_next(): a single incomplete source still gets a run_one_source() recommendation, not run_sources()", {
  local_mocked_bindings(
    .ehr_validate_stage = function(stage, src_name, db_name, base_dir, paths,
                                   run_cohort_annotations, require_log = TRUE) {
      list(ok = !identical(stage, "run_iss"), reason = "x")
    }
  )

  out <- ehr_suggest_next(src_names = "mimic_iv_demo", db_name = "d",
                          base_dir = ".", stages = c("run_iss", "run_concepts"),
                          suggest_merge = FALSE)

  expect_false(any(grepl("^run_sources\\(", out$recommended_code)))
  expect_true(any(grepl("^run_one_source\\(", out$recommended_code)))
})

# ---------------------------------------------------------------------------
# run_one_source_test() does not replicate run_one_source()'s
# run_raw_mods-failure-stops-everything rule (characterization test for
# the documented, accepted behavioral difference).
# ---------------------------------------------------------------------------

test_that("run_one_source_test(): a failed run_raw_mods does not stop later stages when skip_on_error = TRUE", {
  fake_paths <- list(src_name = "mimic_iv_demo", base_dir = ".",
                     cohort_profile = "custom")
  attempted <- character(0)
  local_mocked_bindings(
    ehr_paths                    = function(...) fake_paths,
    .ehr_resolve_db_name          = function(...) "fake_db",
    src_schema_names             = function(...) list(raw = "mimic_iv_demo_coh"),
    .ehr_validate_stage           = function(stage, ...) list(ok = FALSE, reason = "not done"),
    .ehr_with_pipeline_con        = function(db_name, paths, fn) fn(structure(list(), class = "fake_con")),
    .ehr_write_stage_log          = function(...) invisible(TRUE),
    run_one_source = function(src_name, db_name, base_dir, stages, ...) {
      attempted <<- c(attempted, stages)
      if (identical(stages, "run_raw_mods")) {
        stop("Required raw modifications are incomplete; downstream stages refused.",
             call. = FALSE)
      }
      invisible(data.frame())
    }
  )

  expect_no_error(
    run_one_source_test(
      src_name = "mimic_iv_demo", db_name = "d", base_dir = ".",
      stages = c("run_raw_mods", "run_concepts"),
      skip_on_error = TRUE
    )
  )
  expect_setequal(attempted, c("run_raw_mods", "run_concepts"))
})

test_that("run_one_source_test(): skips a stage whose output already validates", {
  fake_paths <- list(src_name = "mimic_iv_demo", base_dir = ".",
                     cohort_profile = "custom")
  ran <- character(0)
  harmonized_post_check_calls <- 0L
  local_mocked_bindings(
    ehr_paths                    = function(...) fake_paths,
    .ehr_resolve_db_name          = function(...) "fake_db",
    src_schema_names             = function(...) list(raw = "mimic_iv_demo_coh"),
    .ehr_validate_stage           = function(stage, ...) {
      if (identical(stage, "run_concepts")) return(list(ok = TRUE, reason = "already done"))
      # run_harmonized_tables: not done on the pre-check, done on the
      # post-run check (simulating a stage that actually gets run).
      harmonized_post_check_calls <<- harmonized_post_check_calls + 1L
      if (harmonized_post_check_calls > 1L) list(ok = TRUE, reason = "now done")
      else list(ok = FALSE, reason = "not done")
    },
    .ehr_with_pipeline_con        = function(db_name, paths, fn) fn(structure(list(), class = "fake_con")),
    .ehr_write_stage_log          = function(...) invisible(TRUE),
    run_one_source = function(src_name, db_name, base_dir, stages, ...) {
      ran <<- c(ran, stages)
      invisible(data.frame())
    }
  )

  out <- run_one_source_test(
    src_name = "mimic_iv_demo", db_name = "d", base_dir = ".",
    stages = c("run_concepts", "run_harmonized_tables")
  )

  expect_false("run_concepts" %in% ran)
  expect_true("run_harmonized_tables" %in% ran)
  expect_equal(out$status[out$stage == "run_concepts"], "skipped")
  expect_equal(out$status[out$stage == "run_harmonized_tables"], "success")
})

# ---------------------------------------------------------------------------
# Remaining symbols with real validation/decision logic or user-visible
# output.
# ---------------------------------------------------------------------------

test_that(".ehr_print_stage_summary(): prints one line per stage and is a no-op on an empty log", {
  expect_no_error(.ehr_print_stage_summary(data.frame(
    stage = character(), status = character(), seconds = numeric(),
    message = character()
  )))

  log <- data.frame(stage = "run_db", status = "success", seconds = 1.23,
                    message = "", stringsAsFactors = FALSE)
  expect_message(.ehr_print_stage_summary(log), "run_db")
  expect_message(.ehr_print_stage_summary(log), "SUCCESS")
})

test_that(".ehr_origin_label(): classifies argument, config, and default origins", {
  old_config <- .ehr_env$config
  on.exit(.ehr_env$config <- old_config, add = TRUE)

  expect_equal(.ehr_origin_label(TRUE), "argument")

  .ehr_env$config <- list(source = list(src_name = "eicu_demo"))
  expect_equal(.ehr_origin_label(FALSE), "config")

  .ehr_env$config <- NULL
  expect_equal(.ehr_origin_label(FALSE), "default")
})

test_that(".ehr_merge_outputs_valid(): reports missing merged tables by name", {
  local_mocked_bindings(
    ehr_paths              = function(...) list(src_name = "mimic_iv_demo"),
    .ehr_with_pipeline_con   = function(db_name, paths, fn) fn(structure(list(), class = "fake_con")),
    .ehr_table_exists        = function(con, schema, table) identical(table, "ts")
  )

  out <- .ehr_merge_outputs_valid(c("mimic_iv_demo", "eicu_demo"), "d", ".", "rendehr")
  expect_false(out$ok)
  expect_match(out$reason, "flat")
  expect_match(out$reason, "cohort_info")
})

test_that(".ehr_merge_outputs_valid(): ok when all three merged tables exist", {
  local_mocked_bindings(
    ehr_paths              = function(...) list(src_name = "mimic_iv_demo"),
    .ehr_with_pipeline_con   = function(db_name, paths, fn) fn(structure(list(), class = "fake_con")),
    .ehr_table_exists        = function(con, schema, table) TRUE
  )

  out <- .ehr_merge_outputs_valid(c("mimic_iv_demo", "eicu_demo"), "d", ".", "rendehr")
  expect_true(out$ok)
})

test_that("print.ehr_suggestion(): renders sources, per-stage status, and recommended code", {
  x <- structure(
    list(
      status = data.frame(src_name = "mimic_iv_demo", stage = "run_iss",
                          ok = FALSE, reason = "not done",
                          stringsAsFactors = FALSE),
      recommended_code = "run_one_source(...)",
      merge_status = NULL,
      db_name = "rendehr", base_dir = ".", src_names = "mimic_iv_demo",
      origins = list(src_names = "argument", db_name = "default",
                    base_dir = "default")
    ),
    class = "ehr_suggestion"
  )

  expect_output(print(x), "mimic_iv_demo")
  expect_output(print(x), "run_iss")
  expect_output(print(x), "run_one_source")
})

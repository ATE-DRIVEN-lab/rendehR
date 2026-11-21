test_that("ehr_setup_assistant stage script includes clinical score stages by default", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_true(plan$choices$run_comorbidities)
  expect_true(plan$choices$run_iss)
  expect_match(plan$script, "import_cohort_general\\(")
  expect_false(grepl("import_cohort\\(", plan$script))
  expect_false(grepl("import_cohort_flex\\(", plan$script))
  expect_match(plan$script, "run_comorbidities\\(")
  expect_match(plan$script, "run_iss\\(")
})

test_that("ehr_setup_assistant no-cohort path uses canonical no-filter mode", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    cohort = "none",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )
  expect_match(plan$script, "import_cohort_general\\(")
  expect_match(plan$script, "no_filter = TRUE")
  expect_false(grepl("import_cohort_flex\\(", plan$script))
})

test_that("ehr_setup_assistant can omit clinical score stages in stage script", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    run_comorbidities = FALSE,
    run_iss = FALSE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_false(plan$choices$run_comorbidities)
  expect_false(plan$choices$run_iss)
  expect_false(grepl("run_comorbidities\\(", plan$script))
  expect_false(grepl("run_iss\\(", plan$script))
})

test_that("ehr_setup_assistant full script forwards clinical score choices", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "full",
    config_setup = "skip",
    include_download = FALSE,
    run_comorbidities = FALSE,
    run_iss = TRUE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_match(plan$script, "run_one_source\\(")
  expect_match(plan$script, "run_comorbidities = FALSE")
  expect_match(plan$script, "run_iss = TRUE")
})

test_that("ehr_setup_assistant can save generated script when requested", {
  tmp <- file.path(tempdir(), paste0("ehr_setup_assistant_", Sys.getpid()))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  script_path <- file.path(tmp, "setup.R")

  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    inspect_options = "none",
    load_method = "none",
    save_script = TRUE,
    setup_script_path = script_path,
    interactive = FALSE
  )

  expect_true(file.exists(script_path))
  expect_equal(plan$saved_script_path, script_path)
  expect_equal(readLines(script_path, warn = FALSE), strsplit(plan$script, "\n", fixed = TRUE)[[1]])
  expect_match(plan$next_steps[[1]], "Generated setup script was saved")
})

test_that("ehr_setup_assistant does not save generated script by default", {
  tmp <- file.path(tempdir(), paste0("ehr_setup_assistant_no_save_", Sys.getpid()))
  script_path <- file.path(tmp, "setup.R")

  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    inspect_options = "none",
    load_method = "none",
    setup_script_path = script_path,
    interactive = FALSE
  )

  expect_false(file.exists(script_path))
  expect_null(plan$saved_script_path)
})

# ---------------------------------------------------------------------------
# Generated-script branches not exercised by the six tests above
# (merge, ICU timeline, custom ICD filter, multi-source).
# ---------------------------------------------------------------------------

test_that("ehr_setup_assistant includes a merge block and ICU-timeline block when requested", {
  plan <- ehr_setup_assistant(
    src_name = c("mimic_iv_demo", "eicu_demo"),
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    analysis_level = "icu",
    merge = "icu",
    target_schema = "rendehr_icu_test",
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_match(plan$script, "make_icu_timeline\\(")
  expect_match(plan$script, "merge_sources\\(")
  expect_match(plan$script, "rendehr_icu_test")
  expect_match(plan$script, "for \\(src in src_names\\)")
  # an ICU-level merge needs cohort_info resolved per ICU stay
  expect_match(plan$script, "build_cohort_info_general\\([^)]*analysis_level = \"icu\"")
})

test_that("a full-pipeline ICU plan resolves cohort membership per ICU stay", {
  plan <- function(...) ehr_setup_assistant(src_name = "mimic_iv_demo", cohort = "sepsis3",
    pipeline = "full", config_setup = "skip", include_download = FALSE, load_method = "none",
    inspect_options = "none", interactive = FALSE, ...)
  icu <- plan(analysis_level = "icu", merge = "icu")
  expect_match(icu$script, "enrichment = enrichment_options\\(analysis_level = 'icu'\\)")
  expect_no_error(parse(text = icu$script))
  adm <- plan(merge = "admission")
  expect_no_match(adm$script, "enrichment_options")
})

test_that("ehr_setup_assistant's custom-cohort block validates and includes the ICD filter path", {
  tmp <- file.path(tempdir(), paste0("ehr_setup_assistant_icd_", Sys.getpid()))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  icd_path <- file.path(tmp, "my_icd_filter.csv")
  write.csv(data.frame(icd_code = "S12.0", icd_version = 10), icd_path,
            row.names = FALSE)

  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    cohort = "custom",
    icd_filter_path = icd_path,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_match(plan$script, "icd_filter_path <-", fixed = TRUE)
  expect_match(plan$script, basename(icd_path), fixed = TRUE)
  expect_match(plan$script, "build_cohort_info_general\\(")
})

test_that("ehr_setup_assistant loops over multiple selected sources in the generated script", {
  plan <- ehr_setup_assistant(
    src_name = c("mimic_iv_demo", "eicu_demo"),
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_equal(plan$choices$src_names, c("mimic_iv_demo", "eicu_demo"))
  expect_match(plan$script, "for \\(src in src_names\\)")
  expect_match(plan$script, 'src_names <- c\\("mimic_iv_demo", "eicu_demo"\\)')
})

test_that("ehr_setup_assistant includes a disk-space check for full-scale sources", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    include_disk_check = TRUE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_match(plan$script, "free-space check")
})

# ---------------------------------------------------------------------------
# Validation-bearing console helpers with real decision logic.
# ---------------------------------------------------------------------------

test_that(".setup_base_dir_problem: flags every documented invalid case and accepts a valid path", {
  expect_match(rendehR:::.setup_base_dir_problem(""), "non-empty")
  expect_match(rendehR:::.setup_base_dir_problem(c("a", "b")), "non-empty")
  expect_match(rendehR:::.setup_base_dir_problem("line1\nline2"), "line breaks")

  tmp <- tempfile("s010_base_dir_")
  dir.create(tmp)
  existing_file <- file.path(tmp, "not_a_dir.txt")
  writeLines("x", existing_file)
  expect_match(rendehR:::.setup_base_dir_problem(existing_file), "not a directory")

  nonexistent_parent <- file.path(tmp, "no_such_parent", "child")
  expect_null(rendehR:::.setup_base_dir_problem(nonexistent_parent))

  expect_null(rendehR:::.setup_base_dir_problem(tmp))
  expect_null(rendehR:::.setup_base_dir_problem(file.path(tmp, "new_child")))
})

test_that(".setup_base_dir_value: stops non-interactively on an invalid explicit value", {
  bad <- tempfile("s010_file_parent_")
  writeLines("file", bad)
  bad <- file.path(bad, "child")
  expect_error(
    rendehR:::.setup_base_dir_value(bad, "prompt", ".", interactive = FALSE),
    "not a directory"
  )
})

test_that(".setup_base_dir_value: returns a valid explicit path unchanged", {
  tmp <- tempfile("s010_base_dir_value_")
  dir.create(tmp)
  expect_equal(rendehR:::.setup_base_dir_value(tmp, "prompt", ".", interactive = FALSE), tmp)
})

test_that(".setup_check_icd_filter: warns on a non-csv/rds extension", {
  expect_warning(
    rendehR:::.setup_check_icd_filter("icd_filter.txt", interactive = FALSE),
    "should be a .csv or .rds file"
  )
})

test_that(".setup_check_icd_filter: warns when the file does not exist yet", {
  expect_warning(
    rendehR:::.setup_check_icd_filter(file.path(tempdir(), "does_not_exist_yet.csv"),
                                  interactive = FALSE),
    "does not exist yet"
  )
})

test_that(".setup_check_icd_filter: warns when required columns are missing", {
  tmp <- tempfile("s010_icd_bad_cols_", fileext = ".csv")
  write.csv(data.frame(wrong_col = 1), tmp, row.names = FALSE)
  expect_warning(
    rendehR:::.setup_check_icd_filter(tmp, interactive = FALSE),
    "missing expected column"
  )
})

test_that(".setup_check_icd_filter: silent (no warning, no message) on a valid non-interactive file", {
  tmp <- tempfile("s010_icd_ok_", fileext = ".csv")
  write.csv(data.frame(icd_code = "S12.0", icd_version = 10), tmp, row.names = FALSE)
  expect_no_warning(rendehR:::.setup_check_icd_filter(tmp, interactive = FALSE))
})

test_that(".setup_check_icd_filter: NULL/empty path is a silent no-op", {
  expect_no_warning(rendehR:::.setup_check_icd_filter(NULL, interactive = FALSE))
  expect_no_warning(rendehR:::.setup_check_icd_filter("", interactive = FALSE))
})

# ---------------------------------------------------------------------------
# print.ehr_setup_plan(): direct output test.
# ---------------------------------------------------------------------------

test_that("print.ehr_setup_plan renders the chosen sources and the generated script", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )

  expect_output(print(plan), "rendehR Setup Assistant Plan")
  expect_output(print(plan), "mimic_iv_demo")
  expect_output(print(plan), "run_comorbidities\\(")
})

# ---------------------------------------------------------------------------
# engine = "duckdb": cohort loading only, no PostgreSQL calls generated.
# ---------------------------------------------------------------------------

test_that("ehr_setup_assistant defaults to engine = 'postgres'", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    inspect_options = "none",
    load_method = "none",
    interactive = FALSE
  )
  expect_identical(plan$choices$engine, "postgres")
  expect_match(plan$script, "run_db\\(")
  expect_match(plan$script, "db_name = db_name")
  expect_false(grepl("engine = \"duckdb\"", plan$script, fixed = TRUE))
})

test_that("ehr_setup_assistant engine = 'duckdb' generates only the DuckDB-covered stages", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    engine = "duckdb",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    load_method = "none",
    interactive = FALSE
  )

  expect_identical(plan$choices$engine, "duckdb")
  expect_match(plan$script, "run_db\\(", fixed = FALSE)
  expect_match(plan$script, "engine = \"duckdb\"", fixed = TRUE)
  expect_match(plan$script, "run_raw_mods_duckdb\\(")
  expect_match(plan$script, "build_cohort_info_general_duckdb\\(")

  # None of the PostgreSQL-only stages should be *called* (the closing note
  # mentions some of these function names in prose, without parens+newline,
  # so match on the actual .setup_call() invocation form: "fn(\n").
  expect_false(grepl("run_raw_mods\\(", plan$script))
  expect_false(grepl("build_window_tables\\(\\n", plan$script))
  expect_false(grepl("build_cohort_annotations\\(\\n", plan$script))
  expect_false(grepl("run_concepts\\(\\n", plan$script))
  expect_false(grepl("run_harmonized_tables\\(\\n", plan$script))
  expect_false(grepl("run_comorbidities\\(\\n", plan$script))
  expect_false(grepl("run_iss\\(\\n", plan$script))
  expect_false(grepl("merge_sources\\(\\n", plan$script))

  # Forced-off choices, since all of the above require PostgreSQL.
  expect_false(plan$choices$run_comorbidities)
  expect_false(plan$choices$run_iss)
  expect_identical(plan$choices$merge, "none")
  expect_identical(plan$choices$inspect_options, "none")
  expect_identical(plan$choices$analysis_level, "admission")
})

test_that("ehr_setup_assistant engine = 'duckdb' forces pipeline to 'stages'", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    engine = "duckdb",
    pipeline = "full",
    config_setup = "skip",
    include_download = FALSE,
    load_method = "none",
    interactive = FALSE
  )
  expect_identical(plan$choices$pipeline, "stages")
  expect_false(grepl("run_one_source\\(", plan$script))
})

test_that("ehr_setup_assistant engine = 'duckdb' with cohort = 'none' builds generic cohort_info", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    engine = "duckdb",
    cohort = "none",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    load_method = "none",
    interactive = FALSE
  )
  expect_match(plan$script, "no_filter = TRUE")
  expect_true(grepl("build_cohort_info_general_duckdb\\(", plan$script))
})

test_that("print.ehr_setup_plan renders the Engine line for engine = 'duckdb'", {
  plan <- ehr_setup_assistant(
    src_name = "mimic_iv_demo",
    engine = "duckdb",
    pipeline = "stages",
    config_setup = "skip",
    include_download = FALSE,
    load_method = "none",
    interactive = FALSE
  )
  expect_output(print(plan), "Engine   : duckdb")
})


setup_review_plan <- function(...) {
  ehr_setup_assistant(..., interactive = FALSE, load_method = "none",
    inspect_options = "none", include_download = FALSE)
}

test_that("new nested project paths are accepted without creating files", {
  root <- file.path(tempfile("assistant-new-"), "project")
  plan <- setup_review_plan(base_dir = root)
  expect_false(dir.exists(root))
  expect_identical(plan$choices$data_dir, file.path(root, "data"))
  expect_no_error(parse(text = plan$script))
})

test_that("scope respects cohort selection grain across sources", {
  expect_error(setup_review_plan(src_name = "mimic_iv_demo", import_scope = "icu"),
    "not available here")
  expect_identical(setup_review_plan(src_name = "eicu_demo", import_scope = "icu")$choices$import_scope, "icu")
  expect_error(setup_review_plan(src_name = c("eicu_demo", "mimic_iii_demo"), import_scope = "icu"), "not available here")
  expect_identical(setup_review_plan(cohort = "none")$choices$import_scope, "patient")
  expect_error(setup_review_plan(cohort = "none", import_scope = "hospital"), "not available here")
  expect_identical(setup_review_plan(cohort = "scid")$choices$import_scope, "patient")
  expect_identical(setup_review_plan(cohort = "sepsis3")$choices$import_scope, "hospital")
})

test_that("all sepsis variants and valid scopes produce parseable scripts", {
  for (src in c("mimic_iii_demo", "mimic_iv_demo", "eicu_demo")) {
    for (variant in c("sepsis3", "first_sepsis3", "sepsis3_admission", "first_sepsis3_admission")) {
      for (scope in c("icu", "hospital", "patient")) {
        for (style in c("stages", "full")) {
          plan <- setup_review_plan(src_name = src, cohort = "sepsis3",
            sepsis_bundle = variant, import_scope = scope, pipeline = style,
            memory_limit = "4GB", threads = 2)
          expect_no_error(parse(text = plan$script))
          expect_match(plan$script, paste0('bundle = "', variant, '"'), fixed = TRUE)
          expect_match(plan$script, "build_sepsis_cohort(", fixed = TRUE)
          expect_match(plan$script, "import_scope = import_scope", fixed = TRUE)
          expect_match(plan$script, 'cohort_profile = "generic"', fixed = TRUE)
          expect_false(plan$choices$run_iss)
          expect_false(plan$choices$run_comorbidities)
          expect_false(grepl("\nbuild_cohort_annotations(", plan$script, fixed = TRUE))
        }
      }
    }
  }
})

test_that("ICU merging prepares projections while preserving admission analysis", {
  plan <- setup_review_plan(analysis_level = "admission", merge = "icu", window_before_hours = 6)
  expect_identical(plan$choices$analysis_level, "admission")
  expect_true(plan$choices$make_icu_timeline)
  script <- plan$script
  expect_lt(regexpr("\nmake_icu_timeline", script)[[1]], regexpr("\nmerge_sources", script)[[1]])
  expect_match(script, "window_before_hours = 6", fixed = TRUE)
  expect_error(setup_review_plan(merge = "icu", window_before_hours = -1), "non-negative")
})

test_that("current cohort builders and enrichment defaults agree across styles", {
  filter <- tempfile(fileext = ".csv")
  write.csv(data.frame(icd_code = "A", icd_version = 10), filter, row.names = FALSE)
  for (cohort in c("scid", "custom", "none")) {
    for (style in c("stages", "full")) {
      plan <- setup_review_plan(cohort = cohort, pipeline = style,
        icd_filter_path = if (cohort == "custom") filter else NULL)
      expect_identical(plan$choices$run_iss, cohort == "scid")
      expect_identical(plan$choices$run_comorbidities, cohort == "scid")
      expect_no_error(parse(text = plan$script))
      if (style == "full") expect_match(plan$script, "run_cohort_annotations = FALSE", fixed = TRUE)
      else expect_match(plan$script, "build_cohort_info_general(", fixed = TRUE)
    }
  }
})

# Execute generated build blocks with recording doubles: no DB or downloads.
setup_record_build <- function(plan) {
  env <- new.env(parent = baseenv())
  env$src_names <- plan$choices$src_names
  env$default_src <- env$src_names[[1]]
  env$db_name <- "test"
  env$base_dir <- tempdir()
  env$raw_data_root <- tempdir()
  env$duckdb_root <- tempdir()
  env$import_scope <- plan$choices$import_scope
  env$icd_filter_path <- "filter.csv"
  calls <- list()
  stages <- c("build_sepsis_cohort", "import_cohort_general", "run_one_source",
    "run_db", "run_raw_mods", "run_raw_mods_duckdb", "build_cohort_info_general",
    "build_cohort_info_general_duckdb", "run_comorbidities", "run_iss",
    "run_concepts", "run_harmonized_tables", "make_icu_timeline")
  for (name in stages) env[[name]] <- local({
    stage <- name
    function(...) { calls[[length(calls) + 1L]] <<- list(stage = stage, args = list(...)); invisible(NULL) }
  })
  block <- substring(plan$script, regexpr("# Step 3 -", plan$script)[[1]])
  eval(parse(text = block), env)
  calls
}

test_that("full sepsis scripts import exactly once per source and skip re-import", {
  for (style in c("stages", "full")) {
    plan <- setup_review_plan(src_name = c("mimic_iv_demo", "eicu_demo"),
      cohort = "sepsis3", pipeline = style, import_scope = "icu")
    calls <- setup_record_build(plan)
    selection <- Filter(function(x) x$stage == "build_sepsis_cohort", calls)
    expect_length(selection, 2)
    expect_identical(vapply(selection, function(x) x$args$src_name, ""), plan$choices$src_names)
    expect_true(all(vapply(selection, function(x) x$args$import_scope == "icu", TRUE)))
    expect_false(any(vapply(calls, function(x) x$stage == "import_cohort_general", TRUE)))
    if (style == "full") {
      runs <- Filter(function(x) x$stage == "run_one_source", calls)
      expect_length(runs, 2)
      expect_false(any(vapply(runs, function(x) "import_cohort" %in% x$args$stages, TRUE)))
      expect_true(all(vapply(runs, function(x) "build_cohort_info" %in% x$args$stages, TRUE)))
    }
  }
})

test_that("DuckDB sepsis scripts retain selection and generic information only", {
  calls <- setup_record_build(setup_review_plan(cohort = "sepsis3", engine = "duckdb"))
  expect_identical(vapply(calls, `[[`, "", "stage"),
    c("build_sepsis_cohort", "run_db", "run_raw_mods_duckdb", "build_cohort_info_general_duckdb"))
  expect_identical(calls[[2]]$args$engine, "duckdb")
})

test_that("registered families are offered with explicit capability boundaries", {
  registry_env <- rendehR:::.ehr_env
  registry <- registry_env$source_registry
  on.exit({ registry_env$source_registry <- registry }, add = TRUE)
  rendehR:::.register_source_family("assistant_test", select_id_cols = c("patient", "visit"))
  plan <- setup_review_plan(src_name = "assistant_test_demo", cohort = "none", engine = "duckdb")
  expect_identical(plan$choices$src_name, "assistant_test_demo")
  expect_error(setup_review_plan(src_name = "assistant_test", cohort = "sepsis3"), "Registered families")
  expect_error(setup_review_plan(src_name = "assistant_test", cohort = "none"), "Registered families")
  calls <- setup_record_build(plan)
  expect_identical(vapply(calls, `[[`, "", "stage"), c("import_cohort_general", "run_db"))
})

test_that("invalid sepsis/resource selections fail during planning", {
  expect_error(setup_review_plan(sepsis_bundle = "sepsis3"), "requires")
  expect_error(setup_review_plan(cohort = "sepsis3", sepsis_bundle = "unknown"), "not available here")
  expect_error(setup_review_plan(cohort = "sepsis3", threads = 1.5), "positive integer")
  expect_error(setup_review_plan(cohort = "sepsis3", memory_limit = "oops"), "size")
})

setup_config_block <- function(plan, env) {
  end <- regexpr("# Step 2 -", plan$script)[[1]]
  eval(parse(text = substr(plan$script, 1L, end - 1L)), env)
}

test_that("generated configuration preserves existing files and rejects conflicts", {
  root <- tempfile("assistant-config-")
  plan <- setup_review_plan(base_dir = root, cohort = "sepsis3", config_setup = "interactive")
  env <- new.env(parent = environment(ehr_setup_assistant))
  # Configuration I/O is real and confined to tempdir; no database checks.
  env$ehr_check_config <- function(...) list(ok = TRUE)
  env$ehr_use_config <- function(path) invisible(ehr_load_config(path))
  expect_no_error(setup_config_block(plan, env))
  path <- file.path(root, "config", "ehr_config.json")
  creds <- file.path(root, "config", "config_pass.json")
  original <- readLines(path)
  writeLines('{"existing_credentials":"preserve"}', creds)
  expect_no_error(setup_config_block(plan, env))
  expect_identical(readLines(path), original)
  expect_identical(readLines(creds), '{"existing_credentials":"preserve"}')
  cfg <- jsonlite::read_json(path, simplifyVector = TRUE)
  cfg$cohort$profile <- "scid"
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, null = "null")
  conflicting <- readLines(path)
  expect_error(setup_config_block(plan, env), "cohort profile conflicts")
  expect_identical(readLines(path), conflicting)
  cfg$cohort$profile <- "generic"
  cfg$paths$raw_data_root <- file.path(root, "wrong-root")
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, null = "null")
  expect_error(setup_config_block(plan, env), "storage path conflicts")
})

test_that("custom filter and storage root are validated consistently in config", {
  root <- tempfile("assistant-custom-config-")
  filter <- tempfile(fileext = ".csv")
  write.csv(data.frame(icd_code = "A", icd_version = 10), filter, row.names = FALSE)
  plan <- setup_review_plan(base_dir = root, raw_data_dir = file.path(root, "raw-root"),
    cohort = "custom", icd_filter_path = filter)
  env <- new.env(parent = environment(ehr_setup_assistant))
  env$ehr_check_config <- function(...) list(ok = TRUE)
  env$ehr_use_config <- function(path) invisible(ehr_load_config(path))
  expect_no_error(setup_config_block(plan, env))
  expect_identical(env$raw_data_root, file.path(root, "raw-root"))
  path <- file.path(root, "config", "ehr_config.json")
  cfg <- jsonlite::read_json(path, simplifyVector = TRUE)
  cfg$cohort$icd_filter_path <- "another.csv"
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, null = "null")
  expect_error(setup_config_block(plan, env), "ICD filter conflicts")
})

test_that("inspection table listing handles empty schemas", {
  plan <- ehr_setup_assistant(interactive = FALSE, load_method = "none",
    inspect_options = "tables")
  env <- new.env(parent = baseenv())
  env$default_src <- "mimic_iv_demo"
  env$list_sources <- function() "mimic_iv_demo"
  env$list_tables <- function(...) character()
  start <- regexpr("# Step 7 -", plan$script)[[1]]
  expect_no_error(eval(parse(text = substring(plan$script, start)), env))
  expect_true(all(vapply(env$tables_by_schema, nrow, integer(1)) == 0L))
})

test_that("an unavailable assistant choice names the question and the valid values", {
  expect_error(
    ehr_setup_assistant(src_name = "mimic_iv_demo", cohort = "scid", import_scope = "icu",
                        include_download = FALSE, load_method = "none",
                        inspect_options = "none", interactive = FALSE),
    "Import scope: 'icu' is not available here. Valid choices are: hospital, patient.", fixed = TRUE)
})

test_that("cohort_key returns correct column per source family", {
  expect_equal(cohort_key("mimic_iv"),      "subject_id")
  expect_equal(cohort_key("mimic_iv_demo"), "subject_id")
  expect_equal(cohort_key("mimic_iii"),     "subject_id")
  expect_equal(cohort_key("mimic_iii_demo"),"subject_id")
  expect_equal(cohort_key("eicu"),          "patientunitstayid")
  expect_equal(cohort_key("eicu_demo"),     "patientunitstayid")
})

test_that("cohort_key works for materialized subcohort names", {
  expect_equal(cohort_key("mimic_iv_demo_cervical"), "subject_id")
  expect_equal(cohort_key("eicu_demo_ais_a"),        "patientunitstayid")
})

test_that("cohort_key errors on unknown family", {
  expect_error(cohort_key("unknown_source"), "Unknown")
})

test_that("src_schema_names returns all expected schema keys", {
  sn <- src_schema_names("mimic_iv_demo")
  expect_named(sn, c("raw", "concepts", "derived", "harmonized"),
               ignore.order = TRUE)
  expect_equal(sn$raw,      "mimic_iv_demo_coh")
  expect_equal(sn$concepts, "mimic_iv_demo_coh_concepts")
  expect_equal(sn$harmonized, "mimic_iv_demo_coh_harmonized")
  expect_equal(sn$derived,  "mimic_iv_demo_coh_derived")
})

test_that("src_schema_names works for all three families", {
  expect_equal(src_schema_names("eicu")$raw,      "eicu_coh")
  expect_equal(src_schema_names("mimic_iii")$raw, "mimic_iii_coh")
})

test_that("ehr_paths returns expected path keys", {
  p <- ehr_paths(src_name = "mimic_iv_demo", base_dir = "/tmp")
  required_keys <- c("base_dir", "raw_dir", "cohort_parquet_dir",
                      "duckdb_dir", "duckdb_path", "sql_dir", "config_dir", "credentials_json",
                      "concept_config_csv_ts", "concept_config_csv_dur")
  for (k in required_keys)
    expect_true(k %in% names(p), info = paste("Missing key:", k))
})

test_that("ehr_paths embeds src_name in directory paths", {
  p <- ehr_paths(src_name = "eicu_demo", base_dir = "/data")
  expect_true(grepl("eicu_demo", p$raw_dir))
  expect_true(grepl("eicu_demo", p$cohort_parquet_dir))
  expect_equal(p$duckdb_path, file.path("/data", "data", "duckdb", "eicu_demo.duckdb"))
})

test_that("ehr_paths resolves source from argument, config, then default", {
  old_active_config <- .ehr_env$config
  old_config <- Sys.getenv("EHR_CONFIG", unset = NA_character_)
  on.exit({
    .ehr_env$config <- old_active_config
    if (is.na(old_config)) Sys.unsetenv("EHR_CONFIG") else Sys.setenv(EHR_CONFIG = old_config)
  }, add = TRUE)

  Sys.setenv(EHR_CONFIG = file.path(tempdir(), "ehr_missing_config.json"))
  .ehr_env$config <- NULL
  expect_equal(ehr_paths()$src_name, "mimic_iv_demo")

  cfg <- list(
    source = list(src_name = "eicu_demo"),
    project = list(base_dir = tempdir())
  )
  .ehr_env$config <- cfg
  expect_equal(ehr_paths()$src_name, "eicu_demo")
  expect_equal(ehr_paths("mimic_iii_demo")$src_name, "mimic_iii_demo")
})

test_that("ehr_paths auto-loads an existing config file", {
  old_active_config <- .ehr_env$config
  old_config <- Sys.getenv("EHR_CONFIG", unset = NA_character_)
  tmp <- file.path(tempdir(), paste0("ehr_paths_config_", Sys.getpid()))
  cfg_path <- file.path(tmp, "config", "ehr_config.json")
  on.exit({
    .ehr_env$config <- old_active_config
    if (is.na(old_config)) Sys.unsetenv("EHR_CONFIG") else Sys.setenv(EHR_CONFIG = old_config)
  }, add = TRUE)

  ehr_create_config_template(path = cfg_path,
                             src_name = "eicu_demo",
                             base_dir = tmp,
                             raw_data_root = "raw_files",
                             duckdb_root = "duckdb_files",
                             overwrite = TRUE)
  .ehr_env$config <- NULL
  Sys.setenv(EHR_CONFIG = cfg_path)

  p <- ehr_paths()

  expect_equal(p$src_name, "eicu_demo")
  expect_equal(p$raw_dir, file.path(tmp, "raw_files", "eicu_demo"))
  expect_equal(p$duckdb_path, file.path(tmp, "duckdb_files", "eicu_demo.duckdb"))
})

test_that("ehr_paths uses deterministic parent raw root", {
  tmp <- file.path(tempdir(), paste0("ehr_paths_raw_", Sys.getpid()))
  parent <- file.path(tmp, "physionet")
  source_dir <- file.path(parent, "mimic_iv_demo")
  dir.create(source_dir, recursive = TRUE)

  parent_resolved <- ehr_paths(src_name = "mimic_iv_demo",
                               base_dir = tmp,
                               raw_data_root = parent)

  expect_equal(parent_resolved$raw_dir, source_dir)
  expect_error(
    ehr_paths(src_name = "mimic_iv_demo", base_dir = tmp,
              raw_data_root = source_dir),
    "must be the parent"
  )
  expect_error(
    ehr_paths(src_name = "mimic_iv_demo", base_dir = tmp,
              raw_data_dir = source_dir),
    "raw_data_dir.*retired"
  )
})

test_that("run_one_source uses the raw-root and immutable-input contract", {
  runner_args <- names(formals(run_one_source))

  expect_true("raw_data_root" %in% runner_args)
  expect_false("raw_data_dir" %in% runner_args)
  expect_error(
    run_one_source(delete_source_csv = TRUE),
    "delete_source_csv = TRUE.*retired"
  )
})

test_that("run_one_source gates downstream work on required raw mods", {
  body_text <- paste(deparse(body(run_one_source)), collapse = "\n")
  expect_match(
    body_text,
    "Required raw modifications are incomplete; downstream stages refused.",
    fixed = TRUE
  )
  expect_match(
    body_text,
    'identical(stage, "run_raw_mods") && !isTRUE(stage_ok)',
    fixed = TRUE
  )
})

test_that("direct run_one_source callers use the parent raw-root contract", {
  for (fn in list(run_one_source_test, run_scid_example, ehr_suggest_next)) {
    fn_args <- names(formals(fn))
    expect_true("raw_data_root" %in% fn_args)
    expect_false("raw_data_dir" %in% fn_args)
  }

  caller_code <- paste(
    deparse(body(run_one_source_test)),
    deparse(body(run_scid_example)),
    deparse(body(ehr_suggest_next)),
    collapse = "\n"
  )
  expect_match(caller_code, "raw_data_root")
  expect_false(grepl("raw_data_dir", caller_code, fixed = TRUE))
})

test_that("download destination accepts only a parent root", {
  tmp <- file.path(tempdir(), paste0("ehr_download_dest_", Sys.getpid()))
  root <- file.path(tmp, "physionet")
  source_dir <- file.path(root, "eicu_demo")

  expect_equal(.ehr_download_dest_dir(root, "eicu_demo", tmp), source_dir)
  expect_error(
    .ehr_download_dest_dir(source_dir, "eicu_demo", tmp),
    "must be the parent"
  )
})


test_that("ehr_paths only applies SCI-only ICD defaults to SCI profile; general dictionaries stay available for any profile", {
  cfg <- list(
    source = list(src_name = "mimic_iv_demo"),
    database = list(db_name = "rendehr"),
    project = list(base_dir = tempdir()),
    cohort = list(profile = "custom")
  )

  p <- ehr_paths(config = cfg)

  expect_equal(p$cohort_profile, "custom")
  # SCI-only assets (live under cohort.profiles.scid.* in config) stay NULL
  # for a non-scid profile.
  expect_null(p$icd_filter_path)
  expect_null(p$icds_cause_path)
  expect_null(p$icds_proc_path)
  # icds_proc_dict_path is a general ICD-procedure dictionary (same category
  # as icds_dict_path, not SCI-specific), so it resolves to the packaged
  # default for any cohort profile, not just "scid".
  expect_equal(basename(p$icds_proc_dict_path), "d_icd_procedures.csv")
})

test_that("ehr_init_project creates but does not overwrite config", {
  old_config <- Sys.getenv("EHR_CONFIG", unset = NA_character_)
  old_active_config <- .ehr_env$config
  tmp <- tempfile("ehr_init_project_")
  cfg_path <- file.path(tmp, "config", "ehr_config.json")

  .ehr_env$config <- NULL
  Sys.setenv(EHR_CONFIG = cfg_path)
  on.exit({
    .ehr_env$config <- old_active_config
    if (is.na(old_config)) Sys.unsetenv("EHR_CONFIG") else Sys.setenv(EHR_CONFIG = old_config)
  }, add = TRUE)

  p1 <- ehr_init_project(base_dir = tmp)
  creds_path <- file.path(tmp, "config", "config_pass.json")
  expect_true(file.exists(cfg_path))
  expect_true(file.exists(creds_path))
  expect_equal(p1$src_name, "mimic_iv_demo")

  cfg <- ehr_load_config(cfg_path)
  cfg$source$src_name <- "eicu_demo"
  cfg$database$db_name <- "custom_db"
  ehr_write_config(cfg, path = cfg_path, overwrite = TRUE)
  ehr_create_credentials_template(path = creds_path,
                                  postgres_password = "keep_me",
                                  overwrite = TRUE)

  p2 <- ehr_init_project()
  cfg2 <- ehr_load_config(cfg_path)
  creds2 <- jsonlite::read_json(creds_path, simplifyVector = TRUE)

  expect_equal(p2$src_name, "eicu_demo")
  expect_equal(cfg2$database$db_name, "custom_db")
  expect_equal(creds2$postgres$password, "keep_me")
})


test_that("pipeline stage resolver keeps default order", {
  defaults <- .ehr_default_pipeline_stages(
    run_cohort_annotations = TRUE,
    run_comorbidities = TRUE,
    run_iss = TRUE,
    run_external = FALSE
  )

  expect_equal(
    .ehr_resolve_pipeline_stages(default_stages = defaults),
    c("import_cohort", "run_db", "run_raw_mods", "build_window_tables",
      "build_cohort_annotations", "run_comorbidities", "run_iss",
      "run_concepts", "run_harmonized_tables")
  )
})

test_that("pipeline stage resolver supports explicit stages in pipeline order", {
  expect_equal(
    .ehr_resolve_pipeline_stages(stages = c("run_harmonized_tables", "run_iss")),
    c("run_iss", "run_harmonized_tables")
  )
})

test_that("pipeline stage resolver rejects invalid stage selections", {
  expect_error(.ehr_resolve_pipeline_stages(stages = "not_a_stage"),
               "Unknown pipeline stage")
  expect_error(.ehr_resolve_pipeline_stages(stages = character()),
               "stages must be")
})

test_that("ehr_suggest_next code builders format runnable commands", {
  expect_match(
    .ehr_run_one_source_code("mimic_iv_demo", "rendehr", ".",
                             c("run_iss", "run_concepts")),
    "run_one_source(", fixed = TRUE
  )
  expect_match(
    .ehr_run_one_source_code("mimic_iv_demo", "rendehr", ".",
                             c("run_iss", "run_concepts")),
    'stages = c("run_iss", "run_concepts")', fixed = TRUE
  )
  compact <- .ehr_run_one_source_code(
    "mimic_iv_demo", "rendehr", ".", "run_concepts",
    include_src = FALSE, include_db = FALSE, include_base = FALSE
  )
  expect_match(compact, "run_one_source(", fixed = TRUE)
  expect_false(grepl("src_name", compact, fixed = TRUE))
  expect_false(grepl("db_name", compact, fixed = TRUE))
  expect_false(grepl("base_dir", compact, fixed = TRUE))
})

test_that("run_sources validates src_names", {
  expect_error(run_sources(character()), "src_names must contain")
})

# ---------------------------------------------------------------------------
# run_concepts()'s structured completion contract. These tests
# target only .ehr_check_concept_layer_results(), the aggregation/raise
# logic extracted from run_concepts() specifically so it is testable
# without a live database connection; they do not exercise run_concepts()'s
# database-opening preamble or any individual layer's SQL execution.
# ---------------------------------------------------------------------------

test_that(".ehr_check_concept_layer_results: silent when every layer fully succeeded", {
  layer_results <- list(
    flat = c(a = TRUE, b = TRUE),
    ts   = c(heart_rate = TRUE)
  )
  expect_no_error(.ehr_check_concept_layer_results(layer_results, "mimic_iv_demo"))
})

test_that(".ehr_check_concept_layer_results: silent when there are no layers at all", {
  expect_no_error(.ehr_check_concept_layer_results(list(), "mimic_iv_demo"))
})

test_that(".ehr_check_concept_layer_results: raises naming the failed item and its layer", {
  layer_results <- list(
    flat = c(a = TRUE, b = TRUE),
    ts   = c(heart_rate = TRUE, broken_concept = FALSE)
  )
  expect_error(
    .ehr_check_concept_layer_results(layer_results, "mimic_iv_demo"),
    "ts/broken_concept"
  )
})

test_that(".ehr_check_concept_layer_results: reports every failure across every layer, not just the first", {
  layer_results <- list(
    flat = c(a = TRUE, bad_flat = FALSE),
    ts   = c(bad_ts = FALSE, good_ts = TRUE)
  )
  err <- tryCatch(
    .ehr_check_concept_layer_results(layer_results, "mimic_iv_demo"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "flat/bad_flat")
  expect_match(err, "ts/bad_ts")
  expect_match(err, "2 concept\\(s\\)/file\\(s\\) failed")
})

test_that(".ehr_check_concept_layer_results: a single-item all-success layer does not falsely trip", {
  # Regression guard: if run_atomic_concepts_unified() reported the
  # *attempted* names regardless of success, naive aggregation would make
  # this look identical to a failure.
  layer_results <- list(ts = c(only_concept = TRUE))
  expect_no_error(.ehr_check_concept_layer_results(layer_results, "mimic_iv_demo"))
})

# Caller-level integration test: proves run_concepts() itself -- not just
# the extracted .ehr_check_concept_layer_results() helper -- actually wires
# each layer's returned result into the final check and propagates a
# failure. Every collaborator is mocked to keep this the smallest reliable
# surface: paths point Layers 4/6/7 at nonexistent directories so those
# blocks are skipped entirely by run_concepts()'s own existing
# dir.exists() guards (not by the mock), leaving only Layers 1-3 and the
# database-connection preamble to mock. This does not replace real-data
# validation of the full seven-layer orchestration against a live database
# (covered by the validation harness, validation/); it proves the
# structured-result plumbing is wired correctly.
test_that("run_concepts: a failing required layer is propagated as a raised error, not silent success", {
  fake_paths <- list(
    credentials_json            = "unused",
    user_concept_config_csv_ts  = NULL,
    user_concept_config_csv_dur = NULL,
    helper_sql_dir               = file.path(tempdir(), "s007_rc_no_helper"),
    pkg_flat_sql_dir             = file.path(tempdir(), "s007_rc_no_flat"),
    concept_config_csv_ts        = "unused_ts.csv",
    pkg_ts_sql_dir                = file.path(tempdir(), "s007_rc_sql_ts"),
    concept_config_csv_dur       = "unused_dur.csv",
    pkg_dur_sql_dir                = file.path(tempdir(), "s007_rc_sql_dur"),
    pkg_repo_sql_dir              = file.path(tempdir(), "s007_rc_no_repo"),
    pkg_ehr_sql_dir               = file.path(tempdir(), "s007_rc_no_ehr"),
    user_sql_dir                  = file.path(tempdir(), "s007_rc_no_user"),
    external_sql_dir              = file.path(tempdir(), "s007_rc_no_external")
  )
  fake_schema_names <- list(raw = "main", concepts = "main", derived = "main")

  local_mocked_bindings(
    ehr_paths                 = function(...) fake_paths,
    .ehr_resolve_db_name       = function(...) "fake_db",
    src_schema_names          = function(...) fake_schema_names,
    load_credentials          = function(...) list(),
    ensure_database_exists    = function(...) invisible(NULL),
    connect_postgres          = function(...) structure(list(), class = "fake_con"),
    create_schemas            = function(...) invisible(NULL),
    run_flat_sql_concepts     = function(...) c(good_flat = TRUE),
    .register_pkg_layer_provenance = function(...) invisible(NULL),
    auto_register_atomic_provenance = function(...) invisible(NULL)
  )
  local_mocked_bindings(
    dbExecute    = function(...) 0L,
    dbDisconnect = function(...) invisible(TRUE),
    .package     = "DBI"
  )
  # run_atomic_concepts_unified() is called twice (typ = "ts", then "dur");
  # succeed on ts, fail on dur, to prove a single failing layer among
  # several successful ones is still caught.
  local_mocked_bindings(
    run_atomic_concepts_unified = function(con, concept_config_path, src_name,
                                           schema_names, typ, sql_out_dir,
                                           user_concepts = NULL, concept_name = NULL,
                                           dry_run = FALSE,
                                           engine = c("postgres", "duckdb"), ...) {
      if (identical(typ, "ts")) c(good_ts = TRUE) else c(bad_dur = FALSE)
    }
  )

  expect_error(
    run_concepts("mimic_iv_demo"),
    "dur/bad_dur"
  )
})

test_that("run_concepts: full success across every attempted layer does not raise", {
  fake_paths <- list(
    credentials_json            = "unused",
    user_concept_config_csv_ts  = NULL,
    user_concept_config_csv_dur = NULL,
    helper_sql_dir               = file.path(tempdir(), "s007_rc2_no_helper"),
    pkg_flat_sql_dir             = file.path(tempdir(), "s007_rc2_no_flat"),
    concept_config_csv_ts        = "unused_ts.csv",
    pkg_ts_sql_dir                = file.path(tempdir(), "s007_rc2_sql_ts"),
    concept_config_csv_dur       = "unused_dur.csv",
    pkg_dur_sql_dir                = file.path(tempdir(), "s007_rc2_sql_dur"),
    pkg_repo_sql_dir              = file.path(tempdir(), "s007_rc2_no_repo"),
    pkg_ehr_sql_dir               = file.path(tempdir(), "s007_rc2_no_ehr"),
    user_sql_dir                  = file.path(tempdir(), "s007_rc2_no_user"),
    external_sql_dir              = file.path(tempdir(), "s007_rc2_no_external")
  )
  fake_schema_names <- list(raw = "main", concepts = "main", derived = "main")

  local_mocked_bindings(
    ehr_paths                 = function(...) fake_paths,
    .ehr_resolve_db_name       = function(...) "fake_db",
    src_schema_names          = function(...) fake_schema_names,
    load_credentials          = function(...) list(),
    ensure_database_exists    = function(...) invisible(NULL),
    connect_postgres          = function(...) structure(list(), class = "fake_con"),
    create_schemas            = function(...) invisible(NULL),
    run_flat_sql_concepts     = function(...) c(good_flat = TRUE),
    run_atomic_concepts_unified = function(con, concept_config_path, src_name,
                                           schema_names, typ, sql_out_dir,
                                           user_concepts = NULL, concept_name = NULL,
                                           dry_run = FALSE,
                                           engine = c("postgres", "duckdb"), ...) {
      setNames(TRUE, paste0("good_", typ))
    },
    .register_pkg_layer_provenance = function(...) invisible(NULL),
    auto_register_atomic_provenance = function(...) invisible(NULL)
  )
  local_mocked_bindings(
    dbExecute    = function(...) 0L,
    dbDisconnect = function(...) invisible(TRUE),
    .package     = "DBI"
  )

  result <- run_concepts("mimic_iv_demo")
  expect_true(result$flat[["good_flat"]])
  expect_true(result$ts[["good_ts"]])
  expect_true(result$dur[["good_dur"]])
})

# Manifest-backed valid-empty behavior and strict direct-ID handling are
# covered end to end in test-cohort-publication.R and
# test-cohort-selection.R.

# ---------------------------------------------------------------------------
# SCI enrichment wiring in the default cohort-info stage.
#
# The generalized cohort path made build_cohort_info the default stage, which
# left scid_classify() reachable only through the opt-in annotation stages. The
# result was a scid cohort_info with no injury columns at all. These tests pin
# the wiring so that regression cannot recur silently.
# ---------------------------------------------------------------------------

# Run build_cohort_info_general() with the database boundary mocked out, and
# return the classify_fn/classify_args the core builder actually received.
.capture_cohort_info_classify <- function(profile, ...) {
  fake_paths <- list(
    src_name        = "mimic_iv_demo",
    base_dir        = tempdir(),
    cohort_profile  = profile,
    credentials_json = "unused",
    icd_filter_path = if (identical(profile, "scid")) "scid_icds.csv" else NULL,
    icds_cause_path = if (identical(profile, "scid")) "injury_cause.csv" else NULL,
    icds_proc_path  = if (identical(profile, "scid")) "procedure_classification.csv" else NULL,
    icds_proc_dict_path   = "d_icd_procedures.csv",
    icds_dict_path        = "d_icd_diagnoses.csv",
    icd_adjudication_path = NULL
  )
  captured <- NULL
  local_mocked_bindings(
    ehr_paths            = function(...) fake_paths,
    .ehr_resolve_db_name = function(...) "fake_db",
    src_schema_names     = function(...) list(raw = "main"),
    load_credentials     = function(...) list(),
    connect_postgres     = function(...) structure(list(), class = "fake_con"),
    .ehr_sci_enrichment_applies = function(...) TRUE,  # tested on its own below
    .build_cohort_info_general_core = function(classify_fn = NULL,
                                               classify_args = list(), ...) {
      captured <<- list(fn = classify_fn, args = classify_args)
      invisible(NULL)
    }
  )
  local_mocked_bindings(dbDisconnect = function(...) invisible(TRUE), .package = "DBI")
  build_cohort_info_general(src_name = "mimic_iv_demo", ...)
  captured
}

test_that("build_cohort_info_general auto-wires scid_classify for the scid profile", {
  got <- .capture_cohort_info_classify("scid")

  expect_identical(got$fn, scid_classify)
  # All four SCI assets are supplied: classification, injury cause, and the
  # procedure classification plus its title dictionary.
  expect_equal(got$args$icds_scid_path, "scid_icds.csv")
  expect_equal(got$args$icds_cause_path, "injury_cause.csv")
  expect_equal(got$args$icds_proc_path, "procedure_classification.csv")
  expect_equal(got$args$icds_proc_dict_path, "d_icd_procedures.csv")
})

test_that("build_cohort_info_general leaves classify_fn NULL for a non-scid profile", {
  got <- .capture_cohort_info_classify("custom")

  expect_null(got$fn)
  expect_equal(got$args, list())
})

test_that("an explicit classify_fn wins over the scid default, directly or via enrichment_options", {
  custom <- function(...) NULL

  direct <- .capture_cohort_info_classify("scid", classify_fn = custom)
  expect_identical(direct$fn, custom)
  # The scid defaults must not be merged into someone else's classify_fn.
  expect_equal(direct$args, list())

  # Same through the option group: the auto-wiring runs after
  # .ehr_apply_grouped_options(), so a grouped value is already in scope and
  # the is.null() guard stands down for it.
  grouped <- .capture_cohort_info_classify(
    "scid", enrichment = enrichment_options(classify_fn = custom))
  expect_identical(grouped$fn, custom)
})

test_that("scid classify_args entries can be overridden without restating the rest", {
  got <- .capture_cohort_info_classify(
    "scid", classify_args = list(icds_cause_path = "my_causes.csv"))

  expect_identical(got$fn, scid_classify)
  expect_equal(got$args$icds_cause_path, "my_causes.csv")
  expect_equal(got$args$icds_scid_path, "scid_icds.csv")
})

# ---------------------------------------------------------------------------
# Annotation stage for a general (non-ICD) cohort.
#
# run_cohort_annotations = TRUE must work for SQL / direct-ID / no-filter
# cohorts, not only ICD cohorts. build_cohort_annotations() delegates to
# build_cohort_annotations_general(): membership comes from
# admission_windows.cohort_index, and no ICD filter is needed unless SCI
# enrichment is requested. This runs the real wrapper against a DuckDB eICU
# fixture standing in for the Postgres raw schema.
# ---------------------------------------------------------------------------

.eicu_annotation_fixture <- function(db) {
  con <- connect_duckdb(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA eicu_demo_coh")
  put <- function(name, df) DBI::dbWriteTable(
    con, DBI::Id(schema = "eicu_demo_coh", table = name), df)
  # Patient p1 has three hospital stays; stay 2 has two ICU stays (21, 22).
  put("patient", data.frame(
    uniquepid = "p1",
    patienthealthsystemstayid = c(1L, 2L, 2L, 3L),
    patientunitstayid = c(11L, 21L, 22L, 31L),
    hospitaldischargeyear = c(2014L, 2015L, 2015L, 2016L),
    hospitaldischargestatus = "Alive"))
  put("diagnosis", data.frame(
    patientunitstayid = c(11L, 21L, 31L),
    icd9code = c("401.9, I10", "806.0, S14.109A", "")))
  # A general cohort (e.g. SQL or direct IDs) selected hospital stay 2 only;
  # 1 and 3 are patient-context stays.
  put("cohort_hierarchy", data.frame(
    uniquepid = "p1",
    patienthealthsystemstayid = c(1L, 2L, 2L, 3L),
    patientunitstayid = c(11L, 21L, 22L, 31L),
    patient_context_index = TRUE,
    definition_filter_index = c(FALSE, TRUE, TRUE, FALSE),
    # Patient import scope: stays 1 and 3 are imported context, not members.
    scope_index = TRUE))
}

.run_annotations_on_duckdb <- function(db, profile, filter = NULL) {
  paths <- list(
    cohort_profile = profile, credentials_json = "unused",
    icd_filter_path = filter, icds_cause_path = NULL,
    icds_proc_path = NULL, icds_proc_dict_path = NULL,
    icds_dict_path = ehr_config_path("d_icd_diagnoses.csv"),
    icd_adjudication_path = NULL)
  local_mocked_bindings(
    ehr_paths            = function(...) paths,
    .ehr_resolve_db_name = function(...) "unused",
    load_credentials     = function(...) list(),
    connect_postgres     = function(...) connect_duckdb(db))
  suppressMessages(build_cohort_annotations("eicu_demo"))
}

test_that("build_cohort_annotations runs for a general eICU cohort without an ICD filter", {
  skip_if_not_installed("duckdb")
  db <- tempfile(fileext = ".duckdb")
  .eicu_annotation_fixture(db)

  expect_no_error(.run_annotations_on_duckdb(db, "general"))

  con <- connect_duckdb(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # Both tables the pipeline's build_cohort_annotations stage check requires.
  for (t in c("d_cohort_icd", "cohort_info")) {
    expect_true(DBI::dbExistsTable(con, DBI::Id(schema = "eicu_demo_coh",
                                                table = t)), info = t)
  }
  expect_identical(nrow(DBI::dbGetQuery(
    con, "SELECT * FROM eicu_demo_coh.d_cohort_icd")), 0L)

  ci <- DBI::dbGetQuery(con, "SELECT * FROM eicu_demo_coh.cohort_info")
  expect_false(any(c("hos_cohort_flg", "scope_index", "scope_level") %in% names(ci)))
  flag <- tapply(ci$cohort_index, ci$patienthealthsystemstayid, unique)
  # cohort_index is the cohort definition; both ICU rows of stay 2 carry it.
  expect_identical(as.logical(flag[c("1", "2", "3")]), c(FALSE, TRUE, FALSE))
  meta <- DBI::dbGetQuery(con, "SELECT * FROM eicu_demo_coh.cohort_info_meta")
  expect_identical(meta$analysis_level, "admission")
  expect_setequal(ci$patientunitstayid[ci$patienthealthsystemstayid == 2L],
                  c(21L, 22L))
})

test_that("the scid annotation profile still requires its SCI ICD list", {
  skip_if_not_installed("duckdb")
  db <- tempfile(fileext = ".duckdb")
  .eicu_annotation_fixture(db)
  expect_error(.run_annotations_on_duckdb(db, "scid", filter = NULL),
               "SCI enrichment \\(cohort.profile = 'scid'\\) requires")
})

test_that("ehr_paths records whether the cohort profile was defaulted", {
  base <- list(source = list(src_name = "mimic_iv_demo"),
               project = list(base_dir = tempdir()))
  expect_true(ehr_paths(config = base)$cohort_profile_is_default)
  expect_identical(ehr_paths(config = base)$cohort_profile, "generic")
  set <- utils::modifyList(base, list(cohort = list(profile = "custom")))
  expect_false(ehr_paths(config = set)$cohort_profile_is_default)
})

test_that("ehr_paths accepts the legacy top-level cohort_profile key", {
  legacy <- list(source = list(src_name = "mimic_iv_demo"),
                 project = list(base_dir = tempdir()),
                 cohort_profile = "custom")
  p <- ehr_paths(config = legacy)
  expect_identical(p$cohort_profile, "custom")
  expect_false(p$cohort_profile_is_default)
})

test_that("the raw parquet cache lives in the project, never in the raw source tree", {
  base <- tempfile("cache-loc-")
  raw_root <- file.path(base, "raw")
  p <- ehr_paths("mimic_iv_demo", base_dir = base, raw_data_root = raw_root)
  expect_false(startsWith(normalizePath(p$raw_parquet_cache_dir, mustWork = FALSE),
                          normalizePath(raw_root, mustWork = FALSE)))
  expect_identical(basename(p$raw_parquet_cache_dir), "mimic_iv_demo")
  moved <- ehr_paths(config = list(source = list(src_name = "mimic_iv_demo"),
                                   project = list(base_dir = base),
                                   paths = list(raw_parquet_cache_root = file.path(base, "shared"))))
  expect_identical(moved$raw_parquet_cache_dir, file.path(base, "shared", "mimic_iv_demo"))
})

test_that("run_concepts() stops on a user_concepts file that does not exist", {
  missing <- file.path(tempdir(), "no_such_user_concepts.csv")
  expect_error(run_concepts("mimic_iv_demo", db_name = "unused", base_dir = tempdir(),
                            user_concepts = missing),
               "`user_concepts` file not found")
})

test_that("SCI enrichment applies only to an SCI ICD list and an SCI or unnamed cohort", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA raw")
  sci <- system.file("extdata", "scid_icds_rendehr.rds", package = "rendehR")
  other <- tempfile(fileext = ".csv")
  writeLines(c("icd_code,icd_version", "S060X0A,10"), other)
  # a non-SCI list (no is_sci) never triggers it; an unnamed cohort with the SCI list does
  expect_false(.ehr_sci_enrichment_applies(other, con, "raw"))
  expect_true(.ehr_sci_enrichment_applies(sci, con, "raw"))
  # a cohort imported by another name keeps its own list: no SCI enrichment
  DBI::dbExecute(con, "CREATE TABLE raw.d_other_icds (icd_code TEXT)")
  expect_false(.ehr_sci_enrichment_applies(sci, con, "raw"))
  DBI::dbExecute(con, "CREATE TABLE raw.d_scid_icds (icd_code TEXT)")
  expect_true(.ehr_sci_enrichment_applies(sci, con, "raw"))
  # only cohorts selected by diagnosis: not SQL, ID-list or whole-source ones
  DBI::dbExecute(con, "CREATE TABLE raw.cohort_meta AS SELECT 'named_icd' AS route")
  expect_true(.ehr_sci_enrichment_applies(sci, con, "raw"))
  DBI::dbExecute(con, "UPDATE raw.cohort_meta SET route = 'sql'")
  expect_false(.ehr_sci_enrichment_applies(sci, con, "raw"))
})

test_that("ehr_init_project(base_dir = ) starts a new project while another is active", {
  old_active <- .ehr_env$config
  withr::defer(.ehr_env$config <- old_active)
  a <- tempfile("proj_a_"); b <- tempfile("proj_b_")
  withr::local_envvar(EHR_CONFIG = file.path(a, "config", "ehr_config.json"))
  .ehr_env$config <- NULL
  suppressMessages(ehr_init_project("mimic_iv_demo", base_dir = a))
  ehr_use_config(file.path(a, "config", "ehr_config.json"))
  suppressMessages(ehr_init_project("eicu_demo", base_dir = b))
  cfg_b <- file.path(b, "config", "ehr_config.json")
  expect_true(file.exists(cfg_b))
  expect_identical(ehr_load_config(cfg_b)$source$src_name, "eicu_demo")
  expect_identical(ehr_load_config(file.path(a, "config", "ehr_config.json"))$source$src_name, "mimic_iv_demo")
})

test_that("run_concepts() asks for run_raw_mods() when a source's *_mod tables are missing", {
  skip_if_not_installed("duckdb")
  bd <- tempfile("mods_")
  f <- ehr_paths("eicu_demo", base_dir = bd)$duckdb_path
  dir.create(dirname(f), recursive = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), f)
  raw <- src_schema_names("eicu_demo")$raw
  DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", raw))
  DBI::dbExecute(con, sprintf("CREATE TABLE %s.admission_windows (x INT)", raw))
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(suppressMessages(run_concepts("eicu_demo", engine = "duckdb", base_dir = bd, layers = "flat")),
               "lab_mod.*run_raw_mods_duckdb\\(\"eicu_demo\"\\)")
})

test_that("ehr_paths() resolves a relative base_dir once, to an absolute path", {
  withr::local_dir(tempdir())
  p <- ehr_paths("mimic_iv_demo", base_dir = "relproj", config = list())
  root <- normalizePath(file.path(tempdir(), "relproj"), winslash = "/", mustWork = FALSE)
  expect_identical(p$base_dir, root)
  expect_identical(p$raw_dir, file.path(root, "data", "source_data", "mimic_iv_demo"))
  expect_identical(p$cohort_parquet_dir, file.path(root, "data", "cohorts_data", "mimic_iv_demo_parquet"))
})

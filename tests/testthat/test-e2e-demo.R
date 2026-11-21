# End to end on the PhysioNet demo files: cohort import, database load, raw
# modifications, windows, concepts, harmonization, comorbidities, ISS, the ICU
# timeline, the merge, and every reader and exporter on the result.
#
# Writes a whole pipeline into a scratch database, so it runs only when
# EHR_TEST_E2E_DB names a database that may be created and dropped (it must
# contain "scratch" or "e2e") and EHR_TEST_E2E_RAW_ROOT points at a directory
# holding mimic_iv_demo/ and eicu_demo/ (PG* variables give the credentials).

.e2e_env <- local({
  cache <- NULL
  function() {
    db <- Sys.getenv("EHR_TEST_E2E_DB", ""); raw <- Sys.getenv("EHR_TEST_E2E_RAW_ROOT", "")
    skip_if(!nzchar(db) || !nzchar(raw), "set EHR_TEST_E2E_DB and EHR_TEST_E2E_RAW_ROOT to run the end-to-end test")
    skip_if(!grepl("scratch|e2e", db), "EHR_TEST_E2E_DB must be a scratch database (name containing 'scratch' or 'e2e')")
    skip_if_not_installed("RPostgres")
    missing <- c("mimic_iv_demo", "eicu_demo")[!dir.exists(file.path(raw, c("mimic_iv_demo", "eicu_demo")))]
    skip_if(length(missing) > 0, paste0("EHR_TEST_E2E_RAW_ROOT has no ", paste(missing, collapse = " or "),
                                        "; download them first with download_source()"))
    if (!is.null(cache)) return(cache)
    creds <- list(postgres = list(host = Sys.getenv("PGHOST", "localhost"), port = as.integer(Sys.getenv("PGPORT", "5432")),
                                  user = Sys.getenv("PGUSER", "postgres"), password = Sys.getenv("PGPASSWORD", "")))
    admin <- connect_postgres("postgres", creds)
    DBI::dbExecute(admin, sprintf('DROP DATABASE IF EXISTS "%s"', db))
    DBI::dbDisconnect(admin)
    base <- file.path(tempdir(), "rendehr_e2e"); unlink(base, recursive = TRUE)
    dir.create(file.path(base, "config"), recursive = TRUE)
    cred_path <- file.path(base, "config", "config_pass.json")
    jsonlite::write_json(creds, cred_path, auto_unbox = TRUE)
    cfg_path <- file.path(base, "config", "ehr_config.json")
    ehr_create_config_template(cfg_path, src_name = "mimic_iv_demo", db_name = db, base_dir = base,
                               raw_data_root = raw, credentials_path = cred_path, cohort_profile = "generic",
                               overwrite = TRUE)
    # the pipeline needs an active project config; restore the previous state
    # afterwards so later test files do not inherit it
    prev_cfg <- rendehR:::.ehr_env$config
    on.exit(assign("config", prev_cfg, envir = rendehR:::.ehr_env), add = TRUE)
    ehr_use_config(cfg_path)
    srcs <- c("mimic_iv_demo", "eicu_demo")
    steps <- list()
    for (s in srcs) {
      steps[[s]] <- list(
        import = import_cohort_general(s, base_dir = base, raw_data_root = raw, cohort = "scid", overwrite = TRUE,
                                       show_progress = FALSE),
        run = run_one_source(src_name = s, db_name = db, base_dir = base, raw_data_root = raw,
                             stages = setdiff(rendehR:::.ehr_default_pipeline_stages(run_cohort_annotations = FALSE, run_comorbidities = TRUE, run_iss = TRUE,
                                                                                     run_external = FALSE), "import_cohort"),
                             run_comorbidities = TRUE, run_iss = TRUE, skip_on_error = FALSE),
        icu = make_icu_timeline(s, db_name = db, base_dir = base))
    }
    merge_sources(srcs, db_name = db, base_dir = base, target_schema = "e2e_adm", analysis_level = "admission")
    merge_sources(srcs, db_name = db, base_dir = base, target_schema = "e2e_icu", analysis_level = "icu")
    ehr_connect(db, creds = creds)
    cache <<- list(db = db, creds = creds, base = base, srcs = srcs, steps = steps)
    cache
  }
})

test_that("the demo pipeline builds every stage for MIMIC-IV and eICU", {
  e <- .e2e_env()
  con <- connect_postgres(e$db, e$creds); on.exit(DBI::dbDisconnect(con))
  for (s in e$srcs) {
    sn <- src_schema_names(s)
    expect_true(all(vapply(c("ts_tab", "flat_tab", "dur_tab", "ts_icu"), function(t)
      DBI::dbExistsTable(con, DBI::Id(schema = sn$harmonized, table = t)), logical(1))), info = s)
    ci <- load_cohort_info(s)
    expect_gt(sum(ci$cohort_index %in% TRUE), 0)
    expect_true(all(c("admission_windows", "icu_windows", "visit_windows", "cohort_info") %in% list_tables(s)), info = s)
    expect_gt(nrow(load_visit_windows(s)), 0)
    expect_gt(nrow(load_icu_windows(s)), 0)
    expect_gt(nrow(load_admission_windows(s)), 0)
    lc <- list_concepts(s); expect_true("heart_rate" %in% lc$concept_name)
    expect_gt(nrow(fetch_concept(s, "heart_rate")), 0)
    f2 <- fetch_concepts(s, c("heart_rate", "sbp"))
    expect_setequal(unique(f2$concept_name), c("heart_rate", "sbp"))
    expect_gt(nrow(peek_table(s, "flat_tab", schema = "harmonized", n = 3)), 0)
    hr <- concept_summary(s, "heart_rate"); expect_gt(hr$row_count, 0)
    vh <- verify_harmonized(con, sn$harmonized)
    expect_true(vh$ok); expect_true(all(vh$checks))
    # source ids and labels are stored trimmed (eICU drug names are space-padded at the source)
    padded <- DBI::dbGetQuery(con, sprintf(
      "SELECT c.table_name FROM information_schema.columns c WHERE c.table_schema = '%s' AND c.column_name = 'source_itemid'",
      sn$concepts))$table_name
    n_pad <- sum(vapply(padded, function(t) as.numeric(DBI::dbGetQuery(con, sprintf(
      'SELECT count(*) n FROM "%s"."%s" WHERE source_itemid <> trim(source_itemid) OR source_label <> trim(source_label)',
      sn$concepts, t))$n), numeric(1)))
    expect_equal(n_pad, 0, info = s)
    if (identical(src_family(s), "eicu")) {
      # nurse-charted copies of a monitor value (same stay, minute and value) are removed, other nurse rows kept
      hr <- DBI::dbGetQuery(con, sprintf(
        "SELECT count(*) FILTER (WHERE n.source_table = 'nursecharting' AND EXISTS (SELECT 1 FROM \"%1$s\".heart_rate v
           WHERE v.source_table = 'vitalperiodic' AND v.patientunitstayid = n.patientunitstayid
             AND v.time_since_icu_min = n.time_since_icu_min AND v.value = n.value))::float8 AS copies,
                count(*) FILTER (WHERE n.source_table = 'nursecharting')::float8 AS nurse
         FROM \"%1$s\".heart_rate n", sn$concepts))
      expect_equal(hr$copies, 0)
      expect_gt(hr$nurse, 0)
    }
    expect_message(cs <- cohort_summary(s), "Cohort summary")
    # PostgreSQL counts arrive as integer64; the summary returns plain numbers
    expect_true(is.numeric(cs$flat_rows) && !inherits(cs$flat_rows, "integer64"))
    expect_gte(sum(unlist(cs$sex)), cs$patients); expect_lte(sum(unlist(cs$sex)), cs$flat_rows)
    key <- cohort_key(s)
    pid <- DBI::dbGetQuery(con, sprintf('SELECT %s AS k FROM "%s".ts_tab LIMIT 1', key, sn$harmonized))$k
    expect_gt(nrow(get_patient_ts(s, pid)), 0)
    expect_true(all(get_patient_ts(s, pid, analysis_level = "icu")$concept_name %in% get_patient_ts(s, pid)$concept_name))
    expect_gt(nrow(get_patient_flat(s, pid)), 0)
    w <- fetch_concepts_wide(s, spec = list(heart_rate = c("mean", "max")), bin_min = 24 * 60, n = 50)
    expect_true(is.data.frame(w))
  }
  expect_true(all(e$srcs %in% list_sources()))
  sc <- search_concepts("heart"); expect_true(all(e$srcs %in% sc$source)); expect_true("heart_rate" %in% sc$concept_name)
})

test_that("comorbidities and ISS are written for the cohort", {
  e <- .e2e_env()
  con <- connect_postgres(e$db, e$creds); on.exit(DBI::dbDisconnect(con))
  for (s in e$srcs) {
    raw <- src_schema_names(s)$raw
    expect_true(DBI::dbExistsTable(con, DBI::Id(schema = raw, table = "comorbidities")), info = s)
    expect_gt(as.numeric(DBI::dbGetQuery(con, sprintf('SELECT count(*) n FROM "%s".comorbidities', raw))$n), 0)
    expect_true(DBI::dbExistsTable(con, DBI::Id(schema = raw, table = "iss")), info = s)
  }
})

test_that("the merged schemas hold every source once per grain and the readers agree", {
  e <- .e2e_env()
  con <- connect_postgres(e$db, e$creds); on.exit(DBI::dbDisconnect(con))
  fl <- load_merged_flat("e2e_adm")
  expect_setequal(unique(fl$source_data), e$srcs)
  expect_equal(anyDuplicated(paste(fl$source_data, fl$src_admission_id)), 0L)
  ci <- load_merged_cohort_info("e2e_adm"); expect_gt(nrow(ci), 0)
  ts <- load_merged_ts("e2e_adm", concepts = "heart_rate")
  n_src <- sum(vapply(e$srcs, function(s) as.numeric(DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) n FROM \"%s\".ts_tab WHERE concept_name = 'heart_rate'", src_schema_names(s)$harmonized))$n), numeric(1)))
  expect_lte(nrow(ts), n_src)
  expect_gt(nrow(ts), 0)
  expect_gt(nrow(load_merged_dur("e2e_adm")), 0)
  wide <- load_merged_ts_wide("e2e_adm", spec = list(heart_rate = c("min", "max")), bin_min = 24 * 60, window = c(0, 0))
  expect_true(all(c("heart_rate_min", "heart_rate_max") %in% names(wide)))
  dw <- load_merged_dur_wide("e2e_adm", spec = list(heparin_exp = "count"), bin_min = 24 * 60, window = c(0, 0))
  expect_true(is.data.frame(dw))
  # ICU-level cohort filtering needs cohort_info built at ICU grain; this one is admission grain
  expect_error(load_merged_ts_wide("e2e_icu", spec = list(heart_rate = "mean"), bin_min = 60, analysis_level = "icu"),
               "ICU-grain membership")
  iw <- load_merged_ts_wide("e2e_icu", spec = list(heart_rate = "mean"), bin_min = 60, analysis_level = "icu",
                            cohort_only = FALSE, n = 100)
  expect_true("heart_rate" %in% names(iw))      # one aggregate: the column is named after the concept
  expect_message(drop_merged_schema("e2e_icu", dry_run = TRUE), "DRY RUN")
  expect_true(DBI::dbExistsTable(con, DBI::Id(schema = "e2e_icu", table = "ts")))
})

test_that("append mode replaces a source's merged rows instead of duplicating them", {
  e <- .e2e_env()
  con <- connect_postgres(e$db, e$creds); on.exit(DBI::dbDisconnect(con))
  counts <- function(schema) vapply(c("flat", "ts", "dur", "cohort_info"), function(t)
    as.numeric(DBI::dbGetQuery(con, sprintf('SELECT count(*) n FROM "%s"."%s"', schema, t))$n), numeric(1))
  by_source <- function(schema, t) DBI::dbGetQuery(con, sprintf(
    'SELECT source_data, count(*)::float8 n FROM "%s"."%s" GROUP BY 1 ORDER BY 1', schema, t))
  before <- counts("e2e_adm"); ts_before <- by_source("e2e_adm", "ts")
  for (s in e$srcs) {
    expect_message(merge_sources(s, db_name = e$db, base_dir = e$base, target_schema = "e2e_adm", append = TRUE),
                   "appended")
  }
  expect_equal(counts("e2e_adm"), before)
  expect_equal(by_source("e2e_adm", "ts"), ts_before)
  fl <- load_merged_flat("e2e_adm")
  expect_equal(anyDuplicated(paste(fl$source_data, fl$src_admission_id)), 0L)
  # the ICU-grain schema appends the same way
  icu_before <- counts("e2e_icu")
  suppressMessages(merge_sources(e$srcs[1], db_name = e$db, base_dir = e$base, target_schema = "e2e_icu",
                                 analysis_level = "icu", append = TRUE))
  expect_equal(counts("e2e_icu"), icu_before)
  # appending needs the merged tables to exist; a failed merge leaves nothing behind
  expect_error(merge_sources(e$srcs[1], db_name = e$db, base_dir = e$base, target_schema = "e2e_missing",
                             append = TRUE), "does not exist")
  expect_false(DBI::dbExistsTable(con, DBI::Id(schema = "e2e_missing", table = "ts")))
  # a source without harmonized tables is refused, naming the stage to run
  expect_error(merge_sources("mimic_iv_demo_none", db_name = e$db, base_dir = e$base, target_schema = "e2e_adm",
                             append = TRUE))
  expect_equal(counts("e2e_adm"), before)
})

test_that("exporters write the tables and queries they are given", {
  e <- .e2e_env()
  out <- file.path(e$base, "export"); dir.create(out, showWarnings = FALSE)
  s <- e$srcs[1]
  export_table_parquet(s, "flat_tab", output_dir = file.path(out, "flat"), schema = "harmonized", overwrite = TRUE, verbose = FALSE)
  pq <- list.files(file.path(out, "flat"), "^part-.*\\.parquet$", full.names = TRUE)
  expect_gt(length(pq), 0)
  n <- nrow(fetch_table(s, "flat_tab", schema = "harmonized"))
  expect_equal(sum(vapply(pq, function(f) nrow(arrow::read_parquet(f)), numeric(1))), n)
  gz <- file.path(out, "dur.csv.gz")
  export_table_csv_gz(s, "dur_tab", output_path = gz, schema = "harmonized", verbose = FALSE)
  expect_equal(nrow(readr::read_csv(gz, show_col_types = FALSE)), nrow(fetch_table(s, "dur_tab", schema = "harmonized")))
  q <- file.path(out, "q.csv.gz")
  export_query(sprintf('SELECT concept_name, count(*) AS n FROM "%s".ts_tab GROUP BY 1', src_schema_names(s)$harmonized),
               output_path = q, verbose = FALSE)
  expect_gt(nrow(readr::read_csv(q, show_col_types = FALSE)), 0)
})

test_that("writers and droppers work on scratch tables and refuse by default", {
  e <- .e2e_env()
  s <- e$srcs[1]
  write_table(s, data.frame(id = 1:3, label = c("a", "b", "c")), "e2e_scratch", schema = "derived")
  expect_equal(nrow(fetch_table(s, "e2e_scratch", schema = "derived")), 3)
  csv <- file.path(e$base, "scratch.csv"); utils::write.csv(data.frame(id = 4:5, label = "d"), csv, row.names = FALSE)
  write_csv_to_schema(s, csv, "e2e_scratch", schema = "derived", overwrite = FALSE, append = TRUE)
  expect_equal(nrow(fetch_table(s, "e2e_scratch", schema = "derived")), 5)
  expect_message(drop_table(s, "e2e_scratch", schema = "derived"), "DRY RUN")   # dry run
  expect_equal(nrow(fetch_table(s, "e2e_scratch", schema = "derived")), 5)
  drop_table(s, "e2e_scratch", schema = "derived", dry_run = FALSE)
  expect_false("e2e_scratch" %in% list_tables(s, schema = "derived"))
  expect_message(drop_source_schemas(s), "DRY RUN")                        # dry run only
  expect_gt(nrow(fetch_table(s, "flat_tab", schema = "harmonized")), 0)
})

test_that("engine = 'duckdb' builds the concepts in DuckDB without touching PostgreSQL", {
  e <- .e2e_env()
  base <- file.path(tempdir(), "rendehr_e2e_duck"); unlink(base, recursive = TRUE)
  dir.create(file.path(base, "config"), recursive = TRUE)
  # credentials that cannot reach any server: a PostgreSQL call would fail the run
  cred <- file.path(base, "config", "config_pass.json")
  jsonlite::write_json(list(postgres = list(host = "127.0.0.1", port = 1L, user = "nobody", password = "x")), cred, auto_unbox = TRUE)
  cfg_path <- file.path(base, "config", "ehr_config.json")
  ehr_create_config_template(cfg_path, src_name = "mimic_iv_demo", db_name = "rendehr_e2e_duck_never", base_dir = base,
                             raw_data_root = Sys.getenv("EHR_TEST_E2E_RAW_ROOT"), credentials_path = cred,
                             cohort_profile = "generic", overwrite = TRUE)
  prev_cfg <- rendehR:::.ehr_env$config
  on.exit(assign("config", prev_cfg, envir = rendehR:::.ehr_env), add = TRUE)
  ehr_use_config(cfg_path)
  import_cohort_general("mimic_iv_demo", base_dir = base, cohort = "scid", overwrite = TRUE, show_progress = FALSE)
  expect_message(
    log <- run_one_source("mimic_iv_demo", base_dir = base, engine = "duckdb", skip_on_error = FALSE,
                          stages = c("run_db", "run_raw_mods", "build_window_tables", "build_cohort_info",
                                     "run_comorbidities", "run_concepts", "run_harmonized_tables")),
    "skipping PostgreSQL-only stage")
  expect_true(all(log$status == "success"))
  expect_false(any(c("run_comorbidities", "run_harmonized_tables") %in% log$stage))
  dcon <- connect_duckdb(ehr_paths("mimic_iv_demo", base_dir = base)$duckdb_path, read_only = TRUE)
  pcon <- connect_postgres(e$db, e$creds); on.exit(DBI::dbDisconnect(pcon), add = TRUE)
  sch <- "mimic_iv_demo_coh_concepts"
  dt <- DBI::dbGetQuery(dcon, sprintf("SELECT table_name FROM information_schema.tables WHERE table_schema = '%s'", sch))$table_name
  pt <- DBI::dbGetQuery(pcon, sprintf("SELECT table_name FROM information_schema.tables WHERE table_schema = '%s'", sch))$table_name
  expect_gt(length(dt), 100)
  # same cohort, same concepts, same rows on both engines
  for (t in intersect(c("heart_rate", "sbp", "creatinine", "hemoglobin", "norepinephrine_exp", "antibiotics_exp"), intersect(dt, pt))) {
    nd <- as.numeric(DBI::dbGetQuery(dcon, sprintf('SELECT count(*) n FROM %s."%s"', sch, t))$n)
    np <- as.numeric(DBI::dbGetQuery(pcon, sprintf('SELECT count(*) n FROM %s."%s"', sch, t))$n)
    expect_equal(nd, np, info = t)
  }
  DBI::dbDisconnect(dcon, shutdown = TRUE)
  # run_one_source() writes no pipeline_log, so the resumable runner first reruns
  # the stages over the finished DuckDB file (republishing cohort_info in place)
  # and logs them; a second call then validates the file and skips every stage
  runner <- function() run_one_source_test("mimic_iv_demo", base_dir = base, engine = "duckdb",
                                           stages = c("run_db", "build_cohort_info", "run_comorbidities", "run_concepts"))
  expect_message(rlog <- runner(), "skipping PostgreSQL-only stage")
  expect_setequal(rlog$stage, c("run_db", "build_cohort_info", "run_concepts"))
  expect_true(all(rlog$status == "success"), info = paste(rlog$stage, rlog$status, rlog$message, collapse = "; "))
  rlog <- suppressMessages(runner())
  expect_true(all(rlog$status == "skipped"), info = paste(rlog$stage, rlog$status, rlog$message, collapse = "; "))
  expect_false("rendehr_e2e_duck_never" %in% DBI::dbGetQuery(pcon, "SELECT datname FROM pg_database")$datname)
})

test_that("the scratch database is dropped and no session state is left behind", {
  e <- .e2e_env()
  ehr_disconnect()
  expect_null(rendehR:::.ehr_env$creds)
  admin <- connect_postgres("postgres", e$creds)
  DBI::dbExecute(admin, sprintf("SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '%s' AND pid <> pg_backend_pid()", e$db))
  DBI::dbExecute(admin, sprintf('DROP DATABASE IF EXISTS "%s"', e$db))
  expect_false(e$db %in% DBI::dbGetQuery(admin, "SELECT datname FROM pg_database")$datname)
  DBI::dbDisconnect(admin)
})

# Tests that do not require a live database connection.
# DB-dependent tests are guarded by skip_if() and only run when
# EHR_TEST_DB is set in the environment.

test_that("ehr_connect stores credentials in package env", {
  fake_creds <- list(host = "localhost", username = "test", password = "x")
  ehr_connect("testdb", fake_creds)
  # Internal env should have values — test indirectly via ehr_disconnect
  expect_no_error(ehr_disconnect())
})

test_that("ehr_disconnect clears credentials", {
  ehr_connect("testdb", list())
  ehr_disconnect()
  # After disconnect, a .with_connection call must error (no credentials)
  expect_error(
    fetch_table("mimic_iv_demo", "admissions"),
    "No credentials stored"
  )
})

test_that("set_active_cohort and clear_active_cohort round-trip", {
  ids <- c(1L, 2L, 3L)
  set_active_cohort("mimic_iv_demo", ids)

  # active_cohort_info should not error
  expect_no_error(active_cohort_info())

  clear_active_cohort("mimic_iv_demo")

  # After clearing, .get_active_ids should return NULL
  # (tested indirectly — fetch without credentials must error on connection,
  #  not on a cohort filter, meaning the filter was cleared)
  expect_error(
    fetch_table("mimic_iv_demo", "admissions"),
    "No credentials stored"
  )
})

test_that("clear_active_cohort(NULL) clears all sources", {
  set_active_cohort("mimic_iv_demo", 1:5)
  set_active_cohort("eicu_demo",     1:3)
  clear_active_cohort()
  expect_no_error(active_cohort_info())  # should print "No active cohort filters"
})

test_that("merged wide id columns follow requested analysis grain", {
  all_cols <- c("ehr_id", "source_data", "src_patient_id",
                "src_admission_id", "src_icu_id", "assigned_icu_id")

  expect_equal(
    .merged_wide_id_cols(all_cols, "admission"),
    c("ehr_id", "source_data", "src_patient_id", "src_admission_id")
  )
  expect_equal(
    .merged_wide_id_cols(all_cols, "icu"),
    c("ehr_id", "source_data", "src_patient_id", "src_admission_id",
      "assigned_icu_id")
  )
  expect_error(
    .merged_wide_id_cols(setdiff(all_cols, "assigned_icu_id"), "icu", "rendehr"),
    "assigned_icu_id"
  )
})

test_that("wide spec normalization supports defaults and numeric bundles", {
  expect_equal(
    .ehr_normalize_spec(c("heart_rate", "map")),
    list(heart_rate = "mean", map = "mean")
  )
  expect_equal(
    .ehr_normalize_spec(c(heart_rate = "all_numeric")),
    list(heart_rate = .ehr_numeric_aggs)
  )
  expect_equal(
    .ehr_normalize_spec(list(heart_rate = c("all_numeric", "last", "count"))),
    list(heart_rate = c(.ehr_numeric_aggs, "last"))
  )
  expect_error(.ehr_normalize_spec(list(heart_rate = "mode")), "Unsupported")
})

test_that("wide aggregate SQL uses numeric reducers for numeric summaries", {
  # Non-numeric text values are NULLed by a guarded cast rather than erroring.
  expect_match(.ehr_wide_agg_sql("min", "event_time", "'hr'"),
               "^min\\(CASE WHEN CAST\\(value AS TEXT\\) ~ .* THEN CAST\\(value AS TEXT\\)::double precision END\\)")
  expect_match(.ehr_wide_agg_sql("max", "event_time", "'hr'"),
               "^max\\(CASE WHEN CAST\\(value AS TEXT\\) ~ .* THEN CAST\\(value AS TEXT\\)::double precision END\\)")
  expect_match(.ehr_wide_agg_sql("sd", "event_time", "'hr'"),
               "stddev_samp")
  expect_match(.ehr_wide_agg_sql("variance", "event_time", "'hr'"),
               "var_samp")
})

test_that("pivot window/complete_grid validate before touching the DB", {
  spec <- list(hr = "mean")
  # complete_grid / window need binning on
  expect_error(
    .ehr_pivot_wide(NULL, "s", "ts", "id", "t", spec, bin_min = NULL,
                    n = Inf, complete_grid = TRUE),
    "require `bin_min`"
  )
  expect_error(
    .ehr_pivot_wide(NULL, "s", "ts", "id", "t", spec, bin_min = NULL,
                    n = Inf, window = c(0, 47)),
    "require `bin_min`"
  )
  # window must be a valid length-2, from <= to range
  expect_error(
    .ehr_pivot_wide(NULL, "s", "ts", "id", "t", spec, bin_min = 60,
                    n = Inf, window = c(0, 1, 2)),
    "length-2"
  )
  expect_error(
    .ehr_pivot_wide(NULL, "s", "ts", "id", "t", spec, bin_min = 60,
                    n = Inf, window = c(5, 2)),
    "from <= to"
  )
})

test_that("register_concept_sql strips mimiciv_icu prefix", {
  sql <- "SELECT subject_id FROM mimiciv_icu.chartevents WHERE itemid = 1"
  adapted <- register_concept_sql(sql, "mimic_iv_demo", "test_concept")
  expect_false(grepl("mimiciv_icu\\.", adapted))
  expect_match(adapted, "chartevents")
})

test_that("register_concept_sql strips mimiciv_hosp prefix", {
  sql <- "SELECT subject_id FROM mimiciv_hosp.admissions"
  adapted <- register_concept_sql(sql, "mimic_iv_demo", "test_concept")
  expect_false(grepl("mimiciv_hosp\\.", adapted))
  expect_match(adapted, "admissions")
})

test_that("register_concept_sql wraps SELECT in CREATE TABLE", {
  sql     <- "SELECT subject_id FROM chartevents WHERE itemid = 1"
  adapted <- register_concept_sql(sql, "mimic_iv_demo", "hr_test")
  expect_match(adapted, 'CREATE TABLE "mimic_iv_demo_coh_concepts"\\."hr_test"')
})

test_that("register_concept_sql does not execute when dry_run = TRUE", {
  sql     <- "SELECT 1 AS x"
  adapted <- register_concept_sql(sql, "mimic_iv_demo", "noop_concept",
                                   dry_run = TRUE, save_to = FALSE)
  expect_type(adapted, "character")
  expect_true(nchar(adapted) > 0)
})

# ---------------------------------------------------------------------------
# DB-dependent tests — only run when EHR_TEST_DB is set
# ---------------------------------------------------------------------------

db_available <- function() nzchar(Sys.getenv("EHR_TEST_DB"))

test_that("fetch_table returns a data frame from a live DB", {
  skip_if_not(db_available(),
              "Set EHR_TEST_DB=<database> to run DB tests")
  skip_unless_pipeline_table("mimic_iv_demo_coh", "icu_windows")

  creds <- load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                        "config/config_pass.json"))
  ehr_connect(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(ehr_disconnect())

  df <- fetch_table("mimic_iv_demo", "icu_windows", n = 5)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
})

test_that("build_subcohort returns patient IDs from a live DB", {
  skip_if_not(db_available(),
              "Set EHR_TEST_DB=<database> to run DB tests")
  skip_unless_pipeline_table("mimic_iv_demo_coh", "cohort_info")

  creds <- load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                        "config/config_pass.json"))
  ehr_connect(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(ehr_disconnect())

  ids <- build_subcohort("mimic_iv_demo")
  expect_type(ids, "integer")
  expect_gt(length(ids), 0)
})

test_that("credentials template and loader include Postgres and PhysioNet", {
  old_creds <- .ehr_env$creds
  vars <- c("PGHOST", "PGPORT", "PGUSER", "PGPASSWORD",
            "PHYSIONET_USER", "PHYSIONET_PASS",
            "RICU_PHYSIONET_USER", "RICU_PHYSIONET_PASS")
  old_env <- Sys.getenv(vars, unset = NA_character_)
  names(old_env) <- vars
  Sys.unsetenv(vars)
  on.exit({
    .ehr_env$creds <- old_creds
    for (v in vars) {
      if (is.na(old_env[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(setNames(old_env[[v]], v)))
    }
  }, add = TRUE)

  .ehr_env$creds <- NULL
  .ehr_env$credential_source <- NULL
  tmp <- file.path(tempdir(), paste0("ehr_creds_", Sys.getpid(), ".json"))
  ehr_create_credentials_template(
    path = tmp,
    postgres_host = "dbhost",
    postgres_port = 15432L,
    postgres_user = "dbuser",
    postgres_password = "dbpass",
    physionet_username = "physuser",
    physionet_password = "physpass",
    overwrite = TRUE
  )

  creds <- load_credentials(tmp)

  expect_equal(creds$postgres$host, "dbhost")
  expect_equal(creds$postgres$port, 15432L)
  expect_equal(creds$postgres$user, "dbuser")
  expect_equal(creds$postgres$password, "dbpass")
  expect_equal(creds$physionet$username, "physuser")
  expect_equal(creds$physionet$password, "physpass")
})

test_that("PhysioNet `user` is read as an alias of `username`", {
  old_creds <- .ehr_env$creds
  old_source <- .ehr_env$credential_source
  vars <- c("PHYSIONET_USER", "PHYSIONET_PASS")
  old_env <- Sys.getenv(vars, unset = NA_character_)
  Sys.unsetenv(vars)
  on.exit({
    .ehr_env$creds <- old_creds
    .ehr_env$credential_source <- old_source
    for (v in vars) {
      if (is.na(old_env[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(setNames(old_env[[v]], v)))
    }
  }, add = TRUE)

  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(postgres = list(user = "postgres", password = "x"),
         physionet = list(user = "physuser", password = "physpass")),
    tmp, auto_unbox = TRUE
  )
  .ehr_env$creds <- NULL
  .ehr_env$credential_source <- NULL
  creds <- load_credentials(tmp)
  expect_equal(creds$physionet$username, "physuser")

  supplied <- list(postgres = list(user = "postgres", password = "x"),
                   physionet = list(user = "physuser", password = "physpass"))
  expect_equal(.ehr_normalize_creds(supplied)$physionet$username, "physuser")
  expect_error(
    .ehr_normalize_physionet(list(username = "a", user = "b")),
    "different values"
  )
})

test_that("credential cache follows the selected file and clears identity", {
  old_creds <- .ehr_env$creds
  old_source <- .ehr_env$credential_source
  # PGHOST and friends override the file by design; clear them for the test.
  vars <- c("PGHOST", "PGPORT", "PGUSER", "PGPASSWORD", "PGSSLMODE")
  old_env <- Sys.getenv(vars, unset = NA_character_)
  Sys.unsetenv(vars)
  on.exit({
    .ehr_env$creds <- old_creds
    .ehr_env$credential_source <- old_source
    for (v in vars) {
      if (is.na(old_env[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(setNames(old_env[[v]], v)))
    }
  }, add = TRUE)

  one <- tempfile(fileext = ".json")
  two <- tempfile(fileext = ".json")
  ehr_create_credentials_template(one, postgres_host = "one", overwrite = TRUE)
  ehr_create_credentials_template(two, postgres_host = "two", overwrite = TRUE)

  .ehr_env$creds <- NULL
  .ehr_env$credential_source <- NULL
  expect_equal(load_credentials(one)$postgres$host, "one")
  expect_equal(load_credentials(two)$postgres$host, "two")
  ehr_disconnect()
  expect_null(.ehr_env$credential_source)
})

test_that("credential missing rules are field-specific and placeholder-safe", {
  expect_true(.ehr_credential_missing(NULL))
  expect_true(.ehr_credential_missing(NA_character_))
  expect_true(.ehr_credential_missing(""))
  expect_false(.ehr_credential_missing("", allow_empty = TRUE))
  expect_true(.ehr_credential_missing("YOUR_POSTGRES_PASSWORD",
                                      allow_empty = TRUE))
  expect_false(.ehr_credential_missing("YOUR_REAL_ACCOUNT"))
})

test_that("load_src_cfg performs exact catalog lookup", {
  path <- tempfile(fileext = ".json")
  writeLines('[{"name":"one","release":"1"},{"name":"two","release":"2"}]',
             path)
  expect_equal(load_src_cfg(path, "two")$release, "2")
  expect_error(load_src_cfg(path, "missing"), "Available: one, two")
})

test_that("PostgreSQL sslmode comes from creds, then PGSSLMODE, then prefer", {
  old <- Sys.getenv("PGSSLMODE", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("PGSSLMODE") else Sys.setenv(PGSSLMODE = old), add = TRUE)
  Sys.unsetenv("PGSSLMODE")
  expect_identical(.ehr_pg_sslmode(list(postgres = list())), "prefer")
  Sys.setenv(PGSSLMODE = "require")
  expect_identical(.ehr_pg_sslmode(list(postgres = list())), "require")
  expect_identical(.ehr_pg_sslmode(list(postgres = list(sslmode = "disable"))), "disable")
  expect_error(.ehr_pg_sslmode(list(postgres = list(sslmode = "bogus"))), "Invalid")
})

test_that("merged loaders and summaries reject unsafe schema/source names before SQL", {
  evil <- 'm".ts t; DROP TABLE x; --'
  for (f in list(load_merged_ts, load_merged_dur, load_merged_flat,
                 load_merged_cohort_info)) {
    expect_error(f(evil, n = 1, con = NULL), "Invalid schema name")
  }
  expect_error(load_merged_ts_wide(evil, spec = list(), con = NULL), "Invalid schema name")
  expect_error(load_merged_dur_wide(evil, spec = list(), con = NULL), "Invalid schema name")
  expect_error(cohort_summary(evil, con = NULL), "Invalid source name")
  expect_error(list_concepts(evil, con = NULL), "Invalid source name")
})

test_that("cohort_summary() returns the counts it prints", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sn <- src_schema_names("mimic_iv_demo")
  for (sc in unlist(sn[c("raw", "concepts", "harmonized")])) DBI::dbExecute(con, sprintf('CREATE SCHEMA "%s"', sc))
  DBI::dbExecute(con, sprintf('CREATE TABLE "%s".icu_windows AS SELECT * FROM (VALUES (1, 10), (1, 11), (2, 12)) t(subject_id, stay_id)', sn$raw))
  DBI::dbExecute(con, sprintf('CREATE TABLE "%s".cohort_info AS SELECT * FROM (VALUES (1, 100), (1, 101), (2, 102), (3, 103)) t(subject_id, hadm_id)', sn$raw))
  DBI::dbExecute(con, sprintf('CREATE TABLE "%s".heart_rate (x INT)', sn$concepts))
  DBI::dbExecute(con, sprintf('CREATE TABLE "%s".ts_tab AS SELECT 1 AS v', sn$harmonized))
  DBI::dbExecute(con, sprintf("CREATE TABLE \"%s\".flat_tab AS SELECT * FROM (VALUES (1, 'F'), (2, 'M'), (3, 'M'), (3, 'M'), (4, NULL)) t(subject_id, sex)", sn$harmonized))
  msgs <- character()
  res <- withCallingHandlers(cohort_summary("mimic_iv_demo", con = con),
                             message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  # ICU patients, cohort patients and patients by sex count people, not rows
  expect_equal(res$icu_windows_rows, 3); expect_equal(res$icu_patients, 2)
  expect_equal(res$cohort_info_rows, 4); expect_equal(res$patients, 3); expect_equal(res$admissions, 4)
  expect_equal(res$concept_tables, 1)
  expect_equal(res$ts_rows, 1); expect_equal(res$flat_rows, 5)
  expect_equal(res$flat_patients, 4)
  expect_equal(res$sex, list(M = 2, F = 1, `(missing)` = 1)[names(res$sex)])
  expect_setequal(names(res$sex), c("M", "F", "(missing)"))
  expect_true(any(grepl("sex (flat patients, 4): M=2", msgs, fixed = TRUE)))
})

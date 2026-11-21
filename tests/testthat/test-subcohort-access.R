# Data access, subcohorts, mutation, export. Covers drop_relation()'s call
# sites, the identifier-validation contract, the version-counter
# cohort-temp cache, and materialize_subcohort()'s default `schemas`
# argument, plus baseline coverage for the read/discovery functions.
#
# Identifier-validation rejections fire before any connection is opened, so
# they need no database at all. materialize_subcohort()'s full flow and the
# version-counter cache use only generic SQL (TEMP TABLE/TRUNCATE/INSERT/
# information_schema), so they are exercised against an in-memory DuckDB
# connection, as in test-validate-helpers.R.
# drop_relation()'s relkind lookup uses Postgres-only pg_class/to_regclass()
# and is EHR_TEST_DB-gated (as for other Postgres-only functions in
# test-access.R).

db_available <- function() nzchar(Sys.getenv("EHR_TEST_DB"))

# ---------------------------------------------------------------------------
# Identifier validation fires before any connection is opened
# ---------------------------------------------------------------------------

test_that("browse_source rejects a malformed schema before connecting", {
  expect_error(
    browse_source("mimic_iv_demo", schema = "bad; drop table x"),
    "Invalid schema name"
  )
})

test_that("fetch_table rejects a malformed schema or table before connecting", {
  expect_error(
    fetch_table("mimic_iv_demo", "admissions", schema = "bad name"),
    "Invalid schema name"
  )
  expect_error(
    fetch_table("mimic_iv_demo", "bad; table", schema = "raw"),
    "Invalid table name"
  )
})

test_that("list_tables rejects a malformed schema before connecting", {
  expect_error(
    list_tables("mimic_iv_demo", schema = "bad.schema"),
    "Invalid schema name"
  )
})

test_that("peek_table rejects a malformed schema before connecting", {
  expect_error(
    peek_table("mimic_iv_demo", "admissions", schema = "bad schema"),
    "Invalid schema name"
  )
})

test_that("drop_schema rejects a malformed schema before connecting", {
  expect_error(
    drop_schema("mimic_iv_demo", schema = "bad schema"),
    "Invalid schema name"
  )
})

test_that("drop_relation rejects malformed schema/relation before connecting", {
  expect_error(
    drop_relation("t", schema = "bad schema"),
    "Invalid schema name"
  )
  expect_error(
    drop_relation("bad relation", schema = "s"),
    "Invalid relation name"
  )
  expect_error(
    drop_relation("no_dot_no_schema"),
    "must be"
  )
})

test_that("drop_relation splits a single \"schema.relation\" string correctly", {
  expect_error(
    drop_relation("bad schema.t"),
    "Invalid schema name"
  )
  expect_error(
    drop_relation("s.t.extra"),
    "must be"
  )
})

test_that("materialize_subcohort rejects a malformed src_name or subgroup_name before connecting", {
  expect_error(
    materialize_subcohort("bad name", 1:3, "sub"),
    "Invalid src_name"
  )
  expect_error(
    materialize_subcohort("mimic_iv_demo", 1:3, "bad sub"),
    "Invalid subgroup_name"
  )
})

# ---------------------------------------------------------------------------
# materialize_subcohort()'s default `schemas` argument
# ---------------------------------------------------------------------------

test_that("materialize_subcohort's default schemas argument resolves to real src_schema_names() keys", {
  schema_names <- src_schema_names("mimic_iv_demo")
  default_schemas <- formals(materialize_subcohort)$schemas
  default_schemas <- eval(default_schemas)
  expect_true(all(default_schemas %in% names(schema_names)))
  expect_setequal(default_schemas, c("raw", "concepts", "harmonized"))
})

# ---------------------------------------------------------------------------
# Version-counter cohort-temp cache - generic SQL, DuckDB
# ---------------------------------------------------------------------------

test_that(".setup_cohort_temp populates _ehr_cohort and _ehr_cohort_version on first call", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  v1 <- .ehr_next_cohort_version()
  .setup_cohort_temp(con, c("1", "2", "3"), v1)

  ids <- DBI::dbGetQuery(con, "SELECT cohort_id FROM _ehr_cohort ORDER BY cohort_id")$cohort_id
  expect_equal(ids, c("1", "2", "3"))
  stored_version <- DBI::dbGetQuery(con, "SELECT version FROM _ehr_cohort_version")$version
  expect_equal(as.integer(stored_version), as.integer(v1))
})

test_that(".setup_cohort_temp skips the rewrite when the version is unchanged", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  v1 <- .ehr_next_cohort_version()
  .setup_cohort_temp(con, c("1", "2", "3"), v1)

  # Same version, different ids: since the version matches, the rewrite is
  # skipped and the *original* ids remain - this directly demonstrates the
  # cache is version-gated, not content-gated.
  .setup_cohort_temp(con, c("9", "9", "9"), v1)
  ids <- DBI::dbGetQuery(con, "SELECT cohort_id FROM _ehr_cohort ORDER BY cohort_id")$cohort_id
  expect_equal(ids, c("1", "2", "3"))
})

test_that(".setup_cohort_temp rewrites when the version changes", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  v1 <- .ehr_next_cohort_version()
  .setup_cohort_temp(con, c("1", "2", "3"), v1)

  v2 <- .ehr_next_cohort_version()
  expect_gt(v2, v1)
  .setup_cohort_temp(con, c("4", "5"), v2)

  ids <- DBI::dbGetQuery(con, "SELECT cohort_id FROM _ehr_cohort ORDER BY cohort_id")$cohort_id
  expect_equal(ids, c("4", "5"))
  stored_version <- DBI::dbGetQuery(con, "SELECT version FROM _ehr_cohort_version")$version
  expect_equal(as.integer(stored_version), as.integer(v2))
})

test_that("set_active_cohort tags each call with a fresh, never-reused version", {
  old <- .ehr_env$active_cohorts
  on.exit(.ehr_env$active_cohorts <- old, add = TRUE)

  set_active_cohort("s011_test_src", 1:3, id_col = "patient_id")
  v1 <- .get_cohort_version(.get_active_ids("s011_test_src"))
  clear_active_cohort("s011_test_src")
  set_active_cohort("s011_test_src", 1:3, id_col = "patient_id")
  v2 <- .get_cohort_version(.get_active_ids("s011_test_src"))

  expect_gt(v2, v1)
  clear_active_cohort("s011_test_src")
})

# ---------------------------------------------------------------------------
# materialize_subcohort(): full flow against an in-memory DuckDB connection
# (generic SQL only: CREATE SCHEMA, information_schema.schemata, CREATE
# TABLE AS SELECT, DROP SCHEMA/TABLE - no Postgres-only syntax)
# ---------------------------------------------------------------------------

test_that("materialize_subcohort filters id-bearing tables and copies the rest", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  src_schema <- src_schema_names("mimic_iv_demo")$raw
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", DBI::dbQuoteIdentifier(con, src_schema)))
  id_col <- cohort_key("mimic_iv_demo")
  DBI::dbWriteTable(con, DBI::Id(schema = src_schema, table = "admissions"),
                    data.frame(x = setNames(1:5, NULL)) |>
                      (\(d) { d[[id_col]] <- 1:5; d })())
  DBI::dbWriteTable(con, DBI::Id(schema = src_schema, table = "lookup"),
                    data.frame(code = c("a", "b"), label = c("A", "B")))

  out <- materialize_subcohort("mimic_iv_demo", ids = c(1, 3),
                                subgroup_name = "test_sub",
                                schemas = "raw", con = con)
  expect_equal(out, "mimic_iv_demo_test_sub")

  dst_schema <- src_schema_names("mimic_iv_demo_test_sub")$raw
  filtered <- DBI::dbReadTable(con, DBI::Id(schema = dst_schema, table = "admissions"))
  expect_equal(nrow(filtered), 2)

  copied <- DBI::dbReadTable(con, DBI::Id(schema = dst_schema, table = "lookup"))
  expect_equal(nrow(copied), 2)
})

test_that("materialize_subcohort's default schemas argument does not error", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # No source schemas exist in this empty database, so every default schema
  # key is "Skipping (not found)" - the point of this test is only that
  # resolving the default `schemas` argument itself does not error.
  expect_no_error(
    materialize_subcohort("mimic_iv_demo", ids = 1:3, subgroup_name = "t2",
                          con = con)
  )
})

# ---------------------------------------------------------------------------
# drop_relation(): Postgres-only relkind lookup (pg_class/to_regclass), so
# EHR_TEST_DB-gated
# ---------------------------------------------------------------------------

test_that("drop_relation drops a real table and reports a real absence, live DB", {
  skip_if_not(db_available(), "Set EHR_TEST_DB=<database> to run DB tests")

  creds <- load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                        "config/config_pass.json"))
  ehr_connect(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(ehr_disconnect())

  con <- connect_postgres(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS s011_drop_relation_test")
  DBI::dbExecute(con,
    "CREATE TABLE s011_drop_relation_test.t (x INTEGER)")

  expect_message(
    drop_relation("t", schema = "s011_drop_relation_test", dry_run = TRUE, con = con),
    "\\[DRY RUN\\] Would drop TABLE"
  )
  expect_true(DBI::dbExistsTable(con,
    DBI::Id(schema = "s011_drop_relation_test", table = "t")))

  expect_message(
    drop_relation("t", schema = "s011_drop_relation_test", dry_run = FALSE, con = con),
    "Dropped TABLE"
  )
  expect_false(DBI::dbExistsTable(con,
    DBI::Id(schema = "s011_drop_relation_test", table = "t")))

  expect_message(
    drop_relation("t", schema = "s011_drop_relation_test", dry_run = FALSE, con = con),
    "does not exist"
  )

  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS s011_drop_relation_test CASCADE")
})

test_that("drop_relation drops a view correctly, live DB", {
  skip_if_not(db_available(), "Set EHR_TEST_DB=<database> to run DB tests")

  creds <- load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                        "config/config_pass.json"))
  con <- connect_postgres(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS s011_drop_relation_test2")
  DBI::dbExecute(con,
    "CREATE TABLE s011_drop_relation_test2.t (x INTEGER)")
  DBI::dbExecute(con,
    "CREATE VIEW s011_drop_relation_test2.v AS SELECT * FROM s011_drop_relation_test2.t")

  expect_message(
    drop_relation("v", schema = "s011_drop_relation_test2", dry_run = FALSE, con = con),
    "Dropped VIEW"
  )

  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS s011_drop_relation_test2 CASCADE")
})

# ---------------------------------------------------------------------------
# Baseline coverage: read/discovery functions
# ---------------------------------------------------------------------------

test_that("drop_table/drop_schema/drop_source_schemas/drop_merged_schema/drop_subcohort default to dry_run = TRUE", {
  expect_true(formals(drop_table)$dry_run)
  expect_true(formals(drop_schema)$dry_run)
  expect_true(formals(drop_source_schemas)$dry_run)
  expect_true(formals(drop_merged_schema)$dry_run)
  expect_true(formals(drop_relation)$dry_run)
  expect_true(formals(drop_subcohort)$dry_run)
})

test_that("active_time_window_info reports no active filters when none are set", {
  old <- .ehr_env$time_windows
  on.exit(.ehr_env$time_windows <- old, add = TRUE)
  .ehr_env$time_windows <- list()
  expect_message(active_time_window_info(), "No active time window filters")
})

test_that("active_time_window_info reports an active filter after set_time_window", {
  old <- .ehr_env$time_windows
  on.exit(.ehr_env$time_windows <- old, add = TRUE)
  set_time_window("s011_test_src", ref = "icu", clip = TRUE)
  expect_message(active_time_window_info(), "s011_test_src")
  clear_time_window("s011_test_src")
})

# ---------------------------------------------------------------------------
# Direct coverage for the remaining zero-caller functions
# (load_visit_windows, load_cohort_info, load_merged_ts_wide,
# load_merged_cohort_info), live DB (Postgres-only fetch_table()/
# .ehr_pivot_wide() SQL paths)
# ---------------------------------------------------------------------------

test_that("load_visit_windows and load_cohort_info fetch their fixed raw table names, live DB", {
  skip_if_not(db_available(), "Set EHR_TEST_DB=<database> to run DB tests")

  creds <- load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                        "config/config_pass.json"))
  con <- connect_postgres(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  raw_schema <- src_schema_names("mimic_iv_demo")$raw
  skip_if_schema_populated(con, raw_schema)
  DBI::dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, raw_schema)))
  DBI::dbWriteTable(con, DBI::Id(schema = raw_schema, table = "visit_windows"),
                    data.frame(subject_id = 1:2, note = c("a", "b")),
                    overwrite = TRUE)
  DBI::dbWriteTable(con, DBI::Id(schema = raw_schema, table = "cohort_info"),
                    data.frame(subject_id = 1:3, cohort_index = c(TRUE, FALSE, TRUE)),
                    overwrite = TRUE)

  vw <- load_visit_windows("mimic_iv_demo", con = con)
  expect_equal(nrow(vw), 2)

  ci <- load_cohort_info("mimic_iv_demo", con = con)
  expect_equal(nrow(ci), 3)

  DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", DBI::dbQuoteIdentifier(con, raw_schema), " CASCADE"))
})

test_that("load_merged_ts_wide and load_merged_cohort_info work against the rendehr merged schema, live DB", {
  skip_if_not(db_available(), "Set EHR_TEST_DB=<database> to run DB tests")

  creds <- load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                        "config/config_pass.json"))
  con <- connect_postgres(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  skip_if_schema_populated(con, "rendehr")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS rendehr")
  DBI::dbWriteTable(con, DBI::Id(schema = "rendehr", table = "ts"),
                    data.frame(ehr_id = c(1L, 1L, 2L), source_data = "mimic_iv_demo",
                              src_patient_id = c(1L, 1L, 2L), src_admission_id = c(1L, 1L, 2L),
                              concept_name = "heart_rate", value = c("70", "72", "80"),
                              unit = "bpm", time_since_hadm_min = c(0, 60, 0)),
                    overwrite = TRUE)
  DBI::dbWriteTable(con, DBI::Id(schema = "rendehr", table = "cohort_info"),
                    data.frame(source_data = "mimic_iv_demo", src_admission_id = c(1L, 2L),
                              cohort_index = c(TRUE, TRUE)),
                    overwrite = TRUE)

  wide <- load_merged_ts_wide("rendehr", spec = c(heart_rate = "mean"), bin_min = 60, con = con)
  expect_true(nrow(wide) > 0)
  expect_true("heart_rate" %in% names(wide))

  ci <- load_merged_cohort_info("rendehr", con = con)
  expect_equal(nrow(ci), 2)

  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS rendehr CASCADE")
})

test_that("fetch_table applies the active subcohort filter via the version-counter cache path, live DB", {
  skip_if_not(db_available(), "Set EHR_TEST_DB=<database> to run DB tests")

  creds <- load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                        "config/config_pass.json"))
  con <- connect_postgres(Sys.getenv("EHR_TEST_DB"), creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  raw_schema <- src_schema_names("mimic_iv_demo")$raw
  skip_if_schema_populated(con, raw_schema)
  DBI::dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, raw_schema)))
  DBI::dbWriteTable(con, DBI::Id(schema = raw_schema, table = "admissions"),
                    data.frame(subject_id = 1:5, note = letters[1:5]),
                    overwrite = TRUE)

  old <- .ehr_env$active_cohorts
  on.exit({
    .ehr_env$active_cohorts <- old
  }, add = TRUE)

  set_active_cohort("mimic_iv_demo", c(2, 4), id_col = "subject_id")
  filtered <- fetch_table("mimic_iv_demo", "admissions", schema = "raw", con = con)
  expect_equal(sort(filtered$subject_id), c(2, 4))

  # A second fetch_table() call on the same connection, same active cohort:
  # exercises the version-counter skip path, not just the first
  # (rewrite) path.
  filtered2 <- fetch_table("mimic_iv_demo", "admissions", schema = "raw", con = con)
  expect_equal(sort(filtered2$subject_id), c(2, 4))

  clear_active_cohort("mimic_iv_demo")
  unfiltered <- fetch_table("mimic_iv_demo", "admissions", schema = "raw", con = con)
  expect_equal(nrow(unfiltered), 5)

  DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", DBI::dbQuoteIdentifier(con, raw_schema), " CASCADE"))
})

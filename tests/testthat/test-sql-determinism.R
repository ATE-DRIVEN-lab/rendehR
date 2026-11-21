# Regression tests for the full-scale determinism finding (D-DET-01): a derived
# concept whose window functions order only by charttime resolves simultaneous
# readings (ties) in physical row order, which differs between builds made with
# different import chunking - so the same data produced different tables.
# The ehr/ SQL files now append a tie-break column; these tests build the tied
# rows in two opposite insertion orders and run the packaged SQL through
# run_external_concepts() against scratch PostgreSQL schema pairs. The two
# results must be identical, and must equal the documented deterministic
# resolution (weight/height DESC; phenylephrine endtime; micro_specimen_id),
# which the old queries only matched by accident of row order.
#
# Requires a live PostgreSQL (EHR_TEST_DB, see helper-db.R); skips otherwise.

.sqldet_db <- function() {
  db <- Sys.getenv("EHR_TEST_DB", "")
  skip_if(!nzchar(db), "set EHR_TEST_DB to a scratch database to run the SQL determinism tests")
  skip_if_not_installed("RPostgres")
  list(db = db, creds = list(postgres = list(
    host = Sys.getenv("PGHOST", "localhost"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "")
  )))
}

.sqldet_drop <- function(db, tags) {
  con <- connect_postgres(db$db, db$creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (tag in tags) for (suffix in c("derived", "raw")) {
    nm <- paste0("sqldet_", tag, "_", suffix)
    DBI::dbExecute(con, sprintf('DROP SCHEMA IF EXISTS "%s" CASCADE', nm))
  }
}

# Build one schema pair (raw + derived) for a tag and run the given packaged
# ehr/ concept files in it, in order, after the helper functions. `ddl` and
# `inserts` run in the raw schema (the statement order of `inserts` is what
# varies between the two physical orders). Schemas live until .sqldet_drop().
.sqldet_run <- function(db, tag, ddl, inserts, src, files) {
  raw <- paste0("sqldet_", tag, "_raw")
  derived <- paste0("sqldet_", tag, "_derived")
  con <- connect_postgres(db$db, db$creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, sprintf('DROP SCHEMA IF EXISTS "%s" CASCADE', derived))
  DBI::dbExecute(con, sprintf('DROP SCHEMA IF EXISTS "%s" CASCADE', raw))
  DBI::dbExecute(con, sprintf('CREATE SCHEMA "%s"', raw))
  DBI::dbExecute(con, sprintf('CREATE SCHEMA "%s"', derived))
  DBI::dbExecute(con, sprintf('SET search_path TO "%s"', raw))
  for (stmt in c(ddl, inserts)) DBI::dbExecute(con, stmt)
  DBI::dbExecute(con, "SET search_path TO public")

  # A minimal ehr/ sql dir: the packaged helper functions, the concept files in
  # their original relative paths, and a manifest giving the execution order.
  dir <- file.path(tempdir(), paste0("sqldet_", tag, "_sql"))
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  ehr_root <- system.file("sql", src, "concepts_sqls", "ehr", package = "rendehR")
  skip_if(!nzchar(ehr_root), "packaged ehr/ SQL not found")
  file.copy(file.path(ehr_root, "postgres-functions.sql"), dir)
  for (f in files) {
    dir.create(file.path(dir, dirname(f)), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(file.path(ehr_root, f), file.path(dir, f), overwrite = TRUE)
    stopifnot(ok)
  }
  writeLines(sprintf("\\i %s", files), file.path(dir, "postgres-make-concepts.sql"))

  suppressMessages(run_external_concepts(
    con = con, paths = list(), schema_names = list(raw = raw, derived = derived),
    src_name = src, sql_dir = dir, skip_on_error = FALSE, execute = TRUE
  ))
  list(raw = raw, derived = derived)
}

.sqldet_rows <- function(db, schema, table, order = "1") {
  con <- connect_postgres(db$db, db$creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s"."%s" ORDER BY %s', schema, table, order))
}

# chartevents/icustays/patients/echo_data fixture shared by the weight and
# height tests. Two ICU stays: an adult with two weights charted in the same
# minute, and a neonate whose height is charted in cm and inches at one instant.
.sqldet_demo_ddl <- function() c(
  "CREATE TABLE chartevents (subject_id INTEGER, icustay_id INTEGER, charttime TIMESTAMPTZ, itemid INTEGER, value TEXT, valuenum DOUBLE PRECISION, error INTEGER)",
  "CREATE TABLE icustays (icustay_id INTEGER, hadm_id INTEGER, intime TIMESTAMPTZ, outtime TIMESTAMPTZ)",
  "CREATE TABLE patients (subject_id INTEGER, dob TIMESTAMPTZ)",
  "CREATE TABLE echo_data (hadm_id INTEGER, charttime TIMESTAMPTZ, weight DOUBLE PRECISION)"
)

test_that("simultaneous weight readings resolve identically regardless of insertion order", {
  db <- .sqldet_db()
  on.exit(.sqldet_drop(db, c("detwa", "detwb")), add = TRUE)
  base <- c(
    "INSERT INTO icustays VALUES (1001, 10, TIMESTAMPTZ '2169-07-10 23:25:29+00', TIMESTAMPTZ '2169-07-23 15:56:35+00')",
    "INSERT INTO icustays VALUES (1002, 20, TIMESTAMPTZ '2186-11-01 07:02:10+00', TIMESTAMPTZ '2186-11-10 06:30:00+00')",
    "INSERT INTO patients VALUES (1, TIMESTAMPTZ '2150-01-01 00:00:00+00')",
    "INSERT INTO patients VALUES (2, TIMESTAMPTZ '2186-10-01 00:00:00+00')",
    "INSERT INTO chartevents VALUES (1, 1001, TIMESTAMPTZ '2169-07-11 00:16:00+00', 762, NULL, 74.2, 0)",
    "INSERT INTO chartevents VALUES (1, 1001, TIMESTAMPTZ '2169-07-22 22:00:00+00', 763, NULL, 68.7, 0)",
    "INSERT INTO chartevents VALUES (2, 1002, TIMESTAMPTZ '2186-11-01 05:02:10+00', 762, NULL, 2.01, 0)",
    "INSERT INTO chartevents VALUES (2, 1002, TIMESTAMPTZ '2186-11-02 12:00:00+00', 763, NULL, 1.92, 0)"
  )
  # The tie: two weights charted at the same instant, inserted in opposite orders.
  tie_a <- c(
    "INSERT INTO chartevents VALUES (1, 1001, TIMESTAMPTZ '2169-07-21 02:00:00+00', 763, NULL, 74.1, 0)",
    "INSERT INTO chartevents VALUES (1, 1001, TIMESTAMPTZ '2169-07-21 02:00:00+00', 763, NULL, 79.1, 0)"
  )
  tie_b <- rev(tie_a)
  a <- .sqldet_run(db, "detwa", .sqldet_demo_ddl(), c(base, tie_a), "mimic_iii", "durations/weight_durations.sql")
  b <- .sqldet_run(db, "detwb", .sqldet_demo_ddl(), c(base, tie_b), "mimic_iii", "durations/weight_durations.sql")
  ra <- .sqldet_rows(db, a$derived, "weight_durations", "1, 2, 4")
  rb <- .sqldet_rows(db, b$derived, "weight_durations", "1, 2, 4")
  expect_equal(ra, rb)

  ic1 <- ra[ra$icustay_id == 1001, ]
  expect_equal(nrow(ic1), 4L)
  long <- ic1[ic1$weight %in% c(74.1, 79.1) & ic1$starttime != ic1$endtime, ]
  zero <- ic1[ic1$weight %in% c(74.1, 79.1) & ic1$starttime == ic1$endtime, ]
  # deterministic tie-break: weight DESC, so 79.1 is the zero-length window
  expect_equal(long$weight, 74.1)
  expect_equal(zero$weight, 79.1)
})

test_that("simultaneous height readings resolve identically regardless of insertion order", {
  db <- .sqldet_db()
  on.exit(.sqldet_drop(db, c("detha", "dethb")), add = TRUE)
  base <- c(
    "INSERT INTO icustays VALUES (1002, 20, TIMESTAMPTZ '2186-11-01 07:02:10+00', TIMESTAMPTZ '2186-11-10 06:30:00+00')",
    "INSERT INTO patients VALUES (2, TIMESTAMPTZ '2186-10-01 00:00:00+00')",
    "INSERT INTO chartevents VALUES (2, 1002, TIMESTAMPTZ '2186-11-01 05:02:10+00', 762, NULL, 2.01, 0)"
  )
  # The tie: the same height charted in cm and in inches at one instant.
  tie_a <- c(
    "INSERT INTO chartevents VALUES (2, 1002, TIMESTAMPTZ '2186-11-02 21:00:00+00', 4188, NULL, 43.25, 0)",
    "INSERT INTO chartevents VALUES (2, 1002, TIMESTAMPTZ '2186-11-02 21:00:00+00', 4187, '17.0472', 17.0472, 0)"
  )
  tie_b <- rev(tie_a)
  # weight_durations first: heightweight reads it (the 2.01 admission weight)
  files <- c("durations/weight_durations.sql", "demographics/heightweight.sql")
  a <- .sqldet_run(db, "detha", .sqldet_demo_ddl(), c(base, tie_a), "mimic_iii", files)
  b <- .sqldet_run(db, "dethb", .sqldet_demo_ddl(), c(base, tie_b), "mimic_iii", files)
  ra <- .sqldet_rows(db, a$derived, "heightweight", "1")
  rb <- .sqldet_rows(db, b$derived, "heightweight", "1")
  expect_equal(ra, rb)
  # deterministic tie-break: height DESC, so the inch reading (43.3 cm) is first
  expect_equal(ra$height_first, 43.3)
  expect_equal(ra$height_min, 43.25)
})

test_that("infusion runs starting the same minute are numbered identically regardless of insertion order", {
  db <- .sqldet_db()
  on.exit(.sqldet_drop(db, c("detpa", "detpb")), add = TRUE)
  ddl <- c(
    "CREATE TABLE inputevents_cv (icustay_id INTEGER, charttime TIMESTAMPTZ, itemid INTEGER, rate DOUBLE PRECISION, amount DOUBLE PRECISION, stopped TEXT)",
    "CREATE TABLE inputevents_mv (icustay_id INTEGER, linkorderid INTEGER, itemid INTEGER, starttime TIMESTAMPTZ, endtime TIMESTAMPTZ, statusdescription TEXT)"
  )
  tie_a <- c(
    "INSERT INTO inputevents_mv VALUES (1003, 5, 221749, TIMESTAMPTZ '2118-09-29 05:38:00+00', TIMESTAMPTZ '2118-09-29 10:23:00+00', 'Valid')",
    "INSERT INTO inputevents_mv VALUES (1003, 6, 221749, TIMESTAMPTZ '2118-09-29 05:38:00+00', TIMESTAMPTZ '2118-09-29 10:03:00+00', 'Valid')"
  )
  tie_b <- rev(tie_a)
  a <- .sqldet_run(db, "detpa", ddl, tie_a, "mimic_iii", "durations/phenylephrine_durations.sql")
  b <- .sqldet_run(db, "detpb", ddl, tie_b, "mimic_iii", "durations/phenylephrine_durations.sql")
  ra <- .sqldet_rows(db, a$derived, "phenylephrine_durations", "2")
  rb <- .sqldet_rows(db, b$derived, "phenylephrine_durations", "2")
  expect_equal(ra, rb)
  # deterministic tie-break: endtime ascending, so vasonum 1 is the earlier run
  expect_equal(ra$vasonum, 1:2)
  expect_equal(format(ra$endtime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
               c("2118-09-29 10:03:00", "2118-09-29 10:23:00"))
})

test_that("cultures charted at the same instant pick the same specimen regardless of insertion order", {
  db <- .sqldet_db()
  on.exit(.sqldet_drop(db, c("detsa", "detsb")), add = TRUE)
  ddl <- c(
    "CREATE TABLE antibiotic (subject_id INTEGER, hadm_id INTEGER, stay_id INTEGER, antibiotic TEXT, starttime TIMESTAMPTZ, stoptime TIMESTAMPTZ)",
    "CREATE TABLE microbiologyevents (micro_specimen_id INTEGER, subject_id INTEGER, hadm_id INTEGER, chartdate DATE, charttime TIMESTAMPTZ, spec_type_desc TEXT, org_name TEXT, org_itemid INTEGER)"
  )
  base <- c(
    "INSERT INTO antibiotic VALUES (1, 10, 100, 'Vancomycin', TIMESTAMPTZ '2150-01-01 10:00:00+00', TIMESTAMPTZ '2150-01-02 10:00:00+00')"
  )
  # Ties in both orders of the file: two cultures charted at one instant
  # (subject 1), and the same drug ordered at one instant in two different
  # admissions (subject 2) - which admission gets the smaller ab_id was
  # previously decided by physical row order.
  tie_a <- c(
    "INSERT INTO antibiotic VALUES (2, 21, 201, 'Vancomycin', TIMESTAMPTZ '2150-02-01 10:00:00+00', TIMESTAMPTZ '2150-02-02 10:00:00+00')",
    "INSERT INTO antibiotic VALUES (2, 20, 200, 'Vancomycin', TIMESTAMPTZ '2150-02-01 10:00:00+00', TIMESTAMPTZ '2150-02-02 10:00:00+00')",
    "INSERT INTO microbiologyevents VALUES (701, 1, 10, DATE '2150-01-01', TIMESTAMPTZ '2150-01-01 02:23:00+00', 'SPEC-B', NULL, NULL)",
    "INSERT INTO microbiologyevents VALUES (700, 1, 10, DATE '2150-01-01', TIMESTAMPTZ '2150-01-01 02:23:00+00', 'SPEC-A', NULL, NULL)"
  )
  tie_b <- rev(tie_a)
  a <- .sqldet_run(db, "detsa", ddl, c(base, tie_a), "mimic_iv", "sepsis/suspicion_of_infection.sql")
  b <- .sqldet_run(db, "detsb", ddl, c(base, tie_b), "mimic_iv", "sepsis/suspicion_of_infection.sql")
  ra <- .sqldet_rows(db, a$derived, "suspicion_of_infection", "1, 4")
  rb <- .sqldet_rows(db, b$derived, "suspicion_of_infection", "1, 4")
  expect_equal(ra, rb)
  # deterministic tie-break: lowest micro_specimen_id, so SPEC-A (id 700)
  s1 <- ra[ra$subject_id == 1, ]
  expect_equal(s1$specimen, "SPEC-A")
  expect_equal(s1$positive_culture, 0)
  # deterministic ab_id tie-break: admission 20 (lower) is numbered first
  s2 <- ra[ra$subject_id == 2, ]
  expect_equal(s2$hadm_id[order(s2$ab_id)], c(20L, 21L))
  expect_equal(unique(s2$suspected_infection), 0)
})

test_that("blood gas specimens drawn at the same instant are both kept regardless of insertion order", {
  db <- .sqldet_db()
  on.exit(.sqldet_drop(db, c("detga", "detgb")), add = TRUE)
  ddl <- c(
    "CREATE TABLE labevents (subject_id INTEGER, hadm_id INTEGER, specimen_id INTEGER, itemid INTEGER, charttime TIMESTAMPTZ, storetime TIMESTAMPTZ, value TEXT, valuenum DOUBLE PRECISION)",
    "CREATE TABLE chartevents (subject_id INTEGER, charttime TIMESTAMPTZ, itemid INTEGER, valuenum DOUBLE PRECISION)"
  )
  base <- "INSERT INTO chartevents VALUES (1, TIMESTAMPTZ '2191-05-14 15:30:00+00', 220277, 96)"
  # an arterial and a mixed venous gas charted at one instant; the old query kept
  # one of them, picked by physical row order
  tie_a <- c(
    "INSERT INTO labevents VALUES (1, 10, 900, 52033, TIMESTAMPTZ '2191-05-14 16:09:00+00', NULL, 'ART.', NULL)",
    "INSERT INTO labevents VALUES (1, 10, 900, 50821, TIMESTAMPTZ '2191-05-14 16:09:00+00', NULL, '74', 74)",
    "INSERT INTO labevents VALUES (1, 10, 901, 52033, TIMESTAMPTZ '2191-05-14 16:09:00+00', NULL, 'MIX.', NULL)",
    "INSERT INTO labevents VALUES (1, 10, 901, 50821, TIMESTAMPTZ '2191-05-14 16:09:00+00', NULL, '49', 49)"
  )
  tie_b <- rev(tie_a)
  a <- .sqldet_run(db, "detga", ddl, c(base, tie_a), "mimic_iv", "measurement/bg.sql")
  b <- .sqldet_run(db, "detgb", ddl, c(base, tie_b), "mimic_iv", "measurement/bg.sql")
  ra <- .sqldet_rows(db, a$derived, "bg", "specimen")
  rb <- .sqldet_rows(db, b$derived, "bg", "specimen")
  expect_equal(ra, rb)
  expect_equal(ra$specimen, c("ART.", "MIX."))
  expect_equal(ra$po2, c(74, 49))
})

test_that("RBC transfusions charted at the same instant keep the documented unit regardless of insertion order", {
  db <- .sqldet_db()
  on.exit(.sqldet_drop(db, c("detra", "detrb")), add = TRUE)
  ddl <- c(
    "CREATE TABLE inputevents_cv (icustay_id INTEGER, charttime TIMESTAMPTZ, itemid INTEGER, amount DOUBLE PRECISION, amountuom TEXT, stopped TEXT)",
    "CREATE TABLE inputevents_mv (icustay_id INTEGER, endtime TIMESTAMPTZ, itemid INTEGER, amount DOUBLE PRECISION, amountuom TEXT)"
  )
  # one undocumented unit (amount imputed as 375 mL, no unit) and one charted in mL, at one instant
  tie_a <- c(
    "INSERT INTO inputevents_cv VALUES (275774, TIMESTAMPTZ '2126-03-18 01:00:00+00', 30179, NULL, NULL, NULL)",
    "INSERT INTO inputevents_cv VALUES (275774, TIMESTAMPTZ '2126-03-18 01:00:00+00', 30179, 375, 'ml', NULL)"
  )
  tie_b <- rev(tie_a)
  a <- .sqldet_run(db, "detra", ddl, tie_a, "mimic_iii", "fluid_balance/rbc_transfusion.sql")
  b <- .sqldet_run(db, "detrb", ddl, tie_b, "mimic_iii", "fluid_balance/rbc_transfusion.sql")
  ra <- .sqldet_rows(db, a$derived, "rbc_transfusion", "2")
  rb <- .sqldet_rows(db, b$derived, "rbc_transfusion", "2")
  expect_equal(ra, rb)
  expect_equal(nrow(ra), 1L)
  expect_equal(ra$amountuom, "ml")
})

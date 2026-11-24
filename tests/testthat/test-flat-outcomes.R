.outcome_sql <- function(con, fam, concept) {
  path <- system.file("sql", fam, "concepts_sqls", "flat", paste0(concept, ".sql"),
                      package = "rendehR")
  sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
  sql <- gsub("{raw_schema}", "raw", sql, fixed = TRUE)
  sql <- gsub("{concepts_schema}", "cc", sql, fixed = TRUE)
  for (stmt in strsplit(sql, ";\\s*\n")[[1]])
    if (nzchar(trimws(gsub("--[^\n]*", "", stmt)))) DBI::dbExecute(con, stmt)
  DBI::dbGetQuery(con, sprintf("SELECT * FROM cc.%s ORDER BY hadm_id", concept))
}

test_that("flat outcomes are unknown, not negative, for admissions outside the imported scope", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (s in c("raw", "cc")) DBI::dbExecute(con, paste("CREATE SCHEMA", s))
  ts <- function(x) as.POSIXct(x, tz = "UTC")
  # Patient 1 is readmitted (admission 11) five days after admission 10 and
  # dies there; admission 11 is outside the imported scope, so it is in
  # visit_windows but not in admissions. Patient 2 has a single admission.
  DBI::dbWriteTable(con, DBI::Id(schema = "raw", table = "visit_windows"), data.frame(
    subject_id = c(1L, 1L, 2L), hadm_id = c(10L, 11L, 20L), stay_id = NA_integer_,
    admittime = ts(c("2020-01-01", "2020-01-10", "2020-03-01")),
    dischtime = ts(c("2020-01-05", "2020-01-12", "2020-03-03"))))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw", table = "admissions"), data.frame(
    subject_id = c(1L, 2L), hadm_id = c(10L, 20L), hospital_expire_flag = 0L,
    admittime = ts(c("2020-01-01", "2020-03-01")), dischtime = ts(c("2020-01-05", "2020-03-03")),
    deathtime = ts(NA)))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw", table = "patients"), data.frame(
    subject_id = c(1L, 2L), dod = ts(c("2020-01-12", NA))))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw", table = "icustays"), data.frame(
    stay_id = integer(), intime = ts(character()), outtime = ts(character())))

  readm <- .outcome_sql(con, "mimic_iv", "readmission_30d")
  expect_identical(readm$readmission_30d, c(1L, NA, 0L))
  mort <- .outcome_sql(con, "mimic_iv", "hospital_mortality")
  expect_identical(mort$hospital_mortality, c(0L, NA, 0L))
  icu <- .outcome_sql(con, "mimic_iv", "icu_mortality")
  expect_true(all(is.na(icu$icu_mortality)))
})

test_that("MIMIC-III admission_service gives every ICU stay of an admission its service", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (s in c("raw", "cc")) DBI::dbExecute(con, paste("CREATE SCHEMA", s))
  ts <- function(x) as.POSIXct(x, tz = "UTC")
  # Admission 10 has two ICU stays; its first service is surgical.
  DBI::dbWriteTable(con, DBI::Id(schema = "raw", table = "visit_windows"), data.frame(
    subject_id = c(1L, 1L, 2L), hadm_id = c(10L, 10L, 20L), icustay_id = c(200L, 100L, 300L)))
  DBI::dbWriteTable(con, DBI::Id(schema = "raw", table = "services"), data.frame(
    subject_id = c(1L, 1L, 2L), hadm_id = c(10L, 10L, 20L),
    curr_service = c("TSURG", "MED", "MED"),
    transfertime = ts(c("2020-01-01 10:00", "2020-01-03 10:00", "2020-03-01 08:00"))))

  svc <- .outcome_sql(con, "mimic_iii", "admission_service")
  svc <- svc[order(svc$hadm_id, svc$icustay_id), ]
  expect_identical(svc$icustay_id, c(100L, 200L, 300L))
  expect_identical(svc$admission_service_raw, c("TSURG", "TSURG", "MED"))
  expect_identical(svc$admission_service, c("surgical", "surgical", "medical"))
})

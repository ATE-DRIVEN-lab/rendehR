test_that("MIMIC ICD evidence is selected at admission grain", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "diagnoses_icd",
    data.frame(
      subject_id = c(1L, 1L, 2L),
      hadm_id = c(10L, 11L, 20L),
      icd_code = c("S14.0", "X00", "S140"),
      icd_version = c(10L, 10L, 10L)
    )
  )
  filter <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "S140", icd_version = "10"), filter)
  selected <- .ehr_select_icd(
    con, "mimic_iv_demo", filter
  )
  expect_identical(selected$key, "hadm_id")
  expect_setequal(selected$values, c("10", "20"))
  expect_identical(selected$policy, "code_and_version")
})

test_that("MIMIC-IV selection does not match a code in the other ICD version", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # V103: ICD-9 "personal history of breast cancer" vs ICD-10 pedal cyclist.
  DBI::dbWriteTable(
    con, "diagnoses_icd",
    data.frame(subject_id = c(1L, 2L), hadm_id = c(10L, 20L),
               icd_code = c("V103", "V103"), icd_version = c(9L, 10L)))
  filter <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "V10.3", icd_version = "ICD-10"),
                   filter)
  expect_message(
    selected <- .ehr_select_icd(con, "mimic_iv_demo", filter),
    "other ICD version")
  expect_identical(selected$values, "20")
  expect_identical(selected$report$other_version_unit_count, 1)
})

test_that("MIMIC-III ICD evidence is also selected at admission grain", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "diagnoses_icd",
    data.frame(
      subject_id = c(1L, 2L),
      hadm_id = c(10L, 20L),
      icd9_code = c("806.0", "X00")
    )
  )
  filter <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "8060"), filter)
  selected <- .ehr_select_icd(con, "mimic_iii_demo", filter)
  expect_identical(selected$key, "hadm_id")
  expect_identical(selected$values, "10")
  expect_identical(selected$policy, "icd9_only")
})

test_that("MIMIC-III selection excludes explicit ICD-10 filter rows", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "diagnoses_icd",
    data.frame(subject_id = c(1L, 2L), hadm_id = c(10L, 20L),
               icd9_code = c("806.0", "V103")))
  filter <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = c("8060", "V103"),
                              icd_version = c("9", "10")), filter)
  expect_message(
    selected <- .ehr_select_icd(con, "mimic_iii_demo", filter),
    "excluded 1 ICD-10 code")
  expect_identical(selected$values, "10")
})

test_that("canonical eICU ICD adapter promotes unit evidence to hospitalizations", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "diagnosis",
    data.frame(
      patientunitstayid = c(100L, 101L, 200L),
      icd9code = c("S14.0, X00", "X00", "S140")
    )
  )
  # Units 100 and 101 share hospitalization 1; unit 200 is hospitalization 2.
  DBI::dbWriteTable(
    con, "patient",
    data.frame(uniquepid = c("a", "a", "b"),
               patienthealthsystemstayid = c(1L, 1L, 2L),
               patientunitstayid = c(100L, 101L, 200L)))
  filter <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "S140", icd_version = "10"), filter)
  selected <- .ehr_select_icd(con, "eicu_demo", filter)
  expect_identical(selected$key, "patienthealthsystemstayid")
  expect_setequal(selected$values, c("1", "2"))
  # Evidence stays at unit grain: 101 has no direct evidence.
  expect_identical(selected$evidence_key, "patientunitstayid")
  expect_setequal(selected$evidence_values, c("100", "200"))
  expect_identical(selected$policy, "code_and_version")

  plan <- .ehr_resolve_selection(
    con = con,
    src_name = "eicu_demo",
    paths = list(icd_filter_path = filter),
    ladder = c(
      "uniquepid", "patienthealthsystemstayid", "patientunitstayid"
    ),
    icd_filter_path = filter,
    import_scope = "icu"
  )
  expect_identical(plan$selection_key, "patienthealthsystemstayid")
  expect_identical(plan$evidence_key, "patientunitstayid")
  # ICD selection is hospital-grain, so an ICU import request clamps to it.
  expect_identical(plan$effective_scope, "patienthealthsystemstayid")
  expect_identical(
    plan$metadata$icd_version_policy,
    "code_and_version"
  )
})

test_that("eICU selection excludes and reports unresolved tokens", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # 31.1 (ICD-9 procedure) compacts to diagnosis 311; V42.7 is official in both
  # versions. Neither may select a stay. 311 itself is a genuine diagnosis.
  # Stay 5 holds an unresolved 31.1 AND a genuine 311: it is affected by an
  # unresolved token but has direct evidence. Stay 6 holds only 31.1, so it
  # has no direct evidence - yet it shares hospitalization 5 with stay 5, so
  # it is still a member: lacking evidence is not exclusion.
  DBI::dbWriteTable(
    con, "diagnosis",
    data.frame(patientunitstayid = c(1L, 2L, 3L, 4L, 5L, 6L),
               icd9code = c("31.1", "V42.7, Z94.4", "311", "", "31.1, 311",
                            "31.1")))
  DBI::dbWriteTable(
    con, "patient",
    data.frame(uniquepid = "p", patienthealthsystemstayid = c(1:5, 5L),
               patientunitstayid = 1:6))
  filter <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = c("311", "V427", "V427"),
                              icd_version = c("9", "9", "10")), filter)
  expect_message(
    selected <- .ehr_select_icd(con, "eicu_demo", filter),
    "no resolved ICD version")
  # Selection is hospital-based; the diagnostic counts stay unit-based.
  expect_setequal(selected$values, c("3", "5"))
  expect_setequal(selected$evidence_values, c("3", "5"))
  expect_identical(selected$report$unresolved_token_count, 4)
  expect_identical(selected$report$unresolved_affected_unit_count, 4)
  expect_identical(selected$report$unresolved_no_evidence_unit_count, 3)
  expect_setequal(selected$report$unresolved_by_status$status,
                  c("ambiguous", "unmatched"))
})

test_that("eICU selection fails clearly without a usable dictionary", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "diagnosis",
                    data.frame(patientunitstayid = 1L, icd9code = "311"))
  filter <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "311", icd_version = "9"), filter)
  expect_error(
    .ehr_select_icd(con, "eicu_demo", filter,
                    icds_dict_path = tempfile(fileext = ".csv")),
    "requires the diagnosis dictionary")
})

test_that("ICD filter version values are validated per source", {
  write_filter <- function(df) {
    p <- tempfile(fileext = ".csv"); readr::write_csv(df, p); p
  }
  no_version <- write_filter(data.frame(icd_code = "8060"))
  expect_error(.ehr_icd_filter_keys(no_version, "mimic_iv_demo"),
               "must contain an `icd_version`")
  expect_error(.ehr_icd_filter_keys(no_version, "eicu_demo"),
               "must contain an `icd_version`")
  expect_identical(.ehr_icd_filter_keys(no_version, "mimic_iii_demo")$icd_version,
                   "9")

  for (bad in list(NA_character_, "", "11", "x", "-9", "-10", "ICD--10",
                   "ICD 10", "9.5")) {
    p <- write_filter(data.frame(icd_code = c("8060", "S140"),
                                 icd_version = c("9", bad)))
    expect_error(.ehr_icd_filter_keys(p, "mimic_iv_demo"),
                 "1 missing, blank or unsupported")
    expect_error(.ehr_icd_filter_keys(p, "mimic_iii_demo"),
                 "1 missing, blank or unsupported")
  }

  ok <- write_filter(data.frame(
    icd_code = c("806.0", "S14.0", "E880.1", "E88.01"),
    icd_version = c("9", " ICD-10 ", "icd9", "10")))
  keys <- .ehr_icd_filter_keys(ok, "mimic_iv_demo")
  expect_identical(keys$icd_code, c("8060", "S140", "E8801", "E8801"))
  expect_identical(keys$icd_version, c("9", "10", "9", "10"))
})

test_that("an accepted version spelling normalizes the same in every path", {
  p <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = c("S14.0", "806.0"),
                              icd_version = c("ICD-10", "icd9"),
                              long_title = c("cord injury", "fracture")), p)
  # Selection keys, the annotation loader, and the published dictionary.
  expect_identical(.ehr_icd_filter_keys(p, "mimic_iv_demo")$icd_version,
                   c("10", "9"))
  expect_identical(.load_icd_filter(p)$icd_version, c("10", "9"))
  expect_identical(.ehr_icd_filter_dict(p)$icd_version, c("10", "9"))

  bad <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "S14.0", icd_version = "-10"), bad)
  expect_error(.ehr_icd_filter_keys(bad, "mimic_iv_demo"), "unsupported")
  expect_error(.load_icd_filter(bad), "unsupported")
  expect_error(.ehr_icd_filter_dict(bad), "unsupported")
})

test_that("published ICD filter dictionary keeps both versions of a code", {
  p <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = c("E8801", "E8801", "E8801"),
                              icd_version = c("9", "10", "10"),
                              long_title = c("fall on stairs", "lactic acidosis",
                                             "lactic acidosis")), p)
  d <- .ehr_icd_filter_dict(p)
  expect_identical(nrow(d), 2L)
  expect_setequal(d$icd_version, c("9", "10"))
})

test_that("experimental eICU diagnostic export remains visible and atomic", {
  skip_if_not_installed("arrow")
  path <- tempfile("eicu_diag_")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  diagnosis <- file.path(path, "diagnosis.csv.gz")
  patient <- file.path(path, "patient.csv.gz")
  dictionary <- file.path(path, "dictionary.csv")
  output <- file.path(path, "diagnosis_icd_mod.parquet")

  readr::write_csv(
    data.frame(
      diagnosisid = "1",
      patientunitstayid = "100",
      activeupondischarge = "True",
      diagnosisoffset = "5",
      diagnosispriority = "Primary",
      diagnosisstring = "test",
      # 123.4: official in both versions (ambiguous); A123: undotted, ICD-10
      # only; 31.1: ICD-9 procedure whose compact form is diagnosis 311;
      # Z99.9: matches nothing (no regex version).
      icd9code = "123.4, A123, 31.1, Z99.9"
    ),
    diagnosis
  )
  readr::write_csv(
    data.frame(
      patientunitstayid = "100",
      patienthealthsystemstayid = "10",
      uniquepid = "P1"
    ),
    patient
  )
  readr::write_csv(
    data.frame(
      icd_code = c("1234", "1234", "A123", "311"),
      icd_version = c("9", "10", "10", "9"),
      long_title = c("nine", "ten", "alpha", "depressive disorder")
    ),
    dictionary
  )

  result <- build_eicu_diagnosis_icd_parquet(
    diagnosis, patient, dictionary, output
  )
  expect_identical(result, output)
  out <- as.data.frame(arrow::read_parquet(output))
  expect_true(all(c(
    "icd_version", "is_ambiguous", "version_source", "match_n",
    "long_titles_all"
  ) %in% names(out)))
  row <- function(tok) out[out$icd_code_raw == tok, ]
  # Same version and status as the shared resolver the pipeline uses.
  expect_identical(row("123.4")$version_source, "ambiguous")
  expect_true(row("123.4")$is_ambiguous)
  expect_true(is.na(row("123.4")$icd_version))
  expect_identical(row("123.4")$match_n, 2L)
  expect_identical(row("A123")$version_source, "compact_unique")
  expect_identical(row("A123")$icd_version, "10")
  expect_identical(row("31.1")$version_source, "unmatched")
  expect_true(is.na(row("31.1")$icd_version))
  expect_identical(row("Z99.9")$version_source, "unmatched")
  expect_true(is.na(row("Z99.9")$icd_version))
  # A regex guess is diagnostic only, never a version.
  expect_identical(row("Z99.9")$regex_version_suggestion, "10")
  expected <- .eicu_resolve_icd_tokens(
    out$icd_code_raw, .eicu_read_icd_dict(dictionary),
    .load_eicu_icd_adjudication(NULL))
  m <- match(toupper(out$icd_code_raw), expected$icd_code_raw)
  expect_identical(out$icd_version, expected$icd_version[m])
  expect_identical(out$version_source, expected$version_resolution_status[m])
  expect_error(
    build_eicu_diagnosis_icd_parquet(
      diagnosis, patient, dictionary, output
    ),
    "already exists"
  )
  expect_identical(
    build_eicu_diagnosis_icd_parquet(
      diagnosis, patient, dictionary, output, overwrite = TRUE
    ),
    output
  )
  expect_false(any(grepl("[.](stage|recovery)-", list.files(
    path, all.files = TRUE
  ))))
})

test_that("ICD selection reports filter codes valid in both versions", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "diagnoses_icd",
                    data.frame(subject_id = 1L, hadm_id = 10L,
                               icd_code = "S140", icd_version = 10L))
  collide <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = c("V10.3", "S14.0"),
                              icd_version = c("10", "10")), collide)
  expect_message(
    selected <- .ehr_select_icd(con, "mimic_iv_demo", collide),
    "1 code\\(s\\) are valid diagnoses in both ICD-9 and ICD-10 \\(e.g. V103\\)")
  expect_identical(selected$report$filter_collision_count, 1)

  clean <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "S14.0", icd_version = "10"), clean)
  expect_no_message(selected <- .ehr_select_icd(con, "mimic_iv_demo", clean))
  expect_identical(selected$report$filter_collision_count, 0)
})

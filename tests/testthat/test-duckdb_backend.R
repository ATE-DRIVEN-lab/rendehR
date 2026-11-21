

test_that("Postgres SQL adapter applies conservative DuckDB rewrites", {
  sql <- paste(
    "CREATE TABLE x (charttime TIMESTAMP(6));",
    "DROP TABLE IF EXISTS old_table CASCADE;",
    "SELECT amount::text, dose::double precision,",
    "substring(original_drugname from '\\([^()]*\\)$') AS unit_part FROM infusiondrug;",
    sep = "\n"
  )

  adapted <- adapt_sql_for_duckdb(sql)

  expect_false(grepl("TIMESTAMP\\(6\\)", adapted))
  expect_true(grepl("charttime TIMESTAMP", adapted, fixed = TRUE))
  expect_true(grepl("DROP TABLE IF EXISTS old_table;", adapted, fixed = TRUE))
  expect_equal(
    adapt_sql_for_duckdb("DROP TABLE IF EXISTS standalone CASCADE"),
    "DROP TABLE IF EXISTS standalone;"
  )
  expect_true(grepl("amount::VARCHAR", adapted, fixed = TRUE))
  expect_true(grepl("dose::DOUBLE", adapted, fixed = TRUE))
  expect_true(grepl(
    "regexp_extract(original_drugname, '\\([^()]*\\)$') AS unit_part",
    adapted,
    fixed = TRUE
  ))
})

test_that("adapted eICU infusiondrug raw modification SQL runs in DuckDB", {
  skip_if_not_installed("duckdb")
  sql_candidates <- c(
    file.path("inst", "sql", "eicu", "raw_mods", "infusiondrug_mod.sql"),
    file.path("..", "..", "inst", "sql", "eicu", "raw_mods", "infusiondrug_mod.sql")
  )
  sql_path <- sql_candidates[file.exists(sql_candidates)]
  skip_if_not(length(sql_path) >= 1L, "bundled eICU raw-mod SQL not available")
  sql_path <- sql_path[[1]]

  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, paste(
    "CREATE TABLE infusiondrug(",
    "infusiondrugid INTEGER, patientunitstayid INTEGER, infusionoffset INTEGER,",
    "drugname VARCHAR, drugrate VARCHAR, infusionrate VARCHAR,",
    "drugamount VARCHAR, volumeoffluid VARCHAR, patientweight VARCHAR)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO infusiondrug VALUES",
    "(11, 1, 5, 'Norepinephrine (mcg/min)', '2.13', '8', '4', '250', NULL),",
    "(12, 2, 6, 'Propofol (mcg/kg/min)', '.5', NULL, NULL, NULL, NULL),",
    "(13, 3, 7, 'Amiodarone (mg/min)', '05', NULL, NULL, NULL, NULL),",
    "(14, 4, 8, 'Midazolam (mg/hr)', '40 mg/hr', NULL, NULL, NULL, NULL),",
    "(15, 5, 9, 'Volume (ml) Phenylephrine ()', 'ERROR', NULL, NULL, NULL, NULL),",
    "(16, 6, 10, 'Heparin (units/hr)', '00', NULL, NULL, NULL, NULL),",
    "(17, 7, 11, 'Mystery drug', '1,200', NULL, '1,200', NULL, NULL)"
  ))

  sql <- paste(readLines(sql_path, warn = FALSE), collapse = "\n")
  DBI::dbExecute(con, adapt_sql_for_duckdb(sql))

  out <- DBI::dbGetQuery(con,
    "SELECT * FROM infusiondrug_mod ORDER BY infusiondrugid")

  expect_equal(out$drugrate, c("2.13", ".5", "05", "40 mg/hr", "ERROR", "00", "1,200"))
  expect_equal(out$mod_drugname, c("Norepinephrine", "Propofol", "Amiodarone",
                                   "Midazolam", "Volume (ml) Phenylephrine",
                                   "Heparin", "Mystery drug"))
  expect_equal(out$mod_drugrate, c(2.13, 0.5, NA, 40, NA, 0, 1200))
  expect_equal(out$mod_drugrate_unit, c("mcg/min", "mcg/kg/min", "mg/min",
                                        "mg/hr", NA, "units/hr", NA))
  expect_equal(out$mod_drugrate_note, c(NA, NA, "05 (ambiguous leading zero)",
                                        "mg/hr", "ERROR", NA, NA))
  expect_equal(out$mod_drugamount, c(4, NA, NA, NA, NA, NA, 1200))
  expect_equal(out$mod_drugstopoffset, out$mod_drugstartoffset)
})


test_that("run_raw_mods_duckdb builds default eICU raw modification tables", {
  skip_if_not_installed("duckdb")

  tmp <- file.path(tempdir(), paste0("duckdb_raw_mods_", Sys.getpid()))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  paths <- ehr_paths(src_name = "eicu", base_dir = tmp)
  skip_if_not(dir.exists(paths$raw_mods_sql_dir), "bundled eICU raw-mod SQL not available")

  db_path <- file.path(tmp, "raw_mods.duckdb")
  con_setup <- connect_duckdb(db_path)
  on.exit(if (DBI::dbIsValid(con_setup)) DBI::dbDisconnect(con_setup, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con_setup, "CREATE SCHEMA eicu_coh")
  DBI::dbExecute(con_setup, paste(
    "CREATE TABLE eicu_coh.admissiondrug(",
    "admissiondrugid INTEGER, patientunitstayid INTEGER, drugoffset INTEGER,",
    "drugenteredoffset INTEGER, drugnotetype VARCHAR, specialtytype VARCHAR,",
    "usertype VARCHAR, rxincluded VARCHAR, writtenineicu VARCHAR, drugname VARCHAR,",
    "drugdosage DOUBLE, drugunit VARCHAR, drugadmitfrequency VARCHAR,",
    "drughiclseqno INTEGER)"
  ))
  DBI::dbExecute(con_setup, paste(
    "INSERT INTO eicu_coh.admissiondrug VALUES",
    "(31, 1, 10, 12, 'Admission', 'eCM Primary', 'THC Nurse', 'True', 'True',",
    " 'Vancomycin   ', 500, ' mg ', 'daily', 4042),",
    "(32, 2, 20, 25, 'Admission', 'eCM Primary', 'THC Nurse', 'True', 'True',",
    " 'EmptyUnit', 100, '', '', 1)"
  ))
  DBI::dbExecute(con_setup, paste(
    "CREATE TABLE eicu_coh.infusiondrug(",
    "infusiondrugid INTEGER, patientunitstayid INTEGER, infusionoffset INTEGER,",
    "drugname VARCHAR, drugrate VARCHAR, infusionrate VARCHAR,",
    "drugamount VARCHAR, volumeoffluid VARCHAR, patientweight VARCHAR)"
  ))
  DBI::dbExecute(con_setup, paste(
    "INSERT INTO eicu_coh.infusiondrug VALUES",
    "(11, 1, 5, 'Norepinephrine (mcg/min)', '2.13', '8', '4', '250', NULL),",
    "(12, 2, 6, 'Mystery drug (unknown)', 'bad', NULL, NULL, NULL, NULL)"
  ))
  DBI::dbExecute(con_setup, paste(
    "CREATE TABLE eicu_coh.medication(",
    "medicationid INTEGER, patientunitstayid INTEGER, drugorderoffset INTEGER,",
    "drugstartoffset INTEGER, drugivadmixture VARCHAR, drugordercancelled VARCHAR,",
    "drugname VARCHAR, drughiclseqno INTEGER, dosage VARCHAR, routeadmin VARCHAR,",
    "frequency VARCHAR, loadingdose VARCHAR, prn VARCHAR, drugstopoffset INTEGER,",
    "gtc INTEGER)"
  ))
  # 1866 and 18084 are a regular and a mixed drug code: the shipped lookup
  # names the first (ACETAMINOPHEN) and leaves out the second.
  DBI::dbExecute(con_setup, paste(
    "INSERT INTO eicu_coh.medication VALUES",
    "(21, 1, 0, 1, 'No', 'No', 'Aspirin', 1820, '81 mg', 'PO', NULL, NULL, 'No', 2, 0),",
    "(22, 2, 0, 3, 'No', 'No', 'OddDose', NULL, 'twice daily', 'IV', NULL, NULL, 'No', 4, 0),",
    "(23, 3, 0, 5, 'No', 'No', NULL, 1866, '1,000 3', 'PO', NULL, NULL, 'No', 6, 2),",
    "(24, 4, 0, 7, 'No', 'No', '  ', 18084, '5-10 mg', 'IV', NULL, NULL, 'No', 8, 0),",
    "(25, 5, 0, 9, 'No', 'Yes', 'Morphine', 1694, '2 10021', 'IV', NULL, NULL, 'Yes', 0, 2)"
  ))
  DBI::dbExecute(con_setup, paste(
    "CREATE TABLE eicu_coh.respiratorycharting(",
    "patientunitstayid INTEGER, respchartoffset INTEGER,",
    "respchartentryoffset INTEGER, respchartvaluelabel VARCHAR, respchartvalue VARCHAR)"
  ))
  DBI::dbExecute(con_setup, paste(
    "INSERT INTO eicu_coh.respiratorycharting VALUES",
    "(1, 7, 8, 'FiO2', '0.4'),",
    "(2, 9, 10, 'FiO2', '40%'),",
    "(3, 11, 12, 'FiO2', 'room air')"
  ))
  DBI::dbExecute(con_setup, paste(
    "CREATE TABLE eicu_coh.lab(",
    "patientunitstayid INTEGER, labresultoffset INTEGER, labname VARCHAR,",
    "labresult DOUBLE, labresulttext VARCHAR)"
  ))
  DBI::dbExecute(con_setup, paste(
    "INSERT INTO eicu_coh.lab VALUES",
    "(1, 1, 'FiO2', 0.4, '0.4'),",
    "(2, 2, 'FiO2', NULL, '40%'),",
    "(3, 3, 'FiO2', 7, '7')"
  ))
  DBI::dbDisconnect(con_setup, shutdown = TRUE)

  expect_true(run_raw_mods_duckdb("eicu", db_path = db_path, base_dir = tmp,
                                  show_progress = FALSE))

  con_check <- connect_duckdb(db_path)
  on.exit(if (DBI::dbIsValid(con_check)) DBI::dbDisconnect(con_check, shutdown = TRUE), add = TRUE)
  mods <- DBI::dbGetQuery(con_check, "
    SELECT table_name FROM information_schema.tables
    WHERE table_schema = 'eicu_coh' AND table_name LIKE '%_mod'
    ORDER BY table_name")$table_name
  expect_equal(mods, c("admissiondrug_mod", "infusiondrug_mod", "lab_mod",
                       "medication_mod", "respiratorycharting_mod"))

  adm <- DBI::dbGetQuery(con_check,
    "SELECT * FROM eicu_coh.admissiondrug_mod ORDER BY admissiondrugid")
  expect_equal(adm$drugunit, c(" mg ", ""))
  expect_equal(adm$mod_drugname, c("Vancomycin", "EmptyUnit"))
  expect_equal(adm$mod_drugdose, c(500, 100))
  expect_equal(adm$mod_drugdose_unit, c("mg", NA))
  expect_equal(adm$mod_drugstopoffset, adm$mod_drugstartoffset)

  inf <- DBI::dbGetQuery(con_check,
    "SELECT * FROM eicu_coh.infusiondrug_mod ORDER BY infusiondrugid")
  expect_equal(inf$mod_drugname, c("Norepinephrine", "Mystery drug"))
  expect_equal(inf$mod_drugrate, c(2.13, NA_real_))
  expect_equal(inf$mod_drugrate_unit, c("mcg/min", "unknown"))

  lookup <- DBI::dbGetQuery(con_check,
    "SELECT COUNT(*) AS n FROM eicu_coh.rendehr_eicu_hicl_drugname_lookup")$n
  expect_gt(lookup, 0)

  med <- DBI::dbGetQuery(con_check,
    "SELECT * FROM eicu_coh.medication_mod ORDER BY medicationid")
  expect_equal(nrow(med), 5L)
  expect_equal(med$mod_drugname, c("Aspirin", "OddDose", "ACETAMINOPHEN", NA, "Morphine"))
  expect_equal(med$mod_drugname_source,
               c("drugname", "drugname", "hicl_lookup", NA, "drugname"))
  expect_equal(med$mod_drugdose, c(81, NA, 1000, 5, 2))
  expect_equal(med$mod_drugdose_unit, c("mg", NA, "mg", "mg", NA))
  expect_equal(med$mod_drugdose_note,
               c(NA, "twice daily", NA, "5-10 mg", "2 10021"))
  expect_equal(med$drugordercancelled[5], "Yes")

  resp <- DBI::dbGetQuery(con_check,
    "SELECT patientunitstayid, respchartvalue FROM eicu_coh.respiratorycharting_mod ORDER BY patientunitstayid")
  expect_equal(resp$respchartvalue, c(40, 40, NA_real_))

  lab <- DBI::dbGetQuery(con_check,
    "SELECT patientunitstayid, labresult FROM eicu_coh.lab_mod ORDER BY patientunitstayid")
  expect_equal(as.numeric(lab$labresult), c(40, 40, NA_real_))
})

test_that("run_raw_mods_duckdb never reports success after a required failure", {
  skip_if_not_installed("duckdb")
  tmp <- file.path(tempdir(), paste0("duckdb_raw_mod_failure_", Sys.getpid()))
  dir.create(file.path(tmp, "sql", "eicu", "raw_mods"),
             recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("CREATE TABLE first_mod AS SELECT 1 AS x;",
             file.path(tmp, "sql", "eicu", "raw_mods", "01_first.sql"))
  writeLines("SELECT * FROM definitely_missing_table;",
             file.path(tmp, "sql", "eicu", "raw_mods", "02_fail.sql"))
  db <- file.path(tmp, "failure.duckdb")
  con <- connect_duckdb(db)
  DBI::dbExecute(con, "CREATE SCHEMA eicu_coh")
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(
    suppressWarnings(run_raw_mods_duckdb(
      "eicu", db_path = db, base_dir = tmp, skip_on_error = TRUE,
      show_progress = FALSE)),
    "Required raw modifications failed"
  )
})

test_that("DuckDB schema adapter uses corrected local schema types", {
  paths_probe <- ehr_paths(src_name = "eicu", base_dir = tempdir())
  skip_if_not(length(.duckdb_schema_sql_files(paths_probe$schema_sql_dir)) > 0,
              "bundled schema SQL not available")

  paths_eicu <- ehr_paths(src_name = "eicu", base_dir = tempdir())
  cfg_eicu <- load_src_cfg(paths_eicu$data_config_json, "eicu")
  sch_eicu <- .duckdb_schema_for_paths(paths_eicu, cfg_eicu)

  expect_equal(unname(sch_eicu$nursecharting["nursingchartid"]), "BIGINT")
  expect_equal(unname(sch_eicu$vitalperiodic["vitalperiodicid"]), "BIGINT")

  paths_miiv <- ehr_paths(src_name = "mimic_iv", base_dir = tempdir())
  cfg_miiv <- load_src_cfg(paths_miiv$data_config_json, "mimic_iv")
  sch_miiv <- .duckdb_schema_for_paths(paths_miiv, cfg_miiv)

  expect_equal(unname(sch_miiv$patients["dod"]), "TIMESTAMP")
  expect_equal(unname(sch_miiv$emar_detail["parent_field_ordinal"]), "VARCHAR")
})

test_that("DuckDB lazy CSV views and materialized tables use corrected schema types", {
  skip_if_not_installed("duckdb")

  tmp <- file.path(tempdir(), paste0("duckdb_schema_test_", Sys.getpid()))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # `driftcol` is deliberately absent from the schema below: it stands in for a
  # source-version drift column the bundled schema predates. It must survive
  # (passthrough), not be silently dropped.
  eicu_csv <- file.path(tmp, "nursecharting.csv")
  writeLines(c("nursingchartid,patientunitstayid,driftcol",
               "900719925474099,123,keepme"), eicu_csv)

  miiv_csv <- file.path(tmp, "patients.csv")
  writeLines(c("subject_id,dod", "1,2020-01-02"), miiv_csv)

  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  eicu_schema <- c(nursingchartid = "BIGINT", patientunitstayid = "INTEGER")
  eicu_expr <- .duckdb_csv_read_expr(con, eicu_csv)
  eicu_select <- .duckdb_typed_select_sql(con, eicu_schema,
                                          available_cols = .csv_header_cols(eicu_csv),
                                          alias = "src")
  DBI::dbExecute(con, sprintf(
    "CREATE VIEW nursecharting AS SELECT %s FROM %s AS src;",
    eicu_select, eicu_expr
  ))

  miiv_schema <- c(subject_id = "INTEGER", dod = "DATE")
  miiv_expr <- .duckdb_csv_read_expr(con, miiv_csv)
  miiv_select <- .duckdb_typed_select_sql(con, miiv_schema,
                                          available_cols = .csv_header_cols(miiv_csv),
                                          alias = "src")
  DBI::dbExecute(con, sprintf(
    "CREATE VIEW patients AS SELECT %s FROM %s AS src;",
    miiv_select, miiv_expr
  ))

  DBI::dbExecute(con, "CREATE TABLE nursecharting_mat AS SELECT * FROM nursecharting;")
  DBI::dbExecute(con, "CREATE TABLE patients_mat AS SELECT * FROM patients;")

  types <- DBI::dbGetQuery(con, "
    SELECT table_name, column_name, data_type
    FROM information_schema.columns
    WHERE table_name IN ('nursecharting', 'nursecharting_mat', 'patients', 'patients_mat')
  ")
  dtype <- function(tbl, col) types$data_type[types$table_name == tbl & types$column_name == col][[1]]

  expect_match(dtype("nursecharting", "nursingchartid"), "BIGINT|HUGEINT|INT64", ignore.case = TRUE)
  expect_match(dtype("nursecharting_mat", "nursingchartid"), "BIGINT|HUGEINT|INT64", ignore.case = TRUE)
  expect_match(dtype("patients", "dod"), "DATE", ignore.case = TRUE)
  expect_match(dtype("patients_mat", "dod"), "DATE", ignore.case = TRUE)

  # A column absent from the schema (source drift) must survive rather than be
  # silently dropped: present in both the view and the materialized table, value
  # preserved verbatim.
  expect_true("driftcol" %in% types$column_name[types$table_name == "nursecharting"])
  expect_true("driftcol" %in% types$column_name[types$table_name == "nursecharting_mat"])
  expect_equal(
    DBI::dbGetQuery(con, "SELECT driftcol FROM nursecharting_mat")$driftcol,
    "keepme")
})

test_that("DuckDB CSV readers use explicit per-file VARCHAR headers", {
  skip_if_not_installed("duckdb")

  tmp <- file.path(tempdir(), paste0("duckdb_csv_headers_", Sys.getpid()))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  first <- file.path(tmp, "part-1.csv")
  second <- file.path(tmp, "part-2.csv")
  writeLines(
    enc2utf8(c(
      "id,label,left_only",
      "1,Jos\u00e9,left-1",
      "2,\u674e,left-2"
    )),
    first, useBytes = TRUE
  )
  writeLines(
    enc2utf8(c(
      "label,id,right_only",
      "Zoe,3,right-3"
    )),
    second, useBytes = TRUE
  )

  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  headers <- lapply(c(first, second), .csv_header_cols)
  expr <- .duckdb_csv_read_expr(
    con, c(first, second), headers = headers
  )

  expect_false(grepl("read_csv_auto", expr, fixed = TRUE))
  # Malformed rows are skipped by default but kept in the rejects report
  # (loading_options(ignore_errors = TRUE)); with ignore_errors = FALSE the
  # reader is strict.
  expect_true(grepl("ignore_errors = true", expr, fixed = TRUE))
  expect_true(grepl("store_rejects", expr, fixed = TRUE))
  strict <- .duckdb_csv_read_expr(con, c(first, second), headers = headers,
                                  ignore_errors = FALSE)
  expect_false(grepl("ignore_errors", strict, fixed = TRUE))
  expect_false(grepl("all_varchar", expr, fixed = TRUE))
  expect_true(grepl("encoding = 'UTF-8'", expr, fixed = TRUE))
  expect_true(grepl("UNION ALL BY NAME", expr, fixed = TRUE))
  read_starts <- gregexpr("read_csv\\(", expr, perl = TRUE)[[1L]]
  expect_equal(sum(read_starts > 0L), 2L)

  out <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM %s AS src ORDER BY CAST(id AS INTEGER)",
      expr
    )
  )
  expect_identical(
    names(out), c("id", "label", "left_only", "right_only")
  )
  expect_identical(out$id, c("1", "2", "3"))
  expect_identical(out$label, enc2utf8(c("Jos\u00e9", "\u674e", "Zoe")))
  expect_identical(out$left_only, c("left-1", "left-2", NA_character_))
  expect_identical(out$right_only, c(NA_character_, NA_character_, "right-3"))

  duplicate <- file.path(tmp, "duplicate.csv")
  writeLines(c("id,ID", "1,2"), duplicate)
  expect_error(
    .duckdb_csv_read_expr(con, duplicate),
    "case-insensitive duplicate"
  )

  conflicting <- file.path(tmp, "conflicting.csv")
  writeLines(c("ID,extra", "4,x"), conflicting)
  expect_error(
    .duckdb_csv_read_expr(con, c(first, conflicting)),
    "conflicting case-insensitive"
  )
  expect_error(
    .duckdb_csv_read_expr(
      con, c(first, second), headers = list(headers[[1L]])
    ),
    "one character vector per file"
  )
})

test_that("window derivatives propagate cohort_index at each grain (visit -> icu -> admission)", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Patient-rooted spine covering the grain-propagation cases:
  #   hadm 10: two ICU stays, only stay 100 in cohort  -> admission rolls up TRUE,
  #            icu keeps the per-stay truth (100 TRUE, 101 FALSE). The two rows
  #            carry *inconsistent* admit/disch times to exercise the id-only
  #            GROUP BY: admission_windows must still collapse to ONE row for hadm 10.
  #   hadm 11: one ICU stay, not in cohort            -> admission FALSE.
  #   hadm 12: admission with NO ICU stay (stay NULL) -> excluded from icu_windows,
  #            still present (FALSE) in admission_windows.
  vw <- data.frame(
    subject_id  = c(1L, 1L, 2L, 3L),
    hadm_id     = c(10L, 10L, 11L, 12L),
    stay_id     = c(100L, 101L, 110L, NA_integer_),
    admittime   = as.POSIXct(c("2020-01-01 00:00", "2020-01-01 06:00",
                               "2020-02-01 00:00", "2020-03-01 00:00"), tz = "UTC"),
    dischtime   = as.POSIXct(c("2020-01-10 00:00", "2020-01-09 00:00",
                               "2020-02-05 00:00", "2020-03-04 00:00"), tz = "UTC"),
    icu_intime  = as.POSIXct(c("2020-01-02 00:00", "2020-01-03 00:00",
                               "2020-02-02 00:00", NA), tz = "UTC"),
    icu_outtime = as.POSIXct(c("2020-01-05 00:00", "2020-01-06 00:00",
                               "2020-02-04 00:00", NA), tz = "UTC"),
    icu_los_min = c(4320, 4320, 2880, NA_real_),
    cohort_index = c(TRUE, FALSE, FALSE, FALSE),
    # ICU-scoped cohort: only stay 100 was imported; the rest is context.
    in_scope = c(TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(con, "CREATE SCHEMA test_coh;")
  DBI::dbWriteTable(con, DBI::Id(schema = "test_coh", table = "visit_windows"), vw)

  .duckdb_create_window_derivatives(con, "test_coh", "mimic_iv_demo")

  icu <- DBI::dbGetQuery(con,
    "SELECT stay_id, cohort_index, in_scope FROM test_coh.icu_windows ORDER BY stay_id")
  adm <- DBI::dbGetQuery(con,
    "SELECT hadm_id, admittime, dischtime, cohort_index, in_scope
       FROM test_coh.admission_windows ORDER BY hadm_id")

  # icu_windows: stay-grain truth preserved; NULL-stay admission dropped.
  expect_equal(icu$stay_id, c(100L, 101L, 110L))
  expect_equal(as.logical(icu$cohort_index), c(TRUE, FALSE, FALSE))
  expect_equal(as.logical(icu$in_scope), c(TRUE, FALSE, FALSE))

  # admission_windows: exactly one row per hadm (id-only GROUP BY), including the
  # ICU-less hadm 12; cohort_index is the OR roll-up to the hospitalization grain.
  expect_equal(adm$hadm_id, c(10L, 11L, 12L))
  expect_equal(as.logical(adm$cohort_index), c(TRUE, FALSE, FALSE))
  expect_equal(as.logical(adm$in_scope), c(TRUE, FALSE, FALSE))

  # hadm 10's inconsistent times collapse to MIN(admit)/MAX(disch), not split rows.
  expect_equal(as.POSIXct(adm$admittime[adm$hadm_id == 10], tz = "UTC"),
               as.POSIXct("2020-01-01 00:00", tz = "UTC"))
  expect_equal(as.POSIXct(adm$dischtime[adm$hadm_id == 10], tz = "UTC"),
               as.POSIXct("2020-01-10 00:00", tz = "UTC"))
})

test_that("eICU admission offsets come from one reference unit stay, not mixed clocks", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Offsets are relative to each unit stay's own admit.
  #   hosp 1: real two-unit stay (units 141313/141314). MIN/MAX across units would
  #           give -446/1072 (span 1518) instead of the true 1085-minute stay.
  #   hosp 2: tied admit offsets with inconsistent discharge offsets; both columns
  #           must come from the same row (lowest patientunitstayid).
  pat <- data.frame(
    uniquepid = c("A", "A", "B", "B"),
    patienthealthsystemstayid = c(1L, 1L, 2L, 2L),
    patientunitstayid = c(141313L, 141314L, 202L, 201L),
    hospitaladmitoffset = c(-13, -446, -50, -50),
    hospitaldischargeoffset = c(1072, 639, 800, 900),
    unitdischargeoffset = c(300, 200, 400, 500)
  )
  expected <- data.frame(
    patienthealthsystemstayid = c(1L, 2L),
    hospitaladmitoffset = c(-13, -50),
    hospitaldischargeoffset = c(1072, 900)
  )
  get_adm <- function(ref) DBI::dbGetQuery(con, paste(
    "SELECT patienthealthsystemstayid, hospitaladmitoffset, hospitaldischargeoffset",
    "FROM", ref, "ORDER BY patienthealthsystemstayid"))

  # Source window SQL (Postgres, adapted to DuckDB).
  DBI::dbWriteTable(con, "patient", pat)
  sql_path <- system.file("sql", "eicu", "db_setup_sqls", "window_tables.sql",
                          package = "rendehR")
  if (!nzchar(sql_path)) {
    sql_path <- test_path("..", "..", "inst", "sql", "eicu", "db_setup_sqls",
                          "window_tables.sql")
  }
  raw_sql <- gsub("--[^\n]*", "", paste(readLines(sql_path), collapse = "\n"))
  for (s in strsplit(raw_sql, ";", fixed = TRUE)[[1]]) {
    if (nzchar(trimws(s))) DBI::dbExecute(con, adapt_sql_for_duckdb(s))
  }
  got <- get_adm("admission_windows")
  expect_equal(got$hospitaladmitoffset, expected$hospitaladmitoffset)
  expect_equal(got$hospitaldischargeoffset, expected$hospitaldischargeoffset)

  # Derivative builder over a cohort spine.
  vw <- DBI::dbGetQuery(con, "SELECT *, TRUE AS cohort_index, TRUE AS in_scope FROM visit_windows")
  DBI::dbExecute(con, "CREATE SCHEMA test_coh;")
  DBI::dbWriteTable(con, DBI::Id(schema = "test_coh", table = "visit_windows"), vw)
  .duckdb_create_window_derivatives(con, "test_coh", "eicu")
  got <- get_adm("test_coh.admission_windows")
  expect_equal(got$hospitaladmitoffset, expected$hospitaladmitoffset)
  expect_equal(got$hospitaldischargeoffset, expected$hospitaldischargeoffset)
})

# Selected-key reachability is tested through the canonical end-to-end
# materializers in test-cohort-hierarchy.R and test-cohort-materialization.R.

test_that("DuckDB run refuses an artifact without a success manifest", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("arrow")
  project <- tempfile("s004-invalid-artifact-")
  parquet_dir <- file.path(
    project, "data", "cohorts_data", "eicu_demo_parquet"
  )
  dir.create(parquet_dir, recursive = TRUE)
  arrow::write_parquet(
    data.frame(patientunitstayid = integer()),
    file.path(parquet_dir, "patient.parquet")
  )
  db_path <- file.path(project, "data", "duckdb", "eicu_demo.duckdb")
  expect_error(
    run_db_duckdb("eicu_demo", db_path = db_path, base_dir = project),
    "success manifest is missing"
  )
  expect_false(file.exists(db_path))
})

test_that("SQL-kind example cohorts resolve their packaged SQL chain", {
  cfg <- list(tables = list(patients = list(), admissions = list(),
                            icustays = list(), chartevents = list(),
                            labevents = list(), prescriptions = list(),
                            microbiologyevents = list(), inputevents = list(),
                            outputevents = list(), procedureevents = list()))
  paths <- list(helper_sql_dir = tempfile("no-helper-"))
  tbls <- .ehr_cohort_cache_tables("mimic_iv_demo", cfg, "sepsis3", paths)
  expect_true(length(tbls) > 0)
  expect_true(all(tbls %in% names(cfg$tables)))
})

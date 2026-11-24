test_that("build_match_clause: itm_itm produces IN clause", {
  sql <- build_match_clause("itm_itm", "itemid", "220045, 220046", NA)
  expect_match(sql, "IN \\(220045, 220046\\)")
  expect_match(sql, "src\\.itemid")
})

test_that("build_match_clause: rgx_itm without dict uses column IN list", {
  sql <- build_match_clause("rgx_itm", "label", "Heart Rate", NA)
  expect_match(sql, "IN \\('heart rate'\\)")
  expect_match(sql, "LOWER\\(TRIM\\(src\\.label\\)\\)")
})

test_that("build_match_clause: rgx_itm splits pipe-delimited values, lowercased", {
  sql <- build_match_clause("rgx_itm", "label", "PT|Ptt|PT - INR", NA)
  expect_match(sql, "IN \\('pt', 'ptt', 'pt - inr'\\)")
})

test_that("build_match_clause: rgx_itm with dict uses dict.label", {
  sql <- build_match_clause("rgx_itm", "itemid", "heart rate", "d_items")
  expect_match(sql, "LOWER\\(TRIM\\(dict\\.label\\)\\)")
})

test_that("build_match_clause: col_itm returns TRUE", {
  expect_equal(build_match_clause("col_itm", "val", "anything", NA), "TRUE")
})

test_that("build_match_clause: empty match_value selects all rows only for col_itm", {
  expect_equal(build_match_clause("col_itm", "val", "", NA), "TRUE")
  expect_equal(build_match_clause("col_itm", "val", NA, NA), "TRUE")
  for (cls in c("itm_itm", "rgx_itm", "rgx_exp_itm")) {
    expect_error(build_match_clause(cls, "itemid", "", NA), "Empty match_value")
    expect_error(build_match_clause(cls, "itemid", NA, NA), "Empty match_value")
    expect_error(build_match_clause(cls, "itemid", "  ", NA), "Empty match_value")
  }
})

test_that("build_match_clause: unknown class errors", {
  expect_error(build_match_clause("bad_class", "x", "1", NA), "Unknown")
})

test_that("build_time_diff: mimic_iv produces EPOCH EXTRACT expression", {
  sql <- build_time_diff("mimic_iv", "charttime", "cw.admittime")
  expect_match(sql, "EXTRACT\\(EPOCH")
  expect_match(sql, "charttime")
  expect_match(sql, "cw\\.admittime")
  expect_match(sql, "/ 60")
})

test_that("build_time_diff: mimic rounds to whole-minute numeric", {
  sql <- build_time_diff("mimic_iv", "charttime", "cw.admittime")
  expect_match(sql, "ROUND\\(")
  expect_match(sql, "::numeric")
})

test_that("build_time_diff: eicu is cast to numeric", {
  expect_match(build_time_diff("eicu", "observationoffset", "cw.icu_intime"),
              "::numeric")
})

test_that("build_time_diff: mimic_iv_demo same as mimic_iv", {
  expect_equal(
    build_time_diff("mimic_iv_demo", "charttime", "cw.admittime"),
    build_time_diff("mimic_iv",      "charttime", "cw.admittime")
  )
})

test_that("build_time_diff: eicu uses addition not subtraction", {
  sql <- build_time_diff("eicu", "observationoffset", "cw.icu_intime")
  expect_match(sql, "\\+")
  expect_false(grepl("EXTRACT|EPOCH", sql))
})

test_that("build_time_diff: unknown dataset errors", {
  expect_error(build_time_diff("unknown_db", "t", "origin"), "Unknown")
})

test_that("get_dataset_config returns registry-backed fields per source", {
  cfg <- get_dataset_config("mimic_iv")
  expect_equal(cfg$select_id_cols, c("subject_id", "hadm_id", "stay_id"))
  expect_equal(cfg$time_origin, "cw.admittime")
  expect_equal(cfg$icu_id_type, "bigint")
  expect_equal(cfg$schema, "mimic_iv_coh")
  expect_null(cfg$row_limit)

  eicu <- get_dataset_config("eicu")
  expect_equal(eicu$select_id_cols,
               c("uniquepid", "patienthealthsystemstayid", "patientunitstayid"))
  expect_equal(eicu$time_origin, "cw.icu_intime")
})

test_that("get_dataset_config derives demo schema and row_limit", {
  cfg <- get_dataset_config("mimic_iv_demo")
  expect_equal(cfg$schema, "mimic_iv_demo_coh")
  expect_equal(cfg$row_limit, 1000L)
  # Non-demo full source has no cap.
  expect_null(get_dataset_config("mimic_iv")$row_limit)
})

test_that("get_dataset_config errors on unknown dataset", {
  expect_error(get_dataset_config("nope"), "Unknown dataset")
})

test_that("get_id_cols returns default vs icu-grain columns", {
  # Admission-grain table -> default id cols.
  expect_equal(get_id_cols("mimic_iv", "admissions"),
               c("subject_id", "hadm_id"))
  # ICU-grain table -> includes stay_id.
  expect_equal(get_id_cols("mimic_iv", "chartevents"),
               c("subject_id", "hadm_id", "stay_id"))
  # eICU has a single grain.
  expect_equal(get_id_cols("eicu", "lab"), "patientunitstayid")
  # MIMIC-III prescriptions are admission-level: ward orders have no icustay_id.
  expect_equal(get_id_cols("mimic_iii", "prescriptions"), c("subject_id", "hadm_id"))
  expect_equal(get_id_cols("mimic_iii", "services"), c("subject_id", "hadm_id"))
  expect_equal(get_id_cols("mimic_iii", "inputevents_cv"), c("subject_id", "hadm_id", "icustay_id"))
  sql <- build_select_unified("prescriptions", "drug", "vancomycin", "dose_val_rx", "dose_unit_rx", "startdate",
                              conversion = "none", source = "mimic_iii", dict = NA, unit = "mg",
                              itm_class = "rgx_exp_itm", raw_schema = "s", concept_name = "antibiotics")
  expect_match(sql, "FROM s.admission_windows cw", fixed = TRUE)
  expect_false(grepl("icustay_id = src.icustay_id", sql, fixed = TRUE))
})

test_that("concept_unit_conversion_func: none returns base CASE expression", {
  sql <- concept_unit_conversion_func("none", "valuenum", NA, NA)
  expect_match(sql, "CAST")
  expect_match(sql, "NUMERIC")
  expect_false(grepl("\\* |/ |32", sql))
})

test_that("concept_unit_conversion_func: numeric parser handles commas and bad text safely", {
  sql <- concept_unit_conversion_func("none", "dose_val_rx", NA, NA)
  expect_match(sql, "REPLACE\\(REPLACE")
  expect_match(sql, ",")
  expect_match(sql, "ELSE NULL")
  expect_match(sql, "TRIM\\(src\\.dose_val_rx::TEXT\\)")
})

test_that("concept_unit_conversion_func: f_to_c wraps in fahrenheit conversion", {
  sql <- concept_unit_conversion_func("f_to_c", "temperature", "valueuom", "C")
  expect_match(sql, "32")
  expect_match(sql, "5\\.0/9\\.0")
})

test_that("concept_unit_conversion_func: multiply conversion", {
  sql <- concept_unit_conversion_func("* 1000", "valuenum", NA, "g/L")
  expect_match(sql, "\\* 1000")
})

test_that("concept_unit_conversion_func: divide conversion", {
  sql <- concept_unit_conversion_func("/ 10", "valuenum", NA, "g/dL")
  expect_match(sql, "/ 10")
})

test_that("concept_unit_conversion_func: *N and /N refuse a unit column", {
  expect_error(concept_unit_conversion_func("* 1000", "valuenum", "valueuom", "g/L", "x"),
               "conversion = 'map'")
  expect_error(concept_unit_conversion_func("/ 10", "valuenum", "valueuom", "g/dL", "x"),
               "conversion = 'map'")
})

test_that("concept_unit_conversion_func: f_to_c converts only Fahrenheit units", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  units <- c("Deg. F", "?F", "\u00b0F", " f ", "C", "garbage", NA)
  rows <- paste(sprintf("('98.6', %s)", ifelse(is.na(units), "NULL",
                                               paste0("'", units, "'"))), collapse = ", ")
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(%s AS DOUBLE) AS v FROM (VALUES %s) AS src(val, unit)",
    concept_unit_conversion_func("f_to_c", "val", "unit", "C"), rows))$v
  expect_equal(got, c(37, 37, 37, 37, 98.6, NA, 98.6), tolerance = 1e-6)
})

test_that("concept_unit_conversion_func: fio2_fraction puts every value on the percent scale", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  vals <- c("0.15", "0.2", "0.21", "1.0", "1.5", "21", "100", "0")
  rows <- paste(sprintf("('%s')", vals), collapse = ", ")
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(%s AS DOUBLE) AS v FROM (VALUES %s) AS src(v)",
    concept_unit_conversion_func("fio2_fraction", "v", NA, "%"), rows))$v
  expect_equal(got, c(15, 20, 21, 100, NA, 21, 100, 0))
})

test_that("concept_unit_conversion_func: ca_ionized_by_value reads the unit from the value", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  vals <- c("1.12", "2", "2.01", "3.49", "3.5", "4.5", "abc")
  rows <- paste(sprintf("('%s', 'mg/dL')", vals), collapse = ", ")
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(%s AS DOUBLE) AS v FROM (VALUES %s) AS src(v, u)",
    concept_unit_conversion_func("ca_ionized_by_value", "v", "u", "mmol/L"), rows))$v
  expect_equal(got, c(1.12, 2, NA, NA, 3.5 * 0.2495, 4.5 * 0.2495, NA), tolerance = 1e-9)
})

test_that("concept_unit_conversion_func: dipstick_grade stores urine grades as 0-3", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  vals <- c("NEG", " neg ", "SM", "1+", "MOD", "Moderate", "LG", "3+", "1.5", "___", NA)
  rows <- paste(sprintf("(%s, 'mg/dL')", ifelse(is.na(vals), "NULL", paste0("'", vals, "'"))),
                collapse = ", ")
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(%s AS DOUBLE) AS v FROM (VALUES %s) AS src(v, u)",
    concept_unit_conversion_func("dipstick_grade", "v", "u", "grade"), rows))$v
  expect_equal(got, c(0, 0, 1, 1, 2, 2, 3, 3, NA, NA, NA))
})

test_that("every shipped concept builds its unit conversion", {
  cfg <- utils::read.csv(ehr_config_path("rendehr_concepts_config.csv"),
                         stringsAsFactors = FALSE, na.strings = c("", "NA"))
  for (i in seq_len(nrow(cfg)))
    expect_type(concept_unit_conversion_func(cfg$conversion[i], cfg$val_var[i],
                                             cfg$unit_var[i], cfg$unit[i],
                                             cfg$concept_name[i]), "character")
})

test_that("concept_unit_conversion_func: unknown conversion errors", {
  expect_error(
    concept_unit_conversion_func("log_transform", "v", "u", "x"),
    "Unknown conversion"
  )
})

test_that("concept_unit_conversion_func: map conversion reads the unit map by concept", {
  sql <- concept_unit_conversion_func(
    "map", "valuenum", "valueuom", "mg/L", concept_name = "c_reactive_protein"
  )
  expect_match(sql, "CASE")
  expect_match(sql, "IS NULL")
})

test_that("concept_unit_conversion_func: map conversion requires concept_name", {
  expect_error(
    concept_unit_conversion_func("map", "valuenum", "valueuom", "mg/dL"),
    "concept_name"
  )
})

test_that("build_id_select: joins spine columns with the cw. prefix", {
  sql <- build_id_select(c("subject_id", "hadm_id", "stay_id"))
  expect_match(sql, "cw\\.subject_id")
  expect_match(sql, "cw\\.hadm_id")
  expect_match(sql, "cw\\.stay_id")
})

test_that("build_join_condition: ANDs spine-to-source equality per key", {
  sql <- build_join_condition(c("subject_id", "hadm_id"))
  expect_match(sql, "cw\\.subject_id = src\\.subject_id")
  expect_match(sql, "AND")
  expect_match(sql, "cw\\.hadm_id = src\\.hadm_id")
})

test_that("build_join_condition: errors with no join keys", {
  expect_error(build_join_condition(character(0)), "No join keys")
})

test_that("build_icu_time_diff: mimic_iv uses EPOCH extraction relative to icu_intime", {
  sql <- build_icu_time_diff("mimic_iv", "charttime")
  expect_match(sql, "EXTRACT\\(EPOCH")
  expect_match(sql, "cw\\.icu_intime")
})

test_that("build_icu_time_diff: eicu is a direct numeric cast", {
  sql <- build_icu_time_diff("eicu", "observationoffset")
  expect_match(sql, "::numeric")
  expect_false(grepl("EXTRACT|EPOCH", sql))
})

# ---------------------------------------------------------------------------
# build_select_unified() / build_atomic_concept_unified_sql(): assert on the
# generated SQL string only -- no database execution, consistent with every
# other test in this file.
# ---------------------------------------------------------------------------

test_that("build_select_unified: ICU-grain itm_itm concept selects from icu_windows", {
  sql <- build_select_unified(
    table_name = "chartevents", sub_var = "itemid", match_value = "220045",
    val_var = "valuenum", unit_var = "valueuom", index_var = "charttime",
    conversion = "none", source = "mimic_iv", dict = NA, unit = "bpm",
    itm_class = "itm_itm", raw_schema = "mimic_iv_demo_coh"
  )
  expect_match(sql, "FROM mimic_iv_demo_coh\\.icu_windows cw")
  expect_match(sql, "INNER JOIN mimic_iv_demo_coh\\.chartevents src")
  expect_match(sql, "src\\.itemid IN \\(220045\\)")
  expect_match(sql, "'icu' AS event_level")
})

test_that("build_select_unified: a concept without a unit stores NULL, not the text 'NA'", {
  args <- list(table_name = "chartevents", sub_var = "itemid", match_value = "220045",
               val_var = "valuenum", unit_var = "valueuom", index_var = "charttime",
               conversion = "none", source = "mimic_iv", dict = NA,
               itm_class = "itm_itm", raw_schema = "mimic_iv_demo_coh")
  for (u in list(NA_character_, "", "NA"))
    expect_match(do.call(build_select_unified, c(args, unit = u)), "NULL::text AS unit", fixed = TRUE)
  expect_match(do.call(build_select_unified, c(args, unit = "bpm")), "'bpm' AS unit", fixed = TRUE)
})

test_that("build_select_unified: the generated SQL actually executes and produces correct values", {
  # Upgrades the string-only assertion above with genuine execution against
  # DuckDB (the package's own established Postgres-syntax-compatible test
  # backend for cohort/window SQL) and a minimal but complete icu_windows
  # fixture, proving the generated SELECT is not merely plausible-looking
  # but runs and computes the right numbers.
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS mimic_iv_demo_coh")
  DBI::dbExecute(con, paste(
    "CREATE TABLE mimic_iv_demo_coh.icu_windows",
    "(subject_id BIGINT, hadm_id BIGINT, stay_id BIGINT,",
    " admittime TIMESTAMP, icu_intime TIMESTAMP, icu_los_min DOUBLE)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO mimic_iv_demo_coh.icu_windows VALUES",
    "(1, 10, 100, TIMESTAMP '2019-12-31 12:00:00',",
    " TIMESTAMP '2020-01-01 00:00:00', 500)"
  ))
  DBI::dbExecute(con, paste(
    "CREATE TABLE mimic_iv_demo_coh.chartevents",
    "(subject_id BIGINT, hadm_id BIGINT, stay_id BIGINT, itemid INT,",
    " charttime TIMESTAMP, valuenum DOUBLE, valueuom TEXT)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO mimic_iv_demo_coh.chartevents VALUES",
    "(1, 10, 100, 220045, TIMESTAMP '2020-01-01 00:10:00', 88, 'bpm')"
  ))

  sql <- build_select_unified(
    table_name = "chartevents", sub_var = "itemid", match_value = "220045",
    val_var = "valuenum", unit_var = "valueuom", index_var = "charttime",
    conversion = "none", source = "mimic_iv", dict = NA, unit = "bpm",
    itm_class = "itm_itm", raw_schema = "mimic_iv_demo_coh"
  )
  row <- DBI::dbGetQuery(con, sql)

  expect_identical(nrow(row), 1L)
  expect_equal(row$time_since_hadm_min, 730)  # hospital admit 12:00 the day before -> 00:10
  expect_equal(row$time_since_icu_min, 10)    # ICU admit 00:00 -> 00:10
  expect_equal(row$icu_los_min, 500)
  expect_equal(row$value, 88)
  expect_identical(row$unit, "bpm")
  expect_identical(row$event_level, "icu")
  expect_identical(row$native_id_level, "icu_stay")
})

test_that("build_select_unified: a col_itm concept keeps only rows where its column is filled", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA eicu_demo_coh")
  DBI::dbExecute(con, paste(
    "CREATE TABLE eicu_demo_coh.icu_windows AS SELECT '1' AS uniquepid,",
    "1 AS patienthealthsystemstayid, 1 AS patientunitstayid,",
    "0 AS icu_intime, 600 AS icu_los_min"))
  DBI::dbExecute(con, paste(
    "CREATE TABLE eicu_demo_coh.vitalperiodic AS SELECT * FROM (VALUES",
    "(1, 5, 12.0, 80.0), (1, 10, NULL, 82.0), (1, 15, 9.0, NULL)) t(patientunitstayid, observationoffset, icp, heartrate)"))
  sql <- build_select_unified(
    table_name = "vitalperiodic", sub_var = NA, match_value = NA,
    val_var = "icp", unit_var = NA, index_var = "observationoffset",
    conversion = "none", source = "eicu", dict = NA, unit = "mmHg",
    itm_class = "col_itm", raw_schema = "eicu_demo_coh", engine = "duckdb"
  )
  got <- DBI::dbGetQuery(con, paste(sql, "ORDER BY time_since_icu_min"))
  expect_equal(got$value, c(12, 9))
})

test_that("build_select_unified: hospital-grain table selects from admission_windows with a NULL ICU id", {
  sql <- build_select_unified(
    table_name = "admissions", sub_var = NA, match_value = NA,
    val_var = "hospital_expire_flag", unit_var = NA, index_var = "admittime",
    conversion = "none", source = "mimic_iv", dict = NA, unit = "flag",
    itm_class = "col_itm", raw_schema = "mimic_iv_demo_coh"
  )
  expect_match(sql, "FROM mimic_iv_demo_coh\\.admission_windows cw")
  expect_match(sql, "NULL::bigint AS stay_id")
  expect_match(sql, "'hospital' AS event_level")
})

test_that("build_select_unified: duration concepts require end_var", {
  expect_error(
    build_select_unified(
      table_name = "inputevents", sub_var = "itemid", match_value = "1",
      val_var = "amount", unit_var = "amountuom", index_var = "starttime",
      conversion = "none", source = "mimic_iv", dict = NA, unit = "mL",
      itm_class = "itm_itm", raw_schema = "mimic_iv_demo_coh", typ = "dur"
    ),
    "end_var"
  )
})

test_that("build_atomic_concept_unified_sql: wraps the union in DROP/CREATE TABLE for the concept name", {
  df <- data.frame(
    concept_name = c("heart_rate", "heart_rate"),
    table_name   = c("chartevents", "chartevents"),
    class        = c("itm_itm", "itm_itm"),
    sub_var      = c("itemid", "itemid"),
    match_value  = c("220045", "220045"),
    val_var      = c("valuenum", "valuenum"),
    unit_var     = c("valueuom", "valueuom"),
    index_var    = c("charttime", "charttime"),
    end_var      = c(NA, NA),
    conversion   = c("none", "none"),
    dict         = c(NA, NA),
    unit         = c("bpm", "bpm"),
    typ          = c("ts", "ts"),
    time_precision = c("datetime", "datetime"),
    stringsAsFactors = FALSE
  )
  sql <- build_atomic_concept_unified_sql(
    df, raw_schema = "mimic_iv_demo_coh",
    concepts_schema = "mimic_iv_demo_coh_concepts", src_name = "mimic_iv_demo"
  )
  expect_match(sql, "DROP TABLE IF EXISTS mimic_iv_demo_coh_concepts\\.heart_rate")
  expect_match(sql, "CREATE TABLE mimic_iv_demo_coh_concepts\\.heart_rate AS")
  expect_match(sql, "UNION ALL")
})

# ---------------------------------------------------------------------------
# run_atomic_concepts_unified(): executes against DuckDB. The SQL-generation
# step (build_atomic_concept_unified_sql()) is mocked with trivial, portable
# SQL so this test targets only the per-concept execution/result-tracking
# behavior, not the Postgres-flavored generated SQL covered by the
# string-assertion tests above.
# ---------------------------------------------------------------------------

test_that("run_atomic_concepts_unified: tracks per-concept success and continues past a failure", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = c("mimic_iv", "mimic_iv"),
      concept_name = c("good_concept", "bad_concept"),
      itm_class    = c("col_itm", "col_itm"),
      typ          = c("ts", "ts"),
      stringsAsFactors = FALSE
    ),
    cfg
  )

  sql_out <- file.path(tempdir(), paste0("s007_test_sql_", as.integer(Sys.time())))
  dir.create(sql_out, recursive = TRUE, showWarnings = FALSE)

  local_mocked_bindings(
    build_atomic_concept_unified_sql = function(df, raw_schema, concepts_schema, src_name, engine = "postgres", ...) {
      if (identical(df$concept_name[1L], "good_concept")) {
        "CREATE TABLE good_concept AS SELECT 1 AS value;"
      } else {
        "CREATE TABLE bad_concept AS SELECT * FROM table_that_does_not_exist;"
      }
    }
  )

  results <- run_atomic_concepts_unified(
    con                 = con,
    concept_config_path = cfg,
    src_name            = "mimic_iv_demo",
    schema_names        = list(raw = "main", concepts = "main"),
    typ                 = "ts",
    sql_out_dir         = sql_out
  )

  expect_true(results[["good_concept"]])
  expect_false(results[["bad_concept"]])
  expect_true(DBI::dbExistsTable(con, "good_concept"))
  expect_false(DBI::dbExistsTable(con, "bad_concept"))
  expect_true(file.exists(file.path(sql_out, "good_concept.sql")))
})

test_that("run_atomic_concepts_unified: returns invisible(logical(0)) when no concept matches", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(source = "eicu", concept_name = "x", itm_class = "col_itm",
              typ = "ts", stringsAsFactors = FALSE),
    cfg
  )
  sql_out <- file.path(tempdir(), paste0("s007_test_sql_empty_", as.integer(Sys.time())))

  results <- run_atomic_concepts_unified(
    con                 = NULL,
    concept_config_path = cfg,
    src_name            = "mimic_iv_demo",
    schema_names        = list(raw = "main", concepts = "main"),
    typ                 = "ts",
    sql_out_dir         = sql_out
  )
  expect_identical(results, logical(0))
})

test_that("run_atomic_concepts_unified: user_concepts replaces only matching concepts and keeps multi-row ones", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = "mimic_iv",
      concept_name = c("temperature", "temperature", "heart_rate", "heart_rate"),
      match_value  = c("223762", "223761", "220045", "220046"),
      itm_class    = "itm_itm",
      typ          = "ts",
      stringsAsFactors = FALSE
    ),
    cfg
  )
  user <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = "mimic_iv",
      concept_name = c("heart_rate", "heart_rate", "user_only"),
      match_value  = c("999001", "999002", "999003"),
      itm_class    = "itm_itm",
      typ          = "ts",
      stringsAsFactors = FALSE
    ),
    user
  )

  local_mocked_bindings(
    build_atomic_concept_unified_sql = function(df, raw_schema, concepts_schema, src_name, engine = "postgres", ...) {
      paste(sort(df$match_value), collapse = ",")
    }
  )

  sql <- run_atomic_concepts_unified(
    concept_config_path = cfg,
    src_name            = "mimic_iv_demo",
    schema_names        = list(raw = "main", concepts = "main"),
    typ                 = "ts",
    sql_out_dir         = tempfile(),
    user_concepts       = user,
    dry_run             = TRUE
  )

  # Untouched base concept keeps both rows; overridden concept takes all of
  # the user's rows and none of the base rows; user-only concept is added.
  expect_identical(sql[["temperature"]], "223761,223762")
  expect_identical(sql[["heart_rate"]],  "999001,999002")
  expect_identical(sql[["user_only"]],   "999003")
})

test_that("run_atomic_concepts_unified: a numeric-only user file binds to a text config", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = "mimic_iv",
      concept_name = c("antibiotics", "heart_rate"),
      match_value  = c("amoxicillin|cefazolin", "220045"),
      itm_class    = c("rgx_itm", "itm_itm"),
      typ          = "ts",
      min_value    = c(NA, 0),
      stringsAsFactors = FALSE
    ),
    cfg
  )
  user <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = "mimic_iv",
      concept_name = "heart_rate",
      match_value  = 999001,
      itm_class    = "itm_itm",
      typ          = "ts",
      min_value    = NA,
      stringsAsFactors = FALSE
    ),
    user
  )
  local_mocked_bindings(
    build_atomic_concept_unified_sql = function(df, ...) {
      paste(sort(as.character(df$match_value)), collapse = ",")
    }
  )
  sql <- run_atomic_concepts_unified(
    concept_config_path = cfg,
    src_name            = "mimic_iv_demo",
    schema_names        = list(raw = "main", concepts = "main"),
    typ                 = "ts",
    sql_out_dir         = tempfile(),
    user_concepts       = user,
    dry_run             = TRUE
  )
  expect_identical(sql[["heart_rate"]], "999001")
  expect_identical(sql[["antibiotics"]], "amoxicillin|cefazolin")
})

test_that(".ehr_align_concept_cols keeps values and types consistent", {
  base  <- data.frame(a = c("x", "y"), b = c(1L, 2L), c = c(NA, NA), d = c(1.5, NA))
  extra <- data.frame(a = 3, b = 4.5, c = "z", d = NA)
  out <- .ehr_align_concept_cols(base, extra)
  expect_type(out$base$a, "character"); expect_identical(out$extra$a, "3")
  expect_type(out$base$b, "double");    expect_identical(out$extra$b, 4.5)
  expect_type(out$base$c, "character"); expect_identical(out$extra$c, "z")
  expect_type(out$extra$d, "double")
  expect_silent(dplyr::bind_rows(out$base, out$extra))
})

test_that("concept_unit_conversion_func: unit matching ignores case and padding", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  eval_sql <- function(expr, unit) {
    DBI::dbGetQuery(con, sprintf(
      "SELECT %s AS v FROM (SELECT '100' AS val, %s AS unit) src",
      expr, DBI::dbQuoteString(con, unit)))$v
  }
  ck <- concept_unit_conversion_func("map", "val", "unit", "U/L",
                                     concept_name = "creatine_kinase")
  expect_equal(eval_sql(ck, "  units/l "), 100)
  expect_equal(eval_sql(ck, "U/L   "), 100)
  expect_true(is.na(eval_sql(ck, "mg/dl")))
  f <- concept_unit_conversion_func("f_to_c", "val", "unit", "C")
  expect_equal(eval_sql(f, " c "), 100)
})

test_that("concept_unit_conversion_func: numeric parser keeps every well-formed number", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  vals <- c("5", "-5", "1,234", "5%", ".5", "+5", "1e3", "-2.5E-2", "5.",
            "<0.5", ">100", "abc", ".", "+", "1e40")
  rows <- paste(sprintf("('%s')", vals), collapse = ", ")
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(%s AS DOUBLE) AS v FROM (VALUES %s) AS src(v)",
    concept_unit_conversion_func("none", "v", NA, NA), rows))$v
  expect_equal(got, c(5, -5, 1234, 5, 0.5, 5, 1000, -0.025, 5,
                      NA, NA, NA, NA, NA, NA))
})

test_that("concept configs with the old misspelled column names still load", {
  old <- data.frame(concept_name = "a", requred_dedup = TRUE, basos_asbolute = NA)
  new <- .ehr_normalize_concept_cols(old)
  expect_named(new, c("concept_name", "required_dedup", "basos_absolute"))
  expect_true(new$required_dedup)
  both <- data.frame(required_dedup = FALSE, requred_dedup = TRUE)
  expect_identical(names(.ehr_normalize_concept_cols(both)), "required_dedup")
  expect_false(.ehr_normalize_concept_cols(both)$required_dedup)
  shipped <- names(readr::read_csv(ehr_config_path("rendehr_concepts_config.csv"),
                                   n_max = 0, show_col_types = FALSE))
  expect_true(all(c("required_dedup", "basos_absolute") %in% shipped))
  expect_false(any(c("requred_dedup", "basos_asbolute") %in% shipped))
})

test_that("the unit-conversion map has one row per concept and unit, all reachable", {
  map <- .load_unit_conversion_map()
  expect_false(anyDuplicated(paste(map$concept_name, tolower(map$source_unit))) > 0)
  cfg <- c(readr::read_csv(ehr_config_path("rendehr_concepts_config.csv"),
                           show_col_types = FALSE)$concept_name,
           readr::read_csv(ehr_config_path("rendehr_duration_concepts_config.csv"),
                           show_col_types = FALSE)$concept_name)
  expect_identical(setdiff(unique(map$concept_name), cfg), character(0))
})

test_that("antibiotics are exposure-only: every row is 1, whatever the dose or unit", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(%s AS DOUBLE) AS v FROM (VALUES ('500', 'mg'), ('2', 'g'), ('1', 'dose'), ('', NULL), ('PYXIS', 'TAB')) AS src(v, u)",
    concept_unit_conversion_func("exposure", "v", "u", NA_character_, "vancomycin_exp")))$v
  expect_equal(got, rep(1, 5))
  dur <- readr::read_csv(ehr_config_path("rendehr_duration_concepts_config.csv"),
                         show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  abx <- dur[dur$concept_name %in% c("antibiotics_exp", "vancomycin_exp", "cefazolin_exp", "piperacillin_exp"), ]
  expect_true(all(abx$conversion == "exposure"))
  expect_true(all(is.na(abx$unit) | abx$unit == ""))
  map <- .load_unit_conversion_map()
  expect_false("antibiotics_exp" %in% map$concept_name)
})

test_that("a unit-map row with an empty factor excludes that unit on purpose", {
  skip_if_not_installed("duckdb")
  map <- .load_unit_conversion_map()
  excl <- map[map$concept_name == "lymphocytes" & .unit_map_excluded(map$factor), ]
  expect_true(all(c("TH/uL", "1000/uL") %in% excl$source_unit))
  # "X 10" is a percentage label used by one eICU hospital (same distribution as
  # "%"), and "proportion" occurs in no source, so it has no row.
  expect_false(any(c("X 10", "proportion") %in% excl$source_unit))
  expect_false("proportion" %in% map$source_unit[map$concept_name %in% c("lymphocytes", "eosinophils")])
  sql <- concept_unit_conversion_func("map", "v", "u", "%", "lymphocytes")
  expect_match(sql, "LOWER(TRIM('TH/uL')) THEN NULL", fixed = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(%s AS DOUBLE) AS v FROM (VALUES ('12', '%%'), ('0.12', 'proportion'), ('1.2', 'TH/uL'), ('3', 'x 10')) AS src(v, u)",
    sql))$v
  expect_equal(got, c(12, NA, NA, 3))
  key <- .dedup_unit_key_sql("map", "u", "%", "lymphocytes")
  expect_false(grepl("'th/ul'", key, fixed = TRUE))
})

test_that("rgx_itm match values are literal source values, not regex patterns", {
  # build_match_clause() matches rgx_itm values exactly (IN list), so an
  # escaped or anchored pattern such as '^PT$' or 'O2 Sat \\(%\\)' never matches.
  cfg <- utils::read.csv(system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"),
                         stringsAsFactors = FALSE)
  vals <- unlist(strsplit(cfg$match_value[cfg$itm_class == "rgx_itm"], "|", fixed = TRUE))
  expect_false(any(grepl("\\\\|^\\^|\\$$", vals)), info = paste(vals[grepl("\\\\|^\\^|\\$$", vals)], collapse = ", "))
  ei <- cfg[cfg$itm_class == "rgx_itm" & cfg$source == "eicu", ]
  expect_equal(ei$match_value[ei$concept_name == "basos"], "-basos")
  expect_equal(ei$match_value[ei$concept_name == "band"], "-bands")
  expect_equal(ei$match_value[ei$concept_name == "pt"], "PT")
})

test_that("run_atomic_concepts_unified: unknown itm_class rows are skipped with a warning naming them", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = c("mimic_iv", "mimic_iv", "mimic_iv", "mimic_iv", "eicu"),
      concept_name = c("heart_rate", "heart_rate", "my_lab", "temperature", "other"),
      table_name   = c("chartevents", "chartevents", "labevents", "chartevents", "lab"),
      match_value  = c("220045", "220046", "50912", "223762", "x"),
      itm_class    = c("itm_itm", "itm_itme", NA, " ITM_ITM ", "bogus"),
      typ          = "ts",
      stringsAsFactors = FALSE
    ),
    cfg
  )
  local_mocked_bindings(
    build_atomic_concept_unified_sql = function(df, ...) paste(sort(df$match_value), collapse = ",")
  )

  expect_warning(
    sql <- run_atomic_concepts_unified(
      concept_config_path = cfg,
      src_name            = "mimic_iv_demo",
      schema_names        = list(raw = "main", concepts = "main"),
      typ                 = "ts",
      sql_out_dir         = tempfile(),
      dry_run             = TRUE
    ),
    "2 problem\\(s\\) in the concept config for source 'mimic_iv'.*heart_rate / chartevents: unknown itm_class 'itm_itme'.*my_lab / labevents: unknown itm_class <missing>"
  )
  # The bad row of heart_rate is dropped, its good row kept; a concept with
  # no valid row is not built; case and padding of a valid class are ignored;
  # the eICU row's bad class is not reported for a MIMIC-IV run.
  expect_identical(sql[["heart_rate"]], "220045")
  expect_false("my_lab" %in% names(sql))
  expect_identical(sql[["temperature"]], "223762")
})

test_that("run_atomic_concepts_unified: no warning when every itm_class is known", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(source = "mimic_iv", concept_name = "heart_rate", table_name = "chartevents",
               match_value = "220045", itm_class = "itm_itm", typ = "ts"),
    cfg
  )
  local_mocked_bindings(
    build_atomic_concept_unified_sql = function(df, ...) "SELECT 1"
  )
  expect_no_warning(run_atomic_concepts_unified(
    concept_config_path = cfg, src_name = "mimic_iv_demo",
    schema_names = list(raw = "main", concepts = "main"), typ = "ts",
    sql_out_dir = tempfile(), dry_run = TRUE
  ))
})

test_that("run_atomic_concepts_unified: a concept built without skipped rows is marked in its OK line", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = "mimic_iv",
      concept_name = c("heart_rate", "heart_rate", "temperature"),
      table_name   = "chartevents",
      match_value  = c("220045", "220046", "223762"),
      itm_class    = c("itm_itm", "itm_itme", "itm_itm"),
      typ          = "ts",
      stringsAsFactors = FALSE
    ),
    cfg
  )
  local_mocked_bindings(
    build_atomic_concept_unified_sql = function(df, ...)
      sprintf("DROP TABLE IF EXISTS %1$s; CREATE TABLE %1$s AS SELECT 1 AS v;", df$concept_name[1])
  )
  msgs <- character(0)
  res <- suppressWarnings(withCallingHandlers(
    run_atomic_concepts_unified(
      con = con, concept_config_path = cfg, src_name = "mimic_iv_demo",
      schema_names = list(raw = "main", concepts = "main"), typ = "ts",
      sql_out_dir = tempfile(), engine = "duckdb"
    ),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  ))
  expect_true(all(res))
  expect_true(any(grepl("^OK: heart_rate \\([0-9.]+ s\\) \\(1 config row\\(s\\) skipped: unknown itm_class\\)", msgs)))
  expect_true(any(grepl("^OK: temperature \\([0-9.]+ s\\)\n$", msgs)))
})

test_that("run_atomic_concepts_unified: stops on a user_concepts file that does not exist", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(source = "mimic_iv", concept_name = "heart_rate", table_name = "chartevents",
               match_value = "220045", itm_class = "itm_itm", typ = "ts"),
    cfg
  )
  missing <- file.path(tempdir(), "no_such_user_concepts.csv")
  sql_out <- tempfile()
  expect_error(
    run_atomic_concepts_unified(
      concept_config_path = cfg, src_name = "mimic_iv_demo",
      schema_names = list(raw = "main", concepts = "main"), typ = "ts",
      sql_out_dir = sql_out, user_concepts = missing, dry_run = TRUE
    ),
    "`user_concepts` file not found"
  )
  # Stops before any SQL is generated or written.
  expect_false(dir.exists(sql_out))
})

test_that("validate_concept_config: the shipped concept configurations are valid", {
  for (f in c("rendehr_concepts_config.csv", "rendehr_duration_concepts_config.csv")) {
    p <- validate_concept_config(system.file("extdata", f, package = "rendehR"))
    expect_identical(nrow(p), 0L, info = paste(f, paste(p$concept_name, p$message, collapse = "; ")))
  }
})

test_that("validate_concept_config: flags unknown classes and empty match_value except for col_itm", {
  cfg <- data.frame(
    source       = c("mimic_iv", "mimic_iv", "mimic_iv", "mimic_iv", "eicu", "eicu", "mimic_iv"),
    concept_name = c("a", "b", "c", "d", "e", "f", "g"),
    table_name   = "t",
    itm_class    = c("itm_itm", "itm_itm", "rgx_itm", "rgx_exp_itm", "col_itm", "bogus", " ITM_ITM "),
    match_value  = c("220045", NA, "  ", "", NA, "x", "1"),
    stringsAsFactors = FALSE
  )
  p <- validate_concept_config(cfg)
  expect_identical(p$row, c(2L, 3L, 4L, 6L))
  expect_identical(p$rule, c(rep("empty_match_value", 3), "unknown_itm_class"))
  expect_true(all(p$severity == "skip"))
  expect_match(p$message[1], "itm_itm needs item ids")
  expect_match(p$message[4], "unknown itm_class 'bogus'")
  # src_name keeps row positions of the full config and checks only its family.
  p_iv <- validate_concept_config(cfg, src_name = "mimic_iv_demo")
  expect_identical(p_iv$row, c(2L, 3L, 4L))
  expect_identical(nrow(validate_concept_config(cfg[c(1, 5, 7), ])), 0L)
})

test_that("run_atomic_concepts_unified: an itm_itm row with an empty match_value is skipped, not read as the whole table", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = "mimic_iv",
      concept_name = c("heart_rate", "heart_rate", "my_hr"),
      table_name   = "chartevents",
      match_value  = c("220045", NA, NA),
      itm_class    = "itm_itm",
      typ          = "ts",
      stringsAsFactors = FALSE
    ),
    cfg
  )
  local_mocked_bindings(
    build_atomic_concept_unified_sql = function(df, ...) paste(sort(df$match_value), collapse = ",")
  )
  expect_warning(
    sql <- run_atomic_concepts_unified(
      concept_config_path = cfg, src_name = "mimic_iv_demo",
      schema_names = list(raw = "main", concepts = "main"), typ = "ts",
      sql_out_dir = tempfile(), dry_run = TRUE
    ),
    "2 problem\\(s\\).*heart_rate / chartevents: empty match_value.*my_hr / chartevents: empty match_value"
  )
  expect_identical(sql[["heart_rate"]], "220045")
  expect_false("my_hr" %in% names(sql))
})

# Chart-copy deduplication (rm_duplicates): option forwarding, rules-file
# validation, and clean-fixture execution of the generated SQL.

# ---------------------------------------------------------------------------
# Option forwarding: omitted -> TRUE, explicit FALSE survives every layer
# ---------------------------------------------------------------------------

test_that("concept_options keeps only an explicit rm_duplicates and validates it", {
  expect_false("rm_duplicates" %in% names(concept_options()))
  expect_identical(concept_options(rm_duplicates = FALSE), list(rm_duplicates = FALSE))
  expect_identical(concept_options(rm_duplicates = TRUE), list(rm_duplicates = TRUE))
  for (bad in list(NA, c(TRUE, FALSE), "yes", 1))
    expect_error(concept_options(rm_duplicates = bad), "TRUE or FALSE")
  expect_true(eval(formals(run_concepts)$rm_duplicates))
  expect_true(eval(formals(run_atomic_concepts_unified)$rm_duplicates))
  expect_true(eval(formals(build_atomic_concept_unified_sql)$rm_duplicates))
  expect_true(.ehr_pipeline_option_defaults()$rm_duplicates)
})

test_that("run_one_source forwards rm_duplicates to run_concepts only when supplied", {
  captured <- NULL
  runner <- function(fn) {
    env <- list2env(list(
      ehr_paths = function(...) list(src_name = "mimic_iv", base_dir = ".", cohort_profile = "general"),
      ehr_init_project = function(...) NULL,
      .ehr_resolve_db_name = function(...) "test_db",
      run_concepts = function(...) { captured <<- list(...); invisible(NULL) }
    ), parent = environment(fn))
    environment(fn) <- env
    fn
  }
  rs <- runner(run_one_source)
  suppressMessages(rs("mimic_iv", stages = "run_concepts"))
  expect_false("rm_duplicates" %in% names(captured))       # run_concepts supplies TRUE
  suppressMessages(rs("mimic_iv", stages = "run_concepts",
                      concepts = concept_options(rm_duplicates = FALSE)))
  expect_false(captured$rm_duplicates)
})

test_that("run_concepts forwards rm_duplicates to both atomic layers", {
  seen <- list()
  paths <- list(credentials_json = "unused", helper_sql_dir = tempfile(), pkg_flat_sql_dir = "flat",
    concept_config_csv_ts = "ts.csv", pkg_ts_sql_dir = "ts", concept_config_csv_dur = "dur.csv",
    pkg_dur_sql_dir = "dur", pkg_repo_sql_dir = tempfile(), pkg_rendehr_sql_dir = "rendehr",
    user_sql_dir = tempfile(), external_sql_dir = tempfile())
  fn <- run_concepts
  environment(fn) <- list2env(list(
    ehr_paths = function(...) paths,
    src_schema_names = function(...) list(raw = "raw", concepts = "concepts", derived = "derived"),
    .ehr_resolve_db_name = function(...) "test_db",
    load_credentials = function(...) list(), ensure_database_exists = function(...) NULL,
    connect_postgres = function(...) list(), create_schemas = function(...) NULL,
    run_atomic_concepts_unified = function(..., typ, rm_duplicates) {
      seen[[typ]] <<- rm_duplicates; c(x = TRUE)
    },
    auto_register_atomic_provenance = function(...) NULL
  ), parent = environment(run_concepts))
  local_mocked_bindings(dbExecute = function(...) 0L, dbDisconnect = function(...) TRUE, .package = "DBI")
  suppressMessages(fn("mimic_iv", layers = c("ts", "dur")))
  expect_identical(seen, list(ts = TRUE, dur = TRUE))
  suppressMessages(fn("mimic_iv", layers = c("ts", "dur"), rm_duplicates = FALSE))
  expect_identical(seen, list(ts = FALSE, dur = FALSE))
  suppressMessages(fn("mimic_iv", concepts = concept_options(layers = c("ts", "dur"), rm_duplicates = FALSE)))
  expect_identical(seen, list(ts = FALSE, dur = FALSE))
})

test_that("run_atomic_concepts_unified passes rm_duplicates to the SQL builder", {
  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(source = "mimic_iv", concept_name = "x", itm_class = "col_itm",
                              typ = "ts", stringsAsFactors = FALSE), cfg)
  got <- NULL
  local_mocked_bindings(build_atomic_concept_unified_sql = function(..., rm_duplicates, dedup) {
    got <<- rm_duplicates; "SELECT 1"
  })
  call <- function(...) run_atomic_concepts_unified(concept_config_path = cfg, src_name = "mimic_iv",
    schema_names = list(raw = "r", concepts = "c"), typ = "ts", sql_out_dir = tempfile(), dry_run = TRUE, ...)
  suppressMessages(call()); expect_true(got)
  suppressMessages(call(rm_duplicates = FALSE)); expect_false(got)
  expect_error(call(rm_duplicates = NA), "TRUE or FALSE")
})

# ---------------------------------------------------------------------------
# Config-driven plan and unit keys
# ---------------------------------------------------------------------------

.dd_plan_cfg <- function() data.frame(
  concept_name = c("k", "k", "k", "hb", "hb", "hbbg", "nolab"),
  table_name = c("labevents", "chartevents", "chartevents", "labevents", "chartevents", "labevents", "chartevents"),
  match_value = c("100", "220200", "150", "500", "220600", "700", "220800"),
  requred_dedup = c("FALSE", "TRUE", "TRUE", NA, "TRUE", NA, "TRUE"),
  dedup_cross_reference = c(NA, NA, NA, NA, "hbbg:700", NA, NA), stringsAsFactors = FALSE)

test_that("the plan is derived from the config: flagged MetaVision chart rows and their concept's labs", {
  plan <- .dedup_plan(.dd_plan_cfg(), "mimic_iv")
  expect_setequal(names(plan), c("k", "hb"))              # lab-less concepts: no dedup
  expect_setequal(plan$k$pairs$chart_itemid, c("220200", "150"))
  expect_setequal(plan$k$pairs$lab_itemid, "100")
  expect_setequal(plan$k$pairs$offset_min, 0L)
  expect_setequal(plan$hb$pairs$lab_itemid, c("500", "700"))
  expect_identical(plan$hb$cross_items$hbbg, "700")
  expect_setequal(.dedup_plan(.dd_plan_cfg(), "mimic_iii")$k$pairs$offset_min, c(0L, 60L))
  expect_length(.dedup_plan(.dd_plan_cfg(), "eicu"), 0L)
  # unflagged chart rows are never candidates
  cfg <- .dd_plan_cfg(); cfg$requred_dedup[2:3] <- "FALSE"
  expect_null(.dedup_plan(cfg, "mimic_iv")$k)
})

test_that("overrides disable a concept's dedup and cross-references to an overridden owner", {
  plan <- .dedup_plan(.dd_plan_cfg(), "mimic_iv", overridden = "hbbg")
  expect_setequal(plan$hb$pairs$lab_itemid[!plan$hb$pairs$cross], "500")
  expect_false(any(plan$hb$pairs$cross))
  expect_match(attr(plan, "notes"), "'hbbg' replaced by user_concepts", all = FALSE)
  plan <- .dedup_plan(.dd_plan_cfg(), "mimic_iv", overridden = "hb")
  expect_null(plan$hb)
  expect_match(attr(plan, "notes"), "dedup for 'hb' disabled", all = FALSE)
})

test_that("the shipped config yields the validated dedup plan", {
  cfg <- as.data.frame(readr::read_csv(system.file("extdata", "rendehr_concepts_config.csv", package = environmentName(environment(.dedup_plan))),
                                       show_col_types = FALSE))
  cfg$match_value <- as.character(cfg$match_value)
  plan <- function(src) .dedup_plan(cfg[cfg$source == src, ], src)
  iv <- plan("mimic_iv"); iii <- plan("mimic_iii")
  expect_length(iv, 51L); expect_length(iii, 50L)
  expect_identical(iv$hemoglobin$cross_items$hemoglobin_bloodgas, "50811")
  expect_true("224828" %in% iv$base_excess$pairs$chart_itemid)   # charted copy of labevents 50802
  expect_true(any(as.integer(unlist(lapply(iii, function(e) e$pairs$chart_itemid))) < 220000L))
  expect_true(all(as.integer(unlist(lapply(iv, function(e) e$pairs$chart_itemid))) >= 220000L))
})

test_that("eICU nurse-charted vitals are paired with the monitor tables of the same concept", {
  cfg <- as.data.frame(readr::read_csv(system.file("extdata", "rendehr_concepts_config.csv", package = environmentName(environment(.dedup_plan))),
                                       show_col_types = FALSE))
  cfg$match_value <- as.character(cfg$match_value)
  plan <- .dedup_plan(cfg[cfg$source == "eicu", ], "eicu")
  expect_setequal(names(plan), c("etco2", "heart_rate", "map", "mpap", "resp_rate", "icp"))
  expect_true(all(vapply(plan, function(e) all(e$pairs$chart_table == "nursecharting"), TRUE)))
  expect_setequal(plan$map$pairs$lab_table, c("vitalperiodic", "vitalaperiodic"))
  expect_setequal(plan$map$pairs$chart_itemid, c("MAP (mmHg)", "Arterial Line MAP (mmHg)", "Invasive BP Mean"))
  expect_setequal(plan$heart_rate$pairs$offset_min, 0L)
  sql <- .dedup_filter_sql(plan$heart_rate, c("uniquepid", "patienthealthsystemstayid", "patientunitstayid"))$ctes
  expect_match(sql, "('nursecharting', 'Heart Rate', 'vitalperiodic', CAST(NULL AS TEXT), 0, TRUE)", fixed = TRUE)
  # matched by a hash join on one text key per row, not by sorting every row
  expect_match(sql, "COALESCE(CAST(c.uniquepid AS TEXT), '\\N') || '|' || COALESCE(CAST(c.patienthealthsystemstayid AS TEXT)", fixed = TRUE)
  expect_false(grepl("PARTITION BY", sql, fixed = TRUE))
  # the MIMIC plan keeps its table pair
  iv <- .dedup_plan(cfg[cfg$source == "mimic_iv", ], "mimic_iv")
  expect_true(all(iv$heart_rate$pairs$chart_table == "chartevents" | is.null(iv$heart_rate)))
})

test_that("malformed dedup_cross_reference values stop the run", {
  cfg <- .dd_plan_cfg(); cfg$dedup_cross_reference[5] <- "hbbg-700"
  expect_error(.dedup_plan(cfg, "mimic_iv"), "concept:itemid")
  cfg$dedup_cross_reference[5] <- "hbbg:999"
  expect_error(.dedup_plan(cfg, "mimic_iv"), "not labevents items of 'hbbg'")
})

test_that("unit keys follow each row's conversion; unresolved units have no key", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  key <- function(units, conversion, unit, concept = "x") {
    vals <- paste(sprintf("(%s)", ifelse(is.na(units), "NULL", paste0("'", units, "'"))), collapse = ", ")
    sql <- .dedup_unit_key_sql(conversion, "valueuom", unit, concept)
    DBI::dbGetQuery(con, sprintf("SELECT %s AS k FROM (VALUES %s) src(valueuom)", sql, vals))$k
  }
  expect_identical(key(c("mEq/L", "MEQ/L ", "m.Eq/L", NA, ""), "none", "mEq/L"), c("meq/l", "meq/l", "meq/l", "meq/l", "meq/l"))
  expect_identical(key(c(NA, ""), "none", NA), c("", ""))                        # unitless concept (e.g. INR)
  expect_identical(key(c("sec", "SECONDS"), "none", "sec"), c("sec", "sec"))       # reviewed alias
  expect_identical(key(c("ng/mL", "ng/mL FEU"), "none", "ng/mL"), c("ng/ml", "ng/mlfeu"))
  expect_identical(key(c("mg/L", "mg/dL", "MG/DL", "xyz", NA), "map", "mg/L", "c_reactive_protein"),
                   c("mg/l", "mg/l", "mg/l", NA, NA))                                 # converted to target, else no key
  expect_identical(key(c("F", "Deg. F", "C", "xyz", NA), "f_to_c", "C"), c("c", "c", "c", NA, "c"))
  expect_identical(key("x", "fio2_fraction", "%"), NA_character_)                  # unsupported conversion
})

# ---------------------------------------------------------------------------
# Clean fixtures: generated SQL executed on DuckDB (and PostgreSQL if configured)
# ---------------------------------------------------------------------------

.dd_cfg_row <- function(source, concept, table, item, unit, flag = NA, conversion = "none", cross = NA) {
  data.frame(source = source, concept_name = concept, table_name = table, itm_class = "itm_itm",
             sub_var = "itemid", match_value = as.character(item), val_var = "valuenum",
             unit_var = "valueuom", index_var = "charttime", end_var = NA, conversion = conversion,
             dict = NA, unit = unit, typ = "ts", time_precision = "datetime", requred_dedup = flag,
             dedup_cross_reference = cross, stringsAsFactors = FALSE)
}

# Fixture tables for one source family inside schema `raw`.
.dd_fixture <- function(con, raw, icu, chart, lab) {
  ts <- "TIMESTAMP"
  DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", raw))
  DBI::dbExecute(con, sprintf("CREATE TABLE %s.icu_windows (subject_id BIGINT, hadm_id BIGINT, %s BIGINT, admittime %s, icu_intime %s, icu_los_min DOUBLE PRECISION)", raw, icu, ts, ts))
  DBI::dbExecute(con, sprintf("INSERT INTO %s.icu_windows VALUES (1, 10, 100, TIMESTAMP '2020-01-01 00:00:00', TIMESTAMP '2020-01-01 00:00:00', 9000), (1, 11, 101, TIMESTAMP '2020-02-01 00:00:00', TIMESTAMP '2020-02-01 00:00:00', 9000)", raw))
  DBI::dbExecute(con, sprintf("CREATE TABLE %s.admission_windows (subject_id BIGINT, hadm_id BIGINT, admittime %s)", raw, ts))
  DBI::dbExecute(con, sprintf("INSERT INTO %s.admission_windows VALUES (1, 10, TIMESTAMP '2020-01-01 00:00:00'), (1, 11, TIMESTAMP '2020-02-01 00:00:00')", raw))
  DBI::dbExecute(con, sprintf("CREATE TABLE %s.chartevents (subject_id BIGINT, hadm_id BIGINT, %s BIGINT, itemid INT, charttime %s, valuenum DOUBLE PRECISION, valueuom TEXT)", raw, icu, ts))
  DBI::dbExecute(con, sprintf("CREATE TABLE %s.labevents (subject_id BIGINT, hadm_id BIGINT, itemid INT, charttime %s, valuenum DOUBLE PRECISION, valueuom TEXT)", raw, ts))
  ins <- function(tab, rows) if (nrow(rows)) for (i in seq_len(nrow(rows))) {
    v <- vapply(rows[i, ], function(x) if (is.na(x)) "NULL" else if (is.character(x) && !grepl("^TIMESTAMP", x)) sprintf("'%s'", x) else as.character(x), "")
    DBI::dbExecute(con, sprintf("INSERT INTO %s.%s VALUES (%s)", raw, tab, paste(v, collapse = ", ")))
  }
  ins("chartevents", chart); ins("labevents", lab)
}
.dd_t <- function(x) sprintf("TIMESTAMP '%s'", x)
.dd_chart <- function(item, time, value, unit = "mEq/L", hadm = 10, stay = 100)
  data.frame(subject_id = 1, hadm_id = hadm, stay = stay, itemid = item, charttime = .dd_t(time), valuenum = value, valueuom = unit, stringsAsFactors = FALSE)
.dd_lab <- function(item, time, value, unit = "mEq/L", hadm = 10)
  data.frame(subject_id = 1, hadm_id = hadm, itemid = item, charttime = .dd_t(time), valuenum = value, valueuom = unit, stringsAsFactors = FALSE)

# Runs the full extraction for a fixture with mocked rules; returns concept tables per setting.
.dd_run <- function(con, src, cfg_rows, chart, lab, concepts = NULL, user = NULL, both = TRUE) {
  icu <- if (src == "mimic_iii") "icustay_id" else "stay_id"
  raw <- paste0("raw_", paste(sample(letters, 8), collapse = ""))
  schemas <- raw   # every schema created here is dropped on exit
  on.exit(for (x in rev(schemas)) try(DBI::dbExecute(con, sprintf("DROP SCHEMA %s CASCADE", x)), silent = TRUE), add = TRUE)
  .dd_fixture(con, raw, icu, chart, lab)
  cfg <- tempfile(fileext = ".csv"); readr::write_csv(cfg_rows, cfg)
  user_path <- NULL
  if (!is.null(user)) { user_path <- tempfile(fileext = ".csv"); readr::write_csv(user, user_path) }
  engine <- if (inherits(con, "duckdb_connection")) "duckdb" else "postgres"
  out <- list(); msgs <- list()
  for (flag in if (both) c(TRUE, FALSE) else TRUE) {
    cs <- paste0(raw, if (flag) "_on" else "_off")
    DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", cs)); schemas <- c(schemas, cs)
    m <- character(0)
    res <- withCallingHandlers(
      run_atomic_concepts_unified(con, cfg, src, list(raw = raw, concepts = cs), typ = "ts",
                                  sql_out_dir = tempfile(), user_concepts = user_path, concept_name = concepts,
                                  engine = engine, rm_duplicates = flag),
      message = function(e) { m <<- c(m, conditionMessage(e)); invokeRestart("muffleMessage") })
    tabs <- lapply(stats::setNames(names(res), names(res)), function(n)
      if (res[[n]]) DBI::dbGetQuery(con, sprintf("SELECT * FROM %s.%s", cs, n)) else NULL)
    out[[if (flag) "on" else "off"]] <- list(results = res, tables = tabs, messages = m)
  }
  out
}
.dd_chart_values <- function(tab) sort(as.numeric(tab$value[tab$source_table == "chartevents"]))

.dd_exact_matching_case <- function(con) {
  cfg <- rbind(.dd_cfg_row("mimic_iv", "k", "labevents", 100, "mEq/L"),
               .dd_cfg_row("mimic_iv", "k", "chartevents", 220200, "mEq/L", flag = TRUE),
               .dd_cfg_row("mimic_iv", "k", "chartevents", 150, "mEq/L", flag = TRUE))  # flagged CareVue-range id: also a copy
  chart <- rbind(
    .dd_chart(220200, "2020-01-01 01:00:00", 4.0),                 # exact: removed
    .dd_chart(220200, "2020-01-01 02:00:15", 4.1),                 # same minute, 15 s later: kept
    .dd_chart(220200, "2020-01-01 03:00:00", 4.2),                 # different value: kept
    .dd_chart(220200, "2020-02-01 04:00:00", 4.4, hadm = 11, stay = 101),  # other admission: kept
    .dd_chart(220200, "2020-01-01 05:00:00", 4.5, unit = NA),      # blank unit, treated as target: removed
    .dd_chart(220200, "2020-01-01 06:00:00", 4.6),                 # lab value NULL: kept
    .dd_chart(220200, "2020-01-01 07:00:00", 4.7),                 # lab not extracted: kept
    .dd_chart(220200, "2020-01-01 08:00:00", 4.8, unit = "MEQ/L "),# unit key normalised: removed
    .dd_chart(220200, "2020-01-01 08:00:00", 4.8, unit = NA),      # same event, blank unit = target: removed
    .dd_chart(150, "2020-01-01 01:00:00", 4.0))                 # CareVue-range id: removed
  lab <- rbind(
    .dd_lab(100, "2020-01-01 01:00:00", 4.0), .dd_lab(100, "2020-01-01 02:00:00", 4.1),
    .dd_lab(100, "2020-01-01 03:00:00", 4.3), .dd_lab(100, "2020-01-01 04:00:00", 4.4),
    .dd_lab(100, "2020-01-01 05:00:00", 4.5), .dd_lab(100, "2020-01-01 06:00:00", NA),
    .dd_lab(100, "2020-01-01 07:00:00", 4.7, hadm = 99), .dd_lab(100, "2020-01-01 08:00:00", 4.8))
  r <- .dd_run(con, "mimic_iv", cfg, chart, lab)
  on <- r$on$tables$k; off <- r$off$tables$k
  expect_equal(.dd_chart_values(off), sort(c(4.0, 4.1, 4.2, 4.4, 4.5, 4.6, 4.7, 4.8, 4.8, 4.0)))
  expect_equal(.dd_chart_values(on), sort(c(4.1, 4.2, 4.4, 4.6, 4.7)))
  # lab rows are never removed; TRUE output is a subset of FALSE with identical rows
  expect_identical(sum(on$source_table == "labevents"), sum(off$source_table == "labevents"))
  expect_identical(names(on), names(off))
  key <- function(t) do.call(paste, c(t[order(t$event_id), ], sep = "\r"))
  expect_true(all(key(on) %in% key(off)))
  expect_identical(nrow(off) - nrow(on), 5L)
}

.dd_repeated_chart_case <- function(con) {
  cfg <- rbind(.dd_cfg_row("mimic_iii", "g", "labevents", 50931, "mg/dL"),
               .dd_cfg_row("mimic_iii", "g", "chartevents", 811, "mg/dL"),
               .dd_cfg_row("mimic_iii", "g", "chartevents", 1529, "mg/dL"))
  chart <- rbind(
    .dd_chart(1529, "2020-01-01 01:00:00", 120, "mg/dL"),   # repeated by item 811: removed
    .dd_chart(811,  "2020-01-01 01:00:00", 120, "mg/dL"),   # kept (lowest item id)
    .dd_chart(1529, "2020-01-01 02:00:00", 130, "mg/dL"),   # different value: kept
    .dd_chart(811,  "2020-01-01 02:00:00", 131, "mg/dL"),   # kept
    .dd_chart(811,  "2020-01-01 03:00:00", NA,  "mg/dL"),   # NULL values: both kept
    .dd_chart(1529, "2020-01-01 03:00:00", NA,  "mg/dL"),
    .dd_chart(811,  "2020-02-01 01:00:00", 120, "mg/dL", hadm = 11, stay = 101))  # other admission: kept
  lab <- .dd_lab(50931, "2020-01-01 01:00:00", 120, "mg/dL")  # lab rows are never removed
  r <- .dd_run(con, "mimic_iii", cfg, chart, lab)
  on <- r$on$tables$g; off <- r$off$tables$g
  expect_identical(nrow(off) - nrow(on), 1L)
  expect_equal(.dd_chart_values(on), c(120, 120, 130, 131))
  expect_identical(on$source_itemid[on$source_table == "chartevents" & on$value %in% 120 & on$hadm_id == 10], "811")
  expect_identical(sum(is.na(on$value)), 2L)
  expect_identical(sum(on$source_table == "labevents"), 1L)
}

.dd_offset_case <- function(con) {
  cfg <- rbind(.dd_cfg_row("mimic_iii", "b", "labevents", 400, "mg/dL"),
               .dd_cfg_row("mimic_iii", "b", "chartevents", 220300, "mg/dL", flag = TRUE))
  chart <- rbind(.dd_chart(220300, "2020-01-01 10:00:00", 1, "mg/dL"), .dd_chart(220300, "2020-01-01 11:00:00", 2, "mg/dL"),
                 .dd_chart(220300, "2020-01-01 12:00:00", 3, "mg/dL"), .dd_chart(220300, "2020-01-01 13:00:00", 4, "mg/dL"),
                 .dd_chart(220300, "2020-01-01 14:00:00", 5, "mg/dL"))
  lab <- rbind(.dd_lab(400, "2020-01-01 09:00:00", 1, "mg/dL"),   # chart - lab = +60: removed
               .dd_lab(400, "2020-01-01 10:01:00", 2, "mg/dL"),   # +59: kept
               .dd_lab(400, "2020-01-01 10:59:00", 3, "mg/dL"),   # +61: kept
               .dd_lab(400, "2020-01-01 14:00:00", 4, "mg/dL"),   # -60 (signed): kept
               .dd_lab(400, "2020-01-01 14:00:00", 5, "mg/dL"))   # 0: removed
  r <- .dd_run(con, "mimic_iii", cfg, chart, lab)
  expect_equal(.dd_chart_values(r$on$tables$b), c(2, 3, 4))
  expect_equal(.dd_chart_values(r$off$tables$b), 1:5)
}

.dd_conversion_case <- function(con) {
  # CRP: lab in mg/dL is converted x10 to mg/L by the conversion map before comparison.
  cfg <- rbind(.dd_cfg_row("mimic_iv", "c_reactive_protein", "labevents", 800, "mg/L", conversion = "map"),
               .dd_cfg_row("mimic_iv", "c_reactive_protein", "chartevents", 220900, "mg/L", flag = TRUE))
  chart <- rbind(.dd_chart(220900, "2020-01-01 01:00:00", 15, "mg/L"),   # equals converted lab: removed
                 .dd_chart(220900, "2020-01-01 02:00:00", 2, "mg/L"))    # equals raw lab only: kept
  lab <- rbind(.dd_lab(800, "2020-01-01 01:00:00", 1.5, "mg/dL"), .dd_lab(800, "2020-01-01 02:00:00", 2, "mg/dL"))
  r <- .dd_run(con, "mimic_iv", cfg, chart, lab)
  expect_equal(.dd_chart_values(r$on$tables$c_reactive_protein), 2)
}

.dd_cross_case <- function(con) {
  cfg <- rbind(.dd_cfg_row("mimic_iv", "hb", "labevents", 500, "g/dL"),
               .dd_cfg_row("mimic_iv", "hb", "chartevents", 220600, "g/dL", flag = TRUE, cross = "hbbg:700"),
               .dd_cfg_row("mimic_iv", "hbbg", "labevents", 700, "g/dL"))
  chart <- rbind(.dd_chart(220600, "2020-01-01 01:00:00", 10, "g/dL"),   # copy of CBC 500
                 .dd_chart(220600, "2020-01-01 02:00:00", 11, "g/dL"))   # copy of blood-gas 700
  lab <- rbind(.dd_lab(500, "2020-01-01 01:00:00", 10, "g/dL"), .dd_lab(700, "2020-01-01 02:00:00", 11, "g/dL"))

  # owner built in the same call: both copies removed, canonical 700 kept in hbbg
  r <- .dd_run(con, "mimic_iv", cfg, chart, lab, both = FALSE)
  expect_equal(.dd_chart_values(r$on$tables$hb), numeric(0))
  expect_equal(as.numeric(r$on$tables$hbbg$value), 11)
  expect_false(any(r$on$tables$hb$source_itemid == "700"))        # reference rows are not output
  expect_identical(names(r$on$results)[1], "hbbg")                 # owner built first

  # owner not selected: blood-gas copy kept and the skip explained
  r <- .dd_run(con, "mimic_iv", cfg, chart, lab, concepts = "hb", both = FALSE)
  expect_equal(.dd_chart_values(r$on$tables$hb), 11)
  expect_match(r$on$messages, "cross-concept reference from 'hb' to 'hbbg' skipped", all = FALSE)
  expect_true(all(r$on$tables$hb$dedup_candidate[r$on$tables$hb$source_table == "chartevents"]))

  # owner fails to build: dependent rebuilt without the cross reference
  broken <- cfg; broken$val_var[broken$concept_name == "hbbg"] <- "no_such_column"
  r <- .dd_run(con, "mimic_iv", broken, chart, lab, both = FALSE)
  expect_false(r$on$results[["hbbg"]])
  expect_equal(.dd_chart_values(r$on$tables$hb), 11)
  expect_match(r$on$messages, "not built in this call", all = FALSE)

  # owner overridden through user_concepts: cross reference disabled, CBC rule kept
  r <- .dd_run(con, "mimic_iv", cfg, chart, lab, both = FALSE,
               user = .dd_cfg_row("mimic_iv", "hbbg", "labevents", 700, "g/dL"))
  expect_equal(.dd_chart_values(r$on$tables$hb), 11)
  expect_match(r$on$messages, "'hbbg' replaced by user_concepts", all = FALSE)

  # dependent overridden: its built-in rule disabled entirely
  r <- .dd_run(con, "mimic_iv", cfg, chart, lab, both = FALSE,
               user = cfg[cfg$concept_name == "hb", ])
  expect_equal(.dd_chart_values(r$on$tables$hb), c(10, 11))
  expect_match(r$on$messages, "dedup for 'hb' disabled", all = FALSE)
}

test_that("FALSE generates exactly the SQL of the builder without dedup", {
  df <- rbind(.dd_cfg_row("mimic_iv", "k", "labevents", 100, "mEq/L"),
              .dd_cfg_row("mimic_iv", "k", "chartevents", 220200, "mEq/L", flag = TRUE))
  df$class <- df$itm_class
  plan <- .dedup_plan(df, "mimic_iv")
  spec <- .dedup_spec(plan$k, df, "k", "k")
  plain <- build_atomic_concept_unified_sql(df, "raw", "cc", "mimic_iv", rm_duplicates = FALSE, dedup = NULL)
  expect_identical(build_atomic_concept_unified_sql(df, "raw", "cc", "mimic_iv", rm_duplicates = FALSE, dedup = spec), plain)
  expect_false(grepl("exact_rn", plain))
  expect_match(build_atomic_concept_unified_sql(df, "raw", "cc", "mimic_iv", rm_duplicates = TRUE, dedup = NULL), "exact_rn")
  deduped <- build_atomic_concept_unified_sql(df, "raw", "cc", "mimic_iv", rm_duplicates = TRUE, dedup = spec)
  expect_false(identical(deduped, plain))
  expect_match(deduped, "event_time_raw")
  expect_false(grepl("time_since_hadm_min -", deduped))   # never matches on whole-minute columns
})

for (engine in c("duckdb", "postgres")) local({
  engine <- engine
  connect <- function() {
    if (engine == "duckdb") {
      skip_if_not_installed("duckdb")
      return(connect_duckdb(":memory:"))
    }
    db <- Sys.getenv("EHR_TEST_PG_DB", "")
    skip_if(!nzchar(db), "set EHR_TEST_PG_DB (and PG* credentials) to run PostgreSQL fixtures")
    skip_if_not_installed("RPostgres")
    DBI::dbConnect(RPostgres::Postgres(), dbname = db)
  }
  cases <- list(
    "exact matching, admission, extraction, values, units and exclusions" = .dd_exact_matching_case,
    "MIMIC-III +60 offset is exact and signed" = .dd_offset_case,
    "repeated chart rows of one concept are kept once" = .dd_repeated_chart_case,
    "unit conversion is applied before comparing values" = .dd_conversion_case,
    "cross-concept references preserve the canonical result" = .dd_cross_case)
  for (nm in names(cases)) local({
    fn <- cases[[nm]]
    test_that(paste0("dedup fixture (", engine, "): ", nm), {
      con <- connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      fn(con)
    })
  })
})

test_that("chart rows of a MIMIC lab concept are marked for chart-copy dedup", {
  # Charted copies of laboratory results duplicate the lab row (same value and
  # minute). Concepts whose charted values are separate measurements (FiO2
  # settings, pulse oximetry) are the documented exceptions.
  cfg <- utils::read.csv(system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"),
                         stringsAsFactors = FALSE)
  independent <- c("fio2", "o2sat")
  for (s in c("mimic_iii", "mimic_iv")) {
    lab <- unique(cfg$concept_name[cfg$source == s & cfg$table_name == "labevents"])
    ch <- cfg[cfg$source == s & cfg$table_name == "chartevents" & cfg$concept_name %in% setdiff(lab, independent), ]
    expect_true(all(ch$required_dedup %in% c("TRUE", TRUE)),
                info = paste(s, ch$concept_name[!ch$required_dedup %in% c("TRUE", TRUE)], collapse = ", "))
  }
})

test_that("CareVue gm/dl and lab g/dL are one unit for chart-copy dedup", {
  expect_equal(.dedup_norm_unit(c("gm/dl", "g/dL", " G/DL ")), c("g/dl", "g/dl", "g/dl"))
  key <- .dedup_unit_key_sql("none", "u", "g/dL", "hemoglobin")
  expect_match(key, "WHEN REPLACE(REPLACE(LOWER(TRIM(CAST(src.u AS TEXT))), ' ', ''), '.', '') = 'gm/dl' THEN 'g/dl'", fixed = TRUE)
})

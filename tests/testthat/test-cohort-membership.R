# cohort_info.cohort_index: one public membership flag, from the cohort
# definition, never from import scope.
#
# Fixture (one patient): hospitalization H1 = 10 with ICU stays U1 = 100 and
# U2 = 101, and a nonqualifying hospitalization H2 = 11 with U3 = 110. In eICU
# only U2 records the matching diagnosis; in MIMIC it belongs to H1, which
# also has an ICU-less admission H3 = 12 to exercise NULL ICU ids.

.member_fixture <- function(fam) {
  if (identical(fam, "eicu")) {
    rows <- data.frame(uniquepid = "P1",
                       patienthealthsystemstayid = c(10L, 10L, 11L),
                       patientunitstayid = c(100L, 101L, 110L))
    list(src = "eicu_demo",
         ladder = c("uniquepid", "patienthealthsystemstayid", "patientunitstayid"),
         rows = rows,
         tables = list(
           patient = rows,
           diagnosis = data.frame(patientunitstayid = c(100L, 101L, 110L),
                                  icd9code = c("I10", "S14.0", "I10"))))
  } else {
    rows <- data.frame(subject_id = 1L, hadm_id = c(10L, 10L, 11L, 12L),
                       stay_id = c(100L, 101L, 110L, NA_integer_))
    list(src = "mimic_iv_demo",
         ladder = c("subject_id", "hadm_id", "stay_id"),
         rows = rows,
         tables = list(diagnoses_icd = data.frame(
           subject_id = 1L, hadm_id = c(10L, 11L, 12L),
           icd_code = c("S14.0", "I10", "I10"), icd_version = 10L)))
  }
}

.member_icd_filter <- function() {
  p <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(icd_code = "S14.0", icd_version = "10"), p)
  p
}

# Resolve a selection through the real engine (ICD by default), build the
# hierarchy, and map cohort_index onto the rows the import scope retained - the
# rows cohort_info would hold. Returns the plan, the hierarchy and cohort_info.
.member_build <- function(fam, import_scope = NULL, level = "icu",
                          direct_ids = NULL, key = NULL, cohort_info = NULL,
                          tamper = NULL) {
  skip_if_not_installed("duckdb")
  f <- .member_fixture(fam)
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (t in names(f$tables))
    DBI::dbWriteTable(con, t, f$tables[[t]], temporary = TRUE)
  DBI::dbWriteTable(con, "ehr_visit_windows_full", f$rows, temporary = TRUE)
  args <- list(con = con, src_name = f$src, paths = list(), ladder = f$ladder,
               import_scope = import_scope, key = key)
  if (is.null(direct_ids)) {
    args$icd_filter_path <- .member_icd_filter()
  } else {
    ids <- tempfile(fileext = ".csv")
    readr::write_csv(stats::setNames(data.frame(as.character(direct_ids)), key), ids)
    args$direct_ids_path <- ids
  }
  plan <- suppressMessages(do.call(.ehr_resolve_selection, args))
  schema <- src_schema_names(f$src)$raw
  plan <- .ehr_build_cohort_hierarchy(con, schema, plan, f$ladder)$plan
  hier <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s.cohort_hierarchy", schema))
  if (!is.null(tamper)) tamper(con, schema)
  imported <- hier[hier$scope_index, f$ladder, drop = FALSE]
  ci <- cohort_info %||% cbind(imported, diag = "x")
  list(plan = plan, hierarchy = hier,
       ci = .attach_cohort_membership(con, schema, f$src, ci, level))
}

.members <- function(ci, fam, col = 3L) {
  ids <- ci[[.member_fixture(fam)$ladder[[col]]]]
  sort(ids[ci$cohort_index & !is.na(ids)])
}

test_that("eICU ICD selection promotes a matching unit to its hospitalization", {
  skip_if_not_installed("duckdb")
  f <- .member_fixture("eicu")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (t in names(f$tables)) DBI::dbWriteTable(con, t, f$tables[[t]])
  sel <- .ehr_select_icd(con, "eicu_demo", .member_icd_filter())
  expect_identical(sel$key, "patienthealthsystemstayid")
  expect_identical(sel$values, "10")
  # Unit evidence is kept, at unit grain, for audit.
  expect_identical(sel$evidence_key, "patientunitstayid")
  expect_identical(sel$evidence_values, "101")
})

test_that("eICU ICD selection rejects evidence units without one hospitalization", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "diagnosis",
                    data.frame(patientunitstayid = c(101L, 102L),
                               icd9code = c("S14.0", "S14.0")))
  DBI::dbWriteTable(con, "patient",
                    data.frame(uniquepid = "P1", patienthealthsystemstayid = 10L,
                               patientunitstayid = 101L))
  expect_error(.ehr_select_icd(con, "eicu_demo", .member_icd_filter()),
               "1 matching unit stay\\(s\\) have no patienthealthsystemstayid")
  DBI::dbWriteTable(con, "patient", overwrite = TRUE,
                    data.frame(uniquepid = "P1",
                               patienthealthsystemstayid = c(10L, 11L, 12L),
                               patientunitstayid = c(101L, 102L, 102L)))
  expect_error(.ehr_select_icd(con, "eicu_demo", .member_icd_filter()),
               "1 map to more than one")
})

test_that("ICD membership is hospital-grain in both sources: U1 and U2 members, U3 not", {
  for (fam in c("eicu", "mimic")) {
    r <- .member_build(fam, import_scope = "patient")
    expect_identical(.members(r$ci, fam), c(100L, 101L), info = fam)
    u3 <- r$ci[[.member_fixture(fam)$ladder[[3]]]] %in% 110L
    expect_false(r$ci$cohort_index[u3], info = fam)
    expect_identical(r$plan$selection_key, .member_fixture(fam)$ladder[[2]],
                     info = fam)
  }
  expect_identical(.member_build("eicu")$plan$evidence_key, "patientunitstayid")
})

test_that("import scope changes context, never membership; ICU requests clamp", {
  for (fam in c("eicu", "mimic")) {
    icu_col <- .member_fixture(fam)$ladder[[3]]
    hosp <- .member_build(fam, import_scope = "hospital")
    pat <- .member_build(fam, import_scope = "patient")
    icu <- .member_build(fam, import_scope = "icu")
    # Hospital import retains H1 only; patient import adds H2 (U3) as context.
    expect_identical(sort(hosp$ci[[icu_col]]), c(100L, 101L), info = fam)
    expect_true(110L %in% pat$ci[[icu_col]], info = fam)
    for (r in list(hosp, pat, icu))
      expect_identical(.members(r$ci, fam), c(100L, 101L), info = fam)
    # ICD selection is hospital-grain, so an ICU import request clamps.
    expect_identical(icu$plan$effective_scope, .member_fixture(fam)$ladder[[2]],
                     info = fam)
    expect_identical(sort(icu$ci[[icu_col]]), c(100L, 101L), info = fam)
  }
})

test_that("admission-level membership is any() over the admission and ignores scope", {
  for (fam in c("eicu", "mimic")) {
    adm <- .member_fixture(fam)$ladder[[2]]
    r <- .member_build(fam, import_scope = "patient", level = "admission")
    flags <- vapply(split(r$ci$cohort_index, r$ci[[adm]]),
                    function(v) { expect_length(unique(v), 1L); v[[1L]] }, logical(1))
    expected <- if (fam == "eicu") c(`10` = TRUE, `11` = FALSE)
                else c(`10` = TRUE, `11` = FALSE, `12` = FALSE)
    expect_identical(flags, expected, info = fam)
    expect_identical(attr(r$ci, "ehr_membership_level"), "admission")
  }
})

test_that("unit selections keep exact ICU membership; admission level rolls up", {
  for (fam in c("eicu", "mimic")) {
    key <- .member_fixture(fam)$ladder[[3]]
    r <- .member_build(fam, import_scope = "patient", direct_ids = 101L, key = key)
    expect_identical(.members(r$ci, fam), 101L, info = fam)
    r <- .member_build(fam, import_scope = "patient", level = "admission",
                       direct_ids = 101L, key = key)
    adm <- .member_fixture(fam)$ladder[[2]]
    expect_true(all(r$ci$cohort_index[r$ci[[adm]] == 10L]), info = fam)
    expect_false(any(r$ci$cohort_index[r$ci[[adm]] != 10L]), info = fam)
  }
})

test_that("MIMIC ICU-less admissions match their NULL-ICU hierarchy row", {
  r <- .member_build("mimic", import_scope = "patient")
  expect_false(r$ci$cohort_index[is.na(r$ci$stay_id)])
  r <- .member_build("mimic", import_scope = "patient", direct_ids = 12L,
                     key = "hadm_id")
  expect_true(r$ci$cohort_index[is.na(r$ci$stay_id)])
})

test_that("duplicate hierarchy rows cannot duplicate cohort_info rows", {
  dup <- function(con, schema) DBI::dbExecute(con, sprintf(
    "INSERT INTO %s.cohort_hierarchy SELECT * FROM %s.cohort_hierarchy",
    schema, schema))
  for (level in c("icu", "admission")) {
    r <- .member_build("eicu", import_scope = "patient", level = level,
                       tamper = dup)
    expect_identical(nrow(r$ci), 3L, info = level)
  }
})

test_that("a cohort_info row without a hierarchy row is an error, not a nonmember", {
  ci <- rbind(cbind(.member_fixture("eicu")$rows, diag = "x"),
              data.frame(uniquepid = "P9", patienthealthsystemstayid = 99L,
                         patientunitstayid = 999L, diag = "x"))
  expect_error(.member_build("eicu", import_scope = "patient", cohort_info = ci),
               "1 cohort_info row\\(s\\) have no .*cohort_hierarchy row")
})

test_that("new cohort_info exposes only cohort_index as a membership field", {
  ci <- cbind(.member_fixture("eicu")$rows, hos_cohort_flg = 0L,
              scope_index = TRUE, scope_level = "icu")
  r <- .member_build("eicu", import_scope = "patient", cohort_info = ci)
  expect_true("cohort_index" %in% names(r$ci))
  expect_false(any(c("hos_cohort_flg", "scope_index", "scope_level") %in% names(r$ci)))
  meta <- .cohort_info_meta_table("eicu_demo", "icu")
  expect_identical(meta$analysis_level, "icu")
})

test_that("legacy and superseded raw schemas must be rebuilt", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA eicu_demo_coh")
  ci <- cbind(.member_fixture("eicu")$rows, hos_cohort_flg = 1L)
  expect_error(.attach_cohort_membership(con, "eicu_demo_coh", "eicu_demo", ci),
               "cohort_hierarchy not found")
  DBI::dbWriteTable(con, DBI::Id(schema = "eicu_demo_coh", table = "cohort_hierarchy"),
                    cbind(.member_fixture("eicu")$rows, scope_index = TRUE))
  expect_error(.attach_cohort_membership(con, "eicu_demo_coh", "eicu_demo", ci),
               "missing column\\(s\\): definition_filter_index")
  # An eICU ICD cohort built with the superseded unit-stay selection.
  DBI::dbWriteTable(con, DBI::Id(schema = "eicu_demo_coh", table = "cohort_meta"),
                    data.frame(source = "eicu_demo", source_family = "eicu",
                               route = "named_icd",
                               selection_key = "patientunitstayid"))
  expect_error(.attach_cohort_membership(con, "eicu_demo_coh", "eicu_demo", ci),
               "superseded unit-stay selection")
})

test_that("only superseded eICU ICD selections are stale", {
  meta <- function(fam, route, key) data.frame(
    source_family = fam, route = route, selection_key = key)
  expect_match(.ehr_stale_selection_reason(meta("eicu", "icd", "patientunitstayid")),
               "Rebuild")
  expect_match(.ehr_stale_selection_reason(meta("eicu", "named_icd", "patientunitstayid")),
               "Rebuild")
  expect_null(.ehr_stale_selection_reason(meta("eicu", "icd", "patienthealthsystemstayid")))
  expect_null(.ehr_stale_selection_reason(meta("eicu", "direct_ids", "patientunitstayid")))
  expect_null(.ehr_stale_selection_reason(meta("mimic_iv", "icd", "hadm_id")))
  expect_null(.ehr_stale_selection_reason(NULL))
})

test_that("ICD evidence is written for audit and report labels are evidence-based", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA s")
  plan <- .ehr_selection_plan(
    "eicu_demo", "icd", "patienthealthsystemstayid",
    evidence_key = "patientunitstayid", effective_scope = "uniquepid",
    values = "10",
    metadata = list(icd_evidence = data.frame(evidence_key = "patientunitstayid",
                                              evidence_id = "101"),
                    icd_selection_report = list(unresolved_no_evidence_unit_count = 2)))
  .ehr_write_metadata(con, "s", list(), plan, data.frame(table = character()),
                      data.frame(table = character()))
  meta <- DBI::dbGetQuery(con, "SELECT * FROM s.cohort_meta")
  expect_identical(meta$selection_key, "patienthealthsystemstayid")
  expect_identical(meta$evidence_key, "patientunitstayid")
  expect_identical(meta$icd_unresolved_no_evidence_unit_count, 2)
  expect_false("icd_unresolved_excluded_unit_count" %in% names(meta))
  ev <- DBI::dbGetQuery(con, "SELECT * FROM s.cohort_icd_evidence")
  expect_identical(ev$evidence_id, "101")
})

test_that("superseded membership arguments are rejected on every accessor", {
  for (arg in c("scope_only", "case_adm_only")) {
    a <- stats::setNames(list(TRUE), arg)
    expect_error(do.call(fetch_concepts_wide, c(list("mimic_iv_demo", "hr"), a)),
                 paste0("`", arg, "` has been removed. Use `cohort_only`"))
    for (fn in list(load_merged_ts, load_merged_dur, load_merged_flat,
                    load_merged_cohort_info))
      expect_error(do.call(fn, c(list("s"), a)),
                   paste0("`", arg, "` has been removed. Use `cohort_only`"))
    for (fn in list(load_merged_ts_wide, load_merged_dur_wide))
      expect_error(do.call(fn, c(list("s", "hr"), a)), "has been removed")
  }
  expect_error(load_merged_ts("s", cohort_only = NA), "must be TRUE or FALSE")
})

# ---------------------------------------------------------------------------
# Accessors and merge (Postgres-only SQL paths), live DB
# ---------------------------------------------------------------------------

.member_pg_con <- function() {
  skip_if_not(nzchar(Sys.getenv("EHR_TEST_DB")),
              "Set EHR_TEST_DB=<database> to run DB tests")
  connect_postgres(Sys.getenv("EHR_TEST_DB"),
                   load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                               "config/config_pass.json")))
}

.member_reset_schema <- function(con, schema) {
  skip_if_schema_populated(con, schema)
  DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", schema))
  DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", schema))
}

# Merged schema shaped like merge_sources(analysis_level = "icu") output: every
# column text. src_a is a unit-selected cohort (only ICU 100 is a member; its
# sibling 101 is imported context); src_b reuses the same local ids and has no
# members. ICU 100 and 101 overlap on the ICU clock.
.member_merged_fixture <- function(con, schema, level = "icu") {
  .member_reset_schema(con, schema)
  ids <- data.frame(source_data = c("src_a", "src_a", "src_b"),
                    src_patient_id = "1", src_admission_id = "10",
                    src_icu_id = c("100", "101", "100"))
  ehr <- paste(ids$src_patient_id, ids$src_admission_id, ids$src_icu_id, sep = "_")
  put <- function(table, df) DBI::dbWriteTable(con, DBI::Id(schema = schema, table = table), df)
  put("cohort_info", cbind(ids, ehr_id = ehr,
                           cohort_index = c("true", "false", "false")))
  put("cohort_info_meta", data.frame(source_data = c("src_a", "src_b"),
                                     analysis_level = level))
  put("ts", cbind(ids, ehr_id = ehr, assigned_icu_id = ids$src_icu_id,
                  concept_name = "heart_rate", value = c("60", "100", "200"),
                  unit = "bpm", time_since_hadm_min = 0, time_since_icu_min = 0))
  put("dur", cbind(ids, ehr_id = ehr, concept_name = "propofol_exp", value = "1",
                   end_offset_min = 60, duration_min = 60))
  put("flat", cbind(ids, ehr_id = ehr, age = c("50", "50", "70")))
}

test_that("merged accessors keep member rows only, per source and grain, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  schema <- "member_test_merged"
  .member_merged_fixture(con, schema)
  on.exit(DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", schema)),
          add = TRUE, after = FALSE)

  # ICU level: only src_a ICU 100 - not its overlapping sibling, not src_b's
  # identical ids - filtered before the grid and the aggregation.
  wide <- load_merged_ts_wide(schema, c(heart_rate = "mean"), bin_min = 60,
                              analysis_level = "icu", complete_grid = TRUE,
                              con = con)
  expect_identical(paste(wide$source_data, wide$assigned_icu_id), "src_a 100")
  expect_equal(wide$heart_rate, 60)
  # Without the filter the overlapping ICU stays stay separate observations.
  all_icu <- load_merged_ts_wide(schema, c(heart_rate = "mean"), bin_min = 60,
                                 analysis_level = "icu", cohort_only = FALSE,
                                 con = con)
  expect_identical(nrow(all_icu), 3L)
  expect_identical(sort(all_icu$heart_rate), c(60, 100, 200))

  ts <- load_merged_ts(schema, con = con)
  expect_identical(paste(ts$source_data, ts$assigned_icu_id), "src_a 100")
  expect_identical(nrow(load_merged_ts(schema, cohort_only = FALSE, con = con)), 3L)

  # Admission-grain data (dur, flat) are matched at admission grain.
  expect_identical(unique(load_merged_dur(schema, con = con)$source_data), "src_a")
  dw <- load_merged_dur_wide(schema, c(propofol_exp = "sum"), bin_min = 60, con = con)
  expect_identical(unique(dw$source_data), "src_a")
  expect_identical(unique(load_merged_flat(schema, con = con)$source_data), "src_a")

  ci <- load_merged_cohort_info(schema, con = con)
  expect_identical(paste(ci$source_data, ci$src_icu_id), "src_a 100")
  expect_identical(nrow(load_merged_cohort_info(schema, cohort_only = FALSE,
                                                con = con)), 3L)
})

test_that("ICU-level filtering refuses admission-grain membership, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  schema <- "member_test_merged"
  .member_merged_fixture(con, schema, level = "admission")
  on.exit(DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", schema)),
          add = TRUE, after = FALSE)
  expect_error(load_merged_ts_wide(schema, c(heart_rate = "mean"),
                                   analysis_level = "icu", con = con),
               "needs ICU-grain membership.*src_a, src_b")
  expect_error(load_merged_ts(schema, con = con), "needs ICU-grain membership")
  DBI::dbExecute(con, sprintf("DROP TABLE %s.cohort_info_meta", schema))
  expect_error(load_merged_ts(schema, con = con), "cohort_info_meta")
  expect_silent(load_merged_ts_wide(schema, c(heart_rate = "mean"), con = con))
})

test_that("legacy cohort_info without cohort_index must be rebuilt, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  schema <- "member_test_merged"
  .member_merged_fixture(con, schema)
  on.exit(DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", schema)),
          add = TRUE, after = FALSE)
  DBI::dbExecute(con, sprintf(
    "ALTER TABLE %s.cohort_info DROP COLUMN cohort_index,
       ADD COLUMN scope_index text DEFAULT 'true',
       ADD COLUMN hos_cohort_flg text DEFAULT '1'", schema))
  expect_error(load_merged_ts(schema, con = con), "has no cohort_index column")
  expect_error(load_merged_cohort_info(schema, con = con), "has no cohort_index column")
  expect_identical(nrow(load_merged_ts(schema, cohort_only = FALSE, con = con)), 3L)
})

test_that("fetch_concepts_wide keeps ICU stays separate and filters members, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sn <- src_schema_names("mimic_iv_demo")
  for (s in c(sn$raw, sn$harmonized)) .member_reset_schema(con, s)
  on.exit(for (s in c(sn$raw, sn$harmonized))
    DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", s)),
    add = TRUE, after = FALSE)
  put <- function(schema, table, df)
    DBI::dbWriteTable(con, DBI::Id(schema = schema, table = table), df,
                      overwrite = TRUE)
  # Unit-selected cohort: ICU 100 is a member, overlapping sibling 101 is not;
  # admission 11 is imported patient context.
  put(sn$raw, "cohort_info", data.frame(
    subject_id = 1L, hadm_id = c(10L, 10L, 11L), stay_id = c(100L, 101L, 110L),
    cohort_index = c(TRUE, FALSE, FALSE), source_data = "mimic_iv_demo"))
  put(sn$raw, "cohort_info_meta", data.frame(source_data = "mimic_iv_demo",
                                             analysis_level = "icu"))
  put(sn$harmonized, "ts_icu", data.frame(
    subject_id = 1L, hadm_id = c(10L, 10L, 11L),
    source_icustay_id = c(100L, 101L, 110L),
    assigned_icustay_id = c(100L, 101L, 110L), concept_name = "heart_rate",
    value = c("60", "100", "80"), time_since_icu_min = c(0, 30, 0),
    time_since_hadm_min = 0))
  put(sn$harmonized, "ts_tab", data.frame(
    subject_id = 1L, hadm_id = c(10L, 10L, 11L), stay_id = c(100L, 101L, 110L),
    concept_name = "heart_rate", value = c("60", "100", "80"),
    time_since_hadm_min = 0, time_since_icu_min = 0))

  all_icu <- fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"),
                                 analysis_level = "icu", cohort_only = FALSE,
                                 con = con)
  expect_identical(sort(all_icu$assigned_icustay_id), c(100L, 101L, 110L))
  wide <- fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"),
                              analysis_level = "icu", con = con)
  expect_identical(wide$assigned_icustay_id, 100L)
  expect_equal(wide$heart_rate, 60)

  # Admission level rolls membership up: admission 10 is a member, so rows of
  # both of its imported ICU stays are kept (ts_tab keys on stay_id too);
  # admission 11 (context) is not.
  adm <- fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"), con = con)
  expect_identical(unique(adm$hadm_id), 10L)
  expect_identical(sort(adm$stay_id), c(100L, 101L))

  put(sn$raw, "cohort_info_meta", data.frame(source_data = "mimic_iv_demo",
                                             analysis_level = "admission"))
  expect_error(fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"),
                                   analysis_level = "icu", con = con),
               "needs ICU-grain membership")
})

test_that("admission-level merge keeps one row per admission (first ICU stay), live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sn <- src_schema_names("mimic_iv_demo")
  target <- "member_merge_first_icu"
  for (s in c(sn$raw, sn$harmonized, target)) .member_reset_schema(con, s)
  on.exit(for (s in c(sn$raw, sn$harmonized, target))
    DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", s)),
    add = TRUE, after = FALSE)
  put <- function(schema, table, df)
    DBI::dbWriteTable(con, DBI::Id(schema = schema, table = table), df)
  # Admission 10 has two ICU stays; 101 starts first, only 100 is a member.
  # Admission 11 has no ICU stay.
  ids <- data.frame(subject_id = 1L, hadm_id = c(10L, 10L, 11L),
                    stay_id = c(100L, 101L, NA_integer_))
  put(sn$raw, "cohort_info", cbind(ids, ehr_id = c("1_10", "1_10", "1_11"),
                                   cohort_index = c(TRUE, FALSE, FALSE),
                                   source_data = "mimic_iv_demo"))
  put(sn$raw, "icu_windows", data.frame(
    subject_id = 1L, hadm_id = c(10L, 10L), stay_id = c(100L, 101L),
    icu_intime = as.POSIXct(c("2020-01-03", "2020-01-01"), tz = "UTC")))
  put(sn$harmonized, "flat_tab", cbind(ids, age = c(50, 51, 60)))

  expect_message(
    merge_sources("mimic_iv_demo", db_name = Sys.getenv("EHR_TEST_DB"),
                  base_dir = tempdir(), target_schema = target,
                  merge_ts = FALSE, merge_dur = FALSE),
    "1 cohort_info row\\(s\\) of mimic_iv_demo belong to a later ICU stay")
  q <- function(table) DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM %s.%s ORDER BY src_admission_id", target, table))
  ci <- q("cohort_info")
  expect_identical(ci$src_admission_id, c("10", "11"))
  expect_identical(ci$src_icu_id, c("101", NA))
  expect_identical(ci$cohort_index, c("true", "false"))
  flat <- q("flat")
  expect_identical(flat$ehr_id, c("1_10", "1_11"))
  expect_identical(flat$age, c("51", "60"))
})

test_that("a merge that fails part-way leaves the previous merged tables, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sn <- src_schema_names("mimic_iv_demo")
  target <- "member_merge_atomic"
  for (s in c(sn$raw, sn$harmonized, target)) .member_reset_schema(con, s)
  on.exit(for (s in c(sn$raw, sn$harmonized, target))
    DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", s)),
    add = TRUE, after = FALSE)
  ids <- data.frame(subject_id = 1L, hadm_id = c(10L, 11L), stay_id = NA_integer_)
  DBI::dbWriteTable(con, DBI::Id(schema = sn$raw, table = "cohort_info"),
                    cbind(ids, ehr_id = c("1_10", "1_11"), source_data = "mimic_iv_demo"))
  DBI::dbWriteTable(con, DBI::Id(schema = sn$harmonized, table = "flat_tab"),
                    cbind(ids, age = c(50, 60)))
  merge <- function() suppressMessages(merge_sources(
    "mimic_iv_demo", db_name = Sys.getenv("EHR_TEST_DB"), base_dir = tempdir(),
    target_schema = target, merge_ts = FALSE, merge_dur = FALSE))
  merge()
  ages <- function() DBI::dbGetQuery(con, sprintf(
    "SELECT age FROM %s.flat ORDER BY src_admission_id", target))$age
  expect_identical(ages(), c("50", "60"))
  # flat and cohort_info rebuild, then the cohort_info_meta step fails.
  DBI::dbExecute(con, sprintf("UPDATE %s.flat_tab SET age = age + 1", sn$harmonized))
  DBI::dbWriteTable(con, DBI::Id(schema = sn$raw, table = "cohort_info_meta"),
                    data.frame(source_data = "mimic_iv_demo"))
  expect_error(merge(), "analysis_level")
  expect_identical(ages(), c("50", "60"))
})

test_that("merged flat keeps a column that only one source provides, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  srcs <- c("mimic_iv_demo", "eicu_demo")
  schemas <- c(unlist(lapply(srcs, function(x) unlist(src_schema_names(x)[c("raw", "harmonized")]))),
               "member_merge_flat_union")
  for (s in schemas) .member_reset_schema(con, s)
  on.exit(for (s in schemas) DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", s)),
          add = TRUE, after = FALSE)
  put <- function(schema, table, df)
    DBI::dbWriteTable(con, DBI::Id(schema = schema, table = table), df)
  mimic <- data.frame(subject_id = 1L, hadm_id = 10L, stay_id = NA_integer_)
  eicu <- data.frame(uniquepid = "P", patienthealthsystemstayid = 20L, patientunitstayid = 200L)
  put("mimic_iv_demo_coh", "cohort_info", cbind(mimic, ehr_id = "1_10", source_data = "mimic_iv_demo"))
  put("eicu_demo_coh", "cohort_info", cbind(eicu, ehr_id = "P_20", source_data = "eicu_demo"))
  put("mimic_iv_demo_coh_harmonized", "flat_tab", cbind(mimic, age = 50, insurance = "Medicare"))
  put("eicu_demo_coh_harmonized", "flat_tab", cbind(eicu, age = 60))
  expect_message(
    merge_sources(srcs, db_name = Sys.getenv("EHR_TEST_DB"), base_dir = tempdir(),
                  target_schema = "member_merge_flat_union", merge_ts = FALSE,
                  merge_dur = FALSE, merge_cohort = FALSE),
    "1 flat column\\(s\\) are NULL for eicu_demo")
  flat <- DBI::dbGetQuery(con, "SELECT source_data, age, insurance
                                FROM member_merge_flat_union.flat ORDER BY source_data")
  expect_identical(flat$age, c("60", "50"))
  expect_identical(flat$insurance, c(NA, "Medicare"))
})

test_that("merged ts keeps value numeric, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sn <- src_schema_names("mimic_iv_demo")
  target <- "member_merge_value_type"
  for (s in c(sn$harmonized, target)) .member_reset_schema(con, s)
  on.exit(for (s in c(sn$harmonized, target))
    DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", s)),
    add = TRUE, after = FALSE)
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s.ts_tab AS SELECT 1 AS subject_id, 10 AS hadm_id,
       CAST(NULL AS integer) AS stay_id, 'heart_rate' AS concept_name,
       CAST(72.125 AS NUMERIC(18,3)) AS value", sn$harmonized))
  suppressMessages(merge_sources(
    "mimic_iv_demo", db_name = Sys.getenv("EHR_TEST_DB"), base_dir = tempdir(),
    target_schema = target, merge_dur = FALSE, merge_flat = FALSE, merge_cohort = FALSE))
  type <- DBI::dbGetQuery(con, sprintf(
    "SELECT data_type FROM information_schema.columns
     WHERE table_schema = '%s' AND table_name = 'ts' AND column_name = 'value'", target))
  expect_identical(type$data_type, "numeric")
  expect_identical(DBI::dbGetQuery(con, sprintf("SELECT value FROM %s.ts", target))$value, 72.125)
})

test_that("merge_sources carries the membership grain per source, live DB", {
  con <- .member_pg_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  srcs <- c("mimic_iv_demo", "eicu_demo")
  target <- "member_merge_test"
  for (s in c(vapply(srcs, function(x) src_schema_names(x)$raw, ""), target))
    .member_reset_schema(con, s)
  on.exit(for (s in c(vapply(srcs, function(x) src_schema_names(x)$raw, ""), target))
    DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", s)),
    add = TRUE, after = FALSE)
  put <- function(schema, table, df)
    DBI::dbWriteTable(con, DBI::Id(schema = schema, table = table), df)
  # Both sources use the local ICU id 100; only the MIMIC stay is a member.
  put("mimic_iv_demo_coh", "cohort_info", data.frame(
    ehr_id = "1_10", subject_id = 1L, hadm_id = 10L, stay_id = 100L,
    cohort_index = TRUE, source_data = "mimic_iv_demo"))
  put("mimic_iv_demo_coh", "cohort_info_meta",
      .cohort_info_meta_table("mimic_iv_demo", "icu"))
  put("eicu_demo_coh", "cohort_info", data.frame(
    ehr_id = "P_10", uniquepid = "P", patienthealthsystemstayid = 10L,
    patientunitstayid = 100L, cohort_index = FALSE, source_data = "eicu_demo"))
  merge <- function(...) suppressMessages(merge_sources(
    ..., db_name = Sys.getenv("EHR_TEST_DB"), base_dir = tempdir(),
    target_schema = target, merge_ts = FALSE, merge_dur = FALSE,
    merge_flat = FALSE, analysis_level = "icu"))
  merge(srcs)
  meta <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s.cohort_info_meta", target))
  expect_identical(meta$source_data, "mimic_iv_demo")
  ci <- load_merged_cohort_info(target, con = con)
  expect_identical(paste(ci$source_data, ci$src_icu_id), "mimic_iv_demo 100")
  # eICU has no recorded grain (legacy cohort_info): ICU filtering refuses it.
  expect_error(.ehr_require_icu_membership(con, target), "for: eicu_demo")
  # Appending one source keeps the other source's grain row.
  put("eicu_demo_coh", "cohort_info_meta", .cohort_info_meta_table("eicu_demo", "icu"))
  merge("eicu_demo", append = TRUE)
  meta <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM %s.cohort_info_meta ORDER BY source_data", target))
  expect_identical(meta$source_data, c("eicu_demo", "mimic_iv_demo"))
  expect_silent(.ehr_require_icu_membership(con, target))
})

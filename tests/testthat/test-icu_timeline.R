# Unit tests for the ICU-timeline SQL builder. These exercise .build_icu_timeline_body
# directly so no Postgres connection is required: we assert on the generated SQL.

keep_cols <- c("uniquepid", "patienthealthsystemstayid", "time_since_hadm_min",
               "time_precision", "concept_name", "value", "unit")

eicu_body <- function(...) {
  args <- list(
    fam = "eicu", src_fqtn = "eicu_demo_coh_harmonized.ts_tab",
    raw = "eicu_demo_coh", icu_id_col = "patientunitstayid", icu_id_type = "bigint",
    keep_cols = keep_cols, window_before_hours = 0, window_after_hours = 0,
    include_other_icu_events = FALSE, overlap = "first",
    include_date_only_events = FALSE, date_time_since_icu = "null"
  )
  do.call(.build_icu_timeline_body, modifyList(args, list(...)))
}

test_that("eICU timeline builds windowed SQL, not a same-stay pass-through", {
  sql <- eicu_body()
  # The windowing CTEs must be present (this is what the old pass-through lacked).
  expect_match(sql, "WITH src AS", fixed = TRUE)
  expect_match(sql, "winb AS", fixed = TRUE)
  expect_match(sql, "win_start_min", fixed = TRUE)
  expect_match(sql, "win_end_min", fixed = TRUE)
  # Assignment must be computed (event_origin can take any of the four labels),
  # not hard-coded to same_icu/in_icu.
  expect_match(sql, "'other_icu'", fixed = TRUE)
  expect_match(sql, "'unassigned_icu'", fixed = TRUE)
})

test_that("eICU groups on patienthealthsystemstayid and uses offset bounds (no EXTRACT)", {
  sql <- eicu_body()
  expect_match(sql, "b.patienthealthsystemstayid = s.patienthealthsystemstayid", fixed = TRUE)
  # eICU windows are already numeric minute offsets -> no timestamp arithmetic.
  expect_false(grepl("EXTRACT(EPOCH", sql, fixed = TRUE))
  expect_match(sql, "icu_intime  AS intime_min", fixed = TRUE)
  expect_match(sql, "icu_outtime AS outtime_min", fixed = TRUE)
})

test_that("MIMIC still groups on hadm_id and uses timestamp arithmetic", {
  sql <- .build_icu_timeline_body(
    fam = "mimic_iv", src_fqtn = "mimic_iv_demo_coh_harmonized.ts_tab",
    raw = "mimic_iv_demo_coh", icu_id_col = "stay_id", icu_id_type = "bigint",
    keep_cols = c("subject_id", "hadm_id", "time_since_hadm_min", "time_precision",
                  "concept_name", "value", "unit"),
    window_before_hours = 0, window_after_hours = 0,
    include_other_icu_events = FALSE, overlap = "first",
    include_date_only_events = FALSE, date_time_since_icu = "null"
  )
  expect_match(sql, "b.hadm_id = s.hadm_id", fixed = TRUE)
  expect_match(sql, "EXTRACT(EPOCH FROM (icu_intime  - admittime))", fixed = TRUE)
})

test_that("window padding flows into win_start/win_end for eICU", {
  sql <- eicu_body(window_before_hours = 6, window_after_hours = 3)
  # 6h before -> -360 min on win_start; 3h after -> +180 min on win_end.
  expect_match(sql, "intime_min + -360 AS win_start_min", fixed = TRUE)
  expect_match(sql, "outtime_min + 180 AS win_end_min", fixed = TRUE)
})

test_that("include_other_icu_events relaxes the assignment filter for eICU", {
  restricted <- eicu_body(include_other_icu_events = FALSE)
  expect_match(restricted, "IS NOT DISTINCT FROM b.assigned_icustay_id", fixed = TRUE)

  relaxed <- eicu_body(include_other_icu_events = TRUE, overlap = "duplicate")
  # With other-ICU events allowed the per-row filter becomes the literal TRUE
  # guard, so cross-stay rows survive the window join.
  expect_match(relaxed, "AND (TRUE)", fixed = TRUE)
})

test_that("overlap=duplicate keeps all window matches; first dedups per source row", {
  dup   <- eicu_body(include_other_icu_events = TRUE, overlap = "duplicate")
  first <- eicu_body(include_other_icu_events = FALSE, overlap = "first")
  expect_false(grepl("DISTINCT ON (src_row_id)", dup, fixed = TRUE))
  expect_match(first, "DISTINCT ON (src_row_id)", fixed = TRUE)
})

test_that("first and nearest preserve identity priority and end with ICU-ID tie-break", {
  first <- eicu_body(include_other_icu_events = TRUE, overlap = "first")
  nearest <- eicu_body(include_other_icu_events = TRUE, overlap = "nearest")

  expect_match(
    first,
    "projection_rank ASC, w_intime ASC, assigned_icustay_id ASC",
    fixed = TRUE
  )
  expect_match(
    nearest,
    paste0(
      "projection_rank ASC, dist ASC, w_intime ASC, ",
      "assigned_icustay_id ASC"
    ),
    fixed = TRUE
  )
})

test_that("timeline identifier and ICU-ID type contracts are narrow", {
  expect_equal(.ehr_validate_identifier("ts_icu_2", "table"), "ts_icu_2")
  expect_error(
    .ehr_validate_identifier("ts_icu; DROP TABLE x", "table"),
    "portable SQL identifier"
  )
  expect_equal(.ehr_icu_id_sql_type("int8"), "bigint")
  expect_equal(.ehr_icu_id_sql_type("VARCHAR"), "text")
  expect_error(.ehr_icu_id_sql_type("bigint); DROP TABLE x"), "must map")
})

test_that("unsupported timeline families are rejected explicitly", {
  expect_equal(.icu_timeline_adm_key("eicu"), "patienthealthsystemstayid")
  expect_equal(.icu_timeline_adm_key("mimic_iii"), "hadm_id")
  expect_error(.icu_timeline_adm_key("custom"), "supports only")
})

test_that("public timeline controls fail before configuration or connection", {
  expect_error(
    make_icu_timeline("mimic_iv_demo", window_before_hours = Inf),
    "finite non-negative"
  )
  expect_error(
    make_icu_timeline("mimic_iv_demo", window_after_hours = c(1, 2)),
    "finite non-negative"
  )
  expect_error(
    make_icu_timeline("mimic_iv_demo", materialize = NA),
    "materialize must be TRUE or FALSE"
  )
  expect_error(
    suppressWarnings(make_icu_timeline(
      "mimic_iv_demo", icustay_id = "stay_id", target_id = "other_id")),
    "must agree"
  )
  expect_error(
    suppressWarnings(make_icu_timeline(
      "mimic_iv_demo",
      include_other_icu_events = FALSE,
      include_other_icu_stays = TRUE
    )),
    "must agree"
  )
  expect_error(
    make_icu_timeline("mimic_iv_demo", out_table = "ts_icu;drop"),
    "portable SQL identifier"
  )
})

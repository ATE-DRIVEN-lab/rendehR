test_that("empty comorbidity maps retain their fixed schemas", {
  charlson <- .empty_comorbidity_map("hadm_id", "charlson")
  elixhauser <- .empty_comorbidity_map("hadm_id", "elixhauser")
  expect_named(charlson,
    c("hadm_id", paste0("charlson_", .comorbidity_flag_columns("charlson")),
      "charlson_score"))
  expect_named(elixhauser,
    c("hadm_id", paste0("elixhauser_", .comorbidity_flag_columns("elixhauser")),
      "elixhauser_score"))
  typed <- .empty_comorbidity_map("hadm_id", "charlson", integer())
  expect_type(typed$hadm_id, "integer")
})

test_that("scores weight only the severe form when mild and severe are both present", {
  skip_if_not_installed("comorbidity")
  diag <- data.frame(
    hadm_id = c(1, 1, 2, 3),
    icd_code = c("5715", "I850", "I850", "5715"),
    icd_version = c("9", "10", "10", "9"))
  got <- .comorbidity_one_map(diag, "hadm_id", "charlson")
  got <- got[order(got$hadm_id), ]
  expect_identical(got$charlson_mld, c(1L, 0L, 1L))
  expect_identical(got$charlson_msld, c(1L, 1L, 0L))
  expect_identical(got$charlson_score, c(4L, 4L, 2L))
})

test_that("hospital comorbidities retain ICU-less admissions and inherit to ICUs", {
  universe <- data.frame(
    subject_id = c(1, 1, 2), hadm_id = c(10, 10, 20),
    stay_id = c(100, 101, NA_integer_))
  diag <- data.frame(
    hadm_id = c(10, 20), icd_code_raw = c("4100", NA_character_),
    icd_code = c("4100", NA_character_), icd_version = c("9", "9"))
  fake <- data.frame(hadm_id = 10L, charlson_mi = 1L,
                     charlson_score = 1L)
  local_mocked_bindings(.run_all_maps = function(...) fake)

  got <- .finalize_comorbidities(universe, diag, "hadm_id", "charlson")
  expect_equal(nrow(got), 3L)
  expect_identical(got$charlson_score[1:2], c(1L, 1L))
  expect_true(is.na(got$stay_id[3]))
  expect_identical(got$comorbidity_status[3], "missing_diagnosis")
  expect_true(is.na(got$charlson_score[3]))
})

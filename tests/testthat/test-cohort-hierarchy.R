.toy_visit_windows <- function() {
  data.frame(
    subject_id = c(
      1L, 1L, 1L, 1L,
      2L, 2L, 2L, 2L,
      3L, 3L, 3L
    ),
    hadm_id = c(
      10L, 10L, 11L, 11L,
      20L, 20L, 20L, 21L,
      30L, 31L, 31L
    ),
    stay_id = c(
      100L, 101L, 102L, 103L,
      200L, 201L, 202L, 203L,
      NA_integer_, 300L, 301L
    ),
    admittime = as.POSIXct("2020-01-01", tz = "UTC") + 1:11,
    dischtime = as.POSIXct("2020-01-02", tz = "UTC") + 1:11,
    icu_intime = as.POSIXct("2020-01-01", tz = "UTC") + 1:11,
    icu_outtime = as.POSIXct("2020-01-02", tz = "UTC") + 1:11,
    icu_los_min = rep(1440, 11),
    stringsAsFactors = FALSE
  )
}

.toy_hierarchy_result <- function(plan) {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "ehr_visit_windows_full", .toy_visit_windows(),
    temporary = TRUE, overwrite = TRUE
  )
  result <- .ehr_build_cohort_hierarchy(
    con, "test_coh", plan, c("subject_id", "hadm_id", "stay_id")
  )
  hierarchy <- DBI::dbGetQuery(
    con, "SELECT * FROM test_coh.cohort_hierarchy
          ORDER BY subject_id, hadm_id, stay_id NULLS FIRST"
  )
  list(result = result, hierarchy = hierarchy)
}

test_that("visit_windows marks the imported scope with in_scope", {
  skip_if_not_installed("duckdb")
  windows_for <- function(scope) {
    con <- connect_duckdb(":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "ehr_visit_windows_full", .toy_visit_windows(),
                      temporary = TRUE, overwrite = TRUE)
    plan <- .ehr_selection_plan(
      "mimic_iv_demo", "direct_ids", "stay_id",
      requested_scope = scope, effective_scope = scope, values = "100")
    .ehr_build_cohort_hierarchy(con, "test_coh", plan,
                                c("subject_id", "hadm_id", "stay_id"))
    DBI::dbGetQuery(con,
      "SELECT v.stay_id, v.in_scope, h.scope_index
         FROM test_coh.visit_windows v JOIN test_coh.cohort_hierarchy h
           USING (subject_id, hadm_id, stay_id) ORDER BY v.stay_id")
  }
  icu <- windows_for("stay_id")
  expect_equal(icu$stay_id, c(100L, 101L, 102L, 103L))
  expect_identical(icu$in_scope, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(icu$in_scope, icu$scope_index)
  expect_true(all(windows_for("subject_id")$in_scope))
})

test_that("ICU evidence keeps full patient context and three exact indexes", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "stay_id",
    requested_scope = "patient", effective_scope = "subject_id",
    values = "100"
  )
  hierarchy <- .toy_hierarchy_result(plan)$hierarchy
  expect_equal(nrow(hierarchy), 4L)
  expect_true(all(hierarchy$patient_context_index))
  expect_equal(sum(hierarchy$definition_filter_index), 1L)
  expect_true(all(hierarchy$scope_index))
  expect_equal(
    hierarchy$stay_id[hierarchy$definition_filter_index],
    100L
  )
})

test_that("ICU scope narrows scope without losing patient context", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "stay_id",
    requested_scope = "icu", effective_scope = "stay_id",
    values = "100"
  )
  hierarchy <- .toy_hierarchy_result(plan)$hierarchy
  expect_equal(nrow(hierarchy), 4L)
  expect_equal(sum(hierarchy$definition_filter_index), 1L)
  expect_equal(sum(hierarchy$scope_index), 1L)
  expect_identical(
    as.logical(hierarchy$scope_index),
    as.logical(hierarchy$definition_filter_index)
  )
})

test_that("ICU scope indexes no-ICU context rows as false, never missing", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "stay_id",
    requested_scope = "icu", effective_scope = "stay_id",
    values = "300"
  )
  hierarchy <- .toy_hierarchy_result(plan)$hierarchy
  no_icu <- is.na(hierarchy$stay_id)

  expect_equal(nrow(hierarchy), 3L)
  expect_true(any(no_icu))
  expect_false(anyNA(hierarchy$definition_filter_index))
  expect_false(anyNA(hierarchy$scope_index))
  expect_true(all(!hierarchy$definition_filter_index[no_icu]))
  expect_true(all(!hierarchy$scope_index[no_icu]))
  expect_equal(sum(hierarchy$definition_filter_index), 1L)
  expect_equal(sum(hierarchy$scope_index), 1L)
})

test_that("ICU selection supports patient admission and ICU effective scopes", {
  expected_scope_rows <- c(subject_id = 4L, hadm_id = 2L, stay_id = 1L)
  for (scope in names(expected_scope_rows)) {
    plan <- .ehr_selection_plan(
      "mimic_iv_demo", "direct_ids", "stay_id",
      requested_scope = scope, effective_scope = scope, values = "100"
    )
    hierarchy <- .toy_hierarchy_result(plan)$hierarchy
    expect_equal(sum(hierarchy$scope_index), expected_scope_rows[[scope]])
    expect_equal(sum(hierarchy$definition_filter_index), 1L)
    expect_true(all(
      hierarchy$scope_index <= hierarchy$patient_context_index
    ))
  }
})

test_that("admission evidence includes its ICU descendants", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "hadm_id",
    requested_scope = "admission", effective_scope = "hadm_id",
    values = "10"
  )
  hierarchy <- .toy_hierarchy_result(plan)$hierarchy
  expect_equal(nrow(hierarchy), 4L)
  expect_equal(sum(hierarchy$definition_filter_index), 2L)
  expect_equal(sum(hierarchy$scope_index), 2L)
  expect_true(all(hierarchy$hadm_id[hierarchy$scope_index] == 10L))
})

test_that("admission selection supports patient and admission effective scopes", {
  patient <- .toy_hierarchy_result(.ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "hadm_id",
    effective_scope = "subject_id", values = "10"
  ))$hierarchy
  admission <- .toy_hierarchy_result(.ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "hadm_id",
    effective_scope = "hadm_id", values = "10"
  ))$hierarchy
  expect_equal(sum(patient$scope_index), 4L)
  expect_equal(sum(admission$scope_index), 2L)
  expect_identical(
    as.logical(patient$definition_filter_index),
    as.logical(admission$definition_filter_index)
  )
})

test_that("patient evidence includes ICU-less admissions", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "subject_id",
    effective_scope = "subject_id", values = "3"
  )
  hierarchy <- .toy_hierarchy_result(plan)$hierarchy
  expect_equal(nrow(hierarchy), 3L)
  expect_true(any(is.na(hierarchy$stay_id)))
  expect_true(all(hierarchy$definition_filter_index))
  expect_true(all(hierarchy$scope_index))
})

test_that("toy hierarchy has three patients six admissions and ten ICU stays", {
  toy <- .toy_visit_windows()
  expect_equal(length(unique(toy$subject_id)), 3L)
  expect_equal(nrow(unique(toy[c("subject_id", "hadm_id")])), 6L)
  expect_equal(length(unique(stats::na.omit(toy$stay_id))), 10L)
  expect_equal(nrow(unique(toy[c("subject_id", "hadm_id", "stay_id")])),
               nrow(toy))
})

test_that("two-level custom hierarchy uses the same index contract", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con, "ehr_visit_windows_full",
    data.frame(patientid = c(1L, 1L, 2L), admissionid = c(10L, 11L, 20L)),
    temporary = TRUE, overwrite = TRUE
  )
  plan <- .ehr_selection_plan(
    "custom", "direct_ids", "admissionid",
    effective_scope = "patientid", values = "10"
  )
  .ehr_build_cohort_hierarchy(
    con, "custom_coh", plan, c("patientid", "admissionid")
  )
  hierarchy <- DBI::dbGetQuery(
    con, "SELECT * FROM custom_coh.cohort_hierarchy ORDER BY admissionid"
  )
  expect_equal(nrow(hierarchy), 2L)
  expect_true(all(hierarchy$patient_context_index))
  expect_equal(sum(hierarchy$definition_filter_index), 1L)
  expect_true(all(hierarchy$scope_index))
})

test_that("unmatched identifiers fail unless partial matching is deliberate", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "stay_id",
    effective_scope = "subject_id", values = c("100", "999")
  )
  expect_error(
    .toy_hierarchy_result(plan),
    "1 of 2 selected identifier"
  )

  plan$allow_unmatched_ids <- TRUE
  result <- .toy_hierarchy_result(plan)$result
  expect_equal(result$plan$matched_count, 1L)
  expect_equal(result$plan$unmatched_count, 1L)
})

test_that("invalid identifier casts and wholly unmatched selections fail safely", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "stay_id",
    effective_scope = "subject_id", values = "not-an-integer"
  )
  expect_error(
    .toy_hierarchy_result(plan),
    "cannot be converted"
  )
  plan$values <- "999"
  plan$allow_unmatched_ids <- TRUE
  expect_error(
    .toy_hierarchy_result(plan),
    "No selected identifier matched"
  )
})

test_that("clinical tables require a configured hierarchy link", {
  clinical <- list(
    cohort_role = "clinical",
    cohort_link_key = "hadm_id",
    cols = list(hadm_id = list(name = "hadm_id", spec = "col_integer"))
  )
  expect_identical(
    .ehr_catalog_table_contract(
      clinical, c("subject_id", "hadm_id", "stay_id"), "events"
    )$link_key,
    "hadm_id"
  )
  clinical$cohort_link_key <- NULL
  expect_error(
    .ehr_catalog_table_contract(
      clinical, c("subject_id", "hadm_id", "stay_id"), "events"
    ),
    "must declare cohort_link_key"
  )
})

test_that("declared table linkage controls filtering and references stay whole", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  clinical <- .ehr_table_filter_sql(
    con,
    list(role = "clinical", link_key = "hadm_id"),
    "cohort_hierarchy", alias = "events"
  )
  reference <- .ehr_table_filter_sql(
    con,
    list(role = "reference", link_key = NULL),
    "cohort_hierarchy", alias = "dictionary"
  )
  expect_true(clinical$filtered)
  expect_identical(clinical$link_key, "hadm_id")
  expect_match(clinical$where, "scope_index", fixed = TRUE)
  expect_false(reference$filtered)
  expect_identical(reference$where, "")
})

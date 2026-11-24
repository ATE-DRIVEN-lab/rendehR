catalog_path <- function() {
  installed <- system.file("extdata", "rendehr-data-source.json", package = "rendehR")
  if (nzchar(installed)) installed else
    file.path(test_path("..", ".."), "inst", "extdata",
              "rendehr-data-source.json")
}

test_that("canonical catalog has the approved inventory and releases", {
  cfg <- .ehr_load_source_catalog(catalog_path())
  expect_identical(
    names(cfg),
    c("eicu", "eicu_demo", "mimic_iii", "mimic_iii_demo",
      "mimic_iv", "mimic_iv_demo", "hirid", "aumc", "sic")
  )
  expect_equal(cfg$mimic_iv$release, "3.1")
  expect_equal(cfg$mimic_iv_demo$release, "2.2")
  expect_identical(cfg$mimic_iv_demo$status, "available_definition")
  expect_equal(cfg$mimic_iv_demo$url, "https://physionet.org/files/mimic-iv-demo/2.2")
})

test_that("catalog projection treats case and order as benign", {
  tbl <- list(cols = list(
    subject_id = list(name = "SUBJECT_ID", spec = "col_integer"),
    charttime = list(name = "CHARTTIME", spec = "col_datetime")
  ))
  plan <- .ehr_catalog_projection_plan(c("CHARTTIME", "SUBJECT_ID"), tbl)
  expect_true(plan$case_or_order_only)
  expect_length(plan$missing_catalog, 0L)
  expect_length(plan$unexpected_actual, 0L)
  expect_identical(plan$retained, c("subject_id", "charttime"))
})

test_that("catalog projection reports missing and unexpected columns", {
  tbl <- list(cols = list(
    subject_id = list(name = "subject_id", spec = "col_integer"),
    value = list(name = "value", spec = "col_double")
  ))
  report <- .ehr_catalog_validation_report(
    c("subject_id", "vendor_extra"), tbl,
    conversion_failures = c(subject_id = 0L, value = 3L)
  )
  expect_identical(report$plan$missing_catalog, "value")
  expect_identical(report$plan$unexpected_actual, "vendor_extra")
  expect_equal(unname(report$conversion_failures), c(0L, 3L))
  expect_false(report$should_stop)
})

test_that("catalog projection maps true raw renames to canonical names", {
  tbl <- list(cols = list(
    apneaparams = list(name = "apneaparms", spec = "col_character"),
    patientunitstayid = list(
      name = "patientunitstayid", spec = "col_integer"
    )
  ))
  plan <- .ehr_catalog_projection_plan(
    c("patientunitstayid", "apneaparms"), tbl
  )
  expect_identical(
    unname(plan$raw_names[c("apneaparams", "patientunitstayid")]),
    c("apneaparms", "patientunitstayid")
  )
  expect_identical(plan$matched_catalog,
                   c("apneaparams", "patientunitstayid"))
  expect_length(plan$missing_catalog, 0L)
})

test_that("required hierarchy and linkage conversion failures stop safely", {
  table_cfg <- list(
    cohort_role = "clinical",
    cohort_link_key = "subject_id",
    cols = list(
      subject_id = list(name = "raw_subject", spec = "col_integer"),
      value = list(name = "value", spec = "col_double")
    )
  )
  report <- data.frame(
    column = c("subject_id", "value"),
    conversion_failures = c(1, 2),
    status = "configured",
    stringsAsFactors = FALSE
  )
  expect_error(
    .ehr_assert_required_projection(
      report, table_cfg, "subject_id", "events"
    ),
    "required hierarchy/linkage"
  )
  report$conversion_failures[[1L]] <- 0
  expect_silent(.ehr_assert_required_projection(
    report, table_cfg, "subject_id", "events"
  ))
})

test_that("every catalog table declares a safe cohort linkage contract", {
  cfg <- .ehr_load_source_catalog(catalog_path())
  ladders <- list(
    eicu = c("uniquepid", "patienthealthsystemstayid",
             "patientunitstayid"),
    mimic_iii = c("subject_id", "hadm_id", "icustay_id"),
    mimic_iv = c("subject_id", "hadm_id", "stay_id"),
    hirid = "patientid",
    aumc = c("patientid", "admissionid"),
    sic = c("patientid", "caseid")
  )
  for (source in names(cfg)) {
    family <- if (grepl("^eicu", source)) "eicu" else
      if (grepl("^mimic_iii", source)) "mimic_iii" else
      if (source %in% c("mimic_iv", "mimic_iv_demo")) "mimic_iv" else
      source
    contracts <- .ehr_validate_catalog_linkage(
      cfg[[source]], ladders[[family]]
    )
    expect_length(contracts, length(cfg[[source]]$tables))
    expect_true(all(vapply(
      contracts,
      function(x) x$role %in% c("clinical", "reference"),
      logical(1L)
    )))
  }
})

test_that("runtime catalog is generated deterministically", {
  script <- file.path(test_path("..", ".."), "data-raw",
                      "generate-rendehr-data-source.R")
  skip_if_not(file.exists(script), "source-only generator is excluded from installed package")
  catalog <- catalog_path()
  before <- readBin(catalog, "raw", n = file.info(catalog)$size)
  status <- system2(file.path(R.home("bin"), "Rscript"), script,
                    stdout = TRUE, stderr = TRUE)
  expect_null(attr(status, "status"))
  after <- readBin(catalog, "raw", n = file.info(catalog)$size)
  expect_identical(after, before)
})

test_that("64-bit eICU row ids are typed BIGINT from the catalog", {
  expect_identical(.ehr_spec_duckdb_type("col_big_integer"), "BIGINT")
  expect_identical(unname(.duckdb_type_map["col_big_integer"]), "BIGINT")
  for (src in c("eicu", "eicu_demo")) {
    cols <- load_src_cfg(catalog_path(), src)$tables$nursecharting$cols
    expect_identical(cols$nursingchartid$spec, "col_big_integer")
    sch <- .duckdb_schema_from_cfg(load_src_cfg(catalog_path(), src))
    expect_identical(unname(sch$vitalperiodic["vitalperiodicid"]), "BIGINT")
  }
})

test_that("MIMIC-III admission-level tables link the cohort on hadm_id, as in MIMIC-IV", {
  # Ward prescriptions and transfers carry no icustay_id; linking them on
  # icustay_id dropped them from every filtered cohort import.
  cat <- jsonlite::read_json(system.file("extdata", "rendehr-data-source.json", package = "rendehR"))
  tabs <- function(s) { x <- cat[[which(vapply(cat, `[[`, "", "name") == s)]]$tables
                        if (is.null(names(x))) stats::setNames(x, vapply(x, `[[`, "", "name")) else x }
  for (s in c("mimic_iii", "mimic_iii_demo", "mimic_iv", "mimic_iv_demo"))
    for (t in c("prescriptions", "transfers"))
      expect_identical(tabs(s)[[t]]$cohort_link_key, "hadm_id", info = paste(s, t))
})

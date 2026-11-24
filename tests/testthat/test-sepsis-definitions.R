test_that("the Sepsis-3 chain ends with the default, absolute or trauma step", {
  for (fam in c("mimic_iii", "mimic_iv", "eicu")) {
    def <- .ehr_sepsis3_setup_sql(fam)
    expect_match(basename(utils::tail(def, 1)), "_sepsis3\\.sql$", info = fam)
    expect_match(paste(readLines(utils::tail(def, 1)), collapse = "\n"), "sofa_rise >= 2", fixed = TRUE)
    abs <- .ehr_sepsis3_setup_sql(fam, "absolute")
    expect_identical(basename(utils::tail(abs, 1)), "sepsis3_absolute.sql", info = fam)
    expect_identical(utils::head(abs, -1), utils::head(def, -1))
    expect_false(any(grepl("variants", utils::head(def, -1))))
  }
  for (fam in c("mimic_iii", "mimic_iv")) {
    tr <- .ehr_sepsis3_setup_sql(fam, bundle = "trauma_sepsis3")
    expect_identical(basename(utils::tail(tr, 1)), "trauma_sepsis3.sql", info = fam)
    expect_identical(.ehr_sepsis3_cohort_spec(fam, "trauma_sepsis3")$cohort_sql,
                     .ehr_sepsis3_cohort_spec(fam, "sepsis3")$cohort_sql)
  }
  expect_error(.ehr_sepsis3_cohort_spec("eicu", "trauma_sepsis3"), "MIMIC-III and MIMIC-IV only")
  expect_error(.ehr_sepsis3_setup_sql("eicu", "nonsense"))
})

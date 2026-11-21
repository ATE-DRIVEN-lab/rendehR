test_that("public cohort API surface matches the approved allowlist", {
  exports <- getNamespaceExports("rendehR")
  retained <- c(
    "select_cohort_ids",
    "list_example_cohorts",
    "cohort_key",
    "import_cohort_general",
    "materialize_cohort_duckdb",
    "subset_parquet_by_key",
    "build_eicu_diagnosis_icd_parquet"
  )
  retired <- c(
    "import_cohort",
    "import_cohort_flex",
    "import_source_data",
    "select_cohort_ids_duckdb",
    "run_cohort_duckdb",
    "subset_source_to_cohort"
  )
  expect_true(all(retained %in% exports))
  expect_false(any(retired %in% exports))
})

test_that("materializers retain tables and exclude retired destructive args", {
  parquet_args <- names(formals(import_cohort_general))
  duckdb_args <- names(formals(materialize_cohort_duckdb))
  expect_true("tables" %in% parquet_args)
  expect_true("tables" %in% duckdb_args)
  expect_true("raw_data_root" %in% parquet_args)
  expect_true("raw_data_root" %in% duckdb_args)
  expect_false(any(c(
    "delete_source_csv", "chunk_length", "write_parquet"
  ) %in% parquet_args))
})

test_that("cohort documentation has one canonical detailed vignette", {
  root <- test_path("..", "..")
  skip_if_not(
    file.exists(file.path(root, "NAMESPACE")),
    "source-only documentation layout is unavailable in the installed package"
  )
  expect_true(file.exists(file.path(
    root, "vignettes", "cohorts.Rmd"
  )))
  expect_false(file.exists(file.path(
    root, "vignettes", "cohort-selection-materialization.Rmd"
  )))
  expect_false(file.exists(file.path(
    root, "vignettes", "ehr-user-guide-fixed.Rmd"
  )))
  namespace <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  expect_false(any(grepl(
    "import_cohort\\)|run_cohort_duckdb\\)|select_cohort_ids_duckdb\\)",
    namespace
  )))
})

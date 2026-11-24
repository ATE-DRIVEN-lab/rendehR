test_that("direct identifier formats share one strict character contract", {
  skip_if_not_installed("arrow")
  values <- data.frame(subject_id = c("001", "002", "002"))
  csv <- tempfile(fileext = ".csv")
  csv_gz <- tempfile(fileext = ".csv.gz")
  parquet <- tempfile(fileext = ".parquet")
  rds <- tempfile(fileext = ".rds")
  readr::write_csv(values, csv)
  readr::write_csv(values, csv_gz)
  arrow::write_parquet(values, parquet)
  saveRDS(values, rds)

  results <- lapply(
    c(csv, csv_gz, parquet, rds),
    .ehr_read_direct_ids, key = "subject_id", src_name = "mimic_iv_demo"
  )
  expect_true(all(vapply(results, function(x)
    identical(x$subject_id, c("001", "002")), logical(1L))))
  expect_true(all(vapply(results, function(x)
    identical(typeof(x$subject_id), "character"), logical(1L))))
})

test_that("direct identifiers reject missing blank malformed structures", {
  blank <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(subject_id = c("1", "", NA)), blank)
  expect_error(
    .ehr_read_direct_ids(blank, "subject_id"),
    "missing or blank identifier"
  )

  missing_key <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(hadm_id = "10"), missing_key)
  expect_error(
    .ehr_read_direct_ids(missing_key, "subject_id"),
    "must contain the column"
  )

  bad_rds <- tempfile(fileext = ".rds")
  saveRDS(list(a = list(1)), bad_rds)
  expect_error(.ehr_read_direct_ids(bad_rds, "subject_id"),
               "data frame or vector")
})

test_that("direct key inference is explicit and unambiguous", {
  ladder <- c("subject_id", "hadm_id", "stay_id")
  one <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(stay_id = "100", note = "private"), one)
  expect_identical(.ehr_infer_direct_id_key(one, ladder), "stay_id")

  many <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(subject_id = "1", hadm_id = "10"), many)
  expect_error(.ehr_infer_direct_id_key(many, ladder),
               "multiple hierarchy columns")
  expect_identical(.ehr_infer_direct_id_key(many, ladder, "hadm_id"),
                   "hadm_id")
})

test_that("selection plan diagnostics are aggregate and values stay private", {
  plan <- .ehr_selection_plan(
    "mimic_iv_demo", "direct_ids", "stay_id",
    values = c("100", "999")
  )
  expect_s3_class(plan, "ehr_selection_plan")
  expect_identical(plan$requested_count, 2L)
  expect_false(grepl("100|999", paste(unlist(plan$metadata), collapse = " ")))
})

test_that("scope resolution cannot discard a coarser selected unit", {
  ladder <- c("subject_id", "hadm_id", "stay_id")
  expect_identical(
    .resolve_import_scope("icu", "subject_id", ladder, FALSE),
    "subject_id"
  )
  expect_identical(
    .resolve_import_scope("icu", "hadm_id", ladder, FALSE),
    "hadm_id"
  )
  expect_identical(
    .resolve_import_scope("icu", "stay_id", ladder, FALSE),
    "stay_id"
  )
})

test_that("SQL setup and R vector or data-frame routes normalize identically", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  paths <- list(icd_filter_path = NULL)
  ladder <- c("subject_id", "hadm_id", "stay_id")

  sql <- .ehr_resolve_selection(
    con, "mimic_iv_demo", paths, ladder,
    setup_sql = c(
      "CREATE TEMP TABLE chosen(stay_id BIGINT)",
      "INSERT INTO chosen VALUES (100)"
    ),
    cohort_sql = "SELECT stay_id FROM chosen",
    key = "stay_id"
  )
  vector <- .ehr_resolve_selection(
    con, "mimic_iv_demo", paths, ladder,
    cohort_fn = function(paths) c("100", "100"), key = "stay_id"
  )
  frame <- .ehr_resolve_selection(
    con, "mimic_iv_demo", paths, ladder,
    cohort_fn = function(paths) data.frame(stay_id = c("100", "100")),
    key = "stay_id"
  )

  expect_identical(sql$values, "100")
  expect_identical(vector$values, "100")
  expect_identical(frame$values, "100")
  expect_identical(
    vapply(list(sql, vector, frame), `[[`, "", "selection_grain"),
    rep("stay_id", 3)
  )
})

test_that("single-file parquet subsetting is strict, bounded, and atomic", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("arrow")
  input <- tempfile(fileext = ".parquet")
  output <- tempfile(fileext = ".parquet")
  unlink(output)
  arrow::write_parquet(
    data.frame(subject_id = c(1L, 2L, 3L), value = letters[1:3]),
    input
  )
  report <- subset_parquet_by_key(
    input, output, "subject_id", c("1", "3")
  )
  expect_equal(report$requested, 2L)
  expect_equal(report$matched, 2)
  expect_equal(report$rows, 2)
  expect_equal(
    as.data.frame(arrow::read_parquet(output))$subject_id,
    c(1L, 3L)
  )
  expect_error(
    subset_parquet_by_key(
      input, tempfile(fileext = ".parquet"), "subject_id", c("1", "999")
    ),
    "1 of 2"
  )
  expect_error(
    subset_parquet_by_key(
      input, tempfile(fileext = ".parquet"), "hadm_id", "1"
    ),
    "does not contain"
  )
})

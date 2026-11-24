test_that("the variable dictionary uses only the four types and covers every flat concept", {
  types <- .flat_concept_types()
  expect_true(all(types %in% c("numeric", "date", "logical", "character")))
  expect_false(anyDuplicated(names(types)) > 0)
  for (fam in c("mimic_iii", "mimic_iv", "eicu")) {
    dir <- system.file("sql", fam, "concepts_sqls", "flat", package = "rendehR")
    concepts <- sub("^eicu_", "", sub("\\.sql$", "", list.files(dir, "\\.sql$")))
    # echo_data holds one row per echo report, not one per admission, so it
    # never becomes a flat column.
    concepts <- setdiff(concepts, "echo_data")
    expect_identical(setdiff(concepts, names(types)), character(0), info = fam)
  }
  expect_identical(types[["age"]], "numeric")
  expect_identical(types[["hospital_mortality"]], "logical")
  expect_identical(types[["care_site"]], "character")
})

test_that("merged flat values are cast to their dictionary type", {
  skip_if_not_installed("duckdb")
  types <- c(age = "numeric", hospital_mortality = "logical", care_site = "character", d = "date")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  got <- DBI::dbGetQuery(con, paste(
    "SELECT", paste(c(
      .merge_typed_expr("ft.age", "age", types),
      .merge_typed_expr("ft.hospital_mortality", "hospital_mortality", types),
      .merge_typed_expr("ft.care_site", "care_site", types),
      .merge_typed_expr("ft.d", "d", types),
      .merge_typed_expr("NULL", "hospital_mortality", types),
      .merge_typed_expr("ft.other", "other", types)), collapse = ", "),
    "FROM (VALUES (64.3, 1, 73, '2150-01-02', 5)) AS ft(age, hospital_mortality, care_site, d, other)"))
  expect_type(got$age, "double"); expect_equal(got$age, 64.3)
  expect_identical(got[[2]], TRUE)
  expect_identical(got$care_site, "73")
  expect_s3_class(got$d, "Date")
  expect_true(is.na(got[[5]]))
  expect_identical(got$other, "5")
})

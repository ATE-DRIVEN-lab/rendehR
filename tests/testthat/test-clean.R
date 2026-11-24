test_that("clean_by_range: ranges data.frame flags values outside bounds", {
  df <- data.frame(
    concept_name = c("hr", "hr", "hr", "map"),
    value        = c(50, 300, 80, -5)
  )
  ranges <- data.frame(
    concept_name = c("hr", "map"),
    min_value    = c(30, 0),
    max_value    = c(200, 150)
  )
  out <- clean_by_range(df, ranges = ranges)
  expect_identical(out$out_of_range, c(FALSE, TRUE, FALSE, TRUE))
  expect_true(is.data.frame(attr(out, "clean_report")))
})

test_that("clean_by_range: ranges named list works the same as data.frame", {
  df <- data.frame(concept_name = "hr", value = 300)
  out_df   <- clean_by_range(df, ranges = data.frame(
    concept_name = "hr", min_value = 30, max_value = 200))
  out_list <- clean_by_range(df, ranges = list(hr = c(30, 200)))
  expect_identical(out_df$out_of_range, out_list$out_of_range)
})

test_that("clean_by_range: action = 'drop' removes flagged rows", {
  df <- data.frame(concept_name = c("hr", "hr"), value = c(50, 300))
  out <- clean_by_range(df, ranges = list(hr = c(30, 200)), action = "drop")
  expect_equal(nrow(out), 1)
  expect_equal(out$value, 50)
})

test_that("clean_by_range: action = 'na' nulls flagged values", {
  df <- data.frame(concept_name = c("hr", "hr"), value = c(50, 300))
  out <- clean_by_range(df, ranges = list(hr = c(30, 200)), action = "na")
  expect_identical(out$value, c(50, NA_real_))
})

test_that("clean_by_range: concept with no bound is left unchanged with a message", {
  df <- data.frame(concept_name = "unknown_concept", value = 999)
  expect_message(
    out <- clean_by_range(df, ranges = list(hr = c(30, 200))),
    "no min/max bound"
  )
  expect_false(out$out_of_range)
})

test_that("clean_by_range: single_concept fallback applies when concept_col is absent", {
  df <- data.frame(value = c(50, 300))
  out <- clean_by_range(df, ranges = list(hr = c(30, 200)),
                        concept_col = "concept_name", single_concept = "hr")
  expect_identical(out$out_of_range, c(FALSE, TRUE))
})

test_that("clean_by_range: a directly-supplied ranges data.frame does not merge duplicate rows (first match wins)", {
  # Unlike the config-CSV path (see next test), .resolve_range_bounds()'s
  # ranges-data.frame branch builds bounds via setNames(Map(...), concept_name)
  # with no per-concept min/max reduction, so a duplicate concept_name row is
  # shadowed, not merged, by list [[ ]] lookup semantics.
  df <- data.frame(concept_name = c("hr", "hr"), value = c(25, 210))
  ranges <- data.frame(
    concept_name = c("hr", "hr"),
    min_value    = c(30, 20),
    max_value    = c(180, 220)
  )
  out <- clean_by_range(df, ranges = ranges)
  expect_identical(out$out_of_range, c(TRUE, TRUE))
})

test_that("clean_by_range: config-CSV path uses the most-permissive window across duplicate rows", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(
    data.frame(concept_name = c("hr", "hr"), min_value = c(30, 20),
              max_value = c(180, 220), source = "mimic_iv"),
    tmp, row.names = FALSE
  )
  df <- data.frame(concept_name = c("hr", "hr"), value = c(25, 210))
  out <- clean_by_range(df, config_path = tmp)
  # min(min_value)=20, max(max_value)=220 -> both values now in range
  expect_identical(out$out_of_range, c(FALSE, FALSE))
})

test_that("clean_by_range: errors on missing value_col", {
  expect_error(clean_by_range(data.frame(x = 1), ranges = list()),
              "not found")
})

test_that("clean_by_range: errors on malformed ranges argument", {
  expect_error(clean_by_range(data.frame(value = 1), ranges = 1:3),
              "data frame or named list")
})

test_that("clean_by_outlier: iqr method flags a clear outlier", {
  set.seed(1)
  df <- data.frame(
    concept_name = "hr",
    value = c(70, 72, 68, 71, 69, 73, 500)
  )
  out <- clean_by_outlier(df, method = "iqr")
  expect_true(out$is_outlier[7])
  expect_false(any(out$is_outlier[1:6]))
})

test_that("clean_by_outlier: mad method flags a clear outlier", {
  df <- data.frame(concept_name = "hr", value = c(70, 71, 69, 70, 500))
  out <- clean_by_outlier(df, method = "mad")
  expect_true(out$is_outlier[5])
})

test_that("clean_by_outlier: sd method flags a clear outlier given a stable baseline", {
  # sd() is computed over the whole vector including the outlier itself, so a
  # single huge value among very few points can inflate its own detection
  # threshold (documented in clean_by_outlier()'s own roxygen: "sensitive to
  # the very outliers it tries to find"). A larger, stable baseline avoids
  # that self-inflation and lets the outlier be genuinely detected.
  set.seed(1)
  df <- data.frame(concept_name = "hr", value = c(rnorm(30, mean = 70, sd = 2), 500))
  out <- clean_by_outlier(df, method = "sd")
  expect_true(out$is_outlier[31])
  expect_equal(sum(out$is_outlier), 1)
})

test_that("clean_by_outlier: sd method can fail to flag an outlier in a very small sample (documented caveat)", {
  df <- data.frame(concept_name = "hr", value = c(70, 71, 69, 70, 500))
  out <- clean_by_outlier(df, method = "sd")
  expect_false(out$is_outlier[5])
})

test_that("clean_by_outlier: groups smaller than min_n are left unflagged", {
  df <- data.frame(concept_name = "rare", value = c(1, 1000))
  out <- clean_by_outlier(df, min_n = 5)
  expect_false(any(out$is_outlier))
  report <- attr(out, "clean_report")
  expect_equal(report$n_outlier, 0)
})

test_that("clean_by_outlier: action = 'drop' removes flagged rows", {
  set.seed(1)
  df <- data.frame(concept_name = "hr", value = c(70, 72, 68, 71, 69, 73, 500))
  out <- clean_by_outlier(df, method = "iqr", action = "drop")
  expect_equal(nrow(out), 6)
})

test_that("clean_by_outlier: custom k widens/narrows bounds", {
  df <- data.frame(concept_name = "hr", value = c(70, 71, 69, 70, 80))
  out_default <- clean_by_outlier(df, method = "sd", k = 3)
  out_tight   <- clean_by_outlier(df, method = "sd", k = 0.01)
  expect_false(any(out_default$is_outlier))
  expect_true(any(out_tight$is_outlier))
})

test_that("clean_by_outlier: errors on missing value_col", {
  expect_error(clean_by_outlier(data.frame(x = 1)), "not found")
})

test_that(".clean_groups: falls back to one group when concept_col absent", {
  groups <- .clean_groups(data.frame(value = 1:3), concept_col = "concept_name")
  expect_identical(groups, rep("__all__", 3))
})

test_that(".apply_clean_action: flag/drop/na behave as documented", {
  df <- data.frame(value = c(1, 2, 3))
  bad <- c(FALSE, TRUE, NA)

  flagged <- .apply_clean_action(df, bad, "value", "flag", "bad_flag")
  expect_identical(flagged$bad_flag, c(FALSE, TRUE, FALSE))

  dropped <- .apply_clean_action(df, bad, "value", "drop", "bad_flag")
  expect_equal(nrow(dropped), 2)

  na_out <- .apply_clean_action(df, bad, "value", "na", "bad_flag")
  expect_identical(na_out$value, c(1, NA, 3))
})

test_that(".outlier_bounds: iqr/mad/sd produce distinct, method-appropriate bounds", {
  x <- c(60, 65, 70, 75, 80, 85, 90)
  b_iqr <- .outlier_bounds(x, "iqr", 1.5)
  b_mad <- .outlier_bounds(x, "mad", 3)
  b_sd  <- .outlier_bounds(x, "sd", 3)
  expect_true(all(c("lower", "upper") %in% names(b_iqr)))
  expect_true(b_iqr[["lower"]] < min(x))
  expect_true(b_iqr[["upper"]] > max(x))
  expect_true(is.numeric(b_mad))
  expect_true(is.numeric(b_sd))
})

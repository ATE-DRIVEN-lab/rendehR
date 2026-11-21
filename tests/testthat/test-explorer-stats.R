# Statistics helpers of the cohort explorer (R/explorer_stats.R).

test_that("Wilson interval matches the closed form and stays within [0, 1]", {
  w <- wilson(c(0, 5, 10), c(10, 10, 10))
  expect_equal(w$p, c(0, 0.5, 1))
  expect_equal(w$lo[1], 0)
  expect_equal(w$hi[3], 1)
  # k = 5, n = 10: 0.5 +- 1.96 * sqrt(0.025 + 1.96^2 / 400) / (1 + 1.96^2 / 10)
  z <- 1.96; den <- 1 + z^2 / 10; h <- z * sqrt(0.25 / 10 + z^2 / 400) / den
  expect_equal(w$lo[2], 0.5 - h)
  expect_equal(w$hi[2], 0.5 + h)
})

test_that("standardized mean differences use the pooled SD", {
  x1 <- c(1, 2, 3, 4); x2 <- c(2, 3, 4, 5)
  expect_equal(smd_cont(x1, x2), (mean(x1) - mean(x2)) / sqrt((var(x1) + var(x2)) / 2))
  expect_true(is.na(smd_cont(1, c(1, 2))))
  expect_equal(smd_bin(0.5, 0.5), 0)
  expect_true(is.na(smd_bin(0, 0)))
})

test_that("formatting helpers", {
  expect_equal(fmt_n(1234567), "1,234,567")
  expect_equal(n_pct(1, 4), "1 (25.0%)")
  expect_equal(n_pct(0, 0), "–")
  expect_equal(med_iqr(c(1, 2, 3, NA)), "2.0 [1.5–2.5]")
  expect_equal(mean_sd(numeric(0)), "–")
  expect_equal(mask_n(c(0, 5, 10)), c("0", "<10", "10"))
  expect_equal(kpct(3, 10), "<10")
  expect_equal(kpct(3, 10, mask = FALSE), "3 (30.0%)")
  m <- mask_cells(data.frame(a = c("5 (50.0%)", "12 (60.0%)"), stringsAsFactors = FALSE))
  expect_equal(m$a, c("<10", "12 (60.0%)"))
  expect_equal(safe_min(c(NA, NA)), NA_real_)
})

test_that("char_table describes each group and the overall column", {
  d <- data.frame(g = rep(c("a", "b"), each = 6), age = c(1:6, 11:16), sex = rep(c("f", "m"), 6),
                  died = c(1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0), stringsAsFactors = FALSE)
  vars <- data.frame(var = c("age", "sex", "died"), label = c("Age", "Sex", "Died"), type = c("cont", "cat", "bin"))
  t1 <- char_table(d, "g", vars)
  expect_equal(names(t1), c("variable", "level", "a", "b", "Overall", "missing_pct", "p_value"))
  expect_equal(t1$a[t1$variable == "N"], "6")
  expect_equal(t1$b[t1$variable == "Age" & t1$level == "median [IQR]"], med_iqr(11:16))
  expect_equal(t1$Overall[t1$variable == "Died"], "3 (25.0%)")
  expect_equal(t1$a[t1$variable == "Sex" & t1$level == "f"], "3 (50.0%)")
  expect_true(nzchar(t1$p_value[t1$variable == "Age" & t1$level == "mean (SD)"]))
  t0 <- char_table(d, ".all", vars)
  expect_equal(names(t0), c("variable", "level", "Overall", "missing_pct", "p_value"))
})

test_that("compare2 returns SMDs named by the two groups", {
  d <- data.frame(g = rep(c("a", "b"), each = 5), x = c(1:5, 3:7), y = c(1, 1, 0, 0, 0, 1, 0, 0, 0, 0))
  vars <- data.frame(var = c("x", "y"), label = c("X", "Y"), type = c("cont", "bin"))
  r <- compare2(d, "g", "a", "b", vars)
  expect_true(all(c("a", "b", "smd", "p_value") %in% names(r)))
  expect_equal(r$smd[r$variable == "X"], round(smd_cont(1:5, 3:7), 3))
  expect_equal(r$smd[r$variable == "Y"], round(smd_bin(0.4, 0.2), 3))
})

test_that("prop_by gives Wilson CIs per group and per stratum", {
  d <- data.frame(g = c("a", "a", "b", "b", "b"), s = c("x", "y", "x", "x", "y"), o = c(1, 0, 1, 1, NA))
  p <- prop_by(d, "o", "g")
  expect_equal(p$group, c("a", "b"))
  expect_equal(p$k, c(1L, 2L))
  expect_equal(p$n, c(2L, 2L))
  expect_equal(p$lo, wilson(c(1, 2), c(2, 2))$lo)
  expect_equal(prop_by(d, "o", "g", "s")$group, c("a | x", "a | y", "b | x"))
  expect_equal(prop_by(d, "o", ".all")$n, 4L)
})

test_that("small_cell_mask applies primary and secondary suppression", {
  # one small cell in a row with an Overall column: the next smallest group cell is masked too
  k <- cbind(a = c(5, 40), b = c(20, 30), c = c(50, 60)); k <- cbind(k, Overall = rowSums(k))
  m <- small_cell_mask(k, overall = TRUE, cols = FALSE)
  expect_equal(m[1, ], c(a = 1L, b = 2L, c = 0L, Overall = 0L))
  expect_true(all(m[2, ] == 0L))
  # columns adding up to a known total: a single masked level takes the smallest other level with it
  m <- small_cell_mask(matrix(c(3, 15, 40), 3), cols = TRUE)
  expect_equal(m[, 1], c(1L, 2L, 0L))
  # a hidden group column (N < 10) masks the Overall cells that would reveal it
  m <- small_cell_mask(cbind(c(30, 40), c(4, 2), c(34, 42)), hidden = c(FALSE, TRUE, FALSE), overall = TRUE, cols = FALSE)
  expect_equal(m[, 2], c(3L, 3L))
  expect_equal(m[, 3], c(2L, 2L))
  expect_equal(m[, 1], c(0L, 0L))
  # binary counts: masked when either k or n - k is 1-9
  m <- small_cell_mask(matrix(c(45, 20, 65), 1), n = c(50, 40, 90), binary = TRUE, overall = TRUE)
  expect_equal(m[1, 1], 1L)
  expect_equal(sum(m[1, ] > 0), 2L)
  # no row or column is left with exactly one masked cell
  set.seed(3)
  k <- matrix(sample(c(0:12, 30:60), 30, TRUE), 5); k <- cbind(k, rowSums(k))
  m <- small_cell_mask(k, overall = TRUE)
  expect_false(any(rowSums(m > 0) == 1)); expect_false(any(colSums(m > 0) == 1))
  expect_equal(mask_fmt(c(0L, 1L, 2L, 3L), "x"), c("x", "<10", "masked", "–"))
  expect_equal(mask_bin(c(3, 50, 55, 0), c(60, 60, 60, 60)), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(mask_bin(3, 60, on = FALSE), FALSE)
  expect_equal(mask_k(c(3, 55, 30), c(60, 60, 60)), c("<10", "masked", "30"))
})

test_that("char_table masks small cells with secondary suppression and hides small groups", {
  d <- data.frame(g = rep(c("a", "b", "c"), c(40, 30, 6)),
                  sex = c(rep(c("f", "m"), c(20, 20)), rep(c("f", "m", "x"), c(15, 12, 3)), rep("f", 6)),
                  died = c(rep(0:1, c(36, 4)), rep(0:1, c(15, 15)), rep(0, 6)), age = 1:76)
  vars <- data.frame(var = c("age", "sex", "died"), label = c("Age", "Sex", "Died"), type = c("cont", "cat", "bin"))
  t1 <- char_table(d, "g", vars, mask = TRUE)
  expect_equal(t1$c[t1$variable == "N"], "<10")            # hidden column
  expect_true(all(t1$c[t1$variable != "N"] == "–"))
  expect_equal(t1$Overall[t1$variable == "N"], "masked")   # Overall N would reveal it
  sx <- t1[t1$variable == "Sex", ]
  expect_equal(sx$b[sx$level == "x"], "<10")
  expect_equal(sx$b[sx$level == "m"], "masked")            # complement within column b
  # binary row: a = 4 deaths is masked; with the hidden column c the row has two masked cells
  expect_equal(t1$a[t1$variable == "Died"], "<10")
  expect_false(grepl("^[1-9] \\(", paste(unlist(t1[, c("a", "b", "Overall")]), collapse = " ")))
  expect_equal(t1$missing_pct[t1$variable == "Age" & t1$level == "mean (SD)"], "0.0%")
  # unmasked by default
  expect_equal(char_table(d, "g", vars)$c[1], "6")
})

test_that("cat_p uses Fisher's exact test for sparse tables", {
  tb <- matrix(c(3, 1, 1, 3), 2)
  expect_equal(cat_p(tb), stats::fisher.test(tb)$p.value)
  tb <- matrix(c(30, 20, 25, 35), 2)
  expect_equal(cat_p(tb), suppressWarnings(stats::chisq.test(tb))$p.value)
})

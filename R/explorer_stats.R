# Statistics and formatting helpers for the cohort explorer (run_explorer()).
# Kept free of Shiny so they can be tested directly; the app modules call them
# by name.

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

fmt_pct <- function(x, d = 1) sprintf(paste0("%.", d, "f"), 100 * x)

n_pct <- function(k, n, d = 1) {
  out <- sprintf("%s (%s%%)", fmt_n(k), fmt_pct(k / n, d))
  out[rep_len(n, length(out)) == 0] <- "\u2013"
  out
}

med_iqr <- function(x, d = 1) {
  x <- x[!is.na(x)]
  if (!length(x)) return("\u2013")
  q <- stats::quantile(x, c(.5, .25, .75), names = FALSE)
  sprintf(paste0("%.", d, "f [%.", d, "f\u2013%.", d, "f]"), q[1], q[2], q[3])
}

mean_sd <- function(x, d = 1) {
  x <- x[!is.na(x)]
  if (!length(x)) return("\u2013")
  sprintf(paste0("%.", d, "f (%.", d, "f)"), mean(x), stats::sd(x))
}

# Wilson score interval for k events out of n.
wilson <- function(k, n, z = 1.96) {
  p <- k / n
  den <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  tibble::tibble(p = p, lo = pmax(0, centre - half), hi = pmin(1, centre + half))
}

# Standardized mean differences (pooled SD of the two groups).
smd_cont <- function(x1, x2) {
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  if (length(x1) < 2 || length(x2) < 2) return(NA_real_)
  (mean(x1) - mean(x2)) / sqrt((stats::var(x1) + stats::var(x2)) / 2)
}

smd_bin <- function(p1, p2) {
  s <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
  ifelse(s > 0, (p1 - p2) / s, NA_real_)
}

fmt_p <- function(p) ifelse(is.na(p), "", format.pval(p, digits = 2, eps = 0.001))

# Small-cell rule: counts 1-9 are shown as "<10" without a percentage.
mask_cells <- function(df, on = TRUE) {
  if (!isTRUE(on)) return(df)
  dplyr::mutate(df, dplyr::across(dplyr::where(is.character),
                                  ~ stringr::str_replace(.x, "^[1-9] \\([^)]*\\)", "<10")))
}

mask_n <- function(n, on = TRUE) {
  if (isTRUE(on)) ifelse(n >= 1 & n < 10, "<10", fmt_n(n)) else fmt_n(n)
}

kpct <- function(k, n, mask = TRUE) {
  ifelse(mask & k >= 1 & k < 10, "<10", sprintf("%s (%s%%)", fmt_n(k), fmt_pct(k / n)))
}

# A binary count k of n is masked when either k or n - k is 1-9 (n - k is
# known from N otherwise).
mask_bin <- function(k, n, on = TRUE) {
  isTRUE(on) & ((k >= 1 & k <= 9) | (n - k >= 1 & n - k <= 9))
}
mask_k <- function(k, n, on = TRUE) {
  m <- mask_bin(k, n, on)
  ifelse(!m, fmt_n(k), ifelse(k >= 1 & k <= 9, "<10", "masked"))
}

# Small-cell codes for a count matrix, with secondary suppression so that no
# masked cell can be recovered by subtraction. A row (column) whose cells add
# up to a total the reader can see is protected once it has 0 or >= 2 masked
# cells; with exactly one, the smallest other cell is masked too.
#   overall: the last column is the row total of the others (Table 1).
#   hidden:  columns hidden altogether (group N < 10); their rows mask the
#            overall cell rather than another group.
#   binary:  k counts events out of n (one per column); a cell is also small
#            when n - k is 1-9.
# Returns 0 shown, 1 "<10", 2 secondary ("masked"), 3 hidden column.
small_cell_mask <- function(k, n = NULL, hidden = NULL, binary = FALSE, overall = FALSE,
                            rows = overall, cols = !binary) {
  k <- as.matrix(k); storage.mode(k) <- "double"
  nr <- nrow(k); nc <- ncol(k)
  small <- function(x) !is.na(x) & x >= 1 & x <= 9
  prim <- small(k)
  if (binary) prim <- prim | small(matrix(n, nr, nc, byrow = TRUE) - k)
  code <- matrix(0L, nr, nc, dimnames = dimnames(k))
  code[prim] <- 1L
  hidden <- rep_len(if (is.null(hidden)) FALSE else hidden, nc)
  code[, hidden] <- 3L
  smallest <- function(idx, v) {
    if (!length(idx)) return(integer(0))
    p <- idx[v[idx] > 0]
    if (!length(p)) p <- idx
    p[which.min(v[p])]
  }
  grp <- if (overall) seq_len(nc - 1) else seq_len(nc)
  repeat {
    changed <- FALSE
    if (rows && nc > 1) for (i in seq_len(nr)) {
      m <- code[i, ] > 0
      if (sum(m) != 1) next
      j <- which(m)
      cand <- if (overall && hidden[j]) nc else smallest(grp[!m[grp]], k[i, ])
      if (!length(cand) && overall && j != nc) cand <- nc
      if (length(cand)) { code[i, cand] <- 2L; changed <- TRUE }
    }
    if (cols && nr > 1) for (j in which(!hidden)) {
      m <- code[, j] > 0
      if (sum(m) != 1) next
      cand <- smallest(which(!m), k[, j])
      if (length(cand)) { code[cand, j] <- 2L; changed <- TRUE }
    }
    if (!changed) break
  }
  code
}

# Display text for masked cells; `shown` is used where the code is 0.
mask_fmt <- function(code, shown) {
  ifelse(code == 1L, "<10", ifelse(code == 2L, "masked", ifelse(code == 3L, "\u2013", shown)))
}

# p-value of a contingency table: chi-square, or Fisher's exact test (Monte
# Carlo beyond 2 x 2) when an expected count is below 5.
cat_p <- function(tb) {
  ct <- suppressWarnings(stats::chisq.test(tb))
  if (all(ct$expected >= 5)) return(ct$p.value)
  if (all(dim(tb) == 2)) return(stats::fisher.test(tb)$p.value)
  stats::fisher.test(tb, simulate.p.value = TRUE, B = 2000)$p.value
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) min(x) else NA_real_
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) max(x) else NA_real_
}

# Variables described among ICU admissions only.
.xp_icu_vars <- c("unit_first", "icu_los_total", "icu_death")

# Characteristics table: one column per group plus Overall, a missingness
# column and a p-value across all groups (Kruskal-Wallis / chi-square, or
# Fisher when an expected count is < 5) when there are 2+ groups.
# `vars` is a data frame with columns var, label, type ("cont", "cat", "bin").
# mask = TRUE applies the small-cell rule with secondary suppression (see
# small_cell_mask()): group columns with N < 10 are hidden, and the Overall
# cells that would reveal them are masked.
char_table <- function(df, group, vars, mask = FALSE) {
  df$.g <- if (group == ".all") "Overall" else df[[group]]
  total <- group != ".all"
  cols <- if (!total) "Overall"
          else c(levels(droplevels(factor(df$.g))), "Overall")
  icu_only <- function(d) if ("any_icu" %in% names(d)) d[d$any_icu %in% 1, ] else d
  sub <- function(g, s) {
    d <- if (g == "Overall") df else df[df$.g %in% g, ]
    if (s == "icu") icu_only(d) else d
  }
  dash <- "\u2013"
  mask <- isTRUE(mask)
  n_col <- vapply(cols, function(g) nrow(sub(g, "")), numeric(1))
  hidden <- mask & n_col < 10 & (cols != "Overall" | !total)
  codes <- function(k, n = NULL, binary = FALSE) {
    if (!mask) return(matrix(0L, nrow(k), ncol(k)))
    small_cell_mask(k, n = n, hidden = hidden, binary = binary, overall = total, rows = total,
                    cols = !binary)
  }
  hc <- codes(matrix(n_col, 1), binary = FALSE)
  header <- tibble::tibble(variable = "N", level = "",
                           !!!stats::setNames(as.list(ifelse(hc %in% c(1L, 3L), "<10",
                                                             mask_fmt(hc, fmt_n(n_col)))), cols),
                           missing_pct = "", p_value = "")
  rows <- lapply(seq_len(nrow(vars)), function(i) {
    v <- vars$var[i]; lab <- vars$label[i]; ty <- vars$type[i]
    s <- if (v %in% .xp_icu_vars) "icu" else ""
    d <- if (s == "icu") icu_only(df) else df
    miss <- paste0(fmt_pct(mean(is.na(d[[v]]))), "%")
    if (all(is.na(d[[v]])))
      return(tibble::tibble(variable = lab, level = "not available",
                            !!!stats::setNames(as.list(rep(dash, length(cols))), cols),
                            missing_pct = miss, p_value = ""))
    if (ty == "cont") {
      p <- if (length(cols) > 2)
        tryCatch(fmt_p(stats::kruskal.test(d[[v]] ~ factor(d$.g))$p.value), error = function(e) "")
      else ""
      cell <- function(f) stats::setNames(lapply(seq_along(cols), function(j)
        if (hidden[j]) dash else f(sub(cols[j], s)[[v]])), cols)
      dplyr::bind_rows(
        tibble::tibble(variable = lab, level = "mean (SD)", !!!cell(mean_sd), missing_pct = miss, p_value = p),
        tibble::tibble(variable = lab, level = "median [IQR]", !!!cell(med_iqr), missing_pct = "", p_value = ""))
    } else {
      x <- if (ty == "bin") factor(as.integer(as.logical(as.integer(d[[v]]))), levels = c(0, 1))
           else factor(d[[v]])
      p <- if (length(cols) > 2) tryCatch({
        tb <- table(x, d$.g)
        tb <- tb[rowSums(tb) > 0, colSums(tb) > 0, drop = FALSE]
        if (min(dim(tb)) < 2) "" else fmt_p(cat_p(tb))
      }, error = function(e) "") else ""
      lv <- if (ty == "bin") "1" else levels(x)
      xs <- lapply(cols, function(g) {
        z <- sub(g, s)[[v]]
        if (ty == "bin") z <- as.integer(as.logical(as.integer(z)))
        as.character(z)
      })
      n <- vapply(xs, function(z) sum(!is.na(z)), numeric(1))
      k <- matrix(vapply(xs, function(z) vapply(lv, function(l) sum(z == l, na.rm = TRUE), numeric(1)),
                         numeric(length(lv))), nrow = length(lv))
      cd <- codes(k, n, binary = ty == "bin")
      out <- lapply(seq_along(lv), function(r) tibble::tibble(
        variable = lab, level = if (ty == "bin") "n (%)" else lv[r],
        !!!stats::setNames(lapply(seq_along(cols), function(j)
          if (hidden[j]) dash else if (n[j] == 0) dash else mask_fmt(cd[r, j], n_pct(k[r, j], n[j]))), cols),
        missing_pct = "", p_value = ""))
      out[[1]]$missing_pct <- miss
      out[[1]]$p_value <- p
      dplyr::bind_rows(out)
    }
  })
  dplyr::bind_rows(header, rows)
}

# Two-group comparison with SMD and a Wilcoxon / chi-square p-value.
compare2 <- function(df, group, g1, g2, vars) {
  a <- df[df[[group]] %in% g1, ]
  b <- df[df[[group]] %in% g2, ]
  icu_only <- function(d) if ("any_icu" %in% names(d)) d[d$any_icu %in% 1, ] else d
  res <- dplyr::bind_rows(lapply(seq_len(nrow(vars)), function(i) {
    v <- vars$var[i]; ty <- vars$type[i]; s <- v %in% .xp_icu_vars
    aa <- if (s) icu_only(a) else a
    bb <- if (s) icu_only(b) else b
    x1 <- aa[[v]]; x2 <- bb[[v]]
    if (all(is.na(x1)) || all(is.na(x2))) return(NULL)
    if (ty == "cont") {
      tibble::tibble(variable = vars$label[i], level = "median [IQR]", g1 = med_iqr(x1), g2 = med_iqr(x2),
                     smd = smd_cont(x1, x2),
                     p_value = tryCatch(stats::wilcox.test(x1, x2)$p.value, error = function(e) NA_real_))
    } else {
      if (ty == "bin") {
        x1 <- as.integer(as.logical(as.integer(x1)))
        x2 <- as.integer(as.logical(as.integer(x2)))
      }
      u <- unique(c(stats::na.omit(as.character(x1)), stats::na.omit(as.character(x2))))
      lv <- if (ty == "bin") "1" else if (is.factor(aa[[v]])) intersect(levels(aa[[v]]), u) else sort(u)
      p <- tryCatch(suppressWarnings(stats::chisq.test(table(
        c(as.character(x1), as.character(x2)), rep(1:2, c(length(x1), length(x2)))))$p.value),
        error = function(e) NA_real_)
      dplyr::bind_rows(lapply(lv, function(l) {
        k1 <- sum(as.character(x1) == l, na.rm = TRUE); n1 <- sum(!is.na(x1))
        k2 <- sum(as.character(x2) == l, na.rm = TRUE); n2 <- sum(!is.na(x2))
        tibble::tibble(variable = vars$label[i], level = if (ty == "bin") "n (%)" else l,
                       g1 = n_pct(k1, n1), g2 = n_pct(k2, n2), smd = smd_bin(k1 / n1, k2 / n2),
                       p_value = if (l == lv[1]) p else NA_real_)
      }))
    }
  }))
  if (!nrow(res))
    res <- tibble::tibble(variable = character(), level = character(), g1 = character(),
                          g2 = character(), smd = numeric(), p_value = numeric())
  res$smd <- round(res$smd, 3)
  res$p_value <- fmt_p(res$p_value)
  names(res)[names(res) == "g1"] <- g1
  names(res)[names(res) == "g2"] <- g2
  res
}

# Proportion of a 0/1 outcome with Wilson CI by group (and optional strata).
prop_by <- function(df, outcome, group, strata = NULL) {
  gv <- c(if (group != ".all") group, strata)
  d <- df[!is.na(df[[outcome]]), , drop = FALSE]
  grp <- if (!length(gv)) rep("Overall", nrow(d))
         else do.call(paste, c(lapply(gv, function(g) as.character(d[[g]])), sep = " | "))
  g <- factor(grp, levels = sort(unique(grp), method = "radix"))   # C-locale order, as dplyr
  y <- d[[outcome]]
  out <- tibble::tibble(group = levels(g), k = as.integer(tapply(y == 1, g, sum)),
                        n = as.integer(table(g)))
  dplyr::bind_cols(out, wilson(out$k, out$n))
}

# Per group and time bin: admissions with a value, and the median and IQR of
# their per-admission means (x: group, bin, value; one row per admission and
# bin). With mask = TRUE, group-bins with fewer than 10 admissions are dropped
# and counted in attr(, "dropped").
traj_stats <- function(x, bin_h, mask = FALSE) {
  s <- dplyr::summarise(dplyr::group_by(x, group, bin), n = dplyr::n(), median = stats::median(value),
                        q25 = stats::quantile(value, .25, names = FALSE), q75 = stats::quantile(value, .75, names = FALSE),
                        .groups = "drop")
  s$start <- s$bin * bin_h
  hide <- isTRUE(mask) & s$n < 10
  s <- s[!hide, , drop = FALSE]
  attr(s, "dropped") <- sum(hide)
  s
}

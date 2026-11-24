# ___________________________________________________________________________
# clean.R - optional value-cleaning helpers for fetched concept data
#
# These operate on the long-format tibble returned by fetch_concept() /
# fetch_concepts() (one numeric `value` column, optional `concept_name`
# grouping column). They are opt-in: the pipeline never calls them, a user
# applies them downstream when they want range- or outlier-based cleaning.
#
#   clean_by_range()    - bounds come from min_value/max_value in the concept
#                         config CSV (or a user-supplied lookup).
#   clean_by_outlier()  - bounds are computed from the data itself per concept
#                         (IQR / MAD / SD rules).
#
# Both default to action = "flag" (non-destructive: add a logical column and
# attach a summary) so nothing is silently discarded.
# ___________________________________________________________________________

# Apply the chosen action to a logical `bad` vector for one value column.
# action: "flag" (add flag_col), "drop" (remove bad rows), "na" (value -> NA).
.apply_clean_action <- function(df, bad, value_col, action, flag_col) {
  bad[is.na(bad)] <- FALSE
  switch(action,
    flag = {
      df[[flag_col]] <- bad
      df
    },
    drop = df[!bad, , drop = FALSE],
    na = {
      df[[value_col]][bad] <- NA
      df
    },
    stop("Unknown action '", action, "'. Use 'flag', 'drop', or 'na'.")
  )
}

# Per-group split key: the concept column if present, else a single group.
.clean_groups <- function(df, concept_col) {
  if (!is.null(concept_col) && concept_col %in% names(df))
    return(as.character(df[[concept_col]]))
  rep("__all__", nrow(df))
}

# ___________________________________________________________________________
# clean_by_range - drop/flag values outside the config min/max
# ___________________________________________________________________________

#' Clean concept values using config-defined plausibility ranges
#'
#' Flags (or drops / NA-sets) numeric values that fall outside the
#' `[min_value, max_value]` window declared for each concept in the concept
#' config CSV (`rendehr_concepts_config.csv`). This is physiology-based
#' cleaning: bounds are fixed reference ranges, not derived from the data.
#'
#' When a concept has several config rows (e.g. one per source/table), the
#' most permissive window is used: `min(min_value)` .. `max(max_value)`.
#' Concepts with no usable bound are left untouched (a message lists them).
#'
#' @param df A data frame from fetch_concept()/fetch_concepts().
#' @param ranges Optional override. A data frame with columns
#'   `concept_name`, `min_value`, `max_value`; or a named list of
#'   `c(min, max)` vectors. If NULL (default) bounds are read from the
#'   concept config CSV.
#' @param config_path Path to the concept config CSV. Defaults to the bundled
#'   `rendehr_concepts_config.csv`. Ignored when `ranges` is supplied.
#' @param src_name Optional source name; when given, config rows are filtered
#'   to that source family before building bounds.
#' @param value_col Name of the numeric value column. Default "value".
#' @param concept_col Name of the grouping column. Default "concept_name".
#'   If absent from `df`, all rows are treated as one concept named after
#'   `single_concept`.
#' @param single_concept Concept name to assume when `df` has no
#'   `concept_col` (e.g. a single fetch_concept() result). Default NULL,
#'   which means: skip with a message unless `ranges`/config has a single row.
#' @param action One of "flag" (add `out_of_range` logical column, default),
#'   "drop" (remove offending rows), or "na" (set offending values to NA).
#' @return `df` with the action applied. A summary tibble (per concept: n,
#'   n_bad, min/max bound used) is attached as `attr(result, "clean_report")`.
#' @examples
#' hr <- data.frame(concept_name = "heart_rate", value = c(60, 300, -5))
#' ranges <- data.frame(concept_name = "heart_rate", min_value = 0, max_value = 250)
#' clean_by_range(hr, ranges = ranges)
#' clean_by_range(hr, ranges = ranges, action = "na")
#' @export
clean_by_range <- function(df, ranges = NULL, config_path = NULL,
                           src_name = NULL, value_col = "value",
                           concept_col = "concept_name",
                           single_concept = NULL,
                           action = c("flag", "drop", "na")) {
  action <- match.arg(action)
  if (!value_col %in% names(df))
    stop("Value column '", value_col, "' not found in df.")

  bounds <- .resolve_range_bounds(ranges, config_path, src_name)

  groups <- .clean_groups(df, concept_col)
  has_concept_col <- !is.null(concept_col) && concept_col %in% names(df)
  if (!has_concept_col && !is.null(single_concept))
    groups <- rep(single_concept, nrow(df))

  vals <- suppressWarnings(as.numeric(df[[value_col]]))
  bad  <- logical(nrow(df))
  report <- list()
  no_bound <- character(0)

  for (g in unique(groups)) {
    b <- bounds[[g]]
    idx <- which(groups == g)
    if (is.null(b) || all(is.na(b))) {
      no_bound <- c(no_bound, g)
      next
    }
    lo <- b[["min"]]; hi <- b[["max"]]
    g_bad <- rep(FALSE, length(idx))
    if (!is.na(lo)) g_bad <- g_bad | (vals[idx] < lo)
    if (!is.na(hi)) g_bad <- g_bad | (vals[idx] > hi)
    bad[idx] <- g_bad
    report[[g]] <- data.frame(
      concept_name = g, n = length(idx),
      n_flagged = sum(g_bad, na.rm = TRUE),
      min_value = lo, max_value = hi, stringsAsFactors = FALSE)
  }

  if (length(no_bound) > 0)
    message("clean_by_range: no min/max bound for: ",
            paste(unique(no_bound), collapse = ", "), " (left unchanged)")

  out <- .apply_clean_action(df, bad, value_col, action, "out_of_range")
  attr(out, "clean_report") <-
    if (length(report) > 0) do.call(rbind, report) else NULL
  out
}

# Build a named list: concept_name -> c(min=, max=).
.resolve_range_bounds <- function(ranges, config_path, src_name) {
  if (!is.null(ranges)) {
    if (is.data.frame(ranges)) {
      req <- c("concept_name", "min_value", "max_value")
      if (!all(req %in% names(ranges)))
        stop("`ranges` data frame needs columns: ",
             paste(req, collapse = ", "))
      return(stats::setNames(
        Map(function(lo, hi) c(min = lo, max = hi),
            ranges$min_value, ranges$max_value),
        ranges$concept_name))
    }
    if (is.list(ranges))
      return(lapply(ranges, function(v)
        c(min = v[[1]], max = v[[2]])))
    stop("`ranges` must be a data frame or named list.")
  }

  if (is.null(config_path))
    config_path <- ehr_config_path("rendehr_concepts_config.csv")
  cfg <- readr::read_csv(config_path, show_col_types = FALSE)
  if (!is.null(src_name) && "source" %in% names(cfg))
    cfg <- cfg[cfg$source == src_family(src_name), , drop = FALSE]

  cfg$min_value <- suppressWarnings(as.numeric(cfg$min_value))
  cfg$max_value <- suppressWarnings(as.numeric(cfg$max_value))
  cfg <- cfg[!(is.na(cfg$min_value) & is.na(cfg$max_value)), , drop = FALSE]

  bounds <- list()
  for (cn in unique(cfg$concept_name)) {
    rows <- cfg[cfg$concept_name == cn, ]
    bounds[[cn]] <- c(
      min = suppressWarnings(min(rows$min_value, na.rm = TRUE)),
      max = suppressWarnings(max(rows$max_value, na.rm = TRUE)))
  }
  # min/max over all-NA gives Inf/-Inf; normalise to NA.
  lapply(bounds, function(b) {
    b[!is.finite(b)] <- NA_real_
    b
  })
}

# ___________________________________________________________________________
# clean_by_outlier - flag/drop statistical outliers per concept
# ___________________________________________________________________________

#' Clean concept values using statistical outlier rules
#'
#' Flags (or drops / NA-sets) numeric values that fall outside data-derived
#' bounds, computed independently per concept. Unlike clean_by_range() the
#' thresholds come from the data, so this catches values that are implausible
#' *relative to the rest of the concept's distribution*.
#'
#' Methods:
#' \describe{
#'   \item{iqr}{Tukey fences: `[Q1 - k*IQR, Q3 + k*IQR]`, default k = 1.5.
#'     Robust, distribution-light - the usual first choice.}
#'   \item{mad}{`median +/- k * (1.4826 * MAD)`, default k = 3. Most robust
#'     to heavy tails; good for skewed clinical labs.}
#'   \item{sd}{`mean +/- k * sd`, default k = 3. Assumes roughly symmetric
#'     data; sensitive to the very outliers it tries to find.}
#' }
#'
#' @param df A data frame from fetch_concept()/fetch_concepts().
#' @param method One of "iqr" (default), "mad", "sd".
#' @param k Threshold multiplier. Defaults per method: iqr 1.5, mad 3, sd 3.
#' @param value_col Name of the numeric value column. Default "value".
#' @param concept_col Grouping column. Default "concept_name"; if absent, all
#'   rows form one group.
#' @param min_n Minimum non-NA values in a group before outlier detection is
#'   attempted. Smaller groups are left unchanged. Default 5.
#' @param action One of "flag" (add `is_outlier` logical column, default),
#'   "drop", or "na".
#' @return `df` with the action applied. A per-concept summary (n, n_outlier,
#'   lower/upper bound, method) is attached as `attr(result, "clean_report")`.
#' @examples
#' hr <- data.frame(concept_name = "heart_rate", value = c(60, 70, 80, 75, 72, 300, 68, 71))
#' clean_by_outlier(hr, method = "iqr")
#' clean_by_outlier(hr, method = "iqr", action = "drop")
#' @export
clean_by_outlier <- function(df, method = c("iqr", "mad", "sd"), k = NULL,
                             value_col = "value",
                             concept_col = "concept_name", min_n = 5,
                             action = c("flag", "drop", "na")) {
  method <- match.arg(method)
  action <- match.arg(action)
  if (is.null(k)) k <- switch(method, iqr = 1.5, mad = 3, sd = 3)
  if (!value_col %in% names(df))
    stop("Value column '", value_col, "' not found in df.")

  groups <- .clean_groups(df, concept_col)
  vals   <- suppressWarnings(as.numeric(df[[value_col]]))
  bad    <- logical(nrow(df))
  report <- list()

  for (g in unique(groups)) {
    idx <- which(groups == g)
    v   <- vals[idx]
    ok  <- v[!is.na(v)]
    if (length(ok) < min_n) {
      report[[g]] <- data.frame(
        concept_name = g, n = length(idx), n_outlier = 0,
        lower = NA_real_, upper = NA_real_, method = method,
        stringsAsFactors = FALSE)
      next
    }
    b  <- .outlier_bounds(ok, method, k)
    lo <- b[["lower"]]; hi <- b[["upper"]]
    g_bad <- !is.na(v) & (v < lo | v > hi)
    bad[idx] <- g_bad
    report[[g]] <- data.frame(
      concept_name = g, n = length(idx),
      n_outlier = sum(g_bad), lower = lo, upper = hi,
      method = method, stringsAsFactors = FALSE)
  }

  out <- .apply_clean_action(df, bad, value_col, action, "is_outlier")
  attr(out, "clean_report") <-
    if (length(report) > 0) do.call(rbind, report) else NULL
  out
}

# Lower/upper bounds for a non-NA numeric vector under the chosen rule.
.outlier_bounds <- function(x, method, k) {
  switch(method,
    iqr = {
      q <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
      iqr <- q[2] - q[1]
      c(lower = q[1] - k * iqr, upper = q[2] + k * iqr)
    },
    mad = {
      med <- stats::median(x)
      m   <- stats::mad(x, constant = 1.4826)  # already scaled; 0 if no spread
      c(lower = med - k * m, upper = med + k * m)
    },
    sd = {
      mu <- mean(x); s <- stats::sd(x)
      c(lower = mu - k * s, upper = mu + k * s)
    }
  )
}

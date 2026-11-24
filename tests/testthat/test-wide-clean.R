# Wide loaders, clean = "range": out-of-range readings are excluded in SQL
# before aggregation, with a <concept>_n_cleaned column and a clean_report.
#
# Fixture (merged ts, two admissions, first day unless noted):
#   creatinine [0, 15]   A: 1.2, 999999 (sentinel), 15 (on the bound, kept),
#                           0.5 on day 2;  B: 20 (out), 'abc' (not numeric)
#   sodium     [100,200] A: 140, 50 (out low);  B: 135
#   lactate    [-, 30]   A: 2, 40 (out), -1 (kept: no lower bound)
#   heart_rate no bound  A: 80, 999 (kept: unbounded concept)

.wc_ranges <- list(creatinine = c(0, 15), sodium = c(100, 200),
                   lactate = c(NA, 30))

.wc_ts <- function() {
  rows <- data.frame(
    adm = c("A", "A", "A", "A", "B", "B", "A", "A", "B", "A", "A", "A", "A", "A"),
    concept_name = c(rep("creatinine", 6), rep("sodium", 3),
                     rep("lactate", 3), rep("heart_rate", 2)),
    value = c("1.2", "999999", "15", "0.5", "20", "abc", "140", "50", "135",
              "2", "40", "-1", "80", "999"),
    time_since_hadm_min = c(0, 60, 120, 1500, 30, 40, 10, 20, 5, 0, 50, 70, 0, 10))
  data.frame(
    ehr_id = paste0("1_", rows$adm), source_data = "src_a",
    src_patient_id = "1", src_admission_id = rows$adm,
    assigned_icu_id = paste0("icu_", rows$adm),
    concept_name = rows$concept_name, value = rows$value, unit = "u",
    time_since_hadm_min = rows$time_since_hadm_min,
    time_since_icu_min = rows$time_since_hadm_min - 5)
}

.wc_put <- function(con, schema) {
  DBI::dbWriteTable(con, DBI::Id(schema = schema, table = "ts"), .wc_ts())
  schema
}

.wc_duck <- function() {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  DBI::dbExecute(con, "CREATE SCHEMA wc")
  list(con = con, schema = .wc_put(con, "wc"),
       done = function() DBI::dbDisconnect(con, shutdown = TRUE))
}

.wc_pg <- function() {
  skip_if_not(nzchar(Sys.getenv("EHR_TEST_DB")),
              "Set EHR_TEST_DB=<database> to run DB tests")
  con <- connect_postgres(Sys.getenv("EHR_TEST_DB"),
                          load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                                      "config/config_pass.json")))
  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS wide_clean_test CASCADE")
  DBI::dbExecute(con, "CREATE SCHEMA wide_clean_test")
  list(con = con, schema = .wc_put(con, "wide_clean_test"),
       done = function() {
         DBI::dbExecute(con, "DROP SCHEMA IF EXISTS wide_clean_test CASCADE")
         DBI::dbDisconnect(con)
       })
}

.wc_aggs <- c("min", "max", "mean", "median", "sd", "sum", "n", "first", "last")

# Expected first-day values the long way: load_merged_ts() -> clean_by_range()
# -> aggregate in R. One row per admission x concept.
.wc_expected <- function(con, schema) {
  long <- load_merged_ts(schema, cohort_only = FALSE, window_min = c(0, 1440),
                         con = con)
  long$value <- suppressWarnings(as.numeric(long$value))
  flagged <- suppressMessages(clean_by_range(long, ranges = .wc_ranges))
  long$ok <- !is.na(long$value) & !flagged$out_of_range
  long <- long[order(long$time_since_hadm_min), ]
  grid <- expand.grid(src_admission_id = c("A", "B"),
                      concept_name = unique(.wc_ts()$concept_name),
                      stringsAsFactors = FALSE)
  out <- lapply(seq_len(nrow(grid)), function(i) {
    d <- long[long$src_admission_id == grid$src_admission_id[i] &
                long$concept_name == grid$concept_name[i], ]
    v <- d$value[d$ok]
    one <- function(x) if (length(v)) x else NA_real_
    data.frame(grid[i, ],
      min = one(min(v)), max = one(max(v)), mean = one(mean(v)),
      median = one(stats::median(v)),
      sd = if (length(v) > 1) stats::sd(v) else NA_real_,
      sum = one(sum(v)), n = length(v),
      first = one(v[1]), last = one(v[length(v)]),
      n_cleaned = sum(flagged$out_of_range[flagged$src_admission_id ==
                                              grid$src_admission_id[i] &
                                            flagged$concept_name ==
                                              grid$concept_name[i]]))
  })
  do.call(rbind, out)
}

.wc_check_equivalence <- function(con, schema) {
  concepts <- unique(.wc_ts()$concept_name)
  spec <- stats::setNames(rep(list(.wc_aggs), length(concepts)), concepts)
  wide <- suppressMessages(load_merged_ts_wide(
    schema, spec, bin_min = 1440, window = c(0, 0), cohort_only = FALSE,
    clean = "range", ranges = .wc_ranges, con = con))
  exp <- .wc_expected(con, schema)

  for (i in seq_len(nrow(exp))) {
    got <- wide[wide$src_admission_id == exp$src_admission_id[i], ]
    expect_identical(nrow(got), 1L)
    cn <- exp$concept_name[i]
    for (agg in .wc_aggs) {
      expect_equal(as.numeric(got[[paste0(cn, "_", agg)]]), exp[[agg]][i],
                   tolerance = 1e-9,
                   info = paste(exp$src_admission_id[i], cn, agg))
    }
    expect_equal(as.numeric(got[[paste0(cn, "_n_cleaned")]]), exp$n_cleaned[i],
                 info = paste(exp$src_admission_id[i], cn, "n_cleaned"))
  }
}

.wc_check_report <- function(con, schema) {
  wide <- suppressMessages(load_merged_ts_wide(
    schema, c("creatinine", "sodium", "lactate", "heart_rate"),
    bin_min = 1440, window = c(0, 0), cohort_only = FALSE,
    clean = "range", ranges = .wc_ranges, con = con))
  rep <- attr(wide, "clean_report")
  rep <- rep[match(c("creatinine", "sodium", "lactate", "heart_rate"),
                   rep$concept_name), ]
  # Day-2 creatinine is outside the window, so it is not in the report.
  expect_equal(rep$n,             c(5, 3, 3, 2))
  expect_equal(rep$n_cleaned,     c(2, 1, 1, 0))
  expect_equal(rep$n_not_numeric, c(1, 0, 0, 0))
  expect_equal(rep$min_value, c(0, 100, NA, NA))
  expect_equal(rep$max_value, c(15, 200, 30, NA))
}

test_that("clean = 'range' equals clean_by_range() + aggregating in R (DuckDB)", {
  db <- .wc_duck(); on.exit(db$done(), add = TRUE)
  .wc_check_equivalence(db$con, db$schema)
})

test_that("clean = 'range' equals clean_by_range() + aggregating in R (Postgres)", {
  db <- .wc_pg(); on.exit(db$done(), add = TRUE)
  .wc_check_equivalence(db$con, db$schema)
})

test_that("clean_report counts readings, cleaned and non-numeric (DuckDB)", {
  db <- .wc_duck(); on.exit(db$done(), add = TRUE)
  .wc_check_report(db$con, db$schema)
})

test_that("clean_report counts readings, cleaned and non-numeric (Postgres)", {
  db <- .wc_pg(); on.exit(db$done(), add = TRUE)
  .wc_check_report(db$con, db$schema)
})

test_that("clean = 'none' is unchanged: raw values, no extra columns or report", {
  db <- .wc_duck(); on.exit(db$done(), add = TRUE)
  wide <- load_merged_ts_wide(db$schema, c("creatinine", "heart_rate"),
                              bin_min = 1440, window = c(0, 0),
                              cohort_only = FALSE, con = db$con)
  a <- wide[wide$src_admission_id == "A", ]
  expect_equal(a$creatinine, mean(c(1.2, 999999, 15)))
  expect_false(any(grepl("_n_cleaned$", names(wide))))
  expect_null(attr(wide, "clean_report"))
})

test_that("unbounded concepts are kept as-is with a zero n_cleaned column", {
  db <- .wc_duck(); on.exit(db$done(), add = TRUE)
  expect_message(
    wide <- load_merged_ts_wide(db$schema, c("creatinine", "heart_rate"),
                                bin_min = 1440, window = c(0, 0),
                                cohort_only = FALSE, clean = "range",
                                ranges = .wc_ranges, con = db$con),
    "no min/max bound for: heart_rate")
  a <- wide[wide$src_admission_id == "A", ]
  expect_equal(a$heart_rate, mean(c(80, 999)))
  expect_equal(a$heart_rate_n_cleaned, 0)
  expect_equal(a$creatinine, mean(c(1.2, 15)))
})

test_that("a bin whose readings were all cleaned is NA with n_cleaned > 0", {
  db <- .wc_duck(); on.exit(db$done(), add = TRUE)
  wide <- load_merged_ts_wide(db$schema, c(creatinine = "mean"), bin_min = 1440,
                              window = c(0, 0), cohort_only = FALSE,
                              clean = "range", ranges = .wc_ranges, con = db$con)
  b <- wide[wide$src_admission_id == "B", ]
  expect_true(is.na(b$creatinine))
  expect_equal(b$creatinine_n_cleaned, 1)
})

test_that("clean works with complete_grid, raw timestamps and the ICU clock", {
  db <- .wc_duck(); on.exit(db$done(), add = TRUE)

  grid <- load_merged_ts_wide(db$schema, c(creatinine = "mean"), bin_min = 60,
                              window = c(0, 2), complete_grid = TRUE,
                              cohort_only = FALSE, clean = "range",
                              ranges = .wc_ranges, con = db$con)
  a <- grid[grid$src_admission_id == "A", ]
  expect_equal(as.integer(a$t_bin), 0:2)
  expect_equal(a$creatinine, c(1.2, NA, 15))          # 999999 at bin 1 cleaned
  expect_equal(a$creatinine_n_cleaned, c(0, 1, 0))

  raw <- load_merged_ts_wide(db$schema, c(creatinine = "mean"), bin_min = NULL,
                             cohort_only = FALSE, clean = "range",
                             ranges = .wc_ranges, con = db$con)
  expect_equal(sum(raw$creatinine_n_cleaned), 2)      # 999999 (A) and 20 (B)
  expect_false(any(raw$creatinine > 15, na.rm = TRUE))

  icu <- load_merged_ts_wide(db$schema, c(creatinine = "max"), bin_min = 1440,
                             analysis_level = "icu", window = c(0, 0),
                             cohort_only = FALSE, clean = "range",
                             ranges = .wc_ranges, con = db$con)
  expect_equal(icu$creatinine[icu$assigned_icu_id == "icu_A"], 15)
})

test_that("clean = 'range' without ranges uses the concept config bounds", {
  db <- .wc_duck(); on.exit(db$done(), add = TRUE)
  wide <- suppressMessages(load_merged_ts_wide(
    db$schema, c(creatinine = "mean"), bin_min = 1440, window = c(0, 0),
    cohort_only = FALSE, clean = "range", con = db$con))
  cfg <- .resolve_range_bounds(NULL, NULL, NULL)$creatinine
  rep <- attr(wide, "clean_report")
  expect_equal(c(rep$min_value, rep$max_value), unname(cfg))
})

test_that("clean and ranges are validated", {
  expect_error(load_merged_ts_wide("s", "x", clean = "outlier"), "should be one of")
  expect_error(load_merged_ts_wide("s", "x", ranges = list(x = c(0, 1))),
               "only used with clean")
  expect_error(load_merged_ts_wide("s", "x", clean = "range",
                                   ranges = list(x = c("0", "1; DROP TABLE ts"))),
               "must be numeric")
  expect_error(fetch_concepts_wide("mimic_iv_demo", "x", ranges = list(x = c(0, 1))),
               "only used with clean")
})

test_that("fetch_concepts_wide cleans a per-source ts_tab (Postgres)", {
  skip_if_not(nzchar(Sys.getenv("EHR_TEST_DB")),
              "Set EHR_TEST_DB=<database> to run DB tests")
  con <- connect_postgres(Sys.getenv("EHR_TEST_DB"),
                          load_credentials(Sys.getenv("EHR_CREDS_PATH",
                                                      "config/config_pass.json")))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  hs <- src_schema_names("mimic_iv_demo")$harmonized
  # the schema name is fixed by the source, so never replace a built one
  skip_if(DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM information_schema.schemata WHERE schema_name = '%s'", hs))$n > 0,
    paste(hs, "already exists in EHR_TEST_DB"))
  DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", hs))
  on.exit(DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", hs)),
          add = TRUE, after = FALSE)
  DBI::dbWriteTable(con, DBI::Id(schema = hs, table = "ts_tab"), data.frame(
    subject_id = 1L, hadm_id = 10L, stay_id = 100L, concept_name = "creatinine",
    value = c("1.2", "999999", "15"), time_since_hadm_min = c(0, 60, 120)))

  wide <- fetch_concepts_wide("mimic_iv_demo", c(creatinine = "max"),
                              bin_min = 1440, window = c(0, 0),
                              cohort_only = FALSE, clean = "range",
                              ranges = .wc_ranges, con = con)
  expect_equal(wide$creatinine, 15)
  expect_equal(wide$creatinine_n_cleaned, 1)
  expect_equal(attr(wide, "clean_report")$n_cleaned, 1)
})

# cache_raw_parquet_duckdb() on the real MIMIC-IV demo files. Needs only the
# raw files (EHR_TEST_E2E_RAW_ROOT); everything is written under tempdir().
.raw_cache_env <- function() {
  root <- Sys.getenv("EHR_TEST_E2E_RAW_ROOT")
  skip_if(!nzchar(root), "set EHR_TEST_E2E_RAW_ROOT to the PhysioNet root to run the raw-cache tests")
  skip_if_not(dir.exists(file.path(root, "mimic_iv_demo")), "no mimic_iv_demo under EHR_TEST_E2E_RAW_ROOT")
  skip_if_not_installed("duckdb")
  base <- file.path(tempdir(), "rendehr_raw_cache"); unlink(base, recursive = TRUE)
  dir.create(base, recursive = TRUE)
  list(root = root, base = base)
}

test_that("list_example_cohorts() lists the bundled cohorts", {
  ex <- list_example_cohorts()
  expect_s3_class(ex, "data.frame")
  expect_true(all(c("sepsis3", "scid") %in% ex$name))
  expect_true(all(c("name", "kind") %in% names(ex)))
})

test_that("cache_raw_parquet_duckdb() caches raw tables, reuses fresh ones and rebuilds on request", {
  e <- .raw_cache_env()
  run <- function(...) cache_raw_parquet_duckdb("mimic_iv_demo", base_dir = e$base, raw_data_root = e$root,
                                                show_progress = TRUE, ...)
  expect_message(expect_null(run(all_tables = FALSE)), "nothing to cache")
  expect_error(run(tables = "no_such_table"), "not in config")

  dir <- NULL
  expect_message(dir <- run(tables = c("admissions", "patients")), "Caching to parquet: admissions")
  files <- file.path(dir, c("admissions.parquet", "patients.parquet"))
  expect_true(all(file.exists(files)))
  expect_true(file.exists(file.path(dir, "_conversion_report.csv")))
  # every raw row is cached, with the configured types
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw_n <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM read_csv_auto('%s')",
                                        file.path(e$root, "mimic_iv_demo", "hosp", "admissions.csv.gz")))$n
  pq <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n, count(DISTINCT hadm_id) AS h FROM read_parquet('%s')", files[1]))
  expect_equal(pq$n, raw_n)
  expect_equal(pq$h, raw_n)

  expect_message(run(tables = "admissions"), "matches its source file\\(s\\), skipping: admissions")
  expect_message(run(tables = "admissions", overwrite = TRUE), "Caching to parquet: admissions")
  # a bundled cohort resolves the tables it needs
  expect_message(run(cohort = "scid"), "Resolved tables for cohort 'scid'")
})

test_that("a chunked cohort build from the raw parquet cache equals a single-statement build", {
  e <- .raw_cache_env()
  root <- e$root
  base_a <- file.path(e$base, "chunked"); base_b <- file.path(e$base, "single")
  suppressMessages(cache_raw_parquet_duckdb("mimic_iv_demo", base_dir = base_a, raw_data_root = root, show_progress = FALSE))
  spill <- file.path(e$base, "spill"); dir.create(spill)
  # resource settings, the parquet cache and 1,000-row chunks, with progress messages
  msgs <- character()
  withCallingHandlers(
    import_cohort_general("mimic_iv_demo", base_dir = base_a, raw_data_root = root, cohort = "scid", show_progress = TRUE,
                          loading = loading_options(memory_limit = "2GB", threads = 2, temp_directory = spill,
                                                    table_chunk_rows = 1000, use_parquet_cache = TRUE,
                                                    validate_columns = TRUE)),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("chunk\\(s\\)", msgs)))
  expect_true(any(grepl("validating chunk", msgs)))            # type validation runs per chunk too
  expect_true(any(grepl("Published current", msgs)))
  # a current artifact is reused, not rebuilt
  expect_message(import_cohort_general("mimic_iv_demo", base_dir = base_a, raw_data_root = root, cohort = "scid",
                                       show_progress = TRUE), "no rebuild needed")
  suppressMessages(import_cohort_general("mimic_iv_demo", base_dir = base_b, raw_data_root = root, cohort = "scid",
                                         show_progress = FALSE))
  pa <- ehr_paths("mimic_iv_demo", base_dir = base_a)$cohort_parquet_dir
  pb <- ehr_paths("mimic_iv_demo", base_dir = base_b)$cohort_parquet_dir
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  rows <- function(dir, t) DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM read_parquet('%s')", file.path(dir, paste0(t, ".parquet"))))$n
  for (t in c("chartevents", "labevents", "admissions", "icustays")) expect_equal(rows(pa, t), rows(pb, t), info = t)
  same <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM (SELECT * FROM read_parquet('%s') EXCEPT ALL SELECT * FROM read_parquet('%s'))",
    file.path(pa, "chartevents.parquet"), file.path(pb, "chartevents.parquet")))$n
  expect_equal(same, 0)
})

test_that("cohort selection exports ids, infers direct-id keys and rejects bad table requests", {
  e <- .raw_cache_env()
  root <- e$root; base <- file.path(e$base, "select")
  out <- file.path(base, "ids", "scid.csv")
  ids <- suppressMessages(select_cohort_ids("mimic_iv_demo", cohort = "scid", base_dir = base, raw_data_root = root,
                                            out_csv = out))
  expect_true(file.exists(out))
  exported <- readr::read_csv(out, show_col_types = FALSE)
  expect_gt(nrow(exported), 0)
  writeLines("stale", out)                                   # an existing export is replaced
  suppressMessages(select_cohort_ids("mimic_iv_demo", cohort = "scid", base_dir = base, raw_data_root = root, out_csv = out))
  expect_equal(readr::read_csv(out, show_col_types = FALSE), exported)
  expect_error(select_cohort_ids("mimic_iv_demo", cohort = "scid", base_dir = base, raw_data_root = root, out_csv = c("a", "b")),
               "one non-empty path")
  # direct ids from .rds and .parquet, key inferred from the single hierarchy column
  key <- names(exported)[1]
  rds <- file.path(base, "ids.rds"); saveRDS(exported, rds)
  pq <- file.path(base, "ids.parquet"); arrow::write_parquet(exported, pq)
  for (f in c(rds, pq)) {
    d <- suppressMessages(select_cohort_ids("mimic_iv_demo", direct_ids_path = f, base_dir = base, raw_data_root = root))
    expect_setequal(d$values, as.character(exported[[key]]))
    expect_equal(d$evidence_key, key)
  }
  two <- file.path(base, "two.csv"); readr::write_csv(data.frame(subject_id = 1, hadm_id = 2), two)
  expect_error(select_cohort_ids("mimic_iv_demo", direct_ids_path = two, base_dir = base, raw_data_root = root),
               "multiple hierarchy columns")
  none <- file.path(base, "none.csv"); readr::write_csv(data.frame(x = 1), none)
  expect_error(select_cohort_ids("mimic_iv_demo", direct_ids_path = none, base_dir = base, raw_data_root = root),
               "Could not infer a hierarchy key")
  # requested tables must exist in the catalog and on disk
  expect_error(import_cohort_general("mimic_iv_demo", base_dir = base, raw_data_root = root, cohort = "scid",
                                     tables = "no_such_table", show_progress = FALSE), "not in the source catalog")
  empty <- file.path(e$base, "empty_root"); dir.create(file.path(empty, "mimic_iv_demo"), recursive = TRUE)
  expect_error(import_cohort_general("mimic_iv_demo", base_dir = file.path(base, "e"), raw_data_root = empty,
                                     no_filter = TRUE, show_progress = FALSE), "raw|Raw")
})

test_that("a raw CSV that cannot be parsed stops the import and names the file", {
  e <- .raw_cache_env()
  root <- file.path(e$base, "corrupt_root")
  dir.create(file.path(root, "mimic_iv_demo"), recursive = TRUE)
  file.copy(file.path(e$root, "mimic_iv_demo", c("hosp", "icu")), file.path(root, "mimic_iv_demo"), recursive = TRUE)
  f <- file.path(root, "mimic_iv_demo", "icu", "d_items.csv.gz")
  lines <- readLines(gzfile(f))
  # an unclosed quote: read on regardless, the parser would swallow the rows after it
  con <- gzfile(f, "w"); writeLines(c(lines[1:50], '999999,"broken label,x,y', lines[-(1:50)]), con); close(con)
  expect_error(import_cohort_general("mimic_iv_demo", base_dir = file.path(e$base, "corrupt"), raw_data_root = root,
                                     cohort = "scid", show_progress = FALSE),
               "table 'd_items' could not be parsed as CSV \\(d_items.csv.gz\\)")
})

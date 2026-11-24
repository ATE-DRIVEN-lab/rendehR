#!/usr/bin/env Rscript
# Benchmark: cohort-DB creation, DuckDB vs Postgres.
#
# Loads the SAME cohort parquet directory into (a) a temp DuckDB file and
# (b) a throwaway Postgres "bench" database, timing each and reporting the
# resulting on-disk size. Neither arm touches your real project database:
# the Postgres arm creates and drops `{db_name}_bench`.
#
# Usage (from the package root, with devtools):
#   Rscript inst/benchmarks/bench_cohort_load.R mimic_iv_demo
#
# Prereqs: cohort parquet already built (import_cohort_general()), the `duckdb`
# package installed, and (for the Postgres arm) a reachable server + creds.

suppressMessages({
  library(rendehR)     # or: devtools::load_all(".")
})

args     <- commandArgs(trailingOnly = TRUE)
src_name <- if (length(args) >= 1) args[[1]] else "mimic_iv_demo"
base_dir <- if (length(args) >= 2) args[[2]] else NULL
run_pg   <- !identical(Sys.getenv("BENCH_SKIP_POSTGRES"), "1")

paths        <- rendehR::ehr_paths(src_name = src_name, base_dir = base_dir)
schema_names <- rendehR::src_schema_names(src_name)
pq_dir       <- paths$cohort_parquet_dir

stopifnot("cohort parquet not found - run import_cohort_general() first" = dir.exists(pq_dir))
pq_files <- list.files(pq_dir, pattern = "\\.parquet$", full.names = TRUE)
pq_bytes <- sum(file.info(pq_files)$size)
cat(sprintf("\nSource: %s\nParquet: %d files, %.1f MB, at %s\n\n",
            src_name, length(pq_files), pq_bytes / 1e6, pq_dir))

results <- list()

# -- DuckDB arm -------------------------------------------------------------
duck_path <- tempfile(fileext = ".duckdb")
t0 <- Sys.time()
rendehR::run_db_duckdb(src_name, db_path = duck_path, base_dir = base_dir,
                   overwrite = TRUE, show_progress = TRUE)
duck_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
duck_mb   <- file.info(duck_path)$size / 1e6
results$duckdb <- data.frame(engine = "duckdb", seconds = duck_secs,
                             db_size_mb = duck_mb, stringsAsFactors = FALSE)
unlink(duck_path)

# -- Postgres arm (into a throwaway bench database) -------------------------
if (run_pg) {
  creds     <- rendehR::load_credentials(paths$credentials_json)
  bench_db  <- paste0(rendehR:::.ehr_resolve_db_name(NULL), "_bench")
  rendehR::ensure_database_exists(bench_db, creds)
  con <- rendehR::connect_postgres(bench_db, creds)

  t0 <- Sys.time()
  rendehR::create_schemas(con, schema_names)
  DBI::dbExecute(con, sprintf("SET search_path TO %s;", schema_names$raw))
  rendehR::run_sql_dir(con, paths$schema_sql_dir)
  rendehR::load_cohort_parquet_to_postgres(
    con = con, parquet_dir = pq_dir, schema = schema_names$raw,
    overwrite = TRUE, show_progress = TRUE)
  pg_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  pg_mb   <- as.numeric(DBI::dbGetQuery(
    con, "SELECT pg_database_size(current_database()) AS n")$n) / 1e6
  DBI::dbDisconnect(con)

  # Clean up the bench database (force = TRUE bypasses the drop_database prompt).
  admin <- rendehR::connect_postgres("postgres", creds)
  DBI::dbExecute(admin, sprintf("DROP DATABASE IF EXISTS %s WITH (FORCE);", bench_db))
  DBI::dbDisconnect(admin)

  results$postgres <- data.frame(engine = "postgres", seconds = pg_secs,
                                 db_size_mb = pg_mb, stringsAsFactors = FALSE)
} else {
  cat("Skipping Postgres arm (BENCH_SKIP_POSTGRES=1)\n")
}

# -- Report -----------------------------------------------------------------
tab <- do.call(rbind, results)
tab$seconds    <- round(tab$seconds, 2)
tab$db_size_mb <- round(tab$db_size_mb, 1)
cat("\n===== cohort load benchmark =====\n")
print(tab, row.names = FALSE)
if (!is.null(results$postgres))
  cat(sprintf("\nDuckDB speedup: %.1fx\n",
              results$postgres$seconds / results$duckdb$seconds))

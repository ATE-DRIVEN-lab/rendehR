# DuckDB backend: cohort import (DuckDB reads the raw .csv.gz and writes the
# cohort Parquet), loading that Parquet into a .duckdb file (run_db_duckdb()),
# raw modifications, cohort_info and concept extraction for
# run_one_source(engine = "duckdb"). Comorbidities, ISS, the ehr/ layer and
# harmonization stay PostgreSQL-only. DuckDB is embedded, so this path needs no
# server.

# Internal helpers -----------------------------------------------------------

.duckdb_require <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE))
    stop("The 'duckdb' package is required for the DuckDB backend. ",
         "Install it with install.packages('duckdb').", call. = FALSE)
}

# TRUE if `schema.table` exists in the connected DuckDB database.
.duckdb_table_exists <- function(con, schema, table) {
  nrow(DBI::dbGetQuery(con, paste0(
    "SELECT 1 FROM information_schema.tables ",
    "WHERE table_schema = ", DBI::dbQuoteString(con, schema),
    "  AND table_name  = ", DBI::dbQuoteString(con, table)
  ))) > 0
}

.duckdb_row_count <- function(con, schema, table) {
  id <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table))
  as.numeric(DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", id))$n)
}


# DuckDB uses the same package-owned schema contract as the Postgres loader:
# inst/sql/{family}/db_setup_sqls/schema/create_tables.sql. The SQL is written
# in a Postgres-friendly dialect, so this adapter normalizes types to DuckDB.
.duckdb_normalize_schema_type <- function(sql_type) {
  typ <- tolower(sql_type)
  typ <- gsub("--.*$", "", typ)
  typ <- gsub("\\bnot\\s+null\\b.*$", "", typ)
  typ <- gsub("\\bdefault\\b.*$", "", typ)
  typ <- trimws(typ)

  if (grepl("^timestamp\\b", typ)) return("TIMESTAMP")
  if (grepl("^date\\b", typ))      return("DATE")
  if (grepl("^bigint\\b", typ))    return("BIGINT")
  if (grepl("^smallint\\b", typ))  return("SMALLINT")
  if (grepl("^(integer|int)\\b", typ)) return("INTEGER")
  if (grepl("^(boolean|bool)\\b", typ)) return("BOOLEAN")
  if (grepl("^(double precision|double|real|float|numeric|decimal)\\b", typ))
    return("DOUBLE")
  if (grepl("^(text|varchar|character varying|character|char)\\b", typ))
    return("VARCHAR")
  "VARCHAR"
}

.duckdb_schema_sql_files <- function(schema_sql_dir) {
  if (is.null(schema_sql_dir) || !nzchar(schema_sql_dir)) return(character(0))
  if (dir.exists(schema_sql_dir))
    return(sort(list.files(schema_sql_dir, "\\.sql$", full.names = TRUE),
               method = "radix"))
  if (file.exists(schema_sql_dir)) schema_sql_dir else character(0)
}

# Parse simple CREATE TABLE blocks from the local package schema SQL. This is
# intentionally line-oriented because our schema files are formatted with one
# column per line and close each table with a standalone ");".
.duckdb_schema_from_sql_dir <- function(schema_sql_dir) {
  files <- .duckdb_schema_sql_files(schema_sql_dir)
  if (!length(files)) return(list())

  out <- list()
  failed <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    i <- 1L
    while (i <= length(lines)) {
      line <- lines[[i]]
      if (!grepl("^\\s*CREATE\\s+TABLE\\s+", line, ignore.case = TRUE)) {
        i <- i + 1L
        next
      }

      table <- sub(
        "^\\s*CREATE\\s+TABLE\\s+(IF\\s+NOT\\s+EXISTS\\s+)?(\\\"?[A-Za-z0-9_]+\\\"?\\.)?\\\"?([A-Za-z0-9_]+)\\\"?.*$",
        "\\3", line, ignore.case = TRUE
      )
      cols <- character(0)
      i <- i + 1L
      while (i <= length(lines) && !grepl("^\\s*\\)\\s*;", lines[[i]])) {
        col_line <- trimws(gsub("--.*$", "", lines[[i]]))
        col_line <- sub(",\\s*$", "", col_line)
        if (nzchar(col_line) &&
            !grepl("^(PRIMARY|CONSTRAINT|FOREIGN|UNIQUE|CHECK)\\b",
                   col_line, ignore.case = TRUE)) {
          m <- regexec("^\\\"?([^\\\"[:space:]]+)\\\"?\\s+(.+)$", col_line, perl = TRUE)
          hit <- regmatches(col_line, m)[[1]]
          if (length(hit) == 3L) {
            col <- hit[[2]]
            cols[col] <- .duckdb_normalize_schema_type(hit[[3]])
          }
        }
        i <- i + 1L
      }
      out[[tolower(table)]] <- cols
      i <- i + 1L
    }
  }
  out
}

.duckdb_schema_for_paths <- function(paths, cfg) {
  # Config is retained as a fallback for externally registered sources or any
  # table not present in the local schema SQL. The local schema wins wherever it
  # exists, so DuckDB follows the same corrected contract as Postgres.
  .merge_table_schema(.duckdb_schema_from_cfg(cfg),
                      .duckdb_schema_from_sql_dir(paths$schema_sql_dir))
}

.duckdb_typed_select_sql <- function(con, schema = NULL,
                                     available_cols = NULL,
                                     alias = "v") {
  alias_prefix <- if (is.null(alias) || !nzchar(alias)) "" else paste0(alias, ".")
  if (is.null(schema) || !length(schema))
    return(paste0(alias_prefix, "*"))

  out_cols <- names(schema)
  ref_cols <- out_cols
  if (!is.null(available_cols)) {
    idx  <- match(tolower(out_cols), tolower(available_cols))
    keep <- !is.na(idx)
    out_cols <- out_cols[keep]
    ref_cols <- available_cols[idx[keep]]
    schema   <- schema[keep]
  }

  cast_sql <- if (length(out_cols)) {
    refs <- paste0(alias_prefix, as.character(DBI::dbQuoteIdentifier(con, ref_cols)))
    outs <- as.character(DBI::dbQuoteIdentifier(con, out_cols))
    sprintf("TRY_CAST(%s AS %s) AS %s", refs, unname(schema), outs)
  } else character(0)

  # Passthrough: any available column the schema does not mention is kept as-is
  # instead of being dropped. The built-in schema SQL is meant to be complete,
  # but real source CSVs can carry drift columns the bundled schema predates;
  # preserving them (untyped) avoids silent data loss. Only possible when the
  # caller told us which columns actually exist (available_cols).
  pass_sql <- character(0)
  if (!is.null(available_cols)) {
    extra <- available_cols[!tolower(available_cols) %in% tolower(ref_cols)]
    if (length(extra))
      pass_sql <- paste0(alias_prefix,
                         as.character(DBI::dbQuoteIdentifier(con, extra)))
  }

  sel <- c(cast_sql, pass_sql)
  if (!length(sel)) return(paste0(alias_prefix, "*"))
  paste(sel, collapse = ", ")
}


.duckdb_table_schema <- function(table_schema, table) {
  if (is.null(table_schema) || !length(table_schema)) return(NULL)
  table_schema[[table]] %||% table_schema[[tolower(table)]]
}

# ---------------------------------------------------------------------------
# Connection
# ---------------------------------------------------------------------------

#' Open a connection to a DuckDB cohort database
#'
#' @param db_path Path to the \code{.duckdb} file. Its parent directory is
#'   created if needed. Use \code{":memory:"} for an in-memory database.
#' @param read_only Open read-only (allows concurrent readers). Default FALSE.
#' @return A DBI connection. Disconnect with
#'   \code{DBI::dbDisconnect(con, shutdown = TRUE)}.
#' @examples
#' con <- connect_duckdb(":memory:")
#' DBI::dbGetQuery(con, "SELECT 42 AS answer")
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' @export
connect_duckdb <- function(db_path, read_only = FALSE) {
  .duckdb_require()
  if (!identical(db_path, ":memory:")) {
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
  # DuckDB's own default out-of-core spill directory (lazily created under R's
  # tempdir() the first time a large query needs to spill to disk) failed with
  # "No such file or directory" on a real, large cohort-selection query -
  # DuckDB's internal creation of that path isn't reliably recursive. Pre-create
  # and set an explicit one instead of depending on DuckDB's lazy default.
  temp_dir <- file.path(tempdir(), "duckdb_temp")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(con, sprintf("SET temp_directory=%s;",
                              DBI::dbQuoteString(con, temp_dir)))
  con
}

# ---------------------------------------------------------------------------
# Parquet -> DuckDB loader
# ---------------------------------------------------------------------------

#' Load all parquet files from a cohort directory into DuckDB
#'
#' Each \code{.parquet} file in \code{parquet_dir} becomes one table (table name
#' = filename without extension) in \code{schema}. Unlike the Postgres loader,
#' this does not stream batches through R or need pre-created DDL: DuckDB reads
#' the parquet directly with \code{CREATE TABLE ... AS SELECT * FROM
#' read_parquet(...)}, inferring column types from the parquet metadata (which
#' arrow wrote with correct types). That native path is the main thing this
#' backend exists to benchmark.
#'
#' @param con A DuckDB DBI connection (from \code{connect_duckdb()}).
#' @param parquet_dir Directory of cohort parquet files.
#' @param schema Target schema (created if absent).
#' @param overwrite If TRUE (default), each table is replaced
#'   (\code{CREATE OR REPLACE TABLE}). If FALSE, a table that already exists with
#'   a matching row count is skipped; otherwise it is (re)created.
#' @param show_progress Print per-table progress messages.
#' @param table_schema Optional named list of DuckDB column types, table ->
#'   named character vector column -> type. When supplied, tables are cast to
#'   this schema as they are materialized.
#' @return Invisibly, a data frame of \code{table}, \code{rows}, \code{seconds}.
#' @examples
#' \dontrun{
#' con <- connect_duckdb("data/duckdb/mimic_iv_demo.duckdb")
#' load_cohort_parquet_to_duckdb(con, "data/cohorts_data/mimic_iv_demo_parquet", "mimic_iv_demo_coh")
#' }
#' @export
load_cohort_parquet_to_duckdb <- function(con, parquet_dir, schema,
                                          overwrite = TRUE,
                                          show_progress = TRUE,
                                          table_schema = NULL) {
  .duckdb_require()
  if (!dir.exists(parquet_dir))
    stop("parquet_dir does not exist: ", parquet_dir)
  if (!DBI::dbIsValid(con))
    stop("Invalid database connection.")
  schema <- .ehr_validate_db_identifier(schema, "schema name")

  parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$",
                              full.names = TRUE, recursive = FALSE)
  if (length(parquet_files) == 0)
    stop("No parquet files found in: ", parquet_dir)

  DBI::dbExecute(con, sprintf("CREATE SCHEMA IF NOT EXISTS %s;",
                              DBI::dbQuoteIdentifier(con, schema)))

  report <- vector("list", length(parquet_files))
  for (i in seq_along(parquet_files)) {
    pf         <- parquet_files[[i]]
    tbl_name   <- tools::file_path_sans_ext(basename(pf))
    tbl_id     <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = tbl_name))
    start_time <- Sys.time()

    exists <- .duckdb_table_exists(con, schema, tbl_name)
    if (exists && !overwrite) {
      loaded       <- .duckdb_row_count(con, schema, tbl_name)
      parquet_rows <- as.numeric(DBI::dbGetQuery(con, paste0(
        "SELECT count(*) AS n FROM read_parquet(",
        DBI::dbQuoteString(con, pf), ")"))$n)
      if (isTRUE(parquet_rows > 0) && isTRUE(loaded == parquet_rows)) {
        if (show_progress)
          message("Skipping (already loaded): ", tbl_name, "  (", loaded, " rows)")
        report[[i]] <- data.frame(table = tbl_name, rows = loaded, seconds = 0,
                                  stringsAsFactors = FALSE)
        next
      }
    }

    if (show_progress) message("Loading to DuckDB: ", tbl_name)

    # Every path that reaches here is meant to (re)create the table: overwrite,
    # a resume-mode row-count mismatch, or a table that does not exist yet.
    # CREATE OR REPLACE is correct in all three (a plain IF NOT EXISTS would
    # silently keep a stale, wrong-count table in the mismatch case).
    tbl_schema <- .duckdb_table_schema(table_schema, tbl_name)
    parquet_cols <- DBI::dbGetQuery(con, sprintf(
      "DESCRIBE SELECT * FROM read_parquet(%s);",
      DBI::dbQuoteString(con, pf)
    ))$column_name
    select_sql <- .duckdb_typed_select_sql(con, tbl_schema,
                                           available_cols = parquet_cols,
                                           alias = "p")
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS SELECT %s FROM read_parquet(%s) AS p;",
      tbl_id, select_sql, DBI::dbQuoteString(con, pf)
    ))

    n_rows  <- .duckdb_row_count(con, schema, tbl_name)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (show_progress)
      message("  Done: ", tbl_name, "  (", prettyNum(n_rows, big.mark = ","),
              " rows, ", round(elapsed, 1), "s)")
    report[[i]] <- data.frame(table = tbl_name, rows = n_rows,
                              seconds = elapsed, stringsAsFactors = FALSE)
  }

  invisible(do.call(rbind, report))
}

# ---------------------------------------------------------------------------
# run_db equivalent
# ---------------------------------------------------------------------------

#' Create a cohort database in DuckDB
#'
#' DuckDB analogue of the Postgres path in \code{run_db()}: opens (or creates)
#' the \code{.duckdb} file, creates the source schemas, and loads the cohort
#' parquet into the raw schema. Concept extraction is not yet ported to DuckDB;
#' this covers the load stage only.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param db_path Path to the \code{.duckdb} file. Default: a file named after
#'   the source under `paths.duckdb_root` (`data/duckdb` by default).
#' @param base_dir Project root directory.
#' @param overwrite Passed to \code{load_cohort_parquet_to_duckdb()}.
#' @param show_progress Print progress messages.
#' @return Invisibly, the load report data frame.
#' @examples
#' \dontrun{
#' run_db_duckdb("mimic_iv_demo", db_path = "data/duckdb/mimic_iv_demo.duckdb")
#' }
#' @export
run_db_duckdb <- function(src_name, db_path = NULL, base_dir = NULL,
                          overwrite = TRUE, show_progress = TRUE) {
  .duckdb_require()
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  cfg          <- load_src_cfg(paths$data_config_json, paths$src_name)
  schema_names <- src_schema_names(src_name)
  db_path      <- db_path %||% paths$duckdb_path

  artifact <- .ehr_check_parquet_artifact(paths$cohort_parquet_dir)
  if (!isTRUE(artifact$ok))
    stop("Refusing DuckDB load: invalid cohort artifact (",
         artifact$reason, ").", call. = FALSE)

  con <- connect_duckdb(db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  for (s in unlist(schema_names))
    DBI::dbExecute(con, sprintf("CREATE SCHEMA IF NOT EXISTS %s;",
                                DBI::dbQuoteIdentifier(con, s)))

  report <- load_cohort_parquet_to_duckdb(
    con           = con,
    parquet_dir   = paths$cohort_parquet_dir,
    schema        = schema_names$raw,
    overwrite     = overwrite,
    show_progress = show_progress,
    table_schema  = .duckdb_schema_for_paths(paths, cfg)
  )

  if (show_progress)
    message("Parquet loaded into DuckDB: ", src_name, "\n  ", db_path)
  invisible(report)
}

# ---------------------------------------------------------------------------
# Raw modifications
# ---------------------------------------------------------------------------

#' Build modified (_mod) raw tables in a DuckDB cohort database
#'
#' Runs SQL files from the source's `raw_mods` directory against the DuckDB raw
#' schema. The SQL is passed through `adapt_sql_for_duckdb()` so the same
#' Postgres-oriented raw-mod files can be reused where syntax differences are
#' conservative and known.
#'
#' @param src_name Source name (e.g. `"eicu"` or `"eicu_demo"`).
#' @param db_path Path to the DuckDB database. Defaults to `ehr_paths()`'s
#'   source DuckDB path.
#' @param base_dir Project root directory.
#' @param skip_on_error If TRUE, warn and continue evaluating the remaining
#'   files, then fail the overall operation if any required file failed.
#'   Default FALSE.
#' @param show_progress Print per-file progress messages. Default TRUE.
#' @return Invisibly TRUE if mods were run, NULL if no mod directory/files exist.
#' @examples
#' \dontrun{
#' run_raw_mods_duckdb("eicu_demo", db_path = "data/duckdb/eicu_demo.duckdb")
#' }
#' @export
run_raw_mods_duckdb <- function(src_name, db_path = NULL, base_dir = NULL,
                                skip_on_error = FALSE,
                                show_progress = TRUE) {
  .duckdb_require()
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  schema_names <- src_schema_names(src_name)
  schema_names$raw <- .ehr_validate_db_identifier(
    schema_names$raw, "raw schema"
  )
  db_path      <- db_path %||% paths$duckdb_path
  sql_dir      <- paths$raw_mods_sql_dir

  if (!nzchar(sql_dir) || !dir.exists(sql_dir)) {
    if (show_progress)
      message("No raw_mods SQL directory found for '", src_name, "' - skipping.")
    return(invisible(NULL))
  }

  files <- sort(list.files(sql_dir, pattern = "\\.sql$", full.names = TRUE),
                method = "radix")
  if (length(files) == 0L) {
    if (show_progress)
      message("raw_mods directory is empty for '", src_name, "' - skipping.")
    return(invisible(NULL))
  }

  con <- connect_duckdb(db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = !identical(db_path, ":memory:")), add = TRUE)

  if (!schema_names$raw %in% DBI::dbGetQuery(con, "SELECT schema_name FROM information_schema.schemata")$schema_name)
    stop("DuckDB raw schema does not exist: ", schema_names$raw,
         ". Run run_db_duckdb() first.", call. = FALSE)

  DBI::dbExecute(con, sprintf(
    "SET search_path TO %s",
    as.character(DBI::dbQuoteIdentifier(con, schema_names$raw))
  ), immediate = TRUE)
  .ehr_write_raw_mod_lookups(con, src_name, paths$config_dir, schema_names$raw)

  if (show_progress)
    message("Building DuckDB raw mods for '", src_name, "' (",
            length(files), " file(s)) ...")

  failed <- character()
  for (f in files) {
    tbl_name <- tools::file_path_sans_ext(basename(f))
    if (show_progress) message("  ", tbl_name, " ...")
    sql <- .duckdb_sql_text(f)
    if (skip_on_error) {
      tryCatch(
        DBI::dbExecute(con, sql, immediate = TRUE),
        error = function(e) {
          failed <<- c(failed, tbl_name)
          warning("  FAILED: ", tbl_name, ": ", conditionMessage(e),
                  call. = FALSE)
        }
      )
    } else {
      DBI::dbExecute(con, sql, immediate = TRUE)
    }
  }

  if (length(failed))
    stop("Required raw modifications failed: ", paste(failed, collapse = ", "),
         call. = FALSE)
  if (show_progress) message("DuckDB raw mods done: ", src_name)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# SQL cohort selection over the raw CSVs (no parquet, no database load)
# ---------------------------------------------------------------------------

#' Adapt conservative PostgreSQL SQL syntax to DuckDB
#'
#' This is intentionally small and rule-based. It covers syntax differences we
#' use in bundled schema/raw-mod/cohort SQL, while leaving query structure and
#' backend-specific semantics visible to reviewers.
#'
#' @param sql SQL text or character vector.
#' @return A single DuckDB-compatible SQL string.
#' @examples
#' adapt_sql_for_duckdb("SELECT subject_id, valuenum::numeric FROM labevents")
#' @export
adapt_sql_for_duckdb <- function(sql) {
  txt <- paste(sql, collapse = "\n")

  txt <- gsub("\\bTIMESTAMP\\s*\\([0-9]+\\)", "TIMESTAMP",
              txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("::\\s*double\\s+precision\\b", "::DOUBLE",
              txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("::\\s*text\\b", "::VARCHAR",
              txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub(
    "DROP\\s+TABLE\\s+IF\\s+EXISTS\\s+([^;]+?)\\s+CASCADE\\s*;?",
    "DROP TABLE IF EXISTS \\1;",
    txt,
    ignore.case = TRUE,
    perl = TRUE
  )
  txt <- gsub(
    "substring\\s*\\(\\s*([A-Za-z_][A-Za-z0-9_\\.]*?)\\s+from\\s+'([^']*)'\\s*\\)",
    "regexp_extract(\\1, '\\2')",
    txt,
    ignore.case = TRUE,
    perl = TRUE
  )

  txt
}

# Read a .sql file if `x` is an existing path, otherwise treat `x` as inline
# SQL. A trailing semicolon/whitespace is trimmed so the text is usable as a
# single statement in dbGetQuery().
.duckdb_sql_text <- function(x, adapt = TRUE) {
  txt <- if (length(x) == 1L && file.exists(x))
    paste(readLines(x, warn = FALSE), collapse = "\n") else paste(x, collapse = "\n")
  txt <- sub("[;[:space:]]+$", "", txt)
  if (isTRUE(adapt)) adapt_sql_for_duckdb(txt) else txt
}

# Progress must not alter SQL text or statement execution boundaries. Submit
# the complete file through the same DBI call with progress enabled or disabled.
.duckdb_execute_setup_sql <- function(con, sql, label, show_progress = TRUE) {
  started <- proc.time()[["elapsed"]]
  tryCatch(
    DBI::dbExecute(con, sql, immediate = TRUE),
    error = function(e) {
      if (show_progress)
        message("  setup_sql ", label,
                sprintf(" FAILED (%.1fs)", proc.time()[["elapsed"]] - started))
      stop(e)
    }
  )
  if (show_progress)
    message("  setup_sql ", label,
            sprintf(" done (%.1fs)", proc.time()[["elapsed"]] - started))
  invisible(NULL)
}

#' Select cohort key IDs by running SQL over the raw CSVs in DuckDB
#'
#' Runs a SQL cohort definition directly against the downloaded PhysioNet
#' CSV(.gz) files with DuckDB and returns the matching cohort key IDs - without
#' building parquet and without loading anything into a persistent database.
#' This is the intended entry point for "define a cohort in SQL, get IDs, then
#' import only that cohort": the parquet is produced \emph{after} selection, so
#' selection must read the CSVs.
#'
#' Each configured table is exposed as a view named after the table (e.g.
#' \code{chartevents}) built from its raw CSV file(s) via DuckDB's
#' \code{read_csv_auto} (gzip is auto-detected, multi-file tables are unioned),
#' so definition SQL references bare table names exactly as against a loaded
#' schema. Views are lazy: only the tables your SQL actually touches are scanned.
#'
#' For a multi-step definition (e.g. Sepsis-3 = SOFA + suspicion of infection),
#' pass the dependency SQL in order via \code{setup_sql}; each builds a table the
#' next reads. \code{cohort_sql} is the final \code{SELECT} whose result must
#' contain \code{key}. Feed the result (or \code{out_csv}) to
#' \code{import_cohort(direct_ids_path = ...)}.
#'
#' @param src_name Source name (resolves the raw data dir and table->file map
#'   from the source config).
#' @param cohort_sql Final selection SQL - a single \code{SELECT} as an inline
#'   string or a path to a \code{.sql} file. Its result must include \code{key}.
#' @param key Cohort key column to return distinct values of (e.g.
#'   \code{"subject_id"} for MIMIC, \code{"patientunitstayid"} for eICU).
#' @param setup_sql Optional ordered character vector of dependency SQL - each an
#'   inline string or \code{.sql} file path - executed before \code{cohort_sql}.
#' @param tables Optional character vector of table names to expose. Default NULL
#'   exposes every configured table whose CSV file(s) are present.
#' @param base_dir Project root directory.
#' @param raw_data_root Optional parent storage-root override for raw source
#'   data. The source directory is derived as \code{{raw_data_root}/{src_name}}
#'   (matches \code{ehr_paths()}/\code{import_cohort_general()}'s parameter -
#'   the older \code{raw_data_dir} name is retired, see \code{ehr_paths()}).
#' @param out_csv Optional path to write the IDs as a one-column CSV (header =
#'   \code{key}), ready for \code{import_cohort(direct_ids_path = out_csv)}.
#' @param db_path DuckDB database file. Default \code{":memory:"} (nothing is
#'   persisted). Pass a path to keep the intermediate tables for inspection.
#' @param memory_limit,threads Optional DuckDB resource settings for large
#'   sources (e.g. \code{memory_limit = "8GB"}) - \code{setup_sql} scans full
#'   raw tables (e.g. a full CHARTEVENTS), which can exceed DuckDB's default
#'   memory cap on a memory-constrained machine. \code{threads} defaults to
#'   DuckDB's normal parallelism for full sources (single-threaded for demo
#'   sources, see \code{.duckdb_configure_raw_csv_connection()}); pass it
#'   explicitly to override either way.
#' @param use_parquet_cache If TRUE (default), a table whose raw CSV has
#'   already been cached as parquet by \code{cache_raw_parquet_duckdb()} (at
#'   \code{paths$raw_parquet_cache_dir}) is read from that parquet instead of
#'   re-parsing the CSV - substantially faster for repeated selection against
#'   the same source. Tables without a cached parquet still fall back to CSV.
#'   Set FALSE to always read CSV.
#' @param show_progress Print each \code{setup_sql} file before
#'   execution, followed by elapsed seconds on completion or failure.
#' @return A data frame with a single column named \code{key} of distinct IDs
#'   (invisibly when \code{out_csv} is written).
#' @keywords internal
#' @noRd
.ehr_select_cohort_ids_duckdb <- function(src_name, cohort_sql, key,
                                     setup_sql = NULL, tables = NULL,
                                     base_dir = NULL, raw_data_root = NULL,
                                     out_csv = NULL, db_path = ":memory:",
                                     memory_limit = NULL, threads = NULL,
                                     use_parquet_cache = TRUE,
                                     show_progress = TRUE) {
  .duckdb_require()
  paths <- ehr_paths(src_name = src_name, base_dir = base_dir,
                     raw_data_root = raw_data_root)
  cfg   <- load_src_cfg(paths$data_config_json, paths$src_name)
  tbls  <- tables %||% names(cfg$tables)

  missing_cfg <- setdiff(tbls, names(cfg$tables))
  if (length(missing_cfg))
    stop("Tables not in config for '", src_name, "': ",
         paste(missing_cfg, collapse = ", "), call. = FALSE)

  # Resolve each table to its raw CSV file(s); keep only tables fully present.
  # An explicit `tables` request errors on any missing file; the default (all
  # tables) silently skips absent ones so a partial download still works.
  file_map <- lapply(tbls, function(tbl)
    file.path(paths$raw_dir, cfg$tables[[tbl]]$files))
  names(file_map) <- tbls
  present <- vapply(file_map, function(fs) all(file.exists(fs)), logical(1))
  if (!is.null(tables) && any(!present))
    stop("Raw CSV missing for requested table(s): ",
         paste(tbls[!present], collapse = ", "),
         "\nRun download_source(\"", src_name, "\") first.", call. = FALSE)
  file_map <- file_map[present]
  if (length(file_map) == 0)
    stop("No raw CSV files found under ", paths$raw_dir,
         " - run download_source(\"", src_name, "\") first.", call. = FALSE)

  con <- connect_duckdb(db_path)
  if (!is.null(memory_limit))
    DBI::dbExecute(con, sprintf("SET memory_limit='%s';", memory_limit))
  if (!is.null(threads))
    DBI::dbExecute(con, sprintf("SET threads=%d;", as.integer(threads)))
  else
    .duckdb_configure_raw_csv_connection(con, .duckdb_is_demo_source(paths$src_name))
  # Only distinct ids are returned; output order is never meaningful here, so
  # this is a safe, free memory reduction on large scans.
  DBI::dbExecute(con, "SET preserve_insertion_order=false;")
  on.exit(DBI::dbDisconnect(con, shutdown = !identical(db_path, ":memory:")), add = TRUE)

  # Expose each table as a bare-named lazy view over its raw CSV(.gz) file(s),
  # imposing the package-owned schema SQL types before cohort SQL runs.
  cfg_schema <- .duckdb_schema_for_paths(paths, cfg)
  for (tbl in names(file_map)) {
    tbl_schema <- .duckdb_table_schema(cfg_schema, tbl)
    cache_file <- file.path(paths$raw_parquet_cache_dir, paste0(tbl, ".parquet"))

    # A cached parquet (from cache_raw_parquet_duckdb()) preserves the exact
    # original CSV column names, so it is a drop-in replacement for the raw
    # CSV read below - columnar, compressed, already typed by parquet's own
    # metadata, no per-row text parsing. Only trusted when its manifest
    # fingerprint still matches the source file(s) - see
    # .ehr_cache_entry_fresh().
    if (isTRUE(use_parquet_cache) && parquet_has_rows(cache_file) &&
        .ehr_cache_entry_fresh(paths$raw_parquet_cache_dir, tbl, file_map[[tbl]])) {
      available_cols <- DBI::dbGetQuery(con, sprintf(
        "DESCRIBE SELECT * FROM read_parquet(%s);",
        DBI::dbQuoteString(con, cache_file)
      ))$column_name
      select_sql <- .duckdb_typed_select_sql(
        con, tbl_schema,
        available_cols = available_cols,
        alias = "src"
      )
      DBI::dbExecute(con, sprintf(
        "CREATE OR REPLACE VIEW %s AS SELECT %s FROM read_parquet(%s) AS src;",
        DBI::dbQuoteIdentifier(con, tbl), select_sql,
        DBI::dbQuoteString(con, cache_file)
      ))
      next
    }

    headers <- lapply(file_map[[tbl]], .csv_header_cols)
    available_cols <- unique(unlist(headers, use.names = FALSE))
    read_expr <- .duckdb_csv_read_expr(
      con, file_map[[tbl]], headers = headers
    )
    select_sql <- .duckdb_typed_select_sql(
      con, tbl_schema,
      available_cols = available_cols,
      alias = "src"
    )
    DBI::dbExecute(con, sprintf("CREATE OR REPLACE VIEW %s AS SELECT %s FROM %s AS src;",
                                DBI::dbQuoteIdentifier(con, tbl), select_sql, read_expr))
  }

  # Build up any dependency tables in order (SOFA, suspicion_of_infection, ...).
  # Each file runs unchunked against the full raw tables (e.g. a full
  # CHARTEVENTS) - unlike the cohort-table build path, a GROUP BY/window
  # aggregation can't be split into independent hash-bucketed INSERTs without
  # merging partial aggregates, so there is no chunk_rows equivalent here.
  # The per-file message at least identifies which step is running when one
  # of them is the thing that exhausts memory.
  for (i in seq_along(setup_sql)) {
    if (show_progress) {
      message(
        "  setup_sql ", i, "/", length(setup_sql), ": ",
        basename(setup_sql[[i]]), "..."
      )
    }
    .duckdb_execute_setup_sql(con, .duckdb_sql_text(setup_sql[[i]]),
                             basename(setup_sql[[i]]), show_progress)
  }

  res <- DBI::dbGetQuery(con, .duckdb_sql_text(cohort_sql))
  if (!key %in% names(res))
    stop("cohort_sql result has no '", key, "' column. Found: ",
         paste(names(res), collapse = ", "), call. = FALSE)

  ids <- unique(res[[key]][!is.na(res[[key]])])
  message("  SQL cohort: ", length(ids), " unique ", key, " values")
  out <- stats::setNames(data.frame(ids, stringsAsFactors = FALSE), key)

  if (!is.null(out_csv)) {
    # Write every column the cohort_sql selected (e.g. subject_id, hadm_id,
    # icustay_id) so all ID levels are available downstream; deduped on full
    # rows and filtered to non-NULL key. import_cohort() reads only its key
    # column from this CSV, so extra ID columns are harmless.
    full <- unique(res[!is.na(res[[key]]), , drop = FALSE])
    readr::write_csv(full, out_csv)
    message("  Wrote ", nrow(full), " rows x ", ncol(full), " col(s) to: ", out_csv)
    return(invisible(out))
  }
  out
}

# Resolve the raw table(s) a bundled example cohort (.example_cohort_registry())
# actually reads, for a given source - so cache_raw_parquet_duckdb(cohort = ...)
# doesn't require the caller to know or hand-type them.
#
# ICD cohorts (kind = "icd"): only the configured seed table(s) are needed to
# resolve IDs (cohort_seed_tables()).
#
# SQL cohorts (kind = "sql", e.g. sepsis3): the packaged per-family SQL chain
# under inst/sql/{family}/{sql_subdir}/ is read as text and matched against
# every configured table name (word-boundary, case-insensitive) - reliable
# because cohort/setup SQL always references raw tables by their bare
# configured name (see .duckdb_csv_read_expr()'s view-naming contract).
.ehr_cohort_cache_tables <- function(src_name, cfg, cohort, paths) {
  reg <- .example_cohort_registry()
  if (!cohort %in% names(reg))
    stop("Unknown cohort '", cohort, "'. See list_example_cohorts().",
         call. = FALSE)
  entry      <- reg[[cohort]]
  all_tables <- names(cfg$tables)

  # The cohort hierarchy spine (visit_windows/admission_windows/icu_windows)
  # is built for every cohort, regardless of kind - so whatever
  # window_tables.sql reads is always needed on top of whatever the cohort
  # itself resolves to below, or those table(s) stay uncached and every
  # build still re-parses their CSV for the spine step.
  spine_sql    <- file.path(paths$helper_sql_dir, "window_tables.sql")
  spine_tables <- character(0)
  if (file.exists(spine_sql)) {
    spine_txt <- paste(readLines(spine_sql, warn = FALSE), collapse = "\n")
    spine_tables <- all_tables[vapply(all_tables, function(t)
      grepl(paste0("\\b", t, "\\b"), spine_txt, ignore.case = TRUE), logical(1))]
  }

  if (identical(entry$kind, "icd")) {
    seeds <- cohort_seed_tables(src_name)
    if (is.null(seeds) || !length(seeds))
      stop("No cohort_seed_tables configured for '", src_name,
           "'; cannot resolve tables for cohort '", cohort, "'.", call. = FALSE)
    return(union(intersect(seeds, all_tables), spine_tables))
  }

  if (identical(entry$kind, "sql")) {
    fam     <- src_family(src_name)
    sql_dir <- .ehr_extdata("sql", fam, entry$sql_subdir)
    if (!dir.exists(sql_dir))
      stop("No packaged SQL chain found for cohort '", cohort, "' / family '",
           fam, "'. Is the package installed, not just loaded?", call. = FALSE)
    files <- sort(list.files(sql_dir, pattern = "\\.sql$", full.names = TRUE),
                  method = "radix")
    if (!length(files))
      stop("No .sql files found under: ", sql_dir, call. = FALSE)
    txt <- paste(vapply(files, function(f)
      paste(readLines(f, warn = FALSE), collapse = "\n"), character(1)),
      collapse = "\n")
    hits <- all_tables[vapply(all_tables, function(t)
      grepl(paste0("\\b", t, "\\b"), txt, ignore.case = TRUE), logical(1))]
    if (!length(hits))
      stop("No configured table names matched the SQL chain for cohort '",
           cohort, "'.", call. = FALSE)
    return(union(hits, spine_tables))
  }

  stop("Cohort '", cohort, "' has unsupported kind '", entry$kind, "'.",
       call. = FALSE)
}

# Cheap per-table freshness fingerprint for the raw parquet cache: each
# source file's basename, size, and mtime (whole seconds - some filesystems
# don't preserve sub-second precision, which would otherwise make an
# unchanged file's fingerprint flap between runs). Deliberately NOT a content
# hash - hashing a raw CSV that can be tens of GB would scan the entire file
# and defeat the point of caching it. size+mtime is the same fast heuristic
# make/rsync use to detect an unchanged file without reading it.
.ehr_raw_source_fingerprint <- function(files) {
  info <- file.info(files)
  parts <- sprintf("%s:%.0f:%.0f", basename(files), info$size,
                   floor(as.numeric(info$mtime)))
  paste(sort(parts), collapse = "|")
}

.ehr_cache_manifest_path <- function(cache_dir) {
  file.path(cache_dir, "_cache_manifest.json")
}

.ehr_read_cache_manifest <- function(cache_dir) {
  path <- .ehr_cache_manifest_path(cache_dir)
  if (!file.exists(path)) return(list())
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) list()
  )
}

# Records the fingerprint the cache was built from, keyed by table, so a
# later run can tell whether the source file(s) changed since caching -
# read-modify-write against the existing manifest so caching a subset of
# tables doesn't clobber other tables' entries.
.ehr_write_cache_manifest_entry <- function(cache_dir, table, files) {
  manifest <- .ehr_read_cache_manifest(cache_dir)
  manifest[[table]] <- list(
    fingerprint = .ehr_raw_source_fingerprint(files),
    cached_at   = as.character(Sys.time())
  )
  jsonlite::write_json(
    manifest, .ehr_cache_manifest_path(cache_dir),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  invisible(manifest)
}

# TRUE only when a manifest entry exists for `table` and its stored
# fingerprint matches the raw source files' current size/mtime exactly. A
# missing entry (e.g. a cache directory built before this manifest existed)
# is NOT treated as fresh - callers fall back to their normal "no usable
# cache" path (rebuild, or read raw CSV) instead of silently trusting an
# unverified file, which is the gap this fingerprint closes: previously any
# nonempty parquet file at the expected path was trusted regardless of
# whether its source CSV had since changed.
.ehr_cache_entry_fresh <- function(cache_dir, table, files) {
  entry <- .ehr_read_cache_manifest(cache_dir)[[table]]
  if (is.null(entry) || is.null(entry$fingerprint)) return(FALSE)
  identical(entry$fingerprint, .ehr_raw_source_fingerprint(files))
}

#' Materialize a source's raw CSVs as a parquet cache for fast cohort selection
#'
#' Converts each configured table's raw CSV(.gz) file(s) into a single parquet
#' file under \code{paths$raw_parquet_cache_dir}, using the exact same typed
#' read (\code{.duckdb_csv_read_expr()} + package schema TRY_CAST) that
#' \code{.ehr_select_cohort_ids_duckdb()} otherwise applies to CSV on every
#' call - so the cache is a transparent, drop-in stand-in for the CSV: same
#' column names, same types. Original CSV column names are kept.
#'
#' The cache is written inside the project, under
#' \code{<base_dir>/data/raw_parquet_cache/<src_name>} (set
#' \code{paths.raw_parquet_cache_root} in the project config to move it),
#' never into the raw source directory, which is treated as read-only.
#'
#' Run this once per source (or after a source is re-downloaded); every
#' subsequent \code{select_cohort_ids_duckdb()} / \code{import_cohort_general()}
#' call against tables with a cache automatically reads parquet instead of
#' re-parsing CSV (see \code{use_parquet_cache} on
#' \code{.ehr_select_cohort_ids_duckdb()}).
#'
#' By default (\code{keep = FALSE}) the cache is scratch space: it is removed
#' automatically when the R session ends, so repeated selection calls within
#' one session/script are fast without leaving parquet files behind
#' afterward. Pass \code{keep = TRUE} to have it persist across sessions - to
#' remove a kept cache later, just \code{unlink(ehr_paths(src_name)$raw_parquet_cache_dir, recursive = TRUE)}.
#'
#' Not run automatically by \code{run_one_source()} (the canonical pipeline
#' entry point) - call it directly beforehand, or request the
#' \code{"cache_raw_parquet"} stage via \code{run_one_source(stages = ...)}.
#'
#' @param src_name Source name (e.g. \code{"mimic_iii"}).
#' @param tables Character vector of table names to cache. Default NULL, in
#'   which case \code{all_tables}/\code{cohort} decide what gets cached (see
#'   below) - pass this explicitly to scope the cache down to just the tables
#'   your \code{cohort_sql}/\code{setup_sql} actually queries, skipping large
#'   unused tables (e.g. free-text tables) that \code{all_tables} would
#'   otherwise include.
#' @param cohort Optional name of a bundled example cohort (see
#'   \code{list_example_cohorts()}, e.g. \code{"sepsis3"} or \code{"scid"})
#'   - resolves \code{tables} automatically instead of you
#'   having to name them: the tables its packaged SQL chain references (SQL
#'   cohorts) or its configured \code{cohort_seed_tables} (ICD cohorts).
#'   Ignored when \code{tables} is supplied explicitly; otherwise takes
#'   precedence over \code{all_tables} (an explicit cohort is more specific
#'   than the "cache everything" default).
#' @param all_tables If TRUE (the default), cache every table configured for
#'   this source. Ignored whenever \code{tables} or \code{cohort} apply -
#'   pass \code{all_tables = FALSE} together with neither of those to get the
#'   old no-op ("nothing to cache") behavior instead.
#' @param base_dir Project root directory.
#' @param raw_data_root Optional parent storage-root override for raw source
#'   data (as in \code{ehr_paths()}).
#' @param overwrite Re-cache tables that already have a non-empty cached
#'   parquet. Default FALSE (skip tables already cached).
#' @param keep Keep the cache after this R session ends. Default FALSE:
#'   the cache directory is deleted automatically on session exit, so it is
#'   scratch space for this session unless you opt in to persisting it.
#' @param validate_columns If TRUE (the default), check every raw value
#'   against its configured type before writing the cache and record
#'   conversion-failure counts (values that fail \code{TRY_CAST} become a
#'   silent \code{NULL} otherwise, with no way to later tell "genuinely
#'   blank" from "failed to parse" - the original raw text is gone once
#'   cached). Writes a per-column report to
#'   \code{{cache_dir}/_conversion_report.csv} and warns if any column had
#'   failures. Costs a second full scan per table (validate, then
#'   convert-and-write) - set FALSE to skip it and cache at the old,
#'   unvalidated speed.
#' @param memory_limit,threads,temp_directory Optional DuckDB resource
#'   settings for large sources, as in \code{import_cohort_general()} and
#'   \code{materialize_cohort_duckdb()}. \code{temp_directory} lets DuckDB spill to
#'   disk instead of running out of memory on a large table - a scratch spill
#'   directory is always used even when this is left NULL (removed once the
#'   function returns), since the underlying connection is \code{:memory:}
#'   and otherwise has nowhere to spill at all; pass this explicitly (e.g.
#'   \code{temp_directory = "/scratch"}) only to point spilling at a specific
#'   disk (more free space, faster I/O) instead of the default scratch
#'   location.
#' @param show_progress Print per-table progress messages.
#' @return Invisibly, the cache directory path, or NULL if \code{tables} was
#'   NULL, \code{cohort} was NULL, and \code{all_tables} was FALSE (nothing
#'   to do).
#' @examples
#' \dontrun{
#' cache_raw_parquet_duckdb("mimic_iv_demo", tables = c("labevents", "chartevents"))
#' }
#' @export
#' @param cohort_selection Optional overrides from `cohort_selection_options()` or a named list.
#' @param loading Optional overrides from `loading_options()` or a named list.
#' @param execution Optional overrides from `execution_options()` or a named list.
cache_raw_parquet_duckdb <- function(src_name, tables = NULL, cohort = NULL,
                                     all_tables = TRUE,
                                     base_dir = NULL, raw_data_root = NULL,
                                     overwrite = FALSE, keep = FALSE,
                                     validate_columns = TRUE,
                                     memory_limit = NULL, threads = NULL,
                                     temp_directory = NULL,
                                     show_progress = TRUE,
                                     cohort_selection = NULL, loading = NULL, execution = NULL) {
  .ehr_apply_grouped_options(
    list(cohort_selection = cohort_selection, loading = loading, execution = execution),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(cache_raw_parquet_duckdb)), frame = environment())
  .duckdb_require()
  paths <- ehr_paths(src_name = src_name, base_dir = base_dir,
                     raw_data_root = raw_data_root)
  cfg   <- load_src_cfg(paths$data_config_json, paths$src_name)

  if (is.null(tables) && !is.null(cohort)) {
    tables <- .ehr_cohort_cache_tables(paths$src_name, cfg, cohort, paths)
    if (show_progress)
      message("Resolved tables for cohort '", cohort, "': ",
              paste(tables, collapse = ", "))
  } else if (is.null(tables) && isTRUE(all_tables)) {
    tables <- names(cfg$tables)
    if (show_progress)
      message("Caching all ", length(tables), " configured table(s) for '",
              paths$src_name, "'.")
  }
  if (is.null(tables)) {
    if (show_progress)
      message("all_tables = FALSE and no `tables`/`cohort` specified - ",
              "nothing to cache. Pass the table(s) your cohort_sql/setup_sql ",
              "actually query, e.g. tables = c(\"admissions\", \"icustays\"), ",
              "cohort = \"sepsis3\" for a bundled cohort, or leave ",
              "all_tables at its default (TRUE) to cache everything.")
    return(invisible(NULL))
  }
  tbls <- tables

  missing_cfg <- setdiff(tbls, names(cfg$tables))
  if (length(missing_cfg))
    stop("Tables not in config for '", src_name, "': ",
         paste(missing_cfg, collapse = ", "), call. = FALSE)

  file_map <- lapply(tbls, function(tbl)
    file.path(paths$raw_dir, cfg$tables[[tbl]]$files))
  names(file_map) <- tbls
  present <- vapply(file_map, function(fs) all(file.exists(fs)), logical(1))
  if (any(!present))
    stop("Raw CSV missing for requested table(s): ",
         paste(tbls[!present], collapse = ", "),
         "\nRun download_source(\"", src_name, "\") first.", call. = FALSE)
  file_map <- file_map[present]

  cache_dir <- paths$raw_parquet_cache_dir
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (!is.null(memory_limit))
    DBI::dbExecute(con, sprintf("SET memory_limit='%s';", memory_limit))
  if (!is.null(threads))
    DBI::dbExecute(con, sprintf("SET threads=%d;", as.integer(threads)))
  else
    .duckdb_configure_raw_csv_connection(con, .duckdb_is_demo_source(paths$src_name))
  # A `:memory:` connection has nowhere to spill by default, so a table
  # larger than available RAM (e.g. full-scale MIMIC chartevents/labevents)
  # runs the process out of memory outright instead of DuckDB's normal
  # out-of-core behavior kicking in. Always give it a spill directory -
  # caller-supplied if provided (persists, e.g. a known-large source pointed
  # at faster/roomier disk), otherwise a scratch dir removed when this
  # function returns either way.
  spill_dir <- temp_directory %||% tempfile("ehr_cache_spill_")
  if (is.null(temp_directory))
    on.exit(unlink(spill_dir, recursive = TRUE), add = TRUE)
  dir.create(spill_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(con, sprintf("SET temp_directory=%s;",
                              DBI::dbQuoteString(con, spill_dir)))
  # Only distinct/ordered-irrelevant intermediate results are ever read back
  # from this connection (each table is COPY'd straight to its own parquet
  # file), so this is a free memory reduction on large scans - same
  # reasoning as .ehr_select_cohort_ids_duckdb().
  DBI::dbExecute(con, "SET preserve_insertion_order=false;")

  cfg_schema <- .duckdb_schema_for_paths(paths, cfg)
  reports <- list()
  for (tbl in names(file_map)) {
    out_file <- file.path(cache_dir, paste0(tbl, ".parquet"))
    cache_exists <- !overwrite && parquet_has_rows(out_file)
    cache_fresh  <- cache_exists &&
      .ehr_cache_entry_fresh(cache_dir, tbl, file_map[[tbl]])

    if (cache_fresh) {
      if (show_progress) message("  Parquet cache exists and matches its source file(s), skipping: ", tbl)
      next
    }
    if (cache_exists && show_progress) {
      message("  Parquet cache exists but no longer matches its source ",
              "file(s) (size/mtime changed, or no manifest entry) - rebuilding: ", tbl)
    }

    if (show_progress) message("Caching to parquet: ", tbl)
    start_time <- Sys.time()

    headers <- lapply(file_map[[tbl]], .csv_header_cols)
    available_cols <- unique(unlist(headers, use.names = FALSE))
    tbl_schema <- .duckdb_table_schema(cfg_schema, tbl)
    read_expr  <- .duckdb_csv_read_expr(con, file_map[[tbl]], headers = headers)

    # Validate BEFORE writing: a value that fails TRY_CAST becomes a silent
    # NULL in the cached parquet with no record of what was dropped or why -
    # once cached, the original raw text is gone and the distinction between
    # "genuinely blank" and "failed to parse" can never be recovered. This
    # reuses the same reporting the main cohort engine uses under its own
    # validate_columns = TRUE, so cache-building gets the one moment the raw
    # text is available to actually check it. Costs a second full scan of
    # this table's raw CSV (one to validate, one to convert-and-write) -
    # roughly doubles this table's cache-build time - which is the deliberate
    # trade for not silently losing data-quality visibility once caching
    # becomes the default read path.
    if (isTRUE(validate_columns)) {
      raw_view <- paste0(".__ehr_cache_raw_", tbl)
      DBI::dbExecute(con, sprintf(
        "CREATE OR REPLACE VIEW %s AS SELECT * FROM %s AS src",
        DBI::dbQuoteIdentifier(con, raw_view), read_expr
      ))
      if (show_progress) message("  Validating column types: ", tbl)
      reports[[tbl]] <- .ehr_projection_report(
        con, tbl, cfg$tables[[tbl]], headers, raw_view, show_progress = FALSE
      )
    }

    select_sql <- .duckdb_typed_select_sql(con, tbl_schema,
                                           available_cols = available_cols,
                                           alias = "src")
    if (file.exists(out_file) && !file.remove(out_file))
      stop("Could not replace existing parquet cache file: ", out_file, call. = FALSE)
    DBI::dbExecute(con, sprintf(
      "COPY (SELECT %s FROM %s AS src) TO %s (FORMAT PARQUET);",
      select_sql, read_expr, DBI::dbQuoteString(con, out_file)
    ))
    .ehr_write_cache_manifest_entry(cache_dir, tbl, file_map[[tbl]])

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (show_progress) message("  Done: ", tbl, "  (", round(elapsed, 1), "s)")
  }

  if (isTRUE(validate_columns) && length(reports)) {
    report <- do.call(rbind, reports)
    failures <- report[!is.na(report$column) & report$conversion_failures > 0,
                       , drop = FALSE]
    report_path <- file.path(cache_dir, "_conversion_report.csv")
    readr::write_csv(report, report_path)
    if (nrow(failures)) {
      warning(
        sum(failures$conversion_failures), " value(s) across ", nrow(failures),
        " column(s) in ", length(unique(failures$table)), " table(s) failed ",
        "to convert to their configured type while building the parquet ",
        "cache; they are stored as NULL with no way to distinguish them from ",
        "genuinely blank values. Per-column failure counts (not the values ",
        "themselves) are in ", report_path, ".", call. = FALSE
      )
    } else if (show_progress) {
      message("Column-type validation: no conversion failures across ",
              length(reports), " cached table(s). Report: ", report_path)
    }
  }

  if (!isTRUE(keep)) {
    reg.finalizer(globalenv(), function(e) unlink(cache_dir, recursive = TRUE),
                  onexit = TRUE)
    if (show_progress)
      message("Parquet cache ready for ", src_name, ": ", cache_dir,
              "\n  (removed automatically when this R session ends - pass keep = TRUE to persist it)")
  } else if (show_progress) {
    message("Parquet cache ready for ", src_name, ": ", cache_dir)
  }
  invisible(cache_dir)
}

# ---------------------------------------------------------------------------
# End-to-end: select cohort + build a filtered cohort DuckDB database
# ---------------------------------------------------------------------------

# Read the column names from a (possibly .gz) CSV header line.
.csv_header_cols <- function(file) {
  f <- if (grepl("\\.gz$", file, ignore.case = TRUE)) gzfile(file, "rt")
       else file(file, "rt")
  on.exit(close(f))
  line <- tryCatch(readLines(f, n = 1L, warn = FALSE), error = function(e) character(0))
  if (!length(line)) return(character(0))
  line <- sub("^\uFEFF", "", line)                      # strip BOM
  cols <- strsplit(line, ",", fixed = TRUE)[[1]]
  trimws(gsub('^"|"$', "", cols))
}

# Build row-preserving DuckDB CSV relations with an explicit VARCHAR schema for
# every file. Per-file readers are required because files for one logical table
# may have different headers; UNION ALL BY NAME retains that accepted drift
# without making DuckDB auto-detect or reinterpret column values.
.duckdb_csv_read_expr <- function(con, files, headers = NULL,
                                  ignore_errors = TRUE, store_rejects = TRUE) {
  files <- as.character(files)
  if (!length(files) || anyNA(files) || any(!nzchar(files))) {
    stop("CSV input must contain at least one non-empty file path.",
         call. = FALSE)
  }
  if (is.null(headers)) {
    headers <- lapply(files, .csv_header_cols)
  } else if (length(files) == 1L && is.character(headers)) {
    headers <- list(headers)
  }
  if (!is.list(headers) || length(headers) != length(files)) {
    stop("CSV headers must provide one character vector per file.",
         call. = FALSE)
  }

  headers <- lapply(headers, as.character)
  invalid <- vapply(headers, function(header) {
    !length(header) || anyNA(header) || any(!nzchar(trimws(header)))
  }, logical(1L))
  if (any(invalid)) {
    stop("Every CSV file must have a complete non-empty header.",
         call. = FALSE)
  }
  duplicate <- vapply(
    headers, function(header) anyDuplicated(tolower(header)) > 0L,
    logical(1L)
  )
  if (any(duplicate)) {
    stop("CSV headers contain case-insensitive duplicate column names.",
         call. = FALSE)
  }
  actual <- unique(unlist(headers, use.names = FALSE))
  if (anyDuplicated(tolower(actual))) {
    stop("CSV files contain conflicting case-insensitive column names.",
         call. = FALSE)
  }

  readers <- vapply(seq_along(files), function(i) {
    columns <- paste(
      sprintf(
        "%s: %s",
        DBI::dbQuoteString(con, headers[[i]]),
        DBI::dbQuoteString(con, "VARCHAR")
      ),
      collapse = ", "
    )
    # NOTEEVENTS/RADIOLOGY carry free text with quoted embedded newlines; DuckDB's
    # parallel CSV scanner splits files into byte-offset chunks before it knows
    # where a quoted multi-line field ends, so it can misalign chunk boundaries
    # mid-record on these files.
    is_multiline <- grepl("NOTEEVENTS|RADIOLOGY|notes", basename(files[[i]]), ignore.case = TRUE)
    # DuckDB rejects an explicit ignore_errors = false combined with
    # store_rejects = true ("STORE_REJECTS option is only supported when
    # IGNORE_ERRORS is not manually set to false") - so when ignore_errors is
    # off, omit both settings entirely rather than writing ignore_errors =
    # false; DuckDB's own defaults for both are false anyway, and there is
    # nothing to store when a bad row aborts the read immediately.
    reject_opts <- if (isTRUE(ignore_errors)) {
      sprintf(", ignore_errors = true, store_rejects = %s",
             if (isTRUE(store_rejects)) "true" else "false")
    } else {
      ""
    }
    # DuckDB's max_line_size defaults to 2,000,000 bytes and is checked before
    # ignore_errors/store_rejects get a chance to run - a single stray
    # unescaped quote in a free-text field (observed on real, full-size
    # MIMIC-III CHARTEVENTS.csv, ~32MB runaway line) makes the parser treat
    # everything up to the next quote as one giant quoted field, aborting the
    # whole read even with ignore_errors on. DuckDB reserves buffer space
    # scaled to the *configured* max_line_size per CSV reader it opens,
    # regardless of the file's actual size - so this must stay both narrowly
    # scoped (only files large enough that a multi-megabyte runaway line is
    # plausible at all; confirmed even modest ~17-19MB tables get the full
    # reservation once raised) and modestly sized (comfortably above the one
    # observed real runaway, not an arbitrary round number). Raising it
    # unconditionally on every table's view previously created a multi-GB
    # fixed memory floor before any query even ran.
    large_line_size <- file.exists(files[[i]]) && file.size(files[[i]]) > 50 * 1024^2
    max_line_size_opt <- if (large_line_size) ", max_line_size = 40000000" else ""
    sprintf(
      paste0(
        "read_csv(%s, columns = {%s}, header = true, quote = '\"', ",
        "escape = '\"', null_padding = true, strict_mode = false, ",
        "encoding = 'UTF-8', parallel = %s%s%s)"
      ),
      DBI::dbQuoteString(con, files[[i]]), columns,
      if (is_multiline) "false" else "true",
      reject_opts, max_line_size_opt
    )
  }, character(1L))

  if (length(readers) == 1L) return(readers[[1L]])
  paste0(
    "(",
    paste(sprintf("SELECT * FROM %s", readers),
          collapse = " UNION ALL BY NAME "),
    ")"
  )
}


# Demo sources ship tiny, deliberately dirty sample CSVs. DuckDB's compressed-CSV
# scanner can hit internal errors when worker threads rescan lazy views over
# those files in larger query plans, so raw-CSV cohort builds run single-threaded
# for demo sources only. Full sources keep DuckDB's normal parallelism (the whole
# point of this backend on large loads); the user can still override via `threads`.
.duckdb_is_demo_source <- function(src_name) grepl("demo", src_name, ignore.case = TRUE)

.duckdb_configure_raw_csv_connection <- function(con, single_thread = TRUE) {
  if (isTRUE(single_thread)) DBI::dbExecute(con, "PRAGMA threads=1")
  invisible(con)
}

.duckdb_export_table_parquet <- function(con, table_ref, out_file,
                                         overwrite = TRUE,
                                         show_progress = TRUE) {
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(out_file)) {
    if (!overwrite) {
      if (show_progress) message("  Parquet exists, skipping: ", basename(out_file))
      return(FALSE)
    }
    if (!file.remove(out_file))
      stop("Could not replace existing parquet file: ", out_file, call. = FALSE)
  }
  DBI::dbExecute(con, sprintf(
    "COPY %s TO %s (FORMAT PARQUET);",
    table_ref,
    DBI::dbQuoteString(con, out_file)
  ))
  if (show_progress) message("  Parquet written: ", out_file)
  TRUE
}

# Fallback map for external/new sources without package schema SQL. Built-in
# sources use .duckdb_schema_from_sql_dir() as the type authority, so DuckDB and
# Postgres follow the same corrected local schema contract.
.duckdb_type_map <- c(col_integer = "INTEGER", col_big_integer = "BIGINT",
                      col_double = "DOUBLE",
                      col_logical = "BOOLEAN", col_datetime = "TIMESTAMP",
                      col_character = "VARCHAR")

# Per-table `c(csv_column = "DUCKDB_TYPE", ...)` derived from cfg$tables$*$cols.
.duckdb_schema_from_cfg <- function(cfg) {
  out <- lapply(cfg$tables, function(t) {
    cols <- t$cols
    if (is.null(cols) || !length(cols)) return(NULL)
    nm <- vapply(cols, function(cd) cd$name %||% NA_character_, character(1))
    sp <- vapply(cols, function(cd) cd$spec %||% "col_character", character(1))
    ty <- unname(.duckdb_type_map[sp]); ty[is.na(ty)] <- "VARCHAR"
    keep <- !is.na(nm)
    if (!any(keep)) return(NULL)
    # names(cols) is the canonical column name every downstream SQL file
    # assumes (matches the local Postgres schema SQL's convention); cd$name
    # is only the raw source file's own column name (e.g. "SUBJECT_ID" for
    # PhysioNet's full MIMIC-III release, vs the canonical "subject_id") and
    # is used only to confirm the column is actually mapped. It must never
    # be used as the returned schema's names - a source whose raw headers
    # use different casing than the canonical convention would otherwise end
    # up with duplicate case-variant entries once merged with the local
    # SQL-derived schema (.merge_table_schema()'s override match is
    # case-sensitive), silently doubling every generated view's columns.
    stats::setNames(ty[keep], names(cols)[keep])
  })
  out[!vapply(out, is.null, logical(1))]
}

# Merge a user-supplied `table_schema` over the config-derived one, per table and
# per column (user wins). Either may be NULL.
.merge_table_schema <- function(base, override) {
  if (is.null(override) || !length(override)) return(base %||% list())
  base <- base %||% list()
  tbls <- union(names(base), names(override))
  stats::setNames(lapply(tbls, function(t) {
    b <- base[[t]]; o <- override[[t]]
    if (is.null(o)) return(b)
    if (is.null(b)) return(o)
    b[names(o)] <- o; b
  }), tbls)
}

# Source id ladder (coarse -> fine), e.g. c("subject_id","hadm_id","stay_id")
# for MIMIC or c("uniquepid","patienthealthsystemstayid","patientunitstayid")
# for eICU. Read from config so the cohort path stays fully source-agnostic.
.cohort_id_ladder <- function(src_name) {
  lad <- get_dataset_config(src_name)$select_id_cols
  if (is.null(lad) || !length(lad))
    stop("No select_id_cols configured for '", src_name,
         "'; cannot resolve cohort id grain.", call. = FALSE)
  lad
}

# Resolve `import_scope` to a single ladder column: how broadly the raw tables
# are imported around the patient-rooted spine. NULL/unset -> the patient root
# (ladder[1]) so the cohort DB holds full patient context. Accepts a ladder
# column name or an alias (patient/subject, hospital/hadm/admission,
# icu/stay/icustay), mapped by ladder position. Import scope may not be finer
# than the selection grain (keycol = cohort_index_level): a finer request is
# clamped up to it with a message.
.resolve_import_scope <- function(import_scope, keycol, ladder, show_progress = TRUE) {
  n       <- length(ladder)
  lvl_pos <- match(keycol, ladder)
  if (is.na(lvl_pos))
    stop("Selection key '", keycol, "' is not in the id ladder (",
         paste(ladder, collapse = ", "), ").", call. = FALSE)
  if (is.null(import_scope)) return(ladder[1])          # default: full patient context
  alias <- c(patient = 1L, subject = 1L,
             hospital = 2L, hadm = 2L, admission = 2L,
             icu = n, stay = n, icustay = n)
  pos <- if (import_scope %in% ladder) match(import_scope, ladder)
         else if (tolower(import_scope) %in% names(alias)) min(alias[[tolower(import_scope)]], n)
         else stop("Unknown import_scope '", import_scope,
                   "'. Use patient/hospital/icu or a ladder column (",
                   paste(ladder, collapse = ", "), ").", call. = FALSE)
  if (pos > lvl_pos) {
    if (show_progress)
      message("import_scope '", import_scope, "' (", ladder[pos],
              ") is finer than the selection grain '", keycol,
              "'; using '", keycol, "'.")
    pos <- lvl_pos
  }
  ladder[pos]
}

.duckdb_create_window_derivatives <- function(con, schema, src_name) {
  fam <- src_family(src_name)
  vref <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = "visit_windows"))
  aref <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = "admission_windows"))
  iref <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = "icu_windows"))

  # `visit_windows` is the source of truth for cohort_index and in_scope.
  # Admission-level windows are true when any nested visit row is true; ICU
  # windows carry the stay/unit-level flags directly.
  # Roll up to one row per hospitalization by grouping on the ids alone and taking
  # MIN(admittime)/MAX(dischtime). Grouping on the time columns too would split a
  # hadm into several admission_windows rows if a dirty source ever carried
  # inconsistent times within one hadm; grouping on ids only keeps it 1 row/hadm.
  if (identical(fam, "mimic_iv")) {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS
       SELECT subject_id, hadm_id,
              MIN(admittime) AS admittime, MAX(dischtime) AS dischtime,
              ROUND(EXTRACT(EPOCH FROM (MAX(dischtime) - MIN(admittime))) / 60.0) AS hadm_los_min,
              MAX(CASE WHEN cohort_index THEN 1 ELSE 0 END) > 0 AS cohort_index,
              MAX(CASE WHEN in_scope THEN 1 ELSE 0 END) > 0 AS in_scope
       FROM %s
       GROUP BY subject_id, hadm_id;", aref, vref))
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS
       SELECT subject_id, hadm_id, stay_id, admittime, dischtime,
              icu_intime, icu_outtime, icu_los_min, cohort_index, in_scope
       FROM %s WHERE stay_id IS NOT NULL;", iref, vref))
  } else if (identical(fam, "mimic_iii")) {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS
       SELECT subject_id, hadm_id,
              MIN(admittime) AS admittime, MAX(dischtime) AS dischtime,
              ROUND(EXTRACT(EPOCH FROM (MAX(dischtime) - MIN(admittime))) / 60.0) AS hadm_los_min,
              MAX(CASE WHEN cohort_index THEN 1 ELSE 0 END) > 0 AS cohort_index,
              MAX(CASE WHEN in_scope THEN 1 ELSE 0 END) > 0 AS in_scope
       FROM %s
       GROUP BY subject_id, hadm_id;", aref, vref))
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS
       SELECT subject_id, hadm_id, icustay_id, admittime, dischtime,
              icu_intime, icu_outtime, icu_los_min, cohort_index, in_scope
       FROM %s WHERE icustay_id IS NOT NULL;", iref, vref))
  } else if (identical(fam, "eicu")) {
    # eICU offsets are relative to each unit stay's own admit, so the two offset
    # columns are taken from one reference row (the earliest unit stay) rather
    # than MIN/MAX across unit stays, which would mix clocks.
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS
       SELECT uniquepid, patienthealthsystemstayid,
              (array_agg(hospitaladmitoffset ORDER BY hospitaladmitoffset DESC NULLS LAST,
                         patientunitstayid))[1] AS hospitaladmitoffset,
              (array_agg(hospitaldischargeoffset ORDER BY hospitaladmitoffset DESC NULLS LAST,
                         patientunitstayid))[1] AS hospitaldischargeoffset,
              0::DOUBLE AS admittime,
              MAX(hospitaldischargeoffset - hospitaladmitoffset) AS dischtime,
              MAX(hospitaldischargeoffset - hospitaladmitoffset) AS hadm_los_min,
              MAX(CASE WHEN cohort_index THEN 1 ELSE 0 END) > 0 AS cohort_index,
              MAX(CASE WHEN in_scope THEN 1 ELSE 0 END) > 0 AS in_scope
       FROM %s GROUP BY uniquepid, patienthealthsystemstayid;", aref, vref))
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS
       SELECT uniquepid, patientunitstayid, patienthealthsystemstayid,
              hospitaladmitoffset, hospitaldischargeoffset, unitdischargeoffset,
              hospital_start, hospital_end, admittime, dischtime,
              icu_intime, icu_outtime, icu_los_min, cohort_index, in_scope
       FROM %s WHERE patientunitstayid IS NOT NULL;", iref, vref))
  }
  invisible(TRUE)
}

# ---- Named example cohorts -------------------------------------------------
# Ready-made cohort definitions shipped with the package so a well-known cohort
# can be selected by name on any supported source. ICD cohorts resolve to a
# bundled diagnosis list matched by the cohort engine; SQL cohorts resolve to the
# family's cohorts/<name>/ SQL chain. Add an entry to extend.
.example_cohort_registry <- function() list(
  scid = list(
    kind = "icd", icd_file = "scid_icds_rendehr.rds",
    desc = "Spinal cord injury - bundled ICD-9/10 list (the package reference cohort)"),
  sepsis3 = list(
    kind = "sql", sql_subdir = "cohorts/sepsis3", result_table = "sepsis3",
    desc = "Sepsis-3 (Singer 2016): SOFA >= 2 + suspicion of infection, per ICU stay")
)

# Resolve a bundled resource path, working under both an installed package and
# a dev tree loaded with devtools::load_all().
.ehr_extdata <- function(...) {
  f <- system.file(..., package = "rendehR")
  if (nzchar(f)) f else file.path("inst", ...)
}

#' List the example cohorts bundled with the package
#'
#' Ready-made cohort definitions (ICD lists and SQL chains) that
#' [import_cohort_general()] and [materialize_cohort_duckdb()] can select by
#' name via their `cohort` argument.
#'
#' @return A data frame of `name`, `kind`, `grain`, and `description`.
#' @examples
#' list_example_cohorts()
#' @export
list_example_cohorts <- function() {
  reg <- .example_cohort_registry()
  data.frame(
    name        = names(reg),
    kind        = vapply(reg, `[[`, "", "kind"),
    grain       = vapply(reg, function(s)
                    if (identical(s$kind, "icd"))
                      "hospital admission"
                    else "ICU-stay id", ""),
    description = vapply(reg, `[[`, "", "desc"),
    row.names = NULL, stringsAsFactors = FALSE)
}

# Resolve a named example cohort to cohort-engine selection arguments for
# src_name: either list(icd_filter_path, key) or list(setup_sql, cohort_sql, key).
.resolve_example_cohort <- function(name, src_name) {
  reg <- .example_cohort_registry()
  if (!name %in% names(reg))
    stop("Unknown example cohort '", name, "'. Available: ",
         paste(names(reg), collapse = ", "), " (see list_example_cohorts()).",
         call. = FALSE)
  spec <- reg[[name]]
  if (identical(spec$kind, "icd")) {
    f <- .ehr_extdata("extdata", spec$icd_file)
    if (!file.exists(f))
      stop("Bundled ICD list for '", name, "' not found: ", f, call. = FALSE)
    list(icd_filter_path = f, key = cohort_key(src_name))
  } else {
    fam <- src_family(src_name)
    dir <- .ehr_extdata("sql", fam, spec$sql_subdir)
    if (!dir.exists(dir))
      stop("Example cohort '", name, "' is not defined for source family '",
           fam, "' (no ", spec$sql_subdir, " SQL).", call. = FALSE)
    setup <- sort(list.files(dir, "\\.sql$", full.names = TRUE),
                  method = "radix")
    key   <- utils::tail(get_dataset_config(src_name)$select_id_cols, 1)  # ICU-stay id
    list(setup_sql  = setup,
         cohort_sql = sprintf("SELECT DISTINCT %s FROM %s WHERE %s",
                              key, spec$result_table, spec$result_table),
         key        = key)
  }
}

# Expand a named example `cohort` into explicit selection args, guarding against
# mixing it with an explicit selection input. Returns the resolved
# icd_filter_path / cohort_sql / setup_sql / key (unchanged if cohort is NULL).
.apply_example_cohort <- function(cohort, src_name, icd_filter_path, cohort_sql,
                                  setup_sql, cohort_fn, direct_ids_path, key,
                                  no_filter = FALSE) {
  if (is.null(cohort))
    return(list(icd_filter_path = icd_filter_path, cohort_sql = cohort_sql,
                setup_sql = setup_sql, key = key))
  if (!is.null(icd_filter_path) || !is.null(cohort_sql) || !is.null(cohort_fn) ||
      !is.null(direct_ids_path) || isTRUE(no_filter))
    stop("Pass `cohort` (a named example) or an explicit selection input, not ",
         "both. See list_example_cohorts().", call. = FALSE)
  spec <- .resolve_example_cohort(cohort, src_name)
  list(icd_filter_path = spec$icd_filter_path, cohort_sql = spec$cohort_sql,
       setup_sql = spec$setup_sql, key = key %||% spec$key)
}

#' Select and normalize cohort identifiers
#'
#' Resolves exactly one direct-ID, SQL, R-function, ICD, named-cohort, or
#' no-filter request through the canonical DuckDB selection engine. Returned
#' identifiers are character-normalized and validated against the configured
#' source hierarchy. Diagnostics report counts only and never print values.
#'
#' @param src_name Registered source name.
#' @param icd_filter_path ICD-code CSV or RDS file with an `icd_code` column
#'   and an `icd_version` column (9 or 10). `icd_version` is required for
#'   MIMIC-IV and eICU; for MIMIC-III it may be omitted (ICD-9 is assumed) and
#'   ICD-10 rows are excluded. A code matches only in its stated version.
#' @param cohort_sql Final selection query; use `setup_sql` for dependencies.
#' @param setup_sql Optional ordered SQL statements or files.
#' @param cohort_fn Function or function name returning identifiers.
#' @param direct_ids_path CSV, CSV.GZ, Parquet, or RDS identifier file.
#' @param cohort Bundled named cohort.
#' @param no_filter Select the whole available source.
#' @param key Explicit configured hierarchy key.
#' @param import_scope Patient, hospital, ICU, or configured hierarchy key.
#' @param allow_unmatched_ids Permit a partially unmatched selection when at
#'   least one identifier matches.
#' @param out_csv Optional atomic one-column identifier export.
#' @param base_dir Project root.
#' @param raw_data_root Optional parent of the source-specific raw directory.
#' @param window_sql,window_fn Optional hierarchy builder override.
#' @return An object of class `ehr_selection_plan`. Printing it shows the
#'   route, keys, scope and counts; the identifiers themselves are in
#'   `$values`.
#' @examples
#' \dontrun{
#' plan <- select_cohort_ids("mimic_iv_demo", cohort = "scid")
#' plan               # route, keys, scope and counts
#' head(plan$values)  # the selected identifiers
#' }
#' @export
select_cohort_ids <- function(src_name,
                              icd_filter_path = NULL,
                              cohort_sql = NULL,
                              setup_sql = NULL,
                              cohort_fn = NULL,
                              direct_ids_path = NULL,
                              cohort = NULL,
                              no_filter = FALSE,
                              key = NULL,
                              import_scope = NULL,
                              allow_unmatched_ids = FALSE,
                              out_csv = NULL,
                              base_dir = NULL,
                              raw_data_root = NULL,
                              window_sql = NULL,
                              window_fn = NULL) {
  .ehr_select_cohort_plan(
    src_name = src_name,
    base_dir = base_dir,
    raw_data_root = raw_data_root,
    icd_filter_path = icd_filter_path,
    cohort_sql = cohort_sql,
    setup_sql = setup_sql,
    cohort_fn = cohort_fn,
    direct_ids_path = direct_ids_path,
    cohort = cohort,
    no_filter = no_filter,
    key = key,
    import_scope = import_scope,
    allow_unmatched_ids = allow_unmatched_ids,
    window_sql = window_sql,
    window_fn = window_fn,
    out_csv = out_csv
  )
}

#' Print a cohort selection plan
#'
#' Shows how a [select_cohort_ids()] selection resolved: route, selection and
#' evidence keys, requested and effective import scope, and the requested,
#' matched and unmatched counts. Identifier values are never printed; they are
#' in `x$values`.
#'
#' @param x An `ehr_selection_plan`.
#' @param ... Ignored.
#' @return Invisibly, `x`.
#' @export
print.ehr_selection_plan <- function(x, ...) {
  fmt <- function(v) if (is.null(v) || all(is.na(v))) "-" else paste(v, collapse = ", ")
  cat("rendehR cohort selection: ", x$source, "\n", sep = "")
  cat("Route    : ", fmt(x$route), "\n", sep = "")
  cat("Keys     : selection ", fmt(x$selection_key), ", evidence ", fmt(x$evidence_key), "\n", sep = "")
  cat("Scope    : requested ", fmt(x$requested_scope), ", effective ", fmt(x$effective_scope), "\n", sep = "")
  cat("Counts   : ", fmt(x$requested_count), " requested, ", fmt(x$matched_count), " matched, ",
      fmt(x$unmatched_count), " unmatched\n", sep = "")
  cat("Identifiers are in $values (not printed).\n")
  invisible(x)
}

#' Materialize the current cohort as a persistent DuckDB database
#'
#' Uses the canonical selection, hierarchy, three-index, table-linkage, and
#' projection contracts. The complete database is built and validated in
#' same-filesystem staging before it atomically replaces the one current
#' database.
#'
#' A separate, opt-in path from \code{run_one_source()} (the canonical
#' pipeline entry point): it skips Parquet and PostgreSQL entirely for local,
#' Postgres-free access to the raw cohort tables. Concept extraction and
#' harmonization are PostgreSQL-only and do not run against this database -
#' use \code{import_cohort_general()} + \code{run_db()} instead if you need
#' those.
#'
#' When the cohort is selected by ICD code (a named ICD cohort such as
#' `"scid"`, or a raw `icd_filter_path`), the exact code list that
#' defined the cohort is also published as a reference table, `d_<name>_icds`
#' (e.g. `d_scid_icds` for the bundled SCI cohort; a name derived from the
#' file's basename for a raw `icd_filter_path`). SQL-based cohorts (e.g.
#' `"sepsis3"`) have no ICD list and get no such table.
#'
#' @inheritParams select_cohort_ids
#' @param db_path Destination DuckDB file. Defaults to `ehr_paths()` output.
#' @param schema Destination schema inside DuckDB.
#' @param tables Optional bounded table subset.
#' @param memory_limit,threads,temp_directory Optional DuckDB resource limits.
#' @param table_chunk_rows Row threshold for chunked table builds; see
#'   `import_cohort_general()`.
#' @param validate_columns If TRUE, validate raw column types before building;
#'   see `import_cohort_general()`. Off by default.
#' @param ignore_errors,store_rejects Raw CSV error tolerance and reject
#'   visibility; see `import_cohort_general()`.
#' @param use_parquet_cache If TRUE (default), a table whose raw CSV has
#'   already been cached as parquet by `cache_raw_parquet_duckdb()` is read
#'   from that parquet instead of re-parsing CSV. Tables without a cached
#'   parquet still fall back to CSV. Set FALSE to always read CSV.
#' @param overwrite Safely replace the current completed artifact?
#' @param show_progress Print aggregate progress.
#' @return Invisibly returns the normalized plan, reports, status, path, and
#'   `csv_reject_report`.
#' @examples
#' \dontrun{
#' materialize_cohort_duckdb("mimic_iv_demo", db_path = "data/duckdb/mimic_iv_demo.duckdb",
#'                           cohort = "scid")
#' }
#' @export
materialize_cohort_duckdb <- function(src_name = NULL,
                                      db_path = NULL,
                                      icd_filter_path = NULL,
                                      cohort_sql = NULL,
                                      setup_sql = NULL,
                                      cohort_fn = NULL,
                                      direct_ids_path = NULL,
                                      cohort = NULL,
                                      no_filter = FALSE,
                                      key = NULL,
                                      import_scope = NULL,
                                      allow_unmatched_ids = FALSE,
                                      window_sql = NULL,
                                      window_fn = NULL,
                                      schema = NULL,
                                      tables = NULL,
                                      memory_limit = NULL,
                                      threads = NULL,
                                      temp_directory = NULL,
                                      table_chunk_rows = 5e6,
                                      validate_columns = FALSE,
                                      ignore_errors = TRUE,
                                      store_rejects = TRUE,
                                      use_parquet_cache = TRUE,
                                      base_dir = NULL,
                                      raw_data_root = NULL,
                                      overwrite = FALSE,
                                      show_progress = TRUE) {
  .ehr_run_cohort_engine(
    output = "duckdb",
    output_path = db_path,
    src_name = src_name,
    base_dir = base_dir,
    raw_data_root = raw_data_root,
    icd_filter_path = icd_filter_path,
    direct_ids_path = direct_ids_path,
    cohort = cohort,
    cohort_sql = cohort_sql,
    setup_sql = setup_sql,
    cohort_fn = cohort_fn,
    no_filter = no_filter,
    key = key,
    import_scope = import_scope,
    allow_unmatched_ids = allow_unmatched_ids,
    window_sql = window_sql,
    window_fn = window_fn,
    schema = schema,
    tables = tables,
    memory_limit = memory_limit,
    threads = threads,
    temp_directory = temp_directory,
    table_chunk_rows = table_chunk_rows,
    validate_columns = validate_columns,
    ignore_errors = ignore_errors,
    store_rejects = store_rejects,
    use_parquet_cache = use_parquet_cache,
    overwrite = overwrite,
    show_progress = show_progress
  )
}

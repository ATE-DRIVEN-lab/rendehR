# Load cohort parquet files into Postgres.

# Internal helpers -----------------------------------------------------------

# TRUE if `schema.table` exists.
.pg_table_exists <- function(con, schema, table) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  table <- .ehr_validate_db_identifier(table, "table name")
  nrow(DBI::dbGetQuery(con, paste0(
    "SELECT 1 FROM information_schema.tables WHERE table_schema = ",
    DBI::dbQuoteString(con, schema), " AND table_name = ",
    DBI::dbQuoteString(con, table)
  ))) > 0
}

.pg_schema_exists <- function(con, schema) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  nrow(DBI::dbGetQuery(con, paste0(
    "SELECT 1 FROM information_schema.schemata WHERE schema_name = ",
    DBI::dbQuoteString(con, schema)
  ))) > 0
}

# Exact row count of `schema.table` (used to detect a fully vs partially loaded
# table when resuming with overwrite = FALSE).
.pg_row_count <- function(con, schema, table) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  table <- .ehr_validate_db_identifier(table, "table name")
  id <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table))
  as.numeric(DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", id))$n)
}

# Row count of a parquet file from its metadata (cheap - no data scan).
.parquet_num_rows <- function(pf) {
  ds <- arrow::open_dataset(pf, format = "parquet")
  n <- tryCatch(ds$num_rows, error = function(e) NA_real_)
  if (is.null(n) || is.na(n)) n <- nrow(ds)
  as.numeric(n)
}

# ___________________________________________________________________________
# Load-completion manifest - avoids a full count(*) resume-check
# ___________________________________________________________________________
#
# load_cohort_parquet_to_postgres()'s resume mode (overwrite = FALSE) must
# decide whether an existing table can be skipped. Checking with
# .pg_row_count() - a real `count(*)` scan - can take minutes on a huge
# already-loaded table (e.g. chartevents), on every call, even when nothing
# changed since the last successful load.
#
# Instead, once a load actually runs and its row count is verified (the exact
# count Postgres load already computes for free right after loading), it is
# recorded in a tiny marker table. A later resume-check trusts that marker -
# an O(1) indexed lookup - instead of re-scanning the table, but only when it
# still matches both the parquet's own row count (cheap, from Parquet
# metadata) and the cohort build that produced it (`source_completed_at`,
# from cohort_manifest.json). A rebuilt cohort gets a new `completed_at`, which
# automatically invalidates any marker left over from the previous build.

.EHR_LOAD_MANIFEST_TABLE <- "__ehr_load_manifest"

# `completed_at` from a cohort's manifest.json, or NA if the directory has none
# (e.g. a hand-built parquet dir not produced by the cohort engine) - callers
# treat NA as "no fast-path available", not an error, and fall back to the
# exact count(*) check unchanged.
.ehr_cohort_manifest_completed_at <- function(parquet_dir) {
  path <- file.path(parquet_dir, "cohort_manifest.json")
  if (!file.exists(path)) return(NA_character_)
  manifest <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(manifest) || is.null(manifest$completed_at)) return(NA_character_)
  as.character(manifest$completed_at)
}

# Ensures the small load-completion marker table exists in `schema`. Idempotent.
.ehr_ensure_load_manifest_table <- function(con, schema) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  DBI::dbExecute(con, sprintf(
    paste0(
      "CREATE TABLE IF NOT EXISTS %s (table_name TEXT PRIMARY KEY, ",
      "rows BIGINT, source_completed_at TEXT, loaded_at TIMESTAMP)"
    ),
    DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = .EHR_LOAD_MANIFEST_TABLE))
  ))
  invisible(NULL)
}

# The recorded marker for `table_name`, or NULL if none exists yet (never
# loaded via this mechanism, or the manifest table itself doesn't exist yet).
.ehr_load_manifest_lookup <- function(con, schema, table_name) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  table_name <- .ehr_validate_db_identifier(table_name, "table name")
  if (!.pg_table_exists(con, schema, .EHR_LOAD_MANIFEST_TABLE)) return(NULL)
  row <- DBI::dbGetQuery(con, sprintf(
    "SELECT rows, source_completed_at FROM %s WHERE table_name = %s",
    DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = .EHR_LOAD_MANIFEST_TABLE)),
    DBI::dbQuoteString(con, table_name)
  ))
  if (!nrow(row)) return(NULL)
  list(rows = as.numeric(row$rows[[1L]]),
       source_completed_at = as.character(row$source_completed_at[[1L]]))
}

# Records (or replaces) `table_name`'s completion marker. Only ever called
# right after a load actually ran and its row count was verified - never
# speculatively - so the marker always reflects a real, checked outcome.
.ehr_load_manifest_mark <- function(con, schema, table_name, rows,
                                    source_completed_at) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  table_name <- .ehr_validate_db_identifier(table_name, "table name")
  .ehr_ensure_load_manifest_table(con, schema)
  tbl <- DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = .EHR_LOAD_MANIFEST_TABLE)
  )
  DBI::dbExecute(con, sprintf(
    paste0(
      "INSERT INTO %s (table_name, rows, source_completed_at, loaded_at) ",
      "VALUES (%s, %s, %s, now()) ",
      "ON CONFLICT (table_name) DO UPDATE SET rows = EXCLUDED.rows, ",
      "source_completed_at = EXCLUDED.source_completed_at, ",
      "loaded_at = EXCLUDED.loaded_at"
    ),
    tbl, DBI::dbQuoteString(con, table_name),
    sprintf("%.0f", as.numeric(rows)),
    DBI::dbQuoteString(con, as.character(source_completed_at))
  ))
  invisible(NULL)
}

.ehr_index_specs <- function(path) {
  lines <- readLines(path, warn = FALSE)
  create_lines <- grep("^\\s*CREATE\\s+INDEX\\b", lines,
                       value = TRUE, ignore.case = TRUE)
  pattern <- paste0(
    "^\\s*CREATE\\s+INDEX\\s+IF\\s+NOT\\s+EXISTS\\s+",
    "([A-Za-z_][A-Za-z0-9_]*)\\s+ON\\s+",
    "([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^)]+)\\)\\s*;\\s*$"
  )
  matches <- regexec(pattern, create_lines, ignore.case = TRUE, perl = TRUE)
  parts <- regmatches(create_lines, matches)
  if (any(lengths(parts) != 4L))
    stop("Unsupported CREATE INDEX statement in: ", path, call. = FALSE)
  if (!length(parts)) {
    return(data.frame(index = character(), table = character(),
                      columns = I(list()), stringsAsFactors = FALSE))
  }
  data.frame(
    index = vapply(parts, `[[`, character(1L), 2L),
    table = vapply(parts, `[[`, character(1L), 3L),
    columns = I(lapply(parts, function(x) trimws(strsplit(x[[4L]], ",",
                                                          fixed = TRUE)[[1L]]))),
    stringsAsFactors = FALSE
  )
}

.ehr_validate_index_file <- function(path, table_schema) {
  specs <- .ehr_index_specs(path)
  for (i in seq_len(nrow(specs))) {
    invisible(lapply(c(specs$index[[i]], specs$table[[i]],
                       specs$columns[[i]]), .ehr_validate_db_identifier))
    if (!specs$table[[i]] %in% names(table_schema))
      stop("Index references unknown table: ", specs$table[[i]], call. = FALSE)
    missing <- setdiff(specs$columns[[i]], names(table_schema[[specs$table[[i]]]]))
    if (length(missing))
      stop("Index references unknown column(s) in ", specs$table[[i]], ": ",
           paste(missing, collapse = ", "), call. = FALSE)
  }
  specs
}

.ehr_run_postload_indexes <- function(con, path, schema, table_schema) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  specs <- .ehr_validate_index_file(path, table_schema)
  DBI::dbExecute(con, paste0(
    "SET search_path TO ", DBI::dbQuoteIdentifier(con, schema)
  ))
  run_sql_file(con, path)
  invisible(specs)
}

# This is the check that gates run_db()'s "the live raw schema is already
# complete, skip the whole rebuild" decision (pipeline.R, on every
# overwrite = FALSE call - the default) as well as the post-load check on a
# freshly built staging schema. Both call sites resolve every table's row
# count through the load-completion marker first (see
# .ehr_load_manifest_lookup()/.ehr_load_manifest_mark() above) instead of
# unconditionally re-scanning with count(*), which would make a repeat run_db()
# call take minutes against an already-loaded huge table like chartevents. A table with no trustworthy marker yet still gets the exact
# count(*) it always got, and that result is then written as the marker so
# the *next* call benefits.
.pg_validate_loaded_artifact <- function(con, schema, parquet_dir) {
  manifest <- jsonlite::fromJSON(
    file.path(parquet_dir, "cohort_manifest.json"),
    simplifyVector = FALSE
  )
  tables <- unname(unlist(manifest$tables))
  expected_rows <- unlist(manifest$row_counts)
  completed_at <- manifest$completed_at
  completed_at <- if (is.null(completed_at)) NA_character_ else as.character(completed_at)
  missing <- tables[!vapply(
    tables, function(x) .pg_table_exists(con, schema, x), logical(1L)
  )]
  if (length(missing))
    stop("Staged PostgreSQL artifact is missing table: ", missing[[1L]],
         call. = FALSE)
  expected_rows <- as.numeric(expected_rows[tables])

  actual_rows <- vapply(seq_along(tables), function(i) {
    table    <- tables[[i]]
    expected <- expected_rows[[i]]
    marker   <- .ehr_load_manifest_lookup(con, schema, table)
    if (!is.null(marker) && !is.na(completed_at) && !is.na(expected) &&
        isTRUE(marker$source_completed_at == completed_at) &&
        isTRUE(marker$rows == expected)) {
      return(marker$rows)
    }
    rows <- .pg_row_count(con, schema, table)
    # Backfill the marker with what was just found, whether or not it
    # matches `expected` - a mismatch still fails below via the usual check,
    # so a wrong count can never hide behind a false marker.
    if (!is.na(completed_at))
      .ehr_load_manifest_mark(con, schema, table, rows, completed_at)
    rows
  }, numeric(1L))

  if (anyNA(expected_rows) ||
      !identical(unname(actual_rows), expected_rows)) {
    bad <- which(is.na(expected_rows) | actual_rows != expected_rows)[[1L]]
    stop("Staged PostgreSQL row-count mismatch for table: ", tables[[bad]],
         call. = FALSE)
  }
  data.frame(table = tables, rows = actual_rows, stringsAsFactors = FALSE)
}

# Text for a plain double column bound for a NUMERIC column: the shortest of
# 15 or 17 significant digits that reads back as the same double, so the
# text depends only on the value. Classed doubles (dates, times, integer64)
# and other types are returned unchanged.
.ehr_pg_numeric_text <- function(x) {
  if (!is.double(x) || !is.null(attr(x, "class"))) return(x)
  out <- sprintf("%.15g", x)
  fin <- is.finite(x)              # NA/NaN/Inf print as text that does not parse back
  inexact <- fin
  inexact[fin] <- as.numeric(out[fin]) != x[fin]
  out[inexact] <- sprintf("%.17g", x[inexact])
  out[is.nan(x)] <- "NaN"
  out[!is.nan(x) & is.infinite(x)] <- ifelse(x[!is.nan(x) & is.infinite(x)] > 0,
                                             "Infinity", "-Infinity")
  out[is.na(x) & !is.nan(x)] <- NA_character_
  out
}

#' Load all parquet files from a cohort directory into Postgres
#'
#' Each .parquet file in parquet_dir is loaded as one table (table name =
#' filename without extension).
#'
#' Tables are expected to already exist with explicit column types, created by
#' the schema DDL that `run_db()` runs before this loader (`db_setup_sqls/schema`).
#' When a table exists, rows are appended into it with `DBI::dbAppendTable()` so
#' the DDL column types are preserved - the table is never dropped, and types are
#' not re-inferred from the parquet. If a table has no pre-created DDL, the loader
#' falls back to creating it from the parquet (types inferred from the data).
#'
#' Each table is loaded inside a single transaction: a failure part-way through a
#' table rolls that table back, so a table is never left partially populated.
#'
#' In resume mode (`overwrite = FALSE`), a table already confirmed loaded by a
#' prior call is recognized from a small `__ehr_load_manifest` marker table
#' (written right after that prior load verified its own row count) instead of
#' re-scanning the table with `count(*)` every call - a real cost saver on a
#' huge already-loaded table (e.g. chartevents). The marker is only trusted
#' when it still matches both the parquet's row count and the cohort build
#' that produced it (via `cohort_manifest.json`'s `completed_at`); a rebuilt
#' cohort, or a `parquet_dir` with no manifest, transparently falls back to the
#' exact `count(*)` check.
#'
#' @param con DBI connection.
#' @param parquet_dir Path to directory of cohort parquet files.
#' @param schema Target Postgres schema.
#' @param overwrite If TRUE (default), reload every table: an existing table is
#'   truncated (keeping its DDL types) and reloaded. If FALSE, resume: a table
#'   whose row count already matches its parquet is skipped; a partially loaded
#'   table is truncated and reloaded.
#' @param show_progress Print per-table progress messages.
#' @param delete_source_parquet Delete each parquet file after it is
#'   successfully loaded into Postgres. Default FALSE. Saves disk space but
#'   prevents re-loading without regenerating the parquet.
#' @param unlogged If TRUE, set each pre-created (DDL) table to UNLOGGED before
#'   loading, which can be much faster for a bulk load. Default FALSE.
#'   WARNING: UNLOGGED tables are **emptied** by Postgres on any unclean
#'   shutdown/crash recovery and are not replicated. Only safe because these raw
#'   tables are reproducible from the cohort parquet. Set `set_logged = TRUE` to
#'   convert them back to durable LOGGED tables after the load.
#' @param set_logged If TRUE (and `unlogged = TRUE`), convert each table back to
#'   LOGGED after it loads successfully. This makes the data durable but rewrites
#'   the table, giving back some of the time saved. Default FALSE.
#' @param synchronous_commit_off If TRUE, set `synchronous_commit = off` for the
#'   load session (restored afterwards). Speeds bulk loading by not waiting for
#'   WAL fsync on each commit; on a crash the most recent commits can be lost but
#'   the database is not corrupted. Default FALSE.
#' @return Invisibly, a data frame with each table's row count and whether it
#'   was loaded or skipped as already complete.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' load_cohort_parquet_to_postgres(con, "data/cohorts_data/mimic_iv_demo_parquet", "mimic_iv_demo_coh")
#' }
#' @export
load_cohort_parquet_to_postgres <- function(con, parquet_dir, schema,
                                            overwrite = TRUE,
                                            show_progress = TRUE,
                                            delete_source_parquet = FALSE,
                                            unlogged = FALSE,
                                            set_logged = FALSE,
                                            synchronous_commit_off = FALSE) {
  if (!dir.exists(parquet_dir))
    stop("parquet_dir does not exist: ", parquet_dir)
  if (!DBI::dbIsValid(con))
    stop("Invalid database connection.")
  if (isTRUE(set_logged) && !isTRUE(unlogged))
    warning("set_logged = TRUE has no effect unless unlogged = TRUE.", call. = FALSE)

  parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$",
                              full.names = TRUE, recursive = FALSE)
  if (length(parquet_files) == 0)
    stop("No parquet files found in: ", parquet_dir)

  # Identity of the cohort build this parquet_dir came from, used to decide
  # whether a previously recorded load-completion marker (see below) is still
  # trustworthy. NA when there's no cohort_manifest.json (e.g. a hand-built
  # parquet_dir) - the resume fast path simply never applies in that case.
  manifest_completed_at <- .ehr_cohort_manifest_completed_at(parquet_dir)

  # Session-level synchronous_commit override, restored on exit.
  if (isTRUE(synchronous_commit_off)) {
    prev_sync <- DBI::dbGetQuery(con, "SHOW synchronous_commit")[[1]]
    DBI::dbExecute(con, "SET synchronous_commit = off")
    on.exit(
      try(DBI::dbExecute(con, sprintf("SET synchronous_commit = %s", prev_sync)),
          silent = TRUE),
      add = TRUE
    )
    if (show_progress) message("synchronous_commit = off for this load session.")
  }

  report <- vector("list", length(parquet_files))
  for (file_i in seq_along(parquet_files)) {
    pf <- parquet_files[[file_i]]
    tbl_name   <- tools::file_path_sans_ext(basename(pf))
    start_time <- Sys.time()
    tbl_id     <- DBI::Id(schema = schema, table = tbl_name)

    exists       <- .pg_table_exists(con, schema, tbl_name)
    parquet_rows <- .parquet_num_rows(pf)

    # -- Decide what to do with an already-existing table --------------------
    need_truncate <- FALSE
    if (exists) {
      if (!overwrite) {
        # Fast path: trust a previously recorded load-completion marker
        # instead of re-scanning a potentially huge table with count(*) every
        # call (that scan can take minutes on a table like
        # chartevents). Only trusted when it still matches both this
        # parquet's row count and the cohort build that produced it -
        # anything else (never loaded via this mechanism, or the cohort was
        # rebuilt since) falls through to the exact check below unchanged.
        marker <- .ehr_load_manifest_lookup(con, schema, tbl_name)
        marker_ok <- !is.null(marker) && !is.na(parquet_rows) &&
          !is.na(manifest_completed_at) &&
          isTRUE(marker$rows == parquet_rows) &&
          isTRUE(marker$source_completed_at == manifest_completed_at)
        if (marker_ok) {
          if (show_progress)
            message("Skipping (load manifest confirms already loaded): ",
                    tbl_name, "  (", marker$rows, " rows)")
          if (delete_source_parquet) file.remove(pf)
          report[[file_i]] <- data.frame(
            table = tbl_name, rows = marker$rows, action = "skipped",
            stringsAsFactors = FALSE
          )
          next
        }

        # Exact count to detect a fully vs partially loaded table. This is the
        # safe choice (no silent partial skips) - only reached when the fast
        # path above could not confirm completeness on its own.
        loaded <- .pg_row_count(con, schema, tbl_name)
        # isTRUE() guards against an unknown (NA) parquet row count: if we
        # cannot confirm completeness we fall through and reload rather than
        # skip a possibly-incomplete table.
        if (!is.na(parquet_rows) && isTRUE(loaded == parquet_rows)) {
          if (show_progress)
            message("Skipping (already loaded): ", tbl_name,
                    "  (", loaded, " rows)")
          if (!is.na(manifest_completed_at))
            .ehr_load_manifest_mark(con, schema, tbl_name, loaded,
                                    manifest_completed_at)
          if (delete_source_parquet) file.remove(pf)
          report[[file_i]] <- data.frame(
            table = tbl_name, rows = loaded, action = "skipped",
            stringsAsFactors = FALSE
          )
          next
        }
        if (loaded > 0) {
          if (show_progress)
            message("Resuming ", tbl_name, ": found ", loaded, " of ",
                    parquet_rows, " rows; truncating and reloading.")
          need_truncate <- TRUE
        }
      } else {
        need_truncate <- TRUE   # full reload: clear but keep DDL types
      }
    }

    if (show_progress) message("Loading to Postgres: ", tbl_name)

    reader <- arrow::open_dataset(pf, format = "parquet") |>
      arrow::as_record_batch_reader()

    # -- Load this table in a single transaction ----------------------------
    DBI::dbBegin(con)
    tryCatch({
      # A valid empty Parquet may yield no record batch. If DDL did not create
      # the table, create it transactionally from Arrow's typed zero-row data.
      if (!exists && identical(parquet_rows, 0)) {
        empty <- as.data.frame(arrow::read_parquet(pf))
        DBI::dbWriteTable(con, tbl_id, empty, row.names = FALSE,
                          overwrite = FALSE, append = FALSE)
        exists <- TRUE
      }

      if (need_truncate) {
        DBI::dbExecute(con, sprintf(
          "TRUNCATE TABLE %s",
          DBI::dbQuoteIdentifier(con, tbl_id)
        ))
      }

      # Switch to UNLOGGED only once the table is EMPTY (freshly DDL-created or
      # just truncated above). Doing it earlier would force Postgres to rewrite
      # any existing rows just before they are discarded - expensive on a large
      # already-loaded table such as chartevents.
      if (isTRUE(unlogged) && exists) {
        DBI::dbExecute(con, sprintf("ALTER TABLE %s SET UNLOGGED",
                                    DBI::dbQuoteIdentifier(con, tbl_id)))
      }

      n_total <- 0L
      first   <- TRUE
      batch_i <- 0L
      while (!is.null(batch <- reader$read_next_batch())) {
        batch_i <- batch_i + 1L
        chunk <- as.data.frame(batch)
        chunk[] <- lapply(chunk, function(x) if (is.factor(x)) as.character(x) else x)

        if (exists) {
          # Table already has DDL-defined types: append, preserving them.
          # Float columns land in unconstrained NUMERIC, which keeps whatever
          # digits the text carries; RPostgres formats a double column with
          # one digit count for the whole batch, so the stored text (and the
          # event_id hashed from it) depended on batch boundaries. Format
          # each value on its own first.
          chunk[] <- lapply(chunk, .ehr_pg_numeric_text)
          DBI::dbAppendTable(con, tbl_id, chunk)
        } else {
          # No pre-created table: create from first batch, then append.
          DBI::dbWriteTable(con, tbl_id, chunk, row.names = FALSE, append = !first)
        }

        n_total <- n_total + nrow(chunk)
        first   <- FALSE

        # Per-batch progress for long-running tables (e.g. chartevents):
        # report the first batch and every 10th thereafter.
        if (show_progress && (batch_i == 1L || batch_i %% 10L == 0L)) {
          elapsed_min <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
          message("  ... ", tbl_name, ": ", prettyNum(n_total, big.mark = ","),
                  " rows after ", elapsed_min, " min")
        }

        # Reclaim memory periodically rather than every batch (per-batch gc()
        # measurably slows large loads).
        rm(chunk, batch)
        if (batch_i %% 25L == 0L) gc(verbose = FALSE)
      }

      loaded_rows <- .pg_row_count(con, schema, tbl_name)
      if (!is.na(parquet_rows) && !identical(loaded_rows, parquet_rows))
        stop("Loaded row count does not match Parquet metadata.", call. = FALSE)
      # Record the count just verified above so a later resume-check can
      # trust it (see .ehr_load_manifest_lookup()) instead of re-scanning this
      # table. Written inside this same transaction: it only becomes visible
      # if the load itself actually commits.
      if (!is.na(manifest_completed_at))
        .ehr_load_manifest_mark(con, schema, tbl_name, loaded_rows,
                                manifest_completed_at)
      DBI::dbCommit(con)
      report[[file_i]] <- data.frame(
        table = tbl_name, rows = loaded_rows, action = "loaded",
        stringsAsFactors = FALSE
      )

      elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
      if (show_progress)
        message("  Done: ", tbl_name, "  (", n_total, " rows, ", elapsed, "s)")
    }, error = function(e) {
      DBI::dbRollback(con)
      stop("Failed loading table '", tbl_name, "' (rolled back): ",
           conditionMessage(e), call. = FALSE)
    })

    # Convert back to a durable LOGGED table after a successful load. This
    # rewrites the table, so it costs time but restores crash durability.
    if (isTRUE(unlogged) && isTRUE(set_logged) && exists) {
      if (show_progress) message("  Converting to LOGGED: ", tbl_name)
      DBI::dbExecute(con, sprintf("ALTER TABLE %s SET LOGGED",
                                  DBI::dbQuoteIdentifier(con, tbl_id)))
    }

    if (delete_source_parquet) {
      file.remove(pf)
      if (show_progress) message("  Deleted parquet: ", basename(pf))
    }
  }

  invisible(do.call(rbind, report))
}

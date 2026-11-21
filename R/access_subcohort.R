
# ___________________________________________________________________________
# Subgroup / active cohort - session filter + materialization
# ___________________________________________________________________________

# Internal: monotonically increasing cohort-version counter. Every
# set_active_cohort() call is tagged with a fresh, never-reused version, so
# .setup_cohort_temp()'s cache below can tell "this connection's _ehr_cohort
# reflects the current active cohort" via an exact integer comparison rather
# than a probabilistic content fingerprint.
.ehr_next_cohort_version <- function() {
  v <- (.ehr_env$.cohort_version_counter %||% 0L) + 1L
  .ehr_env$.cohort_version_counter <- v
  v
}

# Internal: write ids into a per-connection Postgres TEMP TABLE.
# Using a JOIN instead of IN (...) keeps performance flat for large ID lists.
#
# Skips the TRUNCATE+rewrite when the connection's _ehr_cohort already
# reflects `version` (recorded in a second TEMP TABLE, so the cache is
# naturally scoped to the same Postgres session as _ehr_cohort itself - no
# R-side connection-identity bookkeeping needed). This avoids rewriting the
# table on every one of fetch_table()'s repeated per-concept calls (e.g.
# inside fetch_concepts()) within one connection, without approximating
# equality via a content hash.
.setup_cohort_temp <- function(con, ids, version) {
  current <- tryCatch(
    DBI::dbGetQuery(con, "SELECT version FROM _ehr_cohort_version")$version,
    error = function(e) NA_integer_
  )
  if (length(current) == 1 && !is.na(current) &&
      identical(as.integer(current), as.integer(version))) {
    return(invisible(NULL))
  }

  DBI::dbExecute(con,
    "CREATE TEMP TABLE IF NOT EXISTS _ehr_cohort (cohort_id TEXT)")
  DBI::dbExecute(con, "TRUNCATE _ehr_cohort")
  DBI::dbWriteTable(con, "_ehr_cohort",
                    data.frame(cohort_id = as.character(ids)),
                    append = TRUE, row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS _idx_ehr_cohort ON _ehr_cohort (cohort_id)")

  DBI::dbExecute(con,
    "CREATE TEMP TABLE IF NOT EXISTS _ehr_cohort_version (version INTEGER)")
  DBI::dbExecute(con, "TRUNCATE _ehr_cohort_version")
  DBI::dbExecute(con, "INSERT INTO _ehr_cohort_version VALUES ($1)",
                 params = list(as.integer(version)))
  invisible(NULL)
}

# Internal: return active cohort list (ids + id_col + version) for a source,
# or NULL.
.get_active_ids <- function(src_name) {
  if (is.null(.ehr_env$active_cohorts)) return(NULL)
  .ehr_env$active_cohorts[[src_name]]
}

# Internal: extract just the ID vector / id column / version from the stored
# cohort list.
.get_cohort_ids    <- function(cohort) cohort$ids
.get_cohort_id_col <- function(cohort) cohort$id_col
.get_cohort_version <- function(cohort) cohort$version

# Convert only canonical integer strings: the previous text comparison did
# not match '001', '+1', whitespace, or out-of-range IDs to integer keys.
# Nested CASE guards keep malformed input away from numeric casts regardless
# of predicate evaluation order. Nonmatching inputs remain nonmatches.
.cohort_integer_expr <- function(type) {
  limits <- switch(type,
    int2 = c("-32768", "32767", "smallint"),
    int4 = c("-2147483648", "2147483647", "integer"),
    int8 = c("-9223372036854775808", "9223372036854775807", "bigint"),
    stop("Unsupported integer identifier type: ", type))
  sprintf(paste0("CASE WHEN length(cohort_id) <= 20 ",
    "AND cohort_id ~ '^(0|-?[1-9][0-9]*)$' THEN ",
    "CASE WHEN cohort_id::numeric BETWEEN %s AND %s ",
    "THEN cohort_id::%s END END"), limits[1], limits[2], limits[3])
}

# Shared by interactive access and subcohort materialization. Keep the large
# table's integer key bare so its ordinary index can serve the lookup.
.cohort_filter_join <- function(con, schema, table, id_col) {
  meta <- DBI::dbGetQuery(con,
    "SELECT udt_schema, udt_name FROM information_schema.columns
     WHERE table_schema = $1 AND table_name = $2 AND column_name = $3",
    params = list(schema, table, id_col))
  id <- DBI::dbQuoteIdentifier(con, id_col)
  if (nrow(meta) == 1 && meta$udt_schema == "pg_catalog" &&
      meta$udt_name %in% c("int2", "int4", "int8")) {
    DBI::dbExecute(con, "DROP TABLE IF EXISTS pg_temp._ehr_cohort_typed")
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE _ehr_cohort_typed AS SELECT ",
      .cohort_integer_expr(meta$udt_name), " AS cohort_id FROM _ehr_cohort"))
    DBI::dbExecute(con, "ANALYZE _ehr_cohort_typed")
    return(paste0("JOIN _ehr_cohort_typed c ON t.", id, " = c.cohort_id"))
  }
  # Preserve existing semantics for text and other source-specific ID types.
  paste0("JOIN _ehr_cohort c ON t.", id, "::text = c.cohort_id")
}

#' Build a patient subgroup ID list by filtering cohort_info
#'
#' Queries cohort_info with equality filters on any column. Returns a vector
#' of patient IDs suitable for set_active_cohort() or materialize_subcohort().
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param ... Named filter arguments. Each name must be a column in cohort_info;
#'   each value is a vector of acceptable values (matched with SQL IN).
#'   Example: `scid_injury_level = c("thoracic", "lumbar")` (SCI profile).
#' @param sample_n Integer. Return a random sample of this size instead of
#'   all matching IDs. NULL returns all. Default NULL.
#' @param seed Integer seed for reproducible sampling. Default NULL.
#' @param con Optional explicit DBI connection.
#' @return A vector of patient IDs (integer or character depending on source).
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", creds)
#'
#' # Thoracic or lumbar injuries (columns from the SCI cohort profile)
#' ids <- build_subcohort("mimic_iv_demo",
#'           scid_injury_level = c("thoracic", "lumbar"))
#'
#' # Reproducible random sample of 10 thoracic patients
#' ids <- build_subcohort("mimic_iv_demo",
#'           scid_injury_level = "thoracic",
#'           sample_n     = 10,
#'           seed         = 42)
#'
#' # No filters - sample 50 from the full cohort
#' ids <- build_subcohort("mimic_iv_demo", sample_n = 50)
#' }
#'
#' @param id_col Optional identifier column to return/filter on. Defaults to
#'   `cohort_key(src_name)`.
build_subcohort <- function(src_name, ..., id_col = NULL,
                             sample_n = NULL, seed = NULL, con = NULL) {
  filters      <- list(...)
  schema_names <- src_schema_names(src_name)
  id_col       <- if (!is.null(id_col)) id_col else cohort_key(src_name)

  .with_connection(con, function(con) {
    where_parts <- character(0)
    for (col in names(filters)) {
      vals   <- filters[[col]]
      quoted <- paste(
        vapply(vals, function(v) as.character(DBI::dbQuoteLiteral(con, v)),
               character(1)),
        collapse = ", "
      )
      where_parts <- c(where_parts,
                       sprintf('"%s" IN (%s)', col, quoted))
    }
    where_sql <- if (length(where_parts) > 0)
      paste("WHERE", paste(where_parts, collapse = "\n  AND "))
    else ""

    q <- sprintf('SELECT DISTINCT "%s" FROM "%s".cohort_info %s',
                 id_col, schema_names$raw, where_sql)
    ids <- DBI::dbGetQuery(con, q)[[1]]

    total <- DBI::dbGetQuery(con, sprintf(
      'SELECT COUNT(DISTINCT "%s") AS n FROM "%s".cohort_info',
      id_col, schema_names$raw
    ))$n

    if (length(ids) == 0) {
      message("No patients matched the filter criteria.")
      return(ids)
    }

    if (!is.null(sample_n) && sample_n < length(ids)) {
      if (!is.null(seed)) set.seed(seed)
      ids <- sample(ids, sample_n)
    }

    message(sprintf("Subgroup: %d / %s patients  [%s]",
                    length(ids), format(total, big.mark = ","), src_name))
    ids
  })
}

#' Activate a patient subgroup filter for a source
#'
#' Once set, all fetch_table() / fetch_concept() / fetch_concepts() calls for
#' `src_name` automatically filter rows to these patients only. The filter is
#' implemented via a per-call Postgres TEMP TABLE JOIN - performance is stable
#' even for tens of thousands of IDs.
#'
#' @param src_name Source name.
#' @param ids Vector of patient IDs (from build_subcohort() or any custom vector).
#' @return The cohort IDs, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' ids <- build_subcohort("mimic_iv_demo", scid_injury_level = "thoracic")
#' set_active_cohort("mimic_iv_demo", ids)
#'
#' fetch_concept("mimic_iv_demo", "heart_rate")   # only these patients
#' fetch_concepts("mimic_iv_demo", c("heart_rate", "sbp"))
#'
#' clear_active_cohort("mimic_iv_demo")           # back to full cohort
#' }
#'
#' @param id_col Optional identifier column for `ids`. Defaults to
#'   `cohort_key(src_name)`.
set_active_cohort <- function(src_name, ids, id_col = NULL) {
  id_col <- id_col %||% cohort_key(src_name)
  if (is.null(.ehr_env$active_cohorts))
    .ehr_env$active_cohorts <- list()
  .ehr_env$active_cohorts[[src_name]] <- list(
    ids = ids, id_col = id_col, version = .ehr_next_cohort_version()
  )
  message(sprintf("Active cohort set: %d IDs by '%s'  [%s]",
                  length(ids), id_col, src_name))
  invisible(ids)
}

#' Clear the active patient subgroup filter
#'
#' @param src_name Source name to clear. NULL clears all sources.
#' @export
# No .setup_cohort_temp() version-cache action is needed here: clearing
# removes the cohort entry entirely, so fetch_table() skips the whole
# subcohort-filter branch
# (including .setup_cohort_temp()) until a later set_active_cohort() call,
# which always assigns a fresh, never-reused version - a stale
# _ehr_cohort_version value left over from before the clear can never
# coincidentally match a subsequent, different cohort's version.
#' @return `NULL`, invisibly.
#' @examples
#' clear_active_cohort()
clear_active_cohort <- function(src_name = NULL) {
  if (is.null(src_name)) {
    .ehr_env$active_cohorts <- list()
    message("Active cohort filters cleared for all sources.")
  } else {
    .ehr_env$active_cohorts[[src_name]] <- NULL
    message("Active cohort cleared: ", src_name)
  }
  invisible(NULL)
}

#' Show currently active patient subgroup filters
#' @return The list of active cohort filters, one entry per source with `ids`, `id_col` and `version`, invisibly; `NULL` when none is set.
#' @examples
#' active_cohort_info()
#' @export
active_cohort_info <- function() {
  cohorts <- .ehr_env$active_cohorts
  if (is.null(cohorts) || all(vapply(cohorts, is.null, logical(1)))) {
    message("No active cohort filters.")
    return(invisible(NULL))
  }
  for (src in names(cohorts)) {
    cohort <- cohorts[[src]]
    if (!is.null(cohort))
      message(sprintf("  %-30s  %d IDs by '%s'",
                      src, length(cohort$ids), cohort$id_col))
  }
  invisible(cohorts)
}

# ___________________________________________________________________________
# Time window filter - ICU vs hospital admission reference
# ___________________________________________________________________________

#' Set the time window reference for a source
#'
#' Once set, all fetch_concept() / fetch_concepts() calls for `src_name`
#' automatically apply the time window filter. Use `ref = "icu"` to work in
#' ICU-relative time; set `clip = TRUE` to restrict measurements to within
#' the ICU stay (time_since_icu_min BETWEEN 0 AND icu_los_min).
#'
#' Requires that ts_tab was built after the window tables/icu_los_min update
#' (i.e. ts_tab has time_since_icu_min and icu_los_min columns).
#'
#' @param src_name Source name.
#' @param ref Time reference: "hadm" (default, hospital admission) or "icu".
#' @param clip If TRUE and ref = "icu", filter to 0 <= time_since_icu_min
#'   <= icu_los_min. Ignored when ref = "hadm". Default FALSE.
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' set_time_window("mimic_iv_demo", ref = "icu", clip = TRUE)
#' fetch_concept("mimic_iv_demo", "heart_rate")   # ICU-relative, clipped
#' clear_time_window("mimic_iv_demo")
#'
#' # Composable with subcohort filter:
#' set_active_cohort("mimic_iv_demo", ids)
#' set_time_window("mimic_iv_demo", ref = "icu", clip = TRUE)
#' fetch_concept("mimic_iv_demo", "heart_rate")   # subgroup + ICU window
#' }
set_time_window <- function(src_name, ref = c("hadm", "icu"), clip = FALSE) {
  ref <- match.arg(ref)
  if (is.null(.ehr_env$time_windows))
    .ehr_env$time_windows <- list()
  .ehr_env$time_windows[[src_name]] <- list(ref = ref, clip = clip)
  message(sprintf("Time window set: ref = '%s', clip = %s  [%s]",
                  ref, clip, src_name))
  invisible(NULL)
}

#' Clear the time window filter
#'
#' @param src_name Source to clear. NULL clears all sources.
#' @return `NULL`, invisibly.
#' @examples
#' clear_time_window()
#' @export
clear_time_window <- function(src_name = NULL) {
  if (is.null(src_name)) {
    .ehr_env$time_windows <- list()
    message("Time window filters cleared for all sources.")
  } else {
    .ehr_env$time_windows[[src_name]] <- NULL
    message("Time window cleared: ", src_name)
  }
  invisible(NULL)
}

#' Show currently active time window filters
#' @return The list of active time windows, one entry per source with `ref` and `clip`, invisibly; `NULL` when none is set.
#' @examples
#' active_time_window_info()
#' @export
active_time_window_info <- function() {
  wins <- .ehr_env$time_windows
  if (is.null(wins) || all(vapply(wins, is.null, logical(1)))) {
    message("No active time window filters.")
    return(invisible(NULL))
  }
  for (src in names(wins)) {
    w <- wins[[src]]
    if (!is.null(w))
      message(sprintf("  %-30s  ref = '%s', clip = %s",
                      src, w$ref, w$clip))
  }
  invisible(wins)
}

# Internal: return active time window for a source, or NULL.
.get_time_window <- function(src_name) {
  if (is.null(.ehr_env$time_windows)) return(NULL)
  .ehr_env$time_windows[[src_name]]
}

# ___________________________________________________________________________
# Materialize subgroup - permanent filtered schemas in Postgres
# ___________________________________________________________________________

#' Materialize a patient subgroup as new Postgres schemas
#'
#' Creates independent schemas (e.g. `mimic_iv_demo_cervical_coh`,
#' `mimic_iv_demo_cervical_coh_concepts`, etc.) by filtering all tables
#' from the source schemas to the provided patient IDs. Tables that have no
#' patient ID column (lookup / reference tables) are copied as-is.
#'
#' After materialization the subgroup behaves like a regular source:
#' `fetch_concept("mimic_iv_demo_cervical", "heart_rate")` works without
#' any active-cohort filter.
#'
#' @param src_name Original source name (e.g. "mimic_iv_demo").
#' @param ids Vector of patient IDs from build_subcohort() or custom.
#' @param subgroup_name Short label appended to src_name to form the new
#'   source name: `{src_name}_{subgroup_name}`.
#'   Example: `"cervical"` -> new source `"mimic_iv_demo_cervical"`.
#' @param schemas Which schemas to copy. Default `c("raw", "concepts", "harmonized")`.
#'   Any of: `"raw"`, `"concepts"`, `"harmonized"`, `"derived"`.
#' @param overwrite Drop existing subgroup schemas before recreating.
#'   Default FALSE.
#' @param con Optional explicit DBI connection.
#' @return Invisibly returns the new source name.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", creds)
#'
#' ids <- build_subcohort("mimic_iv_demo", scid_injury_level = "thoracic")
#'
#' materialize_subcohort("mimic_iv_demo", ids, "thoracic")
#'
#' # The subgroup is now a first-class source
#' fetch_concept("mimic_iv_demo_thoracic", "heart_rate")
#' cohort_summary("mimic_iv_demo_thoracic")
#'
#' # Clean up when done
#' drop_subcohort("mimic_iv_demo", "cervical_complete")
#' }
materialize_subcohort <- function(src_name, ids, subgroup_name,
                                   schemas  = c("raw", "concepts", "harmonized"),
                                   overwrite = FALSE,
                                   con = NULL) {
  src_name      <- .ehr_validate_db_identifier(src_name, "src_name")
  subgroup_name <- .ehr_validate_db_identifier(subgroup_name, "subgroup_name")
  schema_names     <- src_schema_names(src_name)
  sub_src_name     <- paste0(src_name, "_", subgroup_name)
  sub_schema_names <- src_schema_names(sub_src_name)
  id_col           <- .ehr_validate_db_identifier(cohort_key(src_name), "id column name")

  message(sprintf("Materializing subgroup '%s' from '%s'  (%d patients)...",
                  subgroup_name, src_name, length(ids)))

  .with_connection(con, function(con) {
    .setup_cohort_temp(con, ids, .ehr_next_cohort_version())

    existing_schemas <- DBI::dbGetQuery(con,
      "SELECT schema_name FROM information_schema.schemata"
    )$schema_name

    for (s in schemas) {
      src_schema <- .ehr_validate_db_identifier(schema_names[[s]], "schema name")
      dst_schema <- .ehr_validate_db_identifier(sub_schema_names[[s]], "schema name")

      if (!src_schema %in% existing_schemas) {
        message("  Skipping (not found): ", src_schema)
        next
      }

      if (overwrite && dst_schema %in% existing_schemas) {
        DBI::dbExecute(con,
          paste0("DROP SCHEMA IF EXISTS ", DBI::dbQuoteIdentifier(con, dst_schema), " CASCADE"))
        message("  Dropped existing: ", dst_schema)
      }

      DBI::dbExecute(con,
        paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, dst_schema)))

      tables <- DBI::dbGetQuery(con,
        "SELECT table_name FROM information_schema.tables
         WHERE table_schema = $1 ORDER BY table_name", params = list(src_schema)
      )$table_name

      message(sprintf("  %s -> %s  (%d tables)",
                      src_schema, dst_schema, length(tables)))

      for (tbl in tables) {
        dst_tbl  <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = dst_schema, table = tbl))
        src_tbl  <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = src_schema, table = tbl))
        tbl_cols <- DBI::dbListFields(
          con, DBI::Id(schema = src_schema, table = tbl))

        if (overwrite) {
          DBI::dbExecute(con,
            paste0("DROP TABLE IF EXISTS ", dst_tbl, " CASCADE"))
        }

        if (id_col %in% tbl_cols) {
          join_sql <- .cohort_filter_join(con, src_schema, tbl, id_col)
          DBI::dbExecute(con, sprintf(
            'CREATE TABLE %s AS
             SELECT t.* FROM %s t
             %s',
            dst_tbl, src_tbl, join_sql
          ))
          n <- DBI::dbGetQuery(con,
            paste0("SELECT COUNT(*) AS n FROM ", dst_tbl))$n
          message(sprintf("    [filtered] %-35s %s rows",
                          tbl, format(n, big.mark = ",")))
        } else {
          DBI::dbExecute(con, sprintf(
            "CREATE TABLE %s AS SELECT * FROM %s", dst_tbl, src_tbl
          ))
          message(sprintf("    [copied]   %s", tbl))
        }
      }
    }

    message(sprintf(
      "\nDone. Subgroup source: '%s'\n%s",
      sub_src_name,
      paste0("  fetch_concept(\"", sub_src_name, "\", \"heart_rate\")  - works as-is")
    ))
    invisible(sub_src_name)
  })
}

#' Drop all schemas for a materialized subgroup
#'
#' Calls drop_source_schemas() for the subgroup source name. Dry-run by default.
#'
#' @param src_name Original source name (e.g. "mimic_iv_demo").
#' @param subgroup_name Subgroup name used when calling materialize_subcohort().
#' @param dry_run If TRUE (default), only print what would be dropped.
#' @param con Optional explicit DBI connection.
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' drop_subcohort("mimic_iv_demo", "cervical_complete")           # preview
#' drop_subcohort("mimic_iv_demo", "cervical_complete", dry_run = FALSE)
#' }
drop_subcohort <- function(src_name, subgroup_name, dry_run = TRUE, con = NULL) {
  sub_src_name <- paste0(src_name, "_", subgroup_name)
  drop_source_schemas(sub_src_name, dry_run = dry_run, con = con)
}

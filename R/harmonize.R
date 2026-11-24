#' Build harmonized time-series and flat output tables.

# ---------------------------------------------------------------------------
# Internal table builders
# ---------------------------------------------------------------------------

#' Build the long-format time-series table from concept tables
#'
#' Unions all valid time-series concept tables in the concepts schema into a
#' single `ts_tab` table. Concepts missing expected columns or not found in the
#' database are silently skipped with a warning.
#'
#' @param con A DBI connection to the Postgres database.
#' @param src_name Source name (e.g. `"mimic_iv_demo"`).
#' @param schema_names Named list of schema names (from `src_schema_names()`).
#' @param concept_names Character vector of concept table names to include.
#' @param table_name Name for the output table (default `"ts_tab"`).
#'
#' @return Invisibly, the number of rows affected by the CREATE TABLE statement.
#' @keywords internal
build_ts_table <- function(con, src_name, schema_names,
                           concept_names, table_name = "ts_tab") {
  target_id <- DBI::dbQuoteIdentifier(con, DBI::Id(
    schema = .ehr_validate_db_identifier(schema_names$harmonized, "schema name"),
    table  = .ehr_validate_db_identifier(table_name, "table name")
  ))
  cfg       <- get_dataset_config(src_name)
  id_cols   <- cfg$select_id_cols
  id_select <- paste(id_cols, collapse = ", ")

  existing_tables <- DBI::dbGetQuery(con, glue::glue(
    "SELECT tablename FROM pg_tables
     WHERE schemaname = '{schema_names$concepts}'"
  ))$tablename

  missing <- setdiff(concept_names, existing_tables)
  if (length(missing) > 0)
    warning("Missing concept tables (skipped): ", paste(missing, collapse = ", "))

  valid_concepts <- intersect(concept_names, existing_tables)
  if (length(valid_concepts) == 0) stop("No valid concept tables found.")

  # Probe each concept individually: a table that cannot be read is skipped,
  # so one bad table cannot block the rest. Also detect whether the concept has
  # ICU time and event metadata. A table with the wrong columns still fails the
  # union; the user SQL contract requires select_id_cols, time, value and unit.
  concept_meta <- lapply(valid_concepts, function(cn) {
    tryCatch({
      cols <- DBI::dbListFields(con, DBI::Id(schema = schema_names$concepts,
                                             table  = cn))
      has_icu <- "time_since_icu_min" %in% cols && "icu_los_min" %in% cols
      list(name = cn, ok = TRUE, has_icu = has_icu, cols = cols)
    }, error = function(e) {
      message("  Skipping ts concept '", cn, "': ", conditionMessage(e))
      list(name = cn, ok = FALSE, has_icu = FALSE, cols = character())
    })
  })

  good_meta    <- Filter(function(m) m$ok,      concept_meta)
  good_concepts <- vapply(good_meta, `[[`, character(1), "name")

  if (length(good_concepts) == 0) stop("No usable ts concept tables found.")

  union_parts <- vapply(good_meta, function(m) {
    icu_cols <- if (m$has_icu)
      "time_since_icu_min, icu_los_min,"
    else
      "NULL::numeric AS time_since_icu_min, NULL::numeric AS icu_los_min,"
    meta_col <- function(col) {
      if (col %in% m$cols) col else sprintf("NULL::text AS %s", col)
    }
    bool_col <- function(col) {
      if (col %in% m$cols) col else sprintf("NULL::boolean AS %s", col)
    }
    sprintf(
      paste0(
        "SELECT %s, time_since_hadm_min, %s '%s' AS concept_name, ",
        "value, unit, %s, %s, %s, %s, %s, %s, %s FROM %s.%s"
      ),
      id_select, icu_cols, m$name,
      meta_col("event_id"),
      meta_col("event_level"),
      meta_col("native_id_level"),
      meta_col("time_precision"),
      bool_col("dedup_candidate"),
      meta_col("source_table"),
      meta_col("source_dataset"),
      schema_names$concepts, m$name
    )
  }, character(1))

  full_sql <- glue::glue(
    "DROP TABLE IF EXISTS {target_id};\n",
    "CREATE TABLE {target_id} AS\n",
    paste(union_parts, collapse = "\nUNION ALL\n"), ";"
  )

  DBI::dbExecute(con, full_sql, immediate = TRUE)
}

#' Build the wide/flat table with one column per static concept
#'
#' Joins each flat concept table onto `visit_windows` using the source's ID
#' columns. Concepts whose join or value column cannot be verified are skipped
#' with a message. If `flat_concepts` is empty the table is still created with
#' only the base ID columns.
#'
#' @param con A DBI connection to the Postgres database.
#' @param src_name Source name (e.g. `"mimic_iv_demo"`).
#' @param schema_names Named list of schema names (from `src_schema_names()`).
#' @param flat_concepts Character vector of concept table names to pivot as
#'   columns (e.g. `c("sex", "race")`).
#' @param table_name Name for the output table (default `"flat_tab"`).
#'
#' @return Invisibly, the number of rows affected by the CREATE TABLE statement.
#' @keywords internal
build_flat_table <- function(con, src_name, schema_names,
                             flat_concepts, table_name = "flat_tab") {
  target_id <- DBI::dbQuoteIdentifier(con, DBI::Id(
    schema = .ehr_validate_db_identifier(schema_names$harmonized, "schema name"),
    table  = .ehr_validate_db_identifier(table_name, "table name")
  ))
  cfg     <- get_dataset_config(src_name)
  id_cols <- cfg$select_id_cols

  # Equality is equivalent to NULL-safe equality when the base key cannot
  # be NULL. Check the actual cohort once instead of assuming source-wide
  # nullability. Ordinary equality gives PostgreSQL hash/merge join keys.
  non_null_cols <- character()
  if (length(flat_concepts) > 0 && any(nzchar(flat_concepts))) {
    non_null_cols <- .non_null_id_cols(con, schema_names$raw, "visit_windows", id_cols)
  }

  # Columns that are identifiers or metadata - not the concept value.
  meta_cols <- c(id_cols, "source_table", "source_dataset",
                 "source_itemid", "source_label",
                 "event_id", "event_level", "native_id_level", "time_precision",
                 "dedup_candidate")

  # For each concept table, find the value column: prefer a column named the
  # same as the concept; otherwise take the first non-metadata column. When
  # more than one non-metadata candidate exists, the choice is positional
  # (declaration order), not semantic - warn so this is never silent.
  .value_col <- function(concept) {
    cols <- tryCatch(
      DBI::dbListFields(con, DBI::Id(schema = schema_names$concepts, table = concept)),
      error = function(e) character(0)
    )
    if (concept %in% cols) concept
    else {
      candidates <- setdiff(cols, meta_cols)
      if (length(candidates) > 1)
        warning(
          "build_flat_table: concept '", concept, "' has ", length(candidates),
          " candidate value columns (", paste(candidates, collapse = ", "),
          "); using '", candidates[1], "' (first by column order). ",
          "Skipped: ", paste(candidates[-1], collapse = ", "),
          call. = FALSE
        )
      if (length(candidates) > 0) candidates[1] else concept
    }
  }

  if (length(flat_concepts) == 0 || all(flat_concepts == "")) {
    select_cols <- ""
    joins       <- ""
  } else {
    # Probe each concept: verify the NULL-safe ID join and value column work
    # before including it, so one broken table can't block the rest.
    # resolve each value column once, so an ambiguity warns once
    value_cols <- list()
    good <- Filter(function(cn) {
      vc <- .value_col(cn)
      value_cols[[cn]] <<- vc
      join_cond <- .null_safe_id_join("base", "t", id_cols, non_null_cols)
      ok <- tryCatch({
        DBI::dbGetQuery(con, sprintf(
          "SELECT base.%s, t.%s FROM %s.visit_windows base
           LEFT JOIN %s.%s t ON %s LIMIT 0",
          id_cols[1], vc,
          schema_names$raw,
          schema_names$concepts, cn,
          join_cond
        ))
        TRUE
      }, error = function(e) {
        message("  Skipping flat concept '", cn, "': ", conditionMessage(e))
        FALSE
      })
      # A flat concept must hold at most one row per visit_windows key;
      # otherwise the LEFT JOIN below would multiply flat_tab rows.
      if (ok) {
        dup <- DBI::dbGetQuery(con, sprintf(
          "SELECT count(*) - count(DISTINCT ROW(%s)) AS n FROM %s.%s",
          paste(id_cols, collapse = ", "), schema_names$concepts, cn))$n
        if (isTRUE(as.numeric(dup) > 0)) {
          warning("build_flat_table: flat concept '", cn, "' has ", dup,
                  " row(s) beyond one per ", paste(id_cols, collapse = "/"),
                  " and was skipped, since joining it would duplicate flat_tab rows.",
                  call. = FALSE)
          ok <- FALSE
        }
      }
      ok
    }, flat_concepts)

    if (length(good) == 0) {
      select_cols <- ""
      joins       <- ""
    } else {
      val_cols    <- vapply(good, function(cn) value_cols[[cn]], character(1), USE.NAMES = FALSE)
      select_cols <- paste0(
        good, ".", val_cols, " AS ", good,
        collapse = ",\n  "
      )
      joins <- paste0(
        "LEFT JOIN ", schema_names$concepts, ".", good,
        " ", good,
        " ON ",
        vapply(good, function(cn) {
          .null_safe_id_join("base", cn, id_cols, non_null_cols)
        }, character(1)),
        collapse = "\n"
      )
    }
  }

  sql <- glue::glue("
DROP TABLE IF EXISTS {target_id};
CREATE TABLE {target_id} AS
SELECT
  '{src_name}' AS source_data,
  {paste(paste0('base.', id_cols), collapse = ',\n  ')}
  {if (nzchar(select_cols)) paste0(',\n  ', select_cols) else ''}
FROM {schema_names$raw}.visit_windows base
{joins};
")

  DBI::dbExecute(con, sql, immediate = TRUE)
  n_base <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM %s.visit_windows", schema_names$raw))$n
  n_flat <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", target_id))$n
  if (!isTRUE(as.numeric(n_flat) == as.numeric(n_base)))
    stop("build_flat_table: ", table_name, " has ", n_flat, " rows but visit_windows has ",
         n_base, "; a flat concept fanned out the join.", call. = FALSE)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Public runner
# ---------------------------------------------------------------------------

#' Build the long-format duration table from concept tables
#'
#' Unions all valid duration concept tables (medication exposures, infusions,
#' vasopressors, ventilation episodes) in the concepts schema into a single
#' `dur_tab` table. Concepts missing expected columns or not found in the
#' database are silently skipped with a warning.
#'
#' @param con A DBI connection to the Postgres database.
#' @param src_name Source name (e.g. `"mimic_iv_demo"`).
#' @param schema_names Named list of schema names (from `src_schema_names()`).
#' @param concept_names Character vector of duration concept table names to include.
#' @param table_name Name for the output table (default `"dur_tab"`).
#'
#' @return Invisibly, the number of rows affected by the CREATE TABLE statement.
#' @keywords internal
build_dur_table <- function(con, src_name, schema_names,
                            concept_names, table_name = "dur_tab") {
  target_id <- DBI::dbQuoteIdentifier(con, DBI::Id(
    schema = .ehr_validate_db_identifier(schema_names$harmonized, "schema name"),
    table  = .ehr_validate_db_identifier(table_name, "table name")
  ))
  cfg       <- get_dataset_config(src_name)
  id_cols   <- cfg$select_id_cols
  id_select <- paste(id_cols, collapse = ", ")

  existing_tables <- DBI::dbGetQuery(con, glue::glue(
    "SELECT tablename FROM pg_tables
     WHERE schemaname = '{schema_names$concepts}'"
  ))$tablename

  missing <- setdiff(concept_names, existing_tables)
  if (length(missing) > 0)
    warning("Missing dur concept tables (skipped): ", paste(missing, collapse = ", "))

  valid_concepts <- intersect(concept_names, existing_tables)
  if (length(valid_concepts) == 0) stop("No valid dur concept tables found.")

  concept_meta <- lapply(valid_concepts, function(cn) {
    tryCatch({
      cols <- DBI::dbListFields(con, DBI::Id(schema = schema_names$concepts,
                                             table  = cn))
      has_icu <- "time_since_icu_min" %in% cols && "icu_los_min" %in% cols
      list(name = cn, ok = TRUE, has_icu = has_icu, cols = cols)
    }, error = function(e) {
      message("  Skipping dur concept '", cn, "': ", conditionMessage(e))
      list(name = cn, ok = FALSE, has_icu = FALSE, cols = character())
    })
  })

  good_meta     <- Filter(function(m) m$ok, concept_meta)
  good_concepts <- vapply(good_meta, `[[`, character(1), "name")

  if (length(good_concepts) == 0) stop("No usable dur concept tables found.")

  union_parts <- vapply(good_meta, function(m) {
    icu_cols <- if (m$has_icu)
      "time_since_icu_min, icu_los_min,"
    else
      "NULL::numeric AS time_since_icu_min, NULL::numeric AS icu_los_min,"
    dur_col <- function(col, type = NULL) {
      if (col %in% m$cols) return(col)
      if (!is.null(type)) return(sprintf("NULL::%s AS %s", type, col))
      sprintf("NULL AS %s", col)
    }
    meta_col <- function(col) {
      if (col %in% m$cols) col else sprintf("NULL::text AS %s", col)
    }
    sprintf(
      paste0(
        "SELECT %s, %s, %s, time_since_hadm_min, %s %s, %s, ",
        "'%s' AS concept_name, value, unit, %s, %s, %s, %s, %s, %s, %s ",
        "FROM %s.%s"
      ),
      id_select,
      dur_col("start_time"),
      dur_col("end_time"),
      icu_cols,
      dur_col("end_offset_min", "numeric"),
      dur_col("duration_min", "numeric"),
      m$name,
      meta_col("event_id"),
      meta_col("event_level"),
      meta_col("native_id_level"),
      meta_col("time_precision"),
      dur_col("dedup_candidate", "boolean"),
      meta_col("source_table"),
      meta_col("source_dataset"),
      schema_names$concepts, m$name
    )
  }, character(1))

  full_sql <- glue::glue(
    "DROP TABLE IF EXISTS {target_id};\n",
    "CREATE TABLE {target_id} AS\n",
    paste(union_parts, collapse = "\nUNION ALL\n"), ";"
  )

  DBI::dbExecute(con, full_sql, immediate = TRUE)
}

# ---------------------------------------------------------------------------
# Public runner
# ---------------------------------------------------------------------------

#' Build harmonized ts_tab and flat_tab output tables for one source
#'
#' Reads the concept config to discover which concept tables exist, builds
#' a long-format time-series table (ts_tab) and optionally a flat table
#' (flat_tab) with one column per flat concept.
#'
#' @param base_dir Project root directory.
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then "rendehr".
#' @param flat_concepts Character vector of flat concept names (e.g. sex, race).
#'   NULL or empty triggers auto-discovery of all non-ts tables in the concepts
#'   schema.
#' @param build_ts Build time-series table (default TRUE).
#' @param build_flat Build flat table (default TRUE).
#' @param build_dur Build duration table - medication exposures and infusion
#'   episodes unioned into a single long-format table (default TRUE).
#' @param ts_table_name Name for the time-series output table.
#' @param flat_table_name Name for the flat output table.
#' @param dur_table_name Name for the duration output table.
#' @return `TRUE`, invisibly, once every harmonized table is built.
#' @examples
#' \dontrun{
#' run_harmonized_tables(src_name = "mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
#' @param harmonization Optional overrides from `harmonization_options()` or a named list.
run_harmonized_tables <- function(base_dir = NULL, src_name, db_name = NULL,
                                  flat_concepts   = NULL,
                                  build_ts        = TRUE,
                                  build_flat      = TRUE,
                                  build_dur       = TRUE,
                                  ts_table_name   = "ts_tab",
                                  flat_table_name = "flat_tab",
                                  dur_table_name  = "dur_tab",
                                  harmonization = NULL) {
  .ehr_apply_grouped_options(
    list(harmonization = harmonization),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(run_harmonized_tables)), frame = environment())
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name      <- .ehr_resolve_db_name(db_name)
  schema_names <- src_schema_names(src_name)

  creds <- load_credentials(paths$credentials_json)
  ensure_database_exists(db_name, creds)
  con   <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  create_schemas(con, schema_names)

  # ts/dur concept discovery is schema introspection, same principle as
  # build_flat's auto-discovery below: any table in the concepts schema
  # matches ts/dur/flat, not whatever the base config CSV happens to list.
  # This makes discovery correct regardless of which route added the
  # concept - base CSV, user_concepts overlay, or hand-written SQL in
  # user/external - instead of only picking up concepts that are rows in
  # concept_config_csv_ts/dur. Contract: ts tables have a time_since_hadm_min
  # column; dur tables additionally have duration_min (set by
  # build_atomic_concept_unified_sql()'s generated SQL, and required of any
  # hand-written dur-shaped concept for it to be recognized here).
  if (build_ts || build_dur) {
    ts_shaped_tables <- DBI::dbGetQuery(con, sprintf(
      "SELECT DISTINCT table_name FROM information_schema.columns
       WHERE table_schema = '%s' AND column_name = 'time_since_hadm_min'",
      schema_names$concepts
    ))$table_name
    dur_shaped_tables <- DBI::dbGetQuery(con, sprintf(
      "SELECT DISTINCT table_name FROM information_schema.columns
       WHERE table_schema = '%s' AND column_name = 'duration_min'",
      schema_names$concepts
    ))$table_name
  }

  if (build_ts) {
    ts_concepts <- sort(setdiff(ts_shaped_tables, dur_shaped_tables))

    if (length(ts_concepts) == 0) {
      message("No TS concepts found for ", src_name)
    } else {
      build_ts_table(con, src_name, schema_names, ts_concepts, ts_table_name)
      message("TS table built: ", ts_table_name)
    }
  }

  if (build_flat) {
    # Auto-discover: if NULL, use every table in the concepts schema that is
    # NOT a ts/dur concept (identified by absence of time_since_hadm_min) AND
    # structurally joinable - i.e. has every one of the source's id columns
    # (subject_id/hadm_id/stay_id and equivalents), the same set
    # build_flat_table()'s null-safe join requires. A table missing one (e.g.
    # a hand-written helper/intermediate table that was never run through the
    # NULL-fill convention build_select_unified() applies to every generated
    # concept - confirmed empirically: it's the only such case) can never
    # join successfully, so it is excluded here with a clear, non-alarming
    # message instead of being discovered, attempted, and failing inside
    # build_flat_table()'s join-probe with a raw Postgres error that reads
    # like something broke.
    if (is.null(flat_concepts)) {
      ts_tables <- DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT table_name FROM information_schema.columns
         WHERE table_schema = '%s' AND column_name = 'time_since_hadm_min'",
        schema_names$concepts
      ))$table_name
      all_tables <- DBI::dbGetQuery(con, sprintf(
        "SELECT tablename FROM pg_tables WHERE schemaname = '%s'",
        schema_names$concepts
      ))$tablename
      # Exclude internal/temporary tables (names starting with "_")
      candidates <- sort(setdiff(all_tables[!startsWith(all_tables, "_")],
                                 ts_tables))

      id_cols <- get_dataset_config(src_name)$select_id_cols
      has_all_ids <- vapply(candidates, function(cn) {
        cols <- DBI::dbListFields(con, DBI::Id(schema = schema_names$concepts, table = cn))
        all(id_cols %in% cols)
      }, logical(1))
      flat_concepts   <- candidates[has_all_ids]
      not_joinable    <- candidates[!has_all_ids]

      if (length(flat_concepts) > 0)
        message("Auto-discovered flat concepts: ", paste(flat_concepts, collapse = ", "))
      if (length(not_joinable) > 0)
        message("Excluded from flat auto-discovery (missing one or more of ",
                paste(id_cols, collapse = ", "), "): ",
                paste(not_joinable, collapse = ", "))
    }

    if (length(flat_concepts) == 0) {
      message("No flat concepts found in ", schema_names$concepts, ". Skipping flat table.")
    } else {
      build_flat_table(con, src_name, schema_names, flat_concepts,
                       flat_table_name)
      message("Flat table built: ", flat_table_name)
    }
  }

  if (build_dur) {
    dur_concepts <- sort(intersect(ts_shaped_tables, dur_shaped_tables))

    if (length(dur_concepts) == 0) {
      message("No dur concepts found for ", src_name)
    } else {
      build_dur_table(con, src_name, schema_names, dur_concepts, dur_table_name)
      message("Dur table built: ", dur_table_name)
    }
  }

  invisible(TRUE)
}

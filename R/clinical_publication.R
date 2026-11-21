# Internal staged and transactional clinical-table publication helpers.

.clinical_run_token <- function() {
  raw <- paste0(Sys.getpid(), "_", format(Sys.time(), "%Y%m%d%H%M%OS6"))
  substr(gsub("[^A-Za-z0-9]", "", raw), 1L, 22L)
}

.clinical_relation_sql <- function(con, schema, table) {
  as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = table)))
}

.clinical_validate_frame <- function(df, table, required_columns = character(),
                                     nonmissing_columns = character(),
                                     unique_columns = character()) {
  if (!is.data.frame(df)) {
    stop("Clinical table `", table, "` is not a data frame.", call. = FALSE)
  }
  missing <- setdiff(required_columns, names(df))
  if (length(missing)) {
    stop("Clinical table `", table, "` is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  for (column in intersect(nonmissing_columns, names(df))) {
    value <- df[[column]]
    if (any(is.na(value) | (is.character(value) & !nzchar(trimws(value))))) {
      stop("Clinical table `", table, "` has missing `", column, "` values.",
           call. = FALSE)
    }
  }
  if (length(unique_columns)) {
    missing_unique <- setdiff(unique_columns, names(df))
    if (length(missing_unique)) {
      stop("Clinical table `", table, "` cannot validate unique key; missing ",
           paste(missing_unique, collapse = ", "), call. = FALSE)
    }
    if (anyDuplicated(df[unique_columns])) {
      stop("Clinical table `", table, "` failed unique-key validation for ",
           paste(unique_columns, collapse = ", "), call. = FALSE)
    }
  }
  invisible(df)
}

.clinical_drop_owned_staging <- function(con, schema, stage_names) {
  for (table in stage_names) {
    if (DBI::dbExistsTable(con, DBI::Id(schema = schema, table = table))) {
      DBI::dbExecute(con, paste0(
        "DROP TABLE ", .clinical_relation_sql(con, schema, table)))
    }
  }
  invisible(NULL)
}

.clinical_drop_duckdb_indexes <- function(con, schema, table) {
  if (!inherits(con, "duckdb_connection")) return(invisible(character()))
  idx <- DBI::dbGetQuery(con, paste0(
    "SELECT index_name FROM duckdb_indexes() WHERE schema_name = ",
    DBI::dbQuoteString(con, schema), " AND table_name = ",
    DBI::dbQuoteString(con, table)))$index_name
  for (i in idx) {
    DBI::dbExecute(con, paste0("DROP INDEX ", DBI::dbQuoteIdentifier(con, schema),
                               ".", DBI::dbQuoteIdentifier(con, i)))
  }
  invisible(idx)
}

.clinical_publish_tables <- function(con, schema, tables, specs = list(),
                                     indexes = list(), inject_failure = NULL) {
  .ehr_validate_db_identifier(schema, "schema name")
  if (!is.list(tables) || !length(tables) || is.null(names(tables)) ||
      any(!nzchar(names(tables)))) {
    stop("Clinical publication requires a named, nonempty table list.",
         call. = FALSE)
  }
  table_names <- names(tables)
  invisible(lapply(table_names, function(table) {
    .ehr_validate_db_identifier(table, "clinical table name")
  }))

  for (table in table_names) {
    spec <- specs[[table]] %||% list()
    .clinical_validate_frame(
      tables[[table]], table,
      required_columns = spec$required_columns %||% character(),
      nonmissing_columns = spec$nonmissing_columns %||% character(),
      unique_columns = spec$unique_columns %||% character()
    )
  }

  token <- .clinical_run_token()
  stage_names <- stats::setNames(
    vapply(table_names, function(table) {
      .ehr_validate_db_identifier(
        substr(paste0(table, "__stage_", token), 1L, 63L),
        "clinical staging table")
    }, character(1)),
    table_names
  )
  backup_names <- stats::setNames(
    vapply(table_names, function(table) {
      .ehr_validate_db_identifier(
        substr(paste0(table, "__backup_", token), 1L, 63L),
        "clinical backup table")
    }, character(1)),
    table_names
  )

  .clinical_drop_owned_staging(con, schema, unname(stage_names))
  on.exit({
    if (DBI::dbIsValid(con)) {
      try(.clinical_drop_owned_staging(con, schema, unname(stage_names)),
          silent = TRUE)
    }
  }, add = TRUE)

  for (table in table_names) {
    DBI::dbWriteTable(
      con,
      DBI::Id(schema = schema, table = stage_names[[table]]),
      as.data.frame(tables[[table]]), overwrite = FALSE, append = FALSE
    )
    written_columns <- DBI::dbListFields(
      con, DBI::Id(schema = schema, table = stage_names[[table]]))
    expected_columns <- names(tables[[table]])
    if (!identical(written_columns, expected_columns)) {
      stop("Staged clinical table `", table,
           "` changed schema during database write.", call. = FALSE)
    }
  }

  if (identical(inject_failure, "after_stage")) {
    stop("Injected publication failure after staging.", call. = FALSE)
  }

  tx_open <- FALSE
  DBI::dbBegin(con)
  tx_open <- TRUE
  tryCatch({
    existing <- vapply(table_names, function(table) {
      DBI::dbExistsTable(con, DBI::Id(schema = schema, table = table))
    }, logical(1))

    for (table in table_names[existing]) {
      # DuckDB refuses to rename a table that carries an index; the index is
      # rebuilt on the promoted table below, so dropping it here loses nothing
      .clinical_drop_duckdb_indexes(con, schema, table)
      DBI::dbExecute(con, paste0(
        "ALTER TABLE ", .clinical_relation_sql(con, schema, table),
        " RENAME TO ", DBI::dbQuoteIdentifier(con, backup_names[[table]])))
    }
    if (identical(inject_failure, "after_backup")) {
      stop("Injected publication failure after backup rename.",
           call. = FALSE)
    }

    for (table in table_names) {
      DBI::dbExecute(con, paste0(
        "ALTER TABLE ", .clinical_relation_sql(con, schema,
                                                stage_names[[table]]),
        " RENAME TO ", DBI::dbQuoteIdentifier(con, table)))
    }
    if (identical(inject_failure, "after_promote")) {
      stop("Injected publication failure after promotion.",
           call. = FALSE)
    }

    for (table in table_names[existing]) {
      DBI::dbExecute(con, paste0(
        "DROP TABLE ", .clinical_relation_sql(con, schema,
                                               backup_names[[table]])))
    }

    for (table in intersect(names(indexes), table_names)) {
      for (index in indexes[[table]]) {
        create_index(con, schema, table, index$name, index$columns)
      }
    }
    if (identical(inject_failure, "after_indexes")) {
      stop("Injected publication failure after index creation.",
           call. = FALSE)
    }

    DBI::dbCommit(con)
    tx_open <- FALSE
  }, error = function(e) {
    if (tx_open && DBI::dbIsValid(con)) DBI::dbRollback(con)
    stop(conditionMessage(e), call. = FALSE)
  })

  invisible(tables)
}

.clinical_publish_table <- function(con, schema, table, df,
                                    required_columns = character(),
                                    nonmissing_columns = character(),
                                    unique_columns = character(),
                                    indexes = list(), inject_failure = NULL) {
  tables <- stats::setNames(list(df), table)
  specs <- stats::setNames(list(list(
    required_columns = required_columns,
    nonmissing_columns = nonmissing_columns,
    unique_columns = unique_columns
  )), table)
  index_set <- stats::setNames(list(indexes), table)
  .clinical_publish_tables(con, schema, tables, specs, index_set,
                           inject_failure = inject_failure)
  invisible(df)
}

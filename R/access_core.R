# Data access layer for the rendehR package.
#
# Two complementary interfaces:
#
# 1. Interactive / exploratory - browse_source() returns an environment with
#    active bindings so you can do src$admissions, src$heart_rate, etc.
#    The connection is persistent and closed with src$disconnect().
#
# 2. Programmatic - fetch_table(), fetch_concept(), fetch_concepts() open a
#    fresh connection per call and close it automatically on return.

#
# ___________________________________________________________________________
# Package-level connection store  (Option D)
# ___________________________________________________________________________

.ehr_env <- new.env(parent = emptyenv())

#' Store database credentials for use by all fetch_* functions
#'
#' Only needs to be called once per session. After this, all fetch_*
#' functions open and close their own connections automatically.
#'
#' Credentials can be passed directly with `creds`, loaded from a JSON file
#' with `config_path`, or resolved by `load_credentials()` from environment
#' variables or interactive prompts. If `ehr_use_config()` has loaded a project
#' config, `db_name` and the credentials path can come from that config.
#'
#' @param db_name Optional Postgres database name (e.g. "rendehr"). If NULL,
#'   uses the active project config.
#' @param creds Optional list from `load_credentials()`. If NULL,
#'   credentials are loaded with `load_credentials(config_path)`.
#' @param config_path Optional path to a JSON credentials file, usually
#'   "config/config_pass.json". Ignored when `creds` is supplied. If NULL,
#'   uses the active project config credentials path when available.
#' @return `NULL`, invisibly. The credentials are kept for the rest of the session.
#' @export
#'
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#' ehr_connect("rendehr", creds)
#'
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#'
#' ehr_use_config("config/ehr_config.json")
#' ehr_connect()
#' }
ehr_connect <- function(db_name = NULL, creds = NULL, config_path = NULL) {
  cfg <- .ehr_env$config %||% list()
  cfg_database <- cfg$database %||% list()

  db_name <- db_name %||% cfg_database$db_name %||% "rendehr"

  if (is.null(config_path)) {
    config_path <- cfg_database$credentials_path
    base_dir <- cfg$project$base_dir %||% getwd()
    config_path <- .ehr_resolve_path(config_path, base_dir)
  }
  if (is.null(creds))
    creds <- load_credentials(config_path)
  creds <- .ehr_normalize_creds(creds)

  .ehr_env$db_name <- db_name
  .ehr_env$creds   <- creds
  message("rendehR: credentials stored for '", db_name, "'.")
  invisible(NULL)
}

#' Clear stored credentials
#' @return `NULL`, invisibly.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' ehr_disconnect()
#' }
#' @export
ehr_disconnect <- function() {
  .ehr_env$db_name <- NULL
  .ehr_env$creds   <- NULL
  .ehr_env$credential_source <- NULL
  message("rendehR: credentials cleared.")
  invisible(NULL)
}

# Internal: open connection per call, auto-close on return
.with_connection <- function(con, fn) {
  if (!is.null(con)) return(fn(con))
  if (is.null(.ehr_env$creds))
    stop('No credentials stored. Run ehr_connect(db_name, config_path = "config/config_pass.json") first.')
  con <- connect_postgres(.ehr_env$db_name, .ehr_env$creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fn(con)
}

.ehr_load_config_for_browse <- function(config_path = NULL) {
  if (!is.null(config_path)) {
    cfg <- ehr_load_config(config_path)
    .ehr_env$config <- cfg
    return(cfg)
  }
  if (!is.null(.ehr_env$config)) return(.ehr_env$config)

  default_path <- .ehr_default_config_path(NULL)
  if (file.exists(default_path)) {
    cfg <- ehr_load_config(default_path)
    .ehr_env$config <- cfg
    return(cfg)
  }
  NULL
}

.ehr_browser_connection <- function(con = NULL, db_name = NULL,
                                    config_path = NULL) {
  if (!is.null(con)) {
    return(list(
      con = con,
      db_name = db_name %||% .ehr_env$db_name %||% .ehr_resolve_db_name(NULL),
      owns_connection = FALSE
    ))
  }

  .ehr_load_config_for_browse(config_path)
  db_name <- .ehr_resolve_db_name(db_name)
  creds <- load_credentials(NULL)

  .ehr_env$db_name <- db_name
  .ehr_env$creds <- creds

  list(
    con = connect_postgres(db_name, creds),
    db_name = db_name,
    owns_connection = TRUE
  )
}

# Resolve a schema key/alias to its actual schema name, then validate it as a
# portable SQL identifier before any caller interpolates it into executed SQL.
# .ehr_schema_name() intentionally passes an unrecognized `schema` argument
# through unchanged, so every call site that resolves a schema name must
# validate it itself.
.ehr_validated_schema_name <- function(schema, schema_names) {
  .ehr_validate_db_identifier(.ehr_schema_name(schema, schema_names), "schema name")
}

# ___________________________________________________________________________
# browse_source - $ access with persistent connection
# ___________________________________________________________________________

#' Browse one source schema with interactive $ access
#'
#' Returns an environment where every table in the schema is an active
#' binding. Accessing a name returns a lazy `dplyr::tbl` - no data is
#' transferred until you call `collect()`.
#'
#' The connection is kept open for the lifetime of the object.
#' Call `src$disconnect()` when finished if the browser opened the connection.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param schema Which schema to attach: "raw", "concepts", "derived", "harmonized".
#'   Also accepts a literal schema name string.
#' @param con Optional explicit DBI connection. If NULL, opens a connection
#'   using `db_name`, the active/default project config, and credentials.
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then the default config file, then "rendehr".
#' @param config_path Optional project config path. If NULL and no config is
#'   active, uses `EHR_CONFIG` or `config/ehr_config.json` when present.
#' @return An environment of class `ehr_source` with one active binding
#'   per table and a `$disconnect()` method.
#' @export
#'
#' @examples
#' \dontrun{
#' raw  <- browse_source("mimic_iv_demo", "raw",
#'                       config_path = "config/ehr_config.json")
#' raw$admissions                          # lazy tbl
#' raw$admissions |> dplyr::collect()     # pull into R
#' raw$icu_windows |> dplyr::glimpse()
#' raw$disconnect()
#'
#' cpts <- browse_source("mimic_iv_demo", "concepts")
#' cpts$heart_rate |> dplyr::collect()
#' cpts$disconnect()
#' }
browse_source <- function(src_name, schema = "raw", con = NULL,
                          db_name = NULL, config_path = NULL) {
  schema_names <- src_schema_names(src_name)
  schema_name  <- .ehr_validated_schema_name(schema, schema_names)

  conn <- .ehr_browser_connection(con, db_name, config_path)
  con <- conn$con

  tables <- DBI::dbGetQuery(con,
    "SELECT table_name FROM information_schema.tables
     WHERE table_schema = $1 ORDER BY table_name",
    params = list(schema_name)
  )$table_name

  ds <- new.env(parent = emptyenv())

  for (tbl in tables) {
    makeActiveBinding(
      tbl,
      local({
        tbl_name    <- tbl
        active_con  <- con
        active_sch  <- schema_name
        function() dplyr::tbl(active_con,
                              DBI::Id(schema = active_sch, table = tbl_name))
      }),
      ds
    )
  }

  ds$.src_name   <- src_name
  ds$.schema     <- schema_name
  ds$.tables     <- tables
  ds$.db_name    <- conn$db_name
  ds$.owns_connection <- conn$owns_connection

  ds$disconnect <- function() {
    if (!isTRUE(conn$owns_connection)) {
      message("Connection was supplied by caller; not disconnected.")
      return(invisible(FALSE))
    }
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
      message("Disconnected: ", schema_name)
    }
    invisible(TRUE)
  }

  ds$tables <- function() tables

  class(ds) <- "ehr_source"
  ds
}

#' @rdname browse_source
#' @export
open_source <- function(src_name, schema = "raw", con = NULL,
                        db_name = NULL, config_path = NULL) {
  browse_source(src_name = src_name, schema = schema, con = con,
                db_name = db_name, config_path = config_path)
}

#' Print a lazy source browser
#'
#' Summarizes the source name, schema, database, and the number and names of
#' the lazy `dplyr::tbl` tables available on the object.
#'
#' @param x An `ehr_source` object, as returned by `browse_source()` or
#'   `open_source()`.
#' @param ... Ignored.
#' @return Invisibly, `x`.
#' @export
print.ehr_source <- function(x, ...) {
  cat("<ehr_source>\n")
  cat("  source :", x$.src_name, "\n")
  cat("  schema :", x$.schema,   "\n")
  cat("  db     :", x$.db_name,  "\n")
  cat("  tables :", length(x$.tables), "-",
      paste(head(x$.tables, 6), collapse = ", "),
      if (length(x$.tables) > 6) "..." else "", "\n")
  invisible(x)
}


#' Browse all EHR database tables with flexible access
#'
#' Experimental flexible browser. Returns lazy database tables either nested by
#' source and layer, flattened by `schema.table`, or both. No rows are loaded
#' until the user calls `collect()` or similar.
#'
#' @param con Optional explicit DBI connection. If NULL, opens a connection
#'   using `db_name`, the active/default project config, and credentials.
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then the default config file, then "rendehr".
#' @param config_path Optional project config path. If NULL and no config is
#'   active, uses `EHR_CONFIG` or `config/ehr_config.json` when present.
#' @param include_system Include PostgreSQL system schemas? Default FALSE.
#' @param table_type Object types to include: `"both"` (default) includes base
#'   tables and views, `"base"` includes only base tables, and `"view"` includes
#'   only views.
#' @param view Return shape: `"source"` returns a source/layer/table nested
#'   list, `"flattened"` returns one list named by `schema.table`, and `"both"`
#'   returns both as `sources` and `flattened`.
#' @return A lazy table browser of class `ehr_db_flex_browser`.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- browse_ehr_db_flex(config_path = "config/ehr_config.json")
#' db$mimic_iv_demo$harmonized$ts_tab
#' db$eicu_demo$concepts$heart_rate
#' db$merged$rendehr$ts          # merge targets grouped under `merged`
#'
#' db2 <- browse_ehr_db_flex(view = "flattened")
#' db2[["mimic_iv_demo_coh_harmonized.ts_tab"]]
#' disconnect_ehr_db(db)
#' disconnect_ehr_db(db2)
#' }
browse_ehr_db_flex <- function(con = NULL, db_name = NULL,
                               config_path = NULL,
                               include_system = FALSE,
                               table_type = c("both", "base", "view"),
                               view = c("source", "flattened", "both")) {
  table_type <- match.arg(table_type)
  view <- match.arg(view)
  conn <- .ehr_browser_connection(con, db_name, config_path)
  con <- conn$con
  owns_connection <- conn$owns_connection

  where_system <- if (isTRUE(include_system)) {
    ""
  } else {
    "AND table_schema NOT IN ('pg_catalog', 'information_schema')
       AND table_schema NOT LIKE 'pg_toast%'"
  }

  table_type_sql <- switch(
    table_type,
    both = "table_type IN ('BASE TABLE', 'VIEW')",
    base = "table_type = 'BASE TABLE'",
    view = "table_type = 'VIEW'"
  )

  tables <- DBI::dbGetQuery(con, sprintf(
    "SELECT table_schema, table_name, table_type
     FROM information_schema.tables
     WHERE %s
       %s
     ORDER BY table_schema, table_name",
    table_type_sql, where_system
  ))


  schema_filter <- if (isTRUE(include_system)) {
    ""
  } else {
    "WHERE schema_name NOT IN ('pg_catalog', 'information_schema')
       AND schema_name NOT LIKE 'pg_toast%'"
  }
  schemas <- DBI::dbGetQuery(con, sprintf(
    "SELECT schema_name
     FROM information_schema.schemata
     %s
     ORDER BY schema_name",
    schema_filter
  ))

  keys <- paste(tables$table_schema, tables$table_name, sep = ".")
  flattened <- stats::setNames(
    lapply(seq_len(nrow(tables)), function(i) {
      dplyr::tbl(
        con,
        DBI::Id(schema = tables$table_schema[[i]], table = tables$table_name[[i]])
      )
    }),
    keys
  )

  # Merge targets (e.g. "rendehr") carry a merge-output table (ts/flat/
  # cohort_info) and are not *_coh* source schemas. Same signature that
  # drop_merged_schema() allowlists. The *_coh* branches in parse_schema() run
  # first, so a source schema's cohort_info never reaches the "merged" branch.
  merged_schemas <- unique(tables$table_schema[
    tables$table_name %in% c("ts", "flat", "cohort_info")])

  parse_schema <- function(schema) {
    standard <- regexec("^(.*)_coh(?:_(concepts|derived|harmonized))?$",
                        schema, perl = TRUE)
    hit <- regmatches(schema, standard)[[1]]
    if (length(hit) > 0L) {
      layer <- if (length(hit) >= 3L && nzchar(hit[[3]])) hit[[3]] else "raw"
      return(list(source = hit[[2]], layer = layer, standard = TRUE))
    }

    external <- regexec("^(.*)_coh_ext_(.+)$", schema, perl = TRUE)
    hit <- regmatches(schema, external)[[1]]
    if (length(hit) > 0L) {
      return(list(source = hit[[2]], layer = paste0("ext_", hit[[3]]),
                  standard = FALSE))
    }

    # Group all merge targets under a single "merged" source, keeping the
    # uniform source -> layer -> table shape (layer = schema name).
    if (schema %in% merged_schemas) {
      return(list(source = "merged", layer = schema, standard = FALSE))
    }

    list(source = "other", layer = schema, standard = FALSE)
  }

  empty_layers <- function() {
    list(raw = list(), concepts = list(), derived = list(), harmonized = list())
  }

  sources <- list()
  for (schema in schemas$schema_name) {
    parsed <- parse_schema(schema)
    src <- parsed$source
    if (isTRUE(parsed$standard)) {
      if (is.null(sources[[src]])) sources[[src]] <- empty_layers()
      else {
        missing_layers <- setdiff(names(empty_layers()), names(sources[[src]]))
        for (missing_layer in missing_layers) sources[[src]][[missing_layer]] <- list()
      }
    }
  }

  for (i in seq_len(nrow(tables))) {
    parsed <- parse_schema(tables$table_schema[[i]])
    src <- parsed$source
    layer <- parsed$layer
    tbl <- tables$table_name[[i]]
    obj <- flattened[[keys[[i]]]]

    if (is.null(sources[[src]])) {
      sources[[src]] <- if (isTRUE(parsed$standard)) empty_layers() else list()
    } else if (isTRUE(parsed$standard)) {
      missing_layers <- setdiff(names(empty_layers()), names(sources[[src]]))
      for (missing_layer in missing_layers) sources[[src]][[missing_layer]] <- list()
    }
    if (is.null(sources[[src]][[layer]])) sources[[src]][[layer]] <- list()
    sources[[src]][[layer]][[tbl]] <- obj
  }

  out <- switch(
    view,
    source = sources,
    flattened = flattened,
    both = list(sources = sources, flattened = flattened, tables = tables)
  )
  attr(out, "tables") <- tables
  attr(out, "connection") <- con
  attr(out, "owns_connection") <- owns_connection
  attr(out, "db_name") <- conn$db_name
  attr(out, "browser_view") <- view
  attr(out, "table_type") <- table_type
  class(out) <- c("ehr_db_flex_browser", class(out))
  out
}

#' Print a flexible database browser
#'
#' Summarizes the database name, browser view (`"source"`, `"flattened"`,
#' or `"both"`), table type, and table count, followed by a reminder of the
#' `$`/`[[` access pattern for the active view.
#'
#' @param x An `ehr_db_flex_browser` object, as returned by
#'   `browse_ehr_db_flex()`.
#' @param ... Ignored.
#' @return Invisibly, `x`.
#' @export
print.ehr_db_flex_browser <- function(x, ...) {
  view <- attr(x, "browser_view") %||% "source"
  tables <- attr(x, "tables")
  n_tables <- if (is.null(tables)) NA_integer_ else nrow(tables)

  cat("<ehr_db_flex_browser>\n")
  cat("  db     :", attr(x, "db_name"), "\n")
  cat("  view   :", view, "\n")
  cat("  type   :", attr(x, "table_type") %||% "both", "\n")
  cat("  tables :", n_tables, "\n")

  if (identical(view, "source")) {
    cat("  sources:", paste(names(x), collapse = ", "), "\n")
    cat("  access : db$mimic_iv_demo$harmonized$ts_tab\n")
    if ("merged" %in% names(x))
      cat("           db$merged$<schema>$<table>   (merged schemas)\n")
  } else if (identical(view, "flattened")) {
    cat("  access : db[[\"schema.table\"]]\n")
  } else {
    cat("  sources:", paste(names(x$sources), collapse = ", "), "\n")
    cat("  access : db$sources$source$layer$table\n")
    cat("           db$flattened[[\"schema.table\"]]\n")
  }
  cat("  note   : lazy tables; use dplyr::filter()/head() before collect() for large tables.\n")
  invisible(x)
}

#' Disconnect a database browser returned by `browse_ehr_db_flex()`
#'
#' @param x Object returned by `browse_ehr_db_flex()`.
#' @return Invisibly returns NULL.
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#' con <- connect_postgres("rendehr", creds)
#' disconnect_ehr_db(con)
#' }
#' @export
disconnect_ehr_db <- function(x) {
  if (!inherits(x, "ehr_db_flex_browser")) {
    stop("x must be returned by browse_ehr_db_flex().", call. = FALSE)
  }
  if (!isTRUE(attr(x, "owns_connection"))) {
    message("Connection was supplied by caller; not disconnected.")
    return(invisible(FALSE))
  }
  con <- attr(x, "connection")
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
    message("Disconnected from EHR database.")
  }
  invisible(TRUE)
}


# ___________________________________________________________________________
# fetch_table - general single-table loader (auto-close)
# ___________________________________________________________________________

#' Fetch a table from any source schema into a tibble
#'
#' Opens a connection, collects the table, closes the connection.
#' Use `schema` to target raw tables, concept tables, or harmonized outputs.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param table Table name.
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string.
#' @param n Max rows to return. Default Inf (all rows).
#' @param con Optional explicit DBI connection (skips auto-open/close).
#' @return A tibble.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' patients <- fetch_table("mimic_iv_demo", "patients")
#' ts <- fetch_table("mimic_iv_demo", "ts_tab", schema = "harmonized", n = 1000)
#' }
#' @export
fetch_table <- function(src_name, table, schema = "raw", n = Inf,
                        con = NULL) {
  schema_names <- src_schema_names(src_name)
  schema_name  <- .ehr_validated_schema_name(schema, schema_names)
  table        <- .ehr_validate_db_identifier(table, "table name")

  .with_connection(con, function(con) {
    if (!grepl("_mod$", table)) {
      mod_exists <- nrow(DBI::dbGetQuery(con,
        "SELECT 1 FROM information_schema.tables
         WHERE table_schema = $1 AND table_name = $2",
        params = list(schema_name, paste0(table, "_mod"))
      )) > 0
      if (mod_exists)
        message("Note: '", table, "_mod' exists in '", schema_name,
                "' - consider using it for preprocessed data.")
    }

    tbl_cols  <- DBI::dbListFields(con, DBI::Id(schema = schema_name, table = table))
    cohort    <- .get_active_ids(src_name)
    tw        <- .get_time_window(src_name)

    where_parts <- character(0)

    if (!is.null(cohort)) {
      ids    <- .get_cohort_ids(cohort)
      id_col <- .ehr_validate_db_identifier(.get_cohort_id_col(cohort), "id column name")
      if (id_col %in% tbl_cols) {
        .setup_cohort_temp(con, ids, .get_cohort_version(cohort))
        where_parts <- c(where_parts,
          .cohort_filter_join(con, schema_name, table, id_col))
      }
    }

    time_where <- character(0)
    if (!is.null(tw) && tw$ref == "icu" && "time_since_icu_min" %in% tbl_cols) {
      if (tw$clip)
        time_where <- "t.time_since_icu_min BETWEEN 0 AND t.icu_los_min"
    }

    if (length(where_parts) > 0 || length(time_where) > 0) {
      join_sql  <- if (length(where_parts) > 0) paste(where_parts, collapse = "\n") else ""
      where_sql <- if (length(time_where) > 0) paste("WHERE", time_where) else ""
      tbl_q <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema_name, table = table))
      q <- sprintf('SELECT t.* FROM %s t %s %s',
                   tbl_q, join_sql, where_sql)
      if (is.finite(n)) q <- paste0(q, " LIMIT ", as.integer(n))
      return(DBI::dbGetQuery(con, q))
    }

    get_table_from_db(con, schema_name, table, n = n)
  })
}

# ___________________________________________________________________________
# fetch_concept - single concept (auto-close)
# ___________________________________________________________________________

#' Fetch one atomic concept table into a tibble
#'
#' Convenience wrapper around fetch_table() that always targets the
#' concepts schema. The concept table is created by run_concepts().
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param concept Concept name (e.g. "heart_rate", "spo2").
#' @param n Max rows to return. Default Inf.
#' @param con Optional explicit DBI connection.
#' @return A tibble with columns: id cols, time_since_hadm_min, value,
#'   unit, source_table, source_dataset, etc.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' hr <- fetch_concept("mimic_iv_demo", "heart_rate")
#' }
#' @export
fetch_concept <- function(src_name, concept, n = Inf, con = NULL) {
  fetch_table(src_name, concept, schema = "concepts", n = n, con = con)
}

# ___________________________________________________________________________
# fetch_concepts - multiple concepts, long format (single connection)
# ___________________________________________________________________________

#' Fetch multiple concept tables and stack them into long format
#'
#' Opens one connection, loads all requested concepts, closes the
#' connection. The returned tibble has a `concept_name` column so you
#' can distinguish concepts after stacking.
#'
#' Columns that differ across concepts (e.g. source_itemid, unit_raw)
#' are coerced to character before binding so the union is always safe.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param concepts Character vector of concept names.
#' @param n Max rows per concept. Default Inf.
#' @param con Optional explicit DBI connection.
#' @return A long tibble with an added `concept_name` column.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' vitals <- fetch_concepts("mimic_iv_demo", c("heart_rate", "sbp", "spo2"))
#' }
#' @export
fetch_concepts <- function(src_name, concepts, n = Inf, con = NULL) {
  schema_names <- src_schema_names(src_name)

  .with_connection(con, function(con) {
    available <- DBI::dbGetQuery(con, sprintf(
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = '%s'",
      schema_names$concepts
    ))$table_name

    missing <- setdiff(concepts, available)
    if (length(missing) > 0)
      warning("Concepts not found (skipped): ",
              paste(missing, collapse = ", "))

    valid <- intersect(concepts, available)
    if (length(valid) == 0) stop("No valid concepts to fetch.")

    # Delegate per-concept loading to fetch_table() so the active cohort AND
    # active time-window filters are applied identically to fetch_concept().
    # Single source of truth for filter logic - no hand-rolled JOIN SQL here.
    dfs <- lapply(valid, function(cn) {
      df <- fetch_table(src_name, cn, schema = "concepts", n = n, con = con)
      df$concept_name <- cn
      for (v in c("value_raw", "unit_raw", "source_itemid", "source_label"))
        if (v %in% names(df)) df[[v]] <- as.character(df[[v]])
      df
    })

    dplyr::bind_rows(dfs)
  })
}

# ___________________________________________________________________________
# list_concepts - discover what's available
# ___________________________________________________________________________

#' List all concept tables available for a source
#'
#' Returns concepts from the atomic concepts schema (_coh_concepts), the
#' derived schema (_coh_derived), and any named external schemas
#' (_coh_ext_*). Joins against the concept_registry table (if it exists) to
#' add origin_name, origin_url, and origin_ref provenance columns.
#'
#' @param src_name Source name.
#' @param con Optional explicit DBI connection.
#' @return A data frame with columns concept_name, schema, origin_name,
#'   origin_url, origin_ref. Sorted alphabetically by concept name.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' head(list_concepts("mimic_iv_demo"))
#' }
#' @export
list_concepts <- function(src_name, con = NULL) {
  src_name <- .ehr_validate_db_identifier(src_name, "source name")
  schema_names <- src_schema_names(src_name)

  .with_connection(con, function(con) {
    all_schemas <- DBI::dbGetQuery(con,
      "SELECT schema_name FROM information_schema.schemata"
    )$schema_name

    .tables_in <- function(schema) {
      if (!schema %in% all_schemas) return(character(0))
      DBI::dbGetQuery(con, sprintf(
        "SELECT table_name FROM information_schema.tables
         WHERE table_schema = '%s' AND table_name != 'concept_registry'
         ORDER BY table_name", schema
      ))$table_name
    }

    # Standard schemas
    atomic  <- .tables_in(schema_names$concepts)

    derived_schema <- schema_names$derived
    derived <- .tables_in(derived_schema)

    # Named external schemas: {src_name}_coh_ext_*
    ext_prefix  <- paste0(src_name, "_coh_ext_")
    ext_schemas <- all_schemas[startsWith(all_schemas, ext_prefix)]
    ext_rows <- do.call(rbind, lapply(ext_schemas, function(s) {
      tbls <- .tables_in(s)
      if (length(tbls) == 0) return(NULL)
      data.frame(concept_name = tbls,
                 schema       = sub(ext_prefix, "", s),  # short label
                 schema_full  = s,
                 stringsAsFactors = FALSE)
    }))

    result <- rbind(
      if (length(atomic)  > 0) data.frame(concept_name = atomic,  schema = "concepts",
                                           schema_full = schema_names$concepts,  stringsAsFactors = FALSE),
      if (length(derived) > 0) data.frame(concept_name = derived, schema = "derived",
                                           schema_full = derived_schema,          stringsAsFactors = FALSE),
      ext_rows
    )

    if (is.null(result) || nrow(result) == 0) {
      message("No concepts found for '", src_name, "'.")
      message("  Run run_concepts() to build atomic concepts.")
      message("  Run run_external_concepts() to build derived/external concepts.")
      return(invisible(data.frame(concept_name = character(), schema = character(),
                                   origin_name = character(), origin_url = character(),
                                   origin_ref  = character())))
    }

    # Join concept_registry for provenance if it exists
    reg_exists <- "concept_registry" %in% DBI::dbGetQuery(con, sprintf(
      "SELECT table_name FROM information_schema.tables WHERE table_schema = '%s'",
      schema_names$raw
    ))$table_name

    if (reg_exists) {
      registry <- DBI::dbGetQuery(con, sprintf(
        "SELECT concept_name, schema_name, origin_name, origin_url, origin_ref
         FROM %s.concept_registry", schema_names$raw
      ))
      result <- merge(result, registry,
                      by.x = c("concept_name", "schema_full"),
                      by.y = c("concept_name", "schema_name"),
                      all.x = TRUE)
    } else {
      result$origin_name <- NA_character_
      result$origin_url  <- NA_character_
      result$origin_ref  <- NA_character_
    }

    result$schema_full <- NULL

    # Derive layer label from schema + origin_name for user-facing clarity.
    # For derived-schema concepts with a known MIT-LCP origin, we use the
    # specific layer key.  Everything else in derived that has a registered
    # origin (e.g. an external module folder name) uses that origin directly
    # so the caller can see which module produced it.
    result$layer <- mapply(function(schema, origin) {
      if (schema == "derived") {
        if (!is.na(origin) && grepl("MIT-LCP", origin, fixed = TRUE) &&
            grepl("adapted", origin, fixed = TRUE))
          "ehr"
        else if (!is.na(origin) && grepl("MIT-LCP", origin, fixed = TRUE))
          "mimic_code"
        else if (!is.na(origin) && nzchar(origin))
          origin          # folder/module name for external or user-path concepts
        else
          "derived"
      } else if (schema == "concepts") {
        if (!is.na(origin) && origin == "user")
          "user"
        else
          "package"   # includes unregistered package concepts (origin = NA)
      } else {
        "external"
      }
    }, result$schema,
    if (is.null(result$origin_name)) rep(NA_character_, nrow(result))
    else result$origin_name,
    SIMPLIFY = TRUE, USE.NAMES = FALSE)

    result <- result[order(result$concept_name), ]
    rownames(result) <- NULL

    known_layers <- c("package", "mimic_code", "ehr", "user", "derived", "external")
    n_pkg  <- sum(result$layer == "package",                na.rm = TRUE)
    n_repo <- sum(result$layer %in% c("mimic_code", "ehr"), na.rm = TRUE)
    n_user <- sum(result$layer == "user",                      na.rm = TRUE)
    n_drv  <- sum(result$layer == "derived",                   na.rm = TRUE)
    ext_layers <- unique(result$layer[!result$layer %in% known_layers])
    n_ext  <- sum(!result$layer %in% known_layers,             na.rm = TRUE)
    ext_msg <- if (length(ext_layers) > 0L)
      paste0("  (modules: ", paste(ext_layers, collapse = ", "), ")")
    else ""
    drv_msg <- if (n_drv > 0L)
      paste0("  |  ", n_drv, " derived (unregistered)")
    else ""
    message(sprintf(
      "Concepts for '%s':  %d package  |  %d repo-derived  |  %d user  |  %d external%s%s",
      src_name, n_pkg, n_repo, n_user, n_ext, ext_msg, drv_msg
    ))

    dupes <- intersect(atomic, derived)
    if (length(dupes) > 0)
      message("  Note: ", length(dupes), " concept(s) in both concepts + derived ",
              "(derived takes precedence): ", paste(dupes, collapse = ", "))

    result
  })
}

# ___________________________________________________________________________
# write_table - push a data frame or CSV into any source schema
# ___________________________________________________________________________

#' Write a data frame to a source schema
#'
#' Pushes a tibble or data frame into any schema associated with a source.
#' Opens a connection, writes the table, closes the connection.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param df Data frame or tibble to write.
#' @param table_name Name to give the table in Postgres.
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string.
#' @param overwrite Drop and recreate the table if it already exists.
#' @param append Append rows to an existing table. Overrides overwrite.
#' @param con Optional explicit DBI connection.
#' @return The number of rows written, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", creds)
#'
#' write_table("mimic_iv_demo", my_annotations, "annotations", schema = "raw")
#' write_table("mimic_iv_demo", new_rows, "annotations", schema = "raw",
#'             append = TRUE)
#' }
write_table <- function(src_name, df, table_name, schema = "raw",
                        overwrite = TRUE, append = FALSE, con = NULL) {
  schema_names <- src_schema_names(src_name)
  schema_name  <- .ehr_schema_name(schema, schema_names)

  .with_connection(con, function(con) {
    tbl_id <- DBI::Id(schema = schema_name, table = table_name)

    if (append) {
      DBI::dbWriteTable(con, tbl_id, df, append = TRUE, row.names = FALSE)
    } else {
      if (overwrite && DBI::dbExistsTable(con, tbl_id))
        DBI::dbExecute(con, paste0(
          "DROP TABLE ", DBI::dbQuoteIdentifier(con, tbl_id), " CASCADE"
        ))
      DBI::dbWriteTable(con, tbl_id, df, append = FALSE, row.names = FALSE)
    }

    n <- nrow(df)
    message("Written ", format(n, big.mark = ","), " rows -> ",
            schema_name, ".", table_name)
    invisible(n)
  })
}

#' Write a CSV file directly to a source schema
#'
#' Reads the CSV into R then calls write_table(). Useful for pushing
#' annotation files or external lookups without a separate import step.
#'
#' @param src_name Source name.
#' @param csv_path Path to the CSV file.
#' @param table_name Name to give the table in Postgres. Defaults to the
#'   filename without extension.
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string.
#' @param overwrite Drop and recreate if table already exists.
#' @param append Append rows to existing table.
#' @param con Optional explicit DBI connection.
#' @return The number of rows written, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' write_csv_to_schema("mimic_iv_demo", "data/my_labels.csv",
#'                     schema = "raw")
#' }
write_csv_to_schema <- function(src_name, csv_path, table_name = NULL,
                                schema = "raw", overwrite = TRUE,
                                append = FALSE, con = NULL) {
  if (!file.exists(csv_path))
    stop("File not found: ", csv_path)

  if (is.null(table_name))
    table_name <- tools::file_path_sans_ext(basename(csv_path))

  df <- readr::read_csv(csv_path, show_col_types = FALSE)
  write_table(src_name, df, table_name, schema = schema,
              overwrite = overwrite, append = append, con = con)
}

# ___________________________________________________________________________
# Safe removal - dry_run = TRUE by default, whitelisted schema names only
# ___________________________________________________________________________

# Guard: only allow dropping schemas that follow the rendehR naming convention.
# This prevents accidentally dropping system schemas or unrelated schemas.
.assert_ehr_schema <- function(schema_name) {
  if (!grepl("_coh(_concepts|_derived|_harmonized)?$", schema_name))
    stop(
      "'", schema_name, "' does not look like an rendehR schema.\n",
      "Only schemas matching *_coh, *_coh_concepts, *_coh_derived, or *_coh_harmonized ",
      "can be dropped via this function."
    )
}

#' Safely drop a single table from a source schema
#'
#' Dry-run by default - prints what would be dropped without doing anything.
#' Set `dry_run = FALSE` to execute.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param table Table name to drop.
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string.
#' @param dry_run If TRUE (default), only print what would happen.
#' @param con Optional explicit DBI connection.
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # preview first
#' drop_table("mimic_iv_demo", "annotations", schema = "raw")
#'
#' # actually drop
#' drop_table("mimic_iv_demo", "annotations", schema = "raw", dry_run = FALSE)
#' }
drop_table <- function(src_name, table, schema = "raw",
                       dry_run = TRUE, con = NULL) {
  schema_names <- src_schema_names(src_name)
  schema_name  <- .ehr_schema_name(schema, schema_names)

  .assert_ehr_schema(schema_name)

  if (dry_run) {
    message("[DRY RUN] Would drop table: ", schema_name, ".", table)
    message("  Re-run with dry_run = FALSE to execute.")
    return(invisible(NULL))
  }

  .with_connection(con, function(con) {
    tbl_id <- DBI::Id(schema = schema_name, table = table)
    if (!DBI::dbExistsTable(con, tbl_id)) {
      message("Table does not exist, nothing to drop: ", schema_name, ".", table)
      return(invisible(NULL))
    }
    DBI::dbExecute(con, paste0(
      "DROP TABLE ", DBI::dbQuoteIdentifier(con, tbl_id), " CASCADE"
    ))
    message("Dropped: ", schema_name, ".", table)
    invisible(NULL)
  })
}

#' Safely drop an entire schema for a source
#'
#' Lists all tables in the schema first, then drops the schema if confirmed.
#' Dry-run by default.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string.
#' @param dry_run If TRUE (default), only print what would happen.
#' @param con Optional explicit DBI connection.
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # preview - shows all tables that would be deleted
#' drop_schema("mimic_iv_demo", "concepts")
#'
#' # actually drop
#' drop_schema("mimic_iv_demo", "concepts", dry_run = FALSE)
#' }
drop_schema <- function(src_name, schema = "concepts",
                        dry_run = TRUE, con = NULL) {
  schema_names <- src_schema_names(src_name)
  schema_name  <- .ehr_validated_schema_name(schema, schema_names)

  .assert_ehr_schema(schema_name)

  .with_connection(con, function(con) {
    tables <- DBI::dbGetQuery(con,
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = $1 ORDER BY table_name",
      params = list(schema_name)
    )$table_name

    if (dry_run) {
      message("[DRY RUN] Would drop schema: ", schema_name)
      if (length(tables) == 0) {
        message("  Schema is empty.")
      } else {
        message("  Contains ", length(tables), " table(s):")
        for (t in tables) message("    - ", t)
      }
      message("  Re-run with dry_run = FALSE to execute.")
      return(invisible(NULL))
    }

    DBI::dbExecute(con,
      paste0("DROP SCHEMA IF EXISTS ", DBI::dbQuoteIdentifier(con, schema_name), " CASCADE")
    )
    message("Dropped schema: ", schema_name,
            " (", length(tables), " tables removed)")
    invisible(NULL)
  })
}

#' Safely drop all schemas for a source
#'
#' Targets all rendehR schemas for a source: raw, concepts, ts, flat, dur, drv.
#' Dry-run by default - always preview before executing.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param dry_run If TRUE (default), only print what would happen.
#' @param con Optional explicit DBI connection.
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # preview everything that would be deleted
#' drop_source_schemas("mimic_iv_demo")
#'
#' # actually drop all schemas for this source
#' drop_source_schemas("mimic_iv_demo", dry_run = FALSE)
#' }
drop_source_schemas <- function(src_name, dry_run = TRUE, con = NULL) {
  schema_names <- src_schema_names(src_name)

  .with_connection(con, function(con) {
    existing <- DBI::dbGetQuery(con,
      "SELECT schema_name FROM information_schema.schemata"
    )$schema_name

    target_schemas <- unlist(schema_names)
    present        <- intersect(target_schemas, existing)
    absent         <- setdiff(target_schemas, existing)

    if (dry_run) {
      message("[DRY RUN] Source: ", src_name)
      message("  Schemas to drop (", length(present), "):")
      for (s in present) {
        n <- DBI::dbGetQuery(con,
          "SELECT COUNT(*) AS n FROM information_schema.tables
           WHERE table_schema = $1", params = list(s)
        )$n
        message("    - ", s, "  (", n, " tables)")
      }
      if (length(absent) > 0)
        message("  Already absent: ", paste(absent, collapse = ", "))
      message("  Re-run with dry_run = FALSE to execute.")
      return(invisible(NULL))
    }

    for (s in present) {
      s <- .ehr_validate_db_identifier(s, "schema name")
      .assert_ehr_schema(s)
      DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", DBI::dbQuoteIdentifier(con, s), " CASCADE"))
      message("Dropped: ", s)
    }
    message("Done. Removed ", length(present), " schema(s) for ", src_name)
    invisible(NULL)
  })
}

# Guard for drop_merged_schema(): the inverse of .assert_ehr_schema. Refuses
# source rendehR schemas (those belong to drop_source_schemas), system schemas, and
# anything that does not carry a merge-output table (ts/flat/cohort_info) - so a
# stray or mistyped schema name can't be dropped through this path.
.assert_merged_schema <- function(con, schema) {
  if (grepl("_coh(_concepts|_derived|_harmonized)?$", schema))
    stop("'", schema, "' is a source rendehR schema; use drop_source_schemas() instead.")
  if (schema %in% c("public", "information_schema") || grepl("^pg_", schema))
    stop("'", schema, "' is a system schema and cannot be dropped here.")
  has_merge_tbl <- DBI::dbGetQuery(con, sprintf(
    "SELECT 1 FROM information_schema.tables
     WHERE table_schema = '%s' AND table_name IN ('ts', 'flat', 'cohort_info')
     LIMIT 1", schema))
  if (nrow(has_merge_tbl) == 0)
    stop("'", schema, "' does not look like a merged schema ",
         "(no ts/flat/cohort_info table). Refusing to drop.")
}

#' Safely drop a merged schema
#'
#' Companion to `drop_schema()` for merge targets created by `merge_sources()`
#' (e.g. "rendehr", "trauma_project"). Those schemas are not `*_coh*` names, so
#' the `drop_schema()` guard refuses them; this function allowlists merge targets
#' instead - it confirms the schema carries a merge-output table
#' (ts/flat/cohort_info) and is not a source or system schema. Lists the tables
#' first; dry-run by default.
#'
#' @param schema Merged schema name passed to merge_sources().
#' @param dry_run If TRUE (default), only print what would happen.
#' @param con Optional explicit DBI connection.
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' drop_merged_schema("rendehr")                    # preview
#' drop_merged_schema("rendehr", dry_run = FALSE)   # actually drop
#' }
drop_merged_schema <- function(schema, dry_run = TRUE, con = NULL) {
  .with_connection(con, function(con) {
    .assert_merged_schema(con, schema)

    tables <- DBI::dbGetQuery(con, sprintf(
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = '%s' ORDER BY table_name",
      schema
    ))$table_name

    if (dry_run) {
      message("[DRY RUN] Would drop merged schema: ", schema)
      if (length(tables) == 0) {
        message("  Schema is empty.")
      } else {
        message("  Contains ", length(tables), " table(s):")
        for (t in tables) message("    - ", t)
      }
      message("  Re-run with dry_run = FALSE to execute.")
      return(invisible(NULL))
    }

    DBI::dbExecute(con, paste0(
      "DROP SCHEMA IF EXISTS ", DBI::dbQuoteIdentifier(con, schema), " CASCADE"
    ))
    message("Dropped merged schema: ", schema,
            " (", length(tables), " tables removed)")
    invisible(NULL)
  })
}

#' Drop a single relation (table, view, or materialized view) by name
#'
#' Unlike `drop_table()`/`drop_schema()`, this is **not** restricted to rendehR
#' `*_coh*` schemas, so it can remove objects in a merged schema (e.g.
#' `rendehr.ts`). It looks up the relation's actual type and issues the matching
#' `DROP ... CASCADE`, so it works whether the target is a table, view, or
#' materialized view. Dry-run by default.
#'
#' @param relation Relation name: either `"schema.table"`, or just `"table"`
#'   together with `schema`.
#' @param schema Optional schema name; prepended to `relation` when supplied.
#' @param dry_run If TRUE (default), only print what would happen.
#' @param con Optional explicit DBI connection.
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' drop_relation("rendehr.ts")                       # preview
#' drop_relation("ts", schema = "rendehr", dry_run = FALSE)
#' }
drop_relation <- function(relation, schema = NULL, dry_run = TRUE, con = NULL) {
  # Accept either a single "schema.relation" string or separate schema/
  # relation arguments; split the combined form before validating each part,
  # since .ehr_drop_relation() requires two separate portable identifiers,
  # not a combined dotted string.
  if (is.null(schema)) {
    parts <- strsplit(relation, ".", fixed = TRUE)[[1]]
    if (length(parts) != 2)
      stop("relation must be \"schema.relation\" when schema is not supplied.",
           call. = FALSE)
    schema   <- parts[1]
    relation <- parts[2]
  }
  schema   <- .ehr_validate_db_identifier(schema, "schema name")
  relation <- .ehr_validate_db_identifier(relation, "relation name")

  .with_connection(con, function(con) {
    lookup <- paste0(schema, ".", relation)

    relkind <- DBI::dbGetQuery(con,
      "SELECT relkind FROM pg_class WHERE oid = to_regclass($1)",
      params = list(lookup))$relkind

    if (length(relkind) == 0 || is.na(relkind)) {
      message("Relation does not exist, nothing to drop: ", lookup)
      return(invisible(NULL))
    }

    kind <- switch(relkind,
      r = "TABLE", p = "TABLE",
      v = "VIEW",  m = "MATERIALIZED VIEW",
      f = "FOREIGN TABLE",
      "TABLE")

    if (dry_run) {
      message("[DRY RUN] Would drop ", kind, ": ", lookup)
      message("  Re-run with dry_run = FALSE to execute.")
      return(invisible(NULL))
    }

    .ehr_drop_relation(con, schema, relation)
    message("Dropped ", kind, ": ", lookup)
    invisible(NULL)
  })
}

# ___________________________________________________________________________
# Discovery helpers
# ___________________________________________________________________________

#' List all tables in a source schema
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string.
#' @param con Optional explicit DBI connection.
#' @return Character vector of table names.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' list_tables("mimic_iv_demo")
#' list_tables("mimic_iv_demo", schema = "harmonized")
#' }
#' @export
list_tables <- function(src_name, schema = "raw", con = NULL) {
  schema_names <- src_schema_names(src_name)
  schema_name  <- .ehr_validated_schema_name(schema, schema_names)

  .with_connection(con, function(con) {
    DBI::dbGetQuery(con,
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = $1 ORDER BY table_name",
      params = list(schema_name)
    )$table_name
  })
}

#' List all rendehR sources present in the database
#'
#' Scans `information_schema.schemata` for schemas matching the `*_coh`
#' pattern and strips the suffix to recover the source names.
#'
#' @param con Optional explicit DBI connection.
#' @return Character vector of source names (e.g. "mimic_iv_demo", "eicu").
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' list_sources()
#' }
#' @export
list_sources <- function(con = NULL) {
  .with_connection(con, function(con) {
    schemas <- DBI::dbGetQuery(con,
      "SELECT schema_name FROM information_schema.schemata
       WHERE schema_name ~ '^.+_coh$' ORDER BY schema_name"
    )$schema_name
    sub("_coh$", "", schemas)
  })
}

#' Preview the first N rows of any table
#'
#' Quick look - like `head()` but reads from Postgres. Does not load the full
#' table. Prints a summary and returns the rows invisibly.
#'
#' @param src_name Source name.
#' @param table Table name.
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string.
#' @param n Number of rows to preview. Default 10.
#' @param con Optional explicit DBI connection.
#' @return Tibble of `n` rows (invisibly).
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' peek_table("mimic_iv_demo", "admissions")
#' }
#' @export
peek_table <- function(src_name, table, schema = "raw", n = 10, con = NULL) {
  schema_names <- src_schema_names(src_name)
  schema_name  <- .ehr_validated_schema_name(schema, schema_names)

  .with_connection(con, function(con) {
    available <- DBI::dbGetQuery(con,
      "SELECT table_name FROM information_schema.tables
       WHERE table_schema = $1 ORDER BY table_name",
      params = list(schema_name)
    )$table_name

    if (!table %in% available) {
      msg <- sprintf("Table '%s' not found in schema '%s'.", table, schema_name)
      suggestions <- suggest_match(table, available)
      if (length(suggestions) > 0)
        msg <- paste0(msg, "\n  Did you mean: ", paste(suggestions, collapse = ", "), "?")
      if (length(available) == 0)
        msg <- paste0(msg, "\n  Schema is empty - has the pipeline been run for '", src_name, "'?")
      stop(msg, call. = FALSE)
    }

    df <- fetch_table(src_name, table, schema = schema, n = n, con = con)
    cat(sprintf("<peek: %s.%s  [%d rows x %d cols]>\n",
                schema_name, table, nrow(df), ncol(df)))
    print(df)
    invisible(df)
  })
}

# ___________________________________________________________________________
# Domain shortcuts - window tables and cohort_info
# ___________________________________________________________________________

#' Fetch the admission_windows table for a source
#'
#' admission_windows has one row per hospital admission/hospital stay.
#'
#' @param src_name Source name.
#' @param n Max rows. Default Inf.
#' @param con Optional explicit DBI connection.
#' @return A tibble.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' load_admission_windows("mimic_iv_demo", n = 5)
#' }
#' @export
load_admission_windows <- function(src_name, n = Inf, con = NULL) {
  fetch_table(src_name, "admission_windows", schema = "raw", n = n, con = con)
}

#' Fetch the icu_windows table for a source
#'
#' icu_windows has one row per ICU/unit stay.
#'
#' @param src_name Source name.
#' @param n Max rows. Default Inf.
#' @param con Optional explicit DBI connection.
#' @return A tibble.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' load_icu_windows("mimic_iv_demo", n = 5)
#' }
#' @export
load_icu_windows <- function(src_name, n = Inf, con = NULL) {
  fetch_table(src_name, "icu_windows", schema = "raw", n = n, con = con)
}

#' Fetch the visit_windows table for a source
#'
#' visit_windows is the admission-to-ICU crosswalk. It keeps admissions without
#' ICU stays as rows with missing ICU fields.
#'
#' @param src_name Source name.
#' @param n Max rows. Default Inf.
#' @param con Optional explicit DBI connection.
#' @return A tibble.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' load_visit_windows("eicu_demo", n = 5)
#' }
#' @export
load_visit_windows <- function(src_name, n = Inf, con = NULL) {
  fetch_table(src_name, "visit_windows", schema = "raw", n = n, con = con)
}

#' Fetch the cohort_info table for a source
#'
#' cohort_info contains per-admission cohort metadata: hospitalization sequence,
#' diagnostic ICD summary, the cohort membership flag cohort_index, and any
#' domain-specific enrichment columns added by the classify_fn supplied to
#' run_cohort_info(). All rows are returned, including imported patient-context
#' rows that are not cohort members.
#'
#' @param src_name Source name.
#' @param n Max rows. Default Inf.
#' @param con Optional explicit DBI connection.
#' @return A tibble.
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' ci <- load_cohort_info("mimic_iv_demo")
#' table(ci$cohort_index)
#' }
#' @export
load_cohort_info <- function(src_name, n = Inf, con = NULL) {
  fetch_table(src_name, "cohort_info", schema = "raw", n = n, con = con)
}

# ___________________________________________________________________________
# Concept exploration
# ___________________________________________________________________________

#' Search for concept names matching a pattern
#'
#' Grep-style search against the list of available concept tables.
#'
#' @param pattern Regex pattern (passed to `grepl`).
#' @param src_name Source name to restrict the search to; `NULL` searches
#'   every source the connection knows.
#' @param ignore_case Case-insensitive match. Default TRUE.
#' @param con Optional explicit DBI connection.
#' @return A data frame with one row per matching concept table: `source`
#'   plus the columns `list_concepts()` returns. When nothing matches, a
#'   zero-row data frame with `source`, `concept_name` and `schema` (a
#'   message is emitted in that case).
#' @export
#'
#' @examples
#' \dontrun{
#' search_concepts("pressure", "mimic_iv_demo")   # sbp, dbp, map...
#' search_concepts("^spo", "mimic_iv_demo")        # spo2 variants
#' search_concepts("kb")                            # across every connected source
#' }
search_concepts <- function(pattern, src_name = NULL, ignore_case = TRUE,
                            con = NULL) {
  sources <- if (!is.null(src_name)) src_name else list_sources(con = con)

  results <- lapply(sources, function(src) {
    df <- suppressMessages(list_concepts(src, con = con))
    if (nrow(df) == 0) return(NULL)
    matched <- df[grepl(pattern, df$concept_name, ignore.case = ignore_case), ,
                  drop = FALSE]
    if (nrow(matched) == 0) return(NULL)
    cbind(source = src, matched, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, Filter(Negate(is.null), results))

  if (is.null(out) || nrow(out) == 0) {
    message("No concepts matching '", pattern, "' found",
            if (!is.null(src_name)) paste0(" in '", src_name, "'") else
              " across all sources", ".")
    return(invisible(data.frame(source = character(), concept_name = character(),
                                schema = character())))
  }

  rownames(out) <- NULL
  out
}

#' Summary statistics for one concept
#'
#' Returns row count, patient count, time range, and numeric value stats
#' for a concept table (if a `value` column is present).
#'
#' @param src_name Source name.
#' @param concept Concept name.
#' @param schema Optional source schema key or full schema name. If NULL, the
#'   concepts schema is tried first, then the derived schema.
#' @param con Optional explicit DBI connection.
#' @return A named list with summary statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' concept_summary("mimic_iv_demo", "heart_rate")
#' }
concept_summary <- function(src_name, concept, schema = NULL, con = NULL) {
  schema_names <- src_schema_names(src_name)

  .with_connection(con, function(con) {
    # Auto-detect schema if not supplied: prefer concepts, fall back to drv
    resolved_schema <- if (!is.null(schema)) {
      .ehr_schema_name(schema, schema_names)
    } else {
      concepts_tbls <- DBI::dbGetQuery(con, sprintf(
        "SELECT table_name FROM information_schema.tables
         WHERE table_schema = '%s'", schema_names$concepts
      ))$table_name
      derived_tbls <- tryCatch(DBI::dbGetQuery(con, sprintf(
        "SELECT table_name FROM information_schema.tables
         WHERE table_schema = '%s'", schema_names$derived
      ))$table_name, error = function(e) character(0))
      if (concept %in% concepts_tbls)  schema_names$concepts
      else if (concept %in% derived_tbls) schema_names$derived
      else stop(sprintf("Concept '%s' not found in concepts or derived schema for '%s'.",
                        concept, src_name), call. = FALSE)
    }

    tbl <- paste0('"', resolved_schema, '"."', concept, '"')

    row_count <- DBI::dbGetQuery(
      con, paste0("SELECT COUNT(*) AS n FROM ", tbl)
    )$n

    cols <- DBI::dbGetQuery(con, sprintf(
      "SELECT column_name FROM information_schema.columns
       WHERE table_schema = '%s' AND table_name = '%s'",
      resolved_schema, concept
    ))$column_name

    id_col <- if ("subject_id"         %in% cols) "subject_id"
              else if ("SUBJECT_ID"    %in% cols) "SUBJECT_ID"
              else if ("patienthealthsystemstayid" %in% cols) "patienthealthsystemstayid"
              else NULL

    pt_count <- if (!is.null(id_col))
      DBI::dbGetQuery(con,
        paste0("SELECT COUNT(DISTINCT ", id_col, ") AS n FROM ", tbl)
      )$n
    else NA_integer_

    val_stats <- if ("value" %in% cols) {
      DBI::dbGetQuery(con, paste0(
        "SELECT MIN(value::float) AS min, MAX(value::float) AS max,
                AVG(value::float) AS mean, PERCENTILE_CONT(0.5)
                WITHIN GROUP (ORDER BY value::float) AS median
         FROM ", tbl, " WHERE value IS NOT NULL"
      ))
    } else NULL

    result <- list(
      concept       = concept,
      source        = src_name,
      schema        = resolved_schema,
      row_count     = row_count,
      patient_count = pt_count,
      columns       = cols
    )
    if (!is.null(val_stats)) {
      result$value_min    <- round(val_stats$min,  3)
      result$value_max    <- round(val_stats$max,  3)
      result$value_mean   <- round(val_stats$mean, 3)
      result$value_median <- round(val_stats$median, 3)
    }
    result
  })
}

# ___________________________________________________________________________
# Patient-level access
# ___________________________________________________________________________

# Resolve the harmonized time-series table for an analysis level:
#   "admission" -> ts_tab  (native grain; each event once)
#   "icu"       -> ts_icu  (derived ICU projection from make_icu_timeline)
# Errors with a clear, actionable message when the requested table is absent.
.ehr_resolve_ts_table <- function(con, schema, analysis_level) {
  analysis_level <- match.arg(analysis_level, c("admission", "icu"))
  tbl <- if (analysis_level == "icu") "ts_icu" else "ts_tab"
  ok <- DBI::dbGetQuery(con, sprintf(
    "SELECT to_regclass('%s.%s') IS NOT NULL AS ok", schema, tbl))$ok
  if (!isTRUE(ok)) {
    if (tbl == "ts_icu")
      stop("ts_icu not found in '", schema, "'. Run make_icu_timeline() first ",
           "for analysis_level = 'icu'.", call. = FALSE)
    stop(tbl, " not found in '", schema,
         "'. Run run_harmonized_tables() first.", call. = FALSE)
  }
  tbl
}

# Resolve the patient/visit id column to filter on within a harmonized ts table.
# ts_tab keeps cohort_key(); ts_icu (the ICU projection) drops the native ICU id
# and exposes it as source_icustay_id, so fall back to that when cohort_key is
# absent (e.g. eICU, whose cohort_key is the unit-stay id).
.ehr_resolve_ts_id_col <- function(con, schema, ts_table, src_name) {
  cols   <- DBI::dbListFields(con, DBI::Id(schema = schema, table = ts_table))
  id_col <- cohort_key(src_name)
  if (id_col %in% cols) return(id_col)
  for (alt in c("source_icustay_id", "assigned_icustay_id")) {
    if (alt %in% cols) return(alt)
  }
  stop("Could not resolve a patient id column in ", schema, ".", ts_table,
       " (looked for ", id_col, ", source_icustay_id, assigned_icustay_id).",
       call. = FALSE)
}

#' Fetch all time-series data for one patient
#'
#' Filters `ts_tab` (the harmonized long-format output) to a single patient.
#' Optionally restrict to specific concepts.
#'
#' @param src_name Source name.
#' @param patient_id The patient identifier value (matches the primary ID
#'   column for the source: subject_id / patientunitstayid).
#' @param concepts Optional character vector of concept names to include.
#'   Default NULL returns all concepts.
#' @param con Optional explicit DBI connection.
#' @param analysis_level Grain of the harmonized series to read: `"admission"`
#'   (default) reads the native-grain `ts_tab` (each event once); `"icu"` reads
#'   the derived `ts_icu` projection built by `make_icu_timeline()`.
#' @return A tibble in long format (concept_name, time, value, unit, ...).
#' @export
#'
#' @examples
#' \dontrun{
#' get_patient_ts("mimic_iv_demo", patient_id = 10000032)
#' get_patient_ts("mimic_iv_demo", 10000032, concepts = c("heart_rate", "spo2"))
#' }
get_patient_ts <- function(src_name, patient_id, concepts = NULL,
                           con = NULL,
                           analysis_level = c("admission", "icu")) {
  analysis_level <- match.arg(analysis_level)
  schema_names   <- src_schema_names(src_name)

  .with_connection(con, function(con) {
    ts_table <- .ehr_resolve_ts_table(con, schema_names$harmonized, analysis_level)
    id_col   <- .ehr_resolve_ts_id_col(con, schema_names$harmonized, ts_table, src_name)

    where <- sprintf("WHERE %s = %s", id_col,
                     DBI::dbQuoteLiteral(con, patient_id))

    if (!is.null(concepts) && length(concepts) > 0) {
      concept_list <- paste(
        sapply(concepts, function(x) DBI::dbQuoteLiteral(con, x)),
        collapse = ", "
      )
      where <- paste0(where, " AND concept_name IN (", concept_list, ")")
    }

    # one admission after another, each in time order
    cols <- DBI::dbListFields(con, DBI::Id(schema = schema_names$harmonized, table = ts_table))
    adm_col <- intersect(c("hadm_id", "patienthealthsystemstayid", "src_admission_id"), cols)[1]
    order_by <- paste(c(id_col, if (!is.na(adm_col)) adm_col, "time_since_hadm_min"), collapse = ", ")
    DBI::dbGetQuery(con, sprintf(
      "SELECT * FROM %s.%s %s ORDER BY %s",
      schema_names$harmonized, ts_table, where, order_by
    ))
  })
}

#' Fetch the flat (baseline) row for one patient
#'
#' Returns the patient's rows from `flat_tab`: one per admission (and ICU
#' stay), with static features such as sex, race, insurance and age.
#'
#' @param src_name Source name.
#' @param patient_id The patient identifier value.
#' @param con Optional explicit DBI connection.
#' @return A tibble with one row per admission of the patient (zero rows if
#'   the patient is not in the cohort).
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' get_patient_flat("mimic_iv_demo", patient_id = 10000032)
#' }
#' @export
get_patient_flat <- function(src_name, patient_id, con = NULL) {
  schema_names <- src_schema_names(src_name)
  id_col       <- cohort_key(src_name)

  .with_connection(con, function(con) {
    DBI::dbGetQuery(con, sprintf(
      "SELECT * FROM %s.flat_tab WHERE %s = %s",
      schema_names$harmonized, id_col,
      DBI::dbQuoteLiteral(con, patient_id)
    ))
  })
}

# ___________________________________________________________________________
# Wide-format pivot of the harmonized time series (Option 4: per-concept spec)
# ___________________________________________________________________________

# Supported wide-pivot aggregates. Every numeric aggregate - including first and
# last - routes its value through .ehr_num_cast() so each one returns a real
# double precision column rather than text, and a stray non-numeric value cannot
# error the whole query (it casts to NULL instead). A genuinely categorical
# concept therefore yields NULL under any numeric aggregate; the wide pivot is a
# numeric matrix by construction.
.ehr_numeric_aggs <- c("min", "max", "mean", "median", "sd",
                       "variance", "sum", "count")
.ehr_supported_aggs <- c(.ehr_numeric_aggs, "first", "last", "n")
.ehr_default_agg <- "mean"

# Guarded numeric cast: numeric-looking text -> double precision, anything else
# (categorical values, blanks, junk) -> NULL. Used by every wide-pivot aggregate
# so numeric output and dirty-data tolerance are uniform across all of them.
.ehr_num_cast <- function(col) {
  # The value column is text in the per-source harmonized tables and numeric
  # in a merged schema; the regex test needs text, so cast first.
  txt <- sprintf("CAST(%s AS TEXT)", col)
  sprintf(
    "CASE WHEN %s ~ '^[+-]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?$' THEN %s::double precision END",
    txt, txt)
}

.ehr_expand_aggs <- function(aggs) {
  if (!is.character(aggs) || length(aggs) == 0)
    stop("Each `spec` entry must be a non-empty character vector of aggregates.",
         call. = FALSE)

  aggs <- tolower(aggs)
  expanded <- unlist(lapply(aggs, function(agg) {
    if (agg %in% c("default", "mean_default")) return(.ehr_default_agg)
    if (agg == "all_numeric") return(.ehr_numeric_aggs)
    agg
  }), use.names = FALSE)

  unsupported <- setdiff(expanded, .ehr_supported_aggs)
  if (length(unsupported) > 0)
    stop("Unsupported aggregate(s): ", paste(unsupported, collapse = ", "),
         ". Use one of: ",
         paste(c(.ehr_supported_aggs, "all_numeric", "default"), collapse = ", "),
         ".", call. = FALSE)

  unique(expanded)
}

# Range-cleaning predicates on the numeric value, for a c(min =, max =) bound
# pair (either side may be NA = unbounded). Both ends are inclusive, matching
# clean_by_range(): only values below min or above max are out of range.
.ehr_sql_num_lit <- function(x) format(x, scientific = FALSE, digits = 15)

.ehr_in_range_sql <- function(num, bounds) {
  parts <- c(
    if (!is.na(bounds[["min"]])) sprintf("%s >= %s", num, .ehr_sql_num_lit(bounds[["min"]])),
    if (!is.na(bounds[["max"]])) sprintf("%s <= %s", num, .ehr_sql_num_lit(bounds[["max"]])))
  paste(parts, collapse = " AND ")
}

.ehr_out_of_range_sql <- function(num, bounds) {
  parts <- c(
    if (!is.na(bounds[["min"]])) sprintf("%s < %s", num, .ehr_sql_num_lit(bounds[["min"]])),
    if (!is.na(bounds[["max"]])) sprintf("%s > %s", num, .ehr_sql_num_lit(bounds[["max"]])))
  paste0("(", paste(parts, collapse = " OR "), ")")
}

# Resolve clean = "range" bounds for the concepts of a wide spec, with the same
# resolver as clean_by_range() (widest window across config rows). Returns a
# named list concept -> c(min =, max =), NULL for concepts with no usable bound.
.ehr_wide_clean_bounds <- function(concepts, ranges = NULL) {
  all_bounds <- .resolve_range_bounds(ranges, NULL, NULL)
  out <- stats::setNames(lapply(concepts, function(cn) {
    b <- all_bounds[[cn]]
    if (is.null(b) || all(is.na(b))) return(NULL)
    if (!is.numeric(b))
      stop("Range bounds for '", cn, "' must be numeric.", call. = FALSE)
    b
  }), concepts)
  unbounded <- concepts[vapply(out, is.null, logical(1))]
  if (length(unbounded))
    message("clean = \"range\": no min/max bound for: ",
            paste(unbounded, collapse = ", "), " (left unchanged)")
  out
}

.ehr_check_wide_clean <- function(clean, ranges) {
  if (!is.null(ranges) && clean == "none")
    stop("`ranges` is only used with clean = \"range\".", call. = FALSE)
  invisible(TRUE)
}

# Map one aggregate name to a Postgres conditional-aggregate expression scoped
# to a single concept. concept_lit must already be a quoted SQL string literal.
# With `bounds`, out-of-range readings are excluded before aggregation.
.ehr_wide_agg_sql <- function(agg, tcol, concept_lit, bounds = NULL) {
  num  <- .ehr_num_cast("value")
  cond <- sprintf("concept_name = %s", concept_lit)
  if (!is.null(bounds)) cond <- paste(cond, "AND", .ehr_in_range_sql(num, bounds))
  filt <- sprintf("FILTER (WHERE %s)", cond)
  switch(agg,
    min      = sprintf("min(%s) %s", num, filt),
    max      = sprintf("max(%s) %s", num, filt),
    mean     = sprintf("avg(%s) %s", num, filt),
    median   = sprintf(
      "percentile_cont(0.5) WITHIN GROUP (ORDER BY %s) %s", num, filt),
    sd       = sprintf("stddev_samp(%s) %s", num, filt),
    variance = sprintf("var_samp(%s) %s", num, filt),
    sum      = sprintf("sum(%s) %s", num, filt),
    n        = ,
    count    = sprintf("count(value) %s", filt),
    first    = sprintf("(array_agg(%s ORDER BY %s ASC)  %s)[1]", num, tcol, filt),
    last     = sprintf("(array_agg(%s ORDER BY %s DESC) %s)[1]", num, tcol, filt),
    stop("Unsupported aggregate: ", agg, call. = FALSE)
  )
}

# Validate and normalise a wide-pivot `spec` to a named list of aggregate
# vectors. Unnamed character vectors default each concept to mean; named
# character vectors are interpreted as concept = aggregate shorthand.
.ehr_normalize_spec <- function(spec) {
  if (!is.list(spec) && is.character(spec)) {
    nm <- names(spec)
    if (is.null(nm) || all(nm == "")) {
      spec <- setNames(as.list(rep(.ehr_default_agg, length(spec))), spec)
    } else {
      if (any(nm == ""))
        stop("`spec` character shorthand must be either all unnamed concepts ",
             "or all named concept = aggregate entries.", call. = FALSE)
      spec <- as.list(spec)
    }
  }
  if (!is.list(spec) || length(spec) == 0 ||
      is.null(names(spec)) || any(names(spec) == ""))
    stop("`spec` must be a non-empty named list mapping concepts to ",
         "aggregates, or an unnamed character vector of concepts.",
         call. = FALSE)

  lapply(spec, .ehr_expand_aggs)
}

# Shared wide-pivot engine. Emits a single conditional-aggregation query that
# pivots a long harmonized table (per-source ts_tab/ts_icu, or a merged ts) to
# wide format, grouped by `id_cols` plus a time bin. Callers resolve which table
# and id columns to use; this function owns the SQL. The pivot happens entirely
# server-side - the long table is never pulled into R.
# ---- Cohort membership filter (cohort_only) --------------------------------

# Superseded membership arguments are rejected rather than aliased:
# case_adm_only kept whole cohort hospital admissions (the retired
# hos_cohort_flg) and scope_only kept the import scope. Neither is the cohort
# definition, which cohort_only filters on.
.ehr_removed_membership_arg <- function(arg) {
  stop("`", arg, "` has been removed. Use `cohort_only` (default TRUE), which ",
       "keeps rows of cohort members (cohort_info.cohort_index, the cohort ",
       "definition). It is not a rename: ", if (identical(arg, "scope_only"))
       "scope_only kept everything inside the import scope, including patient-context admissions"
       else "case_adm_only kept whole cohort hospital admissions, whatever grain the cohort was selected at",
       ". Pass cohort_only = FALSE to read all imported context.",
       call. = FALSE)
}

.ehr_check_cohort_only <- function(cohort_only) {
  if (!is.logical(cohort_only) || length(cohort_only) != 1L || is.na(cohort_only))
    stop("`cohort_only` must be TRUE or FALSE.", call. = FALSE)
  cohort_only
}

# Fail unless `ci_schema`.cohort_info exists and carries cohort_index; returns
# its column names. An artifact without cohort_index predates the single
# membership flag and must be rebuilt: neither hos_cohort_flg nor scope_index
# is a substitute.
.ehr_member_ci_cols <- function(con, ci_schema) {
  ci_id <- DBI::Id(schema = ci_schema, table = "cohort_info")
  if (!DBI::dbExistsTable(con, ci_id))
    stop("cohort_only = TRUE needs \"", ci_schema, "\".cohort_info. Build it ",
         "with build_cohort_info_general() (and merge_sources() for a merged ",
         "schema), or pass cohort_only = FALSE.", call. = FALSE)
  ci_cols <- DBI::dbListFields(con, ci_id)
  if (!"cohort_index" %in% ci_cols)
    stop("\"", ci_schema, "\".cohort_info has no cohort_index column: it was ",
         "built before the single cohort membership flag. Rebuild cohort_info ",
         "with build_cohort_info_general() (then re-run merge_sources() for a ",
         "merged schema), or pass cohort_only = FALSE to read without ",
         "membership filtering.", call. = FALSE)
  ci_cols
}

# ICU-level filtering needs ICU-grain membership. Admission-grain membership is
# rolled up per admission and would admit every sibling ICU stay of a
# unit-selected (SQL, direct-ID, Sepsis-3) cohort. The grain is internal
# metadata (cohort_info_meta), one row per source.
.ehr_require_icu_membership <- function(con, ci_schema) {
  meta_id <- DBI::Id(schema = ci_schema, table = "cohort_info_meta")
  rebuild <- paste0("Rebuild cohort_info with analysis_level = \"icu\" (then ",
                    "re-run merge_sources() for a merged schema).")
  if (!DBI::dbExistsTable(con, meta_id))
    stop("ICU-level membership filtering needs \"", ci_schema,
         "\".cohort_info_meta, which records the membership grain. ", rebuild,
         call. = FALSE)
  sources <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT CAST(source_data AS text) AS source_data FROM %s",
    DBI::dbQuoteIdentifier(con, DBI::Id(schema = ci_schema, table = "cohort_info"))
  ))$source_data
  meta <- DBI::dbGetQuery(con, sprintf(
    "SELECT CAST(source_data AS text) AS source_data,
            CAST(analysis_level AS text) AS analysis_level FROM %s",
    DBI::dbQuoteIdentifier(con, meta_id)))
  level <- meta$analysis_level[match(sources, meta$source_data)]
  bad <- sort(sources[is.na(level) | level != "icu"])
  if (length(bad))
    stop("ICU-level membership filtering needs ICU-grain membership, but \"",
         ci_schema, "\".cohort_info resolved cohort_index at admission level ",
         "(or has no recorded grain) for: ", paste(bad, collapse = ", "), ". ",
         rebuild, call. = FALSE)
  invisible(TRUE)
}

# SQL predicate keeping only rows of cohort members (cohort_info.cohort_index).
# `data_cols` are the data table's key columns and `ci_cols` the matching
# cohort_info columns, in the same order; both sides are cast to text so typed
# source tables and the all-text merged tables compare alike. Merged callers
# include source_data in the keys, because local ids repeat across sources.
# `level = "icu"` requires ICU-grain membership (see above). Rows whose keys are
# NULL match no cohort_info row and are excluded.
.ehr_member_pred <- function(con, ci_schema, data_cols, ci_cols, level,
                             available_cols, data_label, alias = NULL) {
  ci_have <- .ehr_member_ci_cols(con, ci_schema)
  missing_data <- setdiff(data_cols, available_cols)
  missing_ci <- setdiff(ci_cols, ci_have)
  if (length(missing_data) || length(missing_ci))
    stop("cohort_only = TRUE matches ", data_label, " (",
         paste(data_cols, collapse = ", "), ") to \"", ci_schema,
         "\".cohort_info (", paste(ci_cols, collapse = ", "), "); missing: ",
         paste(c(missing_data, missing_ci), collapse = ", "), ".", call. = FALSE)
  if (identical(level, "icu")) .ehr_require_icu_membership(con, ci_schema)
  ci_ref <- DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = ci_schema, table = "cohort_info"))
  as_text <- function(cols, prefix) paste0(
    "CAST(", prefix, DBI::dbQuoteIdentifier(con, cols), " AS text)",
    collapse = ", ")
  sprintf("(%s) IN (SELECT %s FROM %s WHERE cohort_index::boolean)",
          as_text(data_cols, if (is.null(alias)) "" else paste0(alias, ".")),
          as_text(ci_cols, ""), ci_ref)
}

# `clean_bounds` (from .ehr_wide_clean_bounds()) turns on range cleaning:
# out-of-range readings are excluded inside each aggregate's FILTER, every
# concept gets a <concept>_n_cleaned count column (0 when it has no bound), and
# the result carries a per-concept "clean_report" attribute. NULL = no cleaning,
# and the SQL is exactly the uncleaned pivot.
.ehr_pivot_wide <- function(con, schema, table, id_cols, tcol,
                            spec, bin_min, n, cohort_pred = NULL,
                            window = NULL, complete_grid = FALSE,
                            clean_bounds = NULL) {
  if (!is.null(bin_min) && (!is.numeric(bin_min) || length(bin_min) != 1 ||
                            !is.finite(bin_min) || bin_min <= 0))
    stop("`bin_min` must be a single positive number of minutes, or NULL.",
         call. = FALSE)

  # `window` and `complete_grid` operate on integer bin indices, so they only
  # make sense when binning is on. A raw-timestamp pivot has no fixed grid.
  if (is.null(bin_min) && (!is.null(window) || isTRUE(complete_grid)))
    stop("`window` and `complete_grid` require `bin_min` (not NULL): they ",
         "operate on integer bin indices.", call. = FALSE)

  if (!is.null(window)) {
    if (!is.numeric(window) || length(window) != 2 || any(!is.finite(window)) ||
        window[1] > window[2])
      stop("`window` must be a length-2 numeric c(from_bin, to_bin) of bin ",
           "indices, inclusive both ends, with from <= to.", call. = FALSE)
    window <- as.integer(round(window))
  }

  tbl_ref  <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table))
  qtcol    <- DBI::dbQuoteIdentifier(con, tcol)
  concepts <- names(spec)

  available <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT concept_name FROM %s", tbl_ref))$concept_name
  missing <- setdiff(concepts, available)
  if (length(missing) > 0)
    warning("Concepts not present in ", table, " (columns will be all-NA): ",
            paste(missing, collapse = ", "))

  # Build one aggregate column per (concept, aggregate). Single-aggregate
  # concepts keep their plain name; multi-aggregate concepts get a suffix.
  # `alias_names` mirrors the column order for the grid left-join select.
  cleaning <- !is.null(clean_bounds)
  num      <- .ehr_num_cast("value")
  n_cleaned_name <- function(cn) paste0(cn, "_n_cleaned")

  alias_names <- unlist(lapply(concepts, function(cn) {
    aggs <- spec[[cn]]
    c(if (length(aggs) == 1) cn else paste0(cn, "_", aggs),
      if (cleaning) n_cleaned_name(cn))
  }), use.names = FALSE)

  agg_cols <- unlist(lapply(concepts, function(cn) {
    aggs <- spec[[cn]]
    concept_lit <- DBI::dbQuoteLiteral(con, cn)
    bounds <- if (cleaning) clean_bounds[[cn]]
    cols <- vapply(aggs, function(agg) {
      expr  <- .ehr_wide_agg_sql(agg, tcol, concept_lit, bounds = bounds)
      alias <- if (length(aggs) == 1) cn else paste0(cn, "_", agg)
      paste0(expr, " AS ", DBI::dbQuoteIdentifier(con, alias))
    }, character(1))
    if (cleaning) {
      out_cond <- if (is.null(bounds)) "FALSE" else
        paste(sprintf("concept_name = %s", concept_lit), "AND",
              .ehr_out_of_range_sql(num, bounds))
      cols <- c(cols, sprintf("count(*) FILTER (WHERE %s) AS %s", out_cond,
                              DBI::dbQuoteIdentifier(con, n_cleaned_name(cn))))
    }
    cols
  }), use.names = FALSE)

  id_sql <- paste(DBI::dbQuoteIdentifier(con, id_cols), collapse = ", ")

  concept_in <- paste(
    vapply(concepts, function(x) DBI::dbQuoteLiteral(con, x), character(1)),
    collapse = ", ")

  where_sql <- sprintf("concept_name IN (%s)", concept_in)
  if (!is.null(cohort_pred) && nzchar(cohort_pred))
    where_sql <- paste0(where_sql, "\n  AND ", cohort_pred)
  # Window filter: bin index in [from, to] inclusive -> raw time in
  # [from*bin, (to+1)*bin) so the upper bin is fully included.
  if (!is.null(window))
    where_sql <- paste0(where_sql, sprintf(
      "\n  AND %s >= %s AND %s < %s",
      qtcol, window[1] * bin_min, qtcol, (window[2] + 1L) * bin_min))

  # Per-concept cleaning report over the same rows the pivot reads (cohort,
  # concepts, window), regardless of `n`: readings, readings cleaned, readings
  # whose value is missing or not a number, and the bounds used.
  with_report <- function(res) {
    if (!cleaning) return(res)
    cnt <- unlist(lapply(seq_along(concepts), function(i) {
      lit <- DBI::dbQuoteLiteral(con, concepts[i])
      b   <- clean_bounds[[concepts[i]]]
      out <- if (is.null(b)) "FALSE" else .ehr_out_of_range_sql(num, b)
      sprintf(paste0("count(*) FILTER (WHERE concept_name = %s) AS n_%d, ",
                     "count(*) FILTER (WHERE concept_name = %s AND %s) AS c_%d, ",
                     "count(*) FILTER (WHERE concept_name = %s AND %s IS NULL) AS x_%d"),
              lit, i, lit, out, i, lit, num, i)
    }))
    r <- DBI::dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s",
                                      paste(cnt, collapse = ", "), tbl_ref, where_sql))
    idx <- seq_along(concepts)
    # counts arrive as integer64 on Postgres; unlist() would drop the class
    bnd <- function(side) vapply(concepts, function(cn) {
      b <- clean_bounds[[cn]]
      if (is.null(b)) NA_real_ else unname(b[[side]])
    }, numeric(1))
    attr(res, "clean_report") <- data.frame(
      concept_name  = concepts,
      n             = vapply(r[paste0("n_", idx)], as.numeric, numeric(1)),
      n_cleaned     = vapply(r[paste0("c_", idx)], as.numeric, numeric(1)),
      n_not_numeric = vapply(r[paste0("x_", idx)], as.numeric, numeric(1)),
      min_value     = unname(bnd("min")),
      max_value     = unname(bnd("max")),
      row.names = NULL, stringsAsFactors = FALSE)
    res
  }

  # ---- Grid completion: emit every bin in the window per present stay -------
  if (isTRUE(complete_grid)) {
    if (!is.null(window)) {
      lo <- window[1]; hi <- window[2]
    } else {
      rng <- DBI::dbGetQuery(con, sprintf(
        "SELECT min(floor(%s::numeric / %s)::int) AS lo,
                max(floor(%s::numeric / %s)::int) AS hi
         FROM %s WHERE %s",
        qtcol, bin_min, qtcol, bin_min, tbl_ref, where_sql))
      lo <- rng$lo; hi <- rng$hi
    }
    if (!is.na(lo) && !is.na(hi)) {
      alias_sql <- paste(DBI::dbQuoteIdentifier(con, alias_names), collapse = ", ")
      q <- sprintf(
        "WITH agg AS (
           SELECT %s, floor(%s::numeric / %s)::int AS t_bin,
             %s
           FROM %s
           WHERE %s
           GROUP BY %s, t_bin
         ),
         ids AS (SELECT DISTINCT %s FROM agg),
         grid AS (
           SELECT ids.*, g.t_bin
           FROM ids CROSS JOIN generate_series(%d, %d, 1) AS g(t_bin)
         )
         SELECT %s, t_bin, %s
         FROM grid LEFT JOIN agg USING (%s, t_bin)
         ORDER BY %s, t_bin",
        id_sql, qtcol, bin_min,
        paste(agg_cols, collapse = ",\n             "),
        tbl_ref, where_sql, id_sql,
        id_sql,
        as.integer(lo), as.integer(hi),
        id_sql, alias_sql, id_sql, id_sql)
      if (is.finite(n)) q <- paste0(q, "\nLIMIT ", as.integer(n))
      return(with_report(DBI::dbGetQuery(con, q)))
    }
    # No rows matched: fall through to the plain (empty) pivot below.
  }

  # ---- Plain pivot: only bins that actually have data ----------------------
  if (is.null(bin_min)) {
    time_select <- qtcol
    group_time  <- qtcol
  } else {
    time_select <- sprintf("floor(%s::numeric / %s)::int AS t_bin",
                           qtcol, bin_min)
    group_time  <- "t_bin"
  }

  q <- sprintf(
    "SELECT %s, %s,\n  %s\nFROM %s\nWHERE %s\nGROUP BY %s, %s\nORDER BY %s, %s",
    id_sql, time_select,
    paste(agg_cols, collapse = ",\n  "),
    tbl_ref,
    where_sql,
    id_sql, group_time,
    id_sql, group_time)
  if (is.finite(n)) q <- paste0(q, "\nLIMIT ", as.integer(n))

  with_report(DBI::dbGetQuery(con, q))
}

#' Pivot the harmonized time series into wide format (one column per concept)
#'
#' Reads the harmonized long table (`ts_tab` for admission grain, or the
#' `ts_icu` projection for ICU grain) and pivots it to wide format entirely in
#' SQL via conditional aggregation - the long table is never pulled into R.
#'
#' Time is binned by integer arithmetic on the minute axis
#' (`floor(time / bin_min) * bin_min`), so multiple measurements of a concept
#' that fall in the same bin are reconciled by the aggregate(s) you specify per
#' concept. Set `bin_min = NULL` for a raw-timestamp pivot (no binning); ties at
#' the exact same timestamp then collapse with the chosen aggregate.
#'
#' Column naming: a concept with a single aggregate keeps its plain name
#' (`sbp`); a concept with several aggregates is suffixed (`hr_mean`, `hr_max`).
#'
#' @param src_name Source name (e.g. `"mimic_iv_demo"`).
#' @param spec Named list mapping each concept to a character vector of
#'   aggregates. Supported: `"min"`, `"max"`, `"mean"`, `"median"`, `"sd"`,
#'   `"variance"`, `"sum"`, `"n"`/`"count"`, `"first"`, and `"last"`;
#'   `"all_numeric"` expands to all numeric reducers. An unnamed character
#'   vector defaults each concept to `"mean"`; a named character vector is
#'   interpreted as concept = aggregate shorthand. Example:
#'   `list(hr = c("mean", "max"), sbp = "mean", gcs = "last", urine = "sum")`.
#' @param bin_min Bin width in minutes (default 60). `NULL` disables binning
#'   and pivots on the raw timestamp. The emitted `t_bin` column is the 0-based
#'   bin index (`floor(time / bin_min)`), e.g. `0, 1, 2, ...` for hourly bins.
#' @param analysis_level `"admission"` (default) pivots `ts_tab` on
#'   `time_since_hadm_min`; `"icu"` pivots `ts_icu` on `time_since_icu_min`,
#'   keyed on `assigned_icustay_id` so each ICU stay is a separate observation
#'   on its own ICU clock.
#' @param window Optional `c(from_bin, to_bin)` in bin-index units, inclusive
#'   both ends, restricting the output to those bins (applied in SQL before
#'   aggregation). E.g. with `bin_min = 60`, `c(0, 47)` keeps the first 48 h,
#'   `c(0, 0)` keeps only the first hour. `NULL` (default) keeps all bins.
#' @param complete_grid If TRUE, emit a row for every bin in `window` (or, when
#'   `window` is NULL, the observed bin range) for each stay present in the long
#'   table, left-joining the aggregates onto the full grid so unmeasured bins
#'   come back as `NA` (no imputation - fill/mask downstream). Regularizes shape
#'   only, not length-of-stay eligibility. Requires non-NULL `bin_min`. Default
#'   FALSE.
#' @param n Max rows to return. Default Inf.
#' @param cohort_only If TRUE (default), pivot only rows of cohort members
#'   (`cohort_index` in this source's `cohort_info`), matched at the requested
#'   `analysis_level`: on the admission id for `"admission"`, on the assigned
#'   ICU stay for `"icu"` (which needs `cohort_info` built with
#'   `analysis_level = "icu"`). Applied in SQL before aggregation. FALSE reads
#'   all imported context rows; it cannot recover data that was never imported.
#' @param clean `"none"` (default) pivots the values as stored. `"range"`
#'   excludes readings outside each concept's physiologic `[min, max]` (both
#'   ends inclusive) in SQL, before aggregation, using the same bounds as
#'   [clean_by_range()]: the concept config's widest window across sources,
#'   matched on the exact concept name. Every aggregate is then computed over
#'   valid readings only (`first`/`last`/`n` are the first/last/number of
#'   valid readings), each concept gets a `<concept>_n_cleaned` column
#'   counting the readings excluded in that bin (0 for concepts with no bound,
#'   which are left unchanged with a message), and the result carries a
#'   `clean_report` attribute per concept: `n`, `n_cleaned`, `n_not_numeric`
#'   (missing or non-numeric values), `min_value`, `max_value`. A bin whose
#'   readings were all excluded is `NA`; `_n_cleaned > 0` tells it apart from
#'   a bin with no measurement. With `complete_grid`, unmeasured grid bins have
#'   `NA` there too.
#' @param ranges Optional bounds overriding the config, in the formats accepted
#'   by [clean_by_range()]: a named list of `c(min, max)` or a data frame with
#'   `concept_name`, `min_value`, `max_value`. Requires `clean = "range"`.
#' @param con Optional explicit DBI connection.
#' @param case_adm_only,scope_only Removed; supplying either is an error. Use
#'   `cohort_only`.
#' @return A wide data frame: id columns, the time-bin column (`t_bin`, or the
#'   raw time column when `bin_min = NULL`), then one column per
#'   concept and aggregate, named `<concept>_<aggregate>`, or just `<concept>`
#'   when the concept has one aggregate (followed by `<concept>_n_cleaned` with
#'   `clean = "range"`).
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_concepts_wide("mimic_iv_demo", spec = list(
#'   hr    = c("mean", "max"),
#'   sbp   = "mean",
#'   gcs   = "last",
#'   urine = "sum"
#' ), bin_min = 60)
#'
#' # Raw-timestamp pivot, ICU grain
#' fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"),
#'                     bin_min = NULL, analysis_level = "icu")
#'
#' # All imported context rows, not only cohort members
#' fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"),
#'                     cohort_only = FALSE)
#' }
fetch_concepts_wide <- function(src_name, spec, bin_min = 60,
                                analysis_level = c("admission", "icu"),
                                window = NULL, complete_grid = FALSE,
                                n = Inf, cohort_only = TRUE,
                                clean = c("none", "range"), ranges = NULL,
                                con = NULL, case_adm_only, scope_only) {
  if (!missing(case_adm_only)) .ehr_removed_membership_arg("case_adm_only")
  if (!missing(scope_only)) .ehr_removed_membership_arg("scope_only")
  cohort_only    <- .ehr_check_cohort_only(cohort_only)
  analysis_level <- match.arg(analysis_level)
  clean          <- match.arg(clean)
  .ehr_check_wide_clean(clean, ranges)
  schema_names   <- src_schema_names(src_name)
  spec           <- .ehr_normalize_spec(spec)
  cfg            <- get_dataset_config(src_name)
  clean_bounds   <- if (clean == "range") .ehr_wide_clean_bounds(names(spec), ranges)

  .with_connection(con, function(con) {
    ts_table <- .ehr_resolve_ts_table(con, schema_names$harmonized, analysis_level)
    all_cols <- DBI::dbListFields(con, DBI::Id(schema = schema_names$harmonized,
                                               table  = ts_table))

    tcol <- if (analysis_level == "icu") "time_since_icu_min" else "time_since_hadm_min"
    if (!tcol %in% all_cols)
      stop("Time column '", tcol, "' not found in ",
           schema_names$harmonized, ".", ts_table, ".", call. = FALSE)

    id_cols <- intersect(cfg$select_id_cols, all_cols)
    # ts_icu renames the native ICU id to source_icustay_id and carries the ICU
    # observation as assigned_icustay_id; keying on it keeps each ICU stay a
    # separate observation instead of pooling an admission's stays.
    if (analysis_level == "icu") {
      if (!"assigned_icustay_id" %in% all_cols)
        stop(schema_names$harmonized, ".", ts_table, " has no ",
             "assigned_icustay_id. Rebuild it with make_icu_timeline().",
             call. = FALSE)
      id_cols <- unique(c(id_cols, "assigned_icustay_id"))
    }
    if (length(id_cols) == 0)
      stop("No id columns from the source config found in ",
           schema_names$harmonized, ".", ts_table, ".", call. = FALSE)

    cohort_pred <- NULL
    if (cohort_only) {
      id_map  <- .src_id_map(src_name)
      adm_col <- unname(id_map["src_admission_id"])
      icu     <- analysis_level == "icu"
      cohort_pred <- .ehr_member_pred(
        con, schema_names$raw,
        data_cols = c(adm_col, if (icu) "assigned_icustay_id"),
        ci_cols = c(adm_col, if (icu) unname(id_map["src_icu_id"])),
        level = analysis_level, available_cols = all_cols,
        data_label = paste0(schema_names$harmonized, ".", ts_table))
    }

    .ehr_pivot_wide(con, schema_names$harmonized, ts_table,
                    id_cols, tcol, spec, bin_min, n, cohort_pred = cohort_pred,
                    window = window, complete_grid = complete_grid,
                    clean_bounds = clean_bounds)
  })
}

# ___________________________________________________________________________
# Merged schema access
# ___________________________________________________________________________

# Minimal column set for cols = "minimal" loaders. The admission/ICU grain key
# follows the merged table's analysis level: src_icu_id for an ICU-level merge
# (signalled by the ICU-projection columns assigned_icu_id/icu_time_relation),
# otherwise src_admission_id. Only columns that actually exist are kept, so a
# leaner schema never triggers a "column does not exist" error.
.keep_cols <- function(all_cols) {
  is_icu    <- any(c("assigned_icu_id", "icu_time_relation") %in% all_cols)
  grain_key <- if (is_icu) "src_icu_id" else "src_admission_id"
  wanted    <- c("ehr_id", "source_data", grain_key,
                 "concept_name", "value", "unit",
                 "time_since_hadm_min", "time_since_icu_min")
  intersect(wanted, all_cols)
}

.merged_wide_id_cols <- function(all_cols, analysis_level, schema = NULL) {
  if (analysis_level == "icu" && !"assigned_icu_id" %in% all_cols) {
    schema_msg <- if (is.null(schema)) "" else paste0(" in \"", schema, "\".ts")
    stop("ICU-level merged wide pivots require assigned_icu_id", schema_msg,
         ". Rebuild the merged time-series table with merge_sources(..., ",
         "analysis_level = 'icu').", call. = FALSE)
  }

  wanted <- if (analysis_level == "icu") {
    c("ehr_id", "source_data", "src_patient_id", "src_admission_id",
      "assigned_icu_id")
  } else {
    c("ehr_id", "source_data", "src_patient_id", "src_admission_id")
  }
  intersect(wanted, all_cols)
}

#' Load the merged timeseries table from any merged schema
#'
#' The merged schema is created by merge_sources() and contains ts, flat, and
#' cohort_info tables pooled across all sources. Requires that the merge step
#' has already been run.
#'
#' @param schema Target schema name passed to merge_sources() (e.g. "rendehr",
#'   "trauma_project").
#' @param n Max rows. Default Inf.
#' @param concepts Optional character vector to filter concept_name column.
#' @param cols Either "all" (default, every column) or "minimal", which keeps
#'   only the analysis essentials: ehr_id, source_data, the grain key
#'   (src_admission_id for an admission-level merge, src_icu_id for an ICU-level
#'   merge), concept_name, value, unit, and both time axes. Projection happens
#'   in SQL, so "minimal" also reduces transfer for large pulls.
#' @param cohort_only If TRUE (default), return only rows of cohort members
#'   (`cohort_index` in the merged `cohort_info`), matched at the merge grain:
#'   `(source_data, src_admission_id)` for an admission-level merge, plus
#'   `assigned_icu_id` for an ICU-level merge (which needs `cohort_info` built
#'   with `analysis_level = "icu"`). Filter runs in SQL. FALSE reads all
#'   imported context rows; it cannot recover data that was never imported.
#' @param window_min Optional `c(from, to)` in minutes, keeping rows with
#'   `from <= time < to` on the clock chosen by `window_ref` (applied in SQL).
#'   E.g. `c(0, 1440)` keeps the first 24 h. Rows with a missing time are
#'   dropped when a window is set. `NULL` (default) keeps all times. Unlike
#'   the bin-index `window` of `load_merged_ts_wide()`, this is in minutes.
#' @param window_ref Clock for `window_min`: `"admission"` (default) uses
#'   `time_since_hadm_min`, `"icu"` uses `time_since_icu_min`.
#' @param con Optional explicit DBI connection.
#' @param case_adm_only,scope_only Removed; supplying either is an error. Use
#'   `cohort_only`.
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' ts <- load_merged_ts("rendehr")
#' ts <- load_merged_ts("trauma_project", concepts = c("heart_rate", "sbp"))
#' ts <- load_merged_ts("rendehr", cols = "minimal")
#' ts <- load_merged_ts("rendehr", concepts = c("heart_rate", "map", "sofa"),
#'                      cohort_only = FALSE)
#'
#' # First 24 h after hospital admission, filtered server-side
#' ts <- load_merged_ts("rendehr", concepts = c("lactate", "creatinine"),
#'                      window_min = c(0, 1440))
#' }
load_merged_ts <- function(schema, n = Inf, concepts = NULL,
                           cols = c("all", "minimal"),
                           cohort_only = TRUE, window_min = NULL,
                           window_ref = c("admission", "icu"),
                           con = NULL, case_adm_only, scope_only) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  if (!missing(case_adm_only)) .ehr_removed_membership_arg("case_adm_only")
  if (!missing(scope_only)) .ehr_removed_membership_arg("scope_only")
  cohort_only <- .ehr_check_cohort_only(cohort_only)
  cols <- match.arg(cols)
  window_ref <- match.arg(window_ref)
  if (!is.null(window_min) &&
      (!is.numeric(window_min) || length(window_min) != 2 ||
       any(!is.finite(window_min)) || window_min[1] >= window_min[2]))
    stop("`window_min` must be a length-2 numeric c(from, to) in minutes, ",
         "finite, with from < to.", call. = FALSE)
  .with_connection(con, function(con) {
    ts_cols <- DBI::dbListFields(con, DBI::Id(schema = schema, table = "ts"))
    select_sql <- "t.*"
    if (cols == "minimal") {
      keep <- .keep_cols(ts_cols)
      select_sql <- paste0("t.",
        paste(DBI::dbQuoteIdentifier(con, keep), collapse = ", t."))
    }

    where <- character(0)
    if (cohort_only) {
      icu <- "assigned_icu_id" %in% ts_cols
      where <- c(where, .ehr_member_pred(
        con, schema,
        data_cols = c("source_data", "src_admission_id",
                      if (icu) "assigned_icu_id"),
        ci_cols = c("source_data", "src_admission_id", if (icu) "src_icu_id"),
        level = if (icu) "icu" else "admission", available_cols = ts_cols,
        data_label = paste0('"', schema, '".ts'), alias = "t"))
    }
    if (!is.null(concepts) && length(concepts) > 0) {
      concept_list <- paste(
        sapply(concepts, function(x) DBI::dbQuoteLiteral(con, x)),
        collapse = ", "
      )
      where <- c(where, paste0("t.concept_name IN (", concept_list, ")"))
    }
    # Window filter: half-open [from, to) minutes, so c(0, 1440) is exactly
    # the first day and adjacent windows never share a row.
    if (!is.null(window_min)) {
      tcol <- if (window_ref == "icu") "time_since_icu_min" else "time_since_hadm_min"
      if (!tcol %in% ts_cols)
        stop('"', schema, '".ts has no ', tcol, " column for window_ref = '",
             window_ref, "'.", call. = FALSE)
      qtcol <- DBI::dbQuoteIdentifier(con, tcol)
      where <- c(where, sprintf("t.%s >= %s AND t.%s < %s",
        qtcol, format(window_min[1], scientific = FALSE, digits = 15),
        qtcol, format(window_min[2], scientific = FALSE, digits = 15)))
    }
    q <- sprintf('SELECT %s FROM "%s".ts t', select_sql, schema)
    if (length(where)) q <- paste0(q, " WHERE ", paste(where, collapse = " AND "))
    if (is.finite(n)) q <- paste0(q, " LIMIT ", as.integer(n))
    DBI::dbGetQuery(con, q)
  })
}

#' Pivot the merged time series into wide format (one column per concept)
#'
#' The merged-schema counterpart of `fetch_concepts_wide()`: reads the pooled
#' `ts` table built by `merge_sources()` and pivots it to wide format in SQL,
#' keyed by `ehr_id`, `source_data`, `src_patient_id`, and the requested
#' analysis grain. Use this
#' when you want a cross-source, `ehr_id`-keyed wide matrix; use
#' `fetch_concepts_wide()` to pivot a single source's `ts_tab` before merging.
#'
#' Shares the conditional-aggregation engine, binning, per-concept spec, and
#' column-naming rules with `fetch_concepts_wide()` - see there for details.
#'
#' @param schema Target schema name passed to `merge_sources()` (e.g.
#'   `"rendehr"`, `"trauma_project"`).
#' @param spec Named list mapping each concept to a character vector of
#'   aggregates. Supported: `"min"`, `"max"`, `"mean"`, `"median"`, `"sd"`,
#'   `"variance"`, `"sum"`, `"n"`/`"count"`, `"first"`, and `"last"`;
#'   `"all_numeric"` expands to all numeric reducers. An unnamed character
#'   vector defaults each concept to `"mean"`; a named character vector is
#'   interpreted as concept = aggregate shorthand.
#' @param bin_min Bin width in minutes (default 60). `NULL` pivots on the raw
#'   timestamp. The emitted `t_bin` column is the 0-based bin index
#'   (`floor(time / bin_min)`), e.g. `0, 1, 2, ...` for hourly bins.
#' @param analysis_level `"admission"` (default) bins on `time_since_hadm_min`;
#'   `"icu"` bins on `time_since_icu_min`.
#' @param window Optional `c(from_bin, to_bin)` in bin-index units, inclusive
#'   both ends, restricting the output to those bins (applied in SQL before
#'   aggregation). E.g. with `bin_min = 60`, `c(0, 47)` keeps the first 48 h,
#'   `c(0, 0)` keeps only the first hour. `NULL` (default) keeps all bins.
#' @param complete_grid If TRUE, emit a row for every bin in `window` (or, when
#'   `window` is NULL, the observed bin range) for each stay present in `ts`,
#'   left-joining the aggregates onto the full grid so unmeasured bins come back
#'   as `NA`. No imputation is performed - fill/mask downstream in R. Note this
#'   regularizes shape only; it does NOT enforce length-of-stay eligibility, so
#'   still filter `icu_los` from `flat` to define the cohort. Requires a
#'   non-NULL `bin_min`. Default FALSE.
#' @param n Max rows to return. Default Inf.
#' @param cohort_only If TRUE (default), pivot only rows of cohort members
#'   (`cohort_index` in the merged `cohort_info`), matched at the requested
#'   `analysis_level`: `(source_data, src_admission_id)` for `"admission"`,
#'   plus `assigned_icu_id` for `"icu"` (which needs `cohort_info` built with
#'   `analysis_level = "icu"`). Applied in SQL before aggregation. FALSE reads
#'   all imported context rows; it cannot recover data that was never imported.
#' @param clean,ranges Optional range cleaning before aggregation, as in
#'   [fetch_concepts_wide()]: `clean = "range"` excludes readings outside the
#'   config (or `ranges`) bounds inside the SQL aggregates, adds a
#'   `<concept>_n_cleaned` column per concept and a `clean_report` attribute.
#'   Default `"none"`.
#' @param con Optional explicit DBI connection.
#' @param case_adm_only,scope_only Removed; supplying either is an error. Use
#'   `cohort_only`.
#' @return A wide data frame: `ehr_id`, `source_data`, `src_patient_id`, the
#'   merge grain key (`src_admission_id`, plus `assigned_icu_id` for ICU-level
#'   analysis), the `t_bin` index column, then one column per concept and aggregate,
#'   named `<concept>_<aggregate>`, or just `<concept>` when the concept has one
#'   aggregate (followed by `<concept>_n_cleaned` with `clean = "range"`).
#' @export
#'
#' @examples
#' \dontrun{
#' load_merged_ts_wide("rendehr", spec = list(
#'   hr = c("mean", "max"), sbp = "mean", gcs = "last"
#' ), bin_min = 60)
#' load_merged_ts_wide("rendehr", spec = list(hr = "mean"), cohort_only = FALSE)
#'
#' # First 48 h on a complete hourly grid (NA where unmeasured), ICU clock
#' load_merged_ts_wide("rendehr", spec = list(hr = "last", sbp = "last"),
#'   bin_min = 60, analysis_level = "icu",
#'   window = c(0, 47), complete_grid = TRUE)
#' }
load_merged_ts_wide <- function(schema, spec, bin_min = 60,
                                analysis_level = c("admission", "icu"),
                                window = NULL, complete_grid = FALSE,
                                n = Inf, cohort_only = TRUE,
                                clean = c("none", "range"), ranges = NULL,
                                con = NULL, case_adm_only, scope_only) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  if (!missing(case_adm_only)) .ehr_removed_membership_arg("case_adm_only")
  if (!missing(scope_only)) .ehr_removed_membership_arg("scope_only")
  cohort_only    <- .ehr_check_cohort_only(cohort_only)
  analysis_level <- match.arg(analysis_level)
  clean          <- match.arg(clean)
  .ehr_check_wide_clean(clean, ranges)
  spec           <- .ehr_normalize_spec(spec)
  clean_bounds   <- if (clean == "range") .ehr_wide_clean_bounds(names(spec), ranges)

  .with_connection(con, function(con) {
    all_cols <- DBI::dbListFields(con, DBI::Id(schema = schema, table = "ts"))

    tcol <- if (analysis_level == "icu") "time_since_icu_min" else "time_since_hadm_min"
    if (!tcol %in% all_cols)
      stop("Time column '", tcol, "' not found in \"", schema, "\".ts.",
           call. = FALSE)

    id_cols <- .merged_wide_id_cols(all_cols, analysis_level, schema)
    if (length(id_cols) == 0)
      stop("No merged id columns (ehr_id/source_data/src_*_id) found in \"",
           schema, "\".ts.", call. = FALSE)

    cohort_pred <- NULL
    if (cohort_only) {
      icu <- analysis_level == "icu"
      cohort_pred <- .ehr_member_pred(
        con, schema,
        data_cols = c("source_data", "src_admission_id",
                      if (icu) "assigned_icu_id"),
        ci_cols = c("source_data", "src_admission_id", if (icu) "src_icu_id"),
        level = analysis_level, available_cols = all_cols,
        data_label = paste0('"', schema, '".ts'))
    }

    .ehr_pivot_wide(con, schema, "ts", id_cols, tcol, spec, bin_min, n,
                    cohort_pred = cohort_pred,
                    window = window, complete_grid = complete_grid,
                    clean_bounds = clean_bounds)
  })
}

#' Load the merged duration table from any merged schema
#'
#' The interval-valued counterpart of `load_merged_ts()`: reads the pooled
#' `dur` table built by `merge_sources(merge_dur = TRUE)` (medication
#' exposures, ventilation episodes, and similar start/end-valued concepts)
#' instead of point-in-time measurements.
#'
#' @param schema Target schema name passed to `merge_sources()` (e.g.
#'   `"rendehr"`, `"trauma_project"`).
#' @param n Max rows. Default Inf.
#' @param concepts Optional character vector to filter the `concept_name`
#'   column.
#' @param cohort_only If TRUE (default), return only rows of cohort members
#'   (`cohort_index` in the merged `cohort_info`). `dur` has no ICU projection,
#'   so the match is at admission grain on `(source_data, src_admission_id)`:
#'   filtering does not make duration data ICU-specific. Filter runs in SQL.
#'   FALSE reads all imported context rows; it cannot recover data that was
#'   never imported.
#' @param con Optional explicit DBI connection.
#' @param case_adm_only,scope_only Removed; supplying either is an error. Use
#'   `cohort_only`.
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' dur <- load_merged_dur("rendehr")
#' dur <- load_merged_dur("rendehr", concepts = c("norepinephrine_exp", "propofol_exp"))
#' dur <- load_merged_dur("rendehr", cohort_only = FALSE)
#' }
load_merged_dur <- function(schema, n = Inf, concepts = NULL,
                            cohort_only = TRUE, con = NULL, case_adm_only, scope_only) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  if (!missing(case_adm_only)) .ehr_removed_membership_arg("case_adm_only")
  if (!missing(scope_only)) .ehr_removed_membership_arg("scope_only")
  cohort_only <- .ehr_check_cohort_only(cohort_only)
  .with_connection(con, function(con) {
    where <- character(0)
    if (cohort_only) {
      dur_cols <- DBI::dbListFields(con, DBI::Id(schema = schema, table = "dur"))
      where <- c(where, .ehr_member_pred(
        con, schema,
        data_cols = c("source_data", "src_admission_id"),
        ci_cols = c("source_data", "src_admission_id"),
        level = "admission", available_cols = dur_cols,
        data_label = paste0('"', schema, '".dur'), alias = "t"))
    }
    if (!is.null(concepts) && length(concepts) > 0) {
      concept_list <- paste(
        sapply(concepts, function(x) DBI::dbQuoteLiteral(con, x)),
        collapse = ", "
      )
      where <- c(where, paste0("t.concept_name IN (", concept_list, ")"))
    }
    q <- sprintf('SELECT t.* FROM "%s".dur t', schema)
    if (length(where)) q <- paste0(q, " WHERE ", paste(where, collapse = " AND "))
    if (is.finite(n)) q <- paste0(q, " LIMIT ", as.integer(n))
    DBI::dbGetQuery(con, q)
  })
}

# Build the SQL that expands each dur row into one row per bin it overlaps,
# with `value` = minutes of that bin actually covered by the episode.
# start_offset_min is derived (end_offset_min - duration_min) since dur only
# stores the end offset directly. Bins are treated as half-open
# [bin*bin_min, (bin+1)*bin_min); a zero-duration ("point") dur row still
# contributes exactly one row (the bin containing its start) with value = 0,
# so `n`/`count` still see it even though duration-based aggregates
# (`sum`/`mean`/...) correctly contribute nothing from it. Output columns:
# id_cols..., concept_name, bin_time (bin-start, minutes since admission),
# value (minutes exposed in that bin).
.ehr_dur_bin_expand_sql <- function(con, schema, id_cols, concepts, bin_min,
                                    cohort_pred = NULL) {
  id_sql <- paste(DBI::dbQuoteIdentifier(con, id_cols), collapse = ", ")
  concept_in <- paste(
    vapply(concepts, function(x) DBI::dbQuoteLiteral(con, x), character(1)),
    collapse = ", ")
  where_sql <- sprintf(
    "concept_name IN (%s) AND end_offset_min IS NOT NULL AND duration_min IS NOT NULL",
    concept_in)
  if (!is.null(cohort_pred) && nzchar(cohort_pred))
    where_sql <- paste0(where_sql, "\n  AND ", cohort_pred)

  sprintf(
    "SELECT %s, concept_name,
       (b.bin * %s)::numeric AS bin_time,
       GREATEST(
         LEAST(end_offset_min, (b.bin + 1) * %s) -
         GREATEST(end_offset_min - duration_min, b.bin * %s),
         0
       )::text AS value
     FROM %s.dur,
       LATERAL generate_series(
         floor((end_offset_min - duration_min) / %s)::int,
         GREATEST(
           floor((end_offset_min - duration_min) / %s)::int,
           ceil(end_offset_min / %s)::int - 1
         )
       ) AS b(bin)
     WHERE %s",
    id_sql, bin_min, bin_min, bin_min,
    DBI::dbQuoteIdentifier(con, schema),
    bin_min, bin_min, bin_min,
    where_sql
  )
}

#' Pivot the merged duration table into a time-binned wide grid
#'
#' The interval-valued counterpart of `load_merged_ts_wide()`. A `dur` row is
#' an interval (start/end), not a point measurement, so unlike `ts` (where
#' the same static table supports any `bin_min` on demand), each row is first
#' expanded across every bin it overlaps - computing how many minutes of that
#' bin were actually covered by the episode - before the same
#' `FILTER`-based conditional-aggregation pivot `load_merged_ts_wide()` uses
#' is applied to the expanded result (materialized as a temp table, dropped
#' when the call returns). The expansion is specific to the requested
#' `bin_min` and is rebuilt fresh on every call.
#'
#' A zero-duration ("point") `dur` row still contributes exactly one row (the
#' bin containing its start) with 0 exposed minutes, so `n`/`count` still see
#' it even though duration-based aggregates (`sum`/`mean`/...) correctly
#' contribute nothing from it.
#'
#' @param schema Target schema name passed to `merge_sources()` (e.g.
#'   `"rendehr"`, `"trauma_project"`).
#' @param spec Named list mapping each concept to a character vector of
#'   aggregates, applied to minutes-exposed-per-bin (not the raw `dur`
#'   `value` column). Same aggregate names/shorthand as
#'   `load_merged_ts_wide()`: `"min"`, `"max"`, `"mean"`, `"median"`, `"sd"`,
#'   `"variance"`, `"sum"`, `"n"`/`"count"`, `"first"`, `"last"`,
#'   `"all_numeric"`.
#' @param bin_min Bin width in minutes (default 60). The emitted `t_bin`
#'   column is the 0-based bin index (`floor(time / bin_min)`).
#' @param window Optional `c(from_bin, to_bin)` in bin-index units, inclusive
#'   both ends, restricting the output to those bins. `NULL` (default) keeps
#'   all bins.
#' @param complete_grid If TRUE, emit a row for every bin in `window` (or,
#'   when `window` is NULL, the observed bin range) for each admission
#'   present, left-joining the aggregates onto the full grid so unexposed
#'   bins come back as `NA` (not 0 - no imputation is performed; fill/mask
#'   downstream in R). Default FALSE.
#' @param n Max rows to return. Default Inf.
#' @param cohort_only If TRUE (default), restrict to rows of cohort members
#'   (`cohort_index` in the merged `cohort_info`), matched at admission grain
#'   on `(source_data, src_admission_id)`; `dur` has no ICU projection. Applied
#'   before the expansion. FALSE reads all imported context rows.
#' @param con Optional explicit DBI connection.
#' @param case_adm_only,scope_only Removed; supplying either is an error. Use
#'   `cohort_only`.
#' @return A wide data frame: id columns, `t_bin`, then one column per
#'   concept and aggregate (minutes exposed), named `<concept>_<aggregate>`, or
#'   just `<concept>` when the concept has one aggregate.
#' @export
#'
#' @examples
#' \dontrun{
#' load_merged_dur_wide("rendehr", spec = list(
#'   norepinephrine_exp = c("sum", "n")
#' ), bin_min = 1440)
#' load_merged_dur_wide("rendehr", spec = list(propofol_exp = "sum"),
#'   bin_min = 60, window = c(0, 47), complete_grid = TRUE)
#' }
load_merged_dur_wide <- function(schema, spec, bin_min = 60,
                                 window = NULL, complete_grid = FALSE,
                                 n = Inf, cohort_only = TRUE, con = NULL,
                                 case_adm_only, scope_only) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  if (!missing(case_adm_only)) .ehr_removed_membership_arg("case_adm_only")
  if (!missing(scope_only)) .ehr_removed_membership_arg("scope_only")
  cohort_only <- .ehr_check_cohort_only(cohort_only)
  spec <- .ehr_normalize_spec(spec)

  .with_connection(con, function(con) {
    all_cols <- DBI::dbListFields(con, DBI::Id(schema = schema, table = "dur"))
    id_cols <- intersect(
      c("ehr_id", "source_data", "src_patient_id", "src_admission_id", "src_icu_id"),
      all_cols
    )
    if (length(id_cols) == 0)
      stop("No merged id columns (ehr_id/source_data/src_*_id) found in \"",
           schema, "\".dur.", call. = FALSE)

    cohort_pred <- NULL
    if (cohort_only) {
      cohort_pred <- .ehr_member_pred(
        con, schema,
        data_cols = c("source_data", "src_admission_id"),
        ci_cols = c("source_data", "src_admission_id"),
        level = "admission", available_cols = all_cols,
        data_label = paste0('"', schema, '".dur'))
    }

    concepts   <- names(spec)
    expand_sql <- .ehr_dur_bin_expand_sql(con, schema, id_cols, concepts,
                                          bin_min, cohort_pred)

    tmp <- .ehr_validate_db_identifier(
      paste0("dur_wide_expand_", as.integer(Sys.time()), "_",
             sample.int(1e6, 1)),
      "temp table name")
    tmp_id <- DBI::dbQuoteIdentifier(con, DBI::Id(schema = "pg_temp", table = tmp))
    DBI::dbExecute(con, sprintf("CREATE TEMP TABLE %s AS %s", tmp_id, expand_sql))
    on.exit(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", tmp_id)), add = TRUE)

    .ehr_pivot_wide(con, "pg_temp", tmp, id_cols, "bin_time", spec, bin_min, n,
                    cohort_pred = NULL, window = window,
                    complete_grid = complete_grid)
  })
}

#' Load the merged flat table from any merged schema
#'
#' @param schema Target schema name passed to merge_sources().
#' @param n Max rows. Default Inf.
#' @param cohort_only If TRUE (default), return only rows of cohort members
#'   (`cohort_index` in the merged `cohort_info`), matched at admission grain
#'   on `(source_data, src_admission_id)`; flat data do not become ICU-specific
#'   through filtering. FALSE reads all rows.
#' @param case_adm_only,scope_only Removed; supplying either is an error. Use
#'   `cohort_only`.
#' @param con Optional explicit DBI connection.
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- load_merged_flat("rendehr")
#' flat <- load_merged_flat("trauma_project", cohort_only = FALSE)
#' }
load_merged_flat <- function(schema, n = Inf, con = NULL, cohort_only = TRUE,
                             case_adm_only, scope_only) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  if (!missing(case_adm_only)) .ehr_removed_membership_arg("case_adm_only")
  if (!missing(scope_only)) .ehr_removed_membership_arg("scope_only")
  cohort_only <- .ehr_check_cohort_only(cohort_only)
  .with_connection(con, function(con) {
    q <- sprintf('SELECT * FROM "%s".flat', schema)
    if (cohort_only) {
      flat_cols <- DBI::dbListFields(con, DBI::Id(schema = schema, table = "flat"))
      q <- paste0(q, " WHERE ", .ehr_member_pred(
        con, schema,
        data_cols = c("source_data", "src_admission_id"),
        ci_cols = c("source_data", "src_admission_id"),
        level = "admission", available_cols = flat_cols,
        data_label = paste0('"', schema, '".flat')))
    }
    q <- paste0(q, .ehr_merged_order_by(con, schema, "flat"))
    if (is.finite(n)) q <- paste0(q, " LIMIT ", as.integer(n))
    DBI::dbGetQuery(con, q)
  })
}

# Rows of a merged table in a fixed order (source, patient, admission, ICU
# stay), so `n =` and head() give the same rows on every run.
.ehr_merged_order_by <- function(con, schema, table) {
  cols <- DBI::dbListFields(con, DBI::Id(schema = schema, table = table))
  keys <- intersect(c("source_data", "src_patient_id", "src_admission_id", "src_icu_id"), cols)
  if (!length(keys)) return("")
  paste0(" ORDER BY ", paste(DBI::dbQuoteIdentifier(con, keys), collapse = ", "))
}

#' Load the merged cohort_info table from any merged schema
#'
#' @param schema Target schema name passed to merge_sources().
#' @param n Max rows. Default Inf.
#' @param src Optional character vector to filter by source_data column.
#' @param cohort_only If TRUE (default), return only rows of cohort members
#'   (`cohort_index`). FALSE also returns imported patient-context rows.
#' @param con Optional explicit DBI connection.
#' @param case_adm_only,scope_only Removed; supplying either is an error. Use
#'   `cohort_only`.
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' ci <- load_merged_cohort_info("rendehr")
#' ci <- load_merged_cohort_info("trauma_project", cohort_only = FALSE)
#' }
load_merged_cohort_info <- function(schema, n = Inf, src = NULL,
                                    cohort_only = TRUE, con = NULL,
                                    case_adm_only, scope_only) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  if (!missing(case_adm_only)) .ehr_removed_membership_arg("case_adm_only")
  if (!missing(scope_only)) .ehr_removed_membership_arg("scope_only")
  cohort_only <- .ehr_check_cohort_only(cohort_only)
  .with_connection(con, function(con) {
    q <- sprintf('SELECT * FROM "%s".cohort_info', schema)
    where <- character(0)
    if (!is.null(src) && length(src) > 0) {
      src_list <- paste(
        sapply(src, function(x) DBI::dbQuoteLiteral(con, x)),
        collapse = ", "
      )
      where <- c(where, paste0("source_data IN (", src_list, ")"))
    }
    if (cohort_only) {
      .ehr_member_ci_cols(con, schema)
      where <- c(where, "cohort_index::boolean")
    }
    if (length(where) > 0) q <- paste0(q, " WHERE ", paste(where, collapse = " AND "))
    q <- paste0(q, .ehr_merged_order_by(con, schema, "cohort_info"))
    if (is.finite(n)) q <- paste0(q, " LIMIT ", as.integer(n))
    DBI::dbGetQuery(con, q)
  })
}

# ___________________________________________________________________________
# Cohort summary
# ___________________________________________________________________________

#' High-level summary of a source cohort
#'
#' Counts the cohort's patients, admissions and ICU stays and the patients by
#' sex in flat_tab (where each table exists). Useful as a sanity check after
#' running the pipeline.
#'
#' @param src_name Source name.
#' @param con Optional explicit DBI connection.
#' @return Invisibly, a named list of the counts it prints: `source`,
#'   `icu_windows_rows` and `icu_patients` (ICU stays and the patients who had
#'   one), `cohort_info_rows`, `patients` and `admissions` (distinct cohort
#'   patients and hospital admissions), `concept_tables`, `ts_rows`,
#'   `flat_rows` and `sex` (distinct patients by recorded sex; a patient whose
#'   admissions record different values counts under each), each present when
#'   its table exists.
#' @export
#'
#' @examples
#' \dontrun{
#' cohort_summary("mimic_iv_demo")
#' }
cohort_summary <- function(src_name, con = NULL) {
  src_name <- .ehr_validate_db_identifier(src_name, "source name")
  schema_names <- src_schema_names(src_name)
  ids <- .src_id_map(src_name)

  .with_connection(con, function(con) {
    msg <- function(...) message(...)
    out <- list(source = src_name)
    num <- function(x) as.numeric(x)

    existing <- DBI::dbGetQuery(con,
      "SELECT schema_name FROM information_schema.schemata"
    )$schema_name

    msg("\n--- Cohort summary: ", src_name, " ---")

    # icu_windows
    if (schema_names$raw %in% existing) {
      vw_tables <- DBI::dbGetQuery(con, sprintf(
        "SELECT table_name FROM information_schema.tables WHERE table_schema = '%s'",
        schema_names$raw
      ))$table_name

      if ("icu_windows" %in% vw_tables) {
        vw <- DBI::dbGetQuery(con, sprintf(
          "SELECT COUNT(*) AS rows,
                  COUNT(DISTINCT %s) AS patients
           FROM %s.icu_windows",
          ids[["src_patient_id"]], schema_names$raw
        ))
        out$icu_windows_rows <- num(vw$rows); out$icu_patients <- num(vw$patients)
        msg("  icu_windows   : ", format(out$icu_windows_rows, big.mark = ","),
            " ICU stays | ", format(out$icu_patients, big.mark = ","), " patients")
      }

      if ("cohort_info" %in% vw_tables) {
        si <- DBI::dbGetQuery(con, sprintf(
          "SELECT COUNT(*) AS rows, COUNT(DISTINCT %s) AS patients,
                  COUNT(DISTINCT %s) AS admissions
           FROM %s.cohort_info",
          ids[["src_patient_id"]], ids[["src_admission_id"]], schema_names$raw
        ))
        out$cohort_info_rows <- num(si$rows)
        out$patients <- num(si$patients); out$admissions <- num(si$admissions)
        msg("  cohort_info   : ", format(out$cohort_info_rows, big.mark = ","),
            " rows | ", format(out$patients, big.mark = ","), " patients | ",
            format(out$admissions, big.mark = ","), " admissions")
      }
    } else {
      msg("  (raw schema not found: ", schema_names$raw, ")")
    }

    # concepts
    if (schema_names$concepts %in% existing) {
      n_concepts <- DBI::dbGetQuery(con, sprintf(
        "SELECT COUNT(*) AS n FROM information_schema.tables
         WHERE table_schema = '%s'", schema_names$concepts
      ))$n
      out$concept_tables <- num(n_concepts)
      msg("  concepts      : ", out$concept_tables, " tables")
    }

    # ts / flat
    if (schema_names$harmonized %in% existing) {
      ts_tables <- DBI::dbGetQuery(con, sprintf(
        "SELECT table_name FROM information_schema.tables WHERE table_schema = '%s'",
        schema_names$harmonized
      ))$table_name

      if ("ts_tab" %in% ts_tables) {
        ts_n <- DBI::dbGetQuery(con, sprintf(
          "SELECT COUNT(*) AS n FROM %s.ts_tab", schema_names$harmonized
        ))$n
        out$ts_rows <- num(ts_n)
        msg("  ts_tab        : ", format(out$ts_rows, big.mark = ","), " rows")
      }

      if ("flat_tab" %in% ts_tables) {
        flat_n <- DBI::dbGetQuery(con, sprintf(
          "SELECT COUNT(*) AS n FROM %s.flat_tab", schema_names$harmonized
        ))$n
        out$flat_rows <- num(flat_n)
        if (flat_n > 0) {
          flat_patients <- DBI::dbGetQuery(con, sprintf(
            "SELECT COUNT(DISTINCT %s) AS n FROM %s.flat_tab",
            ids[["src_patient_id"]], schema_names$harmonized
          ))$n
          out$flat_patients <- num(flat_patients)
        }
        msg("  flat_tab      : ", format(out$flat_rows, big.mark = ","), " rows | ",
            format(out$flat_patients %||% 0, big.mark = ","), " patients")

        if (flat_n > 0) {
          sex_dist <- tryCatch(
            DBI::dbGetQuery(con, sprintf(
              "SELECT sex, COUNT(DISTINCT %s) AS n FROM %s.flat_tab GROUP BY sex ORDER BY n DESC",
              ids[["src_patient_id"]], schema_names$harmonized
            )),
            error = function(e) NULL
          )
          if (!is.null(sex_dist) && nrow(sex_dist) > 0) {
            # counts arrive as integer64: format them as numbers, not through apply()
            sex_dist$sex <- ifelse(is.na(sex_dist$sex), "(missing)", as.character(sex_dist$sex))
            sex_dist$n <- num(sex_dist$n)
            out$sex <- stats::setNames(as.list(sex_dist$n), sex_dist$sex)
            msg("  sex (flat patients, ",
                format(out$flat_patients %||% sum(sex_dist$n), big.mark = ","),
                "): ", paste0(sex_dist$sex, "=",
                format(sex_dist$n, big.mark = ",", trim = TRUE), collapse = ", "))
          }
        }
      }
    }

    msg("---\n")
    invisible(out)
  })
}

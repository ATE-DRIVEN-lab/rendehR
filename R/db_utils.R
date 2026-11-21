# Database utility functions for the ehr pipeline.

# ___________________________________________________________________________
# Credentials & source config
# ___________________________________________________________________________

.ehr_credential_missing <- function(x, allow_empty = FALSE) {
  placeholders <- c(
    "YOUR_PHYSIONET_USERNAME",
    "YOUR_PHYSIONET_PASSWORD",
    "YOUR_POSTGRES_PASSWORD"
  )
  is.null(x) || length(x) == 0L || anyNA(x) ||
    (is.character(x) && length(x) == 1L &&
       ((!isTRUE(allow_empty) && !nzchar(x)) || x %in% placeholders))
}

.ehr_first_credential <- function(..., allow_empty = FALSE) {
  vals <- list(...)
  for (x in vals) {
    if (!.ehr_credential_missing(x, allow_empty = allow_empty)) return(x)
  }
  NULL
}

.ehr_default_credentials_path <- function(path = NULL) {
  if (!is.null(path)) return(path)

  cfg <- .ehr_env$config %||% list()
  cfg_database <- cfg$database %||% list()
  cfg_path <- cfg_database$credentials_path
  base_dir <- cfg$project$base_dir %||% getwd()
  cfg_path <- .ehr_resolve_path(cfg_path, base_dir)
  if (!is.null(cfg_path)) return(cfg_path)

  default_path <- file.path("config", "config_pass.json")
  if (file.exists(default_path)) default_path else NULL
}

.ehr_credentials_template <- function(postgres_host = "localhost",
                                      postgres_port = 5432L,
                                      postgres_user = "postgres",
                                      postgres_password = "",
                                      physionet_username = NULL,
                                      physionet_password = NULL) {
  list(
    postgres = list(
      host = postgres_host,
      port = postgres_port,
      user = postgres_user,
      password = postgres_password
    ),
    physionet = list(
      username = physionet_username,
      password = physionet_password
    )
  )
}

#' Create a starter credentials file
#'
#' Writes a separate credentials JSON template for Postgres and PhysioNet.
#' This file is intentionally separate from `config/ehr_config.json` so users
#' can keep paths/settings apart from private credentials. Existing files are
#' not overwritten unless `overwrite = TRUE`.
#'
#' @param path Output path. Defaults to `config/config_pass.json`.
#' @param postgres_host Default Postgres host.
#' @param postgres_port Default Postgres port.
#' @param postgres_user Default Postgres user.
#' @param postgres_password Default Postgres password. Empty string means no
#'   password.
#' @param physionet_username Default PhysioNet username. NULL means ask at
#'   download time.
#' @param physionet_password Default PhysioNet password. NULL means ask at
#'   download time.
#' @param overwrite Overwrite an existing file? Default FALSE.
#' @return Invisibly returns `path`.
#' @examples
#' path <- file.path(tempdir(), "config_pass.json")
#' ehr_create_credentials_template(path, postgres_user = "postgres")
#' file.exists(path)
#' @export
ehr_create_credentials_template <- function(
    path = file.path("config", "config_pass.json"),
    postgres_host = "localhost",
    postgres_port = 5432L,
    postgres_user = "postgres",
    postgres_password = "",
    physionet_username = NULL,
    physionet_password = NULL,
    overwrite = FALSE) {
  if (file.exists(path) && !isTRUE(overwrite))
    stop("Credentials file already exists: ", path,
         "\nUse overwrite = TRUE to replace it.")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  creds <- .ehr_credentials_template(
    postgres_host = postgres_host,
    postgres_port = postgres_port,
    postgres_user = postgres_user,
    postgres_password = postgres_password,
    physionet_username = physionet_username,
    physionet_password = physionet_password
  )
  json <- jsonlite::toJSON(creds, pretty = TRUE, auto_unbox = TRUE,
                           null = "null")
  writeLines(json, path)
  message("rendehR: credentials template written to '", path, "'.")
  invisible(path)
}

# PhysioNet credentials are keyed `username`, but `user` (the Postgres key)
# is an easy slip. Accept it as an alias rather than dropping it silently,
# and refuse a block that sets both to different values.
.ehr_normalize_physionet <- function(physionet) {
  physionet <- physionet %||% list()
  alias <- physionet$user
  if (is.null(alias)) return(physionet)
  if (!.ehr_credential_missing(physionet$username) &&
      !identical(as.character(physionet$username), as.character(alias))) {
    stop("PhysioNet credentials set both `username` and `user` to different ",
         "values; keep only `username`.", call. = FALSE)
  }
  if (.ehr_credential_missing(physionet$username)) physionet$username <- alias
  physionet$user <- NULL
  physionet
}

.ehr_normalize_creds <- function(creds) {
  if (is.list(creds) && !is.null(creds$physionet))
    creds$physionet <- .ehr_normalize_physionet(creds$physionet)
  creds
}

#' Load Postgres and PhysioNet credentials
#'
#' Postgres fields are resolved from environment variables, then the nested
#' JSON credentials file, then documented defaults. Environment variables
#' (`PGHOST`, `PGPORT`, `PGUSER`, `PGPASSWORD`, `PGSSLMODE`) therefore override
#' the file. `sslmode` defaults to `"prefer"` (use TLS when the server offers
#' it); set `postgres.sslmode` or `PGSSLMODE` to `"disable"` or `"require"` to
#' change that. PhysioNet uses only
#' `PHYSIONET_USER` and `PHYSIONET_PASS`, followed by nested
#' `physionet.username` (`physionet.user` is accepted as an alias) and
#' `physionet.password`. Missing PhysioNet credentials
#' are allowed until a protected operation calls [set_physionet_env()].
#' One credential set is retained for the selected file/project during the
#' active R session and is replaced when that safe source identity changes.
#'
#' @param path Optional path to a JSON credentials file. If NULL, uses the
#'   active project config credentials path, then `config/config_pass.json` if
#'   it exists.
#' @return Named list with `postgres` and `physionet` sub-lists.
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#' names(creds)
#' }
#' @export
load_credentials <- function(path = NULL) {
  path <- .ehr_default_credentials_path(path)
  source_id <- if (is.null(path)) "<environment>" else
    normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!is.null(.ehr_env$creds) &&
      identical(.ehr_env$credential_source, source_id)) {
    return(.ehr_env$creds)
  }
  cfg <- list()
  if (!is.null(path) && file.exists(path))
    cfg <- jsonlite::read_json(path, simplifyVector = TRUE)

  file_pg <- cfg$postgres %||% list()
  file_phys <- .ehr_normalize_physionet(cfg$physionet)

  pg <- list(
    host = .ehr_first_credential(Sys.getenv("PGHOST", unset = NA_character_),
                                 file_pg$host, "localhost"),
    port = .ehr_first_credential(Sys.getenv("PGPORT", unset = NA_character_),
                                 file_pg$port, 5432L),
    user = .ehr_first_credential(Sys.getenv("PGUSER", unset = NA_character_),
                                 file_pg$user, "postgres"),
    password = .ehr_first_credential(
      Sys.getenv("PGPASSWORD", unset = NA_character_),
      file_pg$password,
      allow_empty = TRUE
    ),
    sslmode = .ehr_first_credential(Sys.getenv("PGSSLMODE", unset = NA_character_),
                                    file_pg$sslmode, "prefer")
  )
  pg$port <- as.integer(pg$port)

  if (is.null(pg$password)) {
    if (interactive()) {
      pg$password <- get_cred(NULL, "PGPASSWORD",
                              "Postgres password (press Enter if none): ")
    } else {
      pg$password <- ""
    }
  }

  retired <- c("RICU_PHYSIONET_USER", "RICU_PHYSIONET_PASS")
  present <- retired[nzchar(Sys.getenv(retired, unset = ""))]
  if (length(present)) {
    stop(
      "Retired PhysioNet environment variable(s) detected: ",
      paste(present, collapse = ", "),
      ". Rename them to PHYSIONET_USER and PHYSIONET_PASS.",
      call. = FALSE
    )
  }

  phys <- list(
    username = .ehr_first_credential(
      Sys.getenv("PHYSIONET_USER", unset = NA_character_),
      file_phys$username,
      allow_empty = FALSE
    ),
    password = .ehr_first_credential(
      Sys.getenv("PHYSIONET_PASS", unset = NA_character_),
      file_phys$password,
      allow_empty = FALSE
    )
  )
  creds <- list(postgres = pg, physionet = phys)
  .ehr_env$creds <- creds
  .ehr_env$credential_source <- source_id
  creds
}

#' Set credentials for a protected PhysioNet operation
#'
#' Requires non-missing values and sets only `PHYSIONET_USER` and
#' `PHYSIONET_PASS`. Credential values are never included in diagnostics.
#' @param creds List returned by load_credentials().
#' @return The logical vector returned by `Sys.setenv()`, invisibly.
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#' set_physionet_env(creds)
#' }
#' @export
set_physionet_env <- function(creds) {
  physionet <- .ehr_normalize_physionet(creds$physionet)
  if (interactive()) {
    if (.ehr_credential_missing(physionet$username)) {
      physionet$username <- readline("PhysioNet username: ")
    }
    if (.ehr_credential_missing(physionet$password)) {
      physionet$password <- readline("PhysioNet password: ")
    }
  }
  if (.ehr_credential_missing(physionet$username) ||
      .ehr_credential_missing(physionet$password)) {
    stop(
      "PhysioNet credentials are required for this protected operation. Set ",
      "PHYSIONET_USER and PHYSIONET_PASS or provide nested PhysioNet fields ",
      "in the credentials file.",
      call. = FALSE
    )
  }
  Sys.setenv(
    PHYSIONET_USER = physionet$username,
    PHYSIONET_PASS = physionet$password
  )
}

#' Load a single source config block from a data-sources JSON file
#' @param config_path Path to the canonical `rendehr-data-source.json`.
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @return Named list of source config fields.
#' @examples
#' \dontrun{
#' cfg <- load_src_cfg("config/ehr_config.json", "mimic_iv_demo")
#' }
#' @export
load_src_cfg <- function(config_path, src_name) {
  all_cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
  for (entry in all_cfg) {
    if (identical(entry$name, src_name)) return(entry)
  }
  available <- vapply(all_cfg, function(e) e$name %||% "???", character(1))
  stop("Source '", src_name, "' not found in JSON. Available: ",
       paste(available, collapse = ", "))
}

# ___________________________________________________________________________
# Connection management
# ___________________________________________________________________________

# Database identifiers are configuration, not SQL text. Keep the accepted
# portable identifier surface deliberately narrow and quote again at execution.
.ehr_validate_db_identifier <- function(x, kind = "identifier") {
  if (length(x) != 1L || is.na(x) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", x))
    stop("Invalid ", kind, ": expected ^[A-Za-z_][A-Za-z0-9_]*$.",
         call. = FALSE)
  x
}

# Point bare table names at `schemas` (first one receives CREATEs, the rest
# are read fallbacks). PostgreSQL takes `SET search_path TO a, b`; DuckDB's
# SET only accepts one scalar, so there the list goes in as one string.
.ehr_set_search_path <- function(con, schemas) {
  schemas <- vapply(schemas, .ehr_validate_db_identifier, character(1),
                    kind = "schema name", USE.NAMES = FALSE)
  sql <- if (inherits(con, "duckdb_connection")) {
    sprintf("SET search_path = '%s'", paste(schemas, collapse = ","))
  } else {
    paste0("SET search_path TO ", paste(
      vapply(schemas, function(s) as.character(DBI::dbQuoteIdentifier(con, s)),
             character(1)), collapse = ", "))
  }
  DBI::dbExecute(con, sql, immediate = TRUE)
}

# libpq sslmode for a credentials list: its postgres$sslmode, else PGSSLMODE,
# else "prefer".
.ehr_pg_sslmode <- function(creds) {
  mode <- creds$postgres$sslmode
  if (.ehr_credential_missing(mode)) mode <- Sys.getenv("PGSSLMODE", unset = "")
  if (!nzchar(mode)) mode <- "prefer"
  allowed <- c("disable", "allow", "prefer", "require", "verify-ca", "verify-full")
  if (!mode %in% allowed)
    stop("Invalid PostgreSQL sslmode '", mode, "'; use one of: ",
         paste(allowed, collapse = ", "), ".", call. = FALSE)
  mode
}

.ehr_protected_database <- function(x) {
  tolower(x) %in% c("postgres", "template0", "template1")
}

#' Open a connection to a Postgres database
#'
#' General-purpose connection helper: `db_name` is required, with no
#' default, so this connects to whatever database you actually name -
#' not tied to any database called "rendehr" specifically.
#' @param db_name Database name to connect to (e.g. "rendehrdov" below is
#'   just an example database name, not a requirement).
#' @param creds List returned by load_credentials(). `creds$postgres$sslmode`
#'   (or `PGSSLMODE`) sets the libpq SSL mode; the default is `"prefer"`.
#' @return DBI connection object.
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#' con <- connect_postgres("rendehr", creds)
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#' }
#' @export
connect_postgres <- function(db_name, creds) {
  db_name <- .ehr_validate_db_identifier(db_name, "database name")
  connect <- function(password) .ehr_pg_dbconnect(
    dbname   = db_name,
    host     = creds$postgres$host     %||% "localhost",
    port     = creds$postgres$port     %||% 5432L,
    user     = creds$postgres$user     %||% "postgres",
    password = password,
    sslmode  = .ehr_pg_sslmode(creds)
  )
  auth_failed <- function(e) grepl("password authentication failed|no password supplied",
                                   conditionMessage(e), ignore.case = TRUE)
  con <- tryCatch(connect(creds$postgres$password), error = identity)
  # A wrong stored password: ask again in an interactive session (empty
  # answer stops) and keep the accepted one for this session only.
  tries <- 0L
  while (inherits(con, "error") && auth_failed(con) && is_interactive() && tries < 3L) {
    tries <- tries + 1L
    message("PostgreSQL refused the password for user '",
            creds$postgres$user %||% "postgres", "'. Enter it again (empty to stop).")
    password <- .ehr_readline("PostgreSQL password: ")
    if (!nzchar(password)) break
    con <- tryCatch(connect(password), error = identity)
    if (!inherits(con, "error") && !is.null(.ehr_env$creds) &&
        identical(.ehr_env$creds$postgres$user, creds$postgres$user) &&
        identical(.ehr_env$creds$postgres$host, creds$postgres$host))
      .ehr_env$creds$postgres$password <- password
  }
  if (inherits(con, "error")) {
    if (auth_failed(con))
      stop("PostgreSQL refused the password for user '", creds$postgres$user %||% "postgres",
           "'. Correct it in the credentials file (postgres$password) or PGPASSWORD.",
           call. = FALSE)
    stop(con)
  }
  con
}

# DBI::dbConnect() behind a seam, so the password prompt can be tested.
# Server NOTICEs ("table ... does not exist, skipping" from DROP ... IF EXISTS)
# are noise to the user; warnings and errors still come through.
.ehr_pg_quiet <- "-c client_min_messages=warning"
.ehr_pg_dbconnect <- function(...) DBI::dbConnect(RPostgres::Postgres(), ..., options = .ehr_pg_quiet)

#' Create the target database if it does not exist
#' @param db_name Database name to create.
#' @param creds List returned by load_credentials().
#' @return `NULL`, invisibly.
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#' ensure_database_exists("rendehr", creds)
#' }
#' @export
ensure_database_exists <- function(db_name, creds) {
  db_name <- .ehr_validate_db_identifier(db_name, "database name")
  if (.ehr_protected_database(db_name))
    stop("Protected PostgreSQL database target refused: ", db_name, call. = FALSE)
  admin_con <- DBI::dbConnect(
    RPostgres::Postgres(), options = .ehr_pg_quiet,
    dbname   = "postgres",
    host     = creds$postgres$host     %||% "localhost",
    port     = creds$postgres$port     %||% 5432L,
    user     = creds$postgres$user     %||% "postgres",
    password = creds$postgres$password,
    sslmode  = .ehr_pg_sslmode(creds)
  )
  on.exit(DBI::dbDisconnect(admin_con), add = TRUE)

  exists <- DBI::dbGetQuery(
    admin_con,
    paste0("SELECT 1 FROM pg_database WHERE datname = ",
           DBI::dbQuoteString(admin_con, db_name))
  )
  if (nrow(exists) == 0)
    DBI::dbExecute(admin_con, paste0(
      "CREATE DATABASE ", DBI::dbQuoteIdentifier(admin_con, db_name)))
  invisible(NULL)
}

#' Drop a Postgres database entirely
#'
#' Connects as admin (to the "postgres" database) and drops the target
#' database. Forces disconnection of any open sessions first. Dry-run by
#' default.
#'
#' @param db_name Database to drop.
#' @param creds Credentials list (from load_credentials()). Uses stored
#'   ehr_connect() creds if NULL.
#' @param dry_run If TRUE (default), only print what would be dropped.
#' @param confirm The database name typed by the user to confirm the drop. If
#'   `NULL` (default), an interactive session asks for it at the console; a
#'   non-interactive session cannot confirm, so nothing is dropped. Apps pass
#'   the name the user typed in their own confirmation dialog.
#' @return `TRUE` invisibly when the database was dropped, `FALSE` when the
#'   drop was not confirmed, `NULL` for a dry run.
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#' drop_database("rendehr_scratch", creds)                   # dry run: reports only
#' drop_database("rendehr_scratch", creds, dry_run = FALSE)
#' drop_database("rendehr_scratch", creds, dry_run = FALSE, confirm = "rendehr_scratch")
#' }
#' @export
drop_database <- function(db_name, creds = NULL, dry_run = TRUE, confirm = NULL) {
  db_name <- .ehr_validate_db_identifier(db_name, "database name")
  if (.ehr_protected_database(db_name))
    stop("Protected PostgreSQL database target refused: ", db_name, call. = FALSE)
  creds <- creds %||% .ehr_env$creds %||% stop("No credentials: call ehr_connect() or pass creds.")

  if (dry_run) {
    message("[dry_run] Would drop database: ", db_name)
    return(invisible(NULL))
  }

  if (is.null(confirm))
    confirm <- if (interactive()) readline(sprintf("Type the database name to confirm deletion [%s]: ", db_name)) else ""
  if (!identical(confirm, db_name)) {
    message("Aborted - name did not match.")
    return(invisible(FALSE))
  }

  admin_con <- DBI::dbConnect(
    RPostgres::Postgres(), options = .ehr_pg_quiet,
    dbname   = "postgres",
    host     = creds$postgres$host     %||% "localhost",
    port     = creds$postgres$port     %||% 5432L,
    user     = creds$postgres$user     %||% "postgres",
    password = creds$postgres$password,
    sslmode  = .ehr_pg_sslmode(creds)
  )
  on.exit(DBI::dbDisconnect(admin_con), add = TRUE)

  DBI::dbExecute(admin_con, sprintf(
    "SELECT pg_terminate_backend(pid)
     FROM pg_stat_activity
     WHERE datname = %s AND pid <> pg_backend_pid()",
    DBI::dbQuoteString(admin_con, db_name)
  ))
  DBI::dbExecute(admin_con, paste0(
    "DROP DATABASE IF EXISTS ", DBI::dbQuoteIdentifier(admin_con, db_name)))
  message("Dropped database: ", db_name)
  invisible(TRUE)
}


# Resolve Postgres credentials for server-level browsing. If `config_path`
# points to config/config_pass.json, read that directly. If it points to a
# project config, load the project config and resolve its credentials path.
.ehr_psql_browse_creds <- function(creds = NULL, config_path = NULL) {
  if (!is.null(creds)) return(creds)

  if (!is.null(config_path)) {
    raw <- jsonlite::read_json(config_path, simplifyVector = TRUE)
    if (!is.null(raw$postgres)) {
      file_pg <- raw$postgres %||% list()
      pg <- list(
        host = .ehr_first_credential(Sys.getenv("PGHOST", unset = NA_character_),
                                     file_pg$host, "localhost"),
        port = .ehr_first_credential(Sys.getenv("PGPORT", unset = NA_character_),
                                     file_pg$port, 5432L),
        user = .ehr_first_credential(Sys.getenv("PGUSER", unset = NA_character_),
                                     file_pg$user, "postgres"),
        password = .ehr_first_credential(
          Sys.getenv("PGPASSWORD", unset = NA_character_),
          file_pg$password,
          allow_empty = TRUE
        )
      )
      pg$port <- as.integer(pg$port)
      if (is.null(pg$password)) pg$password <- ""
      return(list(postgres = pg, physionet = raw$physionet %||% list()))
    }

    cfg <- ehr_load_config(config_path)
    .ehr_env$config <- cfg
  }

  load_credentials(NULL)
}

#' Browse databases on a Postgres server
#'
#' Lists databases available on the Postgres server described by the supplied
#' credentials. This is server-level inspection: databases do not need to be
#' related to rendehR. By default, protected/system databases (`postgres`,
#' `template0`, `template1`) are hidden, but they can be shown with
#' `include_system = TRUE`.
#'
#' @param config_path Optional path to either `config/config_pass.json`
#'   (Postgres credentials) or `config/ehr_config.json` (project config pointing
#'   to credentials). If NULL, credentials are resolved from the active/default
#'   config and environment variables.
#' @param creds Optional credentials list from `load_credentials()`.
#' @param maintenance_db Database used only for the listing connection. Default
#'   is `"postgres"`.
#' @param include_system Include `postgres`, `template0`, and `template1`?
#'   Default FALSE.
#' @param include_unconnectable Include databases that do not allow connections?
#'   Default FALSE.
#' @return A data frame with database metadata and example browsing code.
#' @export
#'
#' @examples
#' \dontrun{
#' browse_psql(config_path = "config/config_pass.json")
#' browse_psql(config_path = "config/ehr_config.json")
#' browse_psql(include_system = TRUE)
#' }
browse_psql <- function(config_path = NULL, creds = NULL,
                        maintenance_db = "postgres",
                        include_system = FALSE,
                        include_unconnectable = FALSE) {
  creds <- .ehr_psql_browse_creds(creds = creds, config_path = config_path)

  con <- DBI::dbConnect(
    RPostgres::Postgres(), options = .ehr_pg_quiet,
    dbname   = maintenance_db,
    host     = creds$postgres$host     %||% "localhost",
    port     = creds$postgres$port     %||% 5432L,
    user     = creds$postgres$user     %||% "postgres",
    password = creds$postgres$password,
    sslmode  = .ehr_pg_sslmode(creds)
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  where <- character(0)
  if (!isTRUE(include_system)) {
    where <- c(where, "d.datname NOT IN ('postgres', 'template0', 'template1')")
  }
  if (!isTRUE(include_unconnectable)) {
    where <- c(where, "d.datallowconn IS TRUE")
  }
  where_sql <- if (length(where) > 0L) {
    paste("WHERE", paste(where, collapse = " AND "))
  } else {
    ""
  }

  out <- DBI::dbGetQuery(con, sprintf(
    "SELECT
       d.datname AS db_name,
       r.rolname AS owner,
       pg_encoding_to_char(d.encoding) AS encoding,
       d.datcollate AS collation,
       d.datctype AS ctype,
       d.datistemplate AS is_template,
       d.datallowconn AS allow_connections,
       has_database_privilege(d.datname, 'CONNECT') AS can_connect,
       d.datname IN ('postgres', 'template0', 'template1') AS is_system,
       pg_database_size(d.datname) AS size_bytes,
       pg_size_pretty(pg_database_size(d.datname)) AS size_pretty
     FROM pg_database d
     LEFT JOIN pg_roles r ON r.oid = d.datdba
     %s
     ORDER BY is_system, lower(d.datname)",
    where_sql
  ))

  if (nrow(out) > 0L) {
    db_arg <- vapply(out$db_name, encodeString, character(1), quote = '"')
    out$browse_code <- ifelse(
      out$allow_connections & out$can_connect,
      paste0("browse_ehr_db_flex(db_name = ", db_arg, ")"),
      NA_character_
    )
  } else {
    out$browse_code <- character(0)
  }

  out
}


# ___________________________________________________________________________
# Schema management
# ___________________________________________________________________________

#' Create all schemas in a schema_names list if they don't exist
#' @param con DBI connection.
#' @param schema_names Named list of schema name strings (from src_schema_names()).
#' @return `NULL`, invisibly.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' create_schemas(con, unlist(src_schema_names("mimic_iv_demo")))
#' }
#' @export
create_schemas <- function(con, schema_names) {
  for (s in unlist(schema_names)) {
    s <- .ehr_validate_db_identifier(s, "schema name")
    DBI::dbExecute(con, paste0(
      "CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, s), ";"))
  }
}

#' Create a Postgres index if it doesn't already exist
#' @param con DBI connection.
#' @param schema Schema name.
#' @param table Table name.
#' @param index_name Name to give the index.
#' @param columns Character vector of column names to index.
#' @return The value returned by `DBI::dbExecute()` for the `CREATE INDEX` statement.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' create_index(con, "mimic_iv_demo_coh", "labevents", "labevents_hadm_idx", c("hadm_id", "itemid"))
#' }
#' @export
create_index <- function(con, schema, table, index_name, columns) {
  vals <- c(schema, table, index_name, columns)
  invisible(lapply(vals, .ehr_validate_db_identifier))
  DBI::dbExecute(con, paste0(
    "CREATE INDEX IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, index_name),
    " ON ", DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table)),
    " (", paste(DBI::dbQuoteIdentifier(con, columns), collapse = ", "), ");"))
}

#' Write a data frame as a lookup table in Postgres
#' @param con DBI connection.
#' @param schema Schema name.
#' @param table_name Table name.
#' @param df Data frame to write.
#' @param overwrite Drop and recreate if TRUE (default).
#' @return `TRUE`, invisibly, as returned by `DBI::dbWriteTable()`.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' write_lookup_table(con, "public", "my_codes", data.frame(code = c("S14", "S24"), group = "SCI"))
#' }
#' @export
write_lookup_table <- function(con, schema, table_name, df, overwrite = TRUE) {
  .ehr_validate_db_identifier(schema, "schema name")
  .ehr_validate_db_identifier(table_name, "table name")
  id <- DBI::Id(schema = schema, table = table_name)
  if (overwrite)
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ",
      DBI::dbQuoteIdentifier(con, id), " CASCADE"))
  DBI::dbWriteTable(con, id,
                    as.data.frame(df), overwrite = FALSE, append = FALSE)
}

# ___________________________________________________________________________
# SQL execution helpers
# ___________________________________________________________________________

#' Execute a single .sql file against an open connection
#' @param con DBI connection.
#' @param path File path to a .sql file.
#' @return The value returned by `DBI::dbExecute()` for the file's SQL.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' run_sql_file(con, "sql/my_concept.sql")
#' }
#' @export
run_sql_file <- function(con, path) {
  sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
  DBI::dbExecute(con, sql, immediate = TRUE)
}

#' Execute all .sql files in a directory, sorted alphabetically
#' @param con DBI connection.
#' @param dir_path Directory containing .sql files.
#' @return `NULL`, invisibly.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' run_sql_dir(con, "sql/my_concepts")
#' }
#' @export
run_sql_dir <- function(con, dir_path) {
  files <- sort(list.files(dir_path, pattern = "\\.sql$", full.names = TRUE),
                method = "radix")
  for (f in files) run_sql_file(con, f)
}

# ___________________________________________________________________________
# Data access helpers
# ___________________________________________________________________________

#' Fetch a table (or the first n rows) from Postgres into a tibble
#' @param con DBI connection.
#' @param schema Schema name.
#' @param table Table name.
#' @param n Max rows to collect (default Inf = all).
#' @return A tibble.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' get_table_from_db(con, "mimic_iv_demo_coh", "patients", n = 10)
#' }
#' @export
get_table_from_db <- function(con, schema, table, n = Inf) {
  stopifnot(DBI::dbIsValid(con))
  tbl_id <- DBI::Id(schema = schema, table = table)

  if (requireNamespace("dbplyr", quietly = TRUE)) {
    tbl <- dplyr::tbl(con, tbl_id)
    return(if (is.finite(n)) dplyr::collect(tbl, n = n) else dplyr::collect(tbl))
  }

  if (!is.finite(n)) return(tibble::as_tibble(DBI::dbReadTable(con, tbl_id)))
  ref <- DBI::dbQuoteIdentifier(con, tbl_id)
  tibble::as_tibble(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT %d", ref, as.integer(n))))
}

# ___________________________________________________________________________
# Fuzzy name matching
# ___________________________________________________________________________

#' Suggest close matches for a query string against a set of choices
#' @param query Character string to match.
#' @param choices Character vector of candidate strings.
#' @param max_suggestions Maximum number of suggestions to return.
#' @return Character vector of up to max_suggestions matches.
#' @examples
#' suggest_match("mimic_iv_dem", c("mimic_iv_demo", "mimic_iii_demo", "eicu_demo"))
#' @export
suggest_match <- function(query, choices, max_suggestions = 3) {
  if (length(choices) == 0) return(character(0))
  query         <- tolower(query)
  choices_lower <- tolower(choices)

  prefix_hits <- choices[startsWith(choices_lower, query)]
  if (length(prefix_hits) > 0) return(head(prefix_hits, max_suggestions))

  substr_hits <- choices[grepl(query, choices_lower, fixed = TRUE)]
  if (length(substr_hits) > 0) return(head(substr_hits, max_suggestions))

  dists      <- utils::adist(query, choices_lower)
  norm_dists <- dists / pmax(nchar(choices_lower), 1)
  ord        <- order(norm_dists)
  keep       <- norm_dists[ord] <= 0.4
  if (!any(keep)) return(character(0))
  head(choices[ord][keep], max_suggestions)
}

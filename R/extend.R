#' Extension utilities: bring-your-own SQL + project workspace helpers.
#'
#' These functions cover two common scenarios:
#'   1. You found a SQL query somewhere and want to run it as a concept.
#'   2. You want a project-specific schema or database to keep derived work
#'      separate from the raw source schemas.

# ---------------------------------------------------------------------------
# register_concept_sql - adapt and execute an external SQL concept
# ---------------------------------------------------------------------------

#' Adapt and register an external SQL query as a concept
#'
#' Takes a SQL file (or inline SQL string) found online, in a paper supplement,
#' or written by hand and does three things:
#'   1. Strips known schema prefixes (mimiciv_hosp., mimiciv_icu., eicu_crd.,
#'      etc.) so bare table names resolve against search_path.
#'   2. Wraps the query in `CREATE TABLE {target_schema}.{concept_name} AS`.
#'   3. Optionally saves the adapted SQL and/or executes it immediately,
#'      registering provenance in concept_registry.
#'
#' After running, the concept is queryable with fetch_concept().
#'
#' @param sql Source SQL. Either a file path or an inline SQL string.
#' @param src_name Source name (e.g. "mimic_iv_demo"). Determines which
#'   schema prefixes to strip.
#' @param concept_name Name for the output table.
#' @param schema Target schema for the concept table. Accepts a key from
#'   src_schema_names() ("concepts", "derived", "flat", "ts", "dur") or any
#'   literal schema name. Default "concepts" -> _coh_concepts.
#' @param save_to Where to write the adapted SQL. Default `NULL` writes to
#'   `sql/{src_name}/concepts_sqls/user/{concept_name}.sql` (or
#'   `.../external/{module}/{concept_name}.sql` when `module` is given) so a
#'   later `run_concepts()` call picks it up automatically, the same as if
#'   you had dropped the file there by hand. Pass `FALSE` to skip saving, or
#'   an explicit path to save elsewhere.
#' @param module Optional external-layer module name. When given, the
#'   default `save_to` targets `external/{module}/` (its own schema on the
#'   next `run_concepts()` call) instead of `user/`. Ignored if `save_to` is
#'   given explicitly.
#' @param base_dir Project root directory, used only to resolve the default
#'   `save_to` path. Defaults to the current working directory.
#' @param dry_run If TRUE (default), only build/save the adapted SQL - do not
#'   execute it. Set FALSE (or pass `con`) to execute immediately.
#' @param overwrite Prepend DROP TABLE IF EXISTS before CREATE. Default FALSE.
#' @param origin_name Optional human-readable source name for provenance.
#' @param origin_url Optional URL (repo, paper, DOI) for provenance.
#' @param origin_ref Optional citation string for provenance.
#' @param con Optional explicit DBI connection. Implies dry_run = FALSE.
#' @return The adapted SQL string invisibly. Inspect it before executing.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehrdov", creds)
#'
#' # Preview adapted SQL from a file (does NOT execute, but does save to
#' # sql/mimic_iv_demo/concepts_sqls/user/sofa_renal_custom.sql by default)
#' adapted <- register_concept_sql(
#'   "downloads/sofa_renal.sql",
#'   src_name     = "mimic_iv_demo",
#'   concept_name = "sofa_renal_custom"
#' )
#' cat(adapted)
#'
#' # Execute, with provenance, keeping the default save location
#' register_concept_sql(
#'   "downloads/sofa_renal.sql",
#'   src_name     = "mimic_iv_demo",
#'   concept_name = "sofa_renal_custom",
#'   schema       = "derived",
#'   dry_run      = FALSE,
#'   overwrite    = TRUE,
#'   origin_name  = "Smith et al. 2023",
#'   origin_url   = "https://doi.org/10.xxxx",
#'   origin_ref   = "Smith J. et al., J Neurotrauma 2023"
#' )
#'
#' # Inline SQL string, executed immediately, not saved to disk
#' register_concept_sql(
#'   "SELECT subject_id, hadm_id, stay_id,
#'           charttime AS index_time,
#'           valuenum  AS value
#'    FROM   mimiciv_icu.chartevents
#'    WHERE  itemid = 220045",
#'   src_name     = "mimic_iv_demo",
#'   concept_name = "heart_rate_raw",
#'   save_to      = FALSE,
#'   dry_run      = FALSE
#' )
#' }
register_concept_sql <- function(sql,
                                  src_name,
                                  concept_name,
                                  schema      = "concepts",
                                  save_to     = NULL,
                                  module      = NULL,
                                  base_dir    = NULL,
                                  dry_run     = TRUE,
                                  overwrite   = FALSE,
                                  origin_name = NULL,
                                  origin_url  = NULL,
                                  origin_ref  = NULL,
                                  con         = NULL) {
  # 1. File vs inline string - file.exists() only reliable for short strings
  if (nchar(sql) < 500 && file.exists(sql)) {
    sql_text <- paste(readLines(sql, warn = FALSE), collapse = "\n")
  } else {
    sql_text <- sql
  }

  # 2. Strip known schema prefixes -> bare table names (synced with handcrafted_concepts.R)
  family <- src_family(src_name)
  prefixes <- switch(family,
    mimic_iv  = c(
      "mimiciv_derived.", "mimiciv_hosp.", "mimiciv_icu.", "mimic_iv.",
      "`physionet-data.mimiciv_derived`.",
      "`physionet-data.mimiciv_hosp`.", "`physionet-data.mimiciv_icu`.",
      "miiv_derived.", "miiv_hosp.", "miiv_icu.", "miiv.",
      "mimic4_derived.", "mimic4_hosp.", "mimic4_icu.", "mimic4.",
      "m4_derived.", "m4_hosp.", "m4_icu.", "m4."
    ),
    mimic_iii = c(
      "mimiciii.", "mimic_iii.",
      "miii_derived.", "miii.",
      "mimic3_derived.", "mimic3.",
      "m3_derived.", "m3."
    ),
    eicu = c(
      # See handcrafted_concepts.R for why the fully-backtick-quoted
      # `physionet-data.eicu_crd[.derived].table` patterns need their own
      # entries (longer patterns first) plus the unconditional backtick
      # strip below, unlike MIMIC's backtick-closes-at-dataset convention.
      "`physionet-data.eicu_crd_derived.",
      "`physionet-data.eicu_crd.",
      "eicu_crd.", "eicu."
    ),
    character(0)
  )
  for (p in prefixes)
    sql_text <- gsub(p, "", sql_text, fixed = TRUE)
  sql_text <- gsub("`", "", sql_text, fixed = TRUE)

  # 3. Remove any existing CREATE TABLE / DROP TABLE wrapper - keep the SELECT
  sql_text <- gsub(
    "(?i)^\\s*DROP\\s+TABLE\\s+(IF\\s+EXISTS\\s+)?\\S+\\s*;\\s*\n?",
    "", sql_text, perl = TRUE
  )
  sql_text <- sub(
    "(?i)^\\s*CREATE\\s+(UNLOGGED\\s+)?TABLE\\s+(IF\\s+NOT\\s+EXISTS\\s+)?\\S+\\s+AS\\s*",
    "", sql_text, perl = TRUE
  )
  sql_text <- trimws(sql_text)

  # 4. Add semicolon if missing
  if (!grepl(";\\s*$", sql_text)) sql_text <- paste0(sql_text, ";")

  # 5. Resolve target schema (key or literal name)
  schema_names   <- src_schema_names(src_name)
  target_schema  <- .ehr_schema_name(schema, schema_names)
  .ehr_validate_db_identifier(target_schema, "target schema")
  .ehr_validate_db_identifier(concept_name, "concept name")

  drop_line   <- sprintf('DROP TABLE IF EXISTS "%s"."%s";\n', target_schema, concept_name)
  create_line <- sprintf('CREATE TABLE "%s"."%s" AS\n', target_schema, concept_name)

  final_sql <- paste0(
    "-- Adapted by rendehR::register_concept_sql()\n",
    "-- src: ", src_name, "  concept: ", concept_name,
    "  schema: ", target_schema, "\n",
    "SET search_path TO ", schema_names$raw, ";\n\n",
    if (overwrite) drop_line else "",
    create_line,
    sql_text, "\n"
  )

  # 6. Save (default: into user/ or external/{module}/ so a later
  # run_concepts() call rediscovers it automatically, same as a hand-dropped
  # file; pass save_to = FALSE to skip saving, or an explicit path to save
  # elsewhere).
  if (!isFALSE(save_to)) {
    resolved_save_to <- save_to %||% {
      concept_paths <- ehr_paths(src_name = src_name, base_dir = base_dir)
      if (!is.null(module)) {
        file.path(concept_paths$external_sql_dir, module, paste0(concept_name, ".sql"))
      } else {
        file.path(concept_paths$user_sql_dir, paste0(concept_name, ".sql"))
      }
    }
    dir.create(dirname(resolved_save_to), recursive = TRUE, showWarnings = FALSE)
    writeLines(final_sql, resolved_save_to)
    message("Adapted SQL saved: ", resolved_save_to)
  }

  # 7. Execute unless dry_run, then register provenance
  if (!isTRUE(dry_run) || !is.null(con)) {
    .with_connection(con, function(con) {
      qualified <- DBI::dbQuoteIdentifier(
        con, DBI::Id(schema = target_schema, table = concept_name)
      )
      DBI::dbExecute(con, sprintf(
        "CREATE SCHEMA IF NOT EXISTS %s",
        DBI::dbQuoteIdentifier(con, target_schema)
      ), immediate = TRUE)
      exec_sql <- paste0(
        "-- Adapted by rendehR::register_concept_sql()\n",
        "-- src: ", src_name, "  concept: ", concept_name,
        "  schema: ", target_schema, "\n",
        "SET search_path TO ", schema_names$raw, ";\n\n",
        if (overwrite) sprintf("DROP TABLE IF EXISTS %s;\n", qualified) else "",
        sprintf("CREATE TABLE %s AS\n", qualified),
        sql_text, "\n"
      )
      tryCatch(
        DBI::dbExecute(con, exec_sql, immediate = TRUE),
        error = function(e) stop(
          "SQL execution failed for '", concept_name, "':\n",
          conditionMessage(e), "\n\n",
          "Tip: inspect with cat(register_concept_sql(...))\n",
          "Common fixes: join to icu_windows for the cohort filter, or\n",
          "check that SELECT columns match expected concept columns."
        )
      )
      n <- DBI::dbGetQuery(con, sprintf(
        "SELECT COUNT(*) AS n FROM %s", qualified
      ))$n
      message(sprintf("Concept '%s' -> %s  (%s rows)",
                      concept_name, target_schema, format(n, big.mark = ",")))

      register_concept_provenance(
        src_name     = src_name,
        db_name      = NULL,
        concept_name = concept_name,
        schema_name  = target_schema,
        origin_name  = origin_name %||% NA_character_,
        origin_url   = origin_url  %||% NA_character_,
        origin_ref   = origin_ref  %||% NA_character_,
        con          = con
      )
    })
  }

  invisible(final_sql)
}

# ---------------------------------------------------------------------------
# init_project_schema - dedicated schema for a project's derived work
# ---------------------------------------------------------------------------

#' Create a dedicated project schema in the connected database
#'
#' Creates a schema named `project_{name}` (or the literal name if it already
#' starts with "project_") in whichever database `con` is connected to (or
#' the active connection from `ehr_connect()` if `con` is NULL) - not tied
#' to any database named "rendehr" specifically. Use write_table() and
#' fetch_table() with `schema = "project_myname"` to read and write
#' project-specific tables alongside the source schemas without polluting
#' them.
#'
#' @param name Project name. Prefix "project_" is added automatically.
#' @param con Optional explicit DBI connection.
#' @return The schema name (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehrdov", creds)
#'
#' init_project_schema("rehab_outcomes")
#' # -> creates schema "project_rehab_outcomes"
#'
#' # Write your derived data into it
#' write_table("mimic_iv_demo", my_analysis_df,
#'             table_name = "aki_flags",
#'             schema     = "project_rehab_outcomes")
#'
#' # Read back
#' fetch_table("mimic_iv_demo", "aki_flags",
#'             schema = "project_rehab_outcomes")
#' }
init_project_schema <- function(name, con = NULL) {
  schema <- if (startsWith(name, "project_")) name
            else paste0("project_", name)
  .ehr_validate_db_identifier(schema, "project schema name")

  .with_connection(con, function(con) {
    DBI::dbExecute(con, sprintf(
      "CREATE SCHEMA IF NOT EXISTS %s", DBI::dbQuoteIdentifier(con, schema)
    ))
    message("Project schema ready: ", schema)
    message("  write_table(src_name, df, 'table', schema = '", schema, "')")
    message("  fetch_table(src_name, 'table', schema = '", schema, "')")
    invisible(schema)
  })
}

# ---------------------------------------------------------------------------
# clone_source_to_db - copy one source's schemas to a different database
# ---------------------------------------------------------------------------

# Thin wrapper so tests can mock the shell call.
.ehr_run_shell <- function(cmd) system(cmd)

#' Copy a source's schemas into a different Postgres database
#'
#' General-purpose clone: works for any source and any pair of databases,
#' whatever they're actually named - it just copies whichever of the
#' source's schemas you ask for from `from_db` to `to_db`. Creates the
#' target database if it does not exist, then copies every table from each
#' requested schema using `pg_dump | pg_restore` via system calls (requires
#' `pg_dump` in PATH). This is the correct approach when you want full
#' isolation: a colleague can connect to the target project database
#' without touching the source database at all.
#'
#' Alternatively, just point `run_one_source()` at a different `db_name` from
#' the start - that is the simplest way to build an isolated project database.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param from_db Source database name - whatever your project's database is
#'   actually called (e.g. "rendehr" is just the common default when nothing
#'   overrides it; see `.ehr_resolve_db_name()`). Not tied to any specific
#'   name - pass whatever database you're actually copying from.
#' @param to_db Target database name. Created if it does not exist.
#' @param creds List from load_credentials(). Same credentials used for both.
#' @param schemas Which schemas to copy, as keys of [src_schema_names()].
#'   Default: all four of the source's schemas (raw, concepts, derived,
#'   harmonized). "ts", "flat" and "dur" are accepted and resolve to the
#'   harmonized schema that holds those tables.
#' @param overwrite Drop and recreate schemas in the target. Default FALSE.
#' @return The name of the target database, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' creds <- load_credentials("config/config_pass.json")
#'
#' # "rendehrdov" below is just an example database name (a hypothetical dev
#' # database), not a requirement - substitute whatever from_db actually is.
#' clone_source_to_db("mimic_iv_demo",
#'                    from_db  = "rendehrdov",
#'                    to_db    = "project_rehab",
#'                    creds    = creds,
#'                    schemas  = c("raw", "concepts", "harmonized"))
#'
#' # Now connect to the isolated project DB
#' ehr_connect("project_rehab", creds)
#' fetch_concept("mimic_iv_demo", "heart_rate")
#' }
clone_source_to_db <- function(src_name, from_db, to_db, creds,
                                schemas  = c("raw", "concepts", "derived",
                                             "harmonized"),
                                overwrite = FALSE) {
  schema_names <- src_schema_names(src_name)
  unknown <- setdiff(schemas, c(names(schema_names), "ts", "flat", "dur"))
  if (length(unknown))
    stop("Unknown schema key(s): ", paste(unknown, collapse = ", "),
         ". Use any of: ", paste(names(schema_names), collapse = ", "), ".",
         call. = FALSE)
  target_schemas <- unique(vapply(schemas, .ehr_schema_name, character(1),
                                  schema_names = schema_names))

  ensure_database_exists(to_db, creds)

  # load_credentials() nests the server fields under $postgres.
  pg   <- creds$postgres %||% list()
  host <- shQuote(as.character(pg$host %||% "localhost"))
  port <- as.integer(pg$port %||% 5432L)
  user <- shQuote(as.character(pg$user %||% "postgres"))

  # pg_dump/pg_restore read the password from the environment; set it only
  # for the duration of the clone.
  old_pw <- Sys.getenv("PGPASSWORD", unset = NA_character_)
  if (!.ehr_credential_missing(pg$password)) {
    Sys.setenv(PGPASSWORD = as.character(pg$password))
    on.exit(if (is.na(old_pw)) Sys.unsetenv("PGPASSWORD") else
      Sys.setenv(PGPASSWORD = old_pw), add = TRUE)
  }

  for (s in target_schemas) {
    message("Cloning schema: ", s, "  (", from_db, " -> ", to_db, ")")

    dump_cmd <- sprintf(
      "pg_dump -h %s -p %d -U %s -n %s -Fc %s",
      host, port, user, shQuote(s), shQuote(from_db)
    )
    restore_cmd <- sprintf(
      "pg_restore -h %s -p %d -U %s -d %s %s -n %s",
      host, port, user, shQuote(to_db),
      if (overwrite) "--clean --if-exists" else "",
      shQuote(s)
    )

    pipe_cmd <- paste(dump_cmd, "|", restore_cmd)

    ret <- .ehr_run_shell(pipe_cmd)
    if (ret != 0)
      warning("Non-zero exit for schema: ", s,
              ". Check pg_dump/pg_restore output above.")
    else
      message("  Done: ", s)
  }

  message("\nClone complete. Connect with: ehr_connect('", to_db, "', creds)")
  invisible(to_db)
}

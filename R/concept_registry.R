#' Register provenance metadata for one or more concepts
#'
#' Writes rows to the `concept_registry` table in the source's raw schema,
#' creating the table if it does not exist. This allows list_concepts() to
#' display origin_name, origin_url, and origin_ref alongside each concept.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param db_name Optional Postgres database name. If NULL and `con` is not
#'   supplied, uses the active project config, then "rendehr".
#' @param concept_name Character vector of concept names.
#' @param schema_name The Postgres schema where the concept tables live.
#'   Can be a key from src_schema_names() (e.g. "derived") or a full schema
#'   name (e.g. "mimic_iv_demo_coh_ext_smith_2023").
#' @param origin_name Human-readable source label (e.g. "Smith et al. 2023").
#' @param origin_url  URL to repo, paper, or DOI.
#' @param origin_ref  Citation string.
#' @param base_dir Project root directory. Defaults to getwd().
#' @param overwrite If TRUE, replace existing registry rows for these concepts.
#'   Default TRUE.
#' @param con Optional explicit DBI connection.
#' @return Invisibly TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", creds)
#'
#' register_concept_provenance(
#'   src_name     = "mimic_iv_demo",
#'   db_name      = "rendehr",
#'   concept_name = c("sofa", "sapsii"),
#'   schema_name  = "derived",
#'   origin_name  = "MIMIC-Code",
#'   origin_url   = "https://github.com/MIT-LCP/mimic-code",
#'   origin_ref   = "Johnson et al., PhysioNet 2016"
#' )
#'
#' register_concept_provenance(
#'   src_name     = "mimic_iv_demo",
#'   db_name      = "rendehr",
#'   concept_name = c("smith_score", "smith_index"),
#'   schema_name  = "smith_2023",   # auto-expands to _coh_ext_smith_2023
#'   origin_name  = "Smith et al. 2023",
#'   origin_url   = "https://doi.org/10.xxxx/xxxxx",
#'   origin_ref   = "Smith J. et al., J Neurotrauma 2023"
#' )
#' }
register_concept_provenance <- function(src_name, db_name = NULL,
                                         concept_name,
                                         schema_name,
                                         origin_name = NA_character_,
                                         origin_url  = NA_character_,
                                         origin_ref  = NA_character_,
                                         base_dir    = NULL,
                                         overwrite   = TRUE,
                                         con         = NULL) {
  sn <- src_schema_names(src_name)
  .ehr_validate_db_identifier(sn$raw, "raw schema name")

  # Resolve schema_name: key -> full name, or ext shorthand -> full ext name
  resolved_schema <- if (schema_name %in% names(sn)) {
    sn[[schema_name]]
  } else if (grepl("^[a-z0-9_]+$", schema_name) &&
             !grepl("_coh", schema_name)) {
    paste0(src_name, "_coh_ext_", schema_name)
  } else {
    schema_name
  }
  .ehr_validate_db_identifier(resolved_schema, "concept schema name")

  .ensure_registry <- function(con) {
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE IF NOT EXISTS %s.concept_registry (
         concept_name TEXT NOT NULL,
         schema_name  TEXT NOT NULL,
         origin_name  TEXT,
         origin_url   TEXT,
         origin_ref   TEXT,
         built_at     TIMESTAMPTZ DEFAULT now(),
         PRIMARY KEY (concept_name, schema_name)
       )", DBI::dbQuoteIdentifier(con, sn$raw)
    ), immediate = TRUE)
  }

  .do_register <- function(con) {
    .ensure_registry(con)

    rows <- data.frame(
      concept_name = concept_name,
      schema_name  = resolved_schema,
      origin_name  = origin_name,
      origin_url   = origin_url,
      origin_ref   = origin_ref,
      stringsAsFactors = FALSE
    )

    registry_id <- DBI::dbQuoteIdentifier(con, sn$raw)

    if (overwrite) {
      for (i in seq_len(nrow(rows))) {
        DBI::dbExecute(con, sprintf(
          "INSERT INTO %s.concept_registry
             (concept_name, schema_name, origin_name, origin_url, origin_ref)
           VALUES (%s, %s, %s, %s, %s)
           ON CONFLICT (concept_name, schema_name) DO UPDATE SET
             origin_name = EXCLUDED.origin_name,
             origin_url  = EXCLUDED.origin_url,
             origin_ref  = EXCLUDED.origin_ref,
             built_at    = now()",
          registry_id,
          DBI::dbQuoteLiteral(con, rows$concept_name[i]),
          DBI::dbQuoteLiteral(con, rows$schema_name[i]),
          DBI::dbQuoteLiteral(con, rows$origin_name[i]),
          DBI::dbQuoteLiteral(con, rows$origin_url[i]),
          DBI::dbQuoteLiteral(con, rows$origin_ref[i])
        ), immediate = TRUE)
      }
    } else {
      for (i in seq_len(nrow(rows))) {
        DBI::dbExecute(con, sprintf(
          "INSERT INTO %s.concept_registry
             (concept_name, schema_name, origin_name, origin_url, origin_ref)
           VALUES (%s, %s, %s, %s, %s)
           ON CONFLICT DO NOTHING",
          registry_id,
          DBI::dbQuoteLiteral(con, rows$concept_name[i]),
          DBI::dbQuoteLiteral(con, rows$schema_name[i]),
          DBI::dbQuoteLiteral(con, rows$origin_name[i]),
          DBI::dbQuoteLiteral(con, rows$origin_url[i]),
          DBI::dbQuoteLiteral(con, rows$origin_ref[i])
        ), immediate = TRUE)
      }
    }

    message(sprintf("Registered %d concept(s) in %s.concept_registry  [schema: %s]",
                    length(concept_name), sn$raw, resolved_schema))
    invisible(TRUE)
  }

  if (!is.null(con)) {
    .do_register(con)
  } else {
    base_dir <- if (is.null(base_dir)) getwd() else base_dir
    paths    <- ehr_paths(src_name = src_name, base_dir = base_dir)
    db_name  <- .ehr_resolve_db_name(db_name)
    creds    <- load_credentials(paths$credentials_json)
    con2     <- connect_postgres(db_name, creds)
    on.exit(DBI::dbDisconnect(con2), add = TRUE)
    .do_register(con2)
  }
}

# ---------------------------------------------------------------------------
# Internal helpers for auto-registration during pipeline runs
# ---------------------------------------------------------------------------

# Known origin labels, URLs, and citation refs for concept sources.
.origin_labels <- list(
  rendehr    = list(name = "rendehR package",
                    url  = "https://github.com/ATE-DRIVEN-lab/rendehR",
                    ref  = NA_character_),
  blended    = list(name = "BlendedICU",
                    url  = "https://github.com/USM-CHU-FGuyon/BlendedICU",
                    ref  = "Oliver et al., Journal of Biomedical Informatics 2023. doi:10.1016/j.jbi.2023.104502"),
  mimic_code = list(name = "MIMIC-Code (MIT-LCP)",
                    url  = "https://github.com/MIT-LCP/mimic-code",
                    ref  = "Johnson et al., PhysioNet 2016. doi:10.13026/C2XW26"),
  eicu_code  = list(name = "eICU-CRD Code (MIT-LCP)",
                    url  = "https://github.com/MIT-LCP/eicu-code",
                    ref  = "Pollard et al., Scientific Data 2018. doi:10.1038/sdata.2018.178"),
  ehr        = list(name = "rendehR package (adapted from MIT-LCP)",
                    url  = "https://github.com/MIT-LCP/mimic-code",
                    ref  = "Johnson et al., PhysioNet 2016. doi:10.13026/C2XW26"),
  ehr_eicu   = list(name = "rendehR package (adapted from MIT-LCP eICU-CRD Code)",
                    url  = "https://github.com/MIT-LCP/eicu-code",
                    ref  = "Pollard et al., Scientific Data 2018. doi:10.1038/sdata.2018.178")
)

.resolve_origin <- function(ref_key) {
  if (is.null(ref_key) || is.na(ref_key) || !nzchar(ref_key))
    return(.origin_labels$rendehr)
  lbl <- .origin_labels[[ref_key]]
  if (!is.null(lbl)) lbl
  else list(name = ref_key, url = NA_character_, ref = NA_character_)
}

#' Auto-register provenance for atomic concepts from a concept config CSV.
#'
#' Groups concept rows by their `ref` column (if present); maps each ref value
#' to a human-readable origin_name and origin_url. Concepts with no `ref` get
#' origin_name = "rendehR package". Called internally by run_concepts().
#'
#' @param con DBI connection.
#' @param src_name Source name.
#' @param schema_name Full Postgres schema name where concepts were written.
#' @param config_path Path to the concept config CSV.
#' @param concept_names Optional character vector restricting registration to
#'   these concept names (e.g. only the concepts that were actually built
#'   successfully). When `NULL` (the default), every concept in the config
#'   matching the source family is registered, as before.
#' @keywords internal
auto_register_atomic_provenance <- function(con, src_name, schema_name,
                                            config_path,
                                            concept_names = NULL) {
  if (!file.exists(config_path)) return(invisible(NULL))

  cfg <- readr::read_csv(config_path, show_col_types = FALSE)
  cfg <- cfg[cfg$source == src_family(src_name), , drop = FALSE]
  if (!is.null(concept_names))
    cfg <- cfg[cfg$concept_name %in% concept_names, , drop = FALSE]
  if (nrow(cfg) == 0L) return(invisible(NULL))

  has_ref    <- "ref" %in% names(cfg)
  cfg$ref_   <- if (has_ref) cfg$ref else NA_character_
  cfg$ref_[is.na(cfg$ref_) | cfg$ref_ == ""] <- ""

  for (ref_val in unique(cfg$ref_)) {
    # Concept tables are created unquoted, so PostgreSQL stores them in lower
    # case; register the name the table actually has.
    names_for_ref <- unique(tolower(cfg$concept_name[cfg$ref_ == ref_val]))
    origin        <- .resolve_origin(if (nzchar(ref_val)) ref_val else NA)
    register_concept_provenance(
      src_name     = src_name,
      db_name      = NULL,
      concept_name = names_for_ref,
      schema_name  = schema_name,
      origin_name  = origin$name,
      origin_url   = origin$url,
      origin_ref   = origin$ref,
      con          = con
    )
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Snapshot-based registration helpers (used by run_concepts layers 4-7)
# ---------------------------------------------------------------------------

# Returns the current set of table names in a schema (empty character(0) if
# the schema does not exist yet).
.snapshot_tables <- function(con, schema_name) {
  tryCatch(
    DBI::dbGetQuery(con, sprintf(
      "SELECT tablename FROM pg_tables WHERE schemaname = '%s'", schema_name
    ))$tablename,
    error = function(e) character(0)
  )
}

# Registers tables that appeared in `schema_name` after `before_tables` was
# captured.  Only newly created tables are registered; existing entries are
# not overwritten (ON CONFLICT DO NOTHING).
.register_new_tables <- function(con, src_name, before_tables,
                                 schema_name, origin_key,
                                 origin_name = NULL,
                                 origin_url  = NULL,
                                 origin_ref  = NULL) {
  new_tables <- setdiff(.snapshot_tables(con, schema_name), before_tables)
  if (length(new_tables) == 0L) return(invisible(NULL))

  if (is.null(origin_name)) {
    origin      <- .resolve_origin(origin_key)
    origin_name <- origin$name
    origin_url  <- origin$url
    origin_ref  <- origin$ref
  }

  register_concept_provenance(
    src_name     = src_name,
    db_name      = NULL,
    concept_name = new_tables,
    schema_name  = schema_name,
    origin_name  = origin_name,
    origin_url   = origin_url  %||% NA_character_,
    origin_ref   = origin_ref  %||% NA_character_,
    overwrite    = FALSE,
    con          = con
  )
}

# Name of the concept table a packaged SQL file creates. File names do not
# always match it (eicu_age.sql creates age), and PostgreSQL folds the
# unquoted name to lower case, so read it from the CREATE TABLE statement and
# fall back to the lower-cased file stem.
.ehr_sql_created_table <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  m <- regmatches(txt, regexec(
    "(?i)CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?\\{concepts_schema\\}\\.([A-Za-z0-9_]+)",
    txt, perl = TRUE))[[1]]
  tolower(if (length(m)) m[2] else tools::file_path_sans_ext(basename(path)))
}

# Register concepts from a package SQL layer (flat/, ts/, dur/) as rendehR package.
# Derives concept names from the tables the .sql files create. `succeeded`
# optionally restricts registration to these filenames (e.g. only the files
# that executed without error); when NULL (default), every .sql file in the
# directory is registered, as before.
.register_pkg_layer_provenance <- function(con, src_name, sql_dir, schema_name,
                                           succeeded = NULL) {
  if (!nzchar(sql_dir) || !dir.exists(sql_dir)) return(invisible(NULL))
  files <- list.files(sql_dir, pattern = "\\.sql$", recursive = FALSE)
  if (!is.null(succeeded)) files <- intersect(files, succeeded)
  if (length(files) == 0L) return(invisible(NULL))
  register_concept_provenance(
    src_name     = src_name,
    db_name      = NULL,
    concept_name = unique(vapply(file.path(sql_dir, files),
                                 .ehr_sql_created_table, character(1),
                                 USE.NAMES = FALSE)),
    schema_name  = schema_name,
    origin_name  = .origin_labels$rendehr$name,
    origin_url   = .origin_labels$rendehr$url,
    origin_ref   = NA_character_,
    con          = con
  )
}

# Register concepts from the user/ layer. Uses origin_name = "user" so that
# list_concepts() can distinguish them from unregistered package concepts
# (which have origin_name = NA and show as "package"). `succeeded` optionally
# restricts registration to these filenames, as in .register_pkg_layer_provenance().
.register_user_layer_provenance <- function(con, src_name, sql_dir, schema_name,
                                            succeeded = NULL) {
  if (!nzchar(sql_dir) || !dir.exists(sql_dir)) return(invisible(NULL))
  files <- list.files(sql_dir, pattern = "\\.sql$", recursive = TRUE)
  if (!is.null(succeeded)) files <- intersect(files, succeeded)
  if (length(files) == 0L) return(invisible(NULL))
  register_concept_provenance(
    src_name     = src_name,
    db_name      = NULL,
    concept_name = tools::file_path_sans_ext(basename(files)),
    schema_name  = schema_name,
    origin_name  = "user",
    origin_url   = NA_character_,
    origin_ref   = NA_character_,
    con          = con
  )
}

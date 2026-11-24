#' Path to a bundled package config file
#'
#' Returns the full path to a file in `inst/extdata/`. Use this instead of
#' hard-coding paths so the package works when installed anywhere.
#'
#' @param filename File name (e.g. "rendehr-data-source.json").
#' @return Absolute path string.
#' @examples
#' \dontrun{
#' ehr_use_config("config/ehr_config.json")
#' ehr_config_path("config_pass.json")
#' }
#' @export
ehr_config_path <- function(filename) {
  p <- system.file("extdata", filename, package = "rendehR")
  if (!nzchar(p)) stop("Package config file not found: ", filename)
  p
}

# Raw external config-root value, without requiring the directory to exist.
.ehr_config_root_value <- function() {
  p <- Sys.getenv("EHR_EXTERNAL_CONFIG_ROOT", unset = "")
  if (!nzchar(p)) {
    retired <- c("EHR_CONFIG_DIR", "SCIEHR_CONFIG_PATH")
    present <- retired[nzchar(Sys.getenv(retired, unset = ""))]
    if (length(present)) {
      stop(
        paste0(
          "Retired environment variable ", present[[1L]],
          " is set. Rename it to EHR_EXTERNAL_CONFIG_ROOT; legacy aliases ",
          "are not used."
        ),
        call. = FALSE
      )
    }
  }
  if (!nzchar(p)) return(NULL)
  path.expand(p)
}

#' Root of the external user config directory
#'
#' Returns the directory named by `EHR_EXTERNAL_CONFIG_ROOT`. Returns `NULL`
#' when it is unset or when the configured directory does not exist. This
#' lets users supply SQL and config files (for new sources, or overrides of
#' existing ones) from outside the installed package, without forking it. It is
#' the second layer in the resolver order: explicit argument -> project-local ->
#' `EHR_EXTERNAL_CONFIG_ROOT` -> packaged defaults. Retired
#' `EHR_CONFIG_DIR` and `SCIEHR_CONFIG_PATH` values produce migration guidance.
#'
#' @return Absolute directory path, or `NULL`.
#' @examples
#' \dontrun{
#' ehr_use_config("config/ehr_config.json")
#' ehr_config_root()
#' }
#' @export
ehr_config_root <- function() {
  p <- .ehr_config_root_value()
  if (is.null(p)) return(NULL)
  if (!dir.exists(p)) return(NULL)
  p
}

# Path to `filename` inside EHR_EXTERNAL_CONFIG_ROOT if it exists there, else NULL.
# Used to supply supplemental (merged) config such as extra concept-config rows.
.ehr_config_root_file <- function(filename) {
  root <- ehr_config_root()
  if (is.null(root)) return(NULL)
  p <- file.path(root, filename)
  if (file.exists(p)) p else NULL
}

.ehr_is_abs_path <- function(path) {
  grepl("^/|^[A-Za-z]:[\\/]", path) || startsWith(path, "~")
}

.ehr_resolve_path <- function(path, base_dir) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  path <- path.expand(path)
  if (.ehr_is_abs_path(path)) path else file.path(base_dir, path)
}

.ehr_trim_trailing_sep <- function(path) {
  sub("([/]|\\\\)+$", "", path.expand(path))
}

.ehr_scalar_path <- function(x, field, allow_null = TRUE) {
  if (is.null(x) && isTRUE(allow_null)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(x)) || identical(trimws(x), "NA")) {
    stop("`", field, "` must be one non-empty path string or JSON null.",
         call. = FALSE)
  }
  x
}

.ehr_as_source_raw_dir <- function(raw_data_root, src_name, base_dir) {
  raw_data_root <- .ehr_scalar_path(raw_data_root, "raw_data_root")
  if (is.null(raw_data_root)) return(NULL)
  raw_data_root <- .ehr_resolve_path(raw_data_root, base_dir)
  if (identical(basename(.ehr_trim_trailing_sep(raw_data_root)), src_name)) {
    stop(
      "`raw_data_root` must be the parent storage root, not the source ",
      "directory. Supply its parent for source '", src_name, "'.",
      call. = FALSE
    )
  }
  file.path(raw_data_root, src_name)
}

.ehr_download_dest_dir <- function(raw_data_root, src_name, base_dir) {
  .ehr_as_source_raw_dir(raw_data_root, src_name, base_dir)
}

.ehr_default_config_path <- function(config_path = NULL) {
  if (!is.null(config_path)) return(config_path)
  Sys.getenv("EHR_CONFIG", unset = file.path("config", "ehr_config.json"))
}

.ehr_normalize_config <- function(cfg, config_path = NULL) {
  if (!is.list(cfg)) stop("Project config must be a JSON object.", call. = FALSE)
  if (!is.null(cfg$src_name)) {
    stop(
      "Top-level `src_name` is retired. Move it to `source.src_name`.",
      call. = FALSE
    )
  }
  if (!is.null(cfg$base_dir)) {
    stop(
      "Top-level `base_dir` is retired. Move it to `project.base_dir`.",
      call. = FALSE
    )
  }
  if (!is.null(cfg$db_name)) {
    stop(
      "Top-level `db_name` is retired. Move it to `database.db_name`.",
      call. = FALSE
    )
  }
  if (!is.null(cfg$credentials_path)) {
    stop(
      "Top-level `credentials_path` is retired. Move it to ",
      "`database.credentials_path`.",
      call. = FALSE
    )
  }
  cfg$project  <- cfg$project  %||% list()
  cfg$database <- cfg$database %||% list()
  cfg$source   <- cfg$source   %||% list()
  cfg$paths    <- cfg$paths    %||% list()
  cfg$concepts <- cfg$concepts %||% list()
  cfg$cohort   <- cfg$cohort   %||% list()

  defaults_used <- character(0)

  src_name <- cfg$source$src_name
  if (is.null(src_name) || (is.character(src_name) && length(src_name) == 1L &&
                            !is.na(src_name) && !nzchar(trimws(src_name)))) {
    src_name <- "mimic_iv_demo"
    defaults_used <- c(defaults_used, 'source.src_name = "mimic_iv_demo"')
  }
  if (!is.character(src_name) || length(src_name) != 1L || is.na(src_name) ||
      !nzchar(trimws(src_name))) {
    stop("`source.src_name` must be one non-empty string.", call. = FALSE)
  }
  cfg$source$src_name <- src_name

  if (is.null(cfg$database$db_name) || !nzchar(cfg$database$db_name)) {
    cfg$database$db_name <- "rendehr"
    defaults_used <- c(defaults_used, 'database.db_name = "rendehr"')
  }

  if (is.null(cfg$project$base_dir)) {
    defaults_used <- c(defaults_used, paste0('project.base_dir = "', getwd(), '"'))
  }
  cfg$project$base_dir <- cfg$project$base_dir %||% getwd()

  if (is.null(cfg$database$credentials_path)) {
    defaults_used <- c(defaults_used,
                       'database.credentials_path = "config/config_pass.json"')
  }

  if (is.null(cfg$cohort$profile) || !nzchar(cfg$cohort$profile)) {
    cfg$cohort$profile <- "generic"
    defaults_used <- c(defaults_used, 'cohort.profile = "generic"')
  }
  cfg$cohort$profile <- tolower(cfg$cohort$profile)

  retired_paths <- intersect(
    names(cfg$paths),
    c("config_dir", "sql_root", "generated_sql_root")
  )
  if (length(retired_paths)) {
    stop(
      "Retired project path field(s): ",
      paste0("paths.", retired_paths, collapse = ", "),
      ". Config and SQL directories are derived from `project.base_dir`.",
      call. = FALSE
    )
  }
  if (is.null(cfg$paths$raw_data_root))
    defaults_used <- c(defaults_used,
                       'paths.raw_data_root = "data/source_data"')
  if (is.null(cfg$paths$cohort_parquet_root))
    defaults_used <- c(defaults_used,
                       'paths.cohort_parquet_root = "data/cohorts_data"')
  if (is.null(cfg$paths$duckdb_root))
    defaults_used <- c(defaults_used, 'paths.duckdb_root = "data/duckdb"')
  for (field in c("raw_data_root", "cohort_parquet_root", "duckdb_root")) {
    value <- cfg$paths[[field]]
    if (!is.null(value)) .ehr_scalar_path(value, paste0("paths.", field))
  }
  cfg$database$credentials_path <-
    cfg$database$credentials_path %||%
    file.path("config", "config_pass.json")

  cfg$defaults_used <- unique(c(cfg$defaults_used %||% character(0),
                                defaults_used))
  if (!is.null(config_path)) cfg$config_path <- config_path
  cfg
}

.ehr_project_config_template <- function(src_name, db_name, base_dir,
                                         raw_data_root,
                                         cohort_parquet_root,
                                         duckdb_root,
                                         credentials_path,
                                         cohort_profile) {
  list(
    version = 1L,
    project = list(
      base_dir = base_dir
    ),
    database = list(
      db_name = db_name,
      credentials_path = credentials_path
    ),
    source = list(
      src_name = src_name
    ),
    paths = list(
      raw_data_root = raw_data_root,
      cohort_parquet_root = cohort_parquet_root,
      duckdb_root = duckdb_root
    ),
    concepts = list(
      ts_config_path = NULL,
      dur_config_path = NULL,
      user_concepts_path = NULL
    ),
    cohort = list(
      profile = cohort_profile,
      icd_filter_path = NULL,
      icd_dict_path = NULL,
      icd_proc_dict_path = NULL,
      # Per-profile blocks for paths that only make sense under one cohort
      # profile (e.g. the SCI procedure-classification asset).
      # Keeps the common section small; add a new named block here only for
      # the profile(s) actually in use.
      profiles = if (identical(cohort_profile, "scid")) {
        list(scid = list(icd_proc_path = NULL))
      } else {
        list()
      }
    )
  )
}

#' Load project configuration from a JSON file
#'
#' Reads user-facing project settings such as source name, database name,
#' project roots, and the path to the separate credentials file. Passwords
#' should stay in `config/config_pass.json` or environment variables, not in
#' this project config.
#'
#' If `config_path` is omitted, checks the `EHR_CONFIG` environment variable,
#' then falls back to `"config/ehr_config.json"` relative to the current
#' working directory.
#'
#' @param config_path Path to a JSON project config. Required fields may be
#'   either flat (`src_name`, `db_name`) or nested (`source.src_name`,
#'   `database.db_name`).
#' @return Named list of config values. Flat fields are retained for backward
#'   compatibility, and nested fields are added for newer code.
#' @examples
#' path <- file.path(tempdir(), "ehr_config_load.json")
#' ehr_create_config_template(path, base_dir = tempdir())
#' cfg <- ehr_load_config(path)
#' names(cfg)
#' @export
ehr_load_config <- function(config_path = NULL) {
  config_path <- .ehr_default_config_path(config_path)
  if (!file.exists(config_path))
    stop("Config file not found: ", config_path,
         "\nSet EHR_CONFIG envvar or pass config_path explicitly.")

  cfg <- jsonlite::read_json(config_path, simplifyVector = TRUE)
  .ehr_normalize_config(cfg, config_path)
}

#' Write a project configuration JSON file
#'
#' Writes a config list to JSON. This is intended for project settings and
#' paths only; keep passwords in the separate credentials file referenced by
#' `database.credentials_path`.
#'
#' @param config Named list to write.
#' @param path Output path. Defaults to the `EHR_CONFIG` environment variable
#'   or `config/ehr_config.json`.
#' @param overwrite Overwrite an existing file? Default FALSE.
#' @param pretty Pretty-print JSON? Default TRUE.
#' @return Invisibly returns `path`.
#' @examples
#' \dontrun{
#' cfg <- ehr_load_config("config/ehr_config.json")
#' cfg$database$db_name <- "rendehr"
#' ehr_write_config(cfg, "config/ehr_config.json", overwrite = TRUE)
#' }
#' @export
ehr_write_config <- function(config, path = NULL, overwrite = FALSE,
                             pretty = TRUE) {
  path <- .ehr_default_config_path(path)
  if (file.exists(path) && !isTRUE(overwrite))
    stop("Config file already exists: ", path,
         "\nUse overwrite = TRUE to replace it.")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(config, pretty = pretty, auto_unbox = TRUE,
                           null = "null")
  writeLines(json, path)
  message("rendehR: project config written to '", path, "'.")
  invisible(path)
}

#' Create a starter project configuration file
#'
#' Creates a project config template with path roots. Source-specific folders
#' are still derived internally by `ehr_paths()`, so users do not need to list
#' every source in the config.
#'
#' @param path Output path. Defaults to `config/ehr_config.json`.
#' @param src_name Default source name.
#' @param db_name Default Postgres database name.
#' @param base_dir Project root directory.
#' @param raw_data_root Root for raw downloaded source data.
#' @param cohort_parquet_root Root for cohort parquet outputs.
#' @param duckdb_root Root for DuckDB database files.
#' @param credentials_path Path to the separate credentials JSON file.
#' @param cohort_profile Named cohort profile: `"generic"` (the default) or `"scid"`.
#' @param overwrite Overwrite an existing file? Default FALSE.
#' @return Invisibly returns the written config path.
#' @examples
#' path <- file.path(tempdir(), "ehr_config.json")
#' ehr_create_config_template(path, src_name = "mimic_iv_demo", base_dir = tempdir())
#' str(ehr_load_config(path), max.level = 1)
#' @export
ehr_create_config_template <- function(
    path = file.path("config", "ehr_config.json"),
    src_name = "mimic_iv_demo",
    db_name = "rendehr",
    base_dir = ".",
    raw_data_root = file.path("data", "source_data"),
    cohort_parquet_root = file.path("data", "cohorts_data"),
    duckdb_root = file.path("data", "duckdb"),
    credentials_path = file.path("config", "config_pass.json"),
    cohort_profile = "generic",
    overwrite = FALSE) {
  cfg <- .ehr_project_config_template(
    src_name = src_name,
    db_name = db_name,
    base_dir = base_dir,
    raw_data_root = raw_data_root,
    cohort_parquet_root = cohort_parquet_root,
    duckdb_root = duckdb_root,
    credentials_path = credentials_path,
    cohort_profile = cohort_profile
  )
  ehr_write_config(cfg, path = path, overwrite = overwrite)
}

#' Check a project configuration
#'
#' Validates required fields, resolves source-specific paths, and reports
#' likely problems such as a missing credentials file or secrets accidentally
#' placed in the project config.
#'
#' @param config Optional config list. If NULL, loads `config_path` or uses the
#'   active config from `ehr_use_config()`.
#' @param config_path Optional path to a project config JSON file.
#' @param src_name Optional source name to use while resolving paths.
#' @param quiet Suppress summary messages? Default FALSE.
#' @return A list with `ok`, `problems`, `warnings`, and `paths`.
#' @examples
#' \dontrun{
#' ehr_check_config(config_path = "config/ehr_config.json", src_name = "mimic_iv_demo")
#' }
#' @export
ehr_check_config <- function(config = NULL, config_path = NULL,
                             src_name = NULL, quiet = FALSE) {
  problems <- character(0)
  warnings <- character(0)

  if (is.null(config)) {
    if (!is.null(config_path)) {
      config <- ehr_load_config(config_path)
    } else if (!is.null(.ehr_env$config)) {
      config <- .ehr_env$config
    } else {
      config <- ehr_load_config()
    }
  } else {
    config <- .ehr_normalize_config(config)
  }

  paths <- tryCatch(
    ehr_paths(src_name = src_name, config = config),
    error = function(e) {
      problems <<- c(problems, conditionMessage(e))
      NULL
    }
  )

  if (length(config$defaults_used %||% character(0)) > 0L) {
    warnings <- c(warnings,
                  paste0("Default used: ", config$defaults_used))
  }

  secret_names <- grep("password|secret|token", names(unlist(config)),
                       ignore.case = TRUE, value = TRUE)
  if (length(secret_names) > 0L) {
    warnings <- c(warnings,
                  paste0("Project config appears to contain secret-like fields: ",
                         paste(secret_names, collapse = ", ")))
  }

  if (!is.null(paths)) {
    if (!file.exists(paths$credentials_json)) {
      warnings <- c(warnings,
                    paste0("Credentials file not found: ", paths$credentials_json))
    }

    parent_dirs <- unique(dirname(c(
      paths$raw_dir,
      paths$cohort_parquet_dir,
      paths$sql_dir,
      paths$generated_ts_sql_dir,
      paths$generated_dur_sql_dir,
      paths$duckdb_path
    )))
    bad_parents <- parent_dirs[file.exists(parent_dirs) & !dir.exists(parent_dirs)]
    if (length(bad_parents) > 0L) {
      problems <- c(problems,
                    paste0("Path parent exists but is not a directory: ",
                           paste(bad_parents, collapse = ", ")))
    }
  }

  result <- list(
    ok = length(problems) == 0L,
    problems = problems,
    warnings = warnings,
    paths = paths
  )

  if (!isTRUE(quiet)) {
    if (result$ok) message("rendehR: config check passed.")
    else message("rendehR: config check found ", length(problems), " problem(s).")

    if (length(config$defaults_used %||% character(0)) > 0L) {
      message("rendehR: defaults used:")
      for (d in config$defaults_used) message("  - ", d)
    }

    if (!is.null(paths)) {
      message("rendehR: resolved paths:")
      message("  raw data       : ", paths$raw_dir)
      message("  cohort parquet : ", paths$cohort_parquet_dir)
      message("  DuckDB         : ", paths$duckdb_path)
      message("  source SQL     : ", paths$sql_dir)
      message("  generated SQL  : ", dirname(paths$generated_ts_sql_dir))
      message("  credentials    : ", paths$credentials_json)
    }

    if (length(warnings) > 0L)
      message("rendehR: config check has ", length(warnings), " warning(s).")
  }

  invisible(result)
}

#' Use a project configuration for this R session
#'
#' Loads a project config with `ehr_load_config()` and stores it in the
#' package session environment. Path helpers and `ehr_connect()` use this
#' config as their default source of project settings.
#'
#' @param config_path Path to a JSON project config. Defaults to the
#'   `EHR_CONFIG` environment variable or `config/ehr_config.json`.
#' @return Invisibly returns the loaded config.
#' @examples
#' \dontrun{
#' ehr_use_config("config/ehr_config.json")
#' ehr_config_root()
#' }
#' @export
ehr_use_config <- function(config_path = NULL) {
  cfg <- ehr_load_config(config_path)
  .ehr_env$config <- cfg
  .ehr_env$creds <- NULL
  .ehr_env$credential_source <- NULL
  message("rendehR: project config loaded from '", cfg$config_path, "'.")
  invisible(cfg)
}

#' Return the active project configuration
#'
#' @return The config loaded by `ehr_use_config()`.
#' @examples
#' path <- file.path(tempdir(), "ehr_config_active.json")
#' ehr_create_config_template(path, base_dir = tempdir())
#' ehr_use_config(path)
#' names(ehr_config())
#' @export
ehr_config <- function() {
  if (is.null(.ehr_env$config))
    stop("No project config loaded. Run ehr_use_config() first.")
  .ehr_env$config
}

# Resolve a Postgres database name: an explicit argument always wins; otherwise
# fall back to the active project config (db_name or database$db_name), then to
# the "rendehr" default. Mirrors the resolution used by run_one_source().
#
# ehr_connect()'s db_name (.ehr_env$db_name) is a *separate* piece of state,
# used only by direct-access calls like fetch_table()/load_merged_ts() - it
# never feeds this resolution chain. When both are set and disagree, warn
# instead of silently picking one; reordering priority either way just
# trades one silent-redirect surprise for another (see git log for this
# function - a real "why did my pipeline run land in the wrong database"
# case this comment exists to explain).
.ehr_resolve_db_name <- function(db_name = NULL) {
  if (!is.null(db_name)) return(db_name)

  resolved  <- .ehr_env$config$database$db_name %||% "rendehr"
  connected <- .ehr_env$db_name

  if (!is.null(connected) && !identical(connected, resolved))
    message(
      "Note: ehr_connect() targeted '", connected, "', but this call ",
      "defaults to '", resolved, "' (from the active project config). ",
      "ehr_connect()'s target only applies to direct-access calls like ",
      "fetch_table()/load_merged_ts() - pass db_name explicitly here if ",
      "you meant to use '", connected, "'."
    )

  resolved
}

# ---------------------------------------------------------------------------
# Source family registry
# ---------------------------------------------------------------------------
# Maps a source name (e.g. "mimic_iv_demo_cervical_a") to its canonical family
# (e.g. "mimic_iv") via registered id_prefixes. Seeded with the built-in
# families in .onLoad() (see zzz.R). This is the single lookup that every
# src_family()-based dispatch reads, so a future register_source() can add a
# family here without touching pipeline code. The registry is stored in the
# package-level `.ehr_env` (defined in access_core.R).

# Register (or overwrite) a source family and all per-family config the
# pipeline dispatches on: id prefixes (matched against the start of a source
# name), the id columns / time origin / time-diff strategy used to build
# concept SQL, and the cohort key + seed tables used to build cohort IDs.
#
# time_diff selects the timestamp arithmetic strategy used by build_time_diff()
# and build_icu_time_diff():
#   "timestamp_minutes" - ROUND(EXTRACT(EPOCH FROM (index - origin))/60) (MIMIC)
#   "offset_minutes"    - index is already an integer minute offset   (eICU)
#
# cohort_filter is the declarative ICD selector the cohort engine
# (.ehr_select_cohort_ids_duckdb()) runs for a family: a list with diag_table,
# code_col, key, and optional split_commas / strip_dots flags, covering the
# common "filter a diagnosis table by ICD code" pattern. eICU has no
# cohort_filter; the engine selects eICU ICD cohorts with its own branch.
.register_source_family <- function(name,
                                    id_prefixes        = name,
                                    select_id_cols     = NULL,
                                    time_origin        = NULL,
                                    icu_id_type        = "bigint",
                                    time_diff          = c("timestamp_minutes", "offset_minutes"),
                                    id_cols            = NULL,
                                    cohort_key         = NULL,
                                    cohort_seed_tables = NULL,
                                    cohort_filter      = NULL,
                                    demo_row_limit     = 1000L,
                                    source_names       = id_prefixes,
                                    release_metadata   = NULL) {
  if (is.null(.ehr_env$source_registry)) .ehr_env$source_registry <- list()
  .ehr_env$source_registry[[name]] <- list(
    id_prefixes        = as.character(id_prefixes),
    select_id_cols     = select_id_cols,
    time_origin        = time_origin,
    icu_id_type        = icu_id_type,
    time_diff          = match.arg(time_diff),
    id_cols            = id_cols,
    cohort_key         = cohort_key,
    cohort_seed_tables = cohort_seed_tables,
    cohort_filter      = cohort_filter,
    demo_row_limit     = demo_row_limit,
    source_names       = as.character(source_names),
    release_metadata   = release_metadata
  )
  invisible(NULL)
}

# Registry entry for a source name, resolved via its family, or NULL if the
# family is not registered. Callers throw their own error so the historical
# messages ("Unknown dataset" vs "Unknown src_family") are preserved.
.source_entry <- function(src_name) {
  .ehr_env$source_registry[[src_family(src_name)]]
}

# Seed the registry with the families that ship with the package. Called from
# .onLoad(). These values are the single source of truth for family config;
# they mirror the historical dataset_configs / id_cols / build_time_diff /
# cohort_key / cohort_seed_tables definitions exactly.
.register_builtin_sources <- function() {
  .register_source_family(
    "mimic_iv",
    id_prefixes        = c("mimic_iv", "miiv"),
    source_names       = c("mimic_iv", "miiv", "mimic_iv_demo"),
    release_metadata   = list(mimic_iv = "3.1", miiv = "2.2",
                              mimic_iv_demo = "2.2"),
    select_id_cols     = c("subject_id", "hadm_id", "stay_id"),
    time_origin        = "cw.admittime",
    icu_id_type        = "bigint",
    time_diff          = "timestamp_minutes",
    id_cols            = list(
      default         = c("subject_id", "hadm_id"),
      icu             = c("subject_id", "hadm_id", "stay_id"),
      tables_with_icu = c("icustays", "chartevents", "datetimeevents",
                          "ingredientevents", "inputevents", "outputevents",
                          "procedureevents")
    ),
    cohort_key         = "subject_id",
    cohort_seed_tables = "diagnoses_icd",
    cohort_filter      = list(diag_table = "diagnoses_icd",
                              code_col   = "icd_code",
                              key        = "subject_id")
  )
  .register_source_family(
    "mimic_iii",
    id_prefixes        = "mimic_iii",
    source_names       = c("mimic_iii", "mimic_iii_demo"),
    select_id_cols     = c("subject_id", "hadm_id", "icustay_id"),
    time_origin        = "cw.admittime",
    icu_id_type        = "bigint",
    time_diff          = "timestamp_minutes",
    id_cols            = list(
      default         = c("subject_id", "hadm_id"),
      icu             = c("subject_id", "hadm_id", "icustay_id"),
      # prescriptions and services are admission-level tables (MIMIC-III
      # prescriptions carries icustay_id only for orders placed in an ICU;
      # services has none), so they join on subject_id/hadm_id like MIMIC-IV.
      tables_with_icu = c("icustays", "chartevents", "datetimeevents",
                          "inputevents_mv", "inputevents_cv", "outputevents",
                          "procedureevents_mv")
    ),
    cohort_key         = "subject_id",
    cohort_seed_tables = "diagnoses_icd",
    cohort_filter      = list(diag_table = "diagnoses_icd",
                              code_col   = "icd9_code",
                              key        = "subject_id")
  )
  .register_source_family(
    "eicu",
    id_prefixes        = "eicu",
    source_names       = c("eicu", "eicu_demo"),
    select_id_cols     = c("uniquepid", "patienthealthsystemstayid", "patientunitstayid"),
    time_origin        = "cw.icu_intime",
    icu_id_type        = "bigint",
    time_diff          = "offset_minutes",
    id_cols            = list(default = "patientunitstayid"),
    cohort_key         = "patientunitstayid",
    cohort_seed_tables = c("diagnosis", "patient")
  )
  invisible(NULL)
}

#' Canonical source family name
#'
#' Collapses any variant suffix (`_demo`, materialized subcohorts such as
#' `_cervical_a`, dev/capped variants) to the canonical family so concept
#' config and SQL lookups work across all variants without duplicating rows.
#' Resolution is driven by the source registry seeded in `.onLoad()`; unknown
#' names are returned unchanged.
#'
#' @param src_name Source name, e.g. `"mimic_iv_demo"` or `"mimic_iv"`. May be
#'   a character vector.
#' @return Canonical family name(s), e.g. `"mimic_iv"`.
#' @examples
#' src_family("mimic_iii_demo")
#' src_family("eicu")
#' @export
src_family <- function(src_name) {
  removed <- c("eicu_scid", "miii_scid", "miiv_scid")
  bad <- which(!is.na(src_name) & src_name %in% removed)
  if (length(bad)) {
    stop(
      "SCID names are not data sources. Use the canonical source name and ",
      "set `cohort.profile = \"sci\"` (or provide an explicit cohort).",
      call. = FALSE
    )
  }
  reg <- .ehr_env$source_registry

  # Safety net: if the registry has not been seeded (e.g. src_family() is
  # called before .onLoad() runs), fall back to the historical regex so
  # behaviour is unchanged.
  if (is.null(reg) || !length(reg)) {
    return(sub("^(mimic_iv|mimic_iii|eicu).*", "\\1", src_name))
  }

  # Flatten registered families to aligned (prefix, family) vectors, then match
  # the longest prefix first so more specific families win.
  fams     <- names(reg)
  prefixes <- unlist(lapply(fams, function(f) reg[[f]]$id_prefixes), use.names = FALSE)
  fam_of   <- rep(fams, vapply(fams, function(f) length(reg[[f]]$id_prefixes), integer(1)))
  ord      <- order(nchar(prefixes), decreasing = TRUE)
  prefixes <- prefixes[ord]
  fam_of   <- fam_of[ord]

  vapply(src_name, function(s) {
    if (is.na(s)) return(NA_character_)
    hit <- which(startsWith(s, prefixes))
    if (length(hit)) fam_of[hit[1]] else s
  }, character(1), USE.NAMES = FALSE)
}

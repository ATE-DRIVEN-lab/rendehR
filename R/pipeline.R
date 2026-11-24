# Unified pipeline runners for the ehr pipeline.
#
# Each stage function works for any supported source (mimic_iv, mimic_iii,
# eicu - demo or full). Source-specific differences are encapsulated in the
# source registry (.register_builtin_sources() in env_config.R) and the cohort
# dispatchers (cohort.R).
#
# Typical call sequence for one source:
#   import_cohort(src_name)
#   run_db(src_name, db_name, base_dir)
#   run_concepts(src_name, db_name, base_dir)
#   run_harmonized_tables(base_dir, src_name, db_name, ...)
#
# Or run everything at once:
#   run_one_source(src_name, db_name, base_dir, icd_filter_path)

# ---------------------------------------------------------------------------
# Shared path + schema helpers (single definition, used by all sources)
# ---------------------------------------------------------------------------

# Resolver order for config files: explicit override -> project-local
# (config_dir) -> EHR_EXTERNAL_CONFIG_ROOT -> packaged extdata. Concept-config CSVs
# pass use_config_root = FALSE so the env dir supplies *merged* rows via
# user_concepts (see ehr_paths / run_concepts) rather than replacing the base.
.ehr_pkg_or_local_cfg <- function(filename, config_dir, base_dir, override = NULL,
                                  use_config_root = TRUE) {
  if (!is.null(override)) return(.ehr_resolve_path(override, base_dir))
  local <- file.path(config_dir, filename)
  if (file.exists(local)) return(local)
  if (isTRUE(use_config_root)) {
    root <- ehr_config_root()
    if (!is.null(root)) {
      ext <- file.path(root, filename)
      if (file.exists(ext)) return(ext)
    }
  }
  ehr_config_path(filename)
}

# Per-file first-match for SQL: project-local -> EHR_EXTERNAL_CONFIG_ROOT/sql/{fam}
# -> packaged inst/sql/{fam}. Users drop only the files they change; the rest
# are inherited from the package.
.ehr_pkg_or_local_sql <- function(rel, sql_root, src_name, fam) {
  local <- file.path(sql_root, src_name, rel)
  if (file.exists(local)) return(local)
  root <- ehr_config_root()
  if (!is.null(root)) {
    ext <- file.path(root, "sql", fam, rel)
    if (file.exists(ext)) return(ext)
  }
  system.file("sql", fam, rel, package = "rendehR")
}

.ehr_pkg_concepts_sql <- function(layer, fam) {
  base <- system.file("sql", fam, "concepts_sqls", package = "rendehR")
  if (!nzchar(base)) {
    # New family with no packaged SQL: allow an external concepts_sqls dir.
    root <- ehr_config_root()
    if (!is.null(root)) {
      ext <- file.path(root, "sql", fam, "concepts_sqls")
      if (dir.exists(ext)) return(file.path(ext, layer))
    }
    return("")
  }
  file.path(base, layer)
}

.ehr_profile_cfg_path <- function(filename, config_dir, base_dir, override = NULL,
                                  profile = "scid", scid_only = FALSE) {
  if (!is.null(override)) return(.ehr_resolve_path(override, base_dir))
  if (isTRUE(scid_only) && !identical(profile, "scid")) return(NULL)
  .ehr_pkg_or_local_cfg(filename, config_dir, base_dir)
}

#' Build the standard project path list for a source
#'
#' Resolves all project paths for a source. If `ehr_use_config()` has loaded a
#' project config, its path roots are used by default; explicit arguments still
#' win. Source-specific directories are derived internally from `src_name`.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo", "mimic_iii"). This
#'   names the cohort output directories and Postgres schemas. Defaults to the
#'   active project config source, then "mimic_iv_demo". The source family
#'   (mimic_iv, eicu, mimic_iii) is derived from it via `src_family()`.
#' @param base_dir Project root directory. Defaults to the loaded project
#'   config, then to the current working directory.
#' @param raw_data_root Optional parent storage-root override. The source
#'   directory is always derived as `{raw_data_root}/{src_name}`.
#' @param raw_data_dir Retired ambiguous override. Use `raw_data_root`.
#' @param config Optional config list from `ehr_load_config()`. Defaults to the
#'   active config loaded by `ehr_use_config()`, then an existing
#'   `config/ehr_config.json` or `EHR_CONFIG` file.
#' @return Named list of file/directory paths.
#' @examples
#' \dontrun{
#' ehr_use_config("config/ehr_config.json")
#' paths <- ehr_paths("mimic_iv_demo")
#' paths$raw_dir
#' }
#' @export
ehr_paths <- function(src_name = NULL, base_dir = NULL, raw_data_root = NULL,
                      config = NULL, raw_data_dir = NULL) {
  if (!is.null(raw_data_dir)) {
    stop(
      "`raw_data_dir` is retired because it ambiguously meant either a root ",
      "or a source directory. Supply the parent through `raw_data_root`.",
      call. = FALSE
    )
  }
  cfg <- config
  if (is.null(cfg)) {
    active_cfg <- .ehr_env$config
    config_path <- active_cfg$config_path %||% .ehr_default_config_path()
    if (file.exists(config_path)) {
      cfg <- ehr_load_config(config_path)
      .ehr_env$config <- cfg
    } else {
      cfg <- active_cfg %||% list()
    }
  }
  # [[ ]] rather than $: `$` partial-matches on lists, so a legacy top-level
  # cohort_profile key would otherwise be returned as the cohort block.
  cfg_paths    <- cfg[["paths"]]    %||% list()
  cfg_database <- cfg[["database"]] %||% list()
  cfg_concepts <- cfg[["concepts"]] %||% list()
  cfg_cohort   <- cfg[["cohort"]]   %||% list()

  src_name <- src_name %||% cfg$source$src_name %||% "mimic_iv_demo"
  base_dir <- path.expand(base_dir %||% cfg$project$base_dir %||% getwd())
  # absolute from here on: paths resolved against a relative base_dir were
  # resolved against it a second time (raw_dir became base/base/...)
  if (!.ehr_is_abs_path(base_dir)) base_dir <- file.path(getwd(), base_dir)
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  profile <- tolower(cfg_cohort$profile %||% cfg$cohort_profile %||% "generic")
  fam <- src_family(src_name)
  # Paths that only make sense under one cohort profile (e.g. SCI
  # injury-cause/procedure-classification assets) live in this per-profile
  # sub-block instead of the common cohort.* fields.
  cfg_cohort_profile <- cfg_cohort$profiles[[profile]] %||% list()

  config_dir <- .ehr_resolve_path("config", base_dir)
  configured_raw_root <- raw_data_root %||% cfg_paths$raw_data_root %||%
    file.path("data", "source_data")
  configured_raw_root <- .ehr_scalar_path(configured_raw_root, "raw_data_root")
  resolved_raw_root <- .ehr_resolve_path(configured_raw_root, base_dir)
  cohort_parquet_root <- .ehr_resolve_path(
    cfg_paths$cohort_parquet_root %||% file.path("data", "cohorts_data"),
    base_dir
  )
  duckdb_root <- .ehr_resolve_path(
    cfg_paths$duckdb_root %||% file.path("data", "duckdb"),
    base_dir
  )
  sql_root <- .ehr_resolve_path("sql", base_dir)
  generated_sql_root <- .ehr_resolve_path(file.path("sql", "generated"),
                                          base_dir)

  repo_layer <- if (fam == "eicu") "eicu_code" else "mimic_code"

  resolved_raw_dir <- .ehr_as_source_raw_dir(resolved_raw_root, src_name,
                                              base_dir)
  generated_ts_sql_dir <- file.path(generated_sql_root, src_name, "ts")
  generated_dur_sql_dir <- file.path(generated_sql_root, src_name, "dur")

  list(
    src_name               = src_name,
    base_dir               = base_dir,
    raw_dir                = resolved_raw_dir,
    # Cache of the raw CSVs re-materialized as parquet by
    # cache_raw_parquet_duckdb(), keyed by table name. The cache preserves
    # original CSV column names so .ehr_select_cohort_ids_duckdb() can read it
    # as a drop-in replacement for read_csv().
    # Lives in the project, not inside the raw source directory: raw inputs
    # are treated as read-only. paths.raw_parquet_cache_root relocates it
    # (e.g. to share one cache between projects on the same raw data).
    raw_parquet_cache_dir  = file.path(.ehr_resolve_path(
      cfg_paths$raw_parquet_cache_root %||% file.path("data", "raw_parquet_cache"),
      base_dir), src_name),
    cohort_parquet_dir     = file.path(cohort_parquet_root,
                                       paste0(src_name, "_parquet")),
    duckdb_dir             = duckdb_root,
    duckdb_path            = file.path(duckdb_root, paste0(src_name, ".duckdb")),
    sql_dir                = file.path(sql_root, src_name),
    schema_sql_dir         = .ehr_pkg_or_local_sql("db_setup_sqls/schema", sql_root, src_name, fam),
    helper_sql_dir         = .ehr_pkg_or_local_sql("db_setup_sqls", sql_root, src_name, fam),

    # Package concept SQL layers that are read as templates/defaults.
    pkg_flat_sql_dir       = .ehr_pkg_or_local_sql("concepts_sqls/flat", sql_root, src_name, fam),
    pkg_repo_sql_dir       = .ehr_pkg_concepts_sql(repo_layer, fam),
    pkg_ehr_sql_dir        = .ehr_pkg_concepts_sql("ehr", fam),

    # Generated atomic SQL belongs in the user project, not the package.
    generated_ts_sql_dir   = generated_ts_sql_dir,
    generated_dur_sql_dir  = generated_dur_sql_dir,
    pkg_ts_sql_dir         = generated_ts_sql_dir,
    pkg_dur_sql_dir        = generated_dur_sql_dir,

    # Legacy alias kept for callers that still use flat_sql_dir.
    flat_sql_dir           = .ehr_pkg_or_local_sql("concepts_sqls/flat", sql_root, src_name, fam),

    # User project concept SQL layers.
    user_sql_dir           = file.path(sql_root, src_name, "concepts_sqls/user"),
    external_sql_dir       = file.path(sql_root, src_name, "concepts_sqls/external"),
    custom_sql_dir         = file.path(sql_root, src_name, "concepts_sqls/custom"),

    raw_mods_sql_dir       = .ehr_pkg_or_local_sql("raw_mods", sql_root, src_name, fam),
    config_dir             = config_dir,
    data_config_json       = .ehr_pkg_or_local_cfg("rendehr-data-source.json", config_dir, base_dir,
                                              cfg_paths$data_config_json),
    concept_config_csv_ts  = .ehr_pkg_or_local_cfg("rendehr_concepts_config.csv", config_dir, base_dir,
                                              cfg_concepts$ts_config_path, use_config_root = FALSE),
    concept_config_csv_dur = .ehr_pkg_or_local_cfg("rendehr_duration_concepts_config.csv", config_dir, base_dir,
                                              cfg_concepts$dur_config_path, use_config_root = FALSE),
    # Supplemental concept rows from EHR_EXTERNAL_CONFIG_ROOT, merged onto the base
    # config by run_atomic_concepts_unified() (a user concept replaces all base
    # rows with the same source+name; other base concepts are untouched).
    user_concept_config_csv_ts  = .ehr_config_root_file("rendehr_concepts_config.csv"),
    user_concept_config_csv_dur = .ehr_config_root_file("rendehr_duration_concepts_config.csv"),
    credentials_json       = .ehr_resolve_path(
      cfg_database$credentials_path %||% cfg$credentials_path %||%
        file.path("config", "config_pass.json"),
      base_dir
    ),
    cohort_profile         = profile,
    cohort_profile_is_default = is.null(cfg_cohort$profile) &&
      is.null(cfg$cohort_profile),
    icd_filter_path        = .ehr_profile_cfg_path("scid_icds_rendehr.rds",
                                              config_dir, base_dir,
                                              cfg_cohort$icd_filter_path,
                                              profile = profile,
                                              scid_only = TRUE),
    icds_dict_path         = .ehr_pkg_or_local_cfg("d_icd_diagnoses.csv", config_dir, base_dir,
                                              cfg_cohort$icd_dict_path),
    icds_proc_path         = .ehr_profile_cfg_path("scid_procedure_classification_rendehr.csv",
                                              config_dir, base_dir,
                                              cfg_cohort_profile$icd_proc_path,
                                              profile = profile,
                                              scid_only = TRUE),
    # General ICD-procedure dictionary, same category as icds_dict_path above
    # (not SCI-specific) - not gated to any one cohort profile.
    icds_proc_dict_path    = .ehr_pkg_or_local_cfg("d_icd_procedures.csv", config_dir, base_dir,
                                              cfg_cohort$icd_proc_dict_path),
    icd_adjudication_path  = .ehr_pkg_or_local_cfg(
      "eicu_icd_version_adjudication_rendehr.csv", config_dir, base_dir,
      cfg_cohort$icd_adjudication_path)
  )
}

#' Build the standard Postgres schema name list for a source
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @return Named list with raw, concepts, derived, and harmonized schema names.
#' @examples
#' src_schema_names("mimic_iv_demo")
#' @export
src_schema_names <- function(src_name) {
  list(
    raw        = paste0(src_name, "_coh"),
    concepts   = paste0(src_name, "_coh_concepts"),
    derived    = paste0(src_name, "_coh_derived"),
    harmonized = paste0(src_name, "_coh_harmonized")
  )
}

.ehr_schema_name <- function(schema, schema_names) {
  if (schema %in% c("ts", "flat", "dur", "harmonized")) {
    return(schema_names$harmonized)
  }
  if (schema %in% names(schema_names)) return(schema_names[[schema]])
  schema
}

#' Create project directories for a source
#'
#' Creates the raw data, cohort parquet, source SQL, generated SQL, and config
#' directories implied by `ehr_paths()`. It also creates starter project and
#' credentials JSON files when they are missing. If a project config has been
#' loaded with `ehr_use_config()`, its path roots are used by default. If a
#' project config file already exists and no config has been loaded, it is used
#' for path resolution.
#'
#' @param src_name Source name. Defaults to the loaded project config.
#' @param base_dir Project root directory. Defaults to the loaded project
#'   config, then current working directory.
#' @param raw_data_root Optional parent storage-root override. Source paths are
#'   derived beneath it.
#' @param config Optional config list from `ehr_load_config()`.
#' @param create_config Create `config/ehr_config.json` if it is missing?
#'   Default TRUE.
#' @param overwrite_config Overwrite an existing project config? Default FALSE.
#' @param create_credentials Create `config/config_pass.json` if it is missing?
#'   Default TRUE.
#' @param overwrite_credentials Overwrite an existing credentials file? Default
#'   FALSE.
#' @return Invisibly returns the paths list.
#' @examples
#' \dontrun{
#' ehr_init_project(src_name = "mimic_iv_demo", base_dir = "~/my_project")
#' }
#' @export
ehr_init_project <- function(src_name = NULL, base_dir = NULL,
                             raw_data_root = NULL, config = NULL,
                             create_config = TRUE, overwrite_config = FALSE,
                             create_credentials = TRUE,
                             overwrite_credentials = FALSE) {
  cfg <- config %||% .ehr_env$config
  if (is.null(config) && !is.null(base_dir) && !is.null(cfg) &&
      !identical(normalizePath(cfg$project$base_dir %||% "", mustWork = FALSE),
                 normalizePath(base_dir, mustWork = FALSE))) {
    # An explicit base_dir names a project of its own: read its config if it
    # has one, and never create or reuse another project's.
    own <- file.path(base_dir, "config", "ehr_config.json")
    cfg <- if (file.exists(own)) ehr_load_config(own) else NULL
  }
  config_path <- .ehr_default_config_path()

  if (is.null(cfg) && is.null(base_dir) && file.exists(config_path)) {
    cfg <- ehr_load_config(config_path)
    message("rendehR: using existing project config at '", config_path, "'.")
  }

  paths <- ehr_paths(src_name = src_name, base_dir = base_dir,
                     raw_data_root = raw_data_root, config = cfg)

  if (isTRUE(create_config)) {
    config_path <- if (!is.null(cfg) && !is.null(cfg$config_path)) {
      cfg$config_path
    } else {
      file.path(paths$config_dir, "ehr_config.json")
    }

    if (!file.exists(config_path) || isTRUE(overwrite_config)) {
      ehr_create_config_template(path = config_path,
                                 src_name = paths$src_name,
                                 base_dir = paths$base_dir,
                                 raw_data_root = raw_data_root,
                                 overwrite = overwrite_config)
      message("rendehR: edit '", config_path,
              "' if you want different paths, source, or database settings.")
      profile_hint <- if (identical(paths$cohort_profile, "scid"))
        " (the packaged SCI reference cohort)" else " (the generic profile)"
      message("rendehR: cohort.profile defaults to '", paths$cohort_profile,
              "'", profile_hint, ". Set cohort.profile and your own ",
              "icd_filter_path in '", config_path, "' if needed.")
    } else {
      message("rendehR: config already exists at '", config_path,
              "'. Keeping existing file.")
    }
  }

  if (isTRUE(create_credentials)) {
    credentials_path <- paths$credentials_json
    if (!file.exists(credentials_path) || isTRUE(overwrite_credentials)) {
      ehr_create_credentials_template(path = credentials_path,
                                      overwrite = overwrite_credentials)
      message("rendehR: edit '", credentials_path,
              "' to save Postgres or PhysioNet credentials.")
    } else {
      message("rendehR: credentials file already exists at '", credentials_path,
              "'. Keeping existing file.")
    }
  }

  # custom_sql_dir is intentionally not scaffolded here: it is not part of
  # run_concepts()'s own layer sequence (unlike user_sql_dir/external_sql_dir),
  # only a fallback save location for run_external_concepts(save_adapted=TRUE)
  # called directly. Scaffolding it here made it look like a third peer of
  # user/external without functioning as one.
  dirs <- c(
    paths$config_dir,
    paths$raw_dir,
    paths$cohort_parquet_dir,
    paths$sql_dir,
    paths$user_sql_dir,
    paths$external_sql_dir,
    paths$generated_ts_sql_dir,
    paths$generated_dur_sql_dir
  )
  for (d in unique(dirs)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  message("rendehR: project directories initialized for '", paths$src_name, "'.")
  invisible(paths)
}

# ---------------------------------------------------------------------------
# Helper tables (unified - same logic for all sources)
# ---------------------------------------------------------------------------

#' Build admission, ICU, and visit window tables for any source
#'
#' Runs `window_tables.sql` to create three source spines in the raw schema:
#' `admission_windows` (one row per hospital admission), `icu_windows` (one row
#' per ICU stay), and `visit_windows` (admission-to-ICU crosswalk that retains
#' admissions without ICU stays). Always required; call after `run_db()`. Can be
#' re-run independently at any time. Replacement is transactional: if SQL
#' execution or output validation fails, the previous valid window tables are
#' restored.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then "rendehr".
#' @param base_dir Project root directory. Defaults to current working directory.
#' @return `NULL`, invisibly. Called for its side effect of writing admission_windows, icu_windows and visit_windows.
#' @examples
#' \dontrun{
#' build_window_tables("mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
build_window_tables <- function(src_name, db_name = NULL, base_dir = NULL) {
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name      <- .ehr_resolve_db_name(db_name)
  schema_names <- src_schema_names(src_name)
  creds        <- load_credentials(paths$credentials_json)

  con <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql_file <- file.path(paths$helper_sql_dir, "window_tables.sql")
  if (!file.exists(sql_file)) {
    stop("window_tables.sql not found for '", src_name, "' at ", sql_file,
         call. = FALSE)
  }

  raw_schema <- .ehr_validate_identifier(schema_names$raw, "raw schema")
  raw_quoted <- .ehr_quote_identifier(con, raw_schema)
  fam <- src_family(src_name)
  admission_key <- if (identical(fam, "eicu")) {
    "patienthealthsystemstayid"
  } else {
    "hadm_id"
  }
  icu_key <- if (identical(fam, "eicu")) {
    "patientunitstayid"
  } else if (identical(fam, "mimic_iii")) {
    "icustay_id"
  } else if (identical(fam, "mimic_iv")) {
    "stay_id"
  } else {
    stop("Window-table SQL is supported only for built-in source families.",
         call. = FALSE)
  }

  tx_open <- FALSE
  DBI::dbBegin(con)
  tx_open <- TRUE
  tryCatch({
    DBI::dbExecute(
      con, glue::glue("SET LOCAL search_path TO {raw_quoted};"),
      immediate = TRUE
    )
    for (tbl in c("visit_windows", "icu_windows", "admission_windows")) {
      .ehr_drop_relation(con, raw_schema, tbl)
    }
    run_sql_file(con, sql_file)

    required <- list(
      admission_windows = c(admission_key, "admittime", "dischtime"),
      icu_windows = c(admission_key, icu_key, "icu_intime", "icu_outtime"),
      visit_windows = c(admission_key, icu_key, "admittime", "dischtime")
    )
    for (tbl in names(required)) {
      cols <- DBI::dbListFields(
        con, DBI::Id(schema = raw_schema, table = tbl))
      missing_cols <- setdiff(required[[tbl]], cols)
      if (length(missing_cols)) {
        stop(raw_schema, ".", tbl, " is missing required column(s): ",
             paste(missing_cols, collapse = ", "), call. = FALSE)
      }
    }
    for (spec in list(
      c(table = "admission_windows", key = admission_key),
      c(table = "icu_windows", key = icu_key)
    )) {
      fqtn <- .ehr_quote_relation(con, raw_schema, spec[["table"]])
      key <- .ehr_quote_identifier(con, spec[["key"]])
      key_check <- DBI::dbGetQuery(con, glue::glue(
        "SELECT COUNT(*) FILTER (WHERE {key} IS NULL) AS null_keys, ",
        "COUNT(*) - COUNT(DISTINCT {key}) AS duplicate_keys FROM {fqtn}"
      ))
      if (key_check$null_keys[[1]] != 0 || key_check$duplicate_keys[[1]] != 0) {
        stop(raw_schema, ".", spec[["table"]],
             " failed non-null/unique key validation.", call. = FALSE)
      }
    }
    DBI::dbCommit(con)
    tx_open <- FALSE
  }, error = function(e) {
    if (tx_open && DBI::dbIsValid(con)) DBI::dbRollback(con)
    stop(conditionMessage(e), call. = FALSE)
  })

  message("Window tables built: ", src_name,
          " (admission_windows, icu_windows, visit_windows)")
  invisible(NULL)
}

#' Build ICU windows for a source
#'
#' Compatibility alias for `build_window_tables()`.
#' @inheritParams build_window_tables
#' @return `NULL`, invisibly. Called for its side effect of writing admission_windows, icu_windows and visit_windows.
#' @examples
#' \dontrun{
#' build_icu_windows("mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
build_icu_windows <- function(src_name, db_name = NULL, base_dir = NULL) {
  build_window_tables(src_name = src_name, db_name = db_name, base_dir = base_dir)
}

#' Build visit windows for a source
#'
#' Compatibility alias for `build_window_tables()`.
#' @inheritParams build_window_tables
#' @return `NULL`, invisibly. Called for its side effect of writing admission_windows, icu_windows and visit_windows.
#' @examples
#' \dontrun{
#' build_visit_windows("mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
build_visit_windows <- function(src_name, db_name = NULL, base_dir = NULL) {
  build_window_tables(src_name = src_name, db_name = db_name, base_dir = base_dir)
}

#' Build generic cohort information
#'
#' Writes cohort_info with generic columns only: ehr_id, hospitalization
#' sequence, diagnostic ICD summary, and the cohort membership flag
#' `cohort_index`. Use this when you do not need domain-specific enrichment
#' (no classify_fn).
#'
#' @param src_name Source name.
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then "rendehr".
#' @param base_dir Project root directory. Defaults to getwd().
#' @param icd_filter_path Deprecated and ignored: membership comes from the
#'   cohort definition recorded at import (see [run_cohort_info()]).
#' @param analysis_level Grain of the composite `ehr_id` key and of
#'   `cohort_index`: `"admission"` (default) keys on patient + admission;
#'   `"icu"` additionally appends the ICU stay id.
#' @return Invisibly returns the built `cohort_info` data frame.
#' @examples
#' \dontrun{
#' build_cohort_info("mimic_iv_demo", db_name = "rendehr",
#'   icd_filter_path = "config/my_icd_codes.csv")
#' }
#' @export
build_cohort_info <- function(src_name, db_name = NULL, base_dir = NULL,
                               icd_filter_path = NULL,
                               analysis_level = c("admission", "icu")) {
  analysis_level <- match.arg(analysis_level)
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name      <- .ehr_resolve_db_name(db_name)

  schema_names <- src_schema_names(src_name)
  creds        <- load_credentials(paths$credentials_json)
  con          <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  run_cohort_info(
    con             = con,
    schema_names    = schema_names,
    src_name        = src_name,
    icd_filter_path = icd_filter_path,
    icds_dict_path  = paths$icds_dict_path,
    icd_adjudication_path = paths$icd_adjudication_path,
    classify_fn     = NULL,
    analysis_level  = analysis_level
  )
}

# Empty ICD filter used when cohort_info should get its cohort flag from the
# generalized cohort_index spine rather than ICD membership.
.empty_icd_filter_for_source <- function(src_name) {
  src_family(src_name)
  data.frame(icd_code = character(), icd_version = character(),
             stringsAsFactors = FALSE)
}

# Identifier values as join keys: character, without scientific notation for
# numeric ids (as.character(1e5) is "1e+05"), NA kept as NA.
.scope_key_chr <- function(x) {
  out <- if (is.numeric(x) && !inherits(x, "integer64"))
    format(x, scientific = FALSE, trim = TRUE, digits = 15)
  else as.character(x)
  out[is.na(x)] <- NA_character_
  out
}

# Attach the single public membership flag cohort_info.cohort_index. It is the
# canonical cohort-definition membership, cohort_hierarchy.definition_filter_index
# (the same definition as visit_windows.cohort_index), never import scope and
# never an independent ICD calculation. ICD cohorts select hospitalizations, so
# every ICU stay of a qualifying hospitalization is a member; SQL, direct-ID,
# function and Sepsis-3 cohorts keep their selected grain. At ICU level each
# row takes its own hierarchy row's flag (patient, admission and ICU ids, NULL
# ICU matching NULL for an admission without ICU stay); at admission level
# every row of an admission takes any() over that admission's rows. Membership
# does not say that all of a member's data was imported.
#
# The hierarchy is reduced to one row per key before the join, so the join
# cannot duplicate cohort_info rows, and a cohort_info row without a hierarchy
# row is an error rather than silent non-membership. The grain used is kept as
# internal metadata (attribute, published as cohort_info_meta), not a column.
.attach_cohort_membership <- function(con, raw_schema, src_name, cohort_info,
                                      analysis_level = c("admission", "icu")) {
  analysis_level <- match.arg(analysis_level)
  rebuild <- paste0("Rebuild the cohort with import_cohort_general() and ",
                    "run_db(); membership is not derived from import scope ",
                    "or hos_cohort_flg.")
  if (.pg_table_exists(con, raw_schema, "cohort_meta")) {
    stale <- .ehr_stale_selection_reason(
      get_table_from_db(con, raw_schema, "cohort_meta"))
    if (!is.null(stale)) stop(raw_schema, ": ", stale, ".", call. = FALSE)
  }
  if (!.pg_table_exists(con, raw_schema, "cohort_hierarchy"))
    stop(raw_schema, ".cohort_hierarchy not found: cohort_info.cohort_index ",
         "is read from it. ", rebuild, call. = FALSE)

  ladder <- .cohort_id_ladder(src_name)
  keys <- if (identical(analysis_level, "icu")) ladder else ladder[-length(ladder)]
  hier_cols <- DBI::dbListFields(
    con, DBI::Id(schema = raw_schema, table = "cohort_hierarchy"))
  missing_cols <- setdiff(c(keys, "definition_filter_index"), hier_cols)
  if (length(missing_cols))
    stop(raw_schema, ".cohort_hierarchy is missing column(s): ",
         paste(missing_cols, collapse = ", "), ". ", rebuild, call. = FALSE)
  missing_cols <- setdiff(keys, names(cohort_info))
  if (length(missing_cols))
    stop("cohort_info is missing id column(s) needed to map cohort_index: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  hier <- get_table_from_db(con, raw_schema, "cohort_hierarchy")
  flag <- hier$definition_filter_index
  flag <- if (is.logical(flag)) flag
          else ifelse(is.na(flag), NA,
                      tolower(as.character(flag)) %in% c("true", "t", "1"))
  if (anyNA(flag))
    stop(raw_schema, ".cohort_hierarchy.definition_filter_index has missing ",
         "values. ", rebuild, call. = FALSE)

  key_cols <- paste0(".member_key_", seq_along(keys))
  map <- tibble::as_tibble(stats::setNames(
    lapply(hier[keys], .scope_key_chr), key_cols))
  map$cohort_index <- flag
  map <- map %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarise(cohort_index = any(cohort_index), .groups = "drop")

  ci_keys <- tibble::as_tibble(stats::setNames(
    lapply(cohort_info[keys], .scope_key_chr), key_cols))
  out <- dplyr::bind_cols(
    dplyr::select(cohort_info, -dplyr::any_of(
      c("hos_cohort_flg", "scope_index", "scope_level", "cohort_index"))),
    ci_keys) %>%
    dplyr::left_join(map, by = key_cols)
  if (nrow(out) != nrow(cohort_info))
    stop("Mapping cohort_index changed the number of cohort_info rows.",
         call. = FALSE)
  unmapped <- sum(is.na(out$cohort_index))
  if (unmapped)
    stop(unmapped, " cohort_info row(s) have no ", raw_schema,
         ".cohort_hierarchy row at the ", analysis_level, " grain (",
         paste(keys, collapse = ", "), "). The raw schema and its hierarchy ",
         "disagree. ", rebuild, call. = FALSE)
  out <- dplyr::select(out, -dplyr::all_of(key_cols))
  attr(out, "ehr_membership_level") <- analysis_level
  out
}

# Internal grain metadata for cohort_info.cohort_index: one row per source. Read
# by ICU-level accessors, which must not filter on admission-grain membership
# (it would admit every sibling ICU stay of a unit-selected cohort).
.cohort_info_meta_table <- function(src_name, analysis_level) {
  data.frame(source_data = src_name, analysis_level = analysis_level,
             membership_source = "cohort_hierarchy.definition_filter_index",
             built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
             stringsAsFactors = FALSE)
}

.write_cohort_info_general_table <- function(con, schema, cohort_info,
                                             backend = c("postgres", "duckdb")) {
  match.arg(backend)
  tables <- list(
    cohort_info = cohort_info,
    cohort_info_meta = .cohort_info_meta_table(
      cohort_info$source_data[[1L]] %||% NA_character_,
      attr(cohort_info, "ehr_membership_level")))
  .clinical_publish_tables(
    con, schema, tables,
    specs = .cohort_info_publish_specs(),
    indexes = list(cohort_info = list(list(name = "idx_cohort_info_ehr_id",
                                            columns = "ehr_id")))
  )
}

.cohort_info_publish_specs <- function() list(
  cohort_info = list(required_columns = c("ehr_id", "source_data", "cohort_index"),
                     nonmissing_columns = c("ehr_id", "source_data", "cohort_index")),
  cohort_info_meta = list(required_columns = c("source_data", "analysis_level"),
                          nonmissing_columns = c("source_data", "analysis_level")))

.build_cohort_info_general_core <- function(con, src_name, schema_names,
                                            icds_dict_path = NULL,
                                            icd_adjudication_path = NULL,
                                            classify_fn = NULL,
                                            classify_args = list(),
                                            analysis_level = c("admission", "icu"),
                                            backend = c("postgres", "duckdb"),
                                            publish = TRUE) {
  analysis_level <- match.arg(analysis_level)
  backend <- match.arg(backend)

  # Membership comes from cohort_hierarchy.definition_filter_index below, never
  # from re-matching an ICD filter, so the per-source builders take no filter.
  icds_dict <- if (!is.null(icds_dict_path)) .load_icds_dict(icds_dict_path)
               else NULL

  fam <- src_family(src_name)
  adjudication <- if (identical(fam, "eicu")) {
    .load_eicu_icd_adjudication(icd_adjudication_path)
  } else NULL
  cohort_info <- switch(fam,
    mimic_iv  = .build_cohort_info_mimic_iv(con, schema_names$raw, src_name,
                                             icds_dict,
                                             classify_fn, classify_args,
                                             analysis_level),
    mimic_iii = .build_cohort_info_mimic_iii(con, schema_names$raw, src_name,
                                              icds_dict,
                                              classify_fn, classify_args,
                                              analysis_level),
    eicu      = .build_cohort_info_eicu(con, schema_names$raw, src_name,
                                        icds_dict, adjudication,
                                        classify_fn, classify_args,
                                        analysis_level),
    stop("Unsupported source family: ", fam, call. = FALSE)
  )

  cohort_info <- .attach_cohort_membership(
    con, schema_names$raw, src_name, cohort_info, analysis_level)
  cohort_info$source_data <- src_name

  if (isTRUE(publish)) {
    .write_cohort_info_general_table(con, schema_names$raw, cohort_info, backend)
  }

  message("Cohort_info: ", format(nrow(cohort_info), big.mark = ","),
          if (isTRUE(publish)) " rows written to " else " rows built for ",
          schema_names$raw, ".cohort_info (cohort_index from the cohort ",
          "definition, ", analysis_level, " level).")
  invisible(cohort_info)
}

# SCI enrichment needs an SCI ICD list (its is_sci column drives the
# classification) and a cohort selected by diagnosis that is not another named
# example: SQL, ID-list and whole-source cohorts are not SCI cohorts, and an
# import by name publishes that cohort's list as d_<name>_icds in the raw schema.
.ehr_sci_enrichment_applies <- function(icd_filter_path, con, raw_schema) {
  cols <- tryCatch(names(.load_icd_filter(icd_filter_path)), error = function(e) character())
  if (!"is_sci" %in% cols) return(FALSE)
  if (.ehr_table_exists(con, raw_schema, "cohort_meta")) {
    route <- DBI::dbGetQuery(con, sprintf("SELECT route FROM %s LIMIT 1",
      DBI::dbQuoteIdentifier(con, DBI::Id(schema = raw_schema, table = "cohort_meta"))))$route
    if (length(route) && !route[[1L]] %in% c("icd", "named_icd")) return(FALSE)
  }
  named <- DBI::dbGetQuery(con,
    "SELECT table_name FROM information_schema.tables
     WHERE table_schema = $1 AND table_name LIKE 'd\\_%\\_icds' ESCAPE '\\'",
    params = list(raw_schema))$table_name
  !length(named) || "d_scid_icds" %in% named
}

#' Build cohort_info from generalized cohort_index metadata
#'
#' Companion to [import_cohort_general()]. It builds the usual generic
#' `cohort_info` diagnostic and hospitalization summary columns in Postgres, and
#' takes the cohort membership flag `cohort_index` from the cohort definition
#' recorded at import (`cohort_hierarchy`) instead of ICD matching. Use this for direct-ID, SQL, named SQL cohorts such as sepsis3, or
#' any cohort whose membership was written by `import_cohort_general()`.
#'
#' For the `scid` cohort profile this function additionally enriches
#' `cohort_info` with the SCI classification and injury-cause columns, by
#' defaulting `classify_fn` to [scid_classify()]. MIMIC sources also gain the
#' procedure columns (`proc_*`): the procedure-classification asset is reduced
#' to one row per ICD code, and codes with conflicting operative status are
#' marked `ambiguous`. eICU has no ICD-coded procedures and reports
#' `proc_classification_status = "not_reported"`.
#'
#' @param src_name Source name.
#' @param db_name Optional Postgres database name.
#' @param base_dir Project root directory.
#' @param icd_filter_path Optional ICD filter used only to populate ICD-derived
#'   enrichment inputs. Defaults to the project's configured filter
#'   (`ehr_paths()$icd_filter_path`), which is set only for the `scid` profile.
#'   If NULL, no ICDs are treated as cohort-defining; `cohort_index` still
#'   comes from the cohort definition recorded at import.
#' @param classify_fn Optional enrichment function passed through to the source
#'   cohort_info builder. Defaults to [scid_classify()] when the cohort profile
#'   is `scid` and an ICD filter is available; any explicit value, supplied
#'   directly or through `enrichment_options()`, takes precedence.
#' @param classify_args Extra arguments for `classify_fn`. Merged over the
#'   defaults chosen for the `scid` profile, so individual entries such as
#'   `icds_proc_path` can be overridden without replacing the rest.
#' @param analysis_level Grain of the composite `ehr_id` key.
#' @return The cohort_info data frame, invisibly. It is also written to the source's raw schema.
#' @examples
#' \dontrun{
#' build_cohort_info_general("mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
#' @param cohort_selection Optional overrides from `cohort_selection_options()` or a named list.
#' @param enrichment Optional overrides from `enrichment_options()` or a named list.
build_cohort_info_general <- function(src_name, db_name = NULL, base_dir = NULL,
                                      icd_filter_path = NULL,
                                      classify_fn = NULL,
                                      classify_args = list(),
                                      analysis_level = c("admission", "icu"),
                                      cohort_selection = NULL, enrichment = NULL) {
  .ehr_apply_grouped_options(
    list(cohort_selection = cohort_selection, enrichment = enrichment),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(build_cohort_info_general)), frame = environment())
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  src_name     <- paths$src_name
  db_name      <- .ehr_resolve_db_name(db_name)
  schema_names <- src_schema_names(src_name)
  creds        <- load_credentials(paths$credentials_json)

  icd_filter_path <- icd_filter_path %||% paths$icd_filter_path

  # SCI enrichment for the scid cohort profile. This mirrors the auto-wiring in
  # build_cohort_annotations_general(); it lives here too because the
  # generalized cohort path made build_cohort_info the default stage, leaving
  # the annotation stages unreachable by default and SCI enrichment with them.
  #
  # Runs after .ehr_apply_grouped_options() on purpose: by this point an
  # explicit classify_fn, whether passed directly or via enrichment_options(),
  # is already in scope, so the is.null() guard stands down for it.
  #
  con <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (identical(paths$cohort_profile, "scid") && is.null(classify_fn) &&
      !is.null(icd_filter_path) &&
      .ehr_sci_enrichment_applies(icd_filter_path, con, schema_names$raw)) {
    classify_fn <- scid_classify
    classify_args <- utils::modifyList(list(
      icds_scid_path      = icd_filter_path,
      icds_proc_path      = paths$icds_proc_path,
      icds_proc_dict_path = paths$icds_proc_dict_path
    ), classify_args)
  }

  .build_cohort_info_general_core(
    con             = con,
    src_name        = src_name,
    schema_names    = schema_names,
    icds_dict_path  = paths$icds_dict_path,
    icd_adjudication_path = paths$icd_adjudication_path,
    classify_fn     = classify_fn,
    classify_args   = classify_args,
    analysis_level  = analysis_level,
    backend         = "postgres"
  )
}

#' Build generalized cohort_info in a DuckDB database
#'
#' DuckDB wrapper around the same generalized cohort-info core used by
#' [build_cohort_info_general()]. Run this after [materialize_cohort_duckdb()] or
#' `run_db(engine = "duckdb")` has materialized the raw cohort schema and the
#' generalized window tables in the DuckDB file.
#'
#' Unlike [build_cohort_info_general()], this variant applies no cohort-profile
#' defaults: SCI enrichment is Postgres-only, so a `scid` project run through
#' this function produces generic `cohort_info` unless `classify_fn` is supplied
#' explicitly.
#'
#' @param src_name Source name.
#' @param db_path Path to the DuckDB database. Defaults to `paths$duckdb_path`.
#' @param base_dir Project root directory.
#' @param icd_filter_path Optional ICD filter used only to populate ICD-derived
#'   enrichment inputs. Not defaulted from the project config here; `NULL` means
#'   no ICDs are treated as cohort-defining.
#' @param classify_fn Optional enrichment function passed through to the source
#'   cohort_info builder. Not defaulted to [scid_classify()] here.
#' @param classify_args Extra arguments for `classify_fn`.
#' @inheritParams build_cohort_info_general
#' @return The cohort_info data frame, invisibly. It is also written to the source's raw schema.
#' @examples
#' \dontrun{
#' build_cohort_info_general_duckdb("mimic_iv_demo", db_path = "data/duckdb/mimic_iv_demo.duckdb")
#' }
#' @export
build_cohort_info_general_duckdb <- function(src_name, db_path = NULL, base_dir = NULL,
                                             icd_filter_path = NULL,
                                             classify_fn = NULL,
                                             classify_args = list(),
                                             analysis_level = c("admission", "icu")) {
  .duckdb_require()
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  src_name     <- paths$src_name
  schema_names <- src_schema_names(src_name)
  db_path      <- db_path %||% paths$duckdb_path

  con <- connect_duckdb(db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .build_cohort_info_general_core(
    con             = con,
    src_name        = src_name,
    schema_names    = schema_names,
    icds_dict_path  = paths$icds_dict_path,
    icd_adjudication_path = paths$icd_adjudication_path,
    classify_fn     = classify_fn,
    classify_args   = classify_args,
    analysis_level  = analysis_level,
    backend         = "duckdb"
  )
}

#' Build generalized cohort annotation tables
#'
#' Generalized companion to [build_cohort_annotations()]. It preserves the
#' annotation-table behavior when ICD assets are available, but takes analysis
#' membership from the cohort hierarchy: `cohort_info.cohort_index` is copied
#' from the cohort definition recorded at import, not re-derived from ICD
#' matching. Use this after [import_cohort_general()] and [run_db()].
#'
#' @param src_name Source name.
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then "rendehr".
#' @param base_dir Project root directory. Defaults to current working directory.
#' @param icd_filter_path Optional ICD classification .rds/.csv used for
#'   annotation/enrichment only (published as `d_cohort_icd`, empty when not
#'   supplied). It is not the source of `cohort_index`. Required only for the
#'   `scid` profile's default SCI enrichment.
#' @param classify_fn Optional enrichment function passed to
#'   [build_cohort_info_general()]. If NULL and SCI ICD assets are configured,
#'   `scid_classify()` is used for enrichment.
#' @param classify_args Named list of additional arguments passed to `classify_fn`.
#' @param analysis_level Grain of the composite `ehr_id` key: `"admission"`
#'   (default) keys on patient + admission; `"icu"` additionally appends the ICU
#'   stay id.
#' @return `NULL`, invisibly. Called for its side effect of writing the cohort annotation tables to the source's raw schema.
#' @examples
#' \dontrun{
#' build_cohort_annotations_general("mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
build_cohort_annotations_general <- function(src_name, db_name = NULL, base_dir = NULL,
                                             icd_filter_path = NULL,
                                             classify_fn     = NULL,
                                             classify_args   = list(),
                                             analysis_level  = c("admission", "icu")) {
  analysis_level <- match.arg(analysis_level)
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name      <- .ehr_resolve_db_name(db_name)
  schema_names <- src_schema_names(src_name)
  creds        <- load_credentials(paths$credentials_json)

  icd_filter_path <- icd_filter_path %||% paths$icd_filter_path

  # SCI enrichment needs the SCI ICD list; without it, fail rather than
  # silently publishing a scid cohort_info with no SCI columns. Any other
  # profile runs without an ICD filter: membership comes from cohort_index.
  if (identical(paths$cohort_profile, "scid") && is.null(classify_fn) &&
      is.null(icd_filter_path)) {
    stop("SCI enrichment (cohort.profile = 'scid') requires the SCI ICD list ",
         "(icd_filter_path); none is configured.", call. = FALSE)
  }
  if (identical(paths$cohort_profile, "scid") && is.null(classify_fn)) {
    classify_fn <- scid_classify
    classify_args <- utils::modifyList(list(
      icds_scid_path      = icd_filter_path,
      icds_proc_path      = paths$icds_proc_path,
      icds_proc_dict_path = paths$icds_proc_dict_path,
      # This wrapper publishes all of its clinical tables together after the
      # build, so classification must not write the dictionary itself.
      publish             = FALSE
    ), classify_args)
  }

  con <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, glue::glue("SET search_path TO {schema_names$raw};"))

  # In the generalized path, this lookup is annotation metadata only. Cohort
  # membership is cohort_hierarchy.definition_filter_index, copied into
  # cohort_info.cohort_index by .build_cohort_info_general_core().
  d_cohort_icd <- if (!is.null(icd_filter_path)) {
    .load_icd_filter(icd_filter_path)
  } else {
    .empty_icd_filter_for_source(src_name)
  }

  cohort_info <- .build_cohort_info_general_core(
    con             = con,
    src_name        = src_name,
    schema_names    = schema_names,
    icds_dict_path  = paths$icds_dict_path,
    icd_adjudication_path = paths$icd_adjudication_path,
    classify_fn     = classify_fn,
    classify_args   = classify_args,
    analysis_level  = analysis_level,
    backend         = "postgres",
    publish         = FALSE
  )

  tables <- list(d_cohort_icd = d_cohort_icd)
  tables$cohort_info <- cohort_info
  tables$cohort_info_meta <- .cohort_info_meta_table(
    src_name, attr(cohort_info, "ehr_membership_level"))
  specs <- lapply(names(tables), function(table) {
    .cohort_info_publish_specs()[[table]] %||% list(required_columns = "icd_code")
  })
  names(specs) <- names(tables)
  indexes <- list(
    d_cohort_icd = list(list(name = "idx_d_cohort_icd_code", columns = "icd_code")),
    cohort_info = list(list(name = "idx_cohort_info_ehr_id", columns = "ehr_id"))
  )
  .clinical_publish_tables(con, schema_names$raw, tables, specs, indexes)

  message("General cohort annotation tables built: ", src_name,
          " (cohort_index from the cohort definition)")
  invisible(NULL)
}

#' Build cohort annotation tables (ICD lookup, injury cause, derived flags)
#'
#' Pipeline annotation stage (`run_cohort_annotations = TRUE`). Writes the ICD
#' classification lookup (`d_cohort_icd`), the injury cause/etiology table when
#' configured, and `cohort_info` into the raw schema, in one atomic
#' publication. It is the same as [build_cohort_annotations_general()]:
#' `cohort_index` comes from the cohort definition recorded at import, so it works for
#' every selection route - ICD, SQL, direct IDs, function or no filter - and
#' needs no ICD filter unless SCI enrichment is requested.
#'
#' @param src_name Source name.
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then "rendehr".
#' @param base_dir Project root directory. Defaults to current working directory.
#' @param icd_filter_path Optional ICD classification CSV or RDS, published as
#'   `d_cohort_icd` (empty when absent). Defaults to the configured filter (the
#'   package-embedded SCI list for the `scid` profile). Required only for the
#'   `scid` profile's default SCI enrichment; never used for membership.
#' @param classify_fn Optional enrichment function passed to the cohort-info
#'   builder. If NULL and the cohort profile is SCI, `scid_classify()` is used.
#' @param classify_args Named list of additional arguments passed to `classify_fn`.
#' @param analysis_level Grain of the composite `ehr_id` key: `"admission"`
#'   (default) keys on patient + admission; `"icu"` additionally appends the ICU
#'   stay id.
#' @return `NULL`, invisibly. Called for its side effect of writing the cohort annotation tables to the source's raw schema.
#' @examples
#' \dontrun{
#' build_cohort_annotations("mimic_iv_demo", db_name = "rendehr", classify_fn = scid_classify)
#' }
#' @export
#' @param cohort_selection Optional overrides from `cohort_selection_options()` or a named list.
#' @param enrichment Optional overrides from `enrichment_options()` or a named list.
build_cohort_annotations <- function(src_name, db_name = NULL, base_dir = NULL,
                                     icd_filter_path = NULL,
                                     classify_fn     = NULL,
                                     classify_args   = list(),
                                     analysis_level  = c("admission", "icu"),
                                     cohort_selection = NULL, enrichment = NULL) {
  .ehr_apply_grouped_options(
    list(cohort_selection = cohort_selection, enrichment = enrichment),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(build_cohort_annotations)), frame = environment())
  build_cohort_annotations_general(
    src_name        = src_name,
    db_name         = db_name,
    base_dir        = base_dir,
    icd_filter_path = icd_filter_path,
    classify_fn     = classify_fn,
    classify_args   = classify_args,
    analysis_level  = match.arg(analysis_level)
  )
}

# ---------------------------------------------------------------------------
# Stage 1-4: Import + cohort parquet  (unified)
# ---------------------------------------------------------------------------

#' Materialize the current cohort as canonical Parquet
#'
#' Resolves every supported selection route through the shared DuckDB engine,
#' constructs the source-native patient hierarchy with
#' `patient_context_index`, `definition_filter_index`, and `scope_index`, and
#' writes canonical Parquet directly. A transient DuckDB working file may be
#' used while building but is removed before publication.
#'
#' When the cohort is selected by ICD code (a named ICD cohort such as
#' `"scid"`, or a raw `icd_filter_path`), the exact code list that
#' defined the cohort is also published as a reference table, `d_<name>_icds`
#' (e.g. `d_scid_icds` for the bundled SCI cohort; a name derived from the
#' file's basename for a raw `icd_filter_path`). SQL-based cohorts (e.g.
#' `"sepsis3"`) have no ICD list and get no such table.
#'
#' @inheritParams select_cohort_ids
#' @param tables Optional bounded table subset.
#' @param memory_limit,threads,temp_directory Optional DuckDB resource limits.
#' @param table_chunk_rows Row threshold (against each table's configured
#'   `num_rows`, no extra scan needed). Tables above it are built in multiple
#'   smaller `INSERT` batches (split by a hash of the cohort link-key column)
#'   instead of one single `CREATE TABLE AS SELECT`, so peak memory stays
#'   bounded even when DuckDB's own out-of-core spilling isn't enough for a
#'   single huge table (e.g. MIMIC-III `chartevents`).
#'   Default `5e6` chunks only the few tables large enough to need it; small
#'   tables are unaffected either way. Pass `NULL` to always build every
#'   table in one shot. Reference (unfiltered) tables are never chunked.
#' @param validate_columns If TRUE, scan every configured column of every
#'   built table and count values that fail to convert (`TRY_CAST`) to their
#'   configured type, stopping the build if a required patient/visit linkage
#'   column has any failures or is missing raw data. Off by default (`FALSE`)
#'   - this scan touches the full raw data of every table (including huge
#'   ones like `chartevents`) and is expensive; skipping it trusts that the
#'   source's linkage columns are already well-formed (a reasonable default
#'   for a vetted source like published MIMIC data, less so for a new,
#'   unvetted institutional extract). Reference tables are never checked
#'   either way.
#' @param ignore_errors If TRUE (default), a raw CSV row that fails to parse
#'   (beyond what `null_padding`/lenient parsing already tolerates) is
#'   skipped rather than aborting the whole table's read - avoids losing an
#'   entire large import to one bad row. Combined with `store_rejects`, that
#'   row is still recorded, not silently dropped. Set FALSE to fail hard on
#'   any malformed row instead.
#' @param store_rejects If TRUE (default), skipped rows (when
#'   `ignore_errors = TRUE`) are recorded - which file/line, which column,
#'   why - in the returned `csv_reject_report` instead of vanishing with no
#'   trace. Has no effect when nothing gets skipped.
#' @param use_parquet_cache If TRUE (default), a table whose raw CSV has
#'   already been cached as parquet by `cache_raw_parquet_duckdb()` is read
#'   from that parquet instead of re-parsing CSV. Tables without a cached
#'   parquet still fall back to CSV. Set FALSE to always read CSV.
#' @param overwrite Safely replace the one current completed Parquet cohort?
#' @param show_progress Print aggregate progress.
#' @return Invisibly returns the normalized plan, reports, status, path, and
#'   `csv_reject_report` (a data frame, one row per rejected raw CSV row,
#'   empty if none).
#' @examples
#' \dontrun{
#' import_cohort_general("mimic_iv_demo",
#'   icd_filter_path = "config/my_icd_codes.csv")
#' }
#' @export
#' @param cohort_selection Optional overrides from `cohort_selection_options()` or a named list.
#' @param windows Optional overrides from `window_options()` or a named list.
#' @param loading Optional overrides from `loading_options()` or a named list.
#' @param execution Optional overrides from `execution_options()` or a named list.
import_cohort_general <- function(src_name = NULL,
                                  base_dir = NULL,
                                  raw_data_root = NULL,
                                  icd_filter_path = NULL,
                                  direct_ids_path = NULL,
                                  cohort = NULL,
                                  cohort_sql = NULL,
                                  setup_sql = NULL,
                                  cohort_fn = NULL,
                                  no_filter = FALSE,
                                  key = NULL,
                                  import_scope = NULL,
                                  allow_unmatched_ids = FALSE,
                                  window_sql = NULL,
                                  window_fn = NULL,
                                  tables = NULL,
                                  memory_limit = NULL,
                                  threads = NULL,
                                  temp_directory = NULL,
                                  table_chunk_rows = 5e6,
                                  validate_columns = FALSE,
                                  ignore_errors = TRUE,
                                  store_rejects = TRUE,
                                  use_parquet_cache = TRUE,
                                  overwrite = FALSE,
                                  show_progress = TRUE,
                                  cohort_selection = NULL, windows = NULL, loading = NULL, execution = NULL) {
  .ehr_apply_grouped_options(
    list(cohort_selection = cohort_selection, windows = windows, loading = loading, execution = execution),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(import_cohort_general)), frame = environment())
  .ehr_run_cohort_engine(
    output = "parquet",
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

# ---------------------------------------------------------------------------
# Stage 5-8: Load parquet -> Postgres + helper tables  (unified)
# ---------------------------------------------------------------------------

#' Load cohort parquet into Postgres
#'
#' Validates the accepted manifest-backed cohort artifact, loads and validates
#' it in a run-specific staging schema, builds the selected release's indexes,
#' and transactionally promotes the complete schema. A failed load, validation,
#' or index phase leaves the prior raw schema in place. Run
#' build_window_tables() and optionally build_cohort_annotations() afterwards
#' to add lookup and derived tables.
#'
#' @param src_name Source name.
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then "rendehr".
#' @param base_dir Project root directory.
#' @param delete_source_parquet Delete each parquet file after it is
#'   successfully loaded into Postgres. Default FALSE.
#' @param overwrite If FALSE (default), a manifest-complete existing raw schema
#'   is retained and any missing indexes are resumed idempotently. Otherwise a
#'   complete replacement is built in staging. If TRUE, always build and
#'   validate a fresh staged replacement.
#' @param unlogged If TRUE, load into UNLOGGED tables for speed. Much faster, but
#'   UNLOGGED tables are emptied on an unclean shutdown and are not replicated -
#'   safe here only because the raw tables are reproducible from the cohort
#'   parquet. Default FALSE. Use `set_logged = TRUE` to restore durability after.
#' @param set_logged If TRUE (with `unlogged = TRUE`), convert tables back to
#'   durable LOGGED after each loads (rewrites the table). Default FALSE.
#' @param synchronous_commit_off If TRUE, set `synchronous_commit = off` for the
#'   load session (restored afterwards) to speed bulk loading. Default FALSE.
#' @param engine Storage backend: "postgres" (default) or "duckdb". With
#'   "duckdb" the cohort parquet is loaded into a single `.duckdb` file via
#'   `run_db_duckdb()`; `db_name` and the Postgres-only tuning flags are ignored.
#'   `run_one_source(engine = "duckdb")` uses this path and then extracts the
#'   concepts in the same DuckDB file; harmonization stays PostgreSQL-only.
#' @param db_path DuckDB file path (only used when `engine = "duckdb"`). Default
#'   uses `paths.duckdb_root` (`data/duckdb` by default) plus `{src_name}.duckdb`.
#' @param create_indexes Build the bundled post-load indexes before promotion?
#'   Default TRUE. Set FALSE to explicitly defer the potentially expensive
#'   step. Existing complete schemas resume the idempotent index phase.
#' @return Invisibly, a list containing aggregate load, validation, and index
#'   reports for PostgreSQL, or the DuckDB load report.
#' @examples
#' \dontrun{
#' run_db("mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
#' @param loading Optional overrides from `loading_options()` or a named list.
#' @param execution Optional overrides from `execution_options()` or a named list.
run_db <- function(src_name, db_name = NULL, base_dir = NULL,
                   delete_source_parquet = FALSE, overwrite = FALSE,
                   unlogged = FALSE, set_logged = FALSE,
                   synchronous_commit_off = FALSE,
                   engine = c("postgres", "duckdb"), db_path = NULL,
                   create_indexes = TRUE,
                   loading = NULL, execution = NULL) {
  .ehr_apply_grouped_options(
    list(loading = loading, execution = execution),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(run_db)), frame = environment())
  engine <- match.arg(engine)
  if (identical(engine, "duckdb")) {
    # DuckDB backend covers the load stage only (see R/duckdb_backend.R).
    # db_name is a Postgres concept; the DuckDB store is a single file (db_path).
    return(invisible(run_db_duckdb(
      src_name = src_name, db_path = db_path, base_dir = base_dir,
      overwrite = overwrite
    )))
  }

  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name      <- .ehr_resolve_db_name(db_name)
  schema_names <- src_schema_names(src_name)
  creds        <- load_credentials(paths$credentials_json)

  artifact <- .ehr_check_parquet_artifact(paths$cohort_parquet_dir)
  if (!isTRUE(artifact$ok))
    stop("Refusing PostgreSQL load: invalid cohort artifact (",
         artifact$reason, ").", call. = FALSE)

  ensure_database_exists(db_name, creds)
  con <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  raw_schema <- .ehr_validate_db_identifier(schema_names$raw, "raw schema")
  create_schemas(con, schema_names[names(schema_names) != "raw"])
  prior_raw_exists <- .pg_schema_exists(con, raw_schema)
  cfg <- load_src_cfg(paths$data_config_json, paths$src_name)
  table_schema <- .duckdb_schema_for_paths(paths, cfg)
  index_file <- file.path(paths$helper_sql_dir, "indexes.sql")

  if (!isTRUE(overwrite) && prior_raw_exists) {
    prior_report <- tryCatch(
      .pg_validate_loaded_artifact(con, raw_schema, paths$cohort_parquet_dir),
      error = function(e) NULL
    )
    if (!is.null(prior_report)) {
      index_report <- data.frame()
      if (isTRUE(create_indexes) && file.exists(index_file)) {
        index_report <- .ehr_run_postload_indexes(
          con, index_file, raw_schema, table_schema
        )
      }
      if (isTRUE(delete_source_parquet)) {
        parquet_files <- list.files(
          paths$cohort_parquet_dir, pattern = "\\.parquet$", full.names = TRUE
        )
        removed <- file.remove(parquet_files)
        if (any(!removed))
          warning("Complete database retained but some source Parquet files ",
                  "could not be deleted.", call. = FALSE)
      }
      message("PostgreSQL raw schema is already complete: ", raw_schema)
      return(invisible(list(
        load = transform(prior_report, action = "skipped"),
        validation = prior_report,
        indexes = index_report
      )))
    }
  }

  suffix <- paste0("_", Sys.getpid())
  staging <- .ehr_validate_db_identifier(
    paste0(substr(raw_schema, 1L, 63L - nchar("__staging") - nchar(suffix)),
           "__staging", suffix),
    "staging schema")
  backup <- .ehr_validate_db_identifier(
    paste0(substr(raw_schema, 1L, 63L - nchar("__prior") - nchar(suffix)),
           "__prior", suffix),
    "backup schema")
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))

  DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", q(staging), " CASCADE"))
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", q(staging)))
  promoted <- FALSE
  on.exit({
    if (!promoted && DBI::dbIsValid(con))
      try(DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", q(staging),
                                    " CASCADE")), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("SET search_path TO ", q(staging)))
  run_sql_dir(con, paths$schema_sql_dir)

  load_report <- load_cohort_parquet_to_postgres(
    con                    = con,
    parquet_dir            = paths$cohort_parquet_dir,
    schema                 = staging,
    overwrite              = TRUE,
    delete_source_parquet  = FALSE,
    unlogged               = unlogged,
    set_logged             = set_logged,
    synchronous_commit_off = synchronous_commit_off
  )
  validation_report <- .pg_validate_loaded_artifact(
    con, staging, paths$cohort_parquet_dir
  )

  index_report <- data.frame()
  if (isTRUE(create_indexes) && file.exists(index_file)) {
    index_report <- .ehr_run_postload_indexes(
      con, index_file, staging, table_schema
    )
  }

  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", q(backup), " CASCADE"))
    if (prior_raw_exists)
      DBI::dbExecute(con, paste0("ALTER SCHEMA ", q(raw_schema),
                                 " RENAME TO ", q(backup)))
    DBI::dbExecute(con, paste0("ALTER SCHEMA ", q(staging),
                               " RENAME TO ", q(raw_schema)))
    if (prior_raw_exists)
      DBI::dbExecute(con, paste0("DROP SCHEMA ", q(backup), " CASCADE"))
    DBI::dbCommit(con)
    promoted <- TRUE
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("PostgreSQL cohort publication failed; prior raw schema preserved: ",
         conditionMessage(e), call. = FALSE)
  })

  if (isTRUE(delete_source_parquet)) {
    parquet_files <- list.files(paths$cohort_parquet_dir,
                                pattern = "\\.parquet$", full.names = TRUE)
    removed <- file.remove(parquet_files)
    if (any(!removed))
      warning("Database promotion succeeded but some source Parquet files ",
              "could not be deleted.", call. = FALSE)
  }

  has_general_spine <- all(file.exists(file.path(
    paths$cohort_parquet_dir,
    c("admission_windows.parquet", "icu_windows.parquet",
      "visit_windows.parquet", "cohort_meta.parquet")
  )))
  if (has_general_spine) {
    message("Parquet loaded into Postgres: ", src_name,
            "\nLoaded window tables/cohort_meta from cohort parquet; do not run ",
            "build_window_tables() unless you intentionally want to replace the ",
            "cohort-index spine.")
  } else {
    message("Parquet loaded into Postgres: ", src_name,
            "\nRun build_window_tables(\"", src_name, "\") next.")
  }
  invisible(list(
    load = load_report,
    validation = validation_report,
    indexes = index_report
  ))
}

# ---------------------------------------------------------------------------
# Stage 9-10: Atomic + handcrafted concept extraction  (unified)
# ---------------------------------------------------------------------------

#' Extract concepts for one source
#'
#' Create source-specific concept tables from packaged definitions, supplemental
#' concept definitions, and user or external SQL. Select one or more layers with
#' `layers`; by default, all enabled layers are processed.
#'
#' @param src_name Character string identifying the source.
#' @param db_name Character string naming the PostgreSQL database. If `NULL`,
#'   use the active project configuration, falling back to `"rendehr"`.
#'   Ignored when `engine = "duckdb"`.
#' @param base_dir Character string giving the project directory, or `NULL` to
#'   resolve it from the active project configuration and working directory.
#' @param engine Character string, `"postgres"` or `"duckdb"`. Default:
#'   `"postgres"`. DuckDB uses the source database path returned by [ehr_paths()].
#' @param user_concepts Character string giving a supplemental concept CSV.
#'   A concept defined there replaces all packaged rows with the same source
#'   and concept name; other packaged concepts are kept unchanged.
#'   The file must exist. If `NULL`, use a configured supplemental file when
#'   available.
#' @param user_schema Character string naming the target schema for user SQL.
#'   `NULL` uses the source concepts schema.
#' @param external_sql_dir Character string giving an external SQL directory.
#'   `NULL` uses the external-concept directory returned by [ehr_paths()].
#' @param external_schema Character string naming the target schema for a single
#'   external module. `NULL` derives the schema from the module name. Separate
#'   submodules always receive separate schemas.
#' @param concept_name Character vector of concept names or filenames without
#'   `.sql`. Filters the `flat`, `ts`, `dur`, and `user` layers; `NULL` includes all
#'   concepts in those layers. The `ehr` and `external` layers are not filtered.
#' @param dry_run Logical. If `TRUE`, prepare concept SQL without building
#'   concept tables or registering provenance. Default: `FALSE`. See Details
#'   for database connection and file-writing behavior.
#' @param rm_duplicates Logical. Remove chart rows that are validated copies of
#'   lab results (MIMIC) or of monitor values (eICU) in the ts and dur layers,
#'   and repeated chart rows of a ts
#'   concept (see [run_atomic_concepts_unified()]). Default `TRUE`. `FALSE`
#'   extracts exactly the same rows and removes none.
#' @param layers Character vector of layer names from the table below, or `NULL`
#'   to run all enabled layers. Layers run in the listed order, and required
#'   input tables must already exist.
#' @param concepts Named list from [concept_options()] or an ordinary named list
#'   of concept options. A setting cannot be supplied both here and as a direct
#'   argument, even if the values are equal.
#'
#' @details
#' The source's cohort and required observation-window tables must already exist.
#' The function opens and closes its own database connection.
#' \tabular{lll}{
#'   Layer \tab Definitions \tab Output \cr
#'   \code{flat} \tab Packaged SQL \tab Source concepts schema \cr
#'   \code{ts} \tab Time-series definitions from CSV \tab Source concepts schema \cr
#'   \code{dur} \tab Duration definitions from CSV \tab Source concepts schema \cr
#'   \code{ehr} \tab Maintained derived-concept SQL \tab Source derived schema \cr
#'   \code{user} \tab Project SQL \tab Source concepts schema or \code{user_schema} \cr
#'   \code{external} \tab External SQL modules \tab Module-specific schemas
#' }
#' Upstream `repo` SQL is reference material and is not an executable layer.
#'
#' The time-series and duration SQL generators support PostgreSQL and DuckDB.
#' Other SQL must be compatible with the selected engine. The `ehr` layer requires
#' PostgreSQL: it is skipped in a default DuckDB run, and explicitly requesting it
#' with DuckDB raises an error.
#'
#' A dry run still opens a database connection, may initialize schemas, and writes
#' generated time-series or duration SQL files to the configured output directories.
#' It does not build concept tables. Reported SQL failures in an executing run are
#' collected and raised together after the selected layers have been attempted.
#'
#' @return Invisibly, a named list of results for layers that processed at least
#'   one concept or file. In an executing run, each element is a named logical
#'   vector indicating success (`TRUE`) or failure (`FALSE`). For a dry run,
#'   the `flat`, `ts`, `dur`, and `user` elements are named character vectors of
#'   SQL; `ehr` and `external` report SQL adaptation success as logical vectors.
#'   Execution failures raise an error instead of returning a completed result.
#' @seealso [concept_options()], [run_one_source()], [run_flat_sql_concepts()],
#'   [run_atomic_concepts_unified()], [run_external_concepts()]
#' @examples
#' selected <- concept_options(layers = c("flat", "ts"))
#'
#' \dontrun{
#' # Requires an existing cohort and configured database connection.
#' run_concepts("mimic_iv_demo", base_dir = "project", layers = "flat")
#' run_concepts("mimic_iv_demo", base_dir = "project", concepts = selected)
#' }
#' @export
run_concepts <- function(src_name, db_name = NULL, base_dir = NULL,
                         engine           = c("postgres", "duckdb"),
                         user_concepts    = NULL,
                         user_schema      = NULL,
                         external_sql_dir = NULL,
                         external_schema  = NULL,
                         concept_name     = NULL,
                         dry_run          = FALSE, layers = NULL,
                         rm_duplicates    = TRUE,
                         concepts = NULL) {
  .ehr_apply_grouped_options(
    list(concepts = concepts),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(run_concepts)), frame = environment())
  engine        <- match.arg(engine)
  selected_layers <- .ehr_select_concept_layers(layers, engine)
  layer_results <- list()
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  schema_names <- src_schema_names(src_name)

  # An explicit user_concepts CSV wins; otherwise it defaults to your standing
  # per-project supplemental file under EHR_EXTERNAL_CONFIG_ROOT (per-type: ts
  # vs dur config file) if one exists there. Both feed the same overlay slot
  # in run_atomic_concepts_unified(): rows override base-CSV concepts sharing
  # the same concept_name + source; new concept_names are simply added.
  if (!is.null(user_concepts) && !file.exists(user_concepts))
    stop("`user_concepts` file not found: ", user_concepts, call. = FALSE)
  user_concepts_ts  <- user_concepts %||% paths$user_concept_config_csv_ts
  user_concepts_dur <- user_concepts %||% paths$user_concept_config_csv_dur

  if (engine == "postgres") {
    db_name <- .ehr_resolve_db_name(db_name)
    creds   <- load_credentials(paths$credentials_json)
    ensure_database_exists(db_name, creds)
    con <- connect_postgres(db_name, creds)
  } else {
    con <- connect_duckdb(paths$duckdb_path)
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  create_schemas(con, schema_names)
  DBI::dbExecute(con, glue::glue("SET search_path TO {schema_names$raw};"))

  # Preflight: sources that build hospital-grain concepts need the admission
  # spine. Fail early with a clear message if an older DB has icu_windows but not
  # admission_windows (instead of an opaque relation error mid-generation).
  # .ehr_table_exists() is a plain DBI::dbExistsTable() check, so it works
  # unchanged against either backend (unlike Postgres's to_regclass()).
  if (any(c("flat", "ts", "dur", "ehr") %in% selected_layers) &&
      file.exists(file.path(paths$helper_sql_dir, "window_tables.sql"))) {
    if (!.ehr_table_exists(con, schema_names$raw, "admission_windows"))
      stop("admission_windows not found in schema '", schema_names$raw,
           "'. Run build_window_tables(\"", src_name,
           "\") before run_concepts() (it builds admission_windows, icu_windows, and visit_windows).")
    # Sources with raw modifications (eICU's *_mod tables) read them in many
    # concepts; without them every such concept fails on its own.
    mods <- tools::file_path_sans_ext(list.files(paths$raw_mods_sql_dir %||% "", "\\.sql$"))
    missing_mods <- mods[!vapply(mods, function(t) .ehr_table_exists(con, schema_names$raw, t), logical(1))]
    if (length(missing_mods))
      stop("Raw modification table(s) not found in schema '", schema_names$raw, "': ",
           paste(missing_mods, collapse = ", "), ". Run ",
           if (engine == "duckdb") "run_raw_mods_duckdb" else "run_raw_mods",
           "(\"", src_name, "\") before run_concepts().", call. = FALSE)
  }

  if ("flat" %in% selected_layers) {
    # -- Layer 1: flat/ - package handcrafted SQL --------------------------------
    message("\n-- Layer 1: flat concepts (rendehR package)")
    flat_results <- run_flat_sql_concepts(
      con             = con,
      flat_sql_dir    = paths$pkg_flat_sql_dir,
      raw_schema      = schema_names$raw,
      concepts_schema = schema_names$concepts,
      skip_on_error   = TRUE,
      concept_name    = concept_name,
      dry_run         = dry_run
    )
    if (length(flat_results) > 0L) layer_results$flat <- flat_results
    if (!isTRUE(dry_run))
      .register_pkg_layer_provenance(con, src_name, paths$pkg_flat_sql_dir,
                                     schema_names$concepts,
                                     succeeded = names(flat_results)[flat_results])
  }

  if ("ts" %in% selected_layers) {
    # -- Layer 2: ts/ - generated time-series SQL, saved to package -------------
    message("\n-- Layer 2: ts concepts (rendehR package, generated from CSV)")
    ts_results <- run_atomic_concepts_unified(
      con                 = con,
      concept_config_path = paths$concept_config_csv_ts,
      src_name            = src_name,
      schema_names        = schema_names,
      typ                 = "ts",
      sql_out_dir         = paths$pkg_ts_sql_dir,
      user_concepts       = user_concepts_ts,
      concept_name        = concept_name,
      dry_run             = dry_run,
      engine              = engine,
      rm_duplicates       = rm_duplicates
    )
    if (length(ts_results) > 0L) layer_results$ts <- ts_results

    if (!isTRUE(dry_run))
      auto_register_atomic_provenance(con, src_name, schema_names$concepts,
                                      paths$concept_config_csv_ts,
                                      concept_names = names(ts_results)[ts_results])
  }

  if ("dur" %in% selected_layers) {
    # -- Layer 3: dur/ - generated duration SQL, saved to package ---------------
    message("\n-- Layer 3: dur concepts (rendehR package, generated from CSV)")
    dur_results <- run_atomic_concepts_unified(
      con                 = con,
      concept_config_path = paths$concept_config_csv_dur,
      src_name            = src_name,
      schema_names        = schema_names,
      typ                 = "dur",
      sql_out_dir         = paths$pkg_dur_sql_dir,
      user_concepts       = user_concepts_dur,
      concept_name        = concept_name,
      dry_run             = dry_run,
      engine              = engine,
      rm_duplicates       = rm_duplicates
    )
    if (length(dur_results) > 0L) layer_results$dur <- dur_results
    if (!isTRUE(dry_run))
      auto_register_atomic_provenance(con, src_name, schema_names$concepts,
                                      paths$concept_config_csv_dur,
                                      concept_names = names(dur_results)[dur_results])
  }

  # -- repo/ - pre-stripped mimic-code / eicu-code SQL, kept but NOT run ------
  # repo/ (eicu_code/ or mimic_code/) holds the untouched MIT-LCP vendor
  # originals, retained on disk purely as an upstream reference/diff target -
  # never executed by run_concepts(). Fixes and adaptations belong in ehr/
  # (layer 4 below), which is what actually runs. repo/ is not executed
  # because writing its prefix-stripped SQL into ehr/ would overwrite the
  # hand-maintained files there: ehr/ is the single, hand-maintained source
  # of truth and is never auto-overwritten.

  if ("ehr" %in% selected_layers) {
    # -- Layer 4: ehr/ - rendehR-modified repo concepts -------------------------
    # These concepts call PL/pgSQL helper functions from postgres-functions.sql
    # (REGEXP_EXTRACT, DATETIME_ADD, ...), which DuckDB cannot execute at all -
    # skip the whole layer rather than let it fail concept-by-concept.
    if (engine != "postgres") {
      message("\n-- Layer 4: ehr/ concepts skipped (requires PL/pgSQL helper ",
              "functions from postgres-functions.sql; engine = '", engine, "')")
    } else if (nzchar(paths$pkg_ehr_sql_dir) && dir.exists(paths$pkg_ehr_sql_dir) &&
        length(list.files(paths$pkg_ehr_sql_dir, pattern = "\\.sql$",
                          recursive = TRUE)) > 0L) {
      message("\n-- Layer 4: rendehR-modified (ehr/) concepts")
      before_l4 <- .snapshot_tables(con, schema_names$derived)
      ehr_results <- run_external_concepts(con, paths, schema_names, src_name,
                            sql_dir       = paths$pkg_ehr_sql_dir,
                            skip_on_error = TRUE,
                            execute       = !isTRUE(dry_run))
      if (length(ehr_results) > 0L) layer_results$ehr <- ehr_results
      if (!isTRUE(dry_run))
        .register_new_tables(con, src_name, before_l4, schema_names$derived,
                             if (src_family(src_name) == "eicu") "ehr_eicu" else "ehr")
    }
  }

  # -- Layer 5: user/ - user's own SQL ----------------------------------------
  if ("user" %in% selected_layers && dir.exists(paths$user_sql_dir) &&
      length(list.files(paths$user_sql_dir, pattern = "\\.sql$",
                        recursive = TRUE)) > 0L) {
    target_schema <- if (!is.null(user_schema)) {
      message("\n-- Layer 5: user concepts -> schema '", user_schema, "'")
      user_schema
    } else {
      message("\n-- Layer 5: user concepts -> ", schema_names$concepts,
              " (default)\n",
              "   Tip: pass user_schema = 'my_schema' to run_concepts() to",
              " use a custom schema.")
      schema_names$concepts
    }
    user_results <- .run_user_sql_dir(con, paths$user_sql_dir, schema_names$raw,
                                      target_schema, concept_name = concept_name,
                                      dry_run = dry_run)
    if (length(user_results) > 0L) layer_results$user <- user_results
    if (!isTRUE(dry_run))
      .register_user_layer_provenance(con, src_name, paths$user_sql_dir,
                                      target_schema,
                                      succeeded = names(user_results)[user_results])
  }

  # -- Layer 6: external/{module}/ - third-party registered SQL ---------------
  # Each module gets its own schema: {src_name}_coh_ext_{folder_name}.
  # The user can override with external_schema (applies to single-module dirs).
  ext_dir <- external_sql_dir %||% paths$external_sql_dir
  if ("external" %in% selected_layers && !is.null(ext_dir) && dir.exists(ext_dir)) {
    ext_subdirs <- list.dirs(ext_dir, recursive = FALSE, full.names = TRUE)
    modules     <- if (length(ext_subdirs) > 0L) ext_subdirs else list(ext_dir)
    external_results <- logical(0)
    for (d in modules) {
      module_schema <- if (length(modules) == 1L && !is.null(external_schema)) {
        external_schema
      } else {
        paste0(src_name, "_coh_ext_", basename(d))
      }
      message("\n-- Layer 6: external concepts (", basename(d),
              ") -> schema: ", module_schema)
      before_l6 <- .snapshot_tables(con, module_schema)
      module_result <- run_external_concepts(con, paths, schema_names, src_name,
                            sql_dir       = d,
                            save_adapted  = FALSE,
                            skip_on_error = TRUE,
                            target_schema = module_schema,
                            execute       = !isTRUE(dry_run))
      if (length(module_result) > 0L) {
        names(module_result) <- paste0(basename(d), "/", names(module_result))
        external_results <- c(external_results, module_result)
      }
      if (!isTRUE(dry_run))
        .register_new_tables(con, src_name, before_l6, module_schema, basename(d))
    }
    if (length(external_results) > 0L)
      layer_results$external <- external_results
  }

  if (isTRUE(dry_run)) {
    message("\nDry run done: ", src_name,
            " (nothing executed, no provenance recorded)")
    return(invisible(layer_results))
  }

  .ehr_check_concept_layer_results(layer_results, src_name)

  message("\nConcepts done: ", src_name)
  invisible(layer_results)
}

# Aggregates run_concepts()'s per-layer named-logical-vector results
# and raises a single clear error naming every failed item,
# grouped by layer, if any layer reported at least one failure. Silent on
# full success. Kept as a separate function so it is independently testable
# without needing a live database connection. Defined after run_concepts()
# (rather than immediately before it) so this plain-comment-only internal
# function never sits between run_concepts()'s roxygen block and its
# definition -- roxygen2 associates a doc block with the next statement
# textually, and an intervening definition here would misattribute
# run_concepts()'s documentation to this helper instead.
.ehr_check_concept_layer_results <- function(layer_results, src_name) {
  failed <- unlist(lapply(names(layer_results), function(layer) {
    r <- layer_results[[layer]]
    nm <- names(r)[!r]
    if (length(nm) == 0L) return(NULL)
    setNames(nm, rep(layer, length(nm)))
  }))

  if (length(failed) > 0L) {
    detail <- paste0(names(failed), "/", failed, collapse = ", ")
    stop("run_concepts(): ", length(failed),
         " concept(s)/file(s) failed to build for source '", src_name,
         "': ", detail, call. = FALSE)
  }

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Top-level: run all stages for one source
# ---------------------------------------------------------------------------

# Pipeline stage ordering used by run_one_source().
.ehr_pipeline_stage_names <- c(
  "cache_raw_parquet",
  "import_cohort",
  "run_db",
  "run_raw_mods",
  "build_window_tables",
  "build_cohort_annotations",
  "build_cohort_info",
  "run_comorbidities",
  "run_iss",
  "run_concepts",
  "run_external_concepts",
  "run_harmonized_tables"
)

.ehr_default_pipeline_stages <- function(run_cohort_annotations,
                                         run_comorbidities,
                                         run_iss,
                                         run_external) {
  c(
    "import_cohort",
    "run_db",
    "run_raw_mods",
    "build_window_tables",
    if (isTRUE(run_cohort_annotations)) "build_cohort_annotations" else "build_cohort_info",
    if (isTRUE(run_comorbidities)) "run_comorbidities",
    if (isTRUE(run_iss)) "run_iss",
    "run_concepts",
    if (isTRUE(run_external)) "run_external_concepts",
    "run_harmonized_tables"
  )
}

.ehr_resolve_pipeline_stages <- function(stages = NULL,
                                         default_stages = NULL) {
  valid <- .ehr_pipeline_stage_names

  default_stages <- default_stages %||% valid

  if (is.null(stages)) return(default_stages)

  if (!is.character(stages) || length(stages) == 0L || any(!nzchar(stages))) {
    stop("stages must be a non-empty character vector of pipeline stage names.",
         call. = FALSE)
  }

  old_window_stages <- intersect(stages, c("build_visit_windows", "build_icu_windows"))
  if (length(old_window_stages) > 0L) {
    warning("Pipeline stage '", old_window_stages[[1]],
            "' is a compatibility alias; use 'build_window_tables' instead.",
            call. = FALSE)
    stages[stages %in% c("build_visit_windows", "build_icu_windows")] <- "build_window_tables"
  }

  unknown <- setdiff(stages, valid)
  if (length(unknown) > 0L) {
    stop("Unknown pipeline stage(s): ", paste(unknown, collapse = ", "),
         ". Valid stages are: ", paste(valid, collapse = ", "),
         call. = FALSE)
  }

  requested <- unique(stages)
  valid[valid %in% requested]
}

.ehr_print_stage_summary <- function(stage_log) {
  if (nrow(stage_log) == 0L) return(invisible(NULL))

  message("\nStage summary:")
  for (i in seq_len(nrow(stage_log))) {
    msg <- if (nzchar(stage_log$message[[i]])) {
      paste0(" - ", stage_log$message[[i]])
    } else {
      ""
    }
    message(sprintf("  %-26s %-7s %6.1fs%s",
                    stage_log$stage[[i]],
                    toupper(stage_log$status[[i]]),
                    stage_log$seconds[[i]],
                    msg))
  }
  invisible(NULL)
}

.ehr_has_general_window_spine <- function(src_name, db_name = NULL, base_dir = NULL) {
  tryCatch({
    paths <- ehr_paths(src_name = src_name, base_dir = base_dir)
    db_name <- .ehr_resolve_db_name(db_name)
    schema_names <- src_schema_names(paths$src_name)
    creds <- load_credentials(paths$credentials_json)

    con <- connect_postgres(db_name, creds)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    needed <- c("cohort_meta", "admission_windows", "icu_windows", "visit_windows")
    raw_schema <- .ehr_validate_db_identifier(schema_names$raw, "raw schema")
    loaded <- DBI::dbGetQuery(con, sprintf(
      "SELECT table_name FROM information_schema.tables WHERE table_schema = %s",
      DBI::dbQuoteString(con, raw_schema)
    ))$table_name
    if (!all(needed %in% loaded)) return(FALSE)

    all(vapply(c("admission_windows", "icu_windows", "visit_windows"), function(tbl) {
      "cohort_index" %in% DBI::dbListFields(
        con, DBI::Id(schema = schema_names$raw, table = tbl)
      )
    }, logical(1)))
  }, error = function(e) FALSE)
}

#' Run an EHR processing pipeline for one source
#'
#' Import a cohort, load its tables into PostgreSQL, extract concepts, and build
#' harmonized output tables. Select pipeline stages with `stages` and configure
#' their behavior with the option groups below.
#'
#' @param src_name Character string identifying a registered source, for example
#'   `"mimic_iv_demo"` or `"eicu"`. If `NULL`, use the active project configuration,
#'   falling back to `"mimic_iv_demo"`.
#' @param db_name Character string naming the PostgreSQL database. If `NULL`,
#'   use the active project configuration, falling back to `"rendehr"`.
#' @param base_dir Character string giving the project directory. If `NULL`,
#'   use the active project configuration, falling back to the working directory.
#' @param engine Character string, `"postgres"` (default) or `"duckdb"`. With
#'   `"duckdb"` the source is loaded into its DuckDB file (`ehr_paths()$duckdb_path`)
#'   and raw modifications, cohort_info and concept extraction run there; the
#'   PostgreSQL-only stages (cohort annotations, comorbidities, ISS, external
#'   concepts, harmonization) are skipped and PostgreSQL is not used. Cohort
#'   import uses DuckDB with either engine.
#' @param raw_data_root Character string giving the parent directory of the raw
#'   source files. Files for a source are read from `raw_data_root/src_name`.
#'   If `NULL`, use the project configuration or `data/source_data` under `base_dir`.
#' @param stages Character vector of stage names, or `NULL` for the default
#'   stages in the Stages section of [run_one_source()]. Selected stages run in pipeline order, regardless of
#'   their order in this vector. Required inputs for omitted stages must exist.
#' @param ... Named arguments accepted for compatibility with the ungrouped
#'   interface. See [pipeline_compatibility] for supported stage switches and
#'   rules for combining these arguments with option groups.
#' @param cohort_selection Named list from [cohort_selection_options()] specifying
#'   cohort membership, identifier keys, and the source tables to import.
#' @param windows Named list from [window_options()] specifying a custom SQL file
#'   or function for constructing observation windows during cohort import.
#' @param loading Named list from [loading_options()] specifying import resources,
#'   CSV parsing, cache use, and PostgreSQL loading options.
#' @param enrichment Named list from [enrichment_options()] specifying cohort
#'   classification, analysis level, comorbidity maps, and injury-score settings.
#' @param concepts Named list from [concept_options()] specifying concept layers,
#'   concept names, supplemental definitions, and user or external SQL settings.
#' @param harmonization Named list from [harmonization_options()] specifying which
#'   harmonized tables to build and their names.
#' @param execution Named list from [execution_options()] specifying overwrite,
#'   progress reporting, and continuation after a stage fails.
#'
#' @details
#' Source files must be available locally. Stages that use PostgreSQL require
#' connection credentials configured with [load_credentials()]. Project paths and
#' source configuration are resolved with [ehr_paths()].
#'
#' Option groups may also be ordinary named lists. Unspecified options use the
#' defaults of the corresponding stage. Supplying options does not select a stage.
#' For example, `concepts = concept_options(layers = "flat")` limits concept
#' extraction to flat concepts; use `stages = "run_concepts"` to run only that stage.
#'
#' @section Stages:
#' The table gives the execution order and defaults when `stages = NULL` and no
#' compatibility stage switches are supplied. The `scid` cohort profile additionally
#' enables comorbidity and injury-severity scoring.
#' \tabular{lll}{
#'   Stage \tab Operation \tab Default \cr
#'   \code{cache_raw_parquet} \tab Cache raw tables as Parquet \tab No \cr
#'   \code{import_cohort} \tab Select and materialize a cohort \tab Yes \cr
#'   \code{run_db} \tab Load cohort tables into PostgreSQL \tab Yes \cr
#'   \code{run_raw_mods} \tab Apply required source transformations \tab Yes \cr
#'   \code{build_window_tables} \tab Create observation windows when needed \tab Yes \cr
#'   \code{build_cohort_annotations} \tab Build cohort annotations \tab No \cr
#'   \code{build_cohort_info} \tab Build cohort information \tab Yes \cr
#'   \code{run_comorbidities} \tab Compute comorbidity scores \tab SCID profile \cr
#'   \code{run_iss} \tab Compute injury-severity scores \tab SCID profile \cr
#'   \code{run_concepts} \tab Extract source-specific concepts \tab Yes \cr
#'   \code{run_external_concepts} \tab Run external SQL separately \tab No \cr
#'   \code{run_harmonized_tables} \tab Build harmonized output tables \tab Yes
#' }
#'
#' @section Output and errors:
#' Stages create or update project files and database tables. The default is to
#' stop when a stage fails. Set `execution_options(skip_on_error = TRUE)` to continue
#' to subsequent stages; failure of required raw transformations always stops the
#' pipeline. `concept_options(dry_run = TRUE)` previews concept SQL only; other
#' selected stages still execute.
#'
#' @return Invisibly, a data frame with one row per attempted stage and columns:
#'   \describe{
#'     \item{stage}{Character stage label.}
#'     \item{status}{`"success"` or `"failed"`.}
#'     \item{seconds}{Numeric elapsed time in seconds.}
#'     \item{message}{Error message, or an empty string after success.}
#'   }
#'   A call that stops with an error does not return this data frame.
#' @seealso [pipeline_options], [run_concepts()], [run_one_source_test()]
#' @examples
#' # Configure a concept-only run without opening a database connection.
#' flat_only <- concept_options(layers = "flat")
#'
#' \dontrun{
#' # These calls require local source data and project/database configuration.
#' run_one_source("mimic_iv_demo", base_dir = "project")
#'
#' # Rebuild only flat concepts for an existing cohort.
#' run_one_source(
#'   "mimic_iv_demo",
#'   base_dir = "project",
#'   stages = "run_concepts",
#'   concepts = flat_only
#' )
#' }
#' @export
run_one_source <- function(src_name = NULL, db_name = NULL, base_dir = NULL,
                           engine = c("postgres", "duckdb"),
                           raw_data_root = NULL, stages = NULL, ...,
                           cohort_selection = cohort_selection_options(),
                           windows = window_options(),
                           loading = loading_options(),
                           enrichment = enrichment_options(),
                           concepts = concept_options(),
                           harmonization = harmonization_options(),
                           execution = execution_options()) {
  engine <- match.arg(engine)
  # Resolve legacy named arguments and grouped options before any project/DB work.
  resolved <- .ehr_resolve_pipeline_options(
    list(cohort_selection = cohort_selection, windows = windows,
         loading = loading, enrichment = enrichment, concepts = concepts,
         harmonization = harmonization, execution = execution), list(...))
  .ehr_select_concept_layers(resolved$layers, engine)
  explicit <- attr(resolved, "explicit")
  if (isTRUE(resolved$delete_source_csv)) {
    stop(
      "`delete_source_csv = TRUE` is retired; raw source inputs are immutable ",
      "and are never deleted by `run_one_source()`.",
      call. = FALSE
    )
  }
  resolved$analysis_level <- match.arg(resolved$analysis_level, c("admission", "icu"))
  paths <- ehr_paths(src_name = src_name, base_dir = base_dir,
                     raw_data_root = raw_data_root)
  src_name <- paths$src_name
  base_dir <- paths$base_dir
  ehr_init_project(src_name = src_name, base_dir = base_dir,
                   raw_data_root = raw_data_root,
                   create_config = FALSE,
                   create_credentials = FALSE)
  db_name <- .ehr_resolve_db_name(db_name)
  # General cohort import writes a cohort_index spine, so the default cohort-info
  # stage should use build_cohort_info_general(). Callers can still opt into the
  # older annotation stage explicitly with run_cohort_annotations = TRUE.
  # run_cohort_annotations <- run_cohort_annotations %||%
  #   identical(paths$cohort_profile, "scid")
  resolved$run_cohort_annotations <- resolved$run_cohort_annotations %||% FALSE
  # Comorbidities and ISS are both mandatory only for the scid cohort profile;
  # every other profile is opt-in (pass run_comorbidities/run_iss = TRUE/FALSE
  # explicitly to override either way).
  resolved$run_comorbidities <- resolved$run_comorbidities %||% identical(paths$cohort_profile, "scid")
  resolved$run_iss <- resolved$run_iss %||% identical(paths$cohort_profile, "scid")

  default_stages <- .ehr_default_pipeline_stages(
    run_cohort_annotations = resolved$run_cohort_annotations,
    run_comorbidities      = resolved$run_comorbidities,
    run_iss                = resolved$run_iss,
    run_external           = resolved$run_external
  )
  selected_stages <- .ehr_resolve_pipeline_stages(
    stages         = stages,
    default_stages = default_stages
  )
  # engine = "duckdb" runs on the source's DuckDB file only: loading, raw
  # modifications, cohort_info and concepts use their DuckDB implementations,
  # and the PostgreSQL-only stages are left out, so the run never writes to
  # PostgreSQL (before, it loaded PostgreSQL and then looked for concepts'
  # inputs in a DuckDB file nothing had built).
  duck <- identical(engine, "duckdb")
  if (duck) {
    pg_only <- intersect(selected_stages, .ehr_postgres_only_stages)
    if (length(pg_only)) {
      message("engine = 'duckdb': skipping PostgreSQL-only stage(s): ", paste(pg_only, collapse = ", "))
      selected_stages <- setdiff(selected_stages, pg_only)
    }
  }

  message("\n", strrep("=", 70))
  message("SOURCE: ", src_name, "  (", src_family(src_name), ")")
  message("COHORT PROFILE: ", paths$cohort_profile,
          if (isTRUE(paths$cohort_profile_is_default))
            " (default - cohort.profile not set in config)" else "")
  message("STAGES: ", paste(selected_stages, collapse = " -> "))
  message(strrep("=", 70), "\n")

  stage_log <- data.frame(
    stage   = character(),
    status  = character(),
    seconds = numeric(),
    message = character(),
    stringsAsFactors = FALSE
  )

  .record_stage <- function(label, status, seconds, message = "") {
    stage_log <<- rbind(
      stage_log,
      data.frame(stage = label, status = status, seconds = seconds,
                 message = message, stringsAsFactors = FALSE)
    )
    invisible(NULL)
  }

  .step <- function(label, expr) {
    message("-- ", label, " ...")
    start <- Sys.time()
    tryCatch(
      {
        force(expr)
        seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
        .record_stage(label, "success", seconds)
        invisible(TRUE)
      },
      error = function(e) {
        seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
        msg <- conditionMessage(e)
        .record_stage(label, "failed", seconds, msg)
        if (resolved$skip_on_error) {
          message("  FAILED (continuing): ", msg)
          invisible(FALSE)
        } else {
          .ehr_print_stage_summary(stage_log)
          stop("Stage '", label, "' failed: ", msg, call. = FALSE)
        }
      }
    )
  }

  for (stage in selected_stages) {
    stage_ok <- switch(stage,
      cache_raw_parquet = .step("cache_raw_parquet_duckdb",
        # Opt-in only - never part of the default stage list. With `tables`/
        # `cohort` left NULL, caches every configured raw table as typed
        # Parquet (all_tables = TRUE) so the cohort-build stage that follows
        # reads Parquet instead of repeatedly re-scanning/re-decompressing
        # raw CSV.gz on every chunk - the single biggest measured cost on a
        # full-scale source without a cache (one mid-sized table alone took
        # 12+ minutes uncached on real full-scale mimic_iv). Pass `tables`
        # or `cohort` to cache only the table(s) a scoped cohort build
        # actually needs, instead of every configured table. keep = TRUE
        # persists the cache across sessions, not just for the remainder of
        # this run.
        .ehr_call_pipeline_stage(cache_raw_parquet_duckdb,
          list(src_name = src_name, base_dir = base_dir, raw_data_root = raw_data_root, all_tables = TRUE, keep = TRUE), explicit)),
      import_cohort = .step("import_cohort_general",
        # import_cohort(src_name, base_dir, icd_filter_path,
        #               direct_ids_path   = direct_ids_path,
        #               no_filter         = no_filter,
        #               delete_source_csv = delete_source_csv,
        #               raw_data_dir      = raw_data_dir,
        #               overwrite         = overwrite)
        .ehr_call_pipeline_stage(import_cohort_general,
          list(src_name = src_name, base_dir = base_dir, raw_data_root = raw_data_root), explicit)),
      run_db = .step("run_db",
        if (duck) run_db(src_name, base_dir = base_dir, engine = "duckdb")
        else .ehr_call_pipeline_stage(run_db,
          list(src_name = src_name, db_name = db_name, base_dir = base_dir), explicit)),
      run_raw_mods = .step("run_raw_mods",
        if (duck) run_raw_mods_duckdb(src_name, base_dir = base_dir, skip_on_error = resolved$skip_on_error)
        else .ehr_call_pipeline_stage(run_raw_mods,
          list(src_name = src_name, db_name = db_name, base_dir = base_dir), explicit)),
      build_window_tables = .step("build_window_tables", {
        if (duck) {
          message("engine = 'duckdb': window tables are loaded from the cohort parquet.")
        } else if (.ehr_has_general_window_spine(src_name, db_name, base_dir)) {
          message("Generalized window tables already loaded with cohort_index; ",
                  "skipping build_window_tables().")
        } else {
          build_window_tables(src_name, db_name, base_dir)
        }
      }),
      build_cohort_annotations = .step("build_cohort_annotations",
        .ehr_call_pipeline_stage(build_cohort_annotations,
          list(src_name = src_name, db_name = db_name, base_dir = base_dir), explicit)),
      build_cohort_info = .step("build_cohort_info_general",
        # build_cohort_info(src_name, db_name, base_dir, icd_filter_path,
        #                   analysis_level = analysis_level)
        if (duck) build_cohort_info_general_duckdb(src_name, base_dir = base_dir,
                                                   analysis_level = resolved$analysis_level)
        else .ehr_call_pipeline_stage(build_cohort_info_general,
          list(src_name = src_name, db_name = db_name, base_dir = base_dir), explicit)),
      run_comorbidities = .step("run_comorbidities",
        .ehr_call_pipeline_stage(get("run_comorbidities", mode = "function"),
          list(src_name = src_name, db_name = db_name, base_dir = base_dir), explicit)),
      run_iss = .step("run_iss",
        .ehr_call_pipeline_stage(get("run_iss", mode = "function"),
          list(src_name = src_name, db_name = db_name, base_dir = base_dir), explicit)),
      run_concepts = .step("run_concepts",
        .ehr_call_pipeline_stage(run_concepts,
          list(src_name = src_name, db_name = db_name, base_dir = base_dir, engine = engine), explicit)),
      run_external_concepts = .step("run_external_concepts", {
        paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
        schema_names <- src_schema_names(src_name)
        creds        <- load_credentials(paths$credentials_json)
        con          <- connect_postgres(db_name, creds)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        run_external_concepts(con, paths, schema_names, src_name)
      }),
      run_harmonized_tables = .step("run_harmonized_tables",
        .ehr_call_pipeline_stage(run_harmonized_tables,
          list(src_name = src_name, db_name = db_name, base_dir = base_dir), explicit))
    )
    if (identical(stage, "run_raw_mods") && !isTRUE(stage_ok)) {
      .ehr_print_stage_summary(stage_log)
      stop(
        "Required raw modifications are incomplete; downstream stages refused.",
        call. = FALSE
      )
    }
  }

  .ehr_print_stage_summary(stage_log)
  message("\nDone: ", src_name, "\n")
  invisible(stage_log)
}


.ehr_table_exists <- function(con, schema, table) {
  DBI::dbExistsTable(con, DBI::Id(schema = schema, table = table))
}

.ehr_table_count <- function(con, schema, table) {
  target_id <- DBI::dbQuoteIdentifier(con, DBI::Id(
    schema = .ehr_validate_db_identifier(schema, "schema name"),
    table  = .ehr_validate_db_identifier(table, "table name")
  ))
  q <- sprintf("SELECT COUNT(*) AS n FROM %s", target_id)
  as.numeric(DBI::dbGetQuery(con, q)$n[[1]])
}

.ehr_table_has_columns <- function(con, schema, table, columns) {
  if (!.ehr_table_exists(con, schema, table)) return(FALSE)
  all(columns %in% DBI::dbListFields(con, DBI::Id(schema = schema, table = table)))
}

.ehr_valid_table <- function(con, schema, table, columns = character(), allow_empty = FALSE) {
  if (!.ehr_table_exists(con, schema, table)) {
    return(list(ok = FALSE, reason = paste0(schema, ".", table, " does not exist")))
  }
  if (length(columns) > 0L && !.ehr_table_has_columns(con, schema, table, columns)) {
    missing <- setdiff(columns, DBI::dbListFields(con, DBI::Id(schema = schema, table = table)))
    return(list(ok = FALSE, reason = paste0(schema, ".", table,
                                            " missing columns: ",
                                            paste(missing, collapse = ", "))))
  }
  n <- .ehr_table_count(con, schema, table)
  if (!allow_empty && n == 0) {
    return(list(ok = FALSE, reason = paste0(schema, ".", table, " has zero rows")))
  }
  list(ok = TRUE, reason = paste0(schema, ".", table, " valid (", n, " rows)"))
}

.ehr_stage_log_table <- function(con, schema) {
  target_id <- DBI::dbQuoteIdentifier(con, DBI::Id(
    schema = .ehr_validate_db_identifier(schema, "schema name"),
    table  = "pipeline_log"
  ))
  DBI::dbExecute(con, sprintf(
    'CREATE TABLE IF NOT EXISTS %s (
       stage TEXT NOT NULL,
       status TEXT NOT NULL,
       src_name TEXT NOT NULL,
       db_name TEXT NOT NULL,
       base_dir TEXT,
       package_version TEXT,
       started_at TIMESTAMP,
       finished_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
       message TEXT
     )', target_id), immediate = TRUE)
  invisible(TRUE)
}

.ehr_write_stage_log <- function(con, schema, stage, status, src_name, db_name,
                                 base_dir, started_at = Sys.time(), message = "") {
  .ehr_stage_log_table(con, schema)
  pkg_version <- tryCatch(as.character(utils::packageVersion("rendehR")),
                          error = function(e) NA_character_)
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = schema, table = "pipeline_log"),
    data.frame(
      stage = stage,
      status = status,
      src_name = src_name,
      db_name = db_name,
      base_dir = base_dir,
      package_version = pkg_version,
      started_at = as.POSIXct(started_at),
      finished_at = as.POSIXct(Sys.time()),
      message = message,
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )
  invisible(TRUE)
}

.ehr_stage_has_success_log <- function(con, schema, stage, src_name, db_name) {
  if (!.ehr_table_exists(con, schema, "pipeline_log")) return(FALSE)
  q <- sprintf(
    "SELECT 1 FROM \"%s\".\"pipeline_log\"
     WHERE stage = %s AND src_name = %s AND db_name = %s AND status = 'success'
     ORDER BY finished_at DESC LIMIT 1",
    schema,
    DBI::dbQuoteString(con, stage),
    DBI::dbQuoteString(con, src_name),
    DBI::dbQuoteString(con, db_name)
  )
  nrow(DBI::dbGetQuery(con, q)) > 0L
}

.ehr_parquet_files_valid <- function(paths) {
  if (!dir.exists(paths$cohort_parquet_dir)) {
    return(list(ok = FALSE, reason = paste0("Missing parquet directory: ",
                                            paths$cohort_parquet_dir)))
  }
  files <- list.files(paths$cohort_parquet_dir, pattern = "\\.parquet$",
                      full.names = TRUE)
  if (length(files) == 0L) {
    return(list(ok = FALSE, reason = paste0("No parquet files in ",
                                            paths$cohort_parquet_dir)))
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    return(list(ok = FALSE, reason = "arrow is not installed; cannot validate parquet files"))
  }
  bad <- character(0)
  for (f in files) {
    ok <- tryCatch({
      ds <- arrow::open_dataset(f, format = "parquet")
      length(names(ds)) > 0L
    }, error = function(e) FALSE)
    if (!ok) bad <- c(bad, basename(f))
  }
  if (length(bad) > 0L) {
    return(list(ok = FALSE, reason = paste0("Unreadable parquet files: ",
                                            paste(bad, collapse = ", "))))
  }
  list(ok = TRUE, reason = paste0(length(files), " readable parquet files"))
}

# Stages that need PostgreSQL; engine = "duckdb" leaves them out.
.ehr_postgres_only_stages <- c("build_cohort_annotations", "run_comorbidities", "run_iss",
                               "run_external_concepts", "run_harmonized_tables")

.ehr_with_pipeline_con <- function(db_name, paths, fn) {
  creds <- load_credentials(paths$credentials_json)
  con <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fn(con)
}

.ehr_with_stage_con <- function(stage, engine, db_name, paths, fn) {
  # engine = "duckdb" runs every database stage in the source's DuckDB file
  if (!identical(engine, "duckdb"))
    return(.ehr_with_pipeline_con(db_name, paths, fn))
  con <- connect_duckdb(paths$duckdb_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fn(con)
}

.ehr_validate_stage <- function(stage, src_name, db_name, paths,
                                require_log = TRUE, engine = "postgres") {
  schema_names <- src_schema_names(src_name)

  if (identical(stage, "cache_raw_parquet")) {
    # The cache builder owns source-fingerprint validation and reuses fresh files.
    if (require_log)
      return(list(ok = FALSE, reason = "Check raw-cache source fingerprints"))
    cache_paths <- paths
    cache_paths$cohort_parquet_dir <- paths$raw_parquet_cache_dir
    return(.ehr_parquet_files_valid(cache_paths))
  }

  if (identical(stage, "import_cohort")) {
    check <- .ehr_parquet_files_valid(paths)
    if (!isTRUE(check$ok)) return(check)
    meta_file <- file.path(paths$cohort_parquet_dir, "cohort_meta.parquet")
    stale <- if (file.exists(meta_file)) .ehr_stale_selection_reason(
      tryCatch(as.data.frame(arrow::read_parquet(meta_file)),
               error = function(e) NULL))
    if (!is.null(stale)) return(list(ok = FALSE, reason = stale))
    return(check)
  }

  tryCatch(
    .ehr_with_stage_con(stage, engine, db_name, paths, function(con) {
      if (require_log &&
          !.ehr_stage_has_success_log(con, schema_names$raw, stage, src_name, db_name)) {
        return(list(ok = FALSE, reason = paste0("No successful pipeline_log record for ", stage)))
      }

      # A raw schema loaded from a superseded eICU ICD selection must not let
      # run_db or the cohort-info stages be skipped.
      stale <- if (stage %in% c("run_db", "build_cohort_annotations",
                                "build_cohort_info") &&
                   .ehr_table_exists(con, schema_names$raw, "cohort_meta"))
        .ehr_stale_selection_reason(tryCatch(
          get_table_from_db(con, schema_names$raw, "cohort_meta"),
          error = function(e) NULL))
      if (!is.null(stale)) return(list(ok = FALSE, reason = stale))

      switch(stage,
        run_db = {
          parquet_files <- list.files(paths$cohort_parquet_dir,
                                      pattern = "\\.parquet$", full.names = FALSE)
          expected <- tools::file_path_sans_ext(parquet_files)
          if (length(expected) == 0L) return(list(ok = FALSE, reason = "No parquet files to match raw tables"))
          missing <- expected[!vapply(expected, function(tbl) .ehr_table_exists(con, schema_names$raw, tbl), logical(1))]
          if (length(missing) > 0L) return(list(ok = FALSE, reason = paste0("Missing raw tables: ", paste(missing, collapse = ", "))))
          empty <- expected[vapply(expected, function(tbl) .ehr_table_count(con, schema_names$raw, tbl) == 0, logical(1))]
          if (length(empty) > 0L) {
            return(list(ok = TRUE, reason = paste0(
              length(expected), " raw tables exist; empty tables: ",
              paste(empty, collapse = ", ")
            )))
          }
          list(ok = TRUE, reason = paste0(length(expected), " raw tables valid"))
        },
        run_raw_mods = {
          sql_dir <- paths$raw_mods_sql_dir
          if (!nzchar(sql_dir) || !dir.exists(sql_dir)) return(list(ok = TRUE, reason = "No raw_mods SQL directory"))
          files <- list.files(sql_dir, pattern = "\\.sql$", full.names = FALSE)
          if (length(files) == 0L) return(list(ok = TRUE, reason = "raw_mods SQL directory is empty"))
          expected <- tools::file_path_sans_ext(files)
          missing <- expected[!vapply(expected, function(tbl) .ehr_table_exists(con, schema_names$raw, tbl), logical(1))]
          if (length(missing) > 0L) return(list(ok = FALSE, reason = paste0("Missing raw mod tables: ", paste(missing, collapse = ", "))))
          list(ok = TRUE, reason = paste0(length(expected), " raw mod tables exist"))
        },
        build_window_tables = .ehr_valid_table(
          con, schema_names$raw, "visit_windows",
          c(cohort_key(src_name), "admittime", "dischtime", "icu_intime", "icu_outtime", "icu_los_min")
        ),
        build_cohort_annotations = {
          needed <- c("d_cohort_icd", "cohort_info")
          missing <- needed[!vapply(needed, function(tbl) .ehr_table_exists(con, schema_names$raw, tbl), logical(1))]
          if (length(missing) > 0L) return(list(ok = FALSE, reason = paste0("Missing annotation tables: ", paste(missing, collapse = ", "))))
          .ehr_valid_table(con, schema_names$raw, "cohort_info",
                           c("ehr_id", "source_data", "cohort_index"))
        },
        # cohort_index is required so a cohort_info built before the single
        # membership flag (hos_cohort_flg) is rebuilt, not reused.
        build_cohort_info = .ehr_valid_table(
          con, schema_names$raw, "cohort_info",
          c("ehr_id", "source_data", "cohort_index")
        ),
        run_comorbidities = .ehr_valid_table(
          con, schema_names$raw, "comorbidities"
        ),
        run_iss = .ehr_valid_table(
          con, schema_names$raw, "iss", c("iss", "niss", "maxais")
        ),
        run_concepts = {
          n <- nrow(DBI::dbGetQuery(con, sprintf(
            "SELECT 1 FROM information_schema.tables WHERE table_schema = '%s' LIMIT 1",
            schema_names$concepts
          )))
          if (n == 0L) list(ok = FALSE, reason = paste0(schema_names$concepts, " has no concept tables"))
          else list(ok = TRUE, reason = "Concept schema has tables")
        },
        # Always reports not-done: external concept SQL content is
        # arbitrary and user-supplied, so presence alone cannot serve as a
        # reliable "already done" signal. Treating any observed state as
        # "done" risks a false-positive skip; this is a deliberate "always
        # rerun" policy, not an oversight.
        run_external_concepts = list(ok = FALSE, reason = "External concept outputs are user-defined; rerun requested stage"),
        run_harmonized_tables = {
          needed <- c("ts_tab", "flat_tab", "dur_tab")
          missing <- needed[!vapply(needed, function(tbl) .ehr_table_exists(con, schema_names$harmonized, tbl), logical(1))]
          if (length(missing) > 0L) return(list(ok = FALSE, reason = paste0("Missing harmonized tables: ", paste(missing, collapse = ", "))))
          bad_cols <- needed[!vapply(needed, function(tbl) .ehr_table_has_columns(con, schema_names$harmonized, tbl, cohort_key(src_name)), logical(1))]
          if (length(bad_cols) > 0L) return(list(ok = FALSE, reason = paste0("Harmonized tables missing cohort key: ", paste(bad_cols, collapse = ", "))))
          list(ok = TRUE, reason = "Harmonized tables valid")
        },
        list(ok = FALSE, reason = paste0("No validator for stage ", stage))
      )
    }),
    error = function(e) list(ok = FALSE, reason = conditionMessage(e))
  )
}


# R-source-rendering helpers for ehr_suggest_next()'s recommended-code
# output. R/setup_assistant.R's .setup_quote()/.setup_r_vec()/.setup_call()
# render the same kinds of values (scalars, vectors, function calls) for a
# different generator (.setup_script()); the two families are intentionally
# kept separate rather than sharing one implementation.
.ehr_chr_code <- function(x) {
  paste0("c(", paste(sprintf('"%s"', x), collapse = ", "), ")")
}

.ehr_call_code <- function(fn, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) == 0L) return(paste0(fn, "()"))
  lines <- paste0("  ", names(args), " = ", unlist(args, use.names = FALSE))
  paste0(fn, "(\n", paste(lines, collapse = ",\n"), "\n)")
}

.ehr_quote_code <- function(x) {
  paste0('"', x, '"')
}

.ehr_run_one_source_code <- function(src_name, db_name, base_dir, stages,
                                     include_src = TRUE,
                                     include_db = TRUE,
                                     include_base = TRUE) {
  .ehr_call_code("run_one_source", list(
    src_name = if (isTRUE(include_src)) .ehr_quote_code(src_name) else NULL,
    db_name  = if (isTRUE(include_db)) .ehr_quote_code(db_name) else NULL,
    base_dir = if (isTRUE(include_base)) .ehr_quote_code(base_dir) else NULL,
    stages   = .ehr_chr_code(stages)
  ))
}

.ehr_run_sources_code <- function(src_names, db_name, base_dir, stages,
                                  include_db = TRUE,
                                  include_base = TRUE) {
  .ehr_call_code("run_sources", list(
    src_names = .ehr_chr_code(src_names),
    db_name   = if (isTRUE(include_db)) .ehr_quote_code(db_name) else NULL,
    base_dir  = if (isTRUE(include_base)) .ehr_quote_code(base_dir) else NULL,
    stages    = .ehr_chr_code(stages)
  ))
}

.ehr_merge_sources_code <- function(src_names, db_name, base_dir, target_schema,
                                    include_db = TRUE,
                                    include_base = TRUE,
                                    include_target_schema = TRUE) {
  .ehr_call_code("merge_sources", list(
    src_names      = .ehr_chr_code(src_names),
    db_name        = if (isTRUE(include_db)) .ehr_quote_code(db_name) else NULL,
    base_dir       = if (isTRUE(include_base)) .ehr_quote_code(base_dir) else NULL,
    target_schema  = if (isTRUE(include_target_schema)) .ehr_quote_code(target_schema) else NULL
  ))
}

.ehr_origin_label <- function(from_arg) {
  if (isTRUE(from_arg)) return("argument")
  if (!is.null(.ehr_env$config)) return("config")
  "default"
}

.ehr_merge_outputs_valid <- function(src_names, db_name, base_dir, target_schema) {
  paths <- ehr_paths(src_name = src_names[[1]], base_dir = base_dir)
  tryCatch(
    .ehr_with_pipeline_con(db_name, paths, function(con) {
      needed <- c("ts", "flat", "cohort_info")
      missing <- needed[!vapply(needed, function(tbl) .ehr_table_exists(con, target_schema, tbl), logical(1))]
      if (length(missing) > 0L) {
        return(list(ok = FALSE, reason = paste0("Missing merged tables: ", paste(missing, collapse = ", "))))
      }
      list(ok = TRUE, reason = "Merged tables exist")
    }),
    error = function(e) list(ok = FALSE, reason = conditionMessage(e))
  )
}

#' Suggest the next rendehR pipeline command
#'
#' Inspects source-level outputs without running pipeline stages, then suggests
#' the next command to run. The check is conservative: if a stage output cannot
#' be validated, the stage is treated as incomplete.
#'
#' @param src_names Character vector of source names. If NULL, uses the active
#'   project config source, then "mimic_iv_demo".
#' @param db_name Postgres database name. Defaults to the active project config,
#'   then "rendehr".
#' @param base_dir Project root directory. Defaults to the active project
#'   config, then current working directory.
#' @param stages Optional character vector of stage names to check. If NULL,
#'   checks all active stages for each source.
#' @param target_schema Target schema to suggest for `merge_sources()` when all
#'   sources appear complete. Default "rendehr".
#' @param suggest_merge Logical. If TRUE and multiple sources appear complete,
#'   suggest `merge_sources()` when merged outputs are missing. Default TRUE.
#' @param require_log Logical. Passed to the stage validator. Default FALSE so
#'   existing outputs created before pipeline logging can still be recognized.
#' @param run_external Logical. If TRUE, include external SQL concepts in stage
#'   checks and recommendations.
#' @inheritParams run_one_source
#' @return An object of class `ehr_suggestion` with `status`,
#'   `recommended_code`, and `merge_status` fields.
#' @examples
#' \dontrun{
#' ehr_suggest_next(src_names = "mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
#' @param run_cohort_annotations Logical. If TRUE, run
#'   build_cohort_annotations() to write the ICD classification lookup and
#'   cohort enrichment table. Default NULL resolves to FALSE because the
#'   generalized cohort path uses build_cohort_info_general().
#' @param run_comorbidities Logical, or NULL (default). If NULL, mandatory only
#'   for the \code{"scid"} cohort profile (\code{identical(paths$cohort_profile,
#'   "scid")}) - opt-in for every other profile, matching \code{run_iss}. Pass
#'   TRUE/FALSE explicitly to override either way, regardless of profile. When
#'   run, computes Charlson and Elixhauser comorbidity indices and writes to
#'   \code{{raw_schema}.comorbidities}. Requires the \code{comorbidity} package.
#'   Skipped gracefully when \code{skip_on_error = TRUE} and the package is not
#'   installed.
#' @param run_iss Logical, or NULL (default). If NULL, mandatory only for the
#'   \code{"scid"} cohort profile (\code{identical(paths$cohort_profile,
#'   "scid")}) - opt-in for every other profile. Pass TRUE/FALSE explicitly to
#'   override either way, regardless of profile. When run, computes the
#'   Injury Severity Score (ISS/NISS via \code{icdpicr}) and writes to
#'   \code{{raw_schema}.iss}. Requires the \code{icdpicr} package. Skipped
#'   gracefully when \code{skip_on_error = TRUE} and the package is not
#'   installed.
ehr_suggest_next <- function(src_names = NULL, db_name = NULL, base_dir = NULL,
                             stages = NULL,
                             target_schema = "rendehr",
                             suggest_merge = TRUE,
                             require_log = FALSE,
                             run_external = FALSE,
                             run_cohort_annotations = NULL,
                             run_comorbidities = NULL,
                             run_iss = NULL,
                             raw_data_root = NULL) {
  src_from_arg <- !is.null(src_names)
  db_from_arg <- !is.null(db_name)
  base_from_arg <- !is.null(base_dir)
  target_schema_from_arg <- !missing(target_schema)

  if (is.null(src_names)) src_names <- ehr_paths(base_dir = base_dir)$src_name
  src_names <- as.character(src_names)
  if (length(src_names) == 0L || any(!nzchar(src_names))) {
    stop("src_names must contain at least one source name.", call. = FALSE)
  }

  base_paths <- ehr_paths(src_name = src_names[[1]], base_dir = base_dir,
                          raw_data_root = raw_data_root)
  base_dir <- base_paths$base_dir
  db_name <- .ehr_resolve_db_name(db_name)
  origins <- list(
    src_names = .ehr_origin_label(src_from_arg),
    db_name = .ehr_origin_label(db_from_arg),
    base_dir = .ehr_origin_label(base_from_arg)
  )
  include_db_in_code <- isTRUE(db_from_arg)
  include_base_in_code <- isTRUE(base_from_arg)

  status <- data.frame(
    src_name = character(), stage = character(), ok = logical(),
    reason = character(), stringsAsFactors = FALSE
  )
  recommendations <- character(0)
  per_source_missing <- list()

  for (src in src_names) {
    paths <- ehr_paths(src_name = src, base_dir = base_dir,
                       raw_data_root = raw_data_root)
    # Match run_one_source(): the generalized cohort path runs build_cohort_info,
    # which now performs SCI enrichment itself, so the legacy annotation stage is
    # opt-in for every profile. Defaulting this to TRUE for scid would recommend
    # and validate a stage the pipeline does not run.
    profile_annotations <- run_cohort_annotations %||% FALSE
    # Comorbidities and ISS are both mandatory only for the scid cohort
    # profile; every other profile is opt-in (pass run_comorbidities/run_iss =
    # TRUE/FALSE explicitly to override either way).
    profile_comorbidities <- run_comorbidities %||% identical(paths$cohort_profile, "scid")
    profile_iss <- run_iss %||% identical(paths$cohort_profile, "scid")
    default_stages <- .ehr_default_pipeline_stages(
      run_cohort_annotations = profile_annotations,
      run_comorbidities      = profile_comorbidities,
      run_iss                = profile_iss,
      run_external           = run_external
    )
    selected_stages <- .ehr_resolve_pipeline_stages(stages, default_stages)

    checks <- lapply(selected_stages, function(stage) {
      check <- .ehr_validate_stage(stage, src, db_name, paths,
                                   require_log = require_log)
      data.frame(src_name = src, stage = stage, ok = isTRUE(check$ok),
                 reason = check$reason %||% "", stringsAsFactors = FALSE)
    })
    src_status <- do.call(rbind, checks)
    status <- rbind(status, src_status)

    missing <- src_status$stage[!src_status$ok]
    if (length(missing) > 0L) per_source_missing[[src]] <- missing
  }

  # Sources with an identical missing-stage set are recommended together via
  # one run_sources(...) call; a source whose missing
  # stages differ from every other incomplete source keeps its own
  # run_one_source(...) recommendation, since run_sources() forwards one
  # shared `stages` argument to every source it loops over.
  if (length(per_source_missing) > 0L) {
    missing_keys <- vapply(per_source_missing, paste, character(1), collapse = "|")
    for (key in unique(missing_keys)) {
      group_srcs <- names(per_source_missing)[missing_keys == key]
      group_missing <- per_source_missing[[group_srcs[[1]]]]
      if (length(group_srcs) > 1L) {
        recommendations <- c(recommendations,
          .ehr_run_sources_code(
            group_srcs, db_name, base_dir, group_missing,
            include_db = include_db_in_code,
            include_base = include_base_in_code
          )
        )
      } else {
        recommendations <- c(recommendations,
          .ehr_run_one_source_code(
            group_srcs[[1]], db_name, base_dir, group_missing,
            include_src = isTRUE(src_from_arg) || length(src_names) > 1L,
            include_db = include_db_in_code,
            include_base = include_base_in_code
          )
        )
      }
    }
  }

  merge_status <- NULL
  if (isTRUE(suggest_merge) && length(src_names) > 1L) {
    incomplete_sources <- unique(status$src_name[!status$ok])
    if (length(incomplete_sources) == 0L) {
      merge_status <- .ehr_merge_outputs_valid(src_names, db_name, base_dir, target_schema)
      if (!isTRUE(merge_status$ok)) {
        recommendations <- c(recommendations,
          .ehr_merge_sources_code(
            src_names, db_name, base_dir, target_schema,
            include_db = include_db_in_code,
            include_base = include_base_in_code,
            include_target_schema = isTRUE(target_schema_from_arg)
          )
        )
      }
    }
  }

  if (length(recommendations) == 0L) {
    recommendations <- "# No next command suggested; checked outputs appear complete."
  }

  out <- list(
    status = status,
    recommended_code = recommendations,
    merge_status = merge_status,
    db_name = db_name,
    base_dir = base_dir,
    src_names = src_names,
    origins = origins
  )
  class(out) <- "ehr_suggestion"
  print(out)
  invisible(out)
}

#' Print a suggested next pipeline step
#'
#' Summarizes the target database/base directory/sources (and how each was
#' resolved), the per-source, per-stage status (`ok`/`missing` with a
#' reason), merge status when applicable, and the recommended R code to run
#' next.
#'
#' @param x An `ehr_suggestion` object, as returned by `ehr_suggest_next()`.
#' @param ... Ignored.
#' @return Invisibly, `x`.
#' @export
print.ehr_suggestion <- function(x, ...) {
  origin <- x$origins %||% list(src_names = "unknown", db_name = "unknown",
                                base_dir = "unknown")
  cat("rendehR suggested next step\n")
  cat("Database: ", x$db_name, " (", origin$db_name, ")\n", sep = "")
  cat("Base dir: ", x$base_dir, " (", origin$base_dir, ")\n", sep = "")
  cat("Sources: ", paste(x$src_names, collapse = ", "),
      " (", origin$src_names, ")\n\n", sep = "")

  for (src in unique(x$status$src_name)) {
    cat("Source:", src, "\n")
    rows <- x$status[x$status$src_name == src, , drop = FALSE]
    for (i in seq_len(nrow(rows))) {
      mark <- if (isTRUE(rows$ok[[i]])) "[ok]" else "[missing]"
      cat(" ", mark, rows$stage[[i]], "-", rows$reason[[i]], "\n")
    }
    cat("\n")
  }

  if (!is.null(x$merge_status)) {
    cat("Merge:", x$merge_status$reason, "\n\n")
  }

  cat("Recommended code:\n")
  for (cmd in x$recommended_code) {
    cat(cmd, "\n\n", sep = "")
  }
  invisible(x)
}

#' Run a source pipeline with validation-aware resume checks
#'
#' Run the stages of [run_one_source()], skipping stages whose outputs already
#' pass validation. Uses the same grouped options and named compatibility
#' arguments as [run_one_source()]. One default differs on purpose: for the
#' `scid` cohort profile this runner builds cohort_info with
#' [build_cohort_annotations()] instead of [build_cohort_info_general()].
#'
#' @inheritParams run_one_source
#' @param require_log Logical. Require a previous successful stage log as well as
#'   valid database outputs before skipping a stage. Default: `TRUE`.
#' @details The historical resume defaults are retained: cohort annotations,
#'   comorbidities, and injury scores are enabled for the `scid` profile unless
#'   overridden. Use `stages` to select work explicitly.
#'
#'   `execution_options(overwrite = TRUE)` reruns stages even when their outputs
#'   validate. Concept validation and logs use DuckDB when `engine = "duckdb"`;
#'   other database stages use PostgreSQL. Validation checks existing outputs;
#'   it does not detect every change to SQL or concept options. Use overwrite
#'   when rebuilding with changed definitions or a different layer selection.
#' @return Invisibly, a data frame with columns `stage`, `status`, `seconds`, and
#'   `message`. Status is `"success"`, `"failed"`, or `"skipped"`.
#' @seealso [run_one_source()], [pipeline_options]
#' @examples
#' concept_options(layers = "flat")
#' \dontrun{
#' run_one_source_test("mimic_iii", db_name = "scidb",
#'   stages = "run_concepts", concepts = concept_options(layers = "flat"))
#' }
#' @export
run_one_source_test <- function(src_name = NULL, db_name = NULL, base_dir = NULL,
                                engine = c("postgres", "duckdb"),
                                raw_data_root = NULL, stages = NULL, ...,
                                cohort_selection = cohort_selection_options(),
                                windows = window_options(), loading = loading_options(),
                                enrichment = enrichment_options(), concepts = concept_options(),
                                harmonization = harmonization_options(),
                                execution = execution_options(), require_log = TRUE) {
  engine <- match.arg(engine)
  resolved <- .ehr_resolve_pipeline_options(
    list(cohort_selection = cohort_selection, windows = windows, loading = loading,
         enrichment = enrichment, concepts = concepts, harmonization = harmonization,
         execution = execution), list(...))
  .ehr_select_concept_layers(resolved$layers, engine)
  if (isTRUE(resolved$delete_source_csv))
    stop("`delete_source_csv = TRUE` is retired.", call. = FALSE)
  # Read orchestration settings from `resolved` rather than injecting every
  # option name into this frame, where they would shadow local variables.
  forwarded <- attr(resolved, "explicit")

  paths <- ehr_paths(src_name = src_name, base_dir = base_dir,
                     raw_data_root = raw_data_root)
  src_name <- paths$src_name
  base_dir <- paths$base_dir
  db_name <- .ehr_resolve_db_name(db_name)
  # Unlike run_one_source(), this runner keeps the legacy annotation stage for
  # the scid profile instead of forcing FALSE; the two defaults differ on purpose.
  resolved$run_cohort_annotations <- resolved$run_cohort_annotations %||%
    identical(paths$cohort_profile, "scid")
  # Comorbidities and ISS are both mandatory only for the scid cohort profile;
  # every other profile is opt-in (pass run_comorbidities/run_iss = TRUE/FALSE
  # explicitly to override either way).
  resolved$run_comorbidities <- resolved$run_comorbidities %||% identical(paths$cohort_profile, "scid")
  resolved$run_iss <- resolved$run_iss %||% identical(paths$cohort_profile, "scid")

  default_stages <- .ehr_default_pipeline_stages(
    run_cohort_annotations = resolved$run_cohort_annotations,
    run_comorbidities      = resolved$run_comorbidities,
    run_iss                = resolved$run_iss,
    run_external           = resolved$run_external
  )
  selected_stages <- .ehr_resolve_pipeline_stages(stages, default_stages)
  if (identical(engine, "duckdb")) {
    pg_only <- intersect(selected_stages, .ehr_postgres_only_stages)
    if (length(pg_only)) {
      message("engine = 'duckdb': skipping PostgreSQL-only stage(s): ", paste(pg_only, collapse = ", "))
      selected_stages <- setdiff(selected_stages, pg_only)
    }
  }

  message("\n", strrep("=", 70))
  message("SOURCE: ", src_name, "  (", src_family(src_name), ")")
  message("COHORT PROFILE: ", paths$cohort_profile,
          if (isTRUE(paths$cohort_profile_is_default))
            " (default - cohort.profile not set in config)" else "")
  message("STAGES: ", paste(selected_stages, collapse = " -> "))
  message("OVERWRITE: ", isTRUE(resolved$overwrite))
  message(strrep("=", 70), "\n")

  stage_log <- data.frame(
    stage = character(), status = character(), seconds = numeric(),
    message = character(), stringsAsFactors = FALSE
  )

  .add_log <- function(stage, status, seconds, message = "") {
    stage_log <<- rbind(stage_log, data.frame(
      stage = stage, status = status, seconds = seconds, message = message,
      stringsAsFactors = FALSE
    ))
  }

  for (stage in selected_stages) {
    check <- if (isTRUE(resolved$overwrite) || identical(stage, "cache_raw_parquet")) {
      list(ok = FALSE, reason = "overwrite = TRUE")
    } else {
      .ehr_validate_stage(stage, src_name, db_name, paths,
                          require_log = require_log, engine = engine)
    }

    if (isTRUE(check$ok)) {
      message("-- ", stage, " ... skipped; ", check$reason)
      .add_log(stage, "skipped", 0, check$reason)
      next
    }

    message("-- ", stage, " ... running; ", check$reason)
    started <- Sys.time()
    tryCatch({
      run_args <- list(src_name = src_name, db_name = db_name,
                       base_dir = base_dir, engine = engine,
                       raw_data_root = raw_data_root, stages = stage)
      # Single-bracket indexing so an explicitly supplied NULL survives.
      run_args[names(forwarded)] <- forwarded
      do.call(run_one_source, run_args)
      post <- .ehr_validate_stage(stage, src_name, db_name, paths,
                                  require_log = FALSE, engine = engine)
      if (!isTRUE(post$ok)) stop(post$reason, call. = FALSE)
      elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      .add_log(stage, "success", elapsed, post$reason)
      if (!stage %in% c("import_cohort", "cache_raw_parquet")) {
        .ehr_with_stage_con(stage, engine, db_name, paths, function(con) {
          .ehr_write_stage_log(con, src_schema_names(src_name)$raw, stage,
                               "success", src_name, db_name, base_dir,
                               started_at = started, message = post$reason)
        })
      }
      TRUE
    }, error = function(e) {
      elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      msg <- conditionMessage(e)
      .add_log(stage, "failed", elapsed, msg)
      if (!stage %in% c("import_cohort", "cache_raw_parquet")) {
        try(.ehr_with_stage_con(stage, engine, db_name, paths, function(con) {
          .ehr_write_stage_log(con, src_schema_names(src_name)$raw, stage,
                               "failed", src_name, db_name, base_dir,
                               started_at = started, message = msg)
        }), silent = TRUE)
      }
      if (resolved$skip_on_error) {
        message("  FAILED (continuing): ", msg)
        FALSE
      } else {
        .ehr_print_stage_summary(stage_log)
        stop("Stage '", stage, "' failed: ", msg, call. = FALSE)
      }
    })
  }

  .ehr_print_stage_summary(stage_log)
  message("\nDone: ", src_name, "\n")
  invisible(stage_log)
}

#' Run all pipeline stages for multiple sources
#'
#' Loops over `run_one_source()` for each source name. Arguments in `...` are
#' passed unchanged to every source.
#'
#' @param src_names Character vector of source names. If NULL, uses the active
#'   project config source, then "mimic_iv_demo".
#' @param ... Arguments passed to `run_one_source()`.
#' @return Invisibly returns a named logical vector indicating completed sources.
#' @examples
#' \dontrun{
#' run_sources(c("mimic_iv_demo", "eicu_demo"))
#' }
#' @export
run_sources <- function(src_names = NULL, ...) {
  if (is.null(src_names))
    src_names <- ehr_paths()$src_name
  src_names <- as.character(src_names)
  if (length(src_names) == 0L || any(!nzchar(src_names)))
    stop("src_names must contain at least one source name.", call. = FALSE)

  completed <- setNames(rep(FALSE, length(src_names)), src_names)
  for (i in seq_along(src_names)) {
    src_name <- src_names[[i]]
    message("\n", strrep("#", 70))
    message("RUNNING SOURCE ", i, " of ", length(src_names), ": ", src_name)
    message(strrep("#", 70), "\n")
    run_one_source(src_name = src_name, ...)
    completed[[i]] <- TRUE
  }
  invisible(completed)
}

# ---------------------------------------------------------------------------
# SCI reference example
# ---------------------------------------------------------------------------

#' Run the SCI reference pipeline and verify output
#'
#' Runs the full SCI pipeline on mimic_iv_demo using the bundled SCI ICD files
#' as the reference implementation. Useful for verifying the package works
#' correctly after installation or changes.
#'
#' Requires the MIMIC-IV demo raw data under the project's raw data root. The
#' database is the project config's `db_name` (or `db_name`), not the target
#' of [ehr_connect()].
#'
#' @param db_name Optional Postgres database to write to. If NULL, uses the
#'   active project config, then "rendehr".
#' @param base_dir Project root with MIMIC-IV demo parquet data.
#' @param src_name Source to run. Default "mimic_iv_demo".
#' @param raw_data_root Optional parent storage-root override for raw source
#'   data. The source directory is derived as
#'   `{raw_data_root}/{src_name}`.
#' @param overwrite Passed to `run_one_source()`. Default FALSE (resume/skip
#'   mode). Set TRUE to force a full rebuild of the scid example.
#' @param import_scope Passed to `run_one_source()` (and on to
#'   `import_cohort_general()`). Default NULL: full patient history, every
#'   hospitalization a matched patient ever had. Pass `"hospital"` to widen
#'   only to other ICU stays within the same hospitalization, or `"icu"` to
#'   narrow to just the matched ICU stays themselves. Changing this always
#'   requires a full rebuild (`overwrite = TRUE`).
#' @return Invisibly returns a list with cohort_info, heart_rate, and flat_tab.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("scid_example", creds)
#' run_scid_example(base_dir = "/path/to/project")
#' }
run_scid_example <- function(db_name = NULL, base_dir = NULL, src_name = "mimic_iv_demo",
                            raw_data_root = NULL, overwrite = FALSE, import_scope = NULL) {
  db_name <- .ehr_resolve_db_name(db_name)
  message(strrep("=", 60))
  message("rendehR - SCI Reference Example")
  message("Source : ", src_name)
  message("DB     : ", db_name)
  message(strrep("=", 60), "\n")

  run_one_source(
    src_name               = src_name,
    db_name                = db_name,
    base_dir               = base_dir,
    icd_filter_path        = ehr_config_path("scid_icds_rendehr.rds"),
    run_cohort_annotations = TRUE,
    raw_data_root          = raw_data_root,
    overwrite              = overwrite,
    import_scope           = import_scope
  )

  message("\nSCI example complete.")
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Sepsis-3 cohort helper and reference example
# ---------------------------------------------------------------------------

# Per-family cohort_sql/key for the packaged Sepsis-3 SQL chains
# (inst/sql/{family}/cohorts/sepsis3/). Each chain's final `sepsis3` table
# has a different column set, matching that family's native ID levels - see
# each folder's own README for the underlying clinical definition,
# provenance, and per-family adaptations.
#
# `bundle` selects which cohort_sql to run against that same `sepsis3` table
# (and the rest of the setup_sql chain, unchanged):
#   - "sepsis3": every ICU stay that met Sepsis-3 criteria (the original
#     behavior). The suspicion-of-infection event that triggers this can fall
#     anywhere in the ICU stay - this answers "was this patient ever septic
#     during their ICU course", a broader question than most published
#     admission-time sepsis-prevalence studies ask.
#   - "first_sepsis3": same, further restricted to only a patient's
#     absolute first-ever ICU stay (septic or not) - the sepsis3-mimic
#     (Johnson et al. 2018) study-cohort restriction, reproduced here as a
#     ROW_NUMBER() over each family's own ICU-stay/admission table (already
#     loaded by setup_sql) rather than by editing the packaged SQL chain.
#     For eICU, cross-hospitalization order is only approximate
#     (`hospitaldischargeyear` is year-granularity; `unitvisitnumber` only
#     orders stays within the same `patienthealthsystemstayid`), so ties
#     within the same discharge year are broken by `patienthealthsystemstayid`:
#     reproducible, though not necessarily chronological.
#   - "sepsis3_admission": same as "sepsis3", further restricted to only
#     stays whose qualifying suspicion-of-infection event falls within +/-1
#     day of ICU/unit admission - the sepsis3-mimic suspicion-timing
#     exclusion (`exclusion_early_suspicion`/`exclusion_late_suspicion` in
#     `sepsis_suspicion/04_sepsis3_cohort.sql`), which the packaged `sepsis3`
#     chain does not apply on its own. Measured effect is modest on its own
#     (mimic_iii_demo: 95 -> 88); "first_sepsis3" (95 -> 66) is the bigger
#     lever of the two.
#   - "first_sepsis3_admission": both restrictions together (mimic_iii_demo:
#     61) - the closest packaged match to the sepsis3-mimic study
#     population, but still well above that study's own suspected-infection
#     count (`sepsis_suspicion`: 28, pre-SOFA) because it doesn't reproduce
#     that study's separate adult/MetaVision/CSURG exclusions - those are
#     specific to that one MIMIC-III-era study, not part of the Sepsis-3
#     clinical definition shared with MIMIC-IV/eICU, and live in
#     `sepsis_suspicion`/`sepsis_suspicion_modified` instead.
.ehr_sepsis3_cohort_spec <- function(fam, bundle = "sepsis3") {
  bundle <- match.arg(bundle, c("sepsis3", "first_sepsis3",
                                "sepsis3_admission", "first_sepsis3_admission",
                                "trauma_sepsis3"))
  if (identical(bundle, "trauma_sepsis3") && !fam %in% c("mimic_iii", "mimic_iv"))
    stop("bundle = \"trauma_sepsis3\" needs blood cultures and is defined for ",
         "MIMIC-III and MIMIC-IV only.", call. = FALSE)
  # key = icustay_id/stay_id (not subject_id/hadm_id): membership and
  # cohort_index should mark exactly the ICU stays that met Sepsis-3
  # criteria, not every stay belonging to a patient who had at least one
  # septic stay. import_cohort_general() widens to the configured
  # import_scope regardless of key, so a patient's full context is still
  # reachable when wanted - this only changes what counts as "the cohort".
  spec <- switch(fam,
    mimic_iii = list(
      cohort_sql = "SELECT DISTINCT subject_id, hadm_id, icustay_id
                    FROM sepsis3 WHERE sepsis3",
      first_cohort_sql = "WITH first_stay AS (
                    SELECT icustay_id,
                           ROW_NUMBER() OVER (
                             PARTITION BY subject_id ORDER BY intime
                           ) AS rn
                    FROM icustays
                  )
                  SELECT DISTINCT s.subject_id, s.hadm_id, s.icustay_id
                  FROM sepsis3 s
                  JOIN first_stay fs ON s.icustay_id = fs.icustay_id
                  WHERE s.sepsis3 AND fs.rn = 1",
      admission_cohort_sql = "SELECT DISTINCT s.subject_id, s.hadm_id, s.icustay_id
                  FROM sepsis3 s
                  JOIN icustays ie ON s.icustay_id = ie.icustay_id
                  WHERE s.sepsis3
                    AND s.suspected_infection_time BETWEEN
                        ie.intime - INTERVAL 1 DAY AND ie.intime + INTERVAL 1 DAY",
      first_admission_cohort_sql = "WITH first_stay AS (
                    SELECT icustay_id,
                           ROW_NUMBER() OVER (
                             PARTITION BY subject_id ORDER BY intime
                           ) AS rn
                    FROM icustays
                  )
                  SELECT DISTINCT s.subject_id, s.hadm_id, s.icustay_id
                  FROM sepsis3 s
                  JOIN icustays ie ON s.icustay_id = ie.icustay_id
                  JOIN first_stay fs ON s.icustay_id = fs.icustay_id
                  WHERE s.sepsis3 AND fs.rn = 1
                    AND s.suspected_infection_time BETWEEN
                        ie.intime - INTERVAL 1 DAY AND ie.intime + INTERVAL 1 DAY",
      key        = "icustay_id"
    ),
    mimic_iv = list(
      cohort_sql = "SELECT DISTINCT subject_id, stay_id
                    FROM sepsis3 WHERE sepsis3",
      first_cohort_sql = "WITH first_stay AS (
                    SELECT stay_id,
                           ROW_NUMBER() OVER (
                             PARTITION BY subject_id ORDER BY intime
                           ) AS rn
                    FROM icustays
                  )
                  SELECT DISTINCT s.subject_id, s.stay_id
                  FROM sepsis3 s
                  JOIN first_stay fs ON s.stay_id = fs.stay_id
                  WHERE s.sepsis3 AND fs.rn = 1",
      admission_cohort_sql = "SELECT DISTINCT s.subject_id, s.stay_id
                  FROM sepsis3 s
                  JOIN icustays ie ON s.stay_id = ie.stay_id
                  WHERE s.sepsis3
                    AND s.suspected_infection_time BETWEEN
                        ie.intime - INTERVAL 1 DAY AND ie.intime + INTERVAL 1 DAY",
      first_admission_cohort_sql = "WITH first_stay AS (
                    SELECT stay_id,
                           ROW_NUMBER() OVER (
                             PARTITION BY subject_id ORDER BY intime
                           ) AS rn
                    FROM icustays
                  )
                  SELECT DISTINCT s.subject_id, s.stay_id
                  FROM sepsis3 s
                  JOIN icustays ie ON s.stay_id = ie.stay_id
                  JOIN first_stay fs ON s.stay_id = fs.stay_id
                  WHERE s.sepsis3 AND fs.rn = 1
                    AND s.suspected_infection_time BETWEEN
                        ie.intime - INTERVAL 1 DAY AND ie.intime + INTERVAL 1 DAY",
      key        = "stay_id"
    ),
    eicu = list(
      cohort_sql = "SELECT DISTINCT uniquepid, patienthealthsystemstayid,
                           patientunitstayid
                    FROM sepsis3 WHERE sepsis3",
      first_cohort_sql = "WITH first_stay AS (
                    SELECT patientunitstayid,
                           ROW_NUMBER() OVER (
                             PARTITION BY uniquepid
                             ORDER BY hospitaldischargeyear, patienthealthsystemstayid,
                                      unitvisitnumber, patientunitstayid
                           ) AS rn
                    FROM patient
                  )
                  SELECT DISTINCT s.uniquepid, s.patienthealthsystemstayid,
                         s.patientunitstayid
                  FROM sepsis3 s
                  JOIN first_stay fs ON s.patientunitstayid = fs.patientunitstayid
                  WHERE s.sepsis3 AND fs.rn = 1",
      # eICU times are minutes from unit (ICU) admission, so
      # suspected_infection_offset is already relative to admission - no join
      # needed. +/-1 day = +/-1440 minutes.
      admission_cohort_sql = "SELECT DISTINCT s.uniquepid, s.patienthealthsystemstayid,
                         s.patientunitstayid
                  FROM sepsis3 s
                  WHERE s.sepsis3
                    AND s.suspected_infection_offset BETWEEN -1440 AND 1440",
      first_admission_cohort_sql = "WITH first_stay AS (
                    SELECT patientunitstayid,
                           ROW_NUMBER() OVER (
                             PARTITION BY uniquepid
                             ORDER BY hospitaldischargeyear, patienthealthsystemstayid,
                                      unitvisitnumber, patientunitstayid
                           ) AS rn
                    FROM patient
                  )
                  SELECT DISTINCT s.uniquepid, s.patienthealthsystemstayid,
                         s.patientunitstayid
                  FROM sepsis3 s
                  JOIN first_stay fs ON s.patientunitstayid = fs.patientunitstayid
                  WHERE s.sepsis3 AND fs.rn = 1
                    AND s.suspected_infection_offset BETWEEN -1440 AND 1440",
      key        = "patientunitstayid"
    ),
    stop("No packaged Sepsis-3 cohort chain for source family '", fam,
         "'. Available: mimic_iii, mimic_iv, eicu.", call. = FALSE)
  )
  spec$cohort_sql <- switch(bundle,
    sepsis3                  = spec$cohort_sql,
    trauma_sepsis3           = spec$cohort_sql,
    first_sepsis3             = spec$first_cohort_sql,
    sepsis3_admission         = spec$admission_cohort_sql,
    first_sepsis3_admission   = spec$first_admission_cohort_sql
  )
  spec$first_cohort_sql <- NULL
  spec$admission_cohort_sql <- NULL
  spec$first_admission_cohort_sql <- NULL
  spec
}

# Resolve the packaged setup_sql file list for a source family, erroring
# clearly if the package isn't installed (system.file() needs an installed
# package, not just devtools::load_all()).
# The chain's last file builds the sepsis3 table with the default definition;
# definition = "absolute" and bundle = "trauma_sepsis3" swap it for the matching
# file under variants/.
.ehr_sepsis3_setup_sql <- function(fam, definition = "delta", bundle = "sepsis3") {
  definition <- match.arg(definition, c("delta", "absolute"))
  dir <- system.file("sql", fam, "cohorts", "sepsis3", package = "rendehR")
  setup_sql <- sort(list.files(dir, pattern = "\\.sql$", full.names = TRUE),
                    method = "radix")
  if (length(setup_sql) == 0L)
    stop("No packaged Sepsis-3 SQL chain found for '", fam, "' (system.file() ",
         "returned nothing). Is the package installed, not just loaded?",
         call. = FALSE)
  variant <- if (identical(bundle, "trauma_sepsis3")) "trauma_sepsis3.sql"
             else if (identical(definition, "absolute")) "sepsis3_absolute.sql"
  if (!is.null(variant)) {
    last <- grep("_sepsis3\\.sql$", basename(setup_sql))
    setup_sql[last] <- file.path(dir, "variants", variant)
  }
  setup_sql
}

#' Build a Sepsis-3 cohort ID list for a source, optionally importing it
#'
#' Source-aware Sepsis-3 cohort selection: resolves the right packaged
#' `inst/sql/{family}/cohorts/sepsis3/` SQL chain via `src_family(src_name)`,
#' runs it over the raw CSVs through the DuckDB-backed `cohort_sql`/
#' `setup_sql` selection route, and returns
#' the matching cohort IDs. When `import = TRUE` (the default), also feeds
#' the result straight into `import_cohort_general(direct_ids_path = ...)`
#' for you - no separate manual CSV round-trip needed.
#'
#' Works for any source family with a packaged chain (`mimic_iii`,
#' `mimic_iv`, `eicu`).
#'
#' There is no gold-standard way to find Sepsis-3 in ICU records; published
#' studies differ in the SOFA rule, the suspected-infection rule, the timing
#' window and what they count. The default (`definition = "delta"`) follows
#' the YAIB/ricu operationalisation: suspected infection on an antibiotic course
#' lasting at least 72 h (MIMIC: paired with a culture in the 72 h before or the
#' 24 h after the antibiotic; eICU, which has almost no culture data: the
#' antibiotic alone), and a SOFA rise of at least 2 points over the lowest SOFA
#' of the stay so far, within `[-48h, +24h]` of the suspicion; adults only.
#' Organ dysfunction present from ICU admission - an injury, sedation, a
#' vasopressor started on arrival - is the baseline, not sepsis.
#' `definition = "absolute"` keeps the earlier rendehR rule: any antibiotic
#' (MIMIC-III and eICU) or antibiotic-culture pair (MIMIC-IV), and an absolute
#' SOFA of at least 2 from a baseline of 0. See `vignette("sepsis-cohort-definitions")` for the
#' counts each definition gives on the full databases.
#'
#' @param src_name Source name (e.g. "mimic_iii_demo").
#' @param base_dir Project root with the source's raw demo data.
#' @param raw_data_root Optional parent storage-root override for raw source
#'   data. The source directory is derived as
#'   `{raw_data_root}/{src_name}`. Forwarded to both the cohort-selection
#'   step (`.ehr_select_cohort_ids_duckdb()`) and the import step
#'   (`import_cohort_general()`), so both agree on where the raw CSVs live.
#' @param out_csv Optional path to write the full cohort_sql result (every ID
#'   column selected, not just `key`) to CSV. If NULL and `import = TRUE`, a
#'   temporary file is used internally and removed afterward.
#' @param import If TRUE (default), import the selected cohort via
#'   `import_cohort_general(direct_ids_path = ...)`.
#' @param overwrite Passed to `import_cohort_general()` when `import = TRUE`.
#'   Default FALSE.
#' @param bundle Which cohort definition to run against the packaged Sepsis-3
#'   SQL chain (the `setup_sql` chain itself is unchanged either way). Default
#'   `"sepsis3"`: every ICU stay that met Sepsis-3 criteria, with the
#'   qualifying suspicion-of-infection event anywhere in the stay - this
#'   answers "was this patient ever septic during their ICU course", which
#'   runs much higher than admission-time sepsis-prevalence studies (e.g. on
#'   `mimic_iii_demo`, 95 ICU stays of 70 patients are captured). `"first_sepsis3"`: further restricted to only a patient's
#'   absolute first-ever ICU stay (septic or not) - the `sepsis3-mimic`
#'   (Johnson et al. 2018) study-cohort restriction (`mimic_iii_demo`: 95 ->
#'   66 stays). For eICU, cross-hospitalization ordering for
#'   `"first_sepsis3"` is only approximate (eICU has no exact
#'   cross-admission timestamp, only a year-granularity discharge year), so
#'   ties within the same discharge year are broken by
#'   `patienthealthsystemstayid`: reproducible, though not necessarily
#'   chronological.
#'   `"sepsis3_admission"`: instead restricted to stays whose qualifying
#'   suspicion-of-infection event falls within +/-1 day of ICU/unit
#'   admission - the sepsis3-mimic suspicion-timing exclusion; a smaller
#'   effect on its own (`mimic_iii_demo`: 95 -> 88). `"first_sepsis3_admission"`:
#'   both restrictions together (`mimic_iii_demo`: 61) - the closest packaged
#'   match to the sepsis3-mimic study population, but still well above that
#'   study's own suspected-infection count (`sepsis_suspicion`: 28,
#'   pre-SOFA) because it doesn't reproduce that study's separate
#'   adult/MetaVision/CSURG exclusions - those are specific to that one
#'   MIMIC-III-era study, not part of the Sepsis-3 clinical definition
#'   shared with MIMIC-IV/eICU, and live in
#'   `sepsis_suspicion`/`sepsis_suspicion_modified` instead.
#'   `"trauma_sepsis3"` (MIMIC-III and MIMIC-IV only): hospital-acquired sepsis
#'   in trauma admissions, after ML4UWHealth/SepsisOnset_TraumaCohort - a blood
#'   culture at least 72 h after admission with a new intravenous antibiotic
#'   course of at least 4 days starting within 2 days of it, and a rise of at
#'   least 2 points of a SOFA without its GCS and urine-output parts within 3
#'   days of the culture; decided per hospital admission, with every ICU stay
#'   of a septic admission selected. `definition` does not apply to it.
#' @param definition `"delta"` (default): SOFA rise of at least 2 over the
#'   stay's running minimum, antibiotic course of at least 72 h, adults only.
#'   `"absolute"`: the earlier rule, absolute SOFA of at least 2 from a
#'   baseline of 0.
#' @param import_scope Passed to `import_cohort_general()` when `import =
#'   TRUE`. Default `"hospital"`: for every Sepsis-3-positive ICU stay, also
#'   pulls in any other ICU stay within the SAME hospitalization, but not a
#'   patient's other, separate hospitalizations. Sepsis-3 is common enough in
#'   a general ICU population (58% of all `mimic_iii` patients qualify at
#'   least once) that the engine's own default of `NULL` (full patient
#'   history, every hospitalization a patient ever had) ends up including
#'   ~83% of the entire `chartevents` table - almost the whole dataset, not a
#'   focused cohort. Pass `import_scope = NULL` to restore that full
#'   patient-history behavior, or `import_scope = "icu"` to narrow further to
#'   just the exact Sepsis-3-positive stays themselves, with no sibling
#'   admissions or stays at all. Changing this always requires a full rebuild
#'   (`overwrite = TRUE`) - there's no cheap way to re-scope an
#'   already-built cohort.
#' @param memory_limit,threads Optional DuckDB resource settings forwarded to
#'   the cohort-selection step (`.ehr_select_cohort_ids_duckdb()`) - the
#'   `setup_sql` chain scans full raw tables (e.g. a full CHARTEVENTS), which
#'   can exceed DuckDB's default memory cap on a memory-constrained machine.
#'   Unlike `import_cohort_general()`'s cohort-table build, this chain runs
#'   as plain GROUP BY/window SQL (SOFA, vitalsign, ...) and cannot be split
#'   into row-hash chunks the way a straight table copy can.
#' @param show_progress Print each `setup_sql` file before
#'   execution, followed by elapsed seconds on completion or failure. Also
#'   forwarded to `import_cohort_general()` for the import half.
#' @return Invisibly returns a one-column data frame of unique `key` values
#'   (the Sepsis-3 cohort IDs).
#' @export
#'
#' @examples
#' \dontrun{
#' ids <- build_sepsis_cohort("mimic_iii_demo", base_dir = "/path/to/project")
#' # Selection only, no import:
#' ids <- build_sepsis_cohort("eicu_demo", base_dir = "/path/to/project",
#'                            import = FALSE)
#' # Full patient history instead of just the septic hospitalization:
#' ids <- build_sepsis_cohort("mimic_iii_demo", base_dir = "/path/to/project",
#'                            import_scope = NULL, overwrite = TRUE)
#' # Only each patient's first-ever ICU stay (Johnson et al. 2018 restriction):
#' ids <- build_sepsis_cohort("mimic_iii_demo", base_dir = "/path/to/project",
#'                            bundle = "first_sepsis3")
#' }
build_sepsis_cohort <- function(src_name, base_dir = NULL,
                                 raw_data_root = NULL,
                                 out_csv = NULL, import = TRUE,
                                 overwrite = FALSE,
                                 bundle = "sepsis3",
                                 definition = c("delta", "absolute"),
                                 import_scope = "hospital",
                                 memory_limit = NULL, threads = NULL,
                                 show_progress = TRUE) {
  fam       <- src_family(src_name)
  definition <- match.arg(definition)
  spec      <- .ehr_sepsis3_cohort_spec(fam, bundle)
  setup_sql <- .ehr_sepsis3_setup_sql(fam, definition, bundle)

  csv_path <- out_csv %||% if (isTRUE(import)) tempfile(fileext = ".csv") else NULL
  ids <- .ehr_select_cohort_ids_duckdb(
    src_name      = src_name,
    cohort_sql    = spec$cohort_sql,
    key           = spec$key,
    setup_sql     = setup_sql,
    base_dir      = base_dir,
    raw_data_root = raw_data_root,
    out_csv       = csv_path,
    memory_limit  = memory_limit,
    threads       = threads,
    show_progress = show_progress
  )

  if (isTRUE(import)) {
    import_cohort_general(
      src_name        = src_name,
      base_dir        = base_dir,
      raw_data_root   = raw_data_root,
      direct_ids_path = csv_path,
      key             = spec$key,
      import_scope    = import_scope,
      overwrite       = overwrite,
      memory_limit    = memory_limit,
      threads         = threads,
      show_progress   = show_progress
    )
    if (is.null(out_csv)) unlink(csv_path)
  }

  invisible(ids)
}

#' Run the Sepsis-3 reference pipeline and verify output
#'
#' Runs the full pipeline for the Sepsis-3 cohort: selects and imports it via
#' `build_sepsis_cohort()`, then runs every remaining stage of
#' `run_one_source()` (skipping only its own `import_cohort` stage, already
#' done). Works for any source family with a packaged chain (`mimic_iii`,
#' `mimic_iv`, `eicu`). Useful for verifying the package's DuckDB-backed
#' `cohort_sql`/`setup_sql` cohort-selection route works correctly after
#' installation or changes.
#'
#' Requires the source's raw demo data under the project's raw data root. The
#' database is the project config's `db_name` (or `db_name`), not the target
#' of [ehr_connect()].
#'
#' @param db_name Optional Postgres database to write to. If NULL, uses the
#'   active project config, then "rendehr".
#' @param base_dir Project root with the source's raw demo data.
#' @param src_name Source to run. Default "mimic_iii_demo".
#' @param raw_data_root Optional parent storage-root override for raw source
#'   data, forwarded to both `build_sepsis_cohort()`'s selection step and
#'   `run_one_source()`'s remaining stages.
#' @param overwrite If TRUE, overwrite existing cohort/database output for
#'   this source - forwarded to both `build_sepsis_cohort()` and
#'   `run_one_source()`'s remaining stages. Default FALSE, matching
#'   `run_one_source()`'s own default; needed for re-running the example
#'   against a source already imported/loaded.
#' @param definition Forwarded to `build_sepsis_cohort()`: `"delta"` (default)
#'   or `"absolute"`.
#' @param bundle Forwarded to `build_sepsis_cohort()`. Default `"sepsis3"` -
#'   see `build_sepsis_cohort()`'s own `bundle` doc for the `"first_sepsis3"`
#'   (first-ICU-stay-only) alternative.
#' @param import_scope Forwarded to `build_sepsis_cohort()`. Default
#'   `"hospital"` - see `build_sepsis_cohort()`'s own `import_scope` doc for
#'   why this differs from the engine's general `NULL` (full patient-history)
#'   default.
#' @param memory_limit,threads Optional DuckDB resource settings forwarded to
#'   `build_sepsis_cohort()`'s cohort-selection step - useful for a full
#'   (non-demo) source on a memory-constrained machine, where scanning a full
#'   CHARTEVENTS-sized table can exceed DuckDB's default memory cap.
#' @param show_progress Forwarded to `build_sepsis_cohort()` - prints which
#'   `setup_sql` file is running, with elapsed seconds.
#' @return Invisibly returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("sepsis3_example", creds)
#' run_sepsis3_example(base_dir = "/path/to/project")
#' run_sepsis3_example(src_name = "eicu_demo", base_dir = "/path/to/project")
#' run_sepsis3_example(base_dir = "/path/to/project", bundle = "first_sepsis3")
#' }
run_sepsis3_example <- function(db_name = NULL, base_dir = NULL,
                                src_name = "mimic_iii_demo",
                                raw_data_root = NULL,
                                overwrite = FALSE,
                                bundle = "sepsis3",
                                definition = c("delta", "absolute"),
                                import_scope = "hospital",
                                memory_limit = NULL, threads = NULL,
                                show_progress = TRUE) {
  db_name <- .ehr_resolve_db_name(db_name)
  definition <- match.arg(definition)

  message(strrep("=", 60))
  message("rendehR - Sepsis-3 Reference Example")
  message("Source : ", src_name)
  message("DB     : ", db_name)
  message(strrep("=", 60), "\n")

  build_sepsis_cohort(src_name, base_dir = base_dir,
                      raw_data_root = raw_data_root, import = TRUE,
                      overwrite = overwrite, bundle = bundle,
                      definition = definition,
                      import_scope = import_scope,
                      memory_limit = memory_limit, threads = threads,
                      show_progress = show_progress)

  remaining_stages <- setdiff(
    .ehr_default_pipeline_stages(run_cohort_annotations = FALSE,
                                 run_comorbidities = TRUE,
                                 run_iss = TRUE, run_external = FALSE),
    "import_cohort"
  )
  run_one_source(
    src_name      = src_name,
    db_name       = db_name,
    base_dir      = base_dir,
    stages        = remaining_stages,
    raw_data_root = raw_data_root,
    overwrite     = overwrite
  )

  message("\nSepsis-3 example complete.")
  invisible(NULL)
}

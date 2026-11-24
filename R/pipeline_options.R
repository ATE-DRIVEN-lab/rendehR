# Grouped pipeline options. Unspecified fields retain each runner's defaults.

#' Specify cohort selection options
#'
#' Configure cohort membership and the scope of imported source data.
#'
#' @param icd_filter_path Character string giving an ICD-code CSV or RDS file used
#'   to select cohort members. If no selection method is supplied, the importer
#'   uses the cohort definition in the project configuration.
#' @param direct_ids_path Character string giving a CSV, compressed CSV, Parquet,
#'   or RDS file of cohort identifiers. Use `key` to specify the identifier column
#'   when it cannot be inferred.
#' @param cohort Character string naming a bundled cohort, such as `"scid"` or
#'   `"sepsis3"`, or `NULL`. See [list_example_cohorts()] for
#'   available definitions.
#' @param tables Character vector of source table names to import or cache. `NULL`
#'   uses the tables selected by the receiving function. Table selection does not
#'   change cohort membership.
#' @param cohort_sql Character string containing a SQL selection query or the path
#'   to a SQL file. The result must contain the identifier column specified by
#'   `key`.
#' @param setup_sql Character vector of SQL statements or SQL-file paths to
#'   execute, in order, before `cohort_sql`. Default: `NULL`.
#' @param cohort_fn Function, or character string naming a function, called as
#'   `cohort_fn(paths)`. It must return an identifier vector or a data frame
#'   containing the column specified by `key`. Default: `NULL`.
#' @param no_filter Logical. If `TRUE`, include the available source population
#'   without applying a cohort selection method. Default: `FALSE`.
#' @param key Character string naming an identifier in the configured source
#'   hierarchy, such as `"subject_id"`, `"hadm_id"`, or `"stay_id"`. `NULL` lets
#'   the selection method determine the key.
#' @param import_scope Character string selecting patient, hospital, ICU, or a
#'   configured hierarchy key as the import scope. `NULL` lets the importer
#'   resolve the scope from the selection.
#' @param allow_unmatched_ids Logical. If `TRUE`, allow some requested identifiers
#'   to be absent from the source, provided at least one matches. Default:
#'   `FALSE`.
#'
#' @details
#' Supply one selection method: `cohort`, `icd_filter_path`, `direct_ids_path`, `cohort_sql`, or `cohort_fn`. Alternatively, set `no_filter = TRUE`. The importer rejects competing selection methods.
#'
#' Only explicitly supplied arguments are included in the result, including
#' explicit `NULL` values. Omitted settings use the defaults of the receiving
#' function. An empty options list therefore leaves its defaults unchanged.
#'
#' @return A named list of the supplied options, suitable for the `cohort_selection`
#'   argument of [run_one_source()] and compatible direct stage functions.
#' @seealso [pipeline_options], [run_one_source()]
#' @examples
#' cohort_selection_options(direct_ids_path = "ids.csv", key = "subject_id")
#' cohort_selection_options(cohort = "sepsis3")
#' @export
cohort_selection_options <- function(icd_filter_path = NULL,
    direct_ids_path = NULL,
    cohort = NULL,
    tables = NULL,
    cohort_sql = NULL,
    setup_sql = NULL,
    cohort_fn = NULL,
    no_filter = FALSE,
    key = NULL,
    import_scope = NULL,
    allow_unmatched_ids = FALSE) {
  supplied <- names(match.call(expand.dots = FALSE))[-1L]
  values <- mget(as.character(supplied), envir = environment(), inherits = FALSE)
  .ehr_validate_option_group("cohort_selection", values)
  values
}

#' Specify observation-window options
#'
#' Configure the observation-window builder used during cohort import.
#'
#' @param window_sql Character string giving a SQL file that creates the source
#'   observation-window tables, including `visit_windows`. `NULL` uses the
#'   source's configured window SQL.
#' @param window_fn Function, or character string naming a function, called as
#'   `window_fn(con)` with the import connection. It must return a data frame with
#'   the columns required by the source's visit-window definition. When supplied,
#'   it takes precedence over `window_sql`.
#'
#' @details
#' The builder defines the temporal hierarchy used to associate source records with cohort members.
#'
#' Only explicitly supplied arguments are included in the result, including
#' explicit `NULL` values. Omitted settings use the defaults of the receiving
#' function. An empty options list therefore leaves its defaults unchanged.
#'
#' @return A named list of the supplied options, suitable for the `windows`
#'   argument of [run_one_source()] and compatible direct stage functions.
#' @seealso [pipeline_options], [run_one_source()]
#' @examples
#' window_options(window_sql = "windows.sql")
#' @export
window_options <- function(window_sql = NULL,
    window_fn = NULL) {
  supplied <- names(match.call(expand.dots = FALSE))[-1L]
  values <- mget(as.character(supplied), envir = environment(), inherits = FALSE)
  .ehr_validate_option_group("windows", values)
  values
}

#' Specify import and database-loading options
#'
#' Configure import resources, CSV parsing, raw-cache use, and PostgreSQL loading.
#'
#' @param memory_limit Character string specifying a DuckDB memory limit, for
#'   example `"4GB"`. `NULL` uses the receiving function's connection settings.
#' @param threads Positive integer specifying the number of DuckDB worker threads.
#'   `NULL` uses the receiving function's connection settings.
#' @param temp_directory Character string giving a directory for temporary DuckDB
#'   files. `NULL` uses the receiving function's temporary-directory settings.
#' @param table_chunk_rows Positive number used as the table-size threshold for
#'   chunked cohort materialization. Default: `5e6` rows. `NULL` requests a single
#'   build operation per table.
#' @param validate_columns Logical. Check imported values against configured
#'   column types. When omitted, cohort import uses `FALSE` and raw caching
#'   uses `TRUE`, including when called through the pipeline. An explicit value
#'   applies to both stages.
#' @param ignore_errors Logical. Allow the CSV reader to skip malformed rows.
#'   Default: `TRUE` in this package. Use `FALSE` to stop on a parsing error.
#' @param store_rejects Logical. Record rejected-row information when malformed
#'   CSV rows are skipped. Default: `TRUE`.
#' @param use_parquet_cache Logical. Use an available raw Parquet cache during
#'   cohort import. Default: `TRUE`.
#' @param delete_source_parquet Logical. Delete cohort Parquet files after they
#'   have been successfully loaded into PostgreSQL. Default: `FALSE`.
#' @param unlogged Logical. Load PostgreSQL tables as unlogged tables. Default:
#'   `FALSE`. See [run_db()] for storage behavior.
#' @param set_logged Logical. Convert unlogged PostgreSQL tables to logged tables
#'   after loading. Default: `FALSE`.
#' @param synchronous_commit_off Logical. Disable synchronous commit during
#'   PostgreSQL loading. Default: `FALSE`.
#' @param create_indexes Logical. Create the configured PostgreSQL indexes during
#'   loading. Default: `TRUE`.
#'
#' @details
#' Memory, thread, and temporary-directory settings apply to both raw caching
#' and cohort import. Parsing and chunking settings apply to cohort import;
#' PostgreSQL settings apply to database loading. Shared groups are validated
#' in full, then each stage uses only the settings it supports.
#'
#' Only explicitly supplied arguments are included in the result, including
#' explicit `NULL` values. Omitted settings use the defaults of the receiving
#' function. An empty options list therefore leaves its defaults unchanged.
#'
#' @return A named list of the supplied options, suitable for the `loading`
#'   argument of [run_one_source()] and compatible direct stage functions.
#' @seealso [pipeline_options], [run_one_source()]
#' @examples
#' loading_options(memory_limit = "4GB", threads = 2, ignore_errors = FALSE)
#' loading_options(create_indexes = FALSE)
#' @export
loading_options <- function(memory_limit = NULL,
    threads = NULL,
    temp_directory = NULL,
    table_chunk_rows = 5e6,
    validate_columns = FALSE,
    ignore_errors = TRUE,
    store_rejects = TRUE,
    use_parquet_cache = TRUE,
    delete_source_parquet = FALSE,
    unlogged = FALSE,
    set_logged = FALSE,
    synchronous_commit_off = FALSE,
    create_indexes = TRUE) {
  supplied <- names(match.call(expand.dots = FALSE))[-1L]
  values <- mget(as.character(supplied), envir = environment(), inherits = FALSE)
  .ehr_validate_option_group("loading", values)
  values
}

#' Specify cohort-enrichment options
#'
#' Configure cohort classification, comorbidity scoring, and injury-severity scoring.
#'
#' @param analysis_level Character string, `"admission"` or `"icu"`, specifying
#'   the analysis level for cohort information and annotations. Default:
#'   `"admission"`.
#' @param classify_fn Optional classification function used by
#'   [build_cohort_info_general()] or [build_cohort_annotations()]. Default:
#'   `NULL`, which for the `scid` cohort profile resolves to [scid_classify()];
#'   supplying a function here replaces that default rather than adding to it.
#' @param classify_args Named list of additional arguments supplied to
#'   `classify_fn`. Default: an empty list. Entries are merged over the
#'   profile's own defaults, so a single path can be overridden without
#'   restating the others.
#' @param maps Character vector containing `"charlson"`, `"elixhauser"`, or both.
#'   Selects the maps used by [run_comorbidities()]. Default: both.
#' @param icd_adjudication_path Character string giving a reviewed eICU
#'   ICD-version crosswalk for comorbidity and injury scoring. `NULL` uses the
#'   scoring function's default resolution.
#' @param publish Logical. Write computed comorbidity and injury scores to their
#'   database tables. Default: `TRUE`.
#'
#' @details
#' These settings configure selected enrichment stages. Use `stages` in [run_one_source()] to choose which operations to run.
#'
#' Only explicitly supplied arguments are included in the result, including
#' explicit `NULL` values. Omitted settings use the defaults of the receiving
#' function. An empty options list therefore leaves its defaults unchanged.
#'
#' @return A named list of the supplied options, suitable for the `enrichment`
#'   argument of [run_one_source()] and compatible direct stage functions.
#' @seealso [pipeline_options], [run_one_source()]
#' @examples
#' enrichment_options(analysis_level = "icu")
#' enrichment_options(maps = "charlson")
#' @export
enrichment_options <- function(analysis_level = c("admission", "icu"),
    classify_fn = NULL,
    classify_args = list(),
    maps = c("charlson", "elixhauser"),
    icd_adjudication_path = NULL,
    publish = TRUE) {
  supplied <- names(match.call(expand.dots = FALSE))[-1L]
  values <- mget(as.character(supplied), envir = environment(), inherits = FALSE)
  .ehr_validate_option_group("enrichment", values)
  values
}

#' Specify concept-extraction options
#'
#' Select concept layers and configure packaged, user-defined, and external concepts.
#'
#' @param user_concepts Character string giving a supplemental concept CSV. Rows
#'   override packaged definitions with the same source and concept name and may
#'   add new concepts. Default: `NULL`.
#' @param user_schema Character string naming the target schema for user SQL.
#'   `NULL` uses the source concepts schema.
#' @param external_sql_dir Character string giving an external SQL directory.
#'   `NULL` uses the project's configured external-concept directory.
#' @param external_schema Character string naming the target schema for a single
#'   external module. `NULL` derives the schema from the module name. Separate
#'   submodules always receive separate schemas.
#' @param concept_name Character vector of concept names or SQL filenames without
#'   the `.sql` extension. Filters the `flat`, `ts`, `dur`, and `user` layers.
#'   `NULL` includes all concepts in those layers; the `ehr` and `external` layers
#'   are not filtered.
#' @param dry_run Logical. Generate or read concept SQL without building concept
#'   tables or registering provenance. Default: `FALSE`. This setting applies to
#'   concept extraction; it does not preview other pipeline stages. See
#'   [run_concepts()] for connection and file-writing behavior.
#' @param layers Character vector selecting `"flat"`, `"ts"`, `"dur"`, `"ehr"`,
#'   `"user"`, or `"external"`. `NULL` selects all enabled layers. Layers run in
#'   this order; prerequisite tables must already exist. The `ehr` layer requires
#'   PostgreSQL.
#' @param rm_duplicates Logical. Remove chart rows that are validated copies of
#'   lab results (MIMIC) or of monitor values (eICU) during `ts`/`dur` concept
#'   extraction, and repeated chart rows
#'   of a `ts` concept. Default: `TRUE`. `FALSE`
#'   extracts exactly the same rows and removes none. See
#'   [run_atomic_concepts_unified()] for the matching rule.
#'
#' @details
#' In this package, `ehr` selects maintained SQL definitions. Upstream `repo` SQL is retained as reference material and cannot be selected for execution.
#'
#' Only explicitly supplied arguments are included in the result, including
#' explicit `NULL` values. Omitted settings use the defaults of the receiving
#' function. An empty options list therefore leaves its defaults unchanged.
#'
#' @return A named list of the supplied options, suitable for the `concepts`
#'   argument of [run_one_source()] and compatible direct stage functions.
#' @seealso [pipeline_options], [run_one_source()]
#' @examples
#' concept_options(layers = "flat")
#' concept_options(layers = c("ts", "dur"), dry_run = TRUE)
#' @export
concept_options <- function(user_concepts = NULL,
    user_schema = NULL,
    external_sql_dir = NULL,
    external_schema = NULL,
    concept_name = NULL,
    dry_run = FALSE,
    layers = NULL,
    rm_duplicates = TRUE) {
  supplied <- names(match.call(expand.dots = FALSE))[-1L]
  values <- mget(as.character(supplied), envir = environment(), inherits = FALSE)
  .ehr_validate_option_group("concepts", values)
  values
}

#' Specify harmonized-output options
#'
#' Select the harmonized output tables and their names.
#'
#' @param flat_concepts Character vector of flat concept table names to include in
#'   the harmonized flat table. `NULL` or an empty vector requests automatic
#'   discovery.
#' @param build_ts Logical. Build the harmonized time-series table. Default:
#'   `TRUE`.
#' @param build_flat Logical. Build the harmonized flat table. Default: `TRUE`.
#' @param build_dur Logical. Build the harmonized duration table. Default: `TRUE`.
#' @param ts_table_name Character string naming the harmonized time-series table.
#'   Default: `"ts_tab"`.
#' @param flat_table_name Character string naming the harmonized flat table.
#'   Default: `"flat_tab"`.
#' @param dur_table_name Character string naming the harmonized duration table.
#'   Default: `"dur_tab"`.
#'
#' @details
#' Harmonization reads existing concept tables. Its output selection is independent of the layers selected for concept extraction.
#'
#' Only explicitly supplied arguments are included in the result, including
#' explicit `NULL` values. Omitted settings use the defaults of the receiving
#' function. An empty options list therefore leaves its defaults unchanged.
#'
#' @return A named list of the supplied options, suitable for the `harmonization`
#'   argument of [run_one_source()] and compatible direct stage functions.
#' @seealso [pipeline_options], [run_one_source()]
#' @examples
#' harmonization_options(build_ts = FALSE, build_flat = TRUE, build_dur = FALSE)
#' @export
harmonization_options <- function(flat_concepts = NULL,
    build_ts = TRUE,
    build_flat = TRUE,
    build_dur = TRUE,
    ts_table_name = "ts_tab",
    flat_table_name = "flat_tab",
    dur_table_name = "dur_tab") {
  supplied <- names(match.call(expand.dots = FALSE))[-1L]
  values <- mget(as.character(supplied), envir = environment(), inherits = FALSE)
  .ehr_validate_option_group("harmonization", values)
  values
}

#' Specify pipeline execution options
#'
#' Configure rebuild behavior, import progress, and continuation after stage failures.
#'
#' @param skip_on_error Logical. Continue to subsequent pipeline stages after a
#'   failure. Default: `FALSE`. Required raw-transformation failures always stop
#'   [run_one_source()].
#' @param show_progress Logical. Display progress during cohort import. Default:
#'   `TRUE`. The optional raw-cache stage uses the same setting.
#' @param overwrite Logical. Rebuild raw caches, cohort Parquet files, and loaded
#'   cohort tables when requested. Default: `FALSE`, which allows those stages to
#'   reuse complete outputs. This setting does not control whether concept SQL or
#'   harmonization rebuilds their tables.
#'
#' @details
#' Use these options with [run_one_source()] or a direct stage that accepts the relevant settings.
#'
#' Only explicitly supplied arguments are included in the result, including
#' explicit `NULL` values. Omitted settings use the defaults of the receiving
#' function. An empty options list therefore leaves its defaults unchanged.
#'
#' @return A named list of the supplied options, suitable for the `execution`
#'   argument of [run_one_source()] and compatible direct stage functions.
#' @seealso [pipeline_options], [run_one_source()]
#' @examples
#' execution_options(show_progress = FALSE)
#' execution_options(overwrite = TRUE)
#' @export
execution_options <- function(skip_on_error = FALSE,
    show_progress = TRUE,
    overwrite = FALSE) {
  supplied <- names(match.call(expand.dots = FALSE))[-1L]
  values <- mget(as.character(supplied), envir = environment(), inherits = FALSE)
  .ehr_validate_option_group("execution", values)
  values
}

.ehr_option_fields <- list(cohort_selection = c("icd_filter_path", "direct_ids_path", "cohort", "tables", "cohort_sql", "setup_sql", "cohort_fn", "no_filter", "key", "import_scope", "allow_unmatched_ids"),
  windows = c("window_sql", "window_fn"),
  loading = c("memory_limit", "threads", "temp_directory", "table_chunk_rows", "validate_columns", "ignore_errors", "store_rejects", "use_parquet_cache", "delete_source_parquet", "unlogged", "set_logged", "synchronous_commit_off", "create_indexes"),
  enrichment = c("analysis_level", "classify_fn", "classify_args", "maps", "icd_adjudication_path", "publish"),
  concepts = c("user_concepts", "user_schema", "external_sql_dir", "external_schema", "concept_name", "dry_run", "layers", "rm_duplicates"),
  harmonization = c("flat_concepts", "build_ts", "build_flat", "build_dur", "ts_table_name", "flat_table_name", "dur_table_name"),
  execution = c("skip_on_error", "show_progress", "overwrite"))

# Primary owners supply defaults for compatibility validation and orchestration.
# Stage calls below forward only explicit overrides, retaining local defaults.
.ehr_pipeline_option_defaults <- function() {
  owners <- c("import_cohort_general", "run_db", "build_cohort_annotations",
              "build_cohort_info_general", "run_comorbidities", "run_iss",
              "run_concepts", "run_harmonized_tables", "run_raw_mods")
  fields <- unlist(.ehr_option_fields, use.names = FALSE)
  result <- list()
  for (owner in owners) {
    fn <- get(owner, mode = "function")
    defaults <- formals(fn)
    for (key in setdiff(intersect(names(defaults), fields), names(result)))
      result[key] <- list(eval(defaults[[key]], envir = environment(fn)))
  }
  # These select pipeline behavior rather than a direct stage's arguments.
  result$run_external <- FALSE
  result["run_cohort_annotations"] <- list(NULL)
  result["run_comorbidities"] <- list(NULL)
  result["run_iss"] <- list(NULL)
  result$delete_source_csv <- FALSE
  result
}

# Reusable groups are validated before projecting onto a stage's arguments.
.ehr_call_pipeline_stage <- function(fn, args, overrides) {
  accepted <- names(formals(fn))
  # Test doubles and forwarding wrappers may deliberately accept all arguments.
  selected <- if ("..." %in% accepted) overrides else
    overrides[intersect(names(overrides), accepted)]
  for (key in names(selected)) args[key] <- selected[key]
  do.call(fn, args)
}

.ehr_validate_option_group <- function(group, values) {
  if (!group %in% names(.ehr_option_fields))
    stop("Unknown option group: ", group, call. = FALSE)
  if (is.null(values)) return(invisible(NULL))
  if (!is.list(values) || (length(values) &&
      (is.null(names(values)) || anyNA(names(values)) ||
       any(!nzchar(names(values))) || anyDuplicated(names(values)))))
    stop(group, " must be a list with unique, non-empty option names.", call. = FALSE)
  unknown <- setdiff(names(values), .ehr_option_fields[[group]])
  if (length(unknown))
    stop("Unknown ", group, " option(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  flags <- c("no_filter", "allow_unmatched_ids", "validate_columns", "ignore_errors",
             "store_rejects", "use_parquet_cache", "delete_source_parquet", "unlogged",
             "set_logged", "synchronous_commit_off", "create_indexes", "publish",
             "dry_run", "rm_duplicates", "build_ts", "build_flat", "build_dur", "skip_on_error",
             "show_progress", "overwrite")
  for (key in intersect(names(values), flags)) {
    value <- values[[key]]
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop(group, "$", key, " must be TRUE or FALSE.", call. = FALSE)
  }
  if ("layers" %in% names(values)) .ehr_select_concept_layers(values$layers)
  invisible(NULL)
}

# Keep group overrides separate from defaults so explicit NULL is meaningful.
.ehr_flatten_options <- function(groups) {
  if (anyDuplicated(unlist(.ehr_option_fields, use.names = FALSE)))
    stop("Option fields must belong to exactly one group.", call. = FALSE)
  if (anyDuplicated(names(groups)))
    stop("Option groups must have unique names.", call. = FALSE)
  result <- list()
  for (group in names(groups)) {
    .ehr_validate_option_group(group, groups[[group]])
    for (key in names(groups[[group]])) result[key] <- groups[[group]][key]
  }
  result
}

.ehr_resolve_pipeline_options <- function(groups, legacy) {
  defaults <- .ehr_pipeline_option_defaults()
  if (length(legacy) && (is.null(names(legacy)) || anyNA(names(legacy)) ||
      any(!nzchar(names(legacy))) || anyDuplicated(names(legacy))))
    stop("Legacy arguments in ... must be uniquely named; use grouped options.", call. = FALSE)
  unknown <- setdiff(names(legacy), names(defaults))
  if (length(unknown))
    stop("Unknown run_one_source argument(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  overrides <- .ehr_flatten_options(groups)
  conflicts <- intersect(names(legacy), names(overrides))
  if (length(conflicts))
    stop("Supply each option either directly or in its group, not both: ",
         paste(conflicts, collapse = ", "), call. = FALSE)
  for (key in names(legacy)) defaults[key] <- legacy[key]
  for (key in names(overrides)) defaults[key] <- overrides[key]
  attr(defaults, "explicit") <- c(legacy, overrides)
  defaults
}

# Original stage APIs retain their positional arguments and also accept groups.
# Apply only fields consumed by this stage; other valid fields remain reusable.
.ehr_apply_grouped_options <- function(groups, supplied, allowed, frame) {
  overrides <- .ehr_flatten_options(groups)
  overrides <- overrides[intersect(names(overrides), allowed)]
  conflicts <- intersect(names(overrides), supplied)
  if (length(conflicts))
    stop("Supply each option either directly or in its group, not both: ",
         paste(conflicts, collapse = ", "), call. = FALSE)
  if (length(overrides)) list2env(overrides, envir = frame)
  invisible(NULL)
}

.ehr_select_concept_layers <- function(layers = NULL, engine = "postgres") {
  enabled <- c("flat", "ts", "dur", "ehr", "user", "external")
  if (is.null(layers)) return(enabled)
  if (!is.character(layers) || !length(layers) || anyNA(layers) || any(!nzchar(layers)))
    stop("layers must be NULL or a non-empty character vector of layer names.", call. = FALSE)
  unknown <- setdiff(layers, enabled)
  if (length(unknown))
    stop("Unknown or disabled concept layer(s): ", paste(unknown, collapse = ", "),
         ". Available: ", paste(enabled, collapse = ", "), call. = FALSE)
  if (engine == "duckdb" && "ehr" %in% layers)
    stop("The explicitly selected ehr layer requires PostgreSQL (PL/pgSQL).", call. = FALSE)
  enabled[enabled %in% layers]
}

#' Configure the source pipeline
#'
#' Use option constructors to configure related settings in [run_one_source()].
#' Each constructor returns an ordinary named list containing only the arguments
#' supplied in the call.
#'
#' @section Option groups:
#' \tabular{lll}{
#'   Argument \tab Constructor \tab Settings \cr
#'   \code{cohort_selection} \tab \code{\link{cohort_selection_options}} \tab Population and table selection \cr
#'   \code{windows} \tab \code{\link{window_options}} \tab Observation-window construction \cr
#'   \code{loading} \tab \code{\link{loading_options}} \tab Import and database loading \cr
#'   \code{enrichment} \tab \code{\link{enrichment_options}} \tab Classification and scoring \cr
#'   \code{concepts} \tab \code{\link{concept_options}} \tab Concept definitions and layers \cr
#'   \code{harmonization} \tab \code{\link{harmonization_options}} \tab Output tables \cr
#'   \code{execution} \tab \code{\link{execution_options}} \tab Rebuilds and error handling
#' }
#'
#' @section Defaults and validation:
#' An omitted setting uses the receiving function's default. For example,
#' `loading_options()` returns an empty list, whereas
#' `loading_options(ignore_errors = FALSE)` requests strict CSV parsing.
#' An explicitly supplied `NULL` is retained in the list. Constructor usage
#' shows reference defaults; calling a constructor without that argument does
#' not insert its reference value. Stage-specific defaults can differ, such as
#' column validation for raw caching (`TRUE`) and cohort import (`FALSE`).
#'
#' Constructors check option names and logical flags. Stage functions perform
#' additional validation when the options are used. Direct stage functions use only
#' settings applicable to that stage; other valid fields are ignored. A setting supplied both directly
#' and in a group is an error, even when the values agree.
#'
#' @section Selecting work:
#' `stages` selects pipeline operations. `concepts$layers` selects layers within
#' concept extraction. Providing an option group does not select its stage, and
#' selecting concept layers does not change the harmonization output settings.
#' For argument names accepted by earlier interfaces, see [pipeline_compatibility].
#'
#' @seealso [run_one_source()], [run_concepts()]
#' @examples
#' loading_options()
#' loading_options(ignore_errors = FALSE)
#' concept_options(layers = "flat")
#'
#' # Ordinary named lists can also be used as option groups.
#' list(layers = "flat", dry_run = TRUE)
#' @name pipeline_options
NULL

#' Compatibility arguments for the source pipeline
#'
#' [run_one_source()] accepts named arguments from the ungrouped interface through
#' `...`. New calls can use the option constructors described in [pipeline_options].
#'
#' @section Grouped and direct arguments:
#' Settings documented by an option constructor may also be supplied directly.
#' For example, `concept_name = "heart_rate"` and
#' `concepts = concept_options(concept_name = "heart_rate")` specify the same
#' setting. Supplying both forms raises an error. Unknown argument names are
#' rejected. Layer selection is also accepted directly, for example
#' `run_one_source("mimic_iii", stages = "run_concepts", layers = "flat")`.
#'
#' Name arguments after `src_name`, `db_name`, and `base_dir` when updating a
#' positional call. Option-group names must be exact; `cohort` remains a cohort
#' selection setting and is not an abbreviation for `cohort_selection`.
#'
#' @section Stage switches:
#' The following switches affect default stage selection when `stages = NULL`.
#' An explicit `stages` vector determines which stages run.
#' \describe{
#'   \item{\code{run_cohort_annotations}}{Logical or \code{NULL}. \code{TRUE}
#'     selects cohort annotations in place of cohort information. \code{NULL}
#'     resolves to \code{FALSE}.}
#'   \item{\code{run_comorbidities}, \code{run_iss}}{Logical or \code{NULL}.
#'     \code{NULL} enables these stages for the \code{scid} profile and disables
#'     them for other profiles. Explicit values override that default.}
#'   \item{\code{run_external}}{Logical, default \code{FALSE}. Enables the separate
#'     external-SQL stage. It does not control the external layer inside
#'     \code{run_concepts}; use \code{concept_options(layers = ...)} for layer
#'     selection.}
#' }
#'
#' @section Retired arguments:
#' `delete_source_csv = FALSE` is accepted. `delete_source_csv = TRUE` raises an
#' error; raw source files are not deleted by the pipeline.
#'
#' @seealso [run_one_source()], [pipeline_options]
#' @name pipeline_compatibility
NULL

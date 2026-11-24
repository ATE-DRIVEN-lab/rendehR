#' High-level pipeline API - cfg-accepting wrappers
#'
#' These functions provide the primary user-facing API. Each accepts a config
#' list returned by ehr_load_config() and delegates to the underlying stage
#' runner. The lower-level stage runners (import_cohort, run_db, run_concepts,
#' run_harmonized_tables) remain available for granular control.
#'
#' Typical workflow:
#' ```r
#' cfg <- ehr_load_config()   # needs source.src_name and database.db_name
#' download_source(cfg$source$src_name)
#' build_cohort(cfg)
#' extract_concepts(cfg)
#' harmonize_concepts(cfg)
#' ```

#' Load raw data into Postgres and build the SCI cohort
#'
#' Runs the Postgres load + helper table stages (run_db). Assumes data has
#' already been downloaded and imported to parquet via download_source().
#'
#' @param cfg Config list from ehr_load_config(). Must contain
#'   source.src_name and database.db_name. project.base_dir defaults to
#'   getwd() if omitted.
#' @param ... Additional arguments passed to run_db().
#' @return Invisibly NULL.
#' @examples
#' \dontrun{
#' cfg <- ehr_load_config("config/ehr_config.json")
#' build_cohort(cfg)
#' }
#' @export
build_cohort <- function(cfg, ...) {
  run_db(
    src_name = cfg$source$src_name,
    db_name  = cfg$database$db_name,
    base_dir = cfg$project$base_dir,
    ...
  )
}

#' Extract atomic and handcrafted concepts for the configured source
#'
#' @param cfg Config list from ehr_load_config().
#' @param user_concepts Optional path to a supplemental concept CSV. User rows
#'   override package concept definitions with the same concept_name.
#' @param ... Additional arguments passed to run_concepts().
#' @return Invisibly NULL.
#' @examples
#' \dontrun{
#' cfg <- ehr_load_config("config/ehr_config.json")
#' extract_concepts(cfg)
#' }
#' @export
extract_concepts <- function(cfg, user_concepts = NULL, ...) {
  run_concepts(
    src_name      = cfg$source$src_name,
    db_name       = cfg$database$db_name,
    base_dir      = cfg$project$base_dir,
    user_concepts = user_concepts,
    ...
  )
}

#' Build harmonized ts_tab and flat_tab output tables
#'
#' @param cfg Config list from ehr_load_config().
#' @param flat_concepts Character vector of concept names to include in the
#'   flat table. Default: c("sex", "race", "insurance", "age").
#' @param build_ts Build the time-series table. Default TRUE.
#' @param build_flat Build the flat (baseline) table. Default TRUE.
#' @param ... Additional arguments passed to run_harmonized_tables().
#' @return Invisibly NULL.
#' @examples
#' \dontrun{
#' cfg <- ehr_load_config("config/ehr_config.json")
#' harmonize_concepts(cfg, flat_concepts = c("sex", "age"))
#' }
#' @export
harmonize_concepts <- function(cfg,
                               flat_concepts = c("sex", "race", "insurance", "age"),
                               build_ts      = TRUE,
                               build_flat    = TRUE,
                               ...) {
  run_harmonized_tables(
    base_dir      = cfg$project$base_dir,
    src_name      = cfg$source$src_name,
    db_name       = cfg$database$db_name,
    flat_concepts = flat_concepts,
    build_ts      = build_ts,
    build_flat    = build_flat,
    ...
  )
}

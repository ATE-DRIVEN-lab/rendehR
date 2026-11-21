# Generate a guided rendehR setup script

`ehr_setup_assistant()` is a console-first guide. It explains the main
setup decisions, asks for choices when run interactively, and returns a
runnable R script. It does not download data, write schemas, edit
credentials, or run the pipeline itself. The generated script is the
artifact to review, save, and run block by block.

## Usage

``` r
ehr_setup_assistant(
  src_name = NULL,
  source_size = NULL,
  engine = NULL,
  db_name = NULL,
  base_dir = NULL,
  data_dir = NULL,
  config_setup = NULL,
  include_disk_check = NULL,
  cohort = NULL,
  icd_filter_path = NULL,
  pipeline = NULL,
  include_download = NULL,
  raw_data_dir = NULL,
  run_comorbidities = NULL,
  run_iss = NULL,
  analysis_level = NULL,
  make_icu_timeline = NULL,
  window_before_hours = NULL,
  window_after_hours = NULL,
  include_other_icu_events = NULL,
  include_date_only_events = NULL,
  materialize_icu = NULL,
  merge = NULL,
  target_schema = NULL,
  inspect_options = NULL,
  load_method = NULL,
  package_path = getwd(),
  save_script = NULL,
  setup_script_path = NULL,
  overwrite_setup_script = FALSE,
  interactive = base::interactive(),
  import_scope = NULL,
  sepsis_bundle = NULL,
  memory_limit = NULL,
  threads = NULL
)
```

## Arguments

- src_name:

  Optional source name or character vector of source names. When NULL,
  the assistant asks first whether to use demo or full sources, then
  lets the user choose one or more sources.

- source_size:

  Source size mode: `"demo"` or `"full"`. Demo is recommended for first
  tests because it is small and should not require PhysioNet
  credentials.

- engine:

  Database backend for the generated script: `"postgres"` (default) runs
  the complete pipeline, or `"duckdb"` for cohort selection and raw
  loading only (no Postgres server needed for that part). Compatible
  concept layers support DuckDB separately; this assistant does not
  generate them. Harmonization, comorbidities, ISS, clinical/ SCI
  annotations, and
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
  all require PostgreSQL and are omitted from the generated script when
  `engine = "duckdb"`; `pipeline` is forced to `"stages"`
  ([`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
  still uses PostgreSQL for loading and downstream stages), and
  `run_comorbidities`/`run_iss`/`merge`/`inspect_options` are forced off
  since they all require PostgreSQL.

- db_name:

  Database name stored in project config. DuckDB loading does not open a
  PostgreSQL connection.

- base_dir:

  Project root directory to place in generated code. Holds config, SQL,
  and generated files.

- data_dir:

  Directory for large data: raw PhysioNet downloads and cohort parquet
  outputs. When NULL, the assistant asks interactively and defaults to
  `file.path(base_dir, "data")`. Raw downloads go to its
  `physionet_data` subfolder and cohort parquet to its `cohorts_data`
  subfolder. Point this at a large disk to keep the heavy files off the
  project directory.

- config_setup:

  Project config handling mode: `"manual"` creates missing templates for
  the user to edit, `"interactive"` creates missing config from
  assistant answers, and `"skip"` requires existing config. Existing
  files are preserved; generated code stops on conflicting settings. The
  old value `"template"` is accepted as an alias for `"manual"`.

- include_disk_check:

  Include a read-only disk-space check snippet in the generated script?
  Mainly useful for full-data runs.

- cohort:

  Cohort mode: `"scid"`, `"sepsis3"`, `"custom"`, or `"none"`.

- icd_filter_path:

  Path to a custom ICD filter file when `cohort = "custom"`.

- pipeline:

  Pipeline style: `"stages"` emits stage-by-stage calls; `"full"` emits
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
  when possible.

- include_download:

  Include a
  [`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
  block in the script?

- raw_data_dir:

  Optional parent raw-data storage root (source directories live
  underneath it). Used consistently for downloads, config and import.

- run_comorbidities:

  Include the Charlson/Elixhauser comorbidity stage in the generated
  build script? Defaults to TRUE for SCI, FALSE otherwise.

- run_iss:

  Include the Injury Severity Score (ISS/NISS) stage in the generated
  build script? Defaults to TRUE for SCI, FALSE otherwise.

- analysis_level:

  Desired unit of analysis: `"admission"` for hospital
  admission/native-grain analysis, or `"icu"` for ICU-stay analysis.
  This matches the `analysis_level` values used across the package (e.g.
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md),
  [`get_patient_ts()`](https://ate-driven-lab.github.io/rendehR/reference/get_patient_ts.md)).
  `"hadm"` is accepted as a deprecated alias for `"admission"`.

- make_icu_timeline:

  Deprecated/compatibility shortcut. If supplied and `analysis_level` is
  NULL, TRUE maps to `analysis_level = "icu"` and FALSE maps to
  `analysis_level = "admission"`.

- window_before_hours:

  Hours before ICU admission to include in the ICU projection block.

- window_after_hours:

  Hours after ICU discharge to include in the ICU projection block.

- include_other_icu_events:

  Include ICU-native events from other ICU stays when they fall inside
  the requested ICU window?

- include_date_only_events:

  Include date-only events in the ICU projection?

- materialize_icu:

  If TRUE, generate a table for `ts_icu`; if FALSE, generate a view.

- merge:

  Merge mode: `"none"`, `"admission"`, or `"icu"`.

- target_schema:

  Target schema for the optional merge block.

- inspect_options:

  Character vector of post-build inspect/analyze snippets to include.
  Choices are `"tables"`, `"cohort"`, `"concepts"`,
  `"patient_timeline"`, `"descriptive"`, and `"none"`.

- load_method:

  How the generated script should load rendehR: `"library"`,
  `"load_all"`, or `"none"`.

- package_path:

  Package path used when `load_method = "load_all"`.

- save_script:

  Save the generated script to disk? When NULL and interactive, the
  assistant asks at the end. When non-interactive, NULL is treated as
  FALSE.

- setup_script_path:

  Path where the generated script should be saved. Defaults to
  `file.path(base_dir, "ehr_setup_script.R")` when saving.

- overwrite_setup_script:

  Overwrite an existing saved script? Default FALSE.

- interactive:

  If TRUE, print the guide and prompt for missing values. If FALSE, use
  defaults and return the generated plan silently.

- import_scope:

  Surrounding data to retain: `"patient"`, `"hospital"`, or `"icu"`.
  Choices cannot be finer than any selected source's cohort key.
  Defaults to hospital for Sepsis-3, patient otherwise. Unfiltered mode
  only accepts patient. Context encounters do not automatically become
  members.

- sepsis_bundle:

  Sepsis-3 variant: `"sepsis3"`, `"first_sepsis3"`,
  `"sepsis3_admission"`, or `"first_sepsis3_admission"`. Only for
  sepsis3.

- memory_limit:

  Optional DuckDB memory size, e.g. `"4GB"`, for sepsis
  selection/import.

- threads:

  Optional positive integer worker count for sepsis selection/import.

## Value

An object of class `ehr_setup_plan` with `choices`, `script`, and
`next_steps` fields.

## Examples

``` r
project <- file.path(tempdir(), "rendehr-project")
plan <- ehr_setup_assistant(base_dir = project, interactive = FALSE)
cat(plan$script)
#> # Generated by ehr_setup_assistant().
#> # Review this script and run it block by block in your own R session.
#> # The assistant printed this code; it did not run downloads or write to the database.
#> 
#> # Step 0 - Load rendehR --------------------------------------------------
#> # Use library(rendehR) after installation, or devtools::load_all() while developing locally.
#> library(rendehR)
#> 
#> # Step 1 - Set parameters ------------------------------------------------
#> # src_names can contain one source or several sources.
#> # Every pipeline call below passes src_name explicitly.
#> # base_dir is the project root (config, SQL, generated files); created here if missing.
#> # data_dir is for the large files (raw downloads, cohort parquet, DuckDB); point it at a large disk.
#> # raw_data_root, cohort_parquet_root, and duckdb_root are stored in the config so every stage uses them.
#> src_names <- c("mimic_iv_demo")
#> default_src <- src_names[[1]]
#> db_name <- "rendehr"
#> base_dir <- "/tmp/Rtmpew7WRg/rendehr-project"
#> data_dir <- "/tmp/Rtmpew7WRg/rendehr-project/data"
#> raw_data_root <- file.path(data_dir, 'physionet_data')
#> cohort_parquet_root <- file.path(data_dir, 'cohorts_data')
#> duckdb_root <- file.path(data_dir, 'duckdb')
#> import_scope <- "patient"
#> # Patient: retain all available encounters for selected patients; uses more storage.
#> # Imported context does not automatically become cohort membership.
#> if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
#> if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
#> 
#> # Step 1b - Project config ------------------------------------------------
#> # Only missing config/credentials files are created; existing files are never overwritten.
#> # Config setup mode: manual.
#> project_config_path <- file.path(base_dir, 'config', 'ehr_config.json')
#> credentials_config_path <- file.path(base_dir, 'config', 'config_pass.json')
#> if (!file.exists(project_config_path)) {
#>   ehr_create_config_template(
#>     path = project_config_path,
#>     src_name = default_src,
#>     db_name = db_name,
#>     base_dir = base_dir,
#>     raw_data_root = raw_data_root,
#>     cohort_parquet_root = cohort_parquet_root,
#>     duckdb_root = duckdb_root,
#>     credentials_path = "config/config_pass.json",
#>     cohort_profile = "scid",
#>     overwrite = FALSE
#>   )
#> }
#> if (!file.exists(credentials_config_path)) ehr_create_credentials_template(credentials_config_path)
#> # Existing configuration and credentials are preserved.
#> # Edit the configuration and credentials before continuing with the next block.
#> cfg <- ehr_load_config(project_config_path)
#> # Stop before downloads or database stages when saved settings conflict with this plan.
#> same_path <- function(a, b) identical(normalizePath(path.expand(a), winslash = '/', mustWork = FALSE), normalizePath(path.expand(b), winslash = '/', mustWork = FALSE))
#> resolve_config_path <- function(p) { if (is.null(p)) return(NULL); if (grepl('^/|^[A-Za-z]:', p) || startsWith(p, '~')) path.expand(p) else file.path(cfg$project$base_dir, p) }
#> if (!identical(cfg$cohort$profile, "scid")) stop('Config cohort profile conflicts with this plan; review the config or regenerate the plan.')
#> if (!identical(cfg$database$db_name, db_name)) stop('Config database conflicts with this plan.')
#> if (!identical(cfg$source$src_name, default_src)) stop('Config default source conflicts with this plan.')
#> if (!same_path(cfg$project$base_dir, base_dir)) stop('Config project directory conflicts with this plan.')
#> expected_roots <- list(raw_data_root = raw_data_root, cohort_parquet_root = cohort_parquet_root, duckdb_root = duckdb_root)
#> for (root in names(expected_roots)) {
#>   actual <- resolve_config_path(cfg$paths[[root]])
#>   if (is.null(actual) || !same_path(actual, expected_roots[[root]])) stop('Config storage path conflicts with this plan: ', root)
#> }
#> ehr_use_config(project_config_path)
#> for (src in src_names) {
#>   check <- ehr_check_config(src_name = src, quiet = FALSE)
#>   if (!isTRUE(check$ok)) stop(paste(check$problems, collapse = '; '))
#> }
#> 
#> # Step 2 - Download source data ------------------------------------------
#> # Skipped in this generated script. Run download_source(src_name) later if data are not present.
#> 
#> # Step 3 - Build selected source(s) --------------------------------------
#> # Stage-by-stage is recommended for a first run because each output can be inspected.
#> # Only one source was selected, so the generated code runs directly without a loop.
#> src <- default_src
#> # 3a. Import/subset source files to cohort parquet.
#> # overwrite = FALSE reuses compatible cohort artifacts or reports a conflict;
#> # set TRUE to force a rebuild.
#> import_cohort_general(
#>   src_name = src,
#>   base_dir = base_dir,
#>   raw_data_root = raw_data_root,
#>   import_scope = import_scope,
#>   overwrite = FALSE
#> )
#> 
#> # 3b. Load cohort parquet to Postgres raw schema.
#> run_db(
#>   src_name = src,
#>   db_name = db_name,
#>   base_dir = base_dir
#> )
#> 
#> # 3c. Apply source-specific raw table modifications (eICU views/renames; no-op for MIMIC).
#> run_raw_mods(
#>   src_name = src,
#>   db_name = db_name,
#>   base_dir = base_dir
#> )
#> 
#> # 3d. admission_windows/icu_windows/visit_windows were already loaded by
#> # run_db() above (with cohort_index from the cohort parquet). Do not call
#> # build_window_tables() here - it would rebuild them from raw admissions/
#> # icustays with no cohort filtering, overwriting the cohort-aware tables.
#> 
#> # 3e. Build cohort_info from cohort_index; SCI enrichment follows the profile.
#> build_cohort_info_general(
#>   src_name = src,
#>   db_name = db_name,
#>   base_dir = base_dir
#> )
#> 
#> # 3f. Compute Charlson and Elixhauser comorbidity indices.
#> # Requires the comorbidity package; set run_comorbidities = FALSE in ehr_setup_assistant() to omit.
#> run_comorbidities(
#>   src_name = src,
#>   db_name = db_name,
#>   base_dir = base_dir
#> )
#> 
#> # 3g. Compute Injury Severity Score (ISS/NISS).
#> # Requires icdpicr/icdpicr2; set run_iss = FALSE in ehr_setup_assistant() to omit.
#> run_iss(
#>   src_name = src,
#>   db_name = db_name,
#>   base_dir = base_dir
#> )
#> 
#> # 3h. Build source-specific concept tables.
#> run_concepts(
#>   src_name = src,
#>   db_name = db_name,
#>   base_dir = base_dir
#> )
#> 
#> # 3i. Combine concepts into harmonized ts_tab, flat_tab, and dur_tab.
#> run_harmonized_tables(
#>   src_name = src,
#>   db_name = db_name,
#>   base_dir = base_dir
#> )
#> 
#> # Step 6 - Connect for inspection ----------------------------------------
#> # Post-build inspection helpers open database queries using stored credentials.
#> # Run this after editing config/config_pass.json and after the database has been created.
#> ehr_connect(
#>   db_name = db_name
#> )
#> 
#> # Step 7 - Inspect and analyze outputs -----------------------------------
#> # These examples use default_src, the first selected source.
#> # They are meant as starting points for exploration after the build finishes.
#> 
#> # 7a. Show database sources and tables by schema.
#> list_sources()
#> tables_by_schema <- lapply(c('raw', 'concepts', 'derived', 'harmonized'), function(schema) {
#>   local({ tables <- list_tables(default_src, schema = schema); data.frame(schema = rep(schema, length(tables)), table = tables) })
#> })
#> names(tables_by_schema) <- c('raw', 'concepts', 'derived', 'harmonized')
#> tables_by_schema
#> 
#> # 7b. Summarize the cohort and inspect admission/ICU windows.
#> cohort_summary(default_src)
#> load_admission_windows(default_src, n = 10)
#> load_icu_windows(default_src, n = 10)
#> 
#> # 7c. List/search concepts and summarize one available concept.
#> concepts <- list_concepts(default_src)
#> head(concepts, 20)
#> search_concepts('lactate', src_name = default_src)
#> concept_to_check <- if (nrow(concepts) > 0) concepts$concept_name[[1]] else NA_character_
#> if (!is.na(concept_to_check)) concept_summary(default_src, concept_to_check)
#> 
#> # 7d. Inspect one patient timeline. Replace the placeholder first.
#> patient_id <- 'REPLACE_WITH_PATIENT_ID'
#> if (!identical(patient_id, 'REPLACE_WITH_PATIENT_ID')) {
#>   patient_ts <- get_patient_ts(default_src, patient_id, analysis_level = 'admission')
#>   head(patient_ts)
#>   if (requireNamespace('ggplot2', quietly = TRUE) && nrow(patient_ts) > 0) {
#>     ggplot2::ggplot(patient_ts, ggplot2::aes(time_since_hadm_min, suppressWarnings(as.numeric(value)), color = concept_name)) +
#>       ggplot2::geom_line(na.rm = TRUE) +
#>       ggplot2::labs(x = 'Minutes since hospital admission', y = 'Value', color = 'Concept') +
#>       ggplot2::theme(legend.position = 'none')
#>   }
#> } else {
#>   message('Set patient_id to a real source patient identifier before running the timeline example.')
#> }
#> 
#> # 7e. Simple descriptive summaries from flat_tab and ts_tab.
#> flat <- fetch_table(default_src, 'flat_tab', schema = 'harmonized', n = Inf)
#> if ('sex' %in% names(flat)) table(flat$sex, useNA = 'ifany')
#> if ('race' %in% names(flat)) sort(table(flat$race, useNA = 'ifany'), decreasing = TRUE)
#> age_col <- intersect(c('age', 'age_hos_admit', 'age_icu_intime'), names(flat))[1]
#> if (!is.na(age_col)) summary(suppressWarnings(as.numeric(flat[[age_col]])))
#> mort_col <- intersect(c('hospital_mortality', 'death_in_hospital', 'icu_mortality', 'mortality'), names(flat))[1]
#> if (!is.na(mort_col)) table(flat[[mort_col]], useNA = 'ifany')
#> ts_sample <- fetch_table(default_src, 'ts_tab', schema = 'harmonized', n = 100000)
#> if ('concept_name' %in% names(ts_sample)) sort(table(ts_sample$concept_name), decreasing = TRUE)
#> if (all(c('concept_name', 'value') %in% names(ts_sample))) {
#>   ts_sample$value_num <- suppressWarnings(as.numeric(ts_sample$value))
#>   aggregate(value_num ~ concept_name, data = ts_sample, FUN = function(x) c(n = sum(!is.na(x)), median = median(x, na.rm = TRUE)))
#> }
sepsis <- ehr_setup_assistant(base_dir = project, cohort = "sepsis3",
  import_scope = "hospital", sepsis_bundle = "first_sepsis3_admission",
  interactive = FALSE)
sci <- ehr_setup_assistant(base_dir = project, cohort = "scid",
  import_scope = "hospital", interactive = FALSE)
```

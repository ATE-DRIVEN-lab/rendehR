# Run an EHR processing pipeline for one source

Import a cohort, load its tables into PostgreSQL, extract concepts, and
build harmonized output tables. Select pipeline stages with `stages` and
configure their behavior with the option groups below.

## Usage

``` r
run_one_source(
  src_name = NULL,
  db_name = NULL,
  base_dir = NULL,
  engine = c("postgres", "duckdb"),
  raw_data_root = NULL,
  stages = NULL,
  ...,
  cohort_selection = cohort_selection_options(),
  windows = window_options(),
  loading = loading_options(),
  enrichment = enrichment_options(),
  concepts = concept_options(),
  harmonization = harmonization_options(),
  execution = execution_options()
)
```

## Arguments

- src_name:

  Character string identifying a registered source, for example
  `"mimic_iv_demo"` or `"eicu"`. If `NULL`, use the active project
  configuration, falling back to `"mimic_iv_demo"`.

- db_name:

  Character string naming the PostgreSQL database. If `NULL`, use the
  active project configuration, falling back to `"rendehr"`.

- base_dir:

  Character string giving the project directory. If `NULL`, use the
  active project configuration, falling back to the working directory.

- engine:

  Character string, `"postgres"` (default) or `"duckdb"`. With
  `"duckdb"` the source is loaded into its DuckDB file
  (`ehr_paths()$duckdb_path`) and raw modifications, cohort_info and
  concept extraction run there; the PostgreSQL-only stages (cohort
  annotations, comorbidities, ISS, external concepts, harmonization) are
  skipped and PostgreSQL is not used. Cohort import uses DuckDB with
  either engine.

- raw_data_root:

  Character string giving the parent directory of the raw source files.
  Files for a source are read from `raw_data_root/src_name`. If `NULL`,
  use the project configuration or `data/source_data` under `base_dir`.

- stages:

  Character vector of stage names, or `NULL` for the default stages in
  the Stages section of `run_one_source()`. Selected stages run in
  pipeline order, regardless of their order in this vector. Required
  inputs for omitted stages must exist.

- ...:

  Named arguments accepted for compatibility with the ungrouped
  interface. See
  [pipeline_compatibility](https://ate-driven-lab.github.io/rendehR/reference/pipeline_compatibility.md)
  for supported stage switches and rules for combining these arguments
  with option groups.

- cohort_selection:

  Named list from
  [`cohort_selection_options()`](https://ate-driven-lab.github.io/rendehR/reference/cohort_selection_options.md)
  specifying cohort membership, identifier keys, and the source tables
  to import.

- windows:

  Named list from
  [`window_options()`](https://ate-driven-lab.github.io/rendehR/reference/window_options.md)
  specifying a custom SQL file or function for constructing observation
  windows during cohort import.

- loading:

  Named list from
  [`loading_options()`](https://ate-driven-lab.github.io/rendehR/reference/loading_options.md)
  specifying import resources, CSV parsing, cache use, and PostgreSQL
  loading options.

- enrichment:

  Named list from
  [`enrichment_options()`](https://ate-driven-lab.github.io/rendehR/reference/enrichment_options.md)
  specifying cohort classification, analysis level, comorbidity maps,
  and injury-score settings.

- concepts:

  Named list from
  [`concept_options()`](https://ate-driven-lab.github.io/rendehR/reference/concept_options.md)
  specifying concept layers, concept names, supplemental definitions,
  and user or external SQL settings.

- harmonization:

  Named list from
  [`harmonization_options()`](https://ate-driven-lab.github.io/rendehR/reference/harmonization_options.md)
  specifying which harmonized tables to build and their names.

- execution:

  Named list from
  [`execution_options()`](https://ate-driven-lab.github.io/rendehR/reference/execution_options.md)
  specifying overwrite, progress reporting, and continuation after a
  stage fails.

## Value

Invisibly, a data frame with one row per attempted stage and columns:

- stage:

  Character stage label.

- status:

  `"success"` or `"failed"`.

- seconds:

  Numeric elapsed time in seconds.

- message:

  Error message, or an empty string after success.

A call that stops with an error does not return this data frame.

## Details

Source files must be available locally. Stages that use PostgreSQL
require connection credentials configured with
[`load_credentials()`](https://ate-driven-lab.github.io/rendehR/reference/load_credentials.md).
Project paths and source configuration are resolved with
[`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md).

Option groups may also be ordinary named lists. Unspecified options use
the defaults of the corresponding stage. Supplying options does not
select a stage. For example,
`concepts = concept_options(layers = "flat")` limits concept extraction
to flat concepts; use `stages = "run_concepts"` to run only that stage.

## Stages

The table gives the execution order and defaults when `stages = NULL`
and no compatibility stage switches are supplied. The `scid` cohort
profile additionally enables comorbidity and injury-severity scoring.

|                            |                                        |              |
|----------------------------|----------------------------------------|--------------|
| Stage                      | Operation                              | Default      |
| `cache_raw_parquet`        | Cache raw tables as Parquet            | No           |
| `import_cohort`            | Select and materialize a cohort        | Yes          |
| `run_db`                   | Load cohort tables into PostgreSQL     | Yes          |
| `run_raw_mods`             | Apply required source transformations  | Yes          |
| `build_window_tables`      | Create observation windows when needed | Yes          |
| `build_cohort_annotations` | Build cohort annotations               | No           |
| `build_cohort_info`        | Build cohort information               | Yes          |
| `run_comorbidities`        | Compute comorbidity scores             | SCID profile |
| `run_iss`                  | Compute injury-severity scores         | SCID profile |
| `run_concepts`             | Extract source-specific concepts       | Yes          |
| `run_external_concepts`    | Run external SQL separately            | No           |
| `run_harmonized_tables`    | Build harmonized output tables         | Yes          |

## Output and errors

Stages create or update project files and database tables. The default
is to stop when a stage fails. Set
`execution_options(skip_on_error = TRUE)` to continue to subsequent
stages; failure of required raw transformations always stops the
pipeline. `concept_options(dry_run = TRUE)` previews concept SQL only;
other selected stages still execute.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_concepts.md),
[`run_one_source_test()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source_test.md)

## Examples

``` r
# Configure a concept-only run without opening a database connection.
flat_only <- concept_options(layers = "flat")

if (FALSE) { # \dontrun{
# These calls require local source data and project/database configuration.
run_one_source("mimic_iv_demo", base_dir = "project")

# Rebuild only flat concepts for an existing cohort.
run_one_source(
  "mimic_iv_demo",
  base_dir = "project",
  stages = "run_concepts",
  concepts = flat_only
)
} # }
```

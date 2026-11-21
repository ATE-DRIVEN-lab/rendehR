# Run a source pipeline with validation-aware resume checks

Run the stages of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md),
skipping stages whose outputs already pass validation. Uses the same
grouped options and named compatibility arguments as
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md).
One default differs on purpose: for the `scid` cohort profile this
runner builds cohort_info with
[`build_cohort_annotations()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_annotations.md)
instead of
[`build_cohort_info_general()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_info_general.md).

## Usage

``` r
run_one_source_test(
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
  execution = execution_options(),
  require_log = TRUE
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
  the Stages section of
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md).
  Selected stages run in pipeline order, regardless of their order in
  this vector. Required inputs for omitted stages must exist.

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

- require_log:

  Logical. Require a previous successful stage log as well as valid
  database outputs before skipping a stage. Default: `TRUE`.

## Value

Invisibly, a data frame with columns `stage`, `status`, `seconds`, and
`message`. Status is `"success"`, `"failed"`, or `"skipped"`.

## Details

The historical resume defaults are retained: cohort annotations,
comorbidities, and injury scores are enabled for the `scid` profile
unless overridden. Use `stages` to select work explicitly.

`execution_options(overwrite = TRUE)` reruns stages even when their
outputs validate. Concept validation and logs use DuckDB when
`engine = "duckdb"`; other database stages use PostgreSQL. Validation
checks existing outputs; it does not detect every change to SQL or
concept options. Use overwrite when rebuilding with changed definitions
or a different layer selection.

## See also

[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md),
[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md)

## Examples

``` r
concept_options(layers = "flat")
#> $layers
#> [1] "flat"
#> 
if (FALSE) { # \dontrun{
run_one_source_test("mimic_iii", db_name = "scidb",
  stages = "run_concepts", concepts = concept_options(layers = "flat"))
} # }
```

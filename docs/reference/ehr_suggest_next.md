# Suggest the next rendehR pipeline command

Inspects source-level outputs without running pipeline stages, then
suggests the next command to run. The check is conservative: if a stage
output cannot be validated, the stage is treated as incomplete.

## Usage

``` r
ehr_suggest_next(
  src_names = NULL,
  db_name = NULL,
  base_dir = NULL,
  stages = NULL,
  target_schema = "rendehr",
  suggest_merge = TRUE,
  require_log = FALSE,
  run_external = FALSE,
  run_cohort_annotations = NULL,
  run_comorbidities = NULL,
  run_iss = NULL,
  raw_data_root = NULL
)
```

## Arguments

- src_names:

  Character vector of source names. If NULL, uses the active project
  config source, then "mimic_iv_demo".

- db_name:

  Postgres database name. Defaults to the active project config, then
  "rendehr".

- base_dir:

  Project root directory. Defaults to the active project config, then
  current working directory.

- stages:

  Optional character vector of stage names to check. If NULL, checks all
  active stages for each source.

- target_schema:

  Target schema to suggest for
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
  when all sources appear complete. Default "rendehr".

- suggest_merge:

  Logical. If TRUE and multiple sources appear complete, suggest
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
  when merged outputs are missing. Default TRUE.

- require_log:

  Logical. Passed to the stage validator. Default FALSE so existing
  outputs created before pipeline logging can still be recognized.

- run_external:

  Logical. If TRUE, include external SQL concepts in stage checks and
  recommendations.

- run_cohort_annotations:

  Logical. If TRUE, run build_cohort_annotations() to write the ICD
  classification lookup and cohort enrichment table. Default NULL
  resolves to FALSE because the generalized cohort path uses
  build_cohort_info_general().

- run_comorbidities:

  Logical, or NULL (default). If NULL, mandatory only for the `"scid"`
  cohort profile (`identical(paths$cohort_profile, "scid")`) - opt-in
  for every other profile, matching `run_iss`. Pass TRUE/FALSE
  explicitly to override either way, regardless of profile. When run,
  computes Charlson and Elixhauser comorbidity indices and writes to
  `{raw_schema}.comorbidities`. Requires the `comorbidity` package.
  Skipped gracefully when `skip_on_error = TRUE` and the package is not
  installed.

- run_iss:

  Logical, or NULL (default). If NULL, mandatory only for the `"scid"`
  cohort profile (`identical(paths$cohort_profile, "scid")`) - opt-in
  for every other profile. Pass TRUE/FALSE explicitly to override either
  way, regardless of profile. When run, computes the Injury Severity
  Score (ISS/NISS via `icdpicr`) and writes to `{raw_schema}.iss`.
  Requires the `icdpicr` package. Skipped gracefully when
  `skip_on_error = TRUE` and the package is not installed.

- raw_data_root:

  Character string giving the parent directory of the raw source files.
  Files for a source are read from `raw_data_root/src_name`. If `NULL`,
  use the project configuration or `data/source_data` under `base_dir`.

## Value

An object of class `ehr_suggestion` with `status`, `recommended_code`,
and `merge_status` fields.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_suggest_next(src_names = "mimic_iv_demo", db_name = "rendehr")
} # }
```

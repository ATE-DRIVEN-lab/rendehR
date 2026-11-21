# Build cohort annotation tables (ICD lookup, injury cause, derived flags)

Pipeline annotation stage (`run_cohort_annotations = TRUE`). Writes the
ICD classification lookup (`d_cohort_icd`), the injury cause/etiology
table when configured, and `cohort_info` into the raw schema, in one
atomic publication. It is the same as
[`build_cohort_annotations_general()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_annotations_general.md):
`cohort_index` comes from the cohort definition recorded at import, so
it works for every selection route - ICD, SQL, direct IDs, function or
no filter - and needs no ICD filter unless SCI enrichment is requested.

## Usage

``` r
build_cohort_annotations(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  icd_filter_path = NULL,
  icds_inj_path = NULL,
  classify_fn = NULL,
  classify_args = list(),
  analysis_level = c("admission", "icu"),
  cohort_selection = NULL,
  enrichment = NULL
)
```

## Arguments

- src_name:

  Source name.

- db_name:

  Optional Postgres database name. If NULL, uses the active project
  config, then "rendehr".

- base_dir:

  Project root directory. Defaults to current working directory.

- icd_filter_path:

  Optional ICD classification CSV or RDS, published as `d_cohort_icd`
  (empty when absent). Defaults to the configured filter (the
  package-embedded SCI list for the `scid` profile). Required only for
  the `scid` profile's default SCI enrichment; never used for
  membership.

- icds_inj_path:

  SCI-specific. Path to injury cause/etiology CSV. Uses the
  package-embedded SCI file by default.

- classify_fn:

  Optional enrichment function passed to the cohort-info builder. If
  NULL and the cohort profile is SCI,
  [`scid_classify()`](https://ate-driven-lab.github.io/rendehR/reference/scid_classify.md)
  is used.

- classify_args:

  Named list of additional arguments passed to `classify_fn`.

- analysis_level:

  Grain of the composite `ehr_id` key: `"admission"` (default) keys on
  patient + admission; `"icu"` additionally appends the ICU stay id.

- cohort_selection:

  Optional overrides from
  [`cohort_selection_options()`](https://ate-driven-lab.github.io/rendehR/reference/cohort_selection_options.md)
  or a named list.

- enrichment:

  Optional overrides from
  [`enrichment_options()`](https://ate-driven-lab.github.io/rendehR/reference/enrichment_options.md)
  or a named list.

## Value

`NULL`, invisibly. Called for its side effect of writing the cohort
annotation tables to the source's raw schema.

## Examples

``` r
if (FALSE) { # \dontrun{
build_cohort_annotations("mimic_iv_demo", db_name = "rendehr", classify_fn = scid_classify)
} # }
```

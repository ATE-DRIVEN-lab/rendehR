# Build generalized cohort annotation tables

Generalized companion to
[`build_cohort_annotations()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_annotations.md).
It preserves the annotation-table behavior when ICD assets are
available, but takes analysis membership from the cohort hierarchy:
`cohort_info.cohort_index` is copied from the cohort definition recorded
at import, not re-derived from ICD matching. Use this after
[`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
and
[`run_db()`](https://ate-driven-lab.github.io/rendehR/reference/run_db.md).

## Usage

``` r
build_cohort_annotations_general(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  icd_filter_path = NULL,
  icds_inj_path = NULL,
  classify_fn = NULL,
  classify_args = list(),
  analysis_level = c("admission", "icu")
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

  Optional ICD classification .rds/.csv used for annotation/enrichment
  only (published as `d_cohort_icd`, empty when not supplied). It is not
  the source of `cohort_index`. Required only for the `scid` profile's
  default SCI enrichment.

- icds_inj_path:

  Optional injury cause/etiology CSV.

- classify_fn:

  Optional enrichment function passed to
  [`build_cohort_info_general()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_info_general.md).
  If NULL and SCI ICD assets are configured,
  [`scid_classify()`](https://ate-driven-lab.github.io/rendehR/reference/scid_classify.md)
  is used for enrichment.

- classify_args:

  Named list of additional arguments passed to `classify_fn`.

- analysis_level:

  Grain of the composite `ehr_id` key: `"admission"` (default) keys on
  patient + admission; `"icu"` additionally appends the ICU stay id.

## Value

`NULL`, invisibly. Called for its side effect of writing the cohort
annotation tables to the source's raw schema.

## Examples

``` r
if (FALSE) { # \dontrun{
build_cohort_annotations_general("mimic_iv_demo", db_name = "rendehr")
} # }
```

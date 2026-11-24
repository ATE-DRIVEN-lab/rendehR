# Build cohort_info from generalized cohort_index metadata

Companion to
[`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md).
It builds the usual generic `cohort_info` diagnostic and hospitalization
summary columns in Postgres, and takes the cohort membership flag
`cohort_index` from the cohort definition recorded at import
(`cohort_hierarchy`) instead of ICD matching. Use this for direct-ID,
SQL, named SQL cohorts such as sepsis3, or any cohort whose membership
was written by
[`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md).

## Usage

``` r
build_cohort_info_general(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  icd_filter_path = NULL,
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

  Optional Postgres database name.

- base_dir:

  Project root directory.

- icd_filter_path:

  Optional ICD filter used only to populate ICD-derived enrichment
  inputs. Defaults to the project's configured filter
  (`ehr_paths()$icd_filter_path`), which is set only for the `scid`
  profile. If NULL, no ICDs are treated as cohort-defining;
  `cohort_index` still comes from the cohort definition recorded at
  import.

- classify_fn:

  Optional enrichment function passed through to the source cohort_info
  builder. Defaults to
  [`scid_classify()`](https://ate-driven-lab.github.io/rendehR/reference/scid_classify.md)
  when the cohort profile is `scid` and an ICD filter is available; any
  explicit value, supplied directly or through
  [`enrichment_options()`](https://ate-driven-lab.github.io/rendehR/reference/enrichment_options.md),
  takes precedence.

- classify_args:

  Extra arguments for `classify_fn`. Merged over the defaults chosen for
  the `scid` profile, so individual entries such as `icds_cause_path`
  can be overridden without replacing the rest.

- analysis_level:

  Grain of the composite `ehr_id` key.

- cohort_selection:

  Optional overrides from
  [`cohort_selection_options()`](https://ate-driven-lab.github.io/rendehR/reference/cohort_selection_options.md)
  or a named list.

- enrichment:

  Optional overrides from
  [`enrichment_options()`](https://ate-driven-lab.github.io/rendehR/reference/enrichment_options.md)
  or a named list.

## Value

The cohort_info data frame, invisibly. It is also written to the
source's raw schema.

## Details

For the `scid` cohort profile this function additionally enriches
`cohort_info` with the SCI classification and injury-cause columns, by
defaulting `classify_fn` to
[`scid_classify()`](https://ate-driven-lab.github.io/rendehR/reference/scid_classify.md).
MIMIC sources also gain the procedure columns (`proc_*`): the
procedure-classification asset is reduced to one row per ICD code, and
codes with conflicting operative status are marked `ambiguous`. eICU has
no ICD-coded procedures and reports
`proc_classification_status = "not_reported"`.

## Examples

``` r
if (FALSE) { # \dontrun{
build_cohort_info_general("mimic_iv_demo", db_name = "rendehr")
} # }
```

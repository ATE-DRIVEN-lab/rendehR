# Build generic cohort information

Writes cohort_info with generic columns only: ehr_id, hospitalization
sequence, diagnostic ICD summary, and the cohort membership flag
`cohort_index`. Use this when you do not need domain-specific enrichment
(no classify_fn).

## Usage

``` r
build_cohort_info(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  icd_filter_path = NULL,
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

  Project root directory. Defaults to getwd().

- icd_filter_path:

  Deprecated and ignored: membership comes from the cohort definition
  recorded at import (see
  [`run_cohort_info()`](https://ate-driven-lab.github.io/rendehR/reference/run_cohort_info.md)).

- analysis_level:

  Grain of the composite `ehr_id` key and of `cohort_index`:
  `"admission"` (default) keys on patient + admission; `"icu"`
  additionally appends the ICU stay id.

## Value

Invisibly returns the built `cohort_info` data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
build_cohort_info("mimic_iv_demo", db_name = "rendehr",
  icd_filter_path = "config/my_icd_codes.csv")
} # }
```

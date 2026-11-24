# Fetch the cohort_info table for a source

cohort_info contains per-admission cohort metadata: hospitalization
sequence, diagnostic ICD summary, the cohort membership flag
cohort_index, and any domain-specific enrichment columns added by the
classify_fn supplied to run_cohort_info(). All rows are returned,
including imported patient-context rows that are not cohort members.

## Usage

``` r
load_cohort_info(src_name, n = Inf, con = NULL)
```

## Arguments

- src_name:

  Source name.

- n:

  Max rows. Default Inf.

- con:

  Optional explicit DBI connection.

## Value

A tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
ci <- load_cohort_info("mimic_iv_demo")
table(ci$cohort_index)
} # }
```

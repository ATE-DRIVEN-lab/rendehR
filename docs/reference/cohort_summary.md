# High-level summary of a source cohort

Counts the cohort's patients, admissions and ICU stays and the patients
by sex in flat_tab (where each table exists). Useful as a sanity check
after running the pipeline.

## Usage

``` r
cohort_summary(src_name, con = NULL)
```

## Arguments

- src_name:

  Source name.

- con:

  Optional explicit DBI connection.

## Value

Invisibly, a named list of the counts it prints: `source`,
`icu_windows_rows` and `icu_patients` (ICU stays and the patients who
had one), `cohort_info_rows`, `patients` and `admissions` (distinct
cohort patients and hospital admissions), `concept_tables`, `ts_rows`,
`flat_rows` and `sex` (distinct patients by recorded sex; a patient
whose admissions record different values counts under each), each
present when its table exists.

## Examples

``` r
if (FALSE) { # \dontrun{
cohort_summary("mimic_iv_demo")
} # }
```

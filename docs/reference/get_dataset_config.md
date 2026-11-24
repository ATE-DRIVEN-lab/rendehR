# Get source dataset configuration

Returns the configuration list for a supported source, resolved from the
source registry. This is most useful when checking source-specific
identifier columns or schema defaults. The `schema` is the raw cohort
schema for the exact source name (e.g. `"mimic_iv_demo_coh"`), and
`row_limit` is the demo cap when `dataset` is a demo variant, else
`NULL`.

## Usage

``` r
get_dataset_config(dataset)
```

## Arguments

- dataset:

  Source name, such as `"mimic_iv_demo"`, `"mimic_iii_demo"`, or
  `"eicu_demo"`.

## Value

A named list containing source configuration values.

## Examples

``` r
str(get_dataset_config("eicu"))
#> List of 5
#>  $ select_id_cols: chr [1:3] "uniquepid" "patienthealthsystemstayid" "patientunitstayid"
#>  $ time_origin   : chr "cw.icu_intime"
#>  $ icu_id_type   : chr "bigint"
#>  $ row_limit     : NULL
#>  $ schema        : chr "eicu_coh"
```

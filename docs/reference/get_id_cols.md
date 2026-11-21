# Get identifier columns for a source table

Returns the source-specific identifier columns used to join a raw table
to the appropriate rendehR window table.

## Usage

``` r
get_id_cols(dataset, table_name)
```

## Arguments

- dataset:

  Source name, such as `"mimic_iv_demo"`, `"mimic_iii_demo"`, or
  `"eicu_demo"`.

- table_name:

  Raw source table name.

## Value

Character vector of identifier column names.

## Examples

``` r
get_id_cols("mimic_iii", "chartevents")
#> [1] "subject_id" "hadm_id"    "icustay_id"
get_id_cols("mimic_iii", "prescriptions")
#> [1] "subject_id" "hadm_id"   
```

# Return the primary cohort key column for a source

Returns the patient-root identifier configured for the source family.
The canonical cohort engine uses this as the coarsest hierarchy level
and the default effective scope; individual clinical tables are filtered
through their catalog-declared linkage keys.

## Usage

``` r
cohort_key(src_name)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

## Value

Character scalar column name.

## Examples

``` r
cohort_key("mimic_iv_demo")
#> [1] "subject_id"
cohort_key("eicu_demo")
#> [1] "patientunitstayid"
```

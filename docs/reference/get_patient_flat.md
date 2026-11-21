# Fetch the flat (baseline) row for one patient

Returns the patient's rows from `flat_tab`: one per admission (and ICU
stay), with static features such as sex, race, insurance and age.

## Usage

``` r
get_patient_flat(src_name, patient_id, con = NULL)
```

## Arguments

- src_name:

  Source name.

- patient_id:

  The patient identifier value.

- con:

  Optional explicit DBI connection.

## Value

A tibble with one row per admission of the patient (zero rows if the
patient is not in the cohort).

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
get_patient_flat("mimic_iv_demo", patient_id = 10000032)
} # }
```

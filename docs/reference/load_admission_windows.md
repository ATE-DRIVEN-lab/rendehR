# Fetch the admission_windows table for a source

admission_windows has one row per hospital admission/hospital stay.

## Usage

``` r
load_admission_windows(src_name, n = Inf, con = NULL)
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
load_admission_windows("mimic_iv_demo", n = 5)
} # }
```

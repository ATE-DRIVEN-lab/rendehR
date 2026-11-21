# Fetch the icu_windows table for a source

icu_windows has one row per ICU/unit stay.

## Usage

``` r
load_icu_windows(src_name, n = Inf, con = NULL)
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
load_icu_windows("mimic_iv_demo", n = 5)
} # }
```

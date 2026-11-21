# Fetch the visit_windows table for a source

visit_windows is the admission-to-ICU crosswalk. It keeps admissions
without ICU stays as rows with missing ICU fields.

## Usage

``` r
load_visit_windows(src_name, n = Inf, con = NULL)
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
load_visit_windows("eicu_demo", n = 5)
} # }
```

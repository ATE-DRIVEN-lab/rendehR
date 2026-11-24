# Verify cohort anchor table was built correctly

Verify cohort anchor table was built correctly

## Usage

``` r
verify_cohort(con, schema)
```

## Arguments

- con:

  DBI connection.

- schema:

  Raw cohort schema (e.g. "mimic_iv_demo_coh").

## Value

Verification result list.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
verify_cohort(con, "mimic_iv_demo_coh")
} # }
```

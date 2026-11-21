# Verify handcrafted concept tables (SOFA, vitalsign, scores, etc.)

Verify handcrafted concept tables (SOFA, vitalsign, scores, etc.)

## Usage

``` r
verify_handcrafted(con, schema, expected_tables = NULL)
```

## Arguments

- con:

  DBI connection.

- schema:

  Schema where handcrafted concepts were written.

- expected_tables:

  Character vector of table names to check. NULL (the default) checks
  the standard MIMIC-code tables (SOFA, first-day summaries, scores) for
  MIMIC schemas, and that at least one handcrafted table exists for any
  other source family.

## Value

Verification result list.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
verify_handcrafted(con, "mimic_iv_demo_coh_concepts")
} # }
```

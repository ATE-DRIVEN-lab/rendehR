# Verify atomic concept tables were created

Verify atomic concept tables were created

## Usage

``` r
verify_concepts(con, schema, concept_names = NULL)
```

## Arguments

- con:

  DBI connection.

- schema:

  Concepts schema (e.g. "mimic_iv_demo_coh_concepts").

- concept_names:

  Character vector of concept table names. NULL checks that at least one
  table exists in the schema.

## Value

Verification result list.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
verify_concepts(con, "mimic_iv_demo_coh_concepts", concept_names = c("heart_rate", "sbp"))
} # }
```

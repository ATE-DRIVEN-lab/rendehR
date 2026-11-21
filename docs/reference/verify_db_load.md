# Verify raw tables were loaded into Postgres

Verify raw tables were loaded into Postgres

## Usage

``` r
verify_db_load(con, schema, required_tables = NULL)
```

## Arguments

- con:

  DBI connection.

- schema:

  Raw cohort schema.

- required_tables:

  Character vector of table names that must exist and have rows. NULL
  (the default) checks the three window tables plus the core tables of
  the schema's source family (`patients`/`admissions` for MIMIC,
  `patient` for eICU).

## Value

Verification result list.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
verify_db_load(con, "mimic_iv_demo_coh", required_tables = c("patients", "admissions"))
} # }
```

# Fetch a table (or the first n rows) from Postgres into a tibble

Fetch a table (or the first n rows) from Postgres into a tibble

## Usage

``` r
get_table_from_db(con, schema, table, n = Inf)
```

## Arguments

- con:

  DBI connection.

- schema:

  Schema name.

- table:

  Table name.

- n:

  Max rows to collect (default Inf = all).

## Value

A tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
get_table_from_db(con, "mimic_iv_demo_coh", "patients", n = 10)
} # }
```

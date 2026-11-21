# Create a Postgres index if it doesn't already exist

Create a Postgres index if it doesn't already exist

## Usage

``` r
create_index(con, schema, table, index_name, columns)
```

## Arguments

- con:

  DBI connection.

- schema:

  Schema name.

- table:

  Table name.

- index_name:

  Name to give the index.

- columns:

  Character vector of column names to index.

## Value

The value returned by
[`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html) for
the `CREATE INDEX` statement.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
create_index(con, "mimic_iv_demo_coh", "labevents", "labevents_hadm_idx", c("hadm_id", "itemid"))
} # }
```

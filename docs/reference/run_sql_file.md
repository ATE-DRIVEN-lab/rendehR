# Execute a single .sql file against an open connection

Execute a single .sql file against an open connection

## Usage

``` r
run_sql_file(con, path)
```

## Arguments

- con:

  DBI connection.

- path:

  File path to a .sql file.

## Value

The value returned by
[`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html) for
the file's SQL.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
run_sql_file(con, "sql/my_concept.sql")
} # }
```

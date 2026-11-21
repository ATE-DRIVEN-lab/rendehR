# Execute all .sql files in a directory, sorted alphabetically

Execute all .sql files in a directory, sorted alphabetically

## Usage

``` r
run_sql_dir(con, dir_path)
```

## Arguments

- con:

  DBI connection.

- dir_path:

  Directory containing .sql files.

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
run_sql_dir(con, "sql/my_concepts")
} # }
```

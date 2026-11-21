# Create all schemas in a schema_names list if they don't exist

Create all schemas in a schema_names list if they don't exist

## Usage

``` r
create_schemas(con, schema_names)
```

## Arguments

- con:

  DBI connection.

- schema_names:

  Named list of schema name strings (from src_schema_names()).

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
create_schemas(con, unlist(src_schema_names("mimic_iv_demo")))
} # }
```

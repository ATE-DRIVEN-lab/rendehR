# Create the target database if it does not exist

Create the target database if it does not exist

## Usage

``` r
ensure_database_exists(db_name, creds)
```

## Arguments

- db_name:

  Database name to create.

- creds:

  List returned by load_credentials().

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")
ensure_database_exists("rendehr", creds)
} # }
```

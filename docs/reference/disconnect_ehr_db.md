# Disconnect a database browser returned by `browse_ehr_db_flex()`

Disconnect a database browser returned by
[`browse_ehr_db_flex()`](https://ate-driven-lab.github.io/rendehR/reference/browse_ehr_db_flex.md)

## Usage

``` r
disconnect_ehr_db(x)
```

## Arguments

- x:

  Object returned by
  [`browse_ehr_db_flex()`](https://ate-driven-lab.github.io/rendehR/reference/browse_ehr_db_flex.md).

## Value

Invisibly returns NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")
con <- connect_postgres("rendehr", creds)
disconnect_ehr_db(con)
} # }
```

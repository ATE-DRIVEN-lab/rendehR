# List all rendehR sources present in the database

Scans `information_schema.schemata` for schemas matching the `*_coh`
pattern and strips the suffix to recover the source names.

## Usage

``` r
list_sources(con = NULL)
```

## Arguments

- con:

  Optional explicit DBI connection.

## Value

Character vector of source names (e.g. "mimic_iv_demo", "eicu").

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
list_sources()
} # }
```

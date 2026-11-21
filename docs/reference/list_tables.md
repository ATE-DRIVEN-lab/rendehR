# List all tables in a source schema

List all tables in a source schema

## Usage

``` r
list_tables(src_name, schema = "raw", con = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string.

- con:

  Optional explicit DBI connection.

## Value

Character vector of table names.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
list_tables("mimic_iv_demo")
list_tables("mimic_iv_demo", schema = "harmonized")
} # }
```

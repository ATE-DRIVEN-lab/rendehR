# Fetch a table from any source schema into a tibble

Opens a connection, collects the table, closes the connection. Use
`schema` to target raw tables, concept tables, or harmonized outputs.

## Usage

``` r
fetch_table(src_name, table, schema = "raw", n = Inf, con = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- table:

  Table name.

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string.

- n:

  Max rows to return. Default Inf (all rows).

- con:

  Optional explicit DBI connection (skips auto-open/close).

## Value

A tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
patients <- fetch_table("mimic_iv_demo", "patients")
ts <- fetch_table("mimic_iv_demo", "ts_tab", schema = "harmonized", n = 1000)
} # }
```

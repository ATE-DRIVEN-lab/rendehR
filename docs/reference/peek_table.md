# Preview the first N rows of any table

Quick look - like [`head()`](https://rdrr.io/r/utils/head.html) but
reads from Postgres. Does not load the full table. Prints a summary and
returns the rows invisibly.

## Usage

``` r
peek_table(src_name, table, schema = "raw", n = 10, con = NULL)
```

## Arguments

- src_name:

  Source name.

- table:

  Table name.

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string.

- n:

  Number of rows to preview. Default 10.

- con:

  Optional explicit DBI connection.

## Value

Tibble of `n` rows (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
peek_table("mimic_iv_demo", "admissions")
} # }
```

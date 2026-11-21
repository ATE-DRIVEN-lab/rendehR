# Safely drop a single table from a source schema

Dry-run by default - prints what would be dropped without doing
anything. Set `dry_run = FALSE` to execute.

## Usage

``` r
drop_table(src_name, table, schema = "raw", dry_run = TRUE, con = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- table:

  Table name to drop.

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string.

- dry_run:

  If TRUE (default), only print what would happen.

- con:

  Optional explicit DBI connection.

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# preview first
drop_table("mimic_iv_demo", "annotations", schema = "raw")

# actually drop
drop_table("mimic_iv_demo", "annotations", schema = "raw", dry_run = FALSE)
} # }
```

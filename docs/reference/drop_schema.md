# Safely drop an entire schema for a source

Lists all tables in the schema first, then drops the schema if
confirmed. Dry-run by default.

## Usage

``` r
drop_schema(src_name, schema = "concepts", dry_run = TRUE, con = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

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
# preview - shows all tables that would be deleted
drop_schema("mimic_iv_demo", "concepts")

# actually drop
drop_schema("mimic_iv_demo", "concepts", dry_run = FALSE)
} # }
```

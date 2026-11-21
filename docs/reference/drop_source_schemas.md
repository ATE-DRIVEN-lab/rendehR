# Safely drop all schemas for a source

Targets all rendehR schemas for a source: raw, concepts, ts, flat, dur,
drv. Dry-run by default - always preview before executing.

## Usage

``` r
drop_source_schemas(src_name, dry_run = TRUE, con = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- dry_run:

  If TRUE (default), only print what would happen.

- con:

  Optional explicit DBI connection.

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# preview everything that would be deleted
drop_source_schemas("mimic_iv_demo")

# actually drop all schemas for this source
drop_source_schemas("mimic_iv_demo", dry_run = FALSE)
} # }
```

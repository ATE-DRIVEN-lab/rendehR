# Safely drop a merged schema

Companion to
[`drop_schema()`](https://ate-driven-lab.github.io/rendehR/reference/drop_schema.md)
for merge targets created by
[`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
(e.g. "rendehr", "trauma_project"). Those schemas are not `*_coh*`
names, so the
[`drop_schema()`](https://ate-driven-lab.github.io/rendehR/reference/drop_schema.md)
guard refuses them; this function allowlists merge targets instead - it
confirms the schema carries a merge-output table (ts/flat/cohort_info)
and is not a source or system schema. Lists the tables first; dry-run by
default.

## Usage

``` r
drop_merged_schema(schema, dry_run = TRUE, con = NULL)
```

## Arguments

- schema:

  Merged schema name passed to merge_sources().

- dry_run:

  If TRUE (default), only print what would happen.

- con:

  Optional explicit DBI connection.

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
drop_merged_schema("rendehr")                    # preview
drop_merged_schema("rendehr", dry_run = FALSE)   # actually drop
} # }
```

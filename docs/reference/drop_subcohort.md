# Drop all schemas for a materialized subgroup

Calls drop_source_schemas() for the subgroup source name. Dry-run by
default.

## Usage

``` r
drop_subcohort(src_name, subgroup_name, dry_run = TRUE, con = NULL)
```

## Arguments

- src_name:

  Original source name (e.g. "mimic_iv_demo").

- subgroup_name:

  Subgroup name used when calling materialize_subcohort().

- dry_run:

  If TRUE (default), only print what would be dropped.

- con:

  Optional explicit DBI connection.

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
drop_subcohort("mimic_iv_demo", "cervical_complete")           # preview
drop_subcohort("mimic_iv_demo", "cervical_complete", dry_run = FALSE)
} # }
```

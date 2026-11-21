# Drop a single relation (table, view, or materialized view) by name

Unlike
[`drop_table()`](https://ate-driven-lab.github.io/rendehR/reference/drop_table.md)/[`drop_schema()`](https://ate-driven-lab.github.io/rendehR/reference/drop_schema.md),
this is **not** restricted to rendehR `*_coh*` schemas, so it can remove
objects in a merged schema (e.g. `rendehr.ts`). It looks up the
relation's actual type and issues the matching `DROP ... CASCADE`, so it
works whether the target is a table, view, or materialized view. Dry-run
by default.

## Usage

``` r
drop_relation(relation, schema = NULL, dry_run = TRUE, con = NULL)
```

## Arguments

- relation:

  Relation name: either `"schema.table"`, or just `"table"` together
  with `schema`.

- schema:

  Optional schema name; prepended to `relation` when supplied.

- dry_run:

  If TRUE (default), only print what would happen.

- con:

  Optional explicit DBI connection.

## Value

`NULL`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
drop_relation("rendehr.ts")                       # preview
drop_relation("ts", schema = "rendehr", dry_run = FALSE)
} # }
```

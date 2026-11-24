# Build the wide/flat table with one column per static concept

Joins each flat concept table onto `visit_windows` using the source's ID
columns. Concepts whose join or value column cannot be verified are
skipped with a message. If `flat_concepts` is empty the table is still
created with only the base ID columns.

## Usage

``` r
build_flat_table(
  con,
  src_name,
  schema_names,
  flat_concepts,
  table_name = "flat_tab"
)
```

## Arguments

- con:

  A DBI connection to the Postgres database.

- src_name:

  Source name (e.g. `"mimic_iv_demo"`).

- schema_names:

  Named list of schema names (from
  [`src_schema_names()`](https://ate-driven-lab.github.io/rendehR/reference/src_schema_names.md)).

- flat_concepts:

  Character vector of concept table names to pivot as columns (e.g.
  `c("sex", "race")`).

- table_name:

  Name for the output table (default `"flat_tab"`).

## Value

Invisibly, the number of rows affected by the CREATE TABLE statement.

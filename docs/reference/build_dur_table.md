# Build the long-format duration table from concept tables

Unions all valid duration concept tables (medication exposures,
infusions, vasopressors, ventilation episodes) in the concepts schema
into a single `dur_tab` table. Concepts missing expected columns or not
found in the database are silently skipped with a warning.

## Usage

``` r
build_dur_table(
  con,
  src_name,
  schema_names,
  concept_names,
  table_name = "dur_tab"
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

- concept_names:

  Character vector of duration concept table names to include.

- table_name:

  Name for the output table (default `"dur_tab"`).

## Value

Invisibly, the number of rows affected by the CREATE TABLE statement.

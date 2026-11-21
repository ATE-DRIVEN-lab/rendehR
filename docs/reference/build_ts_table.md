# Build harmonized time-series and flat output tables. Build the long-format time-series table from concept tables

Unions all valid time-series concept tables in the concepts schema into
a single `ts_tab` table. Concepts missing expected columns or not found
in the database are silently skipped with a warning.

## Usage

``` r
build_ts_table(
  con,
  src_name,
  schema_names,
  concept_names,
  table_name = "ts_tab"
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

  Character vector of concept table names to include.

- table_name:

  Name for the output table (default `"ts_tab"`).

## Value

Invisibly, the number of rows affected by the CREATE TABLE statement.

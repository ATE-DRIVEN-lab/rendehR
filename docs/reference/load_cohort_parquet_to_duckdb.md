# Load all parquet files from a cohort directory into DuckDB

Each `.parquet` file in `parquet_dir` becomes one table (table name =
filename without extension) in `schema`. Unlike the Postgres loader,
this does not stream batches through R or need pre-created DDL: DuckDB
reads the parquet directly with
`CREATE TABLE ... AS SELECT * FROM read_parquet(...)`, inferring column
types from the parquet metadata (which arrow wrote with correct types).
That native path is the main thing this backend exists to benchmark.

## Usage

``` r
load_cohort_parquet_to_duckdb(
  con,
  parquet_dir,
  schema,
  overwrite = TRUE,
  show_progress = TRUE,
  table_schema = NULL
)
```

## Arguments

- con:

  A DuckDB DBI connection (from
  [`connect_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/connect_duckdb.md)).

- parquet_dir:

  Directory of cohort parquet files.

- schema:

  Target schema (created if absent).

- overwrite:

  If TRUE (default), each table is replaced (`CREATE OR REPLACE TABLE`).
  If FALSE, a table that already exists with a matching row count is
  skipped; otherwise it is (re)created.

- show_progress:

  Print per-table progress messages.

- table_schema:

  Optional named list of DuckDB column types, table -\> named character
  vector column -\> type. When supplied, tables are cast to this schema
  as they are materialized.

## Value

Invisibly, a data frame of `table`, `rows`, `seconds`.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_duckdb("data/duckdb/mimic_iv_demo.duckdb")
load_cohort_parquet_to_duckdb(con, "data/cohorts_data/mimic_iv_demo_parquet", "mimic_iv_demo_coh")
} # }
```

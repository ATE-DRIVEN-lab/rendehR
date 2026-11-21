# Create a cohort database in DuckDB

DuckDB analogue of the Postgres path in
[`run_db()`](https://ate-driven-lab.github.io/rendehR/reference/run_db.md):
opens (or creates) the `.duckdb` file, creates the source schemas, and
loads the cohort parquet into the raw schema. Concept extraction is not
yet ported to DuckDB; this covers the load stage only.

## Usage

``` r
run_db_duckdb(
  src_name,
  db_path = NULL,
  base_dir = NULL,
  overwrite = TRUE,
  show_progress = TRUE
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- db_path:

  Path to the `.duckdb` file. Default: a file named after the source
  under `paths.duckdb_root` (`data/duckdb` by default).

- base_dir:

  Project root directory.

- overwrite:

  Passed to
  [`load_cohort_parquet_to_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/load_cohort_parquet_to_duckdb.md).

- show_progress:

  Print progress messages.

## Value

Invisibly, the load report data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
run_db_duckdb("mimic_iv_demo", db_path = "data/duckdb/mimic_iv_demo.duckdb")
} # }
```

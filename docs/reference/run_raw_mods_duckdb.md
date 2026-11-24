# Build modified (\_mod) raw tables in a DuckDB cohort database

Runs SQL files from the source's `raw_mods` directory against the DuckDB
raw schema. The SQL is passed through
[`adapt_sql_for_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/adapt_sql_for_duckdb.md)
so the same Postgres-oriented raw-mod files can be reused where syntax
differences are conservative and known.

## Usage

``` r
run_raw_mods_duckdb(
  src_name,
  db_path = NULL,
  base_dir = NULL,
  skip_on_error = FALSE,
  show_progress = TRUE
)
```

## Arguments

- src_name:

  Source name (e.g. `"eicu"` or `"eicu_demo"`).

- db_path:

  Path to the DuckDB database. Defaults to
  [`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md)'s
  source DuckDB path.

- base_dir:

  Project root directory.

- skip_on_error:

  If TRUE, warn and continue evaluating the remaining files, then fail
  the overall operation if any required file failed. Default FALSE.

- show_progress:

  Print per-file progress messages. Default TRUE.

## Value

Invisibly TRUE if mods were run, NULL if no mod directory/files exist.

## Examples

``` r
if (FALSE) { # \dontrun{
run_raw_mods_duckdb("eicu_demo", db_path = "data/duckdb/eicu_demo.duckdb")
} # }
```

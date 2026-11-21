# Specify import and database-loading options

Configure import resources, CSV parsing, raw-cache use, and PostgreSQL
loading.

## Usage

``` r
loading_options(
  memory_limit = NULL,
  threads = NULL,
  temp_directory = NULL,
  table_chunk_rows = 5e+06,
  validate_columns = FALSE,
  ignore_errors = TRUE,
  store_rejects = TRUE,
  use_parquet_cache = TRUE,
  delete_source_parquet = FALSE,
  unlogged = FALSE,
  set_logged = FALSE,
  synchronous_commit_off = FALSE,
  create_indexes = TRUE
)
```

## Arguments

- memory_limit:

  Character string specifying a DuckDB memory limit, for example
  `"4GB"`. `NULL` uses the receiving function's connection settings.

- threads:

  Positive integer specifying the number of DuckDB worker threads.
  `NULL` uses the receiving function's connection settings.

- temp_directory:

  Character string giving a directory for temporary DuckDB files. `NULL`
  uses the receiving function's temporary-directory settings.

- table_chunk_rows:

  Positive number used as the table-size threshold for chunked cohort
  materialization. Default: `5e6` rows. `NULL` requests a single build
  operation per table.

- validate_columns:

  Logical. Check imported values against configured column types. When
  omitted, cohort import uses `FALSE` and raw caching uses `TRUE`,
  including when called through the pipeline. An explicit value applies
  to both stages.

- ignore_errors:

  Logical. Allow the CSV reader to skip malformed rows. Default: `TRUE`
  in this package. Use `FALSE` to stop on a parsing error.

- store_rejects:

  Logical. Record rejected-row information when malformed CSV rows are
  skipped. Default: `TRUE`.

- use_parquet_cache:

  Logical. Use an available raw Parquet cache during cohort import.
  Default: `TRUE`.

- delete_source_parquet:

  Logical. Delete cohort Parquet files after they have been successfully
  loaded into PostgreSQL. Default: `FALSE`.

- unlogged:

  Logical. Load PostgreSQL tables as unlogged tables. Default: `FALSE`.
  See
  [`run_db()`](https://ate-driven-lab.github.io/rendehR/reference/run_db.md)
  for storage behavior.

- set_logged:

  Logical. Convert unlogged PostgreSQL tables to logged tables after
  loading. Default: `FALSE`.

- synchronous_commit_off:

  Logical. Disable synchronous commit during PostgreSQL loading.
  Default: `FALSE`.

- create_indexes:

  Logical. Create the configured PostgreSQL indexes during loading.
  Default: `TRUE`.

## Value

A named list of the supplied options, suitable for the `loading`
argument of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
and compatible direct stage functions.

## Details

Memory, thread, and temporary-directory settings apply to both raw
caching and cohort import. Parsing and chunking settings apply to cohort
import; PostgreSQL settings apply to database loading. Shared groups are
validated in full, then each stage uses only the settings it supports.

Only explicitly supplied arguments are included in the result, including
explicit `NULL` values. Omitted settings use the defaults of the
receiving function. An empty options list therefore leaves its defaults
unchanged.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)

## Examples

``` r
loading_options(memory_limit = "4GB", threads = 2, ignore_errors = FALSE)
#> $memory_limit
#> [1] "4GB"
#> 
#> $threads
#> [1] 2
#> 
#> $ignore_errors
#> [1] FALSE
#> 
loading_options(create_indexes = FALSE)
#> $create_indexes
#> [1] FALSE
#> 
```

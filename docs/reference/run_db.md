# Load cohort parquet into Postgres

Validates the accepted manifest-backed cohort artifact, loads and
validates it in a run-specific staging schema, builds the selected
release's indexes, and transactionally promotes the complete schema. A
failed load, validation, or index phase leaves the prior raw schema in
place. Run build_window_tables() and optionally
build_cohort_annotations() afterwards to add lookup and derived tables.

## Usage

``` r
run_db(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  delete_source_parquet = FALSE,
  overwrite = FALSE,
  unlogged = FALSE,
  set_logged = FALSE,
  synchronous_commit_off = FALSE,
  engine = c("postgres", "duckdb"),
  db_path = NULL,
  create_indexes = TRUE,
  loading = NULL,
  execution = NULL
)
```

## Arguments

- src_name:

  Source name.

- db_name:

  Optional Postgres database name. If NULL, uses the active project
  config, then "rendehr".

- base_dir:

  Project root directory.

- delete_source_parquet:

  Delete each parquet file after it is successfully loaded into
  Postgres. Default FALSE.

- overwrite:

  If FALSE (default), a manifest-complete existing raw schema is
  retained and any missing indexes are resumed idempotently. Otherwise a
  complete replacement is built in staging. If TRUE, always build and
  validate a fresh staged replacement.

- unlogged:

  If TRUE, load into UNLOGGED tables for speed. Much faster, but
  UNLOGGED tables are emptied on an unclean shutdown and are not
  replicated - safe here only because the raw tables are reproducible
  from the cohort parquet. Default FALSE. Use `set_logged = TRUE` to
  restore durability after.

- set_logged:

  If TRUE (with `unlogged = TRUE`), convert tables back to durable
  LOGGED after each loads (rewrites the table). Default FALSE.

- synchronous_commit_off:

  If TRUE, set `synchronous_commit = off` for the load session (restored
  afterwards) to speed bulk loading. Default FALSE.

- engine:

  Storage backend: "postgres" (default) or "duckdb". With "duckdb" the
  cohort parquet is loaded into a single `.duckdb` file via
  [`run_db_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/run_db_duckdb.md);
  `db_name` and the Postgres-only tuning flags are ignored.
  `run_one_source(engine = "duckdb")` uses this path and then extracts
  the concepts in the same DuckDB file; harmonization stays
  PostgreSQL-only.

- db_path:

  DuckDB file path (only used when `engine = "duckdb"`). Default uses
  `paths.duckdb_root` (`data/duckdb` by default) plus
  `{src_name}.duckdb`.

- create_indexes:

  Build the bundled post-load indexes before promotion? Default TRUE.
  Set FALSE to explicitly defer the potentially expensive step. Existing
  complete schemas resume the idempotent index phase.

- loading:

  Optional overrides from
  [`loading_options()`](https://ate-driven-lab.github.io/rendehR/reference/loading_options.md)
  or a named list.

- execution:

  Optional overrides from
  [`execution_options()`](https://ate-driven-lab.github.io/rendehR/reference/execution_options.md)
  or a named list.

## Value

Invisibly, a list containing aggregate load, validation, and index
reports for PostgreSQL, or the DuckDB load report.

## Examples

``` r
if (FALSE) { # \dontrun{
run_db("mimic_iv_demo", db_name = "rendehr")
} # }
```

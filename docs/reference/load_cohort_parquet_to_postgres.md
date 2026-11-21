# Load all parquet files from a cohort directory into Postgres

Each .parquet file in parquet_dir is loaded as one table (table name =
filename without extension).

## Usage

``` r
load_cohort_parquet_to_postgres(
  con,
  parquet_dir,
  schema,
  overwrite = TRUE,
  show_progress = TRUE,
  delete_source_parquet = FALSE,
  unlogged = FALSE,
  set_logged = FALSE,
  synchronous_commit_off = FALSE
)
```

## Arguments

- con:

  DBI connection.

- parquet_dir:

  Path to directory of cohort parquet files.

- schema:

  Target Postgres schema.

- overwrite:

  If TRUE (default), reload every table: an existing table is truncated
  (keeping its DDL types) and reloaded. If FALSE, resume: a table whose
  row count already matches its parquet is skipped; a partially loaded
  table is truncated and reloaded.

- show_progress:

  Print per-table progress messages.

- delete_source_parquet:

  Delete each parquet file after it is successfully loaded into
  Postgres. Default FALSE. Saves disk space but prevents re-loading
  without regenerating the parquet.

- unlogged:

  If TRUE, set each pre-created (DDL) table to UNLOGGED before loading,
  which can be much faster for a bulk load. Default FALSE. WARNING:
  UNLOGGED tables are **emptied** by Postgres on any unclean
  shutdown/crash recovery and are not replicated. Only safe because
  these raw tables are reproducible from the cohort parquet. Set
  `set_logged = TRUE` to convert them back to durable LOGGED tables
  after the load.

- set_logged:

  If TRUE (and `unlogged = TRUE`), convert each table back to LOGGED
  after it loads successfully. This makes the data durable but rewrites
  the table, giving back some of the time saved. Default FALSE.

- synchronous_commit_off:

  If TRUE, set `synchronous_commit = off` for the load session (restored
  afterwards). Speeds bulk loading by not waiting for WAL fsync on each
  commit; on a crash the most recent commits can be lost but the
  database is not corrupted. Default FALSE.

## Value

Invisibly, a data frame with each table's row count and whether it was
loaded or skipped as already complete.

## Details

Tables are expected to already exist with explicit column types, created
by the schema DDL that
[`run_db()`](https://ate-driven-lab.github.io/rendehR/reference/run_db.md)
runs before this loader (`db_setup_sqls/schema`). When a table exists,
rows are appended into it with
[`DBI::dbAppendTable()`](https://dbi.r-dbi.org/reference/dbAppendTable.html)
so the DDL column types are preserved - the table is never dropped, and
types are not re-inferred from the parquet. If a table has no
pre-created DDL, the loader falls back to creating it from the parquet
(types inferred from the data).

Each table is loaded inside a single transaction: a failure part-way
through a table rolls that table back, so a table is never left
partially populated.

In resume mode (`overwrite = FALSE`), a table already confirmed loaded
by a prior call is recognized from a small `__ehr_load_manifest` marker
table (written right after that prior load verified its own row count)
instead of re-scanning the table with `count(*)` every call - a real
cost saver on a huge already-loaded table (e.g. chartevents). The marker
is only trusted when it still matches both the parquet's row count and
the cohort build that produced it (via `cohort_manifest.json`'s
`completed_at`); a rebuilt cohort, or a `parquet_dir` with no manifest,
transparently falls back to the exact `count(*)` check.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
load_cohort_parquet_to_postgres(con, "data/cohorts_data/mimic_iv_demo_parquet", "mimic_iv_demo_coh")
} # }
```

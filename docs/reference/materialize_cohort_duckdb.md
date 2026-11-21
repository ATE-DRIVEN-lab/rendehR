# Materialize the current cohort as a persistent DuckDB database

Uses the canonical selection, hierarchy, three-index, table-linkage, and
projection contracts. The complete database is built and validated in
same-filesystem staging before it atomically replaces the one current
database.

## Usage

``` r
materialize_cohort_duckdb(
  src_name = NULL,
  db_path = NULL,
  icd_filter_path = NULL,
  cohort_sql = NULL,
  setup_sql = NULL,
  cohort_fn = NULL,
  direct_ids_path = NULL,
  cohort = NULL,
  no_filter = FALSE,
  key = NULL,
  import_scope = NULL,
  allow_unmatched_ids = FALSE,
  window_sql = NULL,
  window_fn = NULL,
  schema = NULL,
  tables = NULL,
  memory_limit = NULL,
  threads = NULL,
  temp_directory = NULL,
  table_chunk_rows = 5e+06,
  validate_columns = FALSE,
  ignore_errors = TRUE,
  store_rejects = TRUE,
  use_parquet_cache = TRUE,
  base_dir = NULL,
  raw_data_root = NULL,
  overwrite = FALSE,
  show_progress = TRUE
)
```

## Arguments

- src_name:

  Registered source name.

- db_path:

  Destination DuckDB file. Defaults to
  [`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md)
  output.

- icd_filter_path:

  ICD-code CSV or RDS file with an `icd_code` column and an
  `icd_version` column (9 or 10). `icd_version` is required for MIMIC-IV
  and eICU; for MIMIC-III it may be omitted (ICD-9 is assumed) and
  ICD-10 rows are excluded. A code matches only in its stated version.

- cohort_sql:

  Final selection query; use `setup_sql` for dependencies.

- setup_sql:

  Optional ordered SQL statements or files.

- cohort_fn:

  Function or function name returning identifiers.

- direct_ids_path:

  CSV, CSV.GZ, Parquet, or RDS identifier file.

- cohort:

  Bundled named cohort.

- no_filter:

  Select the whole available source.

- key:

  Explicit configured hierarchy key.

- import_scope:

  Patient, hospital, ICU, or configured hierarchy key.

- allow_unmatched_ids:

  Permit a partially unmatched selection when at least one identifier
  matches.

- window_sql, window_fn:

  Optional hierarchy builder override.

- schema:

  Destination schema inside DuckDB.

- tables:

  Optional bounded table subset.

- memory_limit, threads, temp_directory:

  Optional DuckDB resource limits.

- table_chunk_rows:

  Row threshold for chunked table builds; see
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md).

- validate_columns:

  If TRUE, validate raw column types before building; see
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md).
  Off by default.

- ignore_errors, store_rejects:

  Raw CSV error tolerance and reject visibility; see
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md).

- use_parquet_cache:

  If TRUE (default), a table whose raw CSV has already been cached as
  parquet by
  [`cache_raw_parquet_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/cache_raw_parquet_duckdb.md)
  is read from that parquet instead of re-parsing CSV. Tables without a
  cached parquet still fall back to CSV. Set FALSE to always read CSV.

- base_dir:

  Project root.

- raw_data_root:

  Optional parent of the source-specific raw directory.

- overwrite:

  Safely replace the current completed artifact?

- show_progress:

  Print aggregate progress.

## Value

Invisibly returns the normalized plan, reports, status, path, and
`csv_reject_report`.

## Details

A separate, opt-in path from
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
(the canonical pipeline entry point): it skips Parquet and PostgreSQL
entirely for local, Postgres-free access to the raw cohort tables.
Concept extraction and harmonization are PostgreSQL-only and do not run
against this database - use
[`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md) +
[`run_db()`](https://ate-driven-lab.github.io/rendehR/reference/run_db.md)
instead if you need those.

When the cohort is selected by ICD code (a named ICD cohort such as
`"scid"`, or a raw `icd_filter_path`), the exact code list that defined
the cohort is also published as a reference table, `d_<name>_icds` (e.g.
`d_scid_icds` for the bundled SCI cohort; a name derived from the file's
basename for a raw `icd_filter_path`). SQL-based cohorts (e.g.
`"sepsis3"`) have no ICD list and get no such table.

## Examples

``` r
if (FALSE) { # \dontrun{
materialize_cohort_duckdb("mimic_iv_demo", db_path = "data/duckdb/mimic_iv_demo.duckdb",
                          cohort = "scid")
} # }
```

# Materialize the current cohort as canonical Parquet

Resolves every supported selection route through the shared DuckDB
engine, constructs the source-native patient hierarchy with
`patient_context_index`, `definition_filter_index`, and `scope_index`,
and writes canonical Parquet directly. A transient DuckDB working file
may be used while building but is removed before publication.

## Usage

``` r
import_cohort_general(
  src_name = NULL,
  base_dir = NULL,
  raw_data_root = NULL,
  icd_filter_path = NULL,
  direct_ids_path = NULL,
  cohort = NULL,
  cohort_sql = NULL,
  setup_sql = NULL,
  cohort_fn = NULL,
  no_filter = FALSE,
  key = NULL,
  import_scope = NULL,
  allow_unmatched_ids = FALSE,
  window_sql = NULL,
  window_fn = NULL,
  tables = NULL,
  memory_limit = NULL,
  threads = NULL,
  temp_directory = NULL,
  table_chunk_rows = 5e+06,
  validate_columns = FALSE,
  ignore_errors = TRUE,
  store_rejects = TRUE,
  use_parquet_cache = TRUE,
  overwrite = FALSE,
  show_progress = TRUE,
  cohort_selection = NULL,
  windows = NULL,
  loading = NULL,
  execution = NULL
)
```

## Arguments

- src_name:

  Registered source name.

- base_dir:

  Project root.

- raw_data_root:

  Optional parent of the source-specific raw directory.

- icd_filter_path:

  ICD-code CSV or RDS file with an `icd_code` column and an
  `icd_version` column (9 or 10). `icd_version` is required for MIMIC-IV
  and eICU; for MIMIC-III it may be omitted (ICD-9 is assumed) and
  ICD-10 rows are excluded. A code matches only in its stated version.

- direct_ids_path:

  CSV, CSV.GZ, Parquet, or RDS identifier file.

- cohort:

  Bundled named cohort.

- cohort_sql:

  Final selection query; use `setup_sql` for dependencies.

- setup_sql:

  Optional ordered SQL statements or files.

- cohort_fn:

  Function or function name returning identifiers.

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

- tables:

  Optional bounded table subset.

- memory_limit, threads, temp_directory:

  Optional DuckDB resource limits.

- table_chunk_rows:

  Row threshold (against each table's configured `num_rows`, no extra
  scan needed). Tables above it are built in multiple smaller `INSERT`
  batches (split by a hash of the cohort link-key column) instead of one
  single `CREATE TABLE AS SELECT`, so peak memory stays bounded even
  when DuckDB's own out-of-core spilling isn't enough for a single huge
  table (e.g. MIMIC-III `chartevents`). Default `5e6` chunks only the
  few tables large enough to need it; small tables are unaffected either
  way. Pass `NULL` to always build every table in one shot. Reference
  (unfiltered) tables are never chunked.

- validate_columns:

  If TRUE, scan every configured column of every built table and count
  values that fail to convert (`TRY_CAST`) to their configured type,
  stopping the build if a required patient/visit linkage column has any
  failures or is missing raw data. Off by default (`FALSE`)

  - this scan touches the full raw data of every table (including huge
    ones like `chartevents`) and is expensive; skipping it trusts that
    the source's linkage columns are already well-formed (a reasonable
    default for a vetted source like published MIMIC data, less so for a
    new, unvetted institutional extract). Reference tables are never
    checked either way.

- ignore_errors:

  If TRUE (default), a raw CSV row that fails to parse (beyond what
  `null_padding`/lenient parsing already tolerates) is skipped rather
  than aborting the whole table's read - avoids losing an entire large
  import to one bad row. Combined with `store_rejects`, that row is
  still recorded, not silently dropped. Set FALSE to fail hard on any
  malformed row instead.

- store_rejects:

  If TRUE (default), skipped rows (when `ignore_errors = TRUE`) are
  recorded - which file/line, which column, why - in the returned
  `csv_reject_report` instead of vanishing with no trace. Has no effect
  when nothing gets skipped.

- use_parquet_cache:

  If TRUE (default), a table whose raw CSV has already been cached as
  parquet by
  [`cache_raw_parquet_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/cache_raw_parquet_duckdb.md)
  is read from that parquet instead of re-parsing CSV. Tables without a
  cached parquet still fall back to CSV. Set FALSE to always read CSV.

- overwrite:

  Safely replace the one current completed Parquet cohort?

- show_progress:

  Print aggregate progress.

- cohort_selection:

  Optional overrides from
  [`cohort_selection_options()`](https://ate-driven-lab.github.io/rendehR/reference/cohort_selection_options.md)
  or a named list.

- windows:

  Optional overrides from
  [`window_options()`](https://ate-driven-lab.github.io/rendehR/reference/window_options.md)
  or a named list.

- loading:

  Optional overrides from
  [`loading_options()`](https://ate-driven-lab.github.io/rendehR/reference/loading_options.md)
  or a named list.

- execution:

  Optional overrides from
  [`execution_options()`](https://ate-driven-lab.github.io/rendehR/reference/execution_options.md)
  or a named list.

## Value

Invisibly returns the normalized plan, reports, status, path, and
`csv_reject_report` (a data frame, one row per rejected raw CSV row,
empty if none).

## Details

When the cohort is selected by ICD code (a named ICD cohort such as
`"scid"`, or a raw `icd_filter_path`), the exact code list that defined
the cohort is also published as a reference table, `d_<name>_icds` (e.g.
`d_scid_icds` for the bundled SCI cohort; a name derived from the file's
basename for a raw `icd_filter_path`). SQL-based cohorts (e.g.
`"sepsis3"`) have no ICD list and get no such table.

## Examples

``` r
if (FALSE) { # \dontrun{
import_cohort_general("mimic_iv_demo",
  icd_filter_path = "config/my_icd_codes.csv")
} # }
```

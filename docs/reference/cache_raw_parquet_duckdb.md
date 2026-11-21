# Materialize a source's raw CSVs as a parquet cache for fast cohort selection

Converts each configured table's raw CSV(.gz) file(s) into a single
parquet file under `paths$raw_parquet_cache_dir`, using the exact same
typed read (`.duckdb_csv_read_expr()` + package schema TRY_CAST) that
`.ehr_select_cohort_ids_duckdb()` otherwise applies to CSV on every
call - so the cache is a transparent, drop-in stand-in for the CSV: same
column names, same types. Original CSV column names are kept.

## Usage

``` r
cache_raw_parquet_duckdb(
  src_name,
  tables = NULL,
  cohort = NULL,
  all_tables = TRUE,
  base_dir = NULL,
  raw_data_root = NULL,
  overwrite = FALSE,
  keep = FALSE,
  validate_columns = TRUE,
  memory_limit = NULL,
  threads = NULL,
  temp_directory = NULL,
  show_progress = TRUE,
  cohort_selection = NULL,
  loading = NULL,
  execution = NULL
)
```

## Arguments

- src_name:

  Source name (e.g. `"mimic_iii"`).

- tables:

  Character vector of table names to cache. Default NULL, in which case
  `all_tables`/`cohort` decide what gets cached (see below) - pass this
  explicitly to scope the cache down to just the tables your
  `cohort_sql`/`setup_sql` actually queries, skipping large unused
  tables (e.g. free-text tables) that `all_tables` would otherwise
  include.

- cohort:

  Optional name of a bundled example cohort (see
  [`list_example_cohorts()`](https://ate-driven-lab.github.io/rendehR/reference/list_example_cohorts.md),
  e.g. `"sepsis3"` or `"scid"`)

  - resolves `tables` automatically instead of you having to name them:
    the tables its packaged SQL chain references (SQL cohorts) or its
    configured `cohort_seed_tables` (ICD cohorts). Ignored when `tables`
    is supplied explicitly; otherwise takes precedence over `all_tables`
    (an explicit cohort is more specific than the "cache everything"
    default).

- all_tables:

  If TRUE (the default), cache every table configured for this source.
  Ignored whenever `tables` or `cohort` apply - pass
  `all_tables = FALSE` together with neither of those to get the old
  no-op ("nothing to cache") behavior instead.

- base_dir:

  Project root directory.

- raw_data_root:

  Optional parent storage-root override for raw source data (as in
  [`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md)).

- overwrite:

  Re-cache tables that already have a non-empty cached parquet. Default
  FALSE (skip tables already cached).

- keep:

  Keep the cache after this R session ends. Default FALSE: the cache
  directory is deleted automatically on session exit, so it is scratch
  space for this session unless you opt in to persisting it.

- validate_columns:

  If TRUE (the default), check every raw value against its configured
  type before writing the cache and record conversion-failure counts
  (values that fail `TRY_CAST` become a silent `NULL` otherwise, with no
  way to later tell "genuinely blank" from "failed to parse" - the
  original raw text is gone once cached). Writes a per-column report to
  `{cache_dir}/_conversion_report.csv` and warns if any column had
  failures. Costs a second full scan per table (validate, then
  convert-and-write) - set FALSE to skip it and cache at the old,
  unvalidated speed.

- memory_limit, threads, temp_directory:

  Optional DuckDB resource settings for large sources, as in
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
  and
  [`materialize_cohort_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/materialize_cohort_duckdb.md).
  `temp_directory` lets DuckDB spill to disk instead of running out of
  memory on a large table - a scratch spill directory is always used
  even when this is left NULL (removed once the function returns), since
  the underlying connection is `:memory:` and otherwise has nowhere to
  spill at all; pass this explicitly (e.g.
  `temp_directory = "/scratch"`) only to point spilling at a specific
  disk (more free space, faster I/O) instead of the default scratch
  location.

- show_progress:

  Print per-table progress messages.

- cohort_selection:

  Optional overrides from
  [`cohort_selection_options()`](https://ate-driven-lab.github.io/rendehR/reference/cohort_selection_options.md)
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

Invisibly, the cache directory path, or NULL if `tables` was NULL,
`cohort` was NULL, and `all_tables` was FALSE (nothing to do).

## Details

The cache is written inside the project, under
`<base_dir>/data/raw_parquet_cache/<src_name>` (set
`paths.raw_parquet_cache_root` in the project config to move it), never
into the raw source directory, which is treated as read-only.

Run this once per source (or after a source is re-downloaded); every
subsequent `select_cohort_ids_duckdb()` /
[`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
call against tables with a cache automatically reads parquet instead of
re-parsing CSV (see `use_parquet_cache` on
`.ehr_select_cohort_ids_duckdb()`).

By default (`keep = FALSE`) the cache is scratch space: it is removed
automatically when the R session ends, so repeated selection calls
within one session/script are fast without leaving parquet files behind
afterward. Pass `keep = TRUE` to have it persist across sessions - to
remove a kept cache later, just
`unlink(ehr_paths(src_name)$raw_parquet_cache_dir, recursive = TRUE)`.

Not run automatically by
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
(the canonical pipeline entry point) - call it directly beforehand, or
request the `"cache_raw_parquet"` stage via
`run_one_source(stages = ...)`.

## Examples

``` r
if (FALSE) { # \dontrun{
cache_raw_parquet_duckdb("mimic_iv_demo", tables = c("labevents", "chartevents"))
} # }
```

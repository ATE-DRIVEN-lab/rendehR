# Run the Sepsis-3 reference pipeline and verify output

Runs the full pipeline for the Sepsis-3 cohort: selects and imports it
via
[`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md),
then runs every remaining stage of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
(skipping only its own `import_cohort` stage, already done). Works for
any source family with a packaged chain (`mimic_iii`, `mimic_iv`,
`eicu`). Useful for verifying the package's DuckDB-backed
`cohort_sql`/`setup_sql` cohort-selection route works correctly after
installation or changes.

## Usage

``` r
run_sepsis3_example(
  db_name = NULL,
  base_dir = NULL,
  src_name = "mimic_iii_demo",
  raw_data_root = NULL,
  overwrite = FALSE,
  bundle = "sepsis3",
  import_scope = "hospital",
  memory_limit = NULL,
  threads = NULL,
  show_progress = TRUE
)
```

## Arguments

- db_name:

  Optional Postgres database to write to. If NULL, uses the active
  project config, then "rendehr".

- base_dir:

  Project root with the source's raw demo data.

- src_name:

  Source to run. Default "mimic_iii_demo".

- raw_data_root:

  Optional parent storage-root override for raw source data, forwarded
  to both
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md)'s
  selection step and
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)'s
  remaining stages.

- overwrite:

  If TRUE, overwrite existing cohort/database output for this source -
  forwarded to both
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md)
  and
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)'s
  remaining stages. Default FALSE, matching
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)'s
  own default; needed for re-running the example against a source
  already imported/loaded.

- bundle:

  Forwarded to
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md).
  Default `"sepsis3"` - see
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md)'s
  own `bundle` doc for the `"first_sepsis3"` (first-ICU-stay-only)
  alternative.

- import_scope:

  Forwarded to
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md).
  Default `"hospital"` - see
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md)'s
  own `import_scope` doc for why this differs from the engine's general
  `NULL` (full patient-history) default.

- memory_limit, threads:

  Optional DuckDB resource settings forwarded to
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md)'s
  cohort-selection step - useful for a full (non-demo) source on a
  memory-constrained machine, where scanning a full CHARTEVENTS-sized
  table can exceed DuckDB's default memory cap.

- show_progress:

  Forwarded to
  [`build_sepsis_cohort()`](https://ate-driven-lab.github.io/rendehR/reference/build_sepsis_cohort.md) -
  prints which `setup_sql` file is running, with elapsed seconds.

## Value

Invisibly returns NULL.

## Details

Requires the source's raw demo data under the project's raw data root.
The database is the project config's `db_name` (or `db_name`), not the
target of
[`ehr_connect()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_connect.md).

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("sepsis3_example", creds)
run_sepsis3_example(base_dir = "/path/to/project")
run_sepsis3_example(src_name = "eicu_demo", base_dir = "/path/to/project")
run_sepsis3_example(base_dir = "/path/to/project", bundle = "first_sepsis3")
} # }
```

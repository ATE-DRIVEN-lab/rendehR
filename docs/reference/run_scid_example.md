# Run the SCI reference pipeline and verify output

Runs the full SCI pipeline on mimic_iv_demo using the bundled SCI ICD
files as the reference implementation. Useful for verifying the package
works correctly after installation or changes.

## Usage

``` r
run_scid_example(
  db_name = NULL,
  base_dir = NULL,
  src_name = "mimic_iv_demo",
  raw_data_root = NULL,
  overwrite = FALSE,
  import_scope = NULL
)
```

## Arguments

- db_name:

  Optional Postgres database to write to. If NULL, uses the active
  project config, then "rendehr".

- base_dir:

  Project root with MIMIC-IV demo parquet data.

- src_name:

  Source to run. Default "mimic_iv_demo".

- raw_data_root:

  Optional parent storage-root override for raw source data. The source
  directory is derived as `{raw_data_root}/{src_name}`.

- overwrite:

  Passed to
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md).
  Default FALSE (resume/skip mode). Set TRUE to force a full rebuild of
  the scid example.

- import_scope:

  Passed to
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
  (and on to
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)).
  Default NULL: full patient history, every hospitalization a matched
  patient ever had. Pass `"hospital"` to widen only to other ICU stays
  within the same hospitalization, or `"icu"` to narrow to just the
  matched ICU stays themselves. Changing this always requires a full
  rebuild (`overwrite = TRUE`).

## Value

Invisibly returns a list with cohort_info, heart_rate, and flat_tab.

## Details

Requires the MIMIC-IV demo raw data under the project's raw data root.
The database is the project config's `db_name` (or `db_name`), not the
target of
[`ehr_connect()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_connect.md).

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("scid_example", creds)
run_scid_example(base_dir = "/path/to/project")
} # }
```

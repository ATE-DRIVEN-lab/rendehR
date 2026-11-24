# Select and normalize cohort identifiers

Resolves exactly one direct-ID, SQL, R-function, ICD, named-cohort, or
no-filter request through the canonical DuckDB selection engine.
Returned identifiers are character-normalized and validated against the
configured source hierarchy. Diagnostics report counts only and never
print values.

## Usage

``` r
select_cohort_ids(
  src_name,
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
  out_csv = NULL,
  base_dir = NULL,
  raw_data_root = NULL,
  window_sql = NULL,
  window_fn = NULL
)
```

## Arguments

- src_name:

  Registered source name.

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

- out_csv:

  Optional atomic one-column identifier export.

- base_dir:

  Project root.

- raw_data_root:

  Optional parent of the source-specific raw directory.

- window_sql, window_fn:

  Optional hierarchy builder override.

## Value

An object of class `ehr_selection_plan`. Printing it shows the route,
keys, scope and counts; the identifiers themselves are in `$values`.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- select_cohort_ids("mimic_iv_demo", cohort = "scid")
plan               # route, keys, scope and counts
head(plan$values)  # the selected identifiers
} # }
```

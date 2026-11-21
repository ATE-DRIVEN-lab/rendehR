# Build generalized cohort_info in a DuckDB database

DuckDB wrapper around the same generalized cohort-info core used by
[`build_cohort_info_general()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_info_general.md).
Run this after
[`materialize_cohort_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/materialize_cohort_duckdb.md)
or `run_db(engine = "duckdb")` has materialized the raw cohort schema
and the generalized window tables in the DuckDB file.

## Usage

``` r
build_cohort_info_general_duckdb(
  src_name,
  db_path = NULL,
  base_dir = NULL,
  icd_filter_path = NULL,
  classify_fn = NULL,
  classify_args = list(),
  analysis_level = c("admission", "icu")
)
```

## Arguments

- src_name:

  Source name.

- db_path:

  Path to the DuckDB database. Defaults to `paths$duckdb_path`.

- base_dir:

  Project root directory.

- icd_filter_path:

  Optional ICD filter used only to populate ICD-derived enrichment
  inputs. Not defaulted from the project config here; `NULL` means no
  ICDs are treated as cohort-defining.

- classify_fn:

  Optional enrichment function passed through to the source cohort_info
  builder. Not defaulted to
  [`scid_classify()`](https://ate-driven-lab.github.io/rendehR/reference/scid_classify.md)
  here.

- classify_args:

  Extra arguments for `classify_fn`.

- analysis_level:

  Grain of the composite `ehr_id` key.

## Value

The cohort_info data frame, invisibly. It is also written to the
source's raw schema.

## Details

Unlike
[`build_cohort_info_general()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_info_general.md),
this variant applies no cohort-profile defaults: SCI enrichment is
Postgres-only, so a `scid` project run through this function produces
generic `cohort_info` unless `classify_fn` is supplied explicitly.

## Examples

``` r
if (FALSE) { # \dontrun{
build_cohort_info_general_duckdb("mimic_iv_demo", db_path = "data/duckdb/mimic_iv_demo.duckdb")
} # }
```

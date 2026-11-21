# Specify cohort selection options

Configure cohort membership and the scope of imported source data.

## Usage

``` r
cohort_selection_options(
  icd_filter_path = NULL,
  direct_ids_path = NULL,
  cohort = NULL,
  tables = NULL,
  cohort_sql = NULL,
  setup_sql = NULL,
  cohort_fn = NULL,
  no_filter = FALSE,
  key = NULL,
  import_scope = NULL,
  allow_unmatched_ids = FALSE
)
```

## Arguments

- icd_filter_path:

  Character string giving an ICD-code CSV or RDS file used to select
  cohort members. If no selection method is supplied, the importer uses
  the cohort definition in the project configuration.

- direct_ids_path:

  Character string giving a CSV, compressed CSV, Parquet, or RDS file of
  cohort identifiers. Use `key` to specify the identifier column when it
  cannot be inferred.

- cohort:

  Character string naming a bundled cohort, such as `"scid"` or
  `"sepsis3"`, or `NULL`. See
  [`list_example_cohorts()`](https://ate-driven-lab.github.io/rendehR/reference/list_example_cohorts.md)
  for available definitions.

- tables:

  Character vector of source table names to import or cache. `NULL` uses
  the tables selected by the receiving function. Table selection does
  not change cohort membership.

- cohort_sql:

  Character string containing a SQL selection query or the path to a SQL
  file. The result must contain the identifier column specified by
  `key`.

- setup_sql:

  Character vector of SQL statements or SQL-file paths to execute, in
  order, before `cohort_sql`. Default: `NULL`.

- cohort_fn:

  Function, or character string naming a function, called as
  `cohort_fn(paths)`. It must return an identifier vector or a data
  frame containing the column specified by `key`. Default: `NULL`.

- no_filter:

  Logical. If `TRUE`, include the available source population without
  applying a cohort selection method. Default: `FALSE`.

- key:

  Character string naming an identifier in the configured source
  hierarchy, such as `"subject_id"`, `"hadm_id"`, or `"stay_id"`. `NULL`
  lets the selection method determine the key.

- import_scope:

  Character string selecting patient, hospital, ICU, or a configured
  hierarchy key as the import scope. `NULL` lets the importer resolve
  the scope from the selection.

- allow_unmatched_ids:

  Logical. If `TRUE`, allow some requested identifiers to be absent from
  the source, provided at least one matches. Default: `FALSE`.

## Value

A named list of the supplied options, suitable for the
`cohort_selection` argument of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
and compatible direct stage functions.

## Details

Supply one selection method: `cohort`, `icd_filter_path`,
`direct_ids_path`, `cohort_sql`, or `cohort_fn`. Alternatively, set
`no_filter = TRUE`. The importer rejects competing selection methods.

Only explicitly supplied arguments are included in the result, including
explicit `NULL` values. Omitted settings use the defaults of the
receiving function. An empty options list therefore leaves its defaults
unchanged.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)

## Examples

``` r
cohort_selection_options(direct_ids_path = "ids.csv", key = "subject_id")
#> $direct_ids_path
#> [1] "ids.csv"
#> 
#> $key
#> [1] "subject_id"
#> 
cohort_selection_options(cohort = "sepsis3")
#> $cohort
#> [1] "sepsis3"
#> 
```

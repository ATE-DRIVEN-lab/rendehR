# Load the merged flat table from any merged schema

Load the merged flat table from any merged schema

## Usage

``` r
load_merged_flat(
  schema,
  n = Inf,
  con = NULL,
  cohort_only = TRUE,
  case_adm_only,
  scope_only
)
```

## Arguments

- schema:

  Target schema name passed to merge_sources().

- n:

  Max rows. Default Inf.

- con:

  Optional explicit DBI connection.

- cohort_only:

  If TRUE (default), return only rows of cohort members (`cohort_index`
  in the merged `cohort_info`), matched at admission grain on
  `(source_data, src_admission_id)`; flat data do not become
  ICU-specific through filtering. FALSE reads all rows.

- case_adm_only, scope_only:

  Removed; supplying either is an error. Use `cohort_only`.

## Value

A data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
flat <- load_merged_flat("rendehr")
flat <- load_merged_flat("trauma_project", cohort_only = FALSE)
} # }
```

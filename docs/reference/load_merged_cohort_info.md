# Load the merged cohort_info table from any merged schema

Load the merged cohort_info table from any merged schema

## Usage

``` r
load_merged_cohort_info(
  schema,
  n = Inf,
  src = NULL,
  cohort_only = TRUE,
  con = NULL,
  case_adm_only,
  scope_only
)
```

## Arguments

- schema:

  Target schema name passed to merge_sources().

- n:

  Max rows. Default Inf.

- src:

  Optional character vector to filter by source_data column.

- cohort_only:

  If TRUE (default), return only rows of cohort members
  (`cohort_index`). FALSE also returns imported patient-context rows.

- con:

  Optional explicit DBI connection.

- case_adm_only, scope_only:

  Removed; supplying either is an error. Use `cohort_only`.

## Value

A data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
ci <- load_merged_cohort_info("rendehr")
ci <- load_merged_cohort_info("trauma_project", cohort_only = FALSE)
} # }
```

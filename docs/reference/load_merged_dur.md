# Load the merged duration table from any merged schema

The interval-valued counterpart of
[`load_merged_ts()`](https://ate-driven-lab.github.io/rendehR/reference/load_merged_ts.md):
reads the pooled `dur` table built by `merge_sources(merge_dur = TRUE)`
(medication exposures, ventilation episodes, and similar
start/end-valued concepts) instead of point-in-time measurements.

## Usage

``` r
load_merged_dur(
  schema,
  n = Inf,
  concepts = NULL,
  cohort_only = TRUE,
  con = NULL,
  case_adm_only,
  scope_only
)
```

## Arguments

- schema:

  Target schema name passed to
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
  (e.g. `"rendehr"`, `"trauma_project"`).

- n:

  Max rows. Default Inf.

- concepts:

  Optional character vector to filter the `concept_name` column.

- cohort_only:

  If TRUE (default), return only rows of cohort members (`cohort_index`
  in the merged `cohort_info`). `dur` has no ICU projection, so the
  match is at admission grain on `(source_data, src_admission_id)`:
  filtering does not make duration data ICU-specific. Filter runs in
  SQL. FALSE reads all imported context rows; it cannot recover data
  that was never imported.

- con:

  Optional explicit DBI connection.

- case_adm_only, scope_only:

  Removed; supplying either is an error. Use `cohort_only`.

## Value

A data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
dur <- load_merged_dur("rendehr")
dur <- load_merged_dur("rendehr", concepts = c("norepinephrine_exp", "propofol_exp"))
dur <- load_merged_dur("rendehr", cohort_only = FALSE)
} # }
```

# Load the merged timeseries table from any merged schema

The merged schema is created by merge_sources() and contains ts, flat,
and cohort_info tables pooled across all sources. Requires that the
merge step has already been run.

## Usage

``` r
load_merged_ts(
  schema,
  n = Inf,
  concepts = NULL,
  cols = c("all", "minimal"),
  cohort_only = TRUE,
  window_min = NULL,
  window_ref = c("admission", "icu"),
  con = NULL,
  case_adm_only,
  scope_only
)
```

## Arguments

- schema:

  Target schema name passed to merge_sources() (e.g. "rendehr",
  "trauma_project").

- n:

  Max rows. Default Inf.

- concepts:

  Optional character vector to filter concept_name column.

- cols:

  Either "all" (default, every column) or "minimal", which keeps only
  the analysis essentials: ehr_id, source_data, the grain key
  (src_admission_id for an admission-level merge, src_icu_id for an
  ICU-level merge), concept_name, value, unit, and both time axes.
  Projection happens in SQL, so "minimal" also reduces transfer for
  large pulls.

- cohort_only:

  If TRUE (default), return only rows of cohort members (`cohort_index`
  in the merged `cohort_info`), matched at the merge grain:
  `(source_data, src_admission_id)` for an admission-level merge, plus
  `assigned_icu_id` for an ICU-level merge (which needs `cohort_info`
  built with `analysis_level = "icu"`). Filter runs in SQL. FALSE reads
  all imported context rows; it cannot recover data that was never
  imported.

- window_min:

  Optional `c(from, to)` in minutes, keeping rows with
  `from <= time < to` on the clock chosen by `window_ref` (applied in
  SQL). E.g. `c(0, 1440)` keeps the first 24 h. Rows with a missing time
  are dropped when a window is set. `NULL` (default) keeps all times.
  Unlike the bin-index `window` of
  [`load_merged_ts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/load_merged_ts_wide.md),
  this is in minutes.

- window_ref:

  Clock for `window_min`: `"admission"` (default) uses
  `time_since_hadm_min`, `"icu"` uses `time_since_icu_min`.

- con:

  Optional explicit DBI connection.

- case_adm_only, scope_only:

  Removed; supplying either is an error. Use `cohort_only`.

## Value

A data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
ts <- load_merged_ts("rendehr")
ts <- load_merged_ts("trauma_project", concepts = c("heart_rate", "sbp"))
ts <- load_merged_ts("rendehr", cols = "minimal")
ts <- load_merged_ts("rendehr", concepts = c("heart_rate", "map", "sofa"),
                     cohort_only = FALSE)

# First 24 h after hospital admission, filtered server-side
ts <- load_merged_ts("rendehr", concepts = c("lactate", "creatinine"),
                     window_min = c(0, 1440))
} # }
```

# Pivot the merged time series into wide format (one column per concept)

The merged-schema counterpart of
[`fetch_concepts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/fetch_concepts_wide.md):
reads the pooled `ts` table built by
[`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
and pivots it to wide format in SQL, keyed by `ehr_id`, `source_data`,
`src_patient_id`, and the requested analysis grain. Use this when you
want a cross-source, `ehr_id`-keyed wide matrix; use
[`fetch_concepts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/fetch_concepts_wide.md)
to pivot a single source's `ts_tab` before merging.

## Usage

``` r
load_merged_ts_wide(
  schema,
  spec,
  bin_min = 60,
  analysis_level = c("admission", "icu"),
  window = NULL,
  complete_grid = FALSE,
  n = Inf,
  cohort_only = TRUE,
  clean = c("none", "range"),
  ranges = NULL,
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

- spec:

  Named list mapping each concept to a character vector of aggregates.
  Supported: `"min"`, `"max"`, `"mean"`, `"median"`, `"sd"`,
  `"variance"`, `"sum"`, `"n"`/`"count"`, `"first"`, and `"last"`;
  `"all_numeric"` expands to all numeric reducers. An unnamed character
  vector defaults each concept to `"mean"`; a named character vector is
  interpreted as concept = aggregate shorthand.

- bin_min:

  Bin width in minutes (default 60). `NULL` pivots on the raw timestamp.
  The emitted `t_bin` column is the 0-based bin index
  (`floor(time / bin_min)`), e.g. `0, 1, 2, ...` for hourly bins.

- analysis_level:

  `"admission"` (default) bins on `time_since_hadm_min`; `"icu"` bins on
  `time_since_icu_min`.

- window:

  Optional `c(from_bin, to_bin)` in bin-index units, inclusive both
  ends, restricting the output to those bins (applied in SQL before
  aggregation). E.g. with `bin_min = 60`, `c(0, 47)` keeps the first 48
  h, `c(0, 0)` keeps only the first hour. `NULL` (default) keeps all
  bins.

- complete_grid:

  If TRUE, emit a row for every bin in `window` (or, when `window` is
  NULL, the observed bin range) for each stay present in `ts`,
  left-joining the aggregates onto the full grid so unmeasured bins come
  back as `NA`. No imputation is performed - fill/mask downstream in R.
  Note this regularizes shape only; it does NOT enforce length-of-stay
  eligibility, so still filter `icu_los` from `flat` to define the
  cohort. Requires a non-NULL `bin_min`. Default FALSE.

- n:

  Max rows to return. Default Inf.

- cohort_only:

  If TRUE (default), pivot only rows of cohort members (`cohort_index`
  in the merged `cohort_info`), matched at the requested
  `analysis_level`: `(source_data, src_admission_id)` for `"admission"`,
  plus `assigned_icu_id` for `"icu"` (which needs `cohort_info` built
  with `analysis_level = "icu"`). Applied in SQL before aggregation.
  FALSE reads all imported context rows; it cannot recover data that was
  never imported.

- clean, ranges:

  Optional range cleaning before aggregation, as in
  [`fetch_concepts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/fetch_concepts_wide.md):
  `clean = "range"` excludes readings outside the config (or `ranges`)
  bounds inside the SQL aggregates, adds a `<concept>_n_cleaned` column
  per concept and a `clean_report` attribute. Default `"none"`.

- con:

  Optional explicit DBI connection.

- case_adm_only, scope_only:

  Removed; supplying either is an error. Use `cohort_only`.

## Value

A wide data frame: `ehr_id`, `source_data`, `src_patient_id`, the merge
grain key (`src_admission_id`, plus `assigned_icu_id` for ICU-level
analysis), the `t_bin` index column, then one column per concept and
aggregate, named `<concept>_<aggregate>`, or just `<concept>` when the
concept has one aggregate (followed by `<concept>_n_cleaned` with
`clean = "range"`).

## Details

Shares the conditional-aggregation engine, binning, per-concept spec,
and column-naming rules with
[`fetch_concepts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/fetch_concepts_wide.md) -
see there for details.

## Examples

``` r
if (FALSE) { # \dontrun{
load_merged_ts_wide("rendehr", spec = list(
  hr = c("mean", "max"), sbp = "mean", gcs = "last"
), bin_min = 60)
load_merged_ts_wide("rendehr", spec = list(hr = "mean"), cohort_only = FALSE)

# First 48 h on a complete hourly grid (NA where unmeasured), ICU clock
load_merged_ts_wide("rendehr", spec = list(hr = "last", sbp = "last"),
  bin_min = 60, analysis_level = "icu",
  window = c(0, 47), complete_grid = TRUE)
} # }
```

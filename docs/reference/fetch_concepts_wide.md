# Pivot the harmonized time series into wide format (one column per concept)

Reads the harmonized long table (`ts_tab` for admission grain, or the
`ts_icu` projection for ICU grain) and pivots it to wide format entirely
in SQL via conditional aggregation - the long table is never pulled into
R.

## Usage

``` r
fetch_concepts_wide(
  src_name,
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

- src_name:

  Source name (e.g. `"mimic_iv_demo"`).

- spec:

  Named list mapping each concept to a character vector of aggregates.
  Supported: `"min"`, `"max"`, `"mean"`, `"median"`, `"sd"`,
  `"variance"`, `"sum"`, `"n"`/`"count"`, `"first"`, and `"last"`;
  `"all_numeric"` expands to all numeric reducers. An unnamed character
  vector defaults each concept to `"mean"`; a named character vector is
  interpreted as concept = aggregate shorthand. Example:
  `list(hr = c("mean", "max"), sbp = "mean", gcs = "last", urine = "sum")`.

- bin_min:

  Bin width in minutes (default 60). `NULL` disables binning and pivots
  on the raw timestamp. The emitted `t_bin` column is the 0-based bin
  index (`floor(time / bin_min)`), e.g. `0, 1, 2, ...` for hourly bins.

- analysis_level:

  `"admission"` (default) pivots `ts_tab` on `time_since_hadm_min`;
  `"icu"` pivots `ts_icu` on `time_since_icu_min`, keyed on
  `assigned_icustay_id` so each ICU stay is a separate observation on
  its own ICU clock.

- window:

  Optional `c(from_bin, to_bin)` in bin-index units, inclusive both
  ends, restricting the output to those bins (applied in SQL before
  aggregation). E.g. with `bin_min = 60`, `c(0, 47)` keeps the first 48
  h, `c(0, 0)` keeps only the first hour. `NULL` (default) keeps all
  bins.

- complete_grid:

  If TRUE, emit a row for every bin in `window` (or, when `window` is
  NULL, the observed bin range) for each stay present in the long table,
  left-joining the aggregates onto the full grid so unmeasured bins come
  back as `NA` (no imputation - fill/mask downstream). Regularizes shape
  only, not length-of-stay eligibility. Requires non-NULL `bin_min`.
  Default FALSE.

- n:

  Max rows to return. Default Inf.

- cohort_only:

  If TRUE (default), pivot only rows of cohort members (`cohort_index`
  in this source's `cohort_info`), matched at the requested
  `analysis_level`: on the admission id for `"admission"`, on the
  assigned ICU stay for `"icu"` (which needs `cohort_info` built with
  `analysis_level = "icu"`). Applied in SQL before aggregation. FALSE
  reads all imported context rows; it cannot recover data that was never
  imported.

- clean:

  `"none"` (default) pivots the values as stored. `"range"` excludes
  readings outside each concept's physiologic `[min, max]` (both ends
  inclusive) in SQL, before aggregation, using the same bounds as
  [`clean_by_range()`](https://ate-driven-lab.github.io/rendehR/reference/clean_by_range.md):
  the concept config's widest window across sources, matched on the
  exact concept name. Every aggregate is then computed over valid
  readings only (`first`/`last`/`n` are the first/last/number of valid
  readings), each concept gets a `<concept>_n_cleaned` column counting
  the readings excluded in that bin (0 for concepts with no bound, which
  are left unchanged with a message), and the result carries a
  `clean_report` attribute per concept: `n`, `n_cleaned`,
  `n_not_numeric` (missing or non-numeric values), `min_value`,
  `max_value`. A bin whose readings were all excluded is `NA`;
  `_n_cleaned > 0` tells it apart from a bin with no measurement. With
  `complete_grid`, unmeasured grid bins have `NA` there too.

- ranges:

  Optional bounds overriding the config, in the formats accepted by
  [`clean_by_range()`](https://ate-driven-lab.github.io/rendehR/reference/clean_by_range.md):
  a named list of `c(min, max)` or a data frame with `concept_name`,
  `min_value`, `max_value`. Requires `clean = "range"`.

- con:

  Optional explicit DBI connection.

- case_adm_only, scope_only:

  Removed; supplying either is an error. Use `cohort_only`.

## Value

A wide data frame: id columns, the time-bin column (`t_bin`, or the raw
time column when `bin_min = NULL`), then one column per concept and
aggregate, named `<concept>_<aggregate>`, or just `<concept>` when the
concept has one aggregate (followed by `<concept>_n_cleaned` with
`clean = "range"`).

## Details

Time is binned by integer arithmetic on the minute axis
(`floor(time / bin_min) * bin_min`), so multiple measurements of a
concept that fall in the same bin are reconciled by the aggregate(s) you
specify per concept. Set `bin_min = NULL` for a raw-timestamp pivot (no
binning); ties at the exact same timestamp then collapse with the chosen
aggregate.

Column naming: a concept with a single aggregate keeps its plain name
(`sbp`); a concept with several aggregates is suffixed (`hr_mean`,
`hr_max`).

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_concepts_wide("mimic_iv_demo", spec = list(
  hr    = c("mean", "max"),
  sbp   = "mean",
  gcs   = "last",
  urine = "sum"
), bin_min = 60)

# Raw-timestamp pivot, ICU grain
fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"),
                    bin_min = NULL, analysis_level = "icu")

# All imported context rows, not only cohort members
fetch_concepts_wide("mimic_iv_demo", c(heart_rate = "mean"),
                    cohort_only = FALSE)
} # }
```

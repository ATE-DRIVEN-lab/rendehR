# Pivot the merged duration table into a time-binned wide grid

The interval-valued counterpart of
[`load_merged_ts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/load_merged_ts_wide.md).
A `dur` row is an interval (start/end), not a point measurement, so
unlike `ts` (where the same static table supports any `bin_min` on
demand), each row is first expanded across every bin it overlaps -
computing how many minutes of that bin were actually covered by the
episode - before the same `FILTER`-based conditional-aggregation pivot
[`load_merged_ts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/load_merged_ts_wide.md)
uses is applied to the expanded result (materialized as a temp table,
dropped when the call returns). The expansion is specific to the
requested `bin_min` and is rebuilt fresh on every call.

## Usage

``` r
load_merged_dur_wide(
  schema,
  spec,
  bin_min = 60,
  window = NULL,
  complete_grid = FALSE,
  n = Inf,
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

- spec:

  Named list mapping each concept to a character vector of aggregates,
  applied to minutes-exposed-per-bin (not the raw `dur` `value` column).
  Same aggregate names/shorthand as
  [`load_merged_ts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/load_merged_ts_wide.md):
  `"min"`, `"max"`, `"mean"`, `"median"`, `"sd"`, `"variance"`, `"sum"`,
  `"n"`/`"count"`, `"first"`, `"last"`, `"all_numeric"`.

- bin_min:

  Bin width in minutes (default 60). The emitted `t_bin` column is the
  0-based bin index (`floor(time / bin_min)`).

- window:

  Optional `c(from_bin, to_bin)` in bin-index units, inclusive both
  ends, restricting the output to those bins. `NULL` (default) keeps all
  bins.

- complete_grid:

  If TRUE, emit a row for every bin in `window` (or, when `window` is
  NULL, the observed bin range) for each admission present, left-joining
  the aggregates onto the full grid so unexposed bins come back as `NA`
  (not 0 - no imputation is performed; fill/mask downstream in R).
  Default FALSE.

- n:

  Max rows to return. Default Inf.

- cohort_only:

  If TRUE (default), restrict to rows of cohort members (`cohort_index`
  in the merged `cohort_info`), matched at admission grain on
  `(source_data, src_admission_id)`; `dur` has no ICU projection.
  Applied before the expansion. FALSE reads all imported context rows.

- con:

  Optional explicit DBI connection.

- case_adm_only, scope_only:

  Removed; supplying either is an error. Use `cohort_only`.

## Value

A wide data frame: id columns, `t_bin`, then one column per concept and
aggregate (minutes exposed), named `<concept>_<aggregate>`, or just
`<concept>` when the concept has one aggregate.

## Details

A zero-duration ("point") `dur` row still contributes exactly one row
(the bin containing its start) with 0 exposed minutes, so `n`/`count`
still see it even though duration-based aggregates (`sum`/`mean`/...)
correctly contribute nothing from it.

## Examples

``` r
if (FALSE) { # \dontrun{
load_merged_dur_wide("rendehr", spec = list(
  norepinephrine_exp = c("sum", "n")
), bin_min = 1440)
load_merged_dur_wide("rendehr", spec = list(propofol_exp = "sum"),
  bin_min = 60, window = c(0, 47), complete_grid = TRUE)
} # }
```

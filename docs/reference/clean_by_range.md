# Clean concept values using config-defined plausibility ranges

Flags (or drops / NA-sets) numeric values that fall outside the
`[min_value, max_value]` window declared for each concept in the concept
config CSV (`rendehr_concepts_config.csv`). This is physiology-based
cleaning: bounds are fixed reference ranges, not derived from the data.

## Usage

``` r
clean_by_range(
  df,
  ranges = NULL,
  config_path = NULL,
  src_name = NULL,
  value_col = "value",
  concept_col = "concept_name",
  single_concept = NULL,
  action = c("flag", "drop", "na")
)
```

## Arguments

- df:

  A data frame from fetch_concept()/fetch_concepts().

- ranges:

  Optional override. A data frame with columns `concept_name`,
  `min_value`, `max_value`; or a named list of `c(min, max)` vectors. If
  NULL (default) bounds are read from the concept config CSV.

- config_path:

  Path to the concept config CSV. Defaults to the bundled
  `rendehr_concepts_config.csv`. Ignored when `ranges` is supplied.

- src_name:

  Optional source name; when given, config rows are filtered to that
  source family before building bounds.

- value_col:

  Name of the numeric value column. Default "value".

- concept_col:

  Name of the grouping column. Default "concept_name". If absent from
  `df`, all rows are treated as one concept named after
  `single_concept`.

- single_concept:

  Concept name to assume when `df` has no `concept_col` (e.g. a single
  fetch_concept() result). Default NULL, which means: skip with a
  message unless `ranges`/config has a single row.

- action:

  One of "flag" (add `out_of_range` logical column, default), "drop"
  (remove offending rows), or "na" (set offending values to NA).

## Value

`df` with the action applied. A summary tibble (per concept: n, n_bad,
min/max bound used) is attached as `attr(result, "clean_report")`.

## Details

When a concept has several config rows (e.g. one per source/table), the
most permissive window is used: `min(min_value)` .. `max(max_value)`.
Concepts with no usable bound are left untouched (a message lists them).

## Examples

``` r
hr <- data.frame(concept_name = "heart_rate", value = c(60, 300, -5))
ranges <- data.frame(concept_name = "heart_rate", min_value = 0, max_value = 250)
clean_by_range(hr, ranges = ranges)
#>   concept_name value out_of_range
#> 1   heart_rate    60        FALSE
#> 2   heart_rate   300         TRUE
#> 3   heart_rate    -5         TRUE
clean_by_range(hr, ranges = ranges, action = "na")
#>   concept_name value
#> 1   heart_rate    60
#> 2   heart_rate    NA
#> 3   heart_rate    NA
```

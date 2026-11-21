# Clean concept values using statistical outlier rules

Flags (or drops / NA-sets) numeric values that fall outside data-derived
bounds, computed independently per concept. Unlike clean_by_range() the
thresholds come from the data, so this catches values that are
implausible *relative to the rest of the concept's distribution*.

## Usage

``` r
clean_by_outlier(
  df,
  method = c("iqr", "mad", "sd"),
  k = NULL,
  value_col = "value",
  concept_col = "concept_name",
  min_n = 5,
  action = c("flag", "drop", "na")
)
```

## Arguments

- df:

  A data frame from fetch_concept()/fetch_concepts().

- method:

  One of "iqr" (default), "mad", "sd".

- k:

  Threshold multiplier. Defaults per method: iqr 1.5, mad 3, sd 3.

- value_col:

  Name of the numeric value column. Default "value".

- concept_col:

  Grouping column. Default "concept_name"; if absent, all rows form one
  group.

- min_n:

  Minimum non-NA values in a group before outlier detection is
  attempted. Smaller groups are left unchanged. Default 5.

- action:

  One of "flag" (add `is_outlier` logical column, default), "drop", or
  "na".

## Value

`df` with the action applied. A per-concept summary (n, n_outlier,
lower/upper bound, method) is attached as
`attr(result, "clean_report")`.

## Details

Methods:

- iqr:

  Tukey fences: `[Q1 - k*IQR, Q3 + k*IQR]`, default k = 1.5. Robust,
  distribution-light - the usual first choice.

- mad:

  `median +/- k * (1.4826 * MAD)`, default k = 3. Most robust to heavy
  tails; good for skewed clinical labs.

- sd:

  `mean +/- k * sd`, default k = 3. Assumes roughly symmetric data;
  sensitive to the very outliers it tries to find.

## Examples

``` r
hr <- data.frame(concept_name = "heart_rate", value = c(60, 70, 80, 75, 72, 300, 68, 71))
clean_by_outlier(hr, method = "iqr")
#>   concept_name value is_outlier
#> 1   heart_rate    60      FALSE
#> 2   heart_rate    70      FALSE
#> 3   heart_rate    80      FALSE
#> 4   heart_rate    75      FALSE
#> 5   heart_rate    72      FALSE
#> 6   heart_rate   300       TRUE
#> 7   heart_rate    68      FALSE
#> 8   heart_rate    71      FALSE
clean_by_outlier(hr, method = "iqr", action = "drop")
#>   concept_name value
#> 1   heart_rate    60
#> 2   heart_rate    70
#> 3   heart_rate    80
#> 4   heart_rate    75
#> 5   heart_rate    72
#> 7   heart_rate    68
#> 8   heart_rate    71
```

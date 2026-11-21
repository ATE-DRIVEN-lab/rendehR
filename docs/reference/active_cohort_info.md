# Show currently active patient subgroup filters

Show currently active patient subgroup filters

## Usage

``` r
active_cohort_info()
```

## Value

The list of active cohort filters, one entry per source with `ids`,
`id_col` and `version`, invisibly; `NULL` when none is set.

## Examples

``` r
active_cohort_info()
#> No active cohort filters.
```

# Print a cohort selection plan

Shows how a
[`select_cohort_ids()`](https://ate-driven-lab.github.io/rendehR/reference/select_cohort_ids.md)
selection resolved: route, selection and evidence keys, requested and
effective import scope, and the requested, matched and unmatched counts.
Identifier values are never printed; they are in `x$values`.

## Usage

``` r
# S3 method for class 'ehr_selection_plan'
print(x, ...)
```

## Arguments

- x:

  An `ehr_selection_plan`.

- ...:

  Ignored.

## Value

Invisibly, `x`.

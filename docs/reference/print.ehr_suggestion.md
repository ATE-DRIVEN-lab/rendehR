# Print a suggested next pipeline step

Summarizes the target database/base directory/sources (and how each was
resolved), the per-source, per-stage status (`ok`/`missing` with a
reason), merge status when applicable, and the recommended R code to run
next.

## Usage

``` r
# S3 method for class 'ehr_suggestion'
print(x, ...)
```

## Arguments

- x:

  An `ehr_suggestion` object, as returned by
  [`ehr_suggest_next()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_suggest_next.md).

- ...:

  Ignored.

## Value

Invisibly, `x`.

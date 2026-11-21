# Print a lazy source browser

Summarizes the source name, schema, database, and the number and names
of the lazy
[`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html) tables
available on the object.

## Usage

``` r
# S3 method for class 'ehr_source'
print(x, ...)
```

## Arguments

- x:

  An `ehr_source` object, as returned by
  [`browse_source()`](https://ate-driven-lab.github.io/rendehR/reference/browse_source.md)
  or
  [`open_source()`](https://ate-driven-lab.github.io/rendehR/reference/browse_source.md).

- ...:

  Ignored.

## Value

Invisibly, `x`.

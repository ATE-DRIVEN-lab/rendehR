# Print a flexible database browser

Summarizes the database name, browser view (`"source"`, `"flattened"`,
or `"both"`), table type, and table count, followed by a reminder of the
`$`/`[[` access pattern for the active view.

## Usage

``` r
# S3 method for class 'ehr_db_flex_browser'
print(x, ...)
```

## Arguments

- x:

  An `ehr_db_flex_browser` object, as returned by
  [`browse_ehr_db_flex()`](https://ate-driven-lab.github.io/rendehR/reference/browse_ehr_db_flex.md).

- ...:

  Ignored.

## Value

Invisibly, `x`.

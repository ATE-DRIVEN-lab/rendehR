# Run all pipeline stages for multiple sources

Loops over
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
for each source name. Arguments in `...` are passed unchanged to every
source.

## Usage

``` r
run_sources(src_names = NULL, ...)
```

## Arguments

- src_names:

  Character vector of source names. If NULL, uses the active project
  config source, then "mimic_iv_demo".

- ...:

  Arguments passed to
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md).

## Value

Invisibly returns a named logical vector indicating completed sources.

## Examples

``` r
if (FALSE) { # \dontrun{
run_sources(c("mimic_iv_demo", "eicu_demo"))
} # }
```

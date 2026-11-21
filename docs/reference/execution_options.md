# Specify pipeline execution options

Configure rebuild behavior, import progress, and continuation after
stage failures.

## Usage

``` r
execution_options(
  skip_on_error = FALSE,
  show_progress = TRUE,
  overwrite = FALSE
)
```

## Arguments

- skip_on_error:

  Logical. Continue to subsequent pipeline stages after a failure.
  Default: `FALSE`. Required raw-transformation failures always stop
  [`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md).

- show_progress:

  Logical. Display progress during cohort import. Default: `TRUE`. The
  optional raw-cache stage uses the same setting.

- overwrite:

  Logical. Rebuild raw caches, cohort Parquet files, and loaded cohort
  tables when requested. Default: `FALSE`, which allows those stages to
  reuse complete outputs. This setting does not control whether concept
  SQL or harmonization rebuilds their tables.

## Value

A named list of the supplied options, suitable for the `execution`
argument of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
and compatible direct stage functions.

## Details

Use these options with
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
or a direct stage that accepts the relevant settings.

Only explicitly supplied arguments are included in the result, including
explicit `NULL` values. Omitted settings use the defaults of the
receiving function. An empty options list therefore leaves its defaults
unchanged.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)

## Examples

``` r
execution_options(show_progress = FALSE)
#> $show_progress
#> [1] FALSE
#> 
execution_options(overwrite = TRUE)
#> $overwrite
#> [1] TRUE
#> 
```

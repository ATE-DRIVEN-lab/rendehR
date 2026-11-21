# Specify harmonized-output options

Select the harmonized output tables and their names.

## Usage

``` r
harmonization_options(
  flat_concepts = NULL,
  build_ts = TRUE,
  build_flat = TRUE,
  build_dur = TRUE,
  ts_table_name = "ts_tab",
  flat_table_name = "flat_tab",
  dur_table_name = "dur_tab"
)
```

## Arguments

- flat_concepts:

  Character vector of flat concept table names to include in the
  harmonized flat table. `NULL` or an empty vector requests automatic
  discovery.

- build_ts:

  Logical. Build the harmonized time-series table. Default: `TRUE`.

- build_flat:

  Logical. Build the harmonized flat table. Default: `TRUE`.

- build_dur:

  Logical. Build the harmonized duration table. Default: `TRUE`.

- ts_table_name:

  Character string naming the harmonized time-series table. Default:
  `"ts_tab"`.

- flat_table_name:

  Character string naming the harmonized flat table. Default:
  `"flat_tab"`.

- dur_table_name:

  Character string naming the harmonized duration table. Default:
  `"dur_tab"`.

## Value

A named list of the supplied options, suitable for the `harmonization`
argument of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
and compatible direct stage functions.

## Details

Harmonization reads existing concept tables. Its output selection is
independent of the layers selected for concept extraction.

Only explicitly supplied arguments are included in the result, including
explicit `NULL` values. Omitted settings use the defaults of the
receiving function. An empty options list therefore leaves its defaults
unchanged.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)

## Examples

``` r
harmonization_options(build_ts = FALSE, build_flat = TRUE, build_dur = FALSE)
#> $build_ts
#> [1] FALSE
#> 
#> $build_flat
#> [1] TRUE
#> 
#> $build_dur
#> [1] FALSE
#> 
```

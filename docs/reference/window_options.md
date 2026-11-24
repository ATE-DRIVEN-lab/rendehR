# Specify observation-window options

Configure the observation-window builder used during cohort import.

## Usage

``` r
window_options(window_sql = NULL, window_fn = NULL)
```

## Arguments

- window_sql:

  Character string giving a SQL file that creates the source
  observation-window tables, including `visit_windows`. `NULL` uses the
  source's configured window SQL.

- window_fn:

  Function, or character string naming a function, called as
  `window_fn(con)` with the import connection. It must return a data
  frame with the columns required by the source's visit-window
  definition. When supplied, it takes precedence over `window_sql`.

## Value

A named list of the supplied options, suitable for the `windows`
argument of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
and compatible direct stage functions.

## Details

The builder defines the temporal hierarchy used to associate source
records with cohort members.

Only explicitly supplied arguments are included in the result, including
explicit `NULL` values. Omitted settings use the defaults of the
receiving function. An empty options list therefore leaves its defaults
unchanged.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)

## Examples

``` r
window_options(window_sql = "windows.sql")
#> $window_sql
#> [1] "windows.sql"
#> 
```

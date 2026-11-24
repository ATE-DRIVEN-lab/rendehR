# Build visit windows for a source

Compatibility alias for
[`build_window_tables()`](https://ate-driven-lab.github.io/rendehR/reference/build_window_tables.md).

## Usage

``` r
build_visit_windows(src_name, db_name = NULL, base_dir = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- db_name:

  Optional Postgres database name. If NULL, uses the active project
  config, then "rendehr".

- base_dir:

  Project root directory. Defaults to current working directory.

## Value

`NULL`, invisibly. Called for its side effect of writing
admission_windows, icu_windows and visit_windows.

## Examples

``` r
if (FALSE) { # \dontrun{
build_visit_windows("mimic_iv_demo", db_name = "rendehr")
} # }
```

# Build admission, ICU, and visit window tables for any source

Runs `window_tables.sql` to create three source spines in the raw
schema: `admission_windows` (one row per hospital admission),
`icu_windows` (one row per ICU stay), and `visit_windows`
(admission-to-ICU crosswalk that retains admissions without ICU stays).
Always required; call after
[`run_db()`](https://ate-driven-lab.github.io/rendehR/reference/run_db.md).
Can be re-run independently at any time. Replacement is transactional:
if SQL execution or output validation fails, the previous valid window
tables are restored.

## Usage

``` r
build_window_tables(src_name, db_name = NULL, base_dir = NULL)
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
build_window_tables("mimic_iv_demo", db_name = "rendehr")
} # }
```

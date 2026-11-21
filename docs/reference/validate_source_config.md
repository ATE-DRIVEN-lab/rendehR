# Validate a source configuration

Checks that a source spec is structurally complete enough for the
pipeline to use, reporting every problem at once (unlike
[`register_source()`](https://ate-driven-lab.github.io/rendehR/reference/register_source.md),
which stops at the first). Accepts a registered family name, an
in-memory spec list, or a path to a `source-registry.json` file. When a
config `root` is known, it also warns about missing SQL scaffolding
(`window_tables.sql`, schema DDL).

## Usage

``` r
validate_source_config(
  config = NULL,
  name = NULL,
  root = NULL,
  check_sql = TRUE,
  quiet = FALSE
)
```

## Arguments

- config:

  A spec list, or a path to a `source-registry.json` (single object or
  array). Mutually exclusive with `name`.

- name:

  A registered family name to validate from the live registry.

- root:

  Config root for SQL-presence checks. Defaults to
  `EHR_EXTERNAL_CONFIG_ROOT`.

- check_sql:

  Check for expected SQL scaffold files when `root` is available?
  Default TRUE.

- quiet:

  Suppress the summary message? Default FALSE.

## Value

Invisibly, a list with `ok`, `problems`, and `warnings`.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_source_config(config = "config/sources/my_icu.json")
} # }
```

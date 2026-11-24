# Check a concept configuration for invalid rows

Applies the concept configuration rules to every row and lists the
problems found.
[`run_atomic_concepts_unified()`](https://ate-driven-lab.github.io/rendehR/reference/run_atomic_concepts_unified.md)
runs the same check before it builds any SQL: rows with a `"skip"`
problem are left out, rows with a `"warn"` problem are built, and both
are reported in one warning.

## Usage

``` r
validate_concept_config(config, src_name = NULL)
```

## Arguments

- config:

  Path to a concept configuration CSV, or a data frame of its rows.

- src_name:

  Optional source name, such as `"mimic_iv_demo"`. When given, only rows
  of its source family are checked.

## Value

A data frame with one row per problem: `row` (position of the config row
in `config`, header excluded), `source`, `concept_name`, `table_name`,
`rule`, `severity` (`"skip"` or `"warn"`) and `message`. It has zero
rows when the configuration is valid.

## Details

Rules:

- `unknown_itm_class` (skip): `itm_class` is missing or not one of
  `itm_itm`, `rgx_itm`, `rgx_exp_itm` or `col_itm` (case and surrounding
  spaces are ignored).

- `empty_match_value` (skip): `match_value` is empty for a class other
  than `col_itm`. Only `col_itm` reads a whole column; the other classes
  need item ids, labels or a pattern, and without one would select every
  row of the source table.

## Examples

``` r
validate_concept_config(
  system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"))
#> [1] row          source       concept_name table_name   rule        
#> [6] severity     message     
#> <0 rows> (or 0-length row.names)
```

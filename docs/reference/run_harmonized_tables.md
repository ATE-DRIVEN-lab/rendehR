# Build harmonized ts_tab and flat_tab output tables for one source

Reads the concept config to discover which concept tables exist, builds
a long-format time-series table (ts_tab) and optionally a flat table
(flat_tab) with one column per flat concept.

## Usage

``` r
run_harmonized_tables(
  base_dir = NULL,
  src_name,
  db_name = NULL,
  flat_concepts = NULL,
  build_ts = TRUE,
  build_flat = TRUE,
  build_dur = TRUE,
  ts_table_name = "ts_tab",
  flat_table_name = "flat_tab",
  dur_table_name = "dur_tab",
  harmonization = NULL
)
```

## Arguments

- base_dir:

  Project root directory.

- src_name:

  Source name (e.g. "mimic_iv_demo").

- db_name:

  Optional Postgres database name. If NULL, uses the active project
  config, then "rendehr".

- flat_concepts:

  Character vector of flat concept names (e.g. sex, race). NULL or empty
  triggers auto-discovery of all non-ts tables in the concepts schema.

- build_ts:

  Build time-series table (default TRUE).

- build_flat:

  Build flat table (default TRUE).

- build_dur:

  Build duration table - medication exposures and infusion episodes
  unioned into a single long-format table (default TRUE).

- ts_table_name:

  Name for the time-series output table.

- flat_table_name:

  Name for the flat output table.

- dur_table_name:

  Name for the duration output table.

- harmonization:

  Optional overrides from
  [`harmonization_options()`](https://ate-driven-lab.github.io/rendehR/reference/harmonization_options.md)
  or a named list.

## Value

`TRUE`, invisibly, once every harmonized table is built.

## Examples

``` r
if (FALSE) { # \dontrun{
run_harmonized_tables(src_name = "mimic_iv_demo", db_name = "rendehr")
} # }
```

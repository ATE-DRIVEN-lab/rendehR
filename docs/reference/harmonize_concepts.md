# Build harmonized ts_tab and flat_tab output tables

Build harmonized ts_tab and flat_tab output tables

## Usage

``` r
harmonize_concepts(
  cfg,
  flat_concepts = c("sex", "race", "insurance", "age"),
  build_ts = TRUE,
  build_flat = TRUE,
  ...
)
```

## Arguments

- cfg:

  Config list from ehr_load_config().

- flat_concepts:

  Character vector of concept names to include in the flat table.
  Default: c("sex", "race", "insurance", "age").

- build_ts:

  Build the time-series table. Default TRUE.

- build_flat:

  Build the flat (baseline) table. Default TRUE.

- ...:

  Additional arguments passed to run_harmonized_tables().

## Value

Invisibly NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- ehr_load_config("config/ehr_config.json")
harmonize_concepts(cfg, flat_concepts = c("sex", "age"))
} # }
```

# Load a single source config block from a data-sources JSON file

Load a single source config block from a data-sources JSON file

## Usage

``` r
load_src_cfg(config_path, src_name)
```

## Arguments

- config_path:

  Path to the canonical `rendehr-data-source.json`.

- src_name:

  Source name (e.g. "mimic_iv_demo").

## Value

Named list of source config fields.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- load_src_cfg("config/ehr_config.json", "mimic_iv_demo")
} # }
```

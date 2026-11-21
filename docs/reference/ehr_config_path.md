# Path to a bundled package config file

Returns the full path to a file in `inst/extdata/`. Use this instead of
hard-coding paths so the package works when installed anywhere.

## Usage

``` r
ehr_config_path(filename)
```

## Arguments

- filename:

  File name (e.g. "rendehr-data-source.json").

## Value

Absolute path string.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_use_config("config/ehr_config.json")
ehr_config_path("config_pass.json")
} # }
```

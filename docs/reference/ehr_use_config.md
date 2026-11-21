# Use a project configuration for this R session

Loads a project config with
[`ehr_load_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_load_config.md)
and stores it in the package session environment. Path helpers and
[`ehr_connect()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_connect.md)
use this config as their default source of project settings.

## Usage

``` r
ehr_use_config(config_path = NULL)
```

## Arguments

- config_path:

  Path to a JSON project config. Defaults to the `EHR_CONFIG`
  environment variable or `config/ehr_config.json`.

## Value

Invisibly returns the loaded config.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_use_config("config/ehr_config.json")
ehr_config_root()
} # }
```

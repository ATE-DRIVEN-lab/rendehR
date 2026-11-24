# Root of the external user config directory

Returns the directory named by `EHR_EXTERNAL_CONFIG_ROOT`. Returns
`NULL` when it is unset or when the configured directory does not exist.
This lets users supply SQL and config files (for new sources, or
overrides of existing ones) from outside the installed package, without
forking it. It is the second layer in the resolver order: explicit
argument -\> project-local -\> `EHR_EXTERNAL_CONFIG_ROOT` -\> packaged
defaults. Retired `EHR_CONFIG_DIR` and `SCIEHR_CONFIG_PATH` values
produce migration guidance.

## Usage

``` r
ehr_config_root()
```

## Value

Absolute directory path, or `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_use_config("config/ehr_config.json")
ehr_config_root()
} # }
```

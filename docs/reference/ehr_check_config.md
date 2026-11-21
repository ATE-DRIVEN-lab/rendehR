# Check a project configuration

Validates required fields, resolves source-specific paths, and reports
likely problems such as a missing credentials file or secrets
accidentally placed in the project config.

## Usage

``` r
ehr_check_config(
  config = NULL,
  config_path = NULL,
  src_name = NULL,
  quiet = FALSE
)
```

## Arguments

- config:

  Optional config list. If NULL, loads `config_path` or uses the active
  config from
  [`ehr_use_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_use_config.md).

- config_path:

  Optional path to a project config JSON file.

- src_name:

  Optional source name to use while resolving paths.

- quiet:

  Suppress summary messages? Default FALSE.

## Value

A list with `ok`, `problems`, `warnings`, and `paths`.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_check_config(config_path = "config/ehr_config.json", src_name = "mimic_iv_demo")
} # }
```

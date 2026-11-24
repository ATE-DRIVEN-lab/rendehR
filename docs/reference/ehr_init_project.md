# Create project directories for a source

Creates the raw data, cohort parquet, source SQL, generated SQL, and
config directories implied by
[`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md).
It also creates starter project and credentials JSON files when they are
missing. If a project config has been loaded with
[`ehr_use_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_use_config.md),
its path roots are used by default. If a project config file already
exists and no config has been loaded, it is used for path resolution.

## Usage

``` r
ehr_init_project(
  src_name = NULL,
  base_dir = NULL,
  raw_data_root = NULL,
  config = NULL,
  create_config = TRUE,
  overwrite_config = FALSE,
  create_credentials = TRUE,
  overwrite_credentials = FALSE
)
```

## Arguments

- src_name:

  Source name. Defaults to the loaded project config.

- base_dir:

  Project root directory. Defaults to the loaded project config, then
  current working directory.

- raw_data_root:

  Optional parent storage-root override. Source paths are derived
  beneath it.

- config:

  Optional config list from
  [`ehr_load_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_load_config.md).

- create_config:

  Create `config/ehr_config.json` if it is missing? Default TRUE.

- overwrite_config:

  Overwrite an existing project config? Default FALSE.

- create_credentials:

  Create `config/config_pass.json` if it is missing? Default TRUE.

- overwrite_credentials:

  Overwrite an existing credentials file? Default FALSE.

## Value

Invisibly returns the paths list.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_init_project(src_name = "mimic_iv_demo", base_dir = "~/my_project")
} # }
```

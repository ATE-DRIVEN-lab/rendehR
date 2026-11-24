# Build the standard project path list for a source

Resolves all project paths for a source. If
[`ehr_use_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_use_config.md)
has loaded a project config, its path roots are used by default;
explicit arguments still win. Source-specific directories are derived
internally from `src_name`.

## Usage

``` r
ehr_paths(
  src_name = NULL,
  base_dir = NULL,
  raw_data_root = NULL,
  config = NULL,
  raw_data_dir = NULL
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo", "mimic_iii"). This names the cohort
  output directories and Postgres schemas. Defaults to the active
  project config source, then "mimic_iv_demo". The source family
  (mimic_iv, eicu, mimic_iii) is derived from it via
  [`src_family()`](https://ate-driven-lab.github.io/rendehR/reference/src_family.md).

- base_dir:

  Project root directory. Defaults to the loaded project config, then to
  the current working directory.

- raw_data_root:

  Optional parent storage-root override. The source directory is always
  derived as `{raw_data_root}/{src_name}`.

- config:

  Optional config list from
  [`ehr_load_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_load_config.md).
  Defaults to the active config loaded by
  [`ehr_use_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_use_config.md),
  then an existing `config/ehr_config.json` or `EHR_CONFIG` file.

- raw_data_dir:

  Retired ambiguous override. Use `raw_data_root`.

## Value

Named list of file/directory paths.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_use_config("config/ehr_config.json")
paths <- ehr_paths("mimic_iv_demo")
paths$raw_dir
} # }
```

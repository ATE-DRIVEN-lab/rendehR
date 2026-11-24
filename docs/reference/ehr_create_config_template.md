# Create a starter project configuration file

Creates a project config template with path roots. Source-specific
folders are still derived internally by
[`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md),
so users do not need to list every source in the config.

## Usage

``` r
ehr_create_config_template(
  path = file.path("config", "ehr_config.json"),
  src_name = "mimic_iv_demo",
  db_name = "rendehr",
  base_dir = ".",
  raw_data_root = file.path("data", "source_data"),
  cohort_parquet_root = file.path("data", "cohorts_data"),
  duckdb_root = file.path("data", "duckdb"),
  credentials_path = file.path("config", "config_pass.json"),
  cohort_profile = "generic",
  overwrite = FALSE
)
```

## Arguments

- path:

  Output path. Defaults to `config/ehr_config.json`.

- src_name:

  Default source name.

- db_name:

  Default Postgres database name.

- base_dir:

  Project root directory.

- raw_data_root:

  Root for raw downloaded source data.

- cohort_parquet_root:

  Root for cohort parquet outputs.

- duckdb_root:

  Root for DuckDB database files.

- credentials_path:

  Path to the separate credentials JSON file.

- cohort_profile:

  Named cohort profile: `"generic"` (the default) or `"scid"`.

- overwrite:

  Overwrite an existing file? Default FALSE.

## Value

Invisibly returns the written config path.

## Examples

``` r
path <- file.path(tempdir(), "ehr_config.json")
ehr_create_config_template(path, src_name = "mimic_iv_demo", base_dir = tempdir())
#> rendehR: project config written to '/tmp/Rtmpew7WRg/ehr_config.json'.
str(ehr_load_config(path), max.level = 1)
#> List of 9
#>  $ version      : int 1
#>  $ project      :List of 1
#>  $ database     :List of 2
#>  $ source       :List of 1
#>  $ paths        :List of 3
#>  $ concepts     :List of 3
#>  $ cohort       :List of 5
#>  $ defaults_used: chr(0) 
#>  $ config_path  : chr "/tmp/Rtmpew7WRg/ehr_config.json"
```

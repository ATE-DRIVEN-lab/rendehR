# Load project configuration from a JSON file

Reads user-facing project settings such as source name, database name,
project roots, and the path to the separate credentials file. Passwords
should stay in `config/config_pass.json` or environment variables, not
in this project config.

## Usage

``` r
ehr_load_config(config_path = NULL)
```

## Arguments

- config_path:

  Path to a JSON project config. Required fields may be either flat
  (`src_name`, `db_name`) or nested (`source.src_name`,
  `database.db_name`).

## Value

Named list of config values. Flat fields are retained for backward
compatibility, and nested fields are added for newer code.

## Details

If `config_path` is omitted, checks the `EHR_CONFIG` environment
variable, then falls back to `"config/ehr_config.json"` relative to the
current working directory.

## Examples

``` r
path <- file.path(tempdir(), "ehr_config_load.json")
ehr_create_config_template(path, base_dir = tempdir())
#> rendehR: project config written to '/tmp/Rtmpew7WRg/ehr_config_load.json'.
cfg <- ehr_load_config(path)
names(cfg)
#> [1] "version"       "project"       "database"      "source"       
#> [5] "paths"         "concepts"      "cohort"        "defaults_used"
#> [9] "config_path"  
```

# Return the active project configuration

Return the active project configuration

## Usage

``` r
ehr_config()
```

## Value

The config loaded by
[`ehr_use_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_use_config.md).

## Examples

``` r
path <- file.path(tempdir(), "ehr_config_active.json")
ehr_create_config_template(path, base_dir = tempdir())
#> rendehR: project config written to '/tmp/Rtmpew7WRg/ehr_config_active.json'.
ehr_use_config(path)
#> rendehR: project config loaded from '/tmp/Rtmpew7WRg/ehr_config_active.json'.
names(ehr_config())
#> [1] "version"       "project"       "database"      "source"       
#> [5] "paths"         "concepts"      "cohort"        "defaults_used"
#> [9] "config_path"  
```

# High-level pipeline API - cfg-accepting wrappers

These functions provide the primary user-facing API. Each accepts a
config list returned by ehr_load_config() and delegates to the
underlying stage runner. The lower-level stage runners (import_cohort,
run_db, run_concepts, run_harmonized_tables) remain available for
granular control.

## Usage

``` r
build_cohort(cfg, ...)
```

## Arguments

- cfg:

  Config list from ehr_load_config(). Must contain source.src_name and
  database.db_name. project.base_dir defaults to getwd() if omitted.

- ...:

  Additional arguments passed to run_db().

## Value

Invisibly NULL.

## Details

Typical workflow:

    cfg <- ehr_load_config()   # needs source.src_name and database.db_name
    download_source(cfg$source$src_name)
    build_cohort(cfg)
    extract_concepts(cfg)
    harmonize_concepts(cfg)

Load raw data into Postgres and build the SCI cohort

Runs the Postgres load + helper table stages (run_db). Assumes data has
already been downloaded and imported to parquet via download_source().

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- ehr_load_config("config/ehr_config.json")
build_cohort(cfg)
} # }
```

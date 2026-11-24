# Source SQL, concepts and validation

Separate the states: entry registered, SQL written, configuration valid, demo cohort built, concepts extracted, harmonized output checked. One does not imply the next.

## SQL to write

- `db_setup_sqls/schema/create_tables.sql`: DDL for the raw tables with the dataset's real column names and types. Model it on the built-in files (`system.file("sql", "mimic_iv", "db_setup_sqls", "schema", package = "rendehR")`).
- `db_setup_sqls/window_tables.sql`: `admission_windows` (one row per admission), `icu_windows` (one row per ICU stay) and `visit_windows` (admissions with their ICU stays, admissions without one kept), with the dataset's identifiers and the time columns the registry names. Follow `system.file("sql", "mimic_iv", "db_setup_sqls", "window_tables.sql", package = "rendehR")` and `vignette("cohorts", package = "rendehR")` (or <https://ate-driven-lab.github.io/rendehR/articles/cohorts.html>).
- `indexes.sql`: optional.

## Concepts

Add rows with `source = "<name>"` to the configuration root's `rendehr_concepts_config.csv`, one per concept the user needs, following `rendehr-add-concept`. Unmapped concepts are simply absent for the new source.

## Validate

```r
rendehR::validate_source_config(name = "amsterdam", root = config_root)
# not registered yet (incomplete entry): validate the file
rendehR::validate_source_config(config = file.path(config_root, "sources", "amsterdam.json"))
```

It reports every structural problem at once and warns about missing SQL. It does not read data.

## Bounded run

With the user's authorization and a demo variant (`amsterdam_demo`, row limit from the registry):

```r
rendehR::run_one_source("amsterdam_demo", db_name = scratch_db, run_comorbidities = FALSE, run_iss = FALSE)
rendehR::cohort_summary("amsterdam_demo")
```

Check the cohort size against the dataset's documentation, that every window table has unique keys, that times are on the intended clock, and a few concept values against the raw rows. Use a scratch database: the pipeline creates and replaces schemas.

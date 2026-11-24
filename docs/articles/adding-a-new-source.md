# Adding a new data source

rendehR ships MIMIC-III, MIMIC-IV, eICU and their demos. A new source is
a registration plus four assets the pipeline reads: a source entry in
the catalog, raw-loading rules, window SQL, and a concept overlay. The
wizard scaffolds the files; this article explains what each one must
contain.

## What the wizard creates

[`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md)
writes a source directory with the files the pipeline looks for.
Non-interactive use requires the hierarchy columns and the time origin,
because those cannot be guessed:

``` r
add_source_wizard(
  name          = "my_ehr",
  root          = file.path(docs_root, "my-source"),
  select_id_cols = c("patient_id", "admission_id", "icu_id"),
  time_origin   = "admission_time",
  id_cols       = c(patient_id = "patient_id", admission_id = "admission_id",
                    icu_id = "icu_id"),
  cohort_key    = "admission_id",
  register      = FALSE
)
```

`register = TRUE` writes the catalog entry too. The generated
`window_tables.sql` must define three relations; a source that omits one
fails at the window stage:

- `admission_windows` — one row per hospital admission;
- `icu_windows` — one row per ICU stay;
- `visit_windows` — the union used by cohort materialization and
  look-back.

The wizard’s output is a starting point: the column names in your raw
tables and the catalog’s link keys still have to match your data.

## The catalog entry

The catalog is `inst/extdata/rendehr-data-source.json`. An entry names
the source family, its identifier columns, the tables that carry an ICU
key, and the `cohort_link_key` that decides how clinical tables are
filtered to the cohort. `id_prefixes` and `demo_row_limit` are optional.

The catalog is read from three places, first match wins: the project
configuration, the directory in `EHR_EXTERNAL_CONFIG_ROOT`, and the
package. A new source registered outside the package therefore needs
`EHR_EXTERNAL_CONFIG_ROOT` set in the R session (for example in
`.Renviron`); otherwise a fresh session falls back to the package
catalog and does not know the source.

## Raw data and concepts

[`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
knows the PhysioNet releases only. A new source’s raw files are yours to
place; the pipeline reads them from `raw_data_root` using the catalog’s
table and column rules. The wizard does not add a download route.

A concept overlay lives at `<root>/rendehr_concepts_config.csv` and
covers the time-series dictionary. Duration concepts come from
`rendehr_duration_concepts_config.csv` and are not scaffolded; add rows
there for drug exposures the source can measure. Only the required
fields are emitted as `null`; optional fields are omitted, so an overlay
never silently blanks a package default.
[`validate_concept_config()`](https://ate-driven-lab.github.io/rendehR/reference/validate_concept_config.md)
lists the overlay rows the concept stage would skip.

Subcohorts inside a source are materialized with
[`materialize_subcohort()`](https://ate-driven-lab.github.io/rendehR/reference/materialize_subcohort.md);
they are not a per-source “variant” setting. The only per-source variant
the wizard exposes is `demo.row_limit`, which caps a demo import.

## Validate and run

[`validate_source_config()`](https://ate-driven-lab.github.io/rendehR/reference/validate_source_config.md)
checks that the files the source declares exist. It does not prove the
SQL returns matching columns, and
[`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
does not accept a newly registered name. The first real test is an
unfiltered DuckDB run, which is also the only pipeline path a newly
registered family supports before its clinical SQL is written:

``` r
run_one_source("my_ehr", engine = "duckdb",
               stages = c("run_db", "build_cohort_info"))
```

Once the window tables and the concept overlay are in place, the usual
cohort routes apply, and the source can take part in
[`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
like any other.

The add-source assistant can produce the wizard call for you:

``` r
ehr_add_source_assistant(action = "script")
```

It returns the call as text; `action = "write"` asks it to create the
files directly. State the action explicitly in scripts: the default is
`"write"` in an interactive session and `"script"` otherwise.

## Related articles

- [`vignette("projects")`](https://ate-driven-lab.github.io/rendehR/articles/projects.md):
  where the source root and the raw data live
- [`vignette("cohorts")`](https://ate-driven-lab.github.io/rendehR/articles/cohorts.md):
  selection routes for the new source
- [`vignette("pipeline")`](https://ate-driven-lab.github.io/rendehR/articles/pipeline.md):
  the stages that consume these assets

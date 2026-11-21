# Run, verify, and inspect continuation

Establish source, project, database, selected cohort, desired concepts/layers, outputs, and requested execution mode. Use [setup checks](setup-and-readiness.md) and [cohort selection](custom-cohorts.md) as needed. Do not reset an existing project or silently rerun the entire pipeline for one failed stage.

## Planning

run_one_source requires local source files; download_source is a separate acquisition step. Inspect actual stage inputs before execution. Selected stages run in pipeline order; supplying an option group does not select its stage. Never describe concept_options(dry_run=TRUE) as a dry run of the whole pipeline.

Public stages include import_cohort, run_db, run_raw_mods, build_window_tables, build_cohort_info, run_concepts, and run_harmonized_tables, with optional enrichment/external stages. Use installed help for the exact default stage set. The scid profile can enable clinical scores by default; for a no-score request control enrichment stages AND concept layers/allowlists. Default run_concepts includes score SQL, including ehr scores and score-related flat concepts. Read [Setup and readiness](setup-and-readiness.md#enforce-a-no-clinical-scores-request) before generating that workflow. Do not invent run_iss/run_comorbidities fields inside enrichment_options.

The setup template's run_db route already loads cohort-aware windows. Do not blindly insert build_window_tables afterward: rebuilding from raw records can overwrite cohort-aware membership. eICU raw modifications are prerequisites for many concepts; preserve their ordering before extraction.

## Scoped execution example

This example executes concept extraction only, assuming its cohort and window prerequisites already exist. concept_names and project/database values are supplied inputs. It does not promise a side-effect-free preview.

```r
result <- rendehR::run_one_source(
  src_name = src, db_name = database_name, base_dir = project_dir,
  stages = "run_concepts",
  concepts = rendehR::concept_options(layers = "ts", concept_name = concept_names),
  execution = rendehR::execution_options(skip_on_error = FALSE, overwrite = FALSE)
)
```

Use the current grouped options interface. Do not supply the same setting both directly and in a group. Empty constructors preserve receiving-function defaults; explicit NULL can differ from omission. execution_options(overwrite=FALSE) is not a global prohibition on rebuilding concept or harmonized tables.

## Validation

Inspect returned statuses and errors; a function finishing or returning an empty result is not sufficient evidence of all requested outputs. A stopped run may not return a stage-status frame. Record the actual failed stage and message instead of fabricating a complete report.

verify_* results generally include ok, checks, and messages. Inspect isTRUE(result$ok) and actual checks. Their scope is limited:

| Check | What to establish |
| --- | --- |
| verify_download(paths, required_tables) | At this baseline checks cohort Parquet files, despite its name; does not prove remote download integrity |
| verify_db_load(con, schema, required_tables) | Named raw tables present/nonempty; choose source-appropriate requirements rather than MIMIC defaults for all sources |
| verify_cohort(con, schema) | Prepared cohort anchor checks, not independent clinical membership validation |
| verify_concepts(con, schema, concept_names) | Named tables; a NULL name list may merely establish some concept table exists |
| verify_harmonized(con, schema, ...) | Expected output tables/columns; not unit equivalence or clinical validity |

Run only checks relevant to requested outputs: a ts-only workflow should not fail merely because intentionally unbuilt flat/duration outputs are missing. Verify counts, identifier grain, time/units, and sample aggregates separately as appropriate.

## Resume and changed definitions

```r
next_step <- rendehR::ehr_suggest_next(
  src_names = src, db_name = database_name, base_dir = project_dir,
  suggest_merge = FALSE, run_comorbidities = FALSE, run_iss = FALSE
)
```

Use this to inspect status/recommended_code, not to eval recommended code automatically. Flags above reflect a no-score request, not a default for every study. The function conservatively treats unvalidated outputs as incomplete. It does not prove all outputs are current after changed cohort definitions, data releases, SQL, or concept mappings.

Before continuation compare current inputs with the recorded run. Explain expected affected downstream stages and uncertainty. No guaranteed minimal invalidation graph exists in this skill. Do not delete outputs, set overwrite=TRUE, or continue after errors merely to obtain a success message.

Return attempted/completed/failed/not-run stages, evidence, output locations, and the next bounded action. If a package defect blocks progress, report it separately rather than edit package code.

Concept generation writes ts/dur SQL into the project's
`sql/generated/<source>/{ts,dur}` directory (or the configured generated root),
including during a dry run, so it does not modify package files. Use a
disposable project and database for live runs; parse-only checks do not execute
those writes.

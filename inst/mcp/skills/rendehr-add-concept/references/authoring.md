# Author a concept extension

## Define the variable

Collect concept name/description, source and release, underlying tables/identifiers, flat/time-series/duration modality, identifier grain, time origin or interval endpoints, raw and target units, matching logic, transformations, valid-range policy, and source evidence. Specify whether this is a new concept or intentional replacement. Missing versus zero and multiple observations require explicit semantics.

Search with named pattern/src_name arguments in search_concepts and inspect list_concepts/provenance. Check database columns or an authoritative data dictionary. Prefer source-specific evidence to guessed IDs. If a definition requires scientific judgment the user has not supplied, present candidates and unresolved choices.

## Supplemental configuration route

Use a new project CSV for supported atomic mappings. Inspect the installed `rendehr_concepts_config.csv` for time-series definitions or `rendehr_duration_concepts_config.csv` for duration definitions; do not assume the two files share a column order. Resolve files with ehr_config_path and installed documentation rather than copying a development checkout path.

Inspect the selected layer's parser and a comparable supported entry when necessary. Relevant fields can include concept_name, source, table_name, sub_var, val_var, unit_var, index_var, end_var, itm_class, match_value, method, typ, conversion, and descriptive metadata. Use the actual installed header and required semantics, not a invented universal schema.

At this baseline rows are combined with packaged definitions and deduplicated by source plus concept_name with supplemental rows winning. A single replacement row can displace existing mappings for that key; examine all existing rows before overriding. Source values use the source family, not automatically the demo variant. CSV writing must quote commas, quotes, and newlines correctly.

An explicit user_concepts path wins over the standing external-root supplemental path. Do not silently edit the packaged CSV or global definitions when the user requests a project-only extension. For code-only output, return a draft row/file description and execution code without writing or running it.

```r
# Execution example only: prerequisites and approved CSV must already exist.
result <- rendehR::run_concepts(
  src_name = src, db_name = database_name, base_dir = project_dir,
  concepts = rendehR::concept_options(
    user_concepts = supplemental_csv,
    layers = "ts", concept_name = new_concept_name
  )
)
```

Use dur instead of ts only for the matching duration contract. concept_name filters flat/ts/dur/user, not ehr/external. Do not claim an external-module run is limited to one name solely by this argument.

## SQL route

Inspect the source query, dependencies, dialect, cohort restrictions, joins, time alignment, output schema, and cardinality. Adapting schema prefixes is not a SQL translation or a guarantee of compatible concept output. Equal timestamp joins are not a general method of aligning clinical measurements.

Pure adaptation example (no connection and no file saving):

```r
adapted_sql <- rendehR::register_concept_sql(
  sql = user_sql, src_name = src, concept_name = new_concept_name,
  schema = "concepts", save_to = FALSE, dry_run = TRUE,
  overwrite = FALSE
)
cat(adapted_sql)
```

Do not pass con to this preview: it triggers execution even with dry_run=TRUE. Default save_to=NULL writes a file, so it is not a no-write preview. Do not blindly execute third-party SQL or infer clinical validity from its citation.

For an authorized file draft, choose a new project path and explicitly check existing files before saving: overwrite=FALSE controls the SQL DROP prefix, not protection against replacing the saved SQL file. Check ehr_paths(src_name=src, base_dir=project_dir)$user_sql_dir for user-layer discovery. module changes the default external file destination; it does not alone establish an intended schema or correct dependencies.

Keep saved paths, CREATE target schema, run_concepts user_schema/external_schema settings, and later retrieval consistent. A concept in derived or external schemas is not guaranteed accessible through fetch_concept, which targets the concepts schema; use fetch_table with the actual schema.

## Existing project boundaries

List files or tables to be added/replaced before applying an extension. Inspect collisions and avoid overwriting existing concepts under the same name without an intentional replacement request. Use public registration/extraction functions; if the contract cannot represent the requested variable, explain the gap rather than patching package internals.

Evidence: R/extend.R, R/atomic_concepts.R, R/pipeline.R and concept_options/register_concept_sql help. User SQL/config remains unvalidated until the checks in the companion validation guide run.

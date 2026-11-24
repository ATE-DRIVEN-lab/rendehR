# Validate and integrate a concept

Separate four states: definition drafted, artifact saved, concept materialized, and harmonized output verified. Success at one does not imply the next.

## Before execution

Verify required source/window/dependency tables, source version, identifiers, column types, units/conversions, point/interval semantics, NULL handling, and cohort restriction. Review generated SQL for ambiguous joins and duplicated measurements. When replacing an existing definition, record the old definition and outputs that depend on it.

For a request limited to preview, return code and this validation plan; do not connect or write simply because a function offers dry_run. register_concept_sql(dry_run=TRUE, save_to=FALSE) with no con is the SQL preview route. run_atomic_concepts_unified can generate without a connection but still writes SQL to sql_out_dir. run_concepts dry_run can open a database, initialize schemas, and write SQL.

## Execute a bounded example

Use the user's authorized demo/fixture and only the needed layer/dependencies. Inspect returned statuses: an empty vector/list can mean no matching definition, not successful extraction. Preserve actual failures and report any partial output. Do not default to enabling clinical-score or unrelated concept layers.

Check:

- Expected columns and identifier grain; missing keys and duplicates at the defined event grain.
- Expected nonempty/empty behavior based on fixture evidence, not a universal requirement that every rare concept has observations.
- Original and converted units, sample values, plausible bounds agreed for the definition, and missingness.
- Time origin, boundary cases, interval direction/duration, and multiple measurements.
- A small independently computed example from source records, kept local.

verify_concepts checks table availability, not all these invariants. Match source-specific prerequisites; do not treat MIMIC column names as universal.

## Provenance

Record actual origin label, source URL/reference if known, definition file, source/version, and transformation notes. Do not fabricate a citation. register_concept_provenance writes a database registry and defaults to overwriting entries; it is not a read-only report or a local CSV writer.

```r
# Write only after the actual materialized schema and provenance are verified.
rendehR::register_concept_provenance(
  src_name = src, db_name = database_name,
  concept_name = new_concept_name, schema_name = actual_schema,
  origin_name = origin_label, origin_url = origin_url,
  origin_ref = origin_reference, base_dir = project_dir,
  overwrite = FALSE
)
```

Check whether registration/execution already recorded provenance before adding it again. SQL adaptation preview alone does not establish a registry entry.

## Harmonization and downstream work

Determine the output contract required for the intended harmonized modality, including names, ID/time columns, value/unit fields, and relevant config. A successful CREATE TABLE does not prove the harmonizer will consume it.

After authorized extraction, inspect whether source harmonized tables need rebuilding; ICU projections and merged outputs may also depend on the changed concept. Present this as a dependency review, not a guaranteed automatic minimal rerun plan. Verify inclusion and values in the final output, not only the concept table.

Return paths, definition, provenance, exact operations performed, checks passed/failed/not run, and remaining limitations. Keep patient-level evidence in the project. New concept validation does not certify cross-source scientific equivalence.

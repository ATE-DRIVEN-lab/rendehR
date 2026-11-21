# Combine prepared sources

Use only after each selected source has the requested harmonized outputs. This pools existing outputs; it does not load raw datasets or establish scientific comparability.

## Establish the target

Confirm source list, target database/schema, admission versus ICU time-series grain, required modalities, and whether the user wants a new result or an existing target replaced. Prefer a fresh user-selected schema for the first example. Inspect target existence before writing.

- Admission grain reads native ts_tab; ICU grain requires each source's prepared ts_icu.
- Duration uses dur_tab even when ICU time-series grain is requested; do not describe every modality as ICU-projected.
- Retain source_data and native/grain identifiers. Inspect event_id and assigned/native ICU identifiers when reasoning about projected events.
- Shared names do not guarantee matching units, definitions, ascertainment, or release coverage. Report differences and missingness rather than silently equating them.

## Generate the merge call

```r
# Execute only after inspecting the intended new/replaceable target.
rendehR::merge_sources(
  src_names = selected_sources,
  db_name = database_name,
  base_dir = project_dir,
  target_schema = output_schema,
  merge_ts = TRUE, merge_dur = FALSE,
  merge_flat = FALSE, merge_cohort = FALSE,
  append = FALSE, analysis_level = "admission"
)
```

This is a ts-only example, not an instruction to omit other requested modalities. append=FALSE rebuilds the selected target tables inside a single transaction, and both modes roll back on failure, so a failed merge leaves the previous state consistent. append=TRUE replaces rows for specified sources within existing table contracts; it is not blind insertion and requires existing target tables. Neither mode is a read-only check.

For ICU output, verify make_icu_timeline prerequisites and use analysis_level="icu" only when intended. Do not silently generate projections as part of a merge-only request.

## Inspect and retrieve

Use load_merged_ts, load_merged_flat, load_merged_dur, and load_merged_cohort_info for the modalities actually built. Check current help before passing filters or aggregation options.

```r
preview <- rendehR::load_merged_ts(schema = output_schema, n = 100, con = con)
```

A limited preview does not establish complete counts. Compare per-source and per-modality totals with their prepared inputs, check source_data, required nonmissing grain keys, duplicated keys at the actual event/table grain, and unit coverage. Repeated admissions in time-series data can be legitimate; do not deduplicate by admission alone.

The baseline merge code assumes grain-defining IDs are nonmissing when composing ehr_id; check that assumption before relying on uniqueness. An ehr_id is not by itself proof of a globally unique event. Export only after verification and retain source provenance.

Report source versions where available, target, selected modalities/grain, counts/checks, partial failures, and unresolved comparability issues. This is operational merging, not a semantic-equivalence audit.

# Changelog

## rendehR 1.0.0

First public release.

### Changes from the internal builds

- Antibiotics are a medication exposure, `antibiotics_exp`, in the
  duration concepts (`rendehr_duration_concepts_config.csv`) instead of
  the time-series concept `antibiotics`. Each order or administration
  keeps its start and gains its end and duration (prescriptions, MIMIC
  `inputevents`, eICU `medication`); doses are still stored in mg. Code
  that read `antibiotics` from the time-series tables should read
  `antibiotics_exp` from the duration tables.
- The duration concept configuration is renamed from
  `hddl_med_dur_config.csv` to `rendehr_duration_concepts_config.csv`. A
  project override saved under the old name is no longer read; rename
  it.
- `import_cohort_icd_rbackend()` and the source registry’s `cohort_fn`
  field are removed. Cohorts are selected by
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md).
  [`ehr_add_source_assistant()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_add_source_assistant.md)
  and
  [`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md)
  lose their `cohort_fn` and `cohort_strategy` arguments.

### Cohorts and concepts

- The bundled ICD diagnosis dictionary (`d_icd_diagnoses.csv`) is
  rebuilt from the public CMS ICD-9-CM (v23-v32) and ICD-10-CM
  (FY2016-FY2025) files by `data-raw/d_icd_diagnoses.R`. It covers every
  code of the previous dictionary except three ICD-9 category headers
  (780.0, V12.7, V58.1). Eleven retired ICD-9 codes in eICU diagnoses
  that were unmatched now resolve to ICD-9.

- SCI stays are classified from their SCI codes only. A stay with any
  cord, cord-symptom or possible-cord code (`is_sci` 1, 3 or 999) is
  SCI, and its second level is traumatic, non-traumatic or unspecified
  depending on those codes. There are now six second-level categories
  instead of eight; the new `scid_trauma_types` column lists the trauma
  status of the SCI codes.

- eICU medication tables keep their raw columns and add parsed `mod_*`
  columns (drug name, dose or rate, unit, start and stop) in
  `medication_mod`, `infusiondrug_mod` and `admissiondrug_mod`. Orders
  with a blank drug name (36 % in eICU 2.0) get their name from the drug
  code through `rendehr_eicu_hicl_drugname_lookup.csv`, which a file of
  the same name in the project config directory replaces. Infusion doses
  are now the charted rate, not the amount in the bag.

- Some eICU exposure patterns match better: hydromorphone also matches
  the misspelling “Hydromorphine”, acetaminophen and hydrocodone no
  longer match chlorpheniramine, lepirudin matches its recombinant form,
  and linaclotide is no longer counted as sodium chloride.

- eICU ionized calcium is labelled mg/dL throughout, but some hospitals
  report mmol/L under that label. The new conversion
  `ca_ionized_by_value` reads the unit from the value (up to 2 is
  mmol/L, 3.5 or more is mg/dL, the gap is left empty); about a quarter
  of eICU values were mmol/L wrongly divided by 4.

- Calcium bounds follow ricu: 4-20 mg/dL for total and 0.5-2 mmol/L for
  ionized calcium (they were 0-10000). Bounds only affect opt-in range
  cleaning.

- eICU FiO2 also reads the “FIO2 (%)” label; FiO2 has a lower bound of
  20 %.

- pH, INR, GCS and RASS carry a unit (`pH`, `ratio`, `score`) instead of
  the text `'NA'`.

- Hospital discharges to hospice are `hospice` in
  `hospital_discharge_location` (they were `death`; nearly all of these
  patients left the hospital alive).

- Concepts without a unit (medication exposures, flags) store a missing
  unit instead of the text `'NA'`.

- The concept registry credits each concept’s origin: concepts defined
  by rendehR name the package, BlendedICU medication concepts cite its
  paper, and eICU derived concepts link the MIT-LCP eICU-CRD code. The
  time-series configuration no longer names PhysioNet as the origin.

- ICD code columns of the clinical lookups (SCI list, injury causes,
  procedure classification, dictionaries) and of a user’s own ICD list
  are read as text, so codes with leading zeros such as `0331` keep
  them.

- [`validate_concept_config()`](https://ate-driven-lab.github.io/rendehR/reference/validate_concept_config.md)
  checks a concept configuration and lists each invalid row. The concept
  stage runs it first and warns once about every problem. Rows with an
  empty `match_value` or an unknown `itm_class` are skipped and named in
  the warning, and a missing `user_concepts` file stops the stage.

### Loading data

- [`load_merged_ts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/load_merged_ts_wide.md)
  and
  [`fetch_concepts_wide()`](https://ate-driven-lab.github.io/rendehR/reference/fetch_concepts_wide.md)
  can drop values outside each concept’s plausible range before
  aggregating (`clean = "range"`, bounds from the configuration or
  `ranges =`). Each concept gains an `_n_cleaned` column and the result
  a `clean_report`.
- `load_merged_ts(window_min = c(from, to))` keeps only rows in that
  window, filtered in the database, on the admission or ICU clock
  (`window_ref`).

### Explorer

- Cohort Explorer: *Trajectories* shows the median and IQR of a concept
  over the first hours or days of the stay by group; *Compare* puts two
  filter selections side by side with SMDs and outcome rates; *Show R
  code* gives the code that reproduces the current selection.
- Concepts: *Availability* shows how many cohort admissions have each
  concept, *Quality* its units and values outside the plausible range,
  and the medication library adds exposure by day.
- Subject Explorer: filters by age and admission type, the source
  definition of a clicked point, a second patient drawn dashed for
  comparison, and timed procedures on the timeline.
- Every page has a help icon, the link icon reopens a view with its
  filters, and plots save as SVG.
- `run_explorer(cache_dir =)` keeps the data bundle between runs, so
  later starts skip reading the merged tables. It is off by default
  because the file holds individual-level data.

### Licence, documentation and interfaces

- rendehR is now licensed under the GNU General Public License, version
  3 (GPL-3), the license of ricu, on whose concept definitions part of
  the concept layer is modelled. The MIT-licensed material it uses
  (MIT-LCP MIMIC-Code and eICU-Code SQL, the BlendedICU medication list
  with OMOP ingredient IDs) keeps its copyright notices in
  `inst/COPYRIGHTS`.

- The documentation is now a set of tested articles: get started,
  projects and configuration, cohorts, the pipeline, harmonized data,
  concepts, the explorer, Sepsis-3 cohorts, the setup assistant, adding
  a source, and the MCP server. Every example was run on the open demo
  data; the vignettes are precomputed, so building the package renders
  stored outputs without a database or network.

- The MCP bundle is version 1.0.0, the package version, with three
  skills (`rendehr-user`, `rendehr-add-concept`, `rendehr-add-source`)
  and nine read-only tools. Tool arguments must be a JSON object,
  numbers keep full precision, `list_loaded_concepts` reports the total,
  and `cohort_summary` errors when the source has no cohort tables.

- The bundled example cohorts are `scid` (spinal cord injury, ICD list)
  and `sepsis3` (Sepsis-3 SQL); the traumatic brain injury list (`tbi`)
  was removed.

- The retired cohort entry points (`import_cohort()`,
  `import_cohort_flex()`, `run_cohort_duckdb()`,
  `select_cohort_ids_duckdb()`) have no aliases: use
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md),
  [`materialize_cohort_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/materialize_cohort_duckdb.md)
  and
  [`select_cohort_ids()`](https://ate-driven-lab.github.io/rendehR/reference/select_cohort_ids.md).

- [`drop_database()`](https://ate-driven-lab.github.io/rendehR/reference/drop_database.md)
  gains a `confirm` argument, so apps can pass the name the user typed;
  non-interactive sessions no longer wait at a console prompt.

- New projects default to the `"generic"` cohort profile. Set
  `cohort.profile = "scid"` for the SCI reference implementation, which
  also turns on its clinical enrichment defaults.

- The bundled SCI ICD dictionary is the v2.2.3 dictionary
  (`scid_icds_rendehr.rds`, the default; the CSV is the same table). It
  keeps the same codes and SCI/trauma flags, and describes location with
  `anatomical_region` and `specific_level` in snake_case. In
  `cohort_info`, `scid_injury_level` now holds the region (`"cervical"`,
  `"thoracic"`, …), `scid_injury_location` the coded level (`"c5"`,
  `"t11_t12"`, …), and the non-traumatic placeholder is
  `"not_applicable"`.

- [`cohort_summary()`](https://ate-driven-lab.github.io/rendehR/reference/cohort_summary.md)
  counts people rather than admission rows; SCI enrichment runs only for
  diagnosis-selected SCI cohorts; user SQL in subfolders of
  `concepts_sqls/user` is executed; ICU-level setup scripts build the
  cohort table at ICU grain;
  [`run_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_concepts.md)
  stops when a source’s raw modifications are missing; a relative
  `base_dir` resolves to an absolute path once; and a cohort selection
  plan prints its counts without the identifiers.

- The cohort explorer
  ([`run_explorer()`](https://ate-driven-lab.github.io/rendehR/reference/run_explorer.md))
  has a simpler design and a rebuilt Setup page that configures a
  project for several sources and runs the pipeline stages for all of
  them, and a concept builder that drafts, previews and saves new or
  edited time-series concepts as a user_concepts file. Small-cell
  masking now also suppresses the cells and percentages from which a
  masked count could be recovered, and Kaplan-Meier curves use one index
  admission per patient.

## rendehR 0.1.0

First labelled internal release. rendehR downloads, imports,
cohort-filters and harmonizes ICU EHR data (MIMIC-III, MIMIC-IV, eICU)
into analysis-ready flat, time-series and duration tables, and merges
sources into one schema.

### Highlights

- A cohort explorer
  ([`run_explorer()`](https://ate-driven-lab.github.io/rendehR/reference/run_explorer.md))
  with the logo’s brand theme: cohort overview (accrual, outcome
  heatmap, age pyramid, length of stay), Table 1 with SMDs,
  distributions, outcomes with Wilson CIs, Kaplan-Meier survival,
  long-term trends, cross-tabs, scatter plots, and a per-patient subject
  explorer that reads live time series and verifies source vs merged
  reads.
- Management pages in the same app: Setup applies a project
  configuration on this machine and runs the pipeline stages as
  background tasks with live logs; Cohorts launches a cohort import
  (definition, ICD filter, ID list); Package manages libraries, sources,
  databases and exports.
- An independent validation harness (the `validation/` directory, kept
  on the `dev` branch only): demo and full-profile runs, including a
  concordance level against ricu; results and the findings register live
  in `validation/Report.docx`.
- A local MCP bundle with three skills (`inst/mcp`, version 0.3.2).

### Notes

- Concept storage rounds unit-converted values to 3 decimals; raw values
  keep full source precision (`value_raw`). This rounding is a
  deliberate compatibility decision.
- Vignettes in 0.1.0 were instructive only and used `eval = FALSE`.
  Since 1.0.0 they are precomputed on the open demos.

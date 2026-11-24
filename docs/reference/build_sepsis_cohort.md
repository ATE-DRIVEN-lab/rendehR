# Build a Sepsis-3 cohort ID list for a source, optionally importing it

Source-aware Sepsis-3 cohort selection: resolves the right packaged
`inst/sql/{family}/cohorts/sepsis3/` SQL chain via
`src_family(src_name)`, runs it over the raw CSVs through the
DuckDB-backed `cohort_sql`/ `setup_sql` selection route (suspicion of
infection + absolute SOFA \>= 2 within the suspicion window
`[-48h, +24h]` - see each chain's own README for the full clinical
definition and per-family adaptations), and returns the matching cohort
IDs. When `import = TRUE` (the default), also feeds the result straight
into `import_cohort_general(direct_ids_path = ...)` for you - no
separate manual CSV round-trip needed.

## Usage

``` r
build_sepsis_cohort(
  src_name,
  base_dir = NULL,
  raw_data_root = NULL,
  out_csv = NULL,
  import = TRUE,
  overwrite = FALSE,
  bundle = "sepsis3",
  import_scope = "hospital",
  memory_limit = NULL,
  threads = NULL,
  show_progress = TRUE
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iii_demo").

- base_dir:

  Project root with the source's raw demo data.

- raw_data_root:

  Optional parent storage-root override for raw source data. The source
  directory is derived as `{raw_data_root}/{src_name}`. Forwarded to
  both the cohort-selection step (`.ehr_select_cohort_ids_duckdb()`) and
  the import step
  ([`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)),
  so both agree on where the raw CSVs live.

- out_csv:

  Optional path to write the full cohort_sql result (every ID column
  selected, not just `key`) to CSV. If NULL and `import = TRUE`, a
  temporary file is used internally and removed afterward.

- import:

  If TRUE (default), import the selected cohort via
  `import_cohort_general(direct_ids_path = ...)`.

- overwrite:

  Passed to
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
  when `import = TRUE`. Default FALSE.

- bundle:

  Which cohort definition to run against the packaged Sepsis-3 SQL chain
  (the `setup_sql` chain itself is unchanged either way). Default
  `"sepsis3"`: every ICU stay that met Sepsis-3 criteria, with the
  qualifying suspicion-of-infection event anywhere in the stay - this
  answers "was this patient ever septic during their ICU course", which
  runs much higher than admission-time sepsis-prevalence studies (e.g.
  on `mimic_iii_demo`, 95 ICU stays of 70 patients are captured).
  `"first_sepsis3"`: further restricted to only a patient's absolute
  first-ever ICU stay (septic or not) - the `sepsis3-mimic` (Johnson et
  al. 2018) study-cohort restriction (`mimic_iii_demo`: 95 -\> 66
  stays). For eICU, cross-hospitalization ordering for `"first_sepsis3"`
  is only approximate (eICU has no exact cross-admission timestamp, only
  a year-granularity discharge year), so ties within the same discharge
  year are broken by `patienthealthsystemstayid`: reproducible, though
  not necessarily chronological. `"sepsis3_admission"`: instead
  restricted to stays whose qualifying suspicion-of-infection event
  falls within +/-1 day of ICU/unit admission - the sepsis3-mimic
  suspicion-timing exclusion; a smaller effect on its own
  (`mimic_iii_demo`: 95 -\> 88). `"first_sepsis3_admission"`: both
  restrictions together (`mimic_iii_demo`: 61) - the closest packaged
  match to the sepsis3-mimic study population, but still well above that
  study's own suspected-infection count (`sepsis_suspicion`: 28,
  pre-SOFA) because it doesn't reproduce that study's separate
  adult/MetaVision/CSURG exclusions - those are specific to that one
  MIMIC-III-era study, not part of the Sepsis-3 clinical definition
  shared with MIMIC-IV/eICU, and live in
  `sepsis_suspicion`/`sepsis_suspicion_modified` instead.

- import_scope:

  Passed to
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
  when `import = TRUE`. Default `"hospital"`: for every
  Sepsis-3-positive ICU stay, also pulls in any other ICU stay within
  the SAME hospitalization, but not a patient's other, separate
  hospitalizations. Sepsis-3 is common enough in a general ICU
  population (58% of all `mimic_iii` patients qualify at least once)
  that the engine's own default of `NULL` (full patient history, every
  hospitalization a patient ever had) ends up including ~83% of the
  entire `chartevents` table - almost the whole dataset, not a focused
  cohort. Pass `import_scope = NULL` to restore that full
  patient-history behavior, or `import_scope = "icu"` to narrow further
  to just the exact Sepsis-3-positive stays themselves, with no sibling
  admissions or stays at all. Changing this always requires a full
  rebuild (`overwrite = TRUE`) - there's no cheap way to re-scope an
  already-built cohort.

- memory_limit, threads:

  Optional DuckDB resource settings forwarded to the cohort-selection
  step (`.ehr_select_cohort_ids_duckdb()`) - the `setup_sql` chain scans
  full raw tables (e.g. a full CHARTEVENTS), which can exceed DuckDB's
  default memory cap on a memory-constrained machine. Unlike
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)'s
  cohort-table build, this chain runs as plain GROUP BY/window SQL
  (SOFA, vitalsign, ...) and cannot be split into row-hash chunks the
  way a straight table copy can.

- show_progress:

  Print each `setup_sql` file before execution, followed by elapsed
  seconds on completion or failure. Also forwarded to
  [`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
  for the import half.

## Value

Invisibly returns a one-column data frame of unique `key` values (the
Sepsis-3 cohort IDs).

## Details

Works for any source family with a packaged chain (`mimic_iii`,
`mimic_iv`, `eicu`).

## Examples

``` r
if (FALSE) { # \dontrun{
ids <- build_sepsis_cohort("mimic_iii_demo", base_dir = "/path/to/project")
# Selection only, no import:
ids <- build_sepsis_cohort("eicu_demo", base_dir = "/path/to/project",
                           import = FALSE)
# Full patient history instead of just the septic hospitalization:
ids <- build_sepsis_cohort("mimic_iii_demo", base_dir = "/path/to/project",
                           import_scope = NULL, overwrite = TRUE)
# Only each patient's first-ever ICU stay (Johnson et al. 2018 restriction):
ids <- build_sepsis_cohort("mimic_iii_demo", base_dir = "/path/to/project",
                           bundle = "first_sepsis3")
} # }
```

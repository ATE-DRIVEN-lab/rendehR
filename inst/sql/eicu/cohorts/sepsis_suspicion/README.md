# Suspected-infection cohort — eICU (no SOFA)

The eICU counterpart of MIMIC-III's `sepsis_suspicion_modified` and MIMIC-IV's
`sepsis_suspicion`: select stays with **suspicion of infection only — SOFA is
NOT checked.**

It is just the suspicion half of the [`../sepsis3/`](../sepsis3) chain. For
eICU, suspicion is **antibiotic-based** (per ricu), because eICU has almost no
culture data — see the sepsis3 README. Dropping the SOFA files selects the
broader suspected-infection population.

## Files (dependency order)

| File | Table | From |
|------|-------|------|
| `01_antibiotic.sql` | `antibiotic` | `infusiondrug` + `medication` (ricu abx regexes) |
| `02_suspicion_of_infection.sql` | `suspicion_of_infection` | `antibiotic` (+ `microlab` as annotation) |

`suspicion_of_infection` has one row per antibiotic administration, with
`patientunitstayid`, `suspected_infection` (always 1 here) and
`suspected_infection_offset`.

## Usage

```r
setup <- sort(list.files(
  system.file("sql/eicu/cohorts/sepsis_suspicion", package = "rendehR"),
  pattern = "\\.sql$", full.names = TRUE))

ids <- select_cohort_ids_duckdb(
  "eicu_demo",
  # suspicion_of_infection carries all three eICU ID levels
  cohort_sql = "SELECT DISTINCT uniquepid, patienthealthsystemstayid, patientunitstayid
                FROM suspicion_of_infection WHERE suspected_infection = 1",
  key        = "patientunitstayid",
  setup_sql  = setup,
  out_csv    = "eicu_sepsis_suspicion_ids.csv")

import_cohort("eicu_demo", direct_ids_path = "eicu_sepsis_suspicion_ids.csv", overwrite = TRUE)
run_db_duckdb("eicu_demo")
```

## Verified

On `eicu_demo`: **582 stays** with suspected infection (of 2520 ICU stays).
For contrast, the SOFA-confirmed [`../sepsis3/`](../sepsis3) selects 448.

## Notes

- eICU suspicion = an antibiotic administration; `suspected_infection` is always
  1 in this table, so `WHERE suspected_infection = 1` simply keeps every
  antibiotic-treated stay.
- For the SOFA-confirmed Sepsis-3 cohort instead, use [`../sepsis3/`](../sepsis3)
  (`SELECT DISTINCT patientunitstayid FROM sepsis3 WHERE sepsis3`).

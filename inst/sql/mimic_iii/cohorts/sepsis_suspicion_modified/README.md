# Suspected-infection cohort — Johnson 2018 modified, relaxed exclusions (MIMIC-III)

Same as [`../sepsis_suspicion/`](../sepsis_suspicion) (the `sepsis3-mimic` study
cohort: antibiotic + culture suspicion via POE), **but with only one
exclusion**: stays that lack chartevents data.

The Johnson study's other exclusions — non-first ICU stay, non-adult, cardiac
surgery (CSURG/VSURG/TSURG), CareVue-era (`dbsource`), and suspicion outside
±1 day of ICU admission — are still computed as informational flags in
`sepsis3_cohort` but **do not** drive `excluded` here. Only
`HAS_CHARTEVENTS_DATA = 0` sets `excluded = 1`.

## Files

Identical to `sepsis_suspicion/` except `04_sepsis3_cohort.sql`, whose `excluded`
flag is reduced to the chartevents check. Files: `01_abx_poe_list`,
`02_abx_micro_poe`, `03_suspinfect_poe`, `04_sepsis3_cohort`.

## Usage

```r
setup <- sort(list.files(
  system.file("sql/mimic_iii/cohorts/sepsis_suspicion_modified", package = "rendehR"),
  pattern = "\\.sql$", full.names = TRUE))

ids <- select_cohort_ids_duckdb(
  "mimic_iii_demo",
  cohort_sql = "SELECT DISTINCT subject_id, hadm_id, icustay_id FROM sepsis3_cohort
                WHERE excluded = 0 AND suspected_of_infection_poe = 1",
  key        = "subject_id",
  setup_sql  = setup,
  out_csv    = "sepsis_suspicion_modified_ids.csv")
```

## Verified — vs. `sepsis_suspicion` on `mimic_iii_demo`

| cohort | stays | subjects |
|--------|-------|----------|
| `sepsis_suspicion` (all exclusions) | 28 | 28 |
| `sepsis_suspicion_modified` (chartevents only) | 109 | 78 |
| **difference** | **+81** | **+50** |

Relaxing the exclusions mainly re-admits non-first ICU stays and CareVue-era
stays: `sepsis_suspicion` keeps one stay per subject (first-stay-only), whereas
this cohort keeps every suspected-infection stay that has chartevents.

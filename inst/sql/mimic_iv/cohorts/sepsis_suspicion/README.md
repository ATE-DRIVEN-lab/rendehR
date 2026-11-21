# Suspected-infection cohort — MIMIC-IV (no SOFA)

The MIMIC-IV counterpart of MIMIC-III's
[`sepsis_suspicion_modified`](../../../mimic_iii/cohorts/sepsis_suspicion_modified):
select stays with **suspicion of infection only — SOFA is NOT checked.**

It is just the suspicion half of the [`../sepsis3/`](../sepsis3) chain
(mimic-code): antibiotics paired with cultures. Dropping the SOFA computation
(the 19 component/score files) makes it much faster and selects the broader
"suspected infection" population rather than the SOFA-confirmed Sepsis-3 set.

MIMIC-IV needs no exclusion step: mimic-code's suspicion definition applies no
adult/first-stay/dbsource filters, so this is the natural analogue of
`sepsis_suspicion_modified` (which relaxes the Johnson exclusions to nothing but
the chartevents check).

## Files (dependency order)

| File | Table | From |
|------|-------|------|
| `01_antibiotic.sql` | `antibiotic` | `prescriptions` + `icustays` (mimic-code antibiotic drug list) |
| `02_suspicion_of_infection.sql` | `suspicion_of_infection` | `antibiotic` + `microbiologyevents` (±72h/24h) |

`suspicion_of_infection` carries `subject_id`, `hadm_id`, and `stay_id`, plus
`suspected_infection` (0/1) and `suspected_infection_time`.

## Usage

```r
setup <- sort(list.files(
  system.file("sql/mimic_iv/cohorts/sepsis_suspicion", package = "rendehR"),
  pattern = "\\.sql$", full.names = TRUE))

ids <- select_cohort_ids_duckdb(
  "mimic_iv_demo",
  cohort_sql = "SELECT DISTINCT subject_id, hadm_id, stay_id
                FROM suspicion_of_infection WHERE suspected_infection = 1",
  key        = "subject_id",
  setup_sql  = setup,
  out_csv    = "sepsis_suspicion_ids.csv")

import_cohort("mimic_iv_demo", direct_ids_path = "sepsis_suspicion_ids.csv", overwrite = TRUE)
run_db_duckdb("mimic_iv_demo")
```

## Verified

On `mimic_iv_demo`: **74 subjects / 143 hadm_id / 82 ICU stays**
(`suspected_infection = 1`).

## Notes

- Suspicion is **hospitalization-level**: `stay_id` is NULL for antibiotics
  given outside an ICU stay. Add `AND stay_id IS NOT NULL` to the `cohort_sql`
  to restrict to ICU stays; keep `subject_id`/`hadm_id` for the broader set.
- For the SOFA-confirmed Sepsis-3 cohort instead, use [`../sepsis3/`](../sepsis3)
  (`SELECT DISTINCT subject_id FROM sepsis3 WHERE sepsis3`).
- `out_csv` contains every column the `cohort_sql` selects, so all ID levels are
  available; `import_cohort()` reads only its `key` column.

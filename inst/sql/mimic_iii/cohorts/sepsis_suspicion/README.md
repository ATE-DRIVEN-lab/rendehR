# Suspected-infection cohort — Johnson 2018 (sepsis3-mimic), MIMIC-III

The original **`sepsis3-mimic`** study cohort (Johnson et al., *Crit Care Med*
2018, PMID:29303796; https://github.com/alistairewj/sepsis3-mimic), packaged as
a DuckDB cohort selector. Runs over the raw PhysioNet CSVs via
`select_cohort_ids_duckdb()`.

This is the **suspected-infection study population** — antibiotic + culture
suspicion (POE) plus the study's inclusion/exclusion rules. It is a different
definition from the sibling [`../sepsis3/`](../sepsis3), which applies the
mimic-code-style **SOFA ≥ 2** confirmation (matching the MIMIC-IV / eICU
chains). Use this one when you want the Johnson study cohort specifically.

## Files (dependency order)

| File | Table | What it does |
|------|-------|--------------|
| `01_abx_poe_list.sql` | `abx_poe_list` | antibiotic drug list from `prescriptions` |
| `02_abx_micro_poe.sql` | `abx_micro_poe` | antibiotics linked to `microbiologyevents` cultures (±72h/24h) |
| `03_suspinfect_poe.sql` | `suspinfect_poe` | first suspicion-of-infection time per ICU stay |
| `04_sepsis3_cohort.sql` | `sepsis3_cohort` | study cohort with inclusion/exclusion flags (`excluded`, `suspected_of_infection_poe`); carries `subject_id` |

Touches `prescriptions`, `microbiologyevents`, `icustays`, `admissions`,
`patients`, `services`. Each file is a single `CREATE OR REPLACE TABLE`
statement over the raw-CSV views (DuckDB-adapted: `epoch(...)`,
`INTERVAL n DAY`, no schema prefixes).

## Inclusion / exclusion

`excluded` encodes the study rules: adult, **first** ICU stay, **MetaVision**
only (`dbsource`), has chartevents, and suspicion within ±1 day of ICU
admission. `suspected_of_infection_poe = 1` marks a suspected infection.

## Usage

```r
setup <- sort(list.files(
  system.file("sql/mimic_iii/cohorts/sepsis_suspicion", package = "rendehR"),
  pattern = "\\.sql$", full.names = TRUE))

ids <- select_cohort_ids_duckdb(
  "mimic_iii_demo",
  # sepsis3_cohort carries subject_id, hadm_id and icustay_id; select whichever
  # ID levels you want in the CSV. `key` is the column import_cohort filters on.
  cohort_sql = "SELECT DISTINCT subject_id, hadm_id, icustay_id FROM sepsis3_cohort
                WHERE excluded = 0 AND suspected_of_infection_poe = 1",
  key        = "subject_id",
  setup_sql  = setup,
  out_csv    = "sepsis_suspicion_ids.csv")   # CSV gets all three ID columns

import_cohort("mimic_iii_demo", direct_ids_path = "sepsis_suspicion_ids.csv", overwrite = TRUE)
run_db_duckdb("mimic_iii_demo")
```

## Verified

On `mimic_iii_demo`: **28 subjects** (of 100 subjects / 136 ICU stays).

## Scope / caveats

- **What this selects:** the eligible, suspected-infection cohort — it does
  **not** gate on SOFA ≥ 2. Full Johnson Sepsis-3 = this + SOFA ≥ 2; the
  Johnson SOFA comes from the `sepsis3-mimic` severity-score suite
  (SOFA/SIRS/LODS/qSOFA + ~40 mimic-code concepts with Postgres helper
  functions), which is not ported here. For a SOFA-confirmed Sepsis-3 cohort,
  use [`../sepsis3/`](../sepsis3).
- The MetaVision-only exclusion drops CareVue-era stays by design.

# Sepsis-3 cohort — SQL setup chain (eICU-CRD)

The eICU counterpart of the MIMIC-IV Sepsis-3 chain: **define the cohort in
SQL that runs over the raw PhysioNet CSVs** (via DuckDB), get the patient IDs,
then import only that cohort — no pre-built database required.

```
run_one_source(cohort_sql = ..., setup_sql = ...)   run this chain over raw
      |                                              eICU CSV views -> ID list,
      v                                              build cohort parquet, load
[same pipeline as any other source]                  into Postgres - all one call
```

## Definition

Sepsis-3 = suspicion of infection **and** absolute SOFA ≥ 2 within the
suspicion window `[SI_time − 48h, SI_time + 24h]` (mimic-code onset rule,
baseline SOFA 0). The eICU-specific pieces follow ricu
(`ricu-main/R/callback-sep3.R`):

- **Suspicion of infection = antibiotic administration.** eICU has almost no
  body-fluid culture data (`microlab` covers <3% of stays), so — exactly as
  ricu does for eICU — an antibiotic administration is the SI event and its
  time is the SI time. Cultures are attached only as annotation, never
  required. (Requiring an antibiotic+culture pairing, as MIMIC does, selects
  essentially no eICU patients.)
- **SOFA** is a port of the mimic-code SOFA to eICU, built from the raw
  component tables. eICU simplifications: respiration = PaO₂/FiO₂ only (no
  ventilation flag); cardiovascular = any vasopressor → 2, MAP < 70 → 1 (doses
  unreliable); renal urine = 24h rolling sum of charted urine.

## Files (dependency order)

`01`–`06` build SOFA components from raw tables; `07` assembles hourly SOFA;
`08`–`10` build antibiotics → suspicion → the final `sepsis3` flag. Each file
is a single `CREATE OR REPLACE TABLE` statement (one `setup_sql` step), using
bare table names so it reads the raw-CSV views the DuckDB-backed
cohort-selection route exposes and the tables built by earlier files.

| File | Table | From |
|------|-------|------|
| `01_sep_lab.sql` | `sep_lab` | `lab` — bilirubin, creatinine, platelets |
| `02_sep_bg.sql` | `sep_bg` | `lab` — PaO₂, FiO₂ |
| `03_sep_map.sql` | `sep_map` | `nursecharting` — mean arterial pressure |
| `04_sep_gcs.sql` | `sep_gcs` | `nursecharting` — GCS |
| `05_sep_uo.sql` | `sep_uo` | `intakeoutput` — urine output |
| `06_sep_vaso.sql` | `sep_vaso` | `infusiondrug` + `treatment` — vasopressors |
| `07_sofa.sql` | `sofa` | hourly SOFA (24h window) |
| `08_antibiotic.sql` | `antibiotic` | `infusiondrug` + `medication` (ricu regexes) |
| `09_suspicion_of_infection.sql` | `suspicion_of_infection` | antibiotics (+ `microlab` annotation) |
| `10_sepsis3.sql` | `sepsis3` | SI window + SOFA ≥ 2 |

Touches 8 raw tables: `lab`, `nursecharting`, `intakeoutput`, `infusiondrug`,
`medication`, `treatment`, `microlab`, `patient`. All times are minutes from
unit (ICU) admission.

## Usage

The easiest path is `build_sepsis_cohort()` (R/pipeline.R) - a source-aware
helper covering all three families (`mimic_iii`, `mimic_iv`, `eicu`) via
`src_family(src_name)` dispatch, selecting and importing the cohort in one
call:

```r
ehr_connect("rendehr", creds)
build_sepsis_cohort("eicu_demo", overwrite = TRUE)
```

### Bundle options

The default (`bundle = "sepsis3"`) flags a stay as septic if *any* antibiotic
administration during the whole ICU stay has a matching SOFA≥2 hour nearby -
it answers "was this patient ever septic during their ICU course", which
runs much higher than admission-time sepsis-prevalence studies (448/2520
stays on `eicu_demo`). Narrower bundles restrict this, mirroring
`mimic_iii`/`mimic_iv`:

| `bundle` | Restriction | `eicu_demo` stays |
|---|---|---|
| `"sepsis3"` (default) | none | 448 |
| `"first_sepsis3"` | patient's absolute first-ever ICU stay only | 350 |
| `"sepsis3_admission"` | antibiotic within ±1 day of unit admission | 347 |
| `"first_sepsis3_admission"` | both of the above | 281 |

`run_sepsis3_example()` goes one step further and runs the full pipeline
(`run_db`, comorbidities, concepts, ...) afterward:

```r
run_sepsis3_example(src_name = "eicu_demo", base_dir = "/path/to/project")
```

For manual/advanced use, pass `cohort_sql`/`setup_sql`/`key` directly to
`run_one_source()` - this runs the same DuckDB-backed selection internally,
no separate CSV round-trip required:

```r
setup <- sort(list.files(
  system.file("sql/eicu/cohorts/sepsis3", package = "rendehR"),
  pattern = "\\.sql$", full.names = TRUE))

run_one_source(
  "eicu_demo",
  # the sepsis3 table carries all three eICU ID levels; select whichever you want
  cohort_sql = "SELECT DISTINCT uniquepid, patienthealthsystemstayid, patientunitstayid
                FROM sepsis3 WHERE sepsis3",
  key        = "patientunitstayid",
  setup_sql  = setup,
  overwrite  = TRUE)
```

eICU ID levels: `uniquepid` (patient) ⊃ `patienthealthsystemstayid` (hospital
admission) ⊃ `patientunitstayid` (ICU stay) — the analogue of MIMIC's
subject_id / hadm_id / stay_id. They come from the raw `patient` table (joined
in `09_suspicion_of_infection.sql`); `visit_windows` does not exist yet at
cohort-selection time. `key` stays `patientunitstayid` (what import filters
on). Use `SELECT DISTINCT patientunitstayid …` if you only want the one.

## Verified

On `eicu_demo` (2520 ICU stays): **448 Sepsis-3 stays (17.8%)**; SOFA in the
valid 0–24 range; whole chain runs in ~4s over the raw CSVs.

## Notes

- To make suspicion stricter (ricu's `abx_min_count` — e.g. ≥2 antibiotics
  within 24h), pre-filter `antibiotic` in `08_antibiotic.sql`; downstream files
  are unchanged.
- `sepsis3` is boolean; `WHERE sepsis3` keeps only patients meeting the
  criteria. The final SELECT uses `patientunitstayid` (eICU cohort key).

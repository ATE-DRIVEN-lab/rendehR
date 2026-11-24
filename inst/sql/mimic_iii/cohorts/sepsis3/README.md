# Sepsis-3 cohort — SQL setup chain (MIMIC-III)

The MIMIC-III counterpart of `inst/sql/mimic_iv/cohorts/sepsis3/`, using the
**same definition** as the MIMIC-IV and eICU chains: **suspicion of infection +
absolute SOFA ≥ 2 within the SI window `[SI_time − 48h, SI_time + 24h]`**
(mimic-code onset rule, baseline SOFA 0). Runs over the raw PhysioNet CSVs via
the DuckDB-backed `cohort_sql`/`setup_sql` selection route (see Usage below).


## Definitions: default and variants

There is no gold-standard way to find Sepsis-3 in ICU records. The chain's last
file builds the `sepsis3` table; `build_sepsis_cohort(definition = , bundle = )`
chooses which file that is.

- **Default** (`definition = "delta"`, `11_sepsis3.sql`): suspected infection is
  an antibiotic paired with a culture in the 72 h before or the 24 h after it, on an antibiotic course lasting at least 72 h (administrations less than
  24 h apart form one course; a course cut short by death within 72 h also
  counts), and sepsis is a SOFA rise of at least 2 points over the lowest SOFA of
  the stay so far within `[SI - 48h, SI + 24h]` (YAIB/ricu `delta_cummin`).
  Adults only (age >= 18 at ICU admission). Organ dysfunction present from ICU admission is the
  baseline, not sepsis.
- **`definition = "absolute"`** (`variants/sepsis3_absolute.sql`): the earlier
  rendehR rule, absolute SOFA >= 2 from a baseline of 0, described in the rest of
  this README; the demo counts below were measured with it.
- **`bundle = "trauma_sepsis3"`** (`variants/trauma_sepsis3.sql`): hospital-
  acquired sepsis in trauma admissions after ML4UWHealth/SepsisOnset_TraumaCohort
  - a blood culture at least 72 h after admission with a new intravenous
  antibiotic course of at least 4 days starting within 2 days of it, and a rise
  of at least 2 points of a SOFA without its GCS and urine-output parts within 3
  days of the culture; decided per hospital admission.

## Provenance

- **Suspicion of infection** (`01`–`03`) is ported from **`sepsis3-mimic`**
  (Johnson et al., *Crit Care Med* 2018) — antibiotic prescriptions paired with
  blood cultures — because MIMIC-III has rich microbiology. This replaces the
  study's inclusion/exclusion cohort (`04_sepsis3_cohort.sql`, removed) with the
  mimic-iv-style SOFA confirmation instead.
- **SOFA** (`04`–`10`) is a port of the mimic-code SOFA to MIMIC-III, built from
  raw tables using mimic-code's MIMIC-III itemid lists (CareVue + MetaVision).
  There is no upstream hourly MIMIC-III SOFA, so it is assembled here the same
  way as the eICU chain: components → hourly SOFA (24h window).

## Files (dependency order)

| File | Table | From |
|------|-------|------|
| `01_abx_poe_list.sql` | `abx_poe_list` | antibiotic drug list (`prescriptions`) |
| `02_abx_micro_poe.sql` | `abx_micro_poe` | antibiotics + `microbiologyevents` cultures |
| `03_suspinfect_poe.sql` | `suspinfect_poe` | first suspicion time per ICU stay |
| `04_sep_lab.sql` | `sep_lab` | `labevents` — bilirubin, creatinine, platelet |
| `05_sep_bg.sql` | `sep_bg` | `labevents` — PaO₂, FiO₂ |
| `06_sep_map.sql` | `sep_map` | `chartevents` — mean arterial pressure |
| `07_sep_gcs.sql` | `sep_gcs` | `chartevents` — GCS (component sum) |
| `08_sep_uo.sql` | `sep_uo` | `outputevents` — urine output |
| `09_sep_vaso.sql` | `sep_vaso` | `inputevents_cv` + `inputevents_mv` — vasopressors |
| `10_sofa.sql` | `sofa` | hourly SOFA (24h window) |
| `11_sepsis3.sql` | `sepsis3` | SI window + SOFA ≥ 2 → carries `subject_id` |

Touches: `prescriptions`, `microbiologyevents`, `icustays`, `labevents`,
`chartevents`, `outputevents`, `inputevents_cv`, `inputevents_mv`. Each file is
a single `CREATE OR REPLACE TABLE` statement over the raw-CSV views. `labevents`
is hospitalization-level, so it is mapped to ICU stays via `hadm_id`; all hours
are whole hours since each stay's ICU `intime`.

## Suspicion handling

A stay can have many antibiotic+culture suspicion events. `11_sepsis3.sql`
considers **all** of them (reading `abx_micro_poe`, not just the earliest in
`suspinfect_poe`): the stay is Sepsis-3 if **any** event has a SOFA hour ≥ 2 in
its `[-48h, +24h]` window, and is emitted once at its earliest qualifying pair.
`SELECT DISTINCT subject_id … WHERE sepsis3` then aggregates to the ID level —
an ID is selected if at least one of its stays is septic.

## MIMIC-III adaptations

- **Respiration** — PaO₂/FiO₂ only (no ventilation flag).
- **Cardiovascular** — any vasopressor → 2, MAP < 70 → 1 (CareVue/MetaVision
  doses and units differ, so presence-based, like the eICU chain).
- **GCS** — summed from motor/verbal/eyes across CareVue + MetaVision itemids;
  intubated rows with no numeric verbal score are dropped (a simplification).
- **Renal** — urine component uses a 24h rolling sum of charted output.

## Usage

The easiest path is `build_sepsis_cohort()` (R/pipeline.R) - a source-aware
helper covering all three families (`mimic_iii`, `mimic_iv`, `eicu`) via
`src_family(src_name)` dispatch, selecting and importing the cohort in one
call:

```r
ehr_connect("rendehr", creds)
build_sepsis_cohort("mimic_iii_demo", overwrite = TRUE)
```

### Bundle options

The default (`bundle = "sepsis3"`) flags a stay as septic if *any* qualifying
suspicion-of-infection event during the whole ICU stay has a matching SOFA≥2
hour nearby - it answers "was this patient ever septic during their ICU
course", which runs much higher than admission-time sepsis-prevalence
studies (95/100 subjects on `mimic_iii_demo`). Three narrower bundles are
available for comparisons against admission-time prevalence estimates
(e.g. Johnson et al. 2018):

| `bundle` | Restriction | `mimic_iii_demo` |
|---|---|---|
| `"sepsis3"` (default) | none | 95 |
| `"first_sepsis3"` | patient's absolute first-ever ICU stay only | 66 |
| `"sepsis3_admission"` | suspicion event within ±1 day of ICU admission | 88 |
| `"first_sepsis3_admission"` | both of the above | 61 |

`"first_sepsis3_admission"` is the closest packaged match to the
sepsis3-mimic study population, but still without that study's separate
adult/MetaVision/CSURG exclusions (see
[`../sepsis_suspicion`](../sepsis_suspicion), which gets to 28 subjects with
all of Johnson's exclusions applied, pre-SOFA) - those exclusions are
specific to that one MIMIC-III-era study and aren't reproduced in the
generic `sepsis3` chain shared with MIMIC-IV/eICU.

`run_sepsis3_example()` goes one step further and runs the full pipeline
(`run_db`, comorbidities, concepts, ...) afterward:

```r
run_sepsis3_example(src_name = "mimic_iii_demo", base_dir = "/path/to/project")
```

For manual/advanced use (e.g. inspecting the cohort before importing it),
`run_one_source()` also accepts `cohort_sql`/`setup_sql`/`key` directly -
this runs the same DuckDB-backed selection internally, no separate CSV
round-trip required:

```r
setup <- sort(list.files(
  system.file("sql/mimic_iii/cohorts/sepsis3", package = "rendehR"),
  pattern = "\\.sql$", full.names = TRUE))

run_one_source(
  "mimic_iii_demo",
  # select every ID level you want available; the sepsis3 table carries
  # subject_id, hadm_id and icustay_id. `key` is the column import filters on.
  cohort_sql = "SELECT DISTINCT subject_id, hadm_id, icustay_id
                FROM sepsis3 WHERE sepsis3",
  key        = "subject_id",
  setup_sql  = setup,
  overwrite  = TRUE)
```

Use `SELECT DISTINCT subject_id …` in `cohort_sql` if you only want the one
ID column carried through.

## Verified

On `mimic_iii_demo` (136 ICU stays / 100 subjects): 110 stays with suspicion,
SOFA peaks spanning 0–21, **99 Sepsis-3 stays / 72 subjects** (all suspicion
events considered; earliest-suspicion-only would give 82 / 63). (Higher than the
Johnson study cohort because this drops the adult/first-stay/MetaVision-only
exclusions and confirms SOFA ≥ 2 rather than eligibility.)

## Notes

- The old Johnson study-cohort file (`04_sepsis3_cohort.sql`, with the
  adult/first-ICU-stay/MetaVision inclusion flags) was removed in favour of the
  mimic-iv-consistent SOFA definition; it remains in git history if needed.
- `sepsis3` is boolean; `WHERE sepsis3` keeps only patients meeting the
  criteria. Final key is `subject_id` (MIMIC cohort key).
- The shared CSV reader was hardened (`quote`/`escape`) so MIMIC-III
  `chartevents` free-text values containing commas parse correctly.

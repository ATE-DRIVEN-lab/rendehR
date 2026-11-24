# Sepsis-3 cohort — SQL setup chain (MIMIC-IV)

A worked example of **defining a cohort with SQL** and feeding it into the
rendehR pipeline. These files reproduce the MIMIC Code Repository's Sepsis-3
definition (SOFA >= 2 within a suspicion-of-infection window) so it can run
directly over the raw PhysioNet CSVs — no pre-built database required.


## Definitions: default and variants

There is no gold-standard way to find Sepsis-3 in ICU records. The chain's last
file builds the `sepsis3` table; `build_sepsis_cohort(definition = , bundle = )`
chooses which file that is.

- **Default** (`definition = "delta"`, `22_sepsis3.sql`): suspected infection is
  the mimic-code antibiotic-culture pairing, on an antibiotic course lasting at least 72 h (administrations less than
  24 h apart form one course; a course cut short by death within 72 h also
  counts), and sepsis is a SOFA rise of at least 2 points over the lowest SOFA of
  the stay so far within `[SI - 48h, SI + 24h]` (YAIB/ricu `delta_cummin`).
  Adults only (anchor_age >= 18). Organ dysfunction present from ICU admission is the
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

Generated from the DuckDB concept SQL in
[`mimic-code`](https://github.com/MIT-LCP/mimic-code)
(`mimic-iv/concepts_duckdb/`). Each source file was mechanically transformed:

- schema prefixes (`mimiciv_derived.`, `mimiciv_hosp.`, `mimiciv_icu.`) stripped
  to **bare names**, so each statement reads the raw-CSV views the
  DuckDB-backed cohort-selection route exposes (bare table names) and the
  tables built by earlier files in this chain;
- `CREATE TABLE` → `CREATE OR REPLACE TABLE` (idempotent, no `DROP`), so each
  file is a **single statement** suitable for one `setup_sql` step.

The files are numbered in dependency (topological) order: `01_*` has no
derived dependencies; `22_sepsis3.sql` is last and reads `sofa` +
`suspicion_of_infection`. The chain touches 7 raw tables: `chartevents`,
`icustays`, `inputevents`, `labevents`, `microbiologyevents`, `outputevents`,
`prescriptions`.

## Usage

The easiest path is `build_sepsis_cohort()` (R/pipeline.R) - a source-aware
helper covering all three families (`mimic_iii`, `mimic_iv`, `eicu`) via
`src_family(src_name)` dispatch, selecting and importing the cohort in one
call:

```r
ehr_connect("rendehr", creds)
build_sepsis_cohort("mimic_iv_demo", overwrite = TRUE)
```

### Bundle options

The default (`bundle = "sepsis3"`) flags a stay as septic if *any* qualifying
suspicion-of-infection event during the whole ICU stay has a matching SOFA≥2
hour nearby - it answers "was this patient ever septic during their ICU
course", which runs much higher than admission-time sepsis-prevalence
studies (62/140 stays on `mimic_iv_demo`). Narrower bundles restrict this,
mirroring `mimic_iii`/`eicu`:

| `bundle` | Restriction | `mimic_iv_demo` stays |
|---|---|---|
| `"sepsis3"` (default) | none | 62 |
| `"first_sepsis3"` | patient's absolute first-ever ICU stay only | 40 |
| `"sepsis3_admission"` | suspicion event within ±1 day of ICU admission | 59 |
| `"first_sepsis3_admission"` | both of the above | 39 |

`run_sepsis3_example()` goes one step further and runs the full pipeline
(`run_db`, comorbidities, concepts, ...) afterward:

```r
run_sepsis3_example(src_name = "mimic_iv_demo", base_dir = "/path/to/project")
```

For manual/advanced use, run every `NN_*.sql` in filename order as
`setup_sql` and pass the cohort key via `cohort_sql`/`key` directly to
`run_one_source()` - this runs the same DuckDB-backed selection internally,
no separate CSV round-trip required:

```r
setup <- sort(list.files(
  system.file("sql/mimic_iv/cohorts/sepsis3", package = "rendehR"),
  pattern = "\\.sql$", full.names = TRUE))

run_one_source(
  "mimic_iv_demo",
  cohort_sql = "SELECT DISTINCT subject_id FROM sepsis3 WHERE sepsis3",
  key        = "subject_id",
  setup_sql  = setup,
  overwrite  = TRUE)
```

## Notes

- MIMIC-III has its own chain (`inst/sql/mimic_iii/cohorts/sepsis3/`,
  different item IDs / table shapes), and eICU has one too
  (`inst/sql/eicu/cohorts/sepsis3/`). `build_sepsis_cohort()`/
  `run_sepsis3_example()` (`R/pipeline.R`) are the source-aware helpers
  covering all three, dispatching the right cohort_sql/key/setup_sql via
  `src_family(src_name)`.
- The final flag `sepsis3` is boolean; `WHERE sepsis3` keeps only patients who
  meet the Sepsis-3 criteria.

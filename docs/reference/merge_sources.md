# Merge harmonized source tables across sources into one schema.

Each source has different patient ID columns (e.g. stay_id vs
patientunitstayid). This function UNION ALLs harmonized source outputs
into a single target schema (default: "rendehr"). For time series,
`analysis_level = "admission"` reads the native-grain `ts_tab`, and
`analysis_level = "icu"` reads the derived ICU projection `ts_icu`
created by
[`make_icu_timeline()`](https://ate-driven-lab.github.io/rendehR/reference/make_icu_timeline.md).
In the merged time-series table, `src_icu_id` is always the
native/source ICU ID. The projection columns `assigned_icu_id`,
`event_origin`, and `icu_time_relation` are emitted only for
`analysis_level = "icu"` (omitted in admission mode, where they would be
all NULL). The merged `ehr_id` is grain-specific:
`analysis_level = "admission"` keys on
`src_patient_id + src_admission_id`, while `analysis_level = "icu"`
additionally appends the (assigned) ICU id. It is recomputed from the
source id columns at merge time, so it always matches the requested
grain regardless of how each source `cohort_info` was built. Use
`event_id` to track the same physical event across admission and ICU
projections.

## Usage

``` r
merge_sources(
  src_names,
  db_name = NULL,
  base_dir = NULL,
  target_schema = "rendehr",
  merge_ts = TRUE,
  merge_dur = TRUE,
  merge_flat = TRUE,
  merge_cohort = TRUE,
  append = FALSE,
  analysis_level = c("admission", "icu")
)
```

## Arguments

- src_names:

  Character vector of source names (e.g. c("mimic_iv_demo",
  "mimic_iii_demo", "eicu_demo")).

- db_name:

  Postgres database name. Defaults to the active project config, then
  "rendehr".

- base_dir:

  Project root directory. Defaults to the active project config, then
  current working directory.

- target_schema:

  Target schema for merged tables. Default "rendehr".

- merge_ts:

  Build merged ts table (rendehr.ts). Default TRUE. Its `value` column
  keeps the source type, NUMERIC(18,3), as does dur's.

- merge_dur:

  Build merged dur table (rendehr.dur) from each source's dur_tab
  (medication exposures, infusions, ventilation episodes). Default TRUE.
  Uses the same ehr_id/append-mode contract as the other merged tables;
  unlike ts, dur has no separate ICU projection variant, so the same
  source dur_tab is read regardless of `analysis_level`.

- merge_flat:

  Build merged flat table (rendehr.flat). Default TRUE. Every flat
  column of any source is kept; it is NULL for sources that do not
  provide it.

- merge_cohort:

  Build merged cohort_info table (rendehr.cohort_info). Default TRUE.

- append:

  If FALSE (default), rebuild merged tables from exactly `src_names`. If
  TRUE, keep existing merged tables and replace rows for `src_names`
  before inserting their current data. Append mode requires the selected
  target tables to already exist and uses their existing columns as the
  schema contract.

  Both modes run in a single transaction that is rolled back on error,
  so a failed merge leaves the previous merged tables unchanged.

- analysis_level:

  Time-series grain to merge: `"admission"` merges native `ts_tab`;
  `"icu"` merges `ts_icu` and requires
  [`make_icu_timeline()`](https://ate-driven-lab.github.io/rendehR/reference/make_icu_timeline.md)
  first. At `"admission"` the merged `flat` and `cohort_info` hold one
  row per admission: the row of its first ICU stay (earliest ICU
  admission time, then lowest ICU id), or the ICU-less row when there is
  no ICU stay. The number of later-ICU rows left out is reported per
  source.

## Value

Invisibly TRUE.

## Details

An admission does not mean exactly the same thing in every source. In
MIMIC-III and MIMIC-IV, `src_admission_id` is the hospital admission
(`hadm_id`) and cohorts are selected per admission. In eICU it is the
hospital stay (`patienthealthsystemstayid`), but cohorts are selected
per ICU unit stay (`patientunitstayid`), so a merged eICU admission
holds only the unit stays that were selected, not necessarily the whole
hospital stay. eICU also has no admission timestamps: its window tables
store offsets in minutes (`admittime` is 0), while MIMIC stores
timestamps. Times in `time_since_hadm_min` count from the hospital
admission in every source.

The merged `ehr_id` is computed as
`concat_ws('_', NULLIF(CAST(part AS text), ''), ...)` over each row's
grain-defining source ID parts (patient, admission, and — at ICU grain —
ICU). `concat_ws()` silently omits any part that is NULL or the empty
string rather than raising, on the assumption that every source row
always has non-NULL values for its grain-defining ID columns. If that
assumption is ever violated for a given row, its `ehr_id` is computed
from only the remaining non-NULL parts, which could coincide with
another row's `ehr_id`; this is a documented, currently unvalidated
assumption, not a runtime-checked invariant.

## Examples

``` r
if (FALSE) { # \dontrun{
merge_sources(c("mimic_iii_demo", "mimic_iv_demo", "eicu_demo"),
              db_name = "rendehr", target_schema = "rendehr_admission")
} # }
```

# Fetch all time-series data for one patient

Filters `ts_tab` (the harmonized long-format output) to a single
patient. Optionally restrict to specific concepts.

## Usage

``` r
get_patient_ts(
  src_name,
  patient_id,
  concepts = NULL,
  con = NULL,
  analysis_level = c("admission", "icu")
)
```

## Arguments

- src_name:

  Source name.

- patient_id:

  The patient identifier value (matches the primary ID column for the
  source: subject_id / patientunitstayid).

- concepts:

  Optional character vector of concept names to include. Default NULL
  returns all concepts.

- con:

  Optional explicit DBI connection.

- analysis_level:

  Grain of the harmonized series to read: `"admission"` (default) reads
  the native-grain `ts_tab` (each event once); `"icu"` reads the derived
  `ts_icu` projection built by
  [`make_icu_timeline()`](https://ate-driven-lab.github.io/rendehR/reference/make_icu_timeline.md).

## Value

A tibble in long format (concept_name, time, value, unit, ...).

## Examples

``` r
if (FALSE) { # \dontrun{
get_patient_ts("mimic_iv_demo", patient_id = 10000032)
get_patient_ts("mimic_iv_demo", 10000032, concepts = c("heart_rate", "spo2"))
} # }
```

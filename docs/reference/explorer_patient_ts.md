# Read one patient's harmonized time series for the explorer

`explorer_patient_ts()` returns the series of one patient either through
the per-source harmonized tables
([`get_patient_ts()`](https://ate-driven-lab.github.io/rendehR/reference/get_patient_ts.md))
or from the merged schema, in one normalized long format (minutes since
hospital admission). `explorer_patient_dur()` returns the patient's
medication exposures from the merged duration table.

## Usage

``` r
explorer_patient_ts(
  bundle,
  source,
  patient_id,
  mode = c("source", "merged"),
  con = NULL
)

explorer_patient_dur(bundle, source, patient_id, con = NULL)
```

## Arguments

- bundle:

  A bundle from
  [`explorer_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_data.md).

- source:

  Source name.

- patient_id:

  Patient identifier (`src_patient_id` in the merged schema).

- mode:

  `"source"` (per-source harmonized tables) or `"merged"`.

- con:

  Optional open DBI connection.

## Value

A tibble with columns adm_id, icu_id, t_hadm, t_icu (minutes), concept,
concept_raw, value (numeric), value_chr, unit and source_table.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- explorer_data("rendehr_admission")
p <- d$patients[1, ]
explorer_patient_ts(d, p$source, p$patient_id)
} # }
```

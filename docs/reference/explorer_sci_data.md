# Load the optional SCI pages of the cohort explorer

Reads a spinal cord injury study's prepared explorer data: R data files
with the flagged admissions and their derived variables (SCI-D category,
neurological classification, complications, interventions, admission
biomarkers), the patient context, code lists, biomarker trajectories,
MAP values and surgery timing, plus optionally the study's concept
categories. Pass the result to
[`run_explorer()`](https://ate-driven-lab.github.io/rendehR/reference/run_explorer.md)
as `sci`.

## Usage

``` r
explorer_sci_data(app_data_dir, analysis_dir = NULL)

explorer_add_sci(data, sci)
```

## Arguments

- app_data_dir:

  Directory with the prepared files (`adm.rds`, `ctx_adm.rds`,
  `ctx_icu.rds`, `patients.rds`, `dx.rds`, `px.rds`, `codes.rds`,
  `bio_daily.rds`, `map_stay.rds`, `map_values.rds`,
  `surgery_timing.rds`; `dur.rds` is used for the medication library
  when present).

- analysis_dir:

  Optional analysis output directory; its
  `tables/T22a_ts_concept_summary.csv` groups the concepts into
  categories.

- data:

  A bundle from
  [`explorer_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_data.md).

- sci:

  The result of `explorer_sci_data()`.

## Value

A list of class `rendehR_explorer_sci`.

`explorer_add_sci()`: the bundle with the SCI analysis set as its cohort
and the SCI pages enabled.

## Examples

``` r
if (FALSE) { # \dontrun{
sci <- explorer_sci_data("analysis/outputs/explorer_data", "analysis/outputs")
run_explorer("rendehr", db_name = "rendehr", sci = sci)
} # }
```

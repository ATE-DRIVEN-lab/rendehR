# Activate a patient subgroup filter for a source

Once set, all fetch_table() / fetch_concept() / fetch_concepts() calls
for `src_name` automatically filter rows to these patients only. The
filter is implemented via a per-call Postgres TEMP TABLE JOIN -
performance is stable even for tens of thousands of IDs.

## Usage

``` r
set_active_cohort(src_name, ids, id_col = NULL)
```

## Arguments

- src_name:

  Source name.

- ids:

  Vector of patient IDs (from build_subcohort() or any custom vector).

- id_col:

  Optional identifier column for `ids`. Defaults to
  `cohort_key(src_name)`.

## Value

The cohort IDs, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
ids <- build_subcohort("mimic_iv_demo", scid_injury_level = "thoracic")
set_active_cohort("mimic_iv_demo", ids)

fetch_concept("mimic_iv_demo", "heart_rate")   # only these patients
fetch_concepts("mimic_iv_demo", c("heart_rate", "sbp"))

clear_active_cohort("mimic_iv_demo")           # back to full cohort
} # }
```

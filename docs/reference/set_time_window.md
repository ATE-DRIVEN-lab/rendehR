# Set the time window reference for a source

Once set, all fetch_concept() / fetch_concepts() calls for `src_name`
automatically apply the time window filter. Use `ref = "icu"` to work in
ICU-relative time; set `clip = TRUE` to restrict measurements to within
the ICU stay (time_since_icu_min BETWEEN 0 AND icu_los_min).

## Usage

``` r
set_time_window(src_name, ref = c("hadm", "icu"), clip = FALSE)
```

## Arguments

- src_name:

  Source name.

- ref:

  Time reference: "hadm" (default, hospital admission) or "icu".

- clip:

  If TRUE and ref = "icu", filter to 0 \<= time_since_icu_min \<=
  icu_los_min. Ignored when ref = "hadm". Default FALSE.

## Value

`NULL`, invisibly.

## Details

Requires that ts_tab was built after the window tables/icu_los_min
update (i.e. ts_tab has time_since_icu_min and icu_los_min columns).

## Examples

``` r
if (FALSE) { # \dontrun{
set_time_window("mimic_iv_demo", ref = "icu", clip = TRUE)
fetch_concept("mimic_iv_demo", "heart_rate")   # ICU-relative, clipped
clear_time_window("mimic_iv_demo")

# Composable with subcohort filter:
set_active_cohort("mimic_iv_demo", ids)
set_time_window("mimic_iv_demo", ref = "icu", clip = TRUE)
fetch_concept("mimic_iv_demo", "heart_rate")   # subgroup + ICU window
} # }
```

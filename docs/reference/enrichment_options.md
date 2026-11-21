# Specify cohort-enrichment options

Configure cohort classification, comorbidity scoring, and
injury-severity scoring.

## Usage

``` r
enrichment_options(
  analysis_level = c("admission", "icu"),
  icds_inj_path = NULL,
  classify_fn = NULL,
  classify_args = list(),
  maps = c("charlson", "elixhauser"),
  icd_adjudication_path = NULL,
  publish = TRUE
)
```

## Arguments

- analysis_level:

  Character string, `"admission"` or `"icu"`, specifying the analysis
  level for cohort information and annotations. Default: `"admission"`.

- icds_inj_path:

  Character string giving an injury-ICD input file for
  [`build_cohort_annotations()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_annotations.md),
  or `NULL` to use that function's configured input.

- classify_fn:

  Optional classification function used by
  [`build_cohort_info_general()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_info_general.md)
  or
  [`build_cohort_annotations()`](https://ate-driven-lab.github.io/rendehR/reference/build_cohort_annotations.md).
  Default: `NULL`, which for the `scid` cohort profile resolves to
  [`scid_classify()`](https://ate-driven-lab.github.io/rendehR/reference/scid_classify.md);
  supplying a function here replaces that default rather than adding to
  it.

- classify_args:

  Named list of additional arguments supplied to `classify_fn`. Default:
  an empty list. Entries are merged over the profile's own defaults, so
  a single path can be overridden without restating the others.

- maps:

  Character vector containing `"charlson"`, `"elixhauser"`, or both.
  Selects the maps used by
  [`run_comorbidities()`](https://ate-driven-lab.github.io/rendehR/reference/run_comorbidities.md).
  Default: both.

- icd_adjudication_path:

  Character string giving a reviewed eICU ICD-version crosswalk for
  comorbidity and injury scoring. `NULL` uses the scoring function's
  default resolution.

- publish:

  Logical. Write computed comorbidity and injury scores to their
  database tables. Default: `TRUE`.

## Value

A named list of the supplied options, suitable for the `enrichment`
argument of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
and compatible direct stage functions.

## Details

These settings configure selected enrichment stages. Use `stages` in
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
to choose which operations to run.

Only explicitly supplied arguments are included in the result, including
explicit `NULL` values. Omitted settings use the defaults of the
receiving function. An empty options list therefore leaves its defaults
unchanged.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)

## Examples

``` r
enrichment_options(analysis_level = "icu")
#> $analysis_level
#> [1] "icu"
#> 
enrichment_options(maps = "charlson")
#> $maps
#> [1] "charlson"
#> 
```

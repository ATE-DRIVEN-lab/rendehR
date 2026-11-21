# Concept dictionary

The concepts rendehR extracts from each source, generated from the
configuration files shipped with the installed package
(`rendehr_concepts_config.csv` for time series,
`rendehr_duration_concepts_config.csv` for medication exposures). Flat
admission-level variables (age, sex, outcomes, length of stay, …) come
from SQL and are listed in
[`vignette("harmonized-data")`](https://ate-driven-lab.github.io/rendehR/articles/harmonized-data.md).

A tick means the source has at least one configuration row for the
concept. *Range* is the plausible range used by
[`clean_by_range()`](https://ate-driven-lab.github.io/rendehR/reference/clean_by_range.md)
and by `clean = "range"` in the wide loaders; the pipeline itself keeps
every value. To add or override a concept for a project, see
[`vignette("concepts")`](https://ate-driven-lab.github.io/rendehR/articles/concepts.md).

## Time-series concepts

130 concepts from 538 configuration rows.

## Medication exposures

108 exposure concepts from 545 configuration rows.

## Reading the configuration in R

``` r
cfg <- read.csv(system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"))
subset(cfg, concept_name == "lactate",
       c(source, table_name, match_value, unit, min_value, max_value, conversion))
validate_concept_config(cfg)   # the checks run_concepts() applies to every row
```

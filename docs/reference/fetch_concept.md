# Fetch one atomic concept table into a tibble

Convenience wrapper around fetch_table() that always targets the
concepts schema. The concept table is created by run_concepts().

## Usage

``` r
fetch_concept(src_name, concept, n = Inf, con = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- concept:

  Concept name (e.g. "heart_rate", "spo2").

- n:

  Max rows to return. Default Inf.

- con:

  Optional explicit DBI connection.

## Value

A tibble with columns: id cols, time_since_hadm_min, value, unit,
source_table, source_dataset, etc.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
hr <- fetch_concept("mimic_iv_demo", "heart_rate")
} # }
```

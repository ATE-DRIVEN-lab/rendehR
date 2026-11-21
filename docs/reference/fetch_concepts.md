# Fetch multiple concept tables and stack them into long format

Opens one connection, loads all requested concepts, closes the
connection. The returned tibble has a `concept_name` column so you can
distinguish concepts after stacking.

## Usage

``` r
fetch_concepts(src_name, concepts, n = Inf, con = NULL)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- concepts:

  Character vector of concept names.

- n:

  Max rows per concept. Default Inf.

- con:

  Optional explicit DBI connection.

## Value

A long tibble with an added `concept_name` column.

## Details

Columns that differ across concepts (e.g. source_itemid, unit_raw) are
coerced to character before binding so the union is always safe.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
vitals <- fetch_concepts("mimic_iv_demo", c("heart_rate", "sbp", "spo2"))
} # }
```

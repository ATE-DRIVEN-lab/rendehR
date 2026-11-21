# Summary statistics for one concept

Returns row count, patient count, time range, and numeric value stats
for a concept table (if a `value` column is present).

## Usage

``` r
concept_summary(src_name, concept, schema = NULL, con = NULL)
```

## Arguments

- src_name:

  Source name.

- concept:

  Concept name.

- schema:

  Optional source schema key or full schema name. If NULL, the concepts
  schema is tried first, then the derived schema.

- con:

  Optional explicit DBI connection.

## Value

A named list with summary statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
concept_summary("mimic_iv_demo", "heart_rate")
} # }
```

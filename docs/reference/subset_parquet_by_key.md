# Safely subset one parquet file to rows matching key values

This bounded utility handles exactly one input file and one output file.
The input must contain `key`; it is never copied unfiltered when the key
is absent. Identifiers use the same strict normalization as cohort
selection, and publication is atomic.

## Usage

``` r
subset_parquet_by_key(
  input_path,
  output_path,
  key,
  key_values,
  allow_unmatched_ids = FALSE,
  overwrite = FALSE
)
```

## Arguments

- input_path:

  Input parquet file.

- output_path:

  Output parquet file.

- key:

  Column name to filter on (e.g. "subject_id").

- key_values:

  Vector of values to keep.

- allow_unmatched_ids:

  Permit a partially unmatched list when at least one identifier
  matches? Default `FALSE`.

- overwrite:

  Replace an existing output atomically?

## Value

Invisibly returns aggregate requested, matched, unmatched, and row
counts. Identifier values are never printed.

## Examples

``` r
if (FALSE) { # \dontrun{
subset_parquet_by_key("raw/labevents.parquet", "cohort/labevents.parquet",
                      key = "subject_id", key_values = c(10000032, 10001217))
} # }
```

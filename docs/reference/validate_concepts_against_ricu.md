# Compare concept config item IDs against ricu's concept dictionary

Returns a tibble of mismatches for manual review. Never auto-overwrites.

## Usage

``` r
validate_concepts_against_ricu(concept_config_path, src_name)
```

## Arguments

- concept_config_path:

  Path to rendehr_concepts_config.csv.

- src_name:

  Source name (e.g. "mimic_iv_demo").

## Value

Tibble with columns: concept_name, your_match_value, ricu_match_value,
class, mismatch_type. Empty = no mismatches.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_concepts_against_ricu(
  system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"), "mimic_iv")
} # }
```

# Suggest close matches for a query string against a set of choices

Suggest close matches for a query string against a set of choices

## Usage

``` r
suggest_match(query, choices, max_suggestions = 3)
```

## Arguments

- query:

  Character string to match.

- choices:

  Character vector of candidate strings.

- max_suggestions:

  Maximum number of suggestions to return.

## Value

Character vector of up to max_suggestions matches.

## Examples

``` r
suggest_match("mimic_iv_dem", c("mimic_iv_demo", "mimic_iii_demo", "eicu_demo"))
#> [1] "mimic_iv_demo"
```

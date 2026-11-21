# Canonical source family name

Collapses any variant suffix (`_demo`, materialized subcohorts such as
`_cervical_a`, dev/capped variants) to the canonical family so concept
config and SQL lookups work across all variants without duplicating
rows. Resolution is driven by the source registry seeded in `.onLoad()`;
unknown names are returned unchanged.

## Usage

``` r
src_family(src_name)
```

## Arguments

- src_name:

  Source name, e.g. `"mimic_iv_demo"` or `"mimic_iv"`. May be a
  character vector.

## Value

Canonical family name(s), e.g. `"mimic_iv"`.

## Examples

``` r
src_family("mimic_iii_demo")
#> [1] "mimic_iii"
src_family("eicu")
#> [1] "eicu"
```

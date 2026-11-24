# Build the standard Postgres schema name list for a source

Build the standard Postgres schema name list for a source

## Usage

``` r
src_schema_names(src_name)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

## Value

Named list with raw, concepts, derived, and harmonized schema names.

## Examples

``` r
src_schema_names("mimic_iv_demo")
#> $raw
#> [1] "mimic_iv_demo_coh"
#> 
#> $concepts
#> [1] "mimic_iv_demo_coh_concepts"
#> 
#> $derived
#> [1] "mimic_iv_demo_coh_derived"
#> 
#> $harmonized
#> [1] "mimic_iv_demo_coh_harmonized"
#> 
```

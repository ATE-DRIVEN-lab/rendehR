# List registered source families

Returns the family names currently known to the source registry
(built-ins plus any registered via
[`register_source()`](https://ate-driven-lab.github.io/rendehR/reference/register_source.md)
or discovered under `EHR_EXTERNAL_CONFIG_ROOT`). Unlike
[`list_sources()`](https://ate-driven-lab.github.io/rendehR/reference/list_sources.md),
this does not require a database connection and reflects registration,
not materialization.

## Usage

``` r
list_registered_sources()
```

## Value

Character vector of registered family names.

## Examples

``` r
list_registered_sources()
#> [1] "eicu"      "mimic_iii" "mimic_iv" 
```

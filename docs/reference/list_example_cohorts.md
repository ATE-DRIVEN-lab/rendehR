# List the example cohorts bundled with the package

Ready-made cohort definitions (ICD lists and SQL chains) that
[`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
and
[`materialize_cohort_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/materialize_cohort_duckdb.md)
can select by name via their `cohort` argument.

## Usage

``` r
list_example_cohorts()
```

## Value

A data frame of `name`, `kind`, `grain`, and `description`.

## Examples

``` r
list_example_cohorts()
#>      name kind              grain
#> 1    scid  icd hospital admission
#> 2 sepsis3  sql        ICU-stay id
#>                                                                 description
#> 1 Spinal cord injury - bundled ICD-9/10 list (the package reference cohort)
#> 2  Sepsis-3 (Singer 2016): SOFA >= 2 + suspicion of infection, per ICU stay
```

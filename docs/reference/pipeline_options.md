# Configure the source pipeline

Use option constructors to configure related settings in
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md).
Each constructor returns an ordinary named list containing only the
arguments supplied in the call.

## Option groups

|                    |                                                                                                              |                                 |
|--------------------|--------------------------------------------------------------------------------------------------------------|---------------------------------|
| Argument           | Constructor                                                                                                  | Settings                        |
| `cohort_selection` | [`cohort_selection_options`](https://ate-driven-lab.github.io/rendehR/reference/cohort_selection_options.md) | Population and table selection  |
| `windows`          | [`window_options`](https://ate-driven-lab.github.io/rendehR/reference/window_options.md)                     | Observation-window construction |
| `loading`          | [`loading_options`](https://ate-driven-lab.github.io/rendehR/reference/loading_options.md)                   | Import and database loading     |
| `enrichment`       | [`enrichment_options`](https://ate-driven-lab.github.io/rendehR/reference/enrichment_options.md)             | Classification and scoring      |
| `concepts`         | [`concept_options`](https://ate-driven-lab.github.io/rendehR/reference/concept_options.md)                   | Concept definitions and layers  |
| `harmonization`    | [`harmonization_options`](https://ate-driven-lab.github.io/rendehR/reference/harmonization_options.md)       | Output tables                   |
| `execution`        | [`execution_options`](https://ate-driven-lab.github.io/rendehR/reference/execution_options.md)               | Rebuilds and error handling     |

## Defaults and validation

An omitted setting uses the receiving function's default. For example,
[`loading_options()`](https://ate-driven-lab.github.io/rendehR/reference/loading_options.md)
returns an empty list, whereas `loading_options(ignore_errors = FALSE)`
requests strict CSV parsing. An explicitly supplied `NULL` is retained
in the list. Constructor usage shows reference defaults; calling a
constructor without that argument does not insert its reference value.
Stage-specific defaults can differ, such as column validation for raw
caching (`TRUE`) and cohort import (`FALSE`).

Constructors check option names and logical flags. Stage functions
perform additional validation when the options are used. Direct stage
functions use only settings applicable to that stage; other valid fields
are ignored. A setting supplied both directly and in a group is an
error, even when the values agree.

## Selecting work

`stages` selects pipeline operations. `concepts$layers` selects layers
within concept extraction. Providing an option group does not select its
stage, and selecting concept layers does not change the harmonization
output settings. For argument names accepted by earlier interfaces, see
[pipeline_compatibility](https://ate-driven-lab.github.io/rendehR/reference/pipeline_compatibility.md).

## See also

[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md),
[`run_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_concepts.md)

## Examples

``` r
loading_options()
#> named list()
loading_options(ignore_errors = FALSE)
#> $ignore_errors
#> [1] FALSE
#> 
concept_options(layers = "flat")
#> $layers
#> [1] "flat"
#> 

# Ordinary named lists can also be used as option groups.
list(layers = "flat", dry_run = TRUE)
#> $layers
#> [1] "flat"
#> 
#> $dry_run
#> [1] TRUE
#> 
```

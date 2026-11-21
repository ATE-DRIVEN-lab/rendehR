# Write a generated setup script to disk

Write a generated setup script to disk

## Usage

``` r
write_setup_script(plan, path, overwrite = FALSE)
```

## Arguments

- plan:

  Object returned by
  [`ehr_setup_assistant()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_setup_assistant.md).

- path:

  Output file path.

- overwrite:

  Overwrite an existing file? Default FALSE.

## Value

Invisibly returns `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ehr_setup_assistant(interactive = FALSE)
write_setup_script(plan, "run_pipeline.R")
} # }
```

# Print an `rendehR` add-source assistant plan

Displays the target source name, resolved config root, detected cohort
logic, the exact
[`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md)
call the assistant would run, and, once executed, whether scaffolding
succeeded and how many files were written.

## Usage

``` r
# S3 method for class 'ehr_add_source_plan'
print(x, ...)
```

## Arguments

- x:

  An `ehr_add_source_plan` object, as returned by
  [`ehr_add_source_assistant()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_add_source_assistant.md).

- ...:

  Ignored.

## Value

Invisibly, `x`.

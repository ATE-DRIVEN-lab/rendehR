# Compatibility arguments for the source pipeline

[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
accepts named arguments from the ungrouped interface through `...`. New
calls can use the option constructors described in
[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md).

## Grouped and direct arguments

Settings documented by an option constructor may also be supplied
directly. For example, `concept_name = "heart_rate"` and
`concepts = concept_options(concept_name = "heart_rate")` specify the
same setting. Supplying both forms raises an error. Unknown argument
names are rejected. Layer selection is also accepted directly, for
example
`run_one_source("mimic_iii", stages = "run_concepts", layers = "flat")`.

Name arguments after `src_name`, `db_name`, and `base_dir` when updating
a positional call. Option-group names must be exact; `cohort` remains a
cohort selection setting and is not an abbreviation for
`cohort_selection`.

## Stage switches

The following switches affect default stage selection when
`stages = NULL`. An explicit `stages` vector determines which stages
run.

- `run_cohort_annotations`:

  Logical or `NULL`. `TRUE` selects cohort annotations in place of
  cohort information. `NULL` resolves to `FALSE`.

- `run_comorbidities`, `run_iss`:

  Logical or `NULL`. `NULL` enables these stages for the `scid` profile
  and disables them for other profiles. Explicit values override that
  default.

- `run_external`:

  Logical, default `FALSE`. Enables the separate external-SQL stage. It
  does not control the external layer inside `run_concepts`; use
  `concept_options(layers = ...)` for layer selection.

## Retired arguments

`delete_source_csv = FALSE` is accepted. `delete_source_csv = TRUE`
raises an error; raw source files are not deleted by the pipeline.

## See also

[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md),
[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md)

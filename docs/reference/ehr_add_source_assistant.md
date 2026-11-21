# Interactive assistant for adding a new data source

The "add a new source" counterpart to
[`ehr_setup_assistant()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_setup_assistant.md).
Run interactively, it asks a short series of questions (ID columns, time
origin, cohort strategy, …), then either scaffolds the source with
[`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md),
prints the equivalent
[`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md)
call for you to run, or validates the assembled spec without writing
anything. Every input is also a keyword argument, so the same call works
non-interactively in a script.

## Usage

``` r
ehr_add_source_assistant(
  name = NULL,
  root = NULL,
  select_id_cols = NULL,
  id_default = NULL,
  id_icu = NULL,
  tables_with_icu = NULL,
  time_origin = NULL,
  time_diff = NULL,
  cohort_key = NULL,
  cohort_seed_tables = NULL,
  diag_table = NULL,
  code_col = NULL,
  filter_key = NULL,
  split_commas = NULL,
  strip_dots = NULL,
  id_prefixes = NULL,
  icu_id_type = NULL,
  demo_row_limit = NULL,
  action = NULL,
  overwrite = FALSE,
  interactive = base::interactive()
)
```

## Arguments

- name:

  Source family name (lowercase, no `_demo`).

- root:

  Config directory to scaffold into. Defaults to
  `EHR_EXTERNAL_CONFIG_ROOT`.

- select_id_cols:

  ID columns carried into every concept table, coarse → fine. A vector,
  or a comma-separated string.

- id_default:

  Join key(s) for non-ICU tables. Defaults to `select_id_cols`.

- id_icu, tables_with_icu:

  Finer ICU-level join key and the tables that carry it. Omit for a
  single granularity.

- time_origin:

  `visit_windows` column used as t=0 (e.g. `"cw.admittime"`).

- time_diff:

  `"timestamp_minutes"` or `"offset_minutes"`.

- cohort_key:

  Patient/stay ID used to subset every table.

- cohort_seed_tables:

  Tables imported first so cohort IDs can be built.

- diag_table, code_col, filter_key, split_commas, strip_dots:

  Fields of the declarative `cohort_filter` that selects cohorts by ICD
  code (see
  [`vignette("adding-a-new-source")`](https://ate-driven-lab.github.io/rendehR/articles/adding-a-new-source.md)).
  `filter_key` defaults to `cohort_key`.

- id_prefixes:

  Advanced: source-name prefixes matched by
  [`src_family()`](https://ate-driven-lab.github.io/rendehR/reference/src_family.md).
  Defaults to `name`. Only needed when variant names don't start with
  `name`.

- icu_id_type:

  Advanced: SQL type of the ICU-level id (`"bigint"` default; use
  `"text"` for string stay IDs).

- demo_row_limit:

  Row cap for the `<name>_demo` variant. Default 1000.

- action:

  `"write"` (scaffold + register via
  [`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md)),
  `"script"` (print the call, touch nothing), or `"dry_run"` (validate
  the spec only). Defaults to `"write"` interactively and to `"script"`
  otherwise, so a non-interactive call writes nothing unless asked.

- overwrite:

  Passed to
  [`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md)
  when `action = "write"`.

- interactive:

  Ask questions on the console? Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly, an `ehr_add_source_plan`: the collected `spec`, the rendered
`call`, the chosen `action`, any `result` (wizard/validation output),
and `next_steps`.

## Details

It collects choices and hands them to
[`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md);
it does not write your source-specific SQL (`window_tables.sql`, schema
DDL) or concept-config rows — those are reported as next steps.

## See also

[`add_source_wizard()`](https://ate-driven-lab.github.io/rendehR/reference/add_source_wizard.md),
[`register_source()`](https://ate-driven-lab.github.io/rendehR/reference/register_source.md),
[`validate_source_config()`](https://ate-driven-lab.github.io/rendehR/reference/validate_source_config.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_add_source_assistant(name = "my_icu", root = "~/data/my_icu")
} # }
```

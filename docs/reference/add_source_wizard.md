# Scaffold a new data source

One-call scaffolder for adding a source family without editing the
package. It writes, under `root` (default `EHR_EXTERNAL_CONFIG_ROOT`):

## Usage

``` r
add_source_wizard(
  name,
  root = NULL,
  select_id_cols = NULL,
  time_origin = NULL,
  time_diff = "timestamp_minutes",
  id_cols = NULL,
  cohort_key = NULL,
  cohort_seed_tables = NULL,
  cohort_filter = NULL,
  id_prefixes = NULL,
  icu_id_type = "bigint",
  demo_row_limit = 1000L,
  overwrite = FALSE,
  register = TRUE
)
```

## Arguments

- name:

  Family name, e.g. `"amsterdam"`.

- root:

  Target config directory. Defaults to `EHR_EXTERNAL_CONFIG_ROOT`;
  created if it does not exist.

- select_id_cols, time_origin, time_diff, id_cols, cohort_key,
  cohort_seed_tables, id_prefixes, icu_id_type:

  Registry fields (see
  [`register_source()`](https://ate-driven-lab.github.io/rendehR/reference/register_source.md)).
  Required fields left `NULL` (`select_id_cols`, `time_origin`,
  `id_cols`, `cohort_key`) are written as `null` placeholders for you to
  fill in; optional ones are omitted.

- cohort_filter:

  Declarative cohort spec (list with `diag_table`, `code_col`, `key`,
  optional `split_commas`/`strip_dots`).

- demo_row_limit:

  Row cap for the `<name>_demo` variant. Default 1000.

- overwrite:

  Overwrite an existing `sources/<name>.json`? Default FALSE.

- register:

  Register the source this session when the spec is valid? Default TRUE.

## Value

Invisibly, a list with `root`, `json` (registry path), `created` (paths
written), `registered` (logical), and `problems` (spec problems, if any,
that blocked registration).

## Details

- `sources/<name>.json` — a registry skeleton (fed to
  [`register_source()`](https://ate-driven-lab.github.io/rendehR/reference/register_source.md)).

- `sql/<name>/…` — the SQL directory tree (`db_setup_sqls/schema`,
  `raw_mods`, `concepts_sqls/{flat,ts,dur,ehr}`) with skeleton
  `create_tables.sql`, `window_tables.sql`, and `indexes.sql`.

- `rendehr_concepts_config.csv` — a header-only concept config (created
  only if absent), where source-specific concept rows are added.

Existing files are never clobbered unless `overwrite = TRUE`. When the
supplied fields form a complete, valid spec the source is also
registered for the current session (`register = TRUE`); an incomplete
skeleton is written but not registered, and the missing fields are
reported.

## Examples

``` r
if (FALSE) { # \dontrun{
add_source_wizard("my_icu", root = "~/data/my_icu",
                  select_id_cols = c("patient_id", "admission_id", "stay_id"))
} # }
```

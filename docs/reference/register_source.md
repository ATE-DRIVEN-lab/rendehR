# Register a data source family

Makes a new source known to the whole pipeline (cohort building, concept
extraction, harmonization, access) without editing package code. Supply
the configuration either inline or as a path to a schema-versioned
family JSON. Sources placed under `EHR_EXTERNAL_CONFIG_ROOT/sources` are
registered automatically when the package loads; call this directly to
register within a session.

## Usage

``` r
register_source(name = NULL, ..., config = NULL, overwrite = FALSE)
```

## Arguments

- name:

  Family name, e.g. `"amsterdam"`. Its demo/subcohort variants
  (`amsterdam_demo`, `amsterdam_demo_cervical`) resolve to it
  automatically. Omit when using `config`.

- ...:

  Inline configuration fields: `select_id_cols`, `time_origin`,
  `time_diff` (`"timestamp_minutes"` or `"offset_minutes"`), `id_cols`
  (list with `default`, optional `icu`/`tables_with_icu`), `cohort_key`,
  `cohort_seed_tables`, `cohort_filter`, `id_prefixes`, `icu_id_type`,
  and `variants` (e.g. `list(demo = list(row_limit = 1000L))`).

- config:

  Optional path to one schema-versioned family JSON object. Mutually
  exclusive with inline `...` fields.

- overwrite:

  Allow replacing a built-in source (`mimic_iv`, `mimic_iii`, `eicu`).
  Replacement is always explicit; JSON cannot request overwrite.

## Value

The registered source name(s), invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
register_source(config = "config/sources/my_icu.json")
list_registered_sources()
} # }
```

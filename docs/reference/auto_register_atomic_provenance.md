# Auto-register provenance for atomic concepts from a concept config CSV.

Groups concept rows by their `ref` column (if present); maps each ref
value to a human-readable origin_name and origin_url. Concepts with no
`ref` get origin_name = "rendehR package". Called internally by
run_concepts().

## Usage

``` r
auto_register_atomic_provenance(
  con,
  src_name,
  schema_name,
  config_path,
  concept_names = NULL
)
```

## Arguments

- con:

  DBI connection.

- src_name:

  Source name.

- schema_name:

  Full Postgres schema name where concepts were written.

- config_path:

  Path to the concept config CSV.

- concept_names:

  Optional character vector restricting registration to these concept
  names (e.g. only the concepts that were actually built successfully).
  When `NULL` (the default), every concept in the config matching the
  source family is registered, as before.

# Specify concept-extraction options

Select concept layers and configure packaged, user-defined, and external
concepts.

## Usage

``` r
concept_options(
  user_concepts = NULL,
  user_schema = NULL,
  external_sql_dir = NULL,
  external_schema = NULL,
  concept_name = NULL,
  dry_run = FALSE,
  layers = NULL,
  rm_duplicates = TRUE
)
```

## Arguments

- user_concepts:

  Character string giving a supplemental concept CSV. Rows override
  packaged definitions with the same source and concept name and may add
  new concepts. Default: `NULL`.

- user_schema:

  Character string naming the target schema for user SQL. `NULL` uses
  the source concepts schema.

- external_sql_dir:

  Character string giving an external SQL directory. `NULL` uses the
  project's configured external-concept directory.

- external_schema:

  Character string naming the target schema for a single external
  module. `NULL` derives the schema from the module name. Separate
  submodules always receive separate schemas.

- concept_name:

  Character vector of concept names or SQL filenames without the `.sql`
  extension. Filters the `flat`, `ts`, `dur`, and `user` layers. `NULL`
  includes all concepts in those layers; the `ehr` and `external` layers
  are not filtered.

- dry_run:

  Logical. Generate or read concept SQL without building concept tables
  or registering provenance. Default: `FALSE`. This setting applies to
  concept extraction; it does not preview other pipeline stages. See
  [`run_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_concepts.md)
  for connection and file-writing behavior.

- layers:

  Character vector selecting `"flat"`, `"ts"`, `"dur"`, `"ehr"`,
  `"user"`, or `"external"`. `NULL` selects all enabled layers. Layers
  run in this order; prerequisite tables must already exist. The `ehr`
  layer requires PostgreSQL.

- rm_duplicates:

  Logical. Remove chart rows that are validated copies of lab results
  (MIMIC) or of monitor values (eICU) during `ts`/`dur` concept
  extraction, and repeated chart rows of a `ts` concept. Default:
  `TRUE`. `FALSE` extracts exactly the same rows and removes none. See
  [`run_atomic_concepts_unified()`](https://ate-driven-lab.github.io/rendehR/reference/run_atomic_concepts_unified.md)
  for the matching rule.

## Value

A named list of the supplied options, suitable for the `concepts`
argument of
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
and compatible direct stage functions.

## Details

In this package, `ehr` selects maintained SQL definitions. Upstream
`repo` SQL is retained as reference material and cannot be selected for
execution.

Only explicitly supplied arguments are included in the result, including
explicit `NULL` values. Omitted settings use the defaults of the
receiving function. An empty options list therefore leaves its defaults
unchanged.

## See also

[pipeline_options](https://ate-driven-lab.github.io/rendehR/reference/pipeline_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)

## Examples

``` r
concept_options(layers = "flat")
#> $layers
#> [1] "flat"
#> 
concept_options(layers = c("ts", "dur"), dry_run = TRUE)
#> $dry_run
#> [1] TRUE
#> 
#> $layers
#> [1] "ts"  "dur"
#> 
```

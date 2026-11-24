# Extension utilities: bring-your-own SQL + project workspace helpers.

These functions cover two common scenarios:

1.  You found a SQL query somewhere and want to run it as a concept.

2.  You want a project-specific schema or database to keep derived work
    separate from the raw source schemas. Adapt and register an external
    SQL query as a concept

## Usage

``` r
register_concept_sql(
  sql,
  src_name,
  concept_name,
  schema = "concepts",
  save_to = NULL,
  module = NULL,
  base_dir = NULL,
  dry_run = TRUE,
  overwrite = FALSE,
  origin_name = NULL,
  origin_url = NULL,
  origin_ref = NULL,
  con = NULL
)
```

## Arguments

- sql:

  Source SQL. Either a file path or an inline SQL string.

- src_name:

  Source name (e.g. "mimic_iv_demo"). Determines which schema prefixes
  to strip.

- concept_name:

  Name for the output table.

- schema:

  Target schema for the concept table. Accepts a key from
  src_schema_names() ("concepts", "derived", "flat", "ts", "dur") or any
  literal schema name. Default "concepts" -\> \_coh_concepts.

- save_to:

  Where to write the adapted SQL. Default `NULL` writes to
  `sql/{src_name}/concepts_sqls/user/{concept_name}.sql` (or
  `.../external/{module}/{concept_name}.sql` when `module` is given) so
  a later
  [`run_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_concepts.md)
  call picks it up automatically, the same as if you had dropped the
  file there by hand. Pass `FALSE` to skip saving, or an explicit path
  to save elsewhere.

- module:

  Optional external-layer module name. When given, the default `save_to`
  targets `external/{module}/` (its own schema on the next
  [`run_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_concepts.md)
  call) instead of `user/`. Ignored if `save_to` is given explicitly.

- base_dir:

  Project root directory, used only to resolve the default `save_to`
  path. Defaults to the current working directory.

- dry_run:

  If TRUE (default), only build/save the adapted SQL - do not execute
  it. Set FALSE (or pass `con`) to execute immediately.

- overwrite:

  Prepend DROP TABLE IF EXISTS before CREATE. Default FALSE.

- origin_name:

  Optional human-readable source name for provenance.

- origin_url:

  Optional URL (repo, paper, DOI) for provenance.

- origin_ref:

  Optional citation string for provenance.

- con:

  Optional explicit DBI connection. Implies dry_run = FALSE.

## Value

The adapted SQL string invisibly. Inspect it before executing.

## Details

Takes a SQL file (or inline SQL string) found online, in a paper
supplement, or written by hand and does three things:

1.  Strips known schema prefixes (mimiciv_hosp., mimiciv_icu.,
    eicu_crd., etc.) so bare table names resolve against search_path.

2.  Wraps the query in `CREATE TABLE {target_schema}.{concept_name} AS`.

3.  Optionally saves the adapted SQL and/or executes it immediately,
    registering provenance in concept_registry.

After running, the concept is queryable with fetch_concept().

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehrdov", creds)

# Preview adapted SQL from a file (does NOT execute, but does save to
# sql/mimic_iv_demo/concepts_sqls/user/sofa_renal_custom.sql by default)
adapted <- register_concept_sql(
  "downloads/sofa_renal.sql",
  src_name     = "mimic_iv_demo",
  concept_name = "sofa_renal_custom"
)
cat(adapted)

# Execute, with provenance, keeping the default save location
register_concept_sql(
  "downloads/sofa_renal.sql",
  src_name     = "mimic_iv_demo",
  concept_name = "sofa_renal_custom",
  schema       = "derived",
  dry_run      = FALSE,
  overwrite    = TRUE,
  origin_name  = "Smith et al. 2023",
  origin_url   = "https://doi.org/10.xxxx",
  origin_ref   = "Smith J. et al., J Neurotrauma 2023"
)

# Inline SQL string, executed immediately, not saved to disk
register_concept_sql(
  "SELECT subject_id, hadm_id, stay_id,
          charttime AS index_time,
          valuenum  AS value
   FROM   mimiciv_icu.chartevents
   WHERE  itemid = 220045",
  src_name     = "mimic_iv_demo",
  concept_name = "heart_rate_raw",
  save_to      = FALSE,
  dry_run      = FALSE
)
} # }
```

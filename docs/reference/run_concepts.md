# Extract concepts for one source

Create source-specific concept tables from packaged definitions,
supplemental concept definitions, and user or external SQL. Select one
or more layers with `layers`; by default, all enabled layers are
processed.

## Usage

``` r
run_concepts(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  engine = c("postgres", "duckdb"),
  user_concepts = NULL,
  user_schema = NULL,
  external_sql_dir = NULL,
  external_schema = NULL,
  concept_name = NULL,
  dry_run = FALSE,
  layers = NULL,
  rm_duplicates = TRUE,
  concepts = NULL
)
```

## Arguments

- src_name:

  Character string identifying the source.

- db_name:

  Character string naming the PostgreSQL database. If `NULL`, use the
  active project configuration, falling back to `"rendehr"`. Ignored
  when `engine = "duckdb"`.

- base_dir:

  Character string giving the project directory, or `NULL` to resolve it
  from the active project configuration and working directory.

- engine:

  Character string, `"postgres"` or `"duckdb"`. Default: `"postgres"`.
  DuckDB uses the source database path returned by
  [`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md).

- user_concepts:

  Character string giving a supplemental concept CSV. A concept defined
  there replaces all packaged rows with the same source and concept
  name; other packaged concepts are kept unchanged. The file must exist.
  If `NULL`, use a configured supplemental file when available.

- user_schema:

  Character string naming the target schema for user SQL. `NULL` uses
  the source concepts schema.

- external_sql_dir:

  Character string giving an external SQL directory. `NULL` uses the
  external-concept directory returned by
  [`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md).

- external_schema:

  Character string naming the target schema for a single external
  module. `NULL` derives the schema from the module name. Separate
  submodules always receive separate schemas.

- concept_name:

  Character vector of concept names or filenames without `.sql`. Filters
  the `flat`, `ts`, `dur`, and `user` layers; `NULL` includes all
  concepts in those layers. The `ehr` and `external` layers are not
  filtered.

- dry_run:

  Logical. If `TRUE`, prepare concept SQL without building concept
  tables or registering provenance. Default: `FALSE`. See Details for
  database connection and file-writing behavior.

- layers:

  Character vector of layer names from the table below, or `NULL` to run
  all enabled layers. Layers run in the listed order, and required input
  tables must already exist.

- rm_duplicates:

  Logical. Remove chart rows that are validated copies of lab results
  (MIMIC) or of monitor values (eICU) in the ts and dur layers, and
  repeated chart rows of a ts concept (see
  [`run_atomic_concepts_unified()`](https://ate-driven-lab.github.io/rendehR/reference/run_atomic_concepts_unified.md)).
  Default `TRUE`. `FALSE` extracts exactly the same rows and removes
  none.

- concepts:

  Named list from
  [`concept_options()`](https://ate-driven-lab.github.io/rendehR/reference/concept_options.md)
  or an ordinary named list of concept options. A setting cannot be
  supplied both here and as a direct argument, even if the values are
  equal.

## Value

Invisibly, a named list of results for layers that processed at least
one concept or file. In an executing run, each element is a named
logical vector indicating success (`TRUE`) or failure (`FALSE`). For a
dry run, the `flat`, `ts`, `dur`, and `user` elements are named
character vectors of SQL; `ehr` and `external` report SQL adaptation
success as logical vectors. Execution failures raise an error instead of
returning a completed result.

## Details

The source's cohort and required observation-window tables must already
exist. The function opens and closes its own database connection.

|            |                                  |                                         |
|------------|----------------------------------|-----------------------------------------|
| Layer      | Definitions                      | Output                                  |
| `flat`     | Packaged SQL                     | Source concepts schema                  |
| `ts`       | Time-series definitions from CSV | Source concepts schema                  |
| `dur`      | Duration definitions from CSV    | Source concepts schema                  |
| `ehr`      | Maintained derived-concept SQL   | Source derived schema                   |
| `user`     | Project SQL                      | Source concepts schema or `user_schema` |
| `external` | External SQL modules             | Module-specific schemas                 |

Upstream `repo` SQL is reference material and is not an executable
layer.

The time-series and duration SQL generators support PostgreSQL and
DuckDB. Other SQL must be compatible with the selected engine. The `ehr`
layer requires PostgreSQL: it is skipped in a default DuckDB run, and
explicitly requesting it with DuckDB raises an error.

A dry run still opens a database connection, may initialize schemas, and
writes generated time-series or duration SQL files to the configured
output directories. It does not build concept tables. Reported SQL
failures in an executing run are collected and raised together after the
selected layers have been attempted.

## See also

[`concept_options()`](https://ate-driven-lab.github.io/rendehR/reference/concept_options.md),
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md),
[`run_flat_sql_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_flat_sql_concepts.md),
[`run_atomic_concepts_unified()`](https://ate-driven-lab.github.io/rendehR/reference/run_atomic_concepts_unified.md),
[`run_external_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_external_concepts.md)

## Examples

``` r
selected <- concept_options(layers = c("flat", "ts"))

if (FALSE) { # \dontrun{
# Requires an existing cohort and configured database connection.
run_concepts("mimic_iv_demo", base_dir = "project", layers = "flat")
run_concepts("mimic_iv_demo", base_dir = "project", concepts = selected)
} # }
```

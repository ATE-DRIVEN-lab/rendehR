# Copy a source's schemas into a different Postgres database

General-purpose clone: works for any source and any pair of databases,
whatever they're actually named - it just copies whichever of the
source's schemas you ask for from `from_db` to `to_db`. Creates the
target database if it does not exist, then copies every table from each
requested schema using `pg_dump | pg_restore` via system calls (requires
`pg_dump` in PATH). This is the correct approach when you want full
isolation: a colleague can connect to the target project database
without touching the source database at all.

## Usage

``` r
clone_source_to_db(
  src_name,
  from_db,
  to_db,
  creds,
  schemas = c("raw", "concepts", "derived", "harmonized"),
  overwrite = FALSE
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- from_db:

  Source database name - whatever your project's database is actually
  called (e.g. "rendehr" is just the common default when nothing
  overrides it; see `.ehr_resolve_db_name()`). Not tied to any specific
  name - pass whatever database you're actually copying from.

- to_db:

  Target database name. Created if it does not exist.

- creds:

  List from load_credentials(). Same credentials used for both.

- schemas:

  Which schemas to copy, as keys of
  [`src_schema_names()`](https://ate-driven-lab.github.io/rendehR/reference/src_schema_names.md).
  Default: all four of the source's schemas (raw, concepts, derived,
  harmonized). "ts", "flat" and "dur" are accepted and resolve to the
  harmonized schema that holds those tables.

- overwrite:

  Drop and recreate schemas in the target. Default FALSE.

## Value

The name of the target database, invisibly.

## Details

Alternatively, just point
[`run_one_source()`](https://ate-driven-lab.github.io/rendehR/reference/run_one_source.md)
at a different `db_name` from the start - that is the simplest way to
build an isolated project database.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")

# "rendehrdov" below is just an example database name (a hypothetical dev
# database), not a requirement - substitute whatever from_db actually is.
clone_source_to_db("mimic_iv_demo",
                   from_db  = "rendehrdov",
                   to_db    = "project_rehab",
                   creds    = creds,
                   schemas  = c("raw", "concepts", "harmonized"))

# Now connect to the isolated project DB
ehr_connect("project_rehab", creds)
fetch_concept("mimic_iv_demo", "heart_rate")
} # }
```

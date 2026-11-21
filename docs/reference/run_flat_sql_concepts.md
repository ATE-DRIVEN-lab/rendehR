# Execute flat SQL concept files for one source

Reads every .sql file in `flat_sql_dir`, substitutes `{raw_schema}` and
`{concepts_schema}` with the actual schema names, then executes each
file. SQL files are expected to use `{raw_schema}` and
`{concepts_schema}` as placeholders - set by the bulk-replacement step
during package setup.

## Usage

``` r
run_flat_sql_concepts(
  con = NULL,
  flat_sql_dir,
  raw_schema,
  concepts_schema,
  skip_on_error = FALSE,
  concept_name = NULL,
  dry_run = FALSE
)
```

## Arguments

- con:

  DBI connection.

- flat_sql_dir:

  Path to the directory containing flat .sql files.

- raw_schema:

  Raw schema name (e.g. "mimic_iv_demo_coh").

- concepts_schema:

  Concepts schema name (e.g. "mimic_iv_demo_coh_concepts").

- skip_on_error:

  If TRUE, log errors and continue; stop on first failure if FALSE
  (default FALSE).

- concept_name:

  Optional character vector to restrict execution to specific concept
  file(s) (matched against the filename without `.sql`), instead of
  every file in `flat_sql_dir`.

- dry_run:

  If TRUE, adapt each matching file's SQL without executing it. `con` is
  not required in this mode. Default FALSE.

## Value

Invisibly returns a named logical vector (TRUE = success per file). When
`dry_run = TRUE`, instead invisibly returns a named character vector of
`file -> adapted SQL text`, and nothing is executed.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
run_flat_sql_concepts(con, flat_sql_dir = "sql/mimic_iv/flat",
  raw_schema = "mimic_iv_demo_coh", concepts_schema = "mimic_iv_demo_coh_concepts")
} # }
```

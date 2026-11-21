# Run MIMIC-code / eICU-code handcrafted SQL concepts.

Executes the adapted SQL scripts in `sql_dir` (by default the project's
`concepts_sqls/external/` directory;
[`run_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/run_concepts.md)
also uses it for the packaged `concepts_sqls/ehr/` layer) in dependency
order. Tables are written to `{src_name}_coh_derived`.

## Usage

``` r
run_external_concepts(
  con,
  paths,
  schema_names,
  src_name,
  sql_dir = NULL,
  categories = NULL,
  skip_on_error = FALSE,
  execute = TRUE,
  save_adapted = FALSE,
  save_to = NULL,
  target_schema = NULL
)
```

## Arguments

- con:

  DBI connection (search_path will be set via postgres-functions.sql).

- paths:

  List from ehr_paths(). Must include `external_sql_dir` when `sql_dir`
  is not given.

- schema_names:

  List from src_schema_names(). Must include `raw` and `derived`.

- src_name:

  Source name used for schema adaptation.

- sql_dir:

  Optional SQL directory. Defaults to `paths$external_sql_dir`.

- categories:

  Character vector of category subdirs to run. NULL = all. Valid values:
  "demographics", "measurement", "comorbidity", "medication",
  "treatment", "firstday", "organfailure", "score", "sepsis".

- skip_on_error:

  If TRUE, log errors and continue; if FALSE, stop on first failure
  (default FALSE).

- execute:

  If TRUE, execute adapted SQL. If FALSE, only adapt/save SQL.

- save_adapted:

  If TRUE, save adapted SQL files.

- save_to:

  Optional output directory for adapted SQL.

- target_schema:

  Optional schema to write user/external concepts to.

## Value

Invisibly returns a named logical vector: TRUE = success per file.

## Details

The SQL files were converted from mimic-code for a specific demo schema.
When `src_name` differs from the schema baked into the files, schema
names are substituted on-the-fly before execution (demo -\> full, or
vice versa).

The "comorbidity" and "score" categories (e.g. `charlson.sql`,
`elixhauser_*.sql`, `apsiii.sql`, `sapsii.sql`, and the MIMIC-III
`sepsis/{angus,explicit,martin}.sql` and `diagnosis/ccs_dx.sql` files)
are unmodified, third-party clinical definitions from the public MIT-LCP
`mimic-code`/`eicu-code` repositories. They are executed unconditionally
whenever the source's packaged `mimic_code`/`eicu_code` directory
exists. This package does not independently re-validate their clinical
content (ICD code lists, scoring formulas), consistent with how
`icdpicr`/ `icdpicr2` are treated as trusted external dependencies. In
particular, `charlson.sql`/`elixhauser_*.sql` here are a *separate*,
independently computed comorbidity output from
[`run_comorbidities()`](https://ate-driven-lab.github.io/rendehR/reference/run_comorbidities.md)'s
accepted `raw.comorbidities` table: they live in a different schema
(`{src}_coh_derived` vs. `raw`) under different table names and use
different underlying logic (community SQL vs. the `comorbidity` R
package). `raw.comorbidities` is this package's accepted comorbidity
output; the tables produced here are a community reference, not a
replacement for it.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
run_external_concepts(con, paths = ehr_paths("mimic_iv_demo"),
  schema_names = src_schema_names("mimic_iv_demo"), src_name = "mimic_iv_demo")
} # }
```

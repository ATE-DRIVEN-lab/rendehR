# Execute atomic concept SQL for all concepts matching a source and type

Reads the concept config CSV, builds CREATE TABLE SQL for each concept,
writes it to sql_out_dir, and executes it against `con`.

## Usage

``` r
run_atomic_concepts_unified(
  con = NULL,
  concept_config_path,
  src_name,
  schema_names,
  typ = NULL,
  sql_out_dir,
  user_concepts = NULL,
  concept_name = NULL,
  dry_run = FALSE,
  engine = c("postgres", "duckdb"),
  rm_duplicates = TRUE
)
```

## Arguments

- con:

  DBI connection. Required unless `dry_run = TRUE`. Its dialect must
  match `engine`.

- concept_config_path:

  Path to rendehr_concepts_config.csv or
  rendehr_duration_concepts_config.csv.

- src_name:

  Source name (e.g. "mimic_iv_demo").

- schema_names:

  List from src_schema_names().

- typ:

  Concept type filter: "ts", "dur", or NULL for all.

- sql_out_dir:

  Directory to write generated SQL files.

- user_concepts:

  Optional path to a supplemental concept CSV. A concept defined here
  (same source + concept_name) replaces all package rows for that
  concept with all of its rows in this file; other package concepts are
  kept unchanged. Lets users add or redefine concepts without editing
  package files. An error if the file does not exist.

- concept_name:

  Optional character vector to restrict execution to specific concept
  name(s), instead of every concept matching `src_name`/ `typ`. Useful
  while iterating on one concept without rebuilding the rest.

- dry_run:

  If `TRUE`, build (and still write to `sql_out_dir`) the SQL for each
  matching concept without executing it. `con` is not required in this
  mode. Default `FALSE`.

- engine:

  SQL dialect to generate: `"postgres"` (default) or `"duckdb"`. Only
  affects the small handful of clauses that differ between the two
  (currently: the case-insensitive regex match used by `rgx_exp_itm`
  concepts). Must match the dialect `con` actually speaks.

- rm_duplicates:

  Logical. If `TRUE` (default), remove chart rows that are copies of lab
  results. A MIMIC chart row (MetaVision or CareVue) flagged
  `required_dedup = TRUE` in the concept config is dropped only when one
  of the concept's extracted lab rows (or an item named in its
  `dedup_cross_reference`, as `concept:itemid`) has the same subject and
  admission, the same stored value, an exact allowed offset between the
  original timestamps (MIMIC-IV 0, MIMIC-III 0 or +60 minutes), and the
  same canonical unit after each row's configured conversion. A blank
  chart unit is treated as the concept's target unit; any other
  unresolved unit is kept. An eICU nurse-charted vital (`nursecharting`)
  flagged `required_dedup = TRUE` is dropped when the same concept has a
  `vitalperiodic` or `vitalaperiodic` row for the same unit stay, minute
  offset and value; the monitor row is kept. In addition, chart rows of
  a ts concept that agree on all ID columns, minute, value and unit (for
  example two CareVue items charting the same glucose) are kept once, as
  the row with the lowest item id; rows with a NULL value and rows from
  other tables (labs, drugs) are never removed by this rule. `FALSE`
  extracts exactly the same rows and removes none. Rules for a concept
  replaced through `user_concepts` are disabled, as are cross-concept
  references to a replaced concept. A cross-concept reference is used
  only when its owning concept is built successfully in the same call;
  otherwise the chart copies are retained.

## Value

Invisibly returns a named logical vector, one entry per attempted
concept, `TRUE` if its table was built successfully and `FALSE` if its
`CREATE TABLE` execution failed. Returns `invisible(logical(0))` when no
concept matches the source/type filter. A failed concept does not stop
the remaining concepts from being attempted; the caller is responsible
for inspecting the returned vector to detect partial or total failure.
When `dry_run = TRUE`, instead invisibly returns a named character
vector of `concept_name -> generated SQL text`, and nothing is executed.

## Details

The configuration is checked with
[`validate_concept_config()`](https://ate-driven-lab.github.io/rendehR/reference/validate_concept_config.md)
before any SQL is built. Its problems are reported in one immediate
warning; rows with a `"skip"` problem (an unknown `itm_class`, or an
empty `match_value` for a class other than `col_itm`) are left out and
the remaining rows are built. A concept built without some of its rows
is reported as `OK: <concept> (<n> config row(s) skipped: <reasons>)`.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
run_atomic_concepts_unified(con,
  concept_config_path = system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"),
  src_name = "mimic_iv_demo", schema_names = src_schema_names("mimic_iv_demo"),
  typ = "ts", sql_out_dir = "sql/generated/mimic_iv_demo/ts", concept_name = "heart_rate")
} # }
```

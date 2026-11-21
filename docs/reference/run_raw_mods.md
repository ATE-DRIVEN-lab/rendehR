# Build modified (\_mod) versions of raw tables for a source

Runs SQL files from the source's `raw_mods` directory (package-embedded
or local override) to create preprocessed versions of raw tables. Each
SQL file produces one `{table}_mod` table in the raw schema, which the
concept config can reference directly.

## Usage

``` r
run_raw_mods(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  skip_on_error = FALSE,
  execution = NULL
)
```

## Arguments

- src_name:

  Source name (e.g. "eicu_demo").

- db_name:

  Optional Postgres database name. If NULL, uses the active project
  config, then "rendehr".

- base_dir:

  Project root directory. Defaults to getwd().

- skip_on_error:

  If TRUE, warn and continue evaluating the remaining SQL files, then
  fail the overall operation if any required file failed. Default FALSE.

- execution:

  Optional overrides from
  [`execution_options()`](https://ate-driven-lab.github.io/rendehR/reference/execution_options.md)
  or a named list.

## Value

Invisibly TRUE if mods were run, NULL if no mod directory found.

## Details

The package ships default mods for eICU:

- `infusiondrug_mod` - parses the drug rate and its unit from `drugrate`
  and the bracket at the end of `drugname`

- `medication_mod` - parses the dose and unit from `dosage` and names
  orders with a blank `drugname` from their drug code

- `admissiondrug_mod`- trims the home-medication name and unit, with the
  same `mod_*` column names as `medication_mod`

- `lab_mod`, `respiratorycharting_mod` - clean FiO2 values to percent

Before the SQL runs, the lookup tables it reads are written to the raw
schema: for eICU, `rendehr_eicu_hicl_drugname_lookup` (drug code to drug
name). A file of the same name in the project config directory replaces
the shipped one.

MIMIC-IV and MIMIC-III have no default mods. Add your own SQL files to
`sql/{src_name}/raw_mods/` in your project directory to override or
extend.

## Examples

``` r
if (FALSE) { # \dontrun{
run_raw_mods("eicu_demo", db_name = "rendehr")
} # }
```

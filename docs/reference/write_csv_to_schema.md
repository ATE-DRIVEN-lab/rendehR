# Write a CSV file directly to a source schema

Reads the CSV into R then calls write_table(). Useful for pushing
annotation files or external lookups without a separate import step.

## Usage

``` r
write_csv_to_schema(
  src_name,
  csv_path,
  table_name = NULL,
  schema = "raw",
  overwrite = TRUE,
  append = FALSE,
  con = NULL
)
```

## Arguments

- src_name:

  Source name.

- csv_path:

  Path to the CSV file.

- table_name:

  Name to give the table in Postgres. Defaults to the filename without
  extension.

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string.

- overwrite:

  Drop and recreate if table already exists.

- append:

  Append rows to existing table.

- con:

  Optional explicit DBI connection.

## Value

The number of rows written, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
write_csv_to_schema("mimic_iv_demo", "data/my_labels.csv",
                    schema = "raw")
} # }
```

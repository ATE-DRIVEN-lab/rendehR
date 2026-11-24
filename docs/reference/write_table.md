# Write a data frame to a source schema

Pushes a tibble or data frame into any schema associated with a source.
Opens a connection, writes the table, closes the connection.

## Usage

``` r
write_table(
  src_name,
  df,
  table_name,
  schema = "raw",
  overwrite = TRUE,
  append = FALSE,
  con = NULL
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- df:

  Data frame or tibble to write.

- table_name:

  Name to give the table in Postgres.

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string.

- overwrite:

  Drop and recreate the table if it already exists.

- append:

  Append rows to an existing table. Overrides overwrite.

- con:

  Optional explicit DBI connection.

## Value

The number of rows written, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", creds)

write_table("mimic_iv_demo", my_annotations, "annotations", schema = "raw")
write_table("mimic_iv_demo", new_rows, "annotations", schema = "raw",
            append = TRUE)
} # }
```

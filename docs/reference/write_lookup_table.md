# Write a data frame as a lookup table in Postgres

Write a data frame as a lookup table in Postgres

## Usage

``` r
write_lookup_table(con, schema, table_name, df, overwrite = TRUE)
```

## Arguments

- con:

  DBI connection.

- schema:

  Schema name.

- table_name:

  Table name.

- df:

  Data frame to write.

- overwrite:

  Drop and recreate if TRUE (default).

## Value

`TRUE`, invisibly, as returned by
[`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
write_lookup_table(con, "public", "my_codes", data.frame(code = c("S14", "S24"), group = "SCI"))
} # }
```

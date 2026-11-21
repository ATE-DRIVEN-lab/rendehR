# Open a connection to a DuckDB cohort database

Open a connection to a DuckDB cohort database

## Usage

``` r
connect_duckdb(db_path, read_only = FALSE)
```

## Arguments

- db_path:

  Path to the `.duckdb` file. Its parent directory is created if needed.
  Use `":memory:"` for an in-memory database.

- read_only:

  Open read-only (allows concurrent readers). Default FALSE.

## Value

A DBI connection. Disconnect with
`DBI::dbDisconnect(con, shutdown = TRUE)`.

## Examples

``` r
con <- connect_duckdb(":memory:")
DBI::dbGetQuery(con, "SELECT 42 AS answer")
#>   answer
#> 1     42
DBI::dbDisconnect(con, shutdown = TRUE)
```

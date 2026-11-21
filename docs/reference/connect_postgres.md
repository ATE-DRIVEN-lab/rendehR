# Open a connection to a Postgres database

General-purpose connection helper: `db_name` is required, with no
default, so this connects to whatever database you actually name - not
tied to any database called "rendehr" specifically.

## Usage

``` r
connect_postgres(db_name, creds)
```

## Arguments

- db_name:

  Database name to connect to (e.g. "rendehrdov" below is just an
  example database name, not a requirement).

- creds:

  List returned by load_credentials(). `creds$postgres$sslmode` (or
  `PGSSLMODE`) sets the libpq SSL mode; the default is `"prefer"`.

## Value

DBI connection object.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")
con <- connect_postgres("rendehr", creds)
DBI::dbListTables(con)
DBI::dbDisconnect(con)
} # }
```

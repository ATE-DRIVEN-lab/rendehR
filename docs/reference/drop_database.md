# Drop a Postgres database entirely

Connects as admin (to the "postgres" database) and drops the target
database. Forces disconnection of any open sessions first. Dry-run by
default.

## Usage

``` r
drop_database(db_name, creds = NULL, dry_run = TRUE, confirm = NULL)
```

## Arguments

- db_name:

  Database to drop.

- creds:

  Credentials list (from load_credentials()). Uses stored ehr_connect()
  creds if NULL.

- dry_run:

  If TRUE (default), only print what would be dropped.

- confirm:

  The database name typed by the user to confirm the drop. If `NULL`
  (default), an interactive session asks for it at the console; a
  non-interactive session cannot confirm, so nothing is dropped. Apps
  pass the name the user typed in their own confirmation dialog.

## Value

`TRUE` invisibly when the database was dropped, `FALSE` when the drop
was not confirmed, `NULL` for a dry run.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")
drop_database("rendehr_scratch", creds)                   # dry run: reports only
drop_database("rendehr_scratch", creds, dry_run = FALSE)
drop_database("rendehr_scratch", creds, dry_run = FALSE, confirm = "rendehr_scratch")
} # }
```

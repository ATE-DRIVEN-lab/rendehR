# Browse one source schema with interactive \$ access

Returns an environment where every table in the schema is an active
binding. Accessing a name returns a lazy
[`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html) - no data
is transferred until you call `collect()`.

## Usage

``` r
browse_source(
  src_name,
  schema = "raw",
  con = NULL,
  db_name = NULL,
  config_path = NULL
)

open_source(
  src_name,
  schema = "raw",
  con = NULL,
  db_name = NULL,
  config_path = NULL
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- schema:

  Which schema to attach: "raw", "concepts", "derived", "harmonized".
  Also accepts a literal schema name string.

- con:

  Optional explicit DBI connection. If NULL, opens a connection using
  `db_name`, the active/default project config, and credentials.

- db_name:

  Optional Postgres database name. If NULL, uses the active project
  config, then the default config file, then "rendehr".

- config_path:

  Optional project config path. If NULL and no config is active, uses
  `EHR_CONFIG` or `config/ehr_config.json` when present.

## Value

An environment of class `ehr_source` with one active binding per table
and a `$disconnect()` method.

## Details

The connection is kept open for the lifetime of the object. Call
`src$disconnect()` when finished if the browser opened the connection.

## Examples

``` r
if (FALSE) { # \dontrun{
raw  <- browse_source("mimic_iv_demo", "raw",
                      config_path = "config/ehr_config.json")
raw$admissions                          # lazy tbl
raw$admissions |> dplyr::collect()     # pull into R
raw$icu_windows |> dplyr::glimpse()
raw$disconnect()

cpts <- browse_source("mimic_iv_demo", "concepts")
cpts$heart_rate |> dplyr::collect()
cpts$disconnect()
} # }
```

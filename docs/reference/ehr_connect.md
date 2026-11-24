# Store database credentials for use by all fetch\_\* functions

Only needs to be called once per session. After this, all fetch\_\*
functions open and close their own connections automatically.

## Usage

``` r
ehr_connect(db_name = NULL, creds = NULL, config_path = NULL)
```

## Arguments

- db_name:

  Optional Postgres database name (e.g. "rendehr"). If NULL, uses the
  active project config.

- creds:

  Optional list from
  [`load_credentials()`](https://ate-driven-lab.github.io/rendehR/reference/load_credentials.md).
  If NULL, credentials are loaded with `load_credentials(config_path)`.

- config_path:

  Optional path to a JSON credentials file, usually
  "config/config_pass.json". Ignored when `creds` is supplied. If NULL,
  uses the active project config credentials path when available.

## Value

`NULL`, invisibly. The credentials are kept for the rest of the session.

## Details

Credentials can be passed directly with `creds`, loaded from a JSON file
with `config_path`, or resolved by
[`load_credentials()`](https://ate-driven-lab.github.io/rendehR/reference/load_credentials.md)
from environment variables or interactive prompts. If
[`ehr_use_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_use_config.md)
has loaded a project config, `db_name` and the credentials path can come
from that config.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")
ehr_connect("rendehr", creds)

ehr_connect("rendehr", config_path = "config/config_pass.json")

ehr_use_config("config/ehr_config.json")
ehr_connect()
} # }
```

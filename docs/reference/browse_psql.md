# Browse databases on a Postgres server

Lists databases available on the Postgres server described by the
supplied credentials. This is server-level inspection: databases do not
need to be related to rendehR. By default, protected/system databases
(`postgres`, `template0`, `template1`) are hidden, but they can be shown
with `include_system = TRUE`.

## Usage

``` r
browse_psql(
  config_path = NULL,
  creds = NULL,
  maintenance_db = "postgres",
  include_system = FALSE,
  include_unconnectable = FALSE
)
```

## Arguments

- config_path:

  Optional path to either `config/config_pass.json` (Postgres
  credentials) or `config/ehr_config.json` (project config pointing to
  credentials). If NULL, credentials are resolved from the
  active/default config and environment variables.

- creds:

  Optional credentials list from
  [`load_credentials()`](https://ate-driven-lab.github.io/rendehR/reference/load_credentials.md).

- maintenance_db:

  Database used only for the listing connection. Default is
  `"postgres"`.

- include_system:

  Include `postgres`, `template0`, and `template1`? Default FALSE.

- include_unconnectable:

  Include databases that do not allow connections? Default FALSE.

## Value

A data frame with database metadata and example browsing code.

## Examples

``` r
if (FALSE) { # \dontrun{
browse_psql(config_path = "config/config_pass.json")
browse_psql(config_path = "config/ehr_config.json")
browse_psql(include_system = TRUE)
} # }
```

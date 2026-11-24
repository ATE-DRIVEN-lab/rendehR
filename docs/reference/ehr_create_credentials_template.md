# Create a starter credentials file

Writes a separate credentials JSON template for Postgres and PhysioNet.
This file is intentionally separate from `config/ehr_config.json` so
users can keep paths/settings apart from private credentials. Existing
files are not overwritten unless `overwrite = TRUE`.

## Usage

``` r
ehr_create_credentials_template(
  path = file.path("config", "config_pass.json"),
  postgres_host = "localhost",
  postgres_port = 5432L,
  postgres_user = "postgres",
  postgres_password = "",
  physionet_username = NULL,
  physionet_password = NULL,
  overwrite = FALSE
)
```

## Arguments

- path:

  Output path. Defaults to `config/config_pass.json`.

- postgres_host:

  Default Postgres host.

- postgres_port:

  Default Postgres port.

- postgres_user:

  Default Postgres user.

- postgres_password:

  Default Postgres password. Empty string means no password.

- physionet_username:

  Default PhysioNet username. NULL means ask at download time.

- physionet_password:

  Default PhysioNet password. NULL means ask at download time.

- overwrite:

  Overwrite an existing file? Default FALSE.

## Value

Invisibly returns `path`.

## Examples

``` r
path <- file.path(tempdir(), "config_pass.json")
ehr_create_credentials_template(path, postgres_user = "postgres")
#> rendehR: credentials template written to '/tmp/Rtmpew7WRg/config_pass.json'.
file.exists(path)
#> [1] TRUE
```

# Load Postgres and PhysioNet credentials

Postgres fields are resolved from environment variables, then the nested
JSON credentials file, then documented defaults. Environment variables
(`PGHOST`, `PGPORT`, `PGUSER`, `PGPASSWORD`, `PGSSLMODE`) therefore
override the file. `sslmode` defaults to `"prefer"` (use TLS when the
server offers it); set `postgres.sslmode` or `PGSSLMODE` to `"disable"`
or `"require"` to change that. PhysioNet uses only `PHYSIONET_USER` and
`PHYSIONET_PASS`, followed by nested `physionet.username`
(`physionet.user` is accepted as an alias) and `physionet.password`.
Missing PhysioNet credentials are allowed until a protected operation
calls
[`set_physionet_env()`](https://ate-driven-lab.github.io/rendehR/reference/set_physionet_env.md).
One credential set is retained for the selected file/project during the
active R session and is replaced when that safe source identity changes.

## Usage

``` r
load_credentials(path = NULL)
```

## Arguments

- path:

  Optional path to a JSON credentials file. If NULL, uses the active
  project config credentials path, then `config/config_pass.json` if it
  exists.

## Value

Named list with `postgres` and `physionet` sub-lists.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")
names(creds)
} # }
```

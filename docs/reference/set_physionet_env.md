# Set credentials for a protected PhysioNet operation

Requires non-missing values and sets only `PHYSIONET_USER` and
`PHYSIONET_PASS`. Credential values are never included in diagnostics.

## Usage

``` r
set_physionet_env(creds)
```

## Arguments

- creds:

  List returned by load_credentials().

## Value

The logical vector returned by
[`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html), invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
creds <- load_credentials("config/config_pass.json")
set_physionet_env(creds)
} # }
```

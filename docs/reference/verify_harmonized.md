# Verify harmonized ts_tab, flat_tab, and dur_tab outputs

Verify harmonized ts_tab, flat_tab, and dur_tab outputs

## Usage

``` r
verify_harmonized(
  con,
  schema,
  ts_table = "ts_tab",
  flat_table = "flat_tab",
  dur_table = "dur_tab",
  required_ts_cols = c("time_since_hadm_min", "concept_name", "value", "unit")
)
```

## Arguments

- con:

  DBI connection.

- schema:

  Harmonized output schema (e.g. "mimic_iv_demo_coh_harmonized").

- ts_table:

  Name of the time-series table.

- flat_table:

  Name of the flat table.

- dur_table:

  Name of the duration table.

- required_ts_cols:

  Expected columns in ts_tab; NULL skips column check.

## Value

Verification result list.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
verify_harmonized(con, "mimic_iv_demo_coh_harmonized")
} # }
```

# Verify a source's cohort Parquet artifact

Checks the cohort Parquet directory written by
[`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
(`paths$cohort_parquet_dir`), not the raw downloads;
[`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
verifies those against the release checksums itself.

## Usage

``` r
verify_download(paths, required_tables = NULL)
```

## Arguments

- paths:

  List from ehr_paths().

- required_tables:

  Character vector of bare table names to check. A table is present as a
  flat `<table>.parquet` artifact (what the cohort builders write) or as
  a `<table>/` directory of parquet parts. NULL checks that any parquet
  files exist and are non-empty.

## Value

Verification result list (ok, checks, messages).

## Examples

``` r
if (FALSE) { # \dontrun{
paths <- ehr_paths("mimic_iv_demo")
verify_download(paths, required_tables = c("patients", "admissions", "labevents"))
} # }
```

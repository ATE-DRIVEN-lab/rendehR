# Download raw EHR source tables

Downloads one or more tables from a configured PhysioNet source.

## Usage

``` r
download_source(src_name = NULL, tables = NULL, raw_data_root = NULL)
```

## Arguments

- src_name:

  Source name (e.g. `"mimic_iii_demo"`, `"eicu_demo"`). Defaults to the
  active project config source, then `"mimic_iv_demo"`.

- tables:

  Optional character vector of table names to download. If NULL, all
  tables for the source are downloaded.

- raw_data_root:

  Directory to store raw downloaded files. This may be either a root
  directory, in which case files are saved under
  `{raw_data_root}/{src_name}`, or the exact source directory ending in
  `src_name`. Defaults to the raw data root implied by
  `ehr_paths(src_name)`.

## Value

Invisibly returns the downloaded filenames.

## Details

Releases, URLs, files, retained columns, and types come from the sole
generated runtime catalog `inst/extdata/rendehr-data-source.json`.

Credentials use the accepted canonical `PHYSIONET_USER` and
`PHYSIONET_PASS` contract. Downloads resume in `.part` files. Existing
and new artifacts are verified against the release-bound SHA-256
manifest before use or publication when the provider publishes one. An
explicit HTTP 404 or 410 for `SHA256SUMS.txt` selects a disclosed
lower-assurance fallback that requires exact positive `Content-Length`
and file-format/readability checks. Malformed, incomplete,
authentication, transient-network, and server-error manifest failures do
not downgrade. Mismatching files are reported and preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
download_source("mimic_iv_demo")                       # open demo, no credentials
download_source("mimic_iv", tables = c("patients", "admissions"))
} # }
```

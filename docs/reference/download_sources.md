# Download raw EHR source tables for multiple sources

Loops over
[`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
for each source name. Source-specific folders are still created
internally as `{raw_data_root}/{src_name}`.

## Usage

``` r
download_sources(src_names = NULL, tables = NULL, raw_data_root = NULL)
```

## Arguments

- src_names:

  Character vector of source names. If NULL, uses the active project
  config source, then "mimic_iv_demo".

- tables:

  Optional character vector of table names to download for each source.
  If NULL, all tables for each source are downloaded.

- raw_data_root:

  Directory for raw downloaded files. This may be either a root
  directory or an exact source directory ending in `src_name`. If NULL,
  each source uses the root implied by `ehr_paths(src_name)`.

## Value

Invisibly returns a named list of downloaded filenames by source.

## Examples

``` r
if (FALSE) { # \dontrun{
download_sources(c("mimic_iii_demo", "mimic_iv_demo", "eicu_demo"))
} # }
```

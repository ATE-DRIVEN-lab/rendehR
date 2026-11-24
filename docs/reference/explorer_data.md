# Build the data bundle for the cohort explorer

Reads an admission-level merged schema (built by
[`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md))
and the per-source visit windows into plain data frames for
[`run_explorer()`](https://ate-driven-lab.github.io/rendehR/reference/run_explorer.md).
Patient time series are not loaded here: the explorer reads them per
patient.

## Usage

``` r
explorer_data(
  schema,
  con = NULL,
  sources = NULL,
  raw_dirs = NULL,
  cache_dir = NULL
)
```

## Arguments

- schema:

  Name of the merged schema (the `target_schema` given to
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)).

- con:

  Optional open DBI connection. If NULL, the credentials stored by
  [`ehr_connect()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_connect.md)
  are used.

- sources:

  Optional character vector of source names to keep.

- raw_dirs:

  Optional named character vector of raw data directories per source,
  used by the raw-versus-harmonized laboratory check. When NULL the
  directories configured with
  [`ehr_paths()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_paths.md)
  are tried.

- cache_dir:

  Optional directory for a cached copy of the bundle (PostgreSQL only).
  The cache is reused while no table in the database has been created,
  dropped or re-analysed since, and the package build is the same;
  otherwise the bundle is rebuilt and the cache replaced. The file holds
  individual-level data: keep the directory private (it is written with
  owner-only permissions).

## Value

A list of class `rendehR_explorer_data` with elements `meta`, `adm`
(cohort admissions), `ctx_adm` (every admission in the schema), `icu`,
`patients`, `vars`, `groups`, `filters`, `concepts`, `med_prev` and
`checks`.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
d <- explorer_data("rendehr_admission")
nrow(d$adm)
} # }
```

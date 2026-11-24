# Run the cohort explorer

Starts an interactive Shiny explorer for a cohort built with rendehR: an
overview, a cohort explorer (a characteristics table with SMDs,
distributions, outcomes with Wilson confidence intervals, Kaplan-Meier
survival, cross-tabs, scatter plots), a subject explorer that reads each
patient's harmonized time series live (timeline, time-series panels,
medications, diagnoses and per-patient verification), a concept browser
with a concept builder, project setup and data checks. Optional spinal
cord injury (SCI) pages are added when `sci` is given.

## Usage

``` r
run_explorer(
  schema = NULL,
  db_name = NULL,
  creds = NULL,
  config_path = NULL,
  data = NULL,
  sci = NULL,
  raw_dirs = NULL,
  cache_dir = NULL,
  host = "127.0.0.1",
  port = NULL,
  launch.browser = interactive()
)
```

## Arguments

- schema:

  Merged schema to explore (the `target_schema` of
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)).
  When NULL, `"rendehr_admission"` (the setup assistant's default) is
  used if it exists, otherwise `"rendehr"` (the default of
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)).

- db_name, creds, config_path:

  Passed to
  [`ehr_connect()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_connect.md)
  when credentials have not been stored yet (or when `db_name` is
  given).

- data:

  Optional prebuilt bundle from
  [`explorer_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_data.md);
  `schema` is then ignored.

- sci:

  Optional SCI extension from
  [`explorer_sci_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_sci_data.md).

- raw_dirs:

  Optional raw data directories per source, see
  [`explorer_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_data.md).

- cache_dir:

  Optional directory where the data bundle is cached between runs, see
  [`explorer_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_data.md).
  Starting from the cache skips reading the merged tables; it holds
  individual-level data.

- host, port:

  Where the app listens.

- launch.browser:

  Open a browser window.

## Value

Called for its side effect (runs until stopped); returns the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
invisibly.

## Details

The app serves individual-level data and listens on `127.0.0.1` by
default. Do not deploy it publicly.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_use_config("config/ehr_config.json")
ehr_connect()
run_explorer("rendehr_admission")
} # }
```

# Build the cohort explorer Shiny app

Returns the Shiny app object for a data bundle from
[`explorer_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_data.md),
without starting it.
[`run_explorer()`](https://ate-driven-lab.github.io/rendehR/reference/run_explorer.md)
builds the bundle and runs the app; use `explorer_app()` to embed or
test the app.

## Usage

``` r
explorer_app(data, live = !is.null(.ehr_env$creds))
```

## Arguments

- data:

  A bundle from
  [`explorer_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_data.md)
  (optionally extended with
  [`explorer_sci_data()`](https://ate-driven-lab.github.io/rendehR/reference/explorer_sci_data.md)).

- live:

  Whether patient-level queries may reach the database. Defaults to TRUE
  when credentials have been stored with
  [`ehr_connect()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_connect.md).

## Value

A `shiny.appobj`.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
app <- explorer_app(explorer_data("rendehr_admission"))
shiny::runApp(app)
} # }
```

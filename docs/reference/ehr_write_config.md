# Write a project configuration JSON file

Writes a config list to JSON. This is intended for project settings and
paths only; keep passwords in the separate credentials file referenced
by `database.credentials_path`.

## Usage

``` r
ehr_write_config(config, path = NULL, overwrite = FALSE, pretty = TRUE)
```

## Arguments

- config:

  Named list to write.

- path:

  Output path. Defaults to the `EHR_CONFIG` environment variable or
  `config/ehr_config.json`.

- overwrite:

  Overwrite an existing file? Default FALSE.

- pretty:

  Pretty-print JSON? Default TRUE.

## Value

Invisibly returns `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- ehr_load_config("config/ehr_config.json")
cfg$database$db_name <- "rendehr"
ehr_write_config(cfg, "config/ehr_config.json", overwrite = TRUE)
} # }
```

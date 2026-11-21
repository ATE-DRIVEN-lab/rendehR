# List all concept tables available for a source

Returns concepts from the atomic concepts schema (\_coh_concepts), the
derived schema (\_coh_derived), and any named external schemas
(*coh_ext*\*). Joins against the concept_registry table (if it exists)
to add origin_name, origin_url, and origin_ref provenance columns.

## Usage

``` r
list_concepts(src_name, con = NULL)
```

## Arguments

- src_name:

  Source name.

- con:

  Optional explicit DBI connection.

## Value

A data frame with columns concept_name, schema, origin_name, origin_url,
origin_ref. Sorted alphabetically by concept name.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", config_path = "config/config_pass.json")
head(list_concepts("mimic_iv_demo"))
} # }
```

# Create a dedicated project schema in the connected database

Creates a schema named `project_{name}` (or the literal name if it
already starts with "project\_") in whichever database `con` is
connected to (or the active connection from
[`ehr_connect()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_connect.md)
if `con` is NULL) - not tied to any database named "rendehr"
specifically. Use write_table() and fetch_table() with
`schema = "project_myname"` to read and write project-specific tables
alongside the source schemas without polluting them.

## Usage

``` r
init_project_schema(name, con = NULL)
```

## Arguments

- name:

  Project name. Prefix "project\_" is added automatically.

- con:

  Optional explicit DBI connection.

## Value

The schema name (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehrdov", creds)

init_project_schema("rehab_outcomes")
# -> creates schema "project_rehab_outcomes"

# Write your derived data into it
write_table("mimic_iv_demo", my_analysis_df,
            table_name = "aki_flags",
            schema     = "project_rehab_outcomes")

# Read back
fetch_table("mimic_iv_demo", "aki_flags",
            schema = "project_rehab_outcomes")
} # }
```

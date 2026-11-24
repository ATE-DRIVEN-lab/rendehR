# Materialize a patient subgroup as new Postgres schemas

Creates independent schemas (e.g. `mimic_iv_demo_cervical_coh`,
`mimic_iv_demo_cervical_coh_concepts`, etc.) by filtering all tables
from the source schemas to the provided patient IDs. Tables that have no
patient ID column (lookup / reference tables) are copied as-is.

## Usage

``` r
materialize_subcohort(
  src_name,
  ids,
  subgroup_name,
  schemas = c("raw", "concepts", "harmonized"),
  overwrite = FALSE,
  con = NULL
)
```

## Arguments

- src_name:

  Original source name (e.g. "mimic_iv_demo").

- ids:

  Vector of patient IDs from build_subcohort() or custom.

- subgroup_name:

  Short label appended to src_name to form the new source name:
  `{src_name}_{subgroup_name}`. Example: `"cervical"` -\> new source
  `"mimic_iv_demo_cervical"`.

- schemas:

  Which schemas to copy. Default `c("raw", "concepts", "harmonized")`.
  Any of: `"raw"`, `"concepts"`, `"harmonized"`, `"derived"`.

- overwrite:

  Drop existing subgroup schemas before recreating. Default FALSE.

- con:

  Optional explicit DBI connection.

## Value

Invisibly returns the new source name.

## Details

After materialization the subgroup behaves like a regular source:
`fetch_concept("mimic_iv_demo_cervical", "heart_rate")` works without
any active-cohort filter.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", creds)

ids <- build_subcohort("mimic_iv_demo", scid_injury_level = "thoracic")

materialize_subcohort("mimic_iv_demo", ids, "thoracic")

# The subgroup is now a first-class source
fetch_concept("mimic_iv_demo_thoracic", "heart_rate")
cohort_summary("mimic_iv_demo_thoracic")

# Clean up when done
drop_subcohort("mimic_iv_demo", "cervical_complete")
} # }
```

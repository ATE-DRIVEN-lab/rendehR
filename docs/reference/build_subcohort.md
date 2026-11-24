# Build a patient subgroup ID list by filtering cohort_info

Queries cohort_info with equality filters on any column. Returns a
vector of patient IDs suitable for set_active_cohort() or
materialize_subcohort().

## Usage

``` r
build_subcohort(
  src_name,
  ...,
  id_col = NULL,
  sample_n = NULL,
  seed = NULL,
  con = NULL
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo").

- ...:

  Named filter arguments. Each name must be a column in cohort_info;
  each value is a vector of acceptable values (matched with SQL IN).
  Example: `scid_injury_level = c("thoracic", "lumbar")` (SCI profile).

- id_col:

  Optional identifier column to return/filter on. Defaults to
  `cohort_key(src_name)`.

- sample_n:

  Integer. Return a random sample of this size instead of all matching
  IDs. NULL returns all. Default NULL.

- seed:

  Integer seed for reproducible sampling. Default NULL.

- con:

  Optional explicit DBI connection.

## Value

A vector of patient IDs (integer or character depending on source).

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", creds)

# Thoracic or lumbar injuries (columns from the SCI cohort profile)
ids <- build_subcohort("mimic_iv_demo",
          scid_injury_level = c("thoracic", "lumbar"))

# Reproducible random sample of 10 thoracic patients
ids <- build_subcohort("mimic_iv_demo",
          scid_injury_level = "thoracic",
          sample_n     = 10,
          seed         = 42)

# No filters - sample 50 from the full cohort
ids <- build_subcohort("mimic_iv_demo", sample_n = 50)
} # }
```

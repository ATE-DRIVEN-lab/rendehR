# Compute hospital-level Charlson and Elixhauser comorbidities

Diagnoses are scored once per hospital admission. ICU identifiers are an
optional projection: admissions without an ICU remain in the output with
a missing ICU identifier, while admissions with multiple ICU stays
inherit the same hospital-level result on each ICU row.

## Usage

``` r
run_comorbidities(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  maps = c("charlson", "elixhauser"),
  con = NULL,
  icd_adjudication_path = NULL,
  publish = TRUE,
  enrichment = NULL
)
```

## Arguments

- src_name:

  Source name (for example, `"mimic_iii_demo"`).

- db_name:

  Optional Postgres database name.

- base_dir:

  Project root. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- maps:

  One or both of `"charlson"` and `"elixhauser"`.

- con:

  Optional explicit DBI connection.

- icd_adjudication_path:

  Optional reviewed eICU ICD-version crosswalk.

- publish:

  Whether to transactionally publish the result.

- enrichment:

  Optional overrides from
  [`enrichment_options()`](https://ate-driven-lab.github.io/rendehR/reference/enrichment_options.md)
  or a named list.

## Value

Invisibly returns the comorbidities data frame.

## Details

Flag columns record whether any code of the category is present, with
ICD-9 and ICD-10 evidence combined. Scores apply the standard hierarchy
to those combined flags (`comorbidity::score(assign0 = TRUE)`): when
both the mild and the severe form of a condition are present (for
example diabetes with and without complications, mild and severe liver
disease, solid tumour and metastatic cancer), only the severe form is
weighted.

eICU ICD versions are resolved from exact dictionary evidence and the
reviewed adjudication asset. Ambiguous or unmatched codes are excluded
from scoring and represented by `comorbidity_status`; regex suggestions
never determine a scoreable version.

## Examples

``` r
if (FALSE) { # \dontrun{
run_comorbidities("mimic_iv_demo", db_name = "rendehr", maps = c("charlson", "elixhauser"))
} # }
```

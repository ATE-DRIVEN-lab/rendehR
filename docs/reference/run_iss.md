# Compute hospital-level injury severity measures

ICD-9 diagnoses are scored with `icdpicr`; ICD-10 diagnoses are scored
with an isolated adapter for the `icdpicr2` 2.1 API (2.1.0 or later).
Diagnoses are scored once per hospital admission and then projected to
optional ICU identifiers. Admissions without an ICU remain present.
Ambiguous eICU ICD versions are excluded and represented by `iss_status`
rather than inferred by regex.

## Usage

``` r
run_iss(
  src_name,
  db_name = NULL,
  base_dir = NULL,
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

Invisibly returns the ISS data frame.

## Details

Both scorers run on every admission, and the reported
`iss`/`niss`/`maxais` are the element-wise maximum of the two: when an
admission has both an ICD-9 and an ICD-10 scoring the higher value is
reported, and when it has only one that one is reported.
`mechmaj`/`mechmin`/`intent` are categorical and cannot be maximised, so
they take the ICD-9 value when present and the ICD-10 value otherwise.
The per-version columns (`*_icd9`, `*_i10`) are always published
alongside, so either scoring can be recovered or compared.

`maxais` is the highest *known* AIS (1-6) across the two scorings. AIS
9 - "injury present, severity not specified" - is never treated as a
severity: it cannot outrank a known AIS, and an admission whose injuries
all have unknown severity gets `maxais`, `iss` and `niss` as NA with
`iss_status = "severity_unknown"`, distinct from `"no_mapped_condition"`
(no code maps to an injury; scores reported as 0). The per-version
columns keep the scorers' raw values, including 9.

This is deliberately a maximum and not a per-admission choice of coding
system. It matters most for eICU, which codes the same stay in both
systems (93.6% of stays in the reference SCID cohort carry both, at a
median 50/50 split), so the two scorings are two representations of the
same clinical content rather than two eras; taking the maximum keeps an
injury that is documented in only one of them.

## Examples

``` r
if (FALSE) { # \dontrun{
run_iss("mimic_iv_demo", db_name = "rendehr")
} # }
```

# Build a normalized eICU diagnosis ICD parquet table from CSV files

eICU stores one or more comma-separated ICD codes in
`diagnosis.icd9code` and does not provide an explicit ICD version. This
utility expands those codes to one row per diagnosis-code pair, joins
patient-level identifiers, resolves each code's ICD version, and writes
a standalone parquet file for audit and inspection.

## Usage

``` r
build_eicu_diagnosis_icd_parquet(
  diagnosis_csv,
  patient_csv,
  icd_dict_path,
  output_path,
  overwrite = FALSE,
  icd_adjudication_path = NULL
)
```

## Arguments

- diagnosis_csv:

  Path to eICU `diagnosis.csv` or `diagnosis.csv.gz`.

- patient_csv:

  Path to eICU `patient.csv` or `patient.csv.gz`.

- icd_dict_path:

  Path to diagnosis dictionary CSV/RDS with `icd_code`, `icd_version` (9
  or 10), and optionally `long_title`.

- output_path:

  Destination parquet file, or a directory where
  `diagnosis_icd_mod.parquet` should be written.

- overwrite:

  Overwrite an existing parquet file? Default `FALSE`.

- icd_adjudication_path:

  Optional reviewed eICU ICD-version crosswalk. Defaults to the packaged
  crosswalk, as in the pipeline.

## Value

Invisibly returns the written parquet file path.

## Details

Versions are resolved by the package's single eICU resolver - the same
one used for cohort selection, `cohort_info`, comorbidities and ISS - so
every code gets exactly the version the pipeline gives it: an exact
match of the dotted token against the dictionary, a compact match only
for tokens written without a dot, then an approved expert adjudication.
Codes that are official in both versions stay `ambiguous`, and codes
that match nothing stay `unmatched`; neither gets a version. There is no
regular-expression fallback: a regex guess is reported separately, in
`regex_version_suggestion`, for diagnosis only. This utility is not the
canonical eICU cohort selector and does not determine cohort membership.

Output columns include `icd_code_raw` (the source token), `icd_code`
(compact), `icd_version` (`"9"`, `"10"` or NA), `version_source` (the
resolution status: `exact_dotted`, `compact_unique`,
`expert_adjudicated`, `ambiguous`, `unmatched` or `malformed`),
`is_ambiguous`, `match_n` and `long_titles_all` (the dictionary
candidates the evidence points to, so ambiguity stays visible),
`long_title`, and `regex_version_suggestion`.

## Examples

``` r
if (FALSE) { # \dontrun{
build_eicu_diagnosis_icd_parquet(
  diagnosis_csv = "data/source_data/eicu/diagnosis.csv.gz",
  patient_csv = "data/source_data/eicu/patient.csv.gz",
  icd_dict_path = system.file("extdata", "d_icd_diagnoses.csv", package = "rendehR"),
  output_path = "data/eicu_diagnosis_icd.parquet")
} # }
```

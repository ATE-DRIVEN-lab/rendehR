# Build and optionally publish cohort information

`cohort_info` is a hospitalization-based association table that contains
generic columns (hospitalization sequence, diagnostic ICD summary,
composite visit key, and the cohort membership flag `cohort_index`).
When ICU identifiers exist, admission-level values may appear on
multiple ICU association rows; source event tables are not duplicated by
this function. Domain enrichment is added by `classify_fn`.

## Usage

``` r
run_cohort_info(
  con,
  schema_names,
  src_name,
  icd_filter_path = NULL,
  icds_dict_path = NULL,
  icd_adjudication_path = NULL,
  classify_fn = NULL,
  classify_args = list(),
  analysis_level = c("admission", "icu"),
  publish = TRUE
)
```

## Arguments

- con:

  DBI connection.

- schema_names:

  List from src_schema_names() - uses \$raw.

- src_name:

  Source name (e.g. "mimic_iv_demo").

- icd_filter_path:

  Deprecated and ignored: membership comes from the cohort definition
  recorded at import. Accepted so existing calls keep working; a note is
  printed when it is supplied.

- icds_dict_path:

  Path to ICD description CSV/RDS with icd_code, icd_version,
  long_title. Optional for MIMIC (long_title columns are then omitted);
  required for eICU, where it resolves ICD versions.

- icd_adjudication_path:

  Optional reviewed eICU ICD-version crosswalk. Used only for eICU after
  exact dictionary matching remains ambiguous.

- classify_fn:

  Optional R function for domain-specific enrichment. Called as:
  do.call(classify_fn, c(list(diag_exp, con, raw_schema, src_name,
  group_col), classify_args)). Must return a data frame keyed by
  group_col with enrichment columns to left-join onto cohort_info. Use
  scid_classify for the SCI reference implementation. Set NULL (default)
  to write generic columns only.

- classify_args:

  Named list of extra arguments forwarded to classify_fn.

- analysis_level:

  Grain of the composite `ehr_id` key and of `cohort_index`:
  `"admission"` (default) builds `ehr_id` from patient + admission ids;
  `"icu"` additionally appends the ICU stay id. The table still carries
  all id columns either way.

- publish:

  Whether to publish `cohort_info` to the raw schema. Set to `FALSE`
  when a caller will publish several clinical tables atomically.

## Value

Invisibly returns the cohort_info data frame.

## Details

`cohort_index` is the single public membership flag. It is the cohort
definition recorded at import
(`cohort_hierarchy.definition_filter_index`), never the import scope:
importing more context (patient rather than hospital `import_scope`)
adds nonmember rows without changing membership. ICD cohorts select
hospitalizations containing a matching diagnosis in every source, so
each ICU stay of a qualifying hospitalization is a member; SQL,
direct-ID, function and Sepsis-3 cohorts keep their selected grain. With
`analysis_level = "icu"` each row carries its own ICU stay's membership;
with `"admission"` every row of an admission carries
[`any()`](https://rdrr.io/r/base/any.html) over that admission's rows.
Membership of a hospitalization does not establish that the diagnosis
was present at the start of any ICU stay.

If `cohort_hierarchy` is missing - data imported before the cohort
hierarchy existed, or a standalone raw schema - a `cohort_info` row has
no hierarchy row, or the cohort is an eICU ICD cohort built with the
superseded unit-stay selection, this function stops with an error;
re-run the cohort import
([`import_cohort_general()`](https://ate-driven-lab.github.io/rendehR/reference/import_cohort_general.md)
and
[`run_db()`](https://ate-driven-lab.github.io/rendehR/reference/run_db.md)).
There is no fallback to ICD matching or to the retired `hos_cohort_flg`.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
run_cohort_info(con, src_schema_names("mimic_iv_demo"), "mimic_iv_demo")
} # }
```

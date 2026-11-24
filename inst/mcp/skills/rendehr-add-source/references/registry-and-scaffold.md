# Registry entry and scaffold

## Fields

| Field | Meaning | Example (AmsterdamUMCdb-like) |
| --- | --- | --- |
| `name` | Family name; `<name>_demo` and `<name>_demo_<subcohort>` resolve to it | `"amsterdam"` |
| `select_id_cols` | Identifier columns, coarsest first (character vector) | `c("patientid", "admissionid")` |
| `id_cols` | `list(default = ...)` join key of the event tables; optional `icu` and `tables_with_icu` for a finer ICU key | `list(default = c("patientid", "admissionid"))` |
| `time_origin` | `visit_windows` column used as t = 0 | `"cw.admittime"` |
| `time_diff` | `"timestamp_minutes"` (timestamps) or `"offset_minutes"` (minutes since an origin) | `"timestamp_minutes"` |
| `cohort_key` | Identifier every table is subset by | `"admissionid"` |
| `cohort_seed_tables` | Tables imported first to build cohort ids | `"admissions"` |
| `cohort_filter` | Declarative selection: `list(diag_table, code_col, key, split_commas, strip_dots)` | `list(diag_table = "diagnoses", code_col = "diagnosis_code", key = "admissionid")` |
| `icu_id_type` | SQL type of the ICU id; `"text"` for string ids | `"bigint"` |

ID fields are character vectors, not named lists. The declarative filter names its key `key`. Without `cohort_filter`, cohorts of the source cannot be selected by ICD code; ID lists (`direct_ids_path`), `cohort_sql` and `import_cohort_general(cohort_fn = ...)` still work.

## Scaffold

```r
plan <- rendehR::ehr_add_source_assistant(
  "amsterdam", root = config_root,
  select_id_cols = c("patientid", "admissionid"), id_default = c("patientid", "admissionid"),
  time_origin = "cw.admittime", time_diff = "timestamp_minutes", cohort_key = "admissionid",
  diag_table = "diagnoses", code_col = "diagnosis_code",
  action = "dry_run", interactive = FALSE)
plan$result$validation    # ok / problems / warnings for the entry the wizard would write
plan$call                 # the add_source_wizard() call to run once reviewed
```

Running that call (or `action = "write"`) writes `sources/<name>.json`, `sql/<name>/db_setup_sqls/{schema/create_tables.sql, window_tables.sql, indexes.sql}` and, if absent, a header-only `rendehr_concepts_config.csv`, and registers the source when the entry is complete. An incomplete entry is written with `null` placeholders and the returned `problems` list what to fill in.

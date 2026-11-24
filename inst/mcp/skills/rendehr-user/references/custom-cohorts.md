# Create and load a custom cohort

Use this guide when adding a cohort or selecting different records from an already registered source. Do not register a new source just because the user has new IDs, ICDs, SQL, or a different scientific definition.

## Define membership and scope

Record the source/version, qualification rule, selection key, extraction scope, and intended analysis grain separately. Patient, hospital admission, and ICU stay IDs are not interchangeable. Ask before converting an ambiguous clinical phrase into codes or temporal logic.

Supply exactly one route:

| User input | Argument | Required interpretation |
| --- | --- | --- |
| ID file (CSV/CSV.GZ/Parquet/RDS) | direct_ids_path | Explicit key where ambiguous; preserve IDs as strings until package normalization |
| User-supplied ICD definition | icd_filter_path | Inspect expected CSV/RDS structure against installed help/examples and source ICD versions |
| Selection SQL or SQL file | cohort_sql | Return the declared key; optional ordered setup_sql provides dependencies |
| R function/function name | cohort_fn | Cohort-selection callback is f(paths), returning an ID vector or a data frame with key |
| Named definition | cohort | Discover with list_example_cohorts(), including source coverage |
| Whole population | no_filter = TRUE | An explicit user choice, not fallback for an empty cohort |

Do not combine competing routes. Do not pass unsupported callback/ID routes through ehr_setup_assistant's narrower cohort menu; call selection/import functions directly.

## Generate a selection call

These are alternative examples, not steps to run consecutively. Variables such as project_dir, ids_path, icd_path, selection_sql_path, and select_my_ids must be resolved from the user's inputs. No examples execute automatically.

```r
# Direct ICU identifiers with explicitly requested patient-wide context.
selection <- rendehR::select_cohort_ids(
  src_name = "mimic_iv_demo", base_dir = project_dir,
  direct_ids_path = ids_path, key = "stay_id", import_scope = "patient",
  allow_unmatched_ids = FALSE
)
```

```r
# SQL route: SELECT must return hadm_id for this example.
selection <- rendehR::select_cohort_ids(
  src_name = "mimic_iv_demo", base_dir = project_dir,
  cohort_sql = selection_sql_path, key = "hadm_id", import_scope = "hospital"
)
```

```r
# Callback route: define select_my_ids <- function(paths) { ... } separately,
# using the user's actual selection logic, returning the declared key.
selection <- rendehR::select_cohort_ids(
  src_name = "mimic_iv_demo", base_dir = project_dir,
  cohort_fn = select_my_ids, key = "subject_id", import_scope = "patient"
)
```

```r
selection <- rendehR::select_cohort_ids(
  src_name = "mimic_iv_demo", base_dir = project_dir,
  icd_filter_path = icd_path
)
```

SQL executes in the canonical selection engine over its available source relations, not arbitrarily in the user's live PostgreSQL session. Verify table names and dialect; inspect setup_sql dependencies and callback side effects. select_cohort_ids is computation, not a text-only preview: it may load/scan data, run supplied logic, and write an optional out_csv. No selection function is run for a code-only request.

## Interpret the selection

The canonical engine validates IDs against the real source hierarchy; malformed, empty, wholly unmatched, and (by default) partially unmatched requests fail. Do not set allow_unmatched_ids=TRUE simply to suppress a failure; explain the excluded fraction and obtain the user's scope decision.

Inspect the selection's aggregate fields (matched_count, unmatched_count, effective_scope) without printing selection$values; the patient_context_index, definition_filter_index and scope_index columns live in the built cohort_hierarchy table. Explain which index answers the user's question.

At this baseline ICD selection retains patient-wide context. Do not promise ICU-only ICD materialization from a scope argument. A scope request cannot discard an explicitly selected broader unit: patient selection is not narrowed to one ICU merely by changing import_scope. Verify effective scope, not just requested scope. Post-materialization subgroup filtering is a different operation.

## Materialize and continue

Use the same explicit route/key/scope with import_cohort_general for Parquet intended for PostgreSQL; use materialize_cohort_duckdb for an explicitly requested persistent DuckDB cohort. Do not pass the entire selection object as an undocumented argument. Repeated selection can recompute results: preserve the definition and input revision, or export normalized IDs deliberately and record the original qualification evidence.

```r
rendehR::import_cohort_general(
  src_name = "mimic_iv_demo", base_dir = project_dir,
  direct_ids_path = ids_path, key = "stay_id", import_scope = "patient",
  allow_unmatched_ids = FALSE, overwrite = FALSE
)
```

Use a separate configured project/output destination for a different cohort when retaining prior work is intended. Review overwrite/reuse behavior before rerunning; file existence alone does not establish compatible artifacts. A table subset reduces imported tables, not membership, and downstream steps still need their prerequisite tables.

Return the exact definition/code, requested and effective scope, aggregate patient/admission/ICU counts available from the results, paths, and unvalidated assumptions. Do not claim qualifying-event lineage or exhaustive exclusion reasons that the package did not record. Continue with [run and verify](run-and-verify.md) only within the user's execution request.

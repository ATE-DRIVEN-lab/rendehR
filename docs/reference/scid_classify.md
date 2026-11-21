# SCI-specific classify_fn for use with run_cohort_info()

Reference implementation of a classify_fn for spinal cord injury
research. Adds SCI classification (traumatic/non-traumatic, top/second
level), injury cause summary, procedure summary, and hos_eth_flg to
cohort_info.

## Usage

``` r
scid_classify(
  diag_exp,
  con,
  raw_schema,
  src_name,
  group_col,
  icds_scid_path = NULL,
  icds_cause_path = NULL,
  icds_proc_path = NULL,
  icds_proc_dict_path = NULL,
  publish = TRUE
)
```

## Arguments

- diag_exp:

  Expanded diagnostic data frame built by the generic builder.

- con:

  DBI connection.

- raw_schema:

  Raw Postgres schema name.

- src_name:

  Source name (e.g. "mimic_iv_demo").

- group_col:

  Visit grouping column (e.g. "hadm_id", "patienthealthsystemstayid").

- icds_scid_path:

  Path to the canonical SCI ICD classification (RDS or CSV). Defaults to
  the package-embedded `scid_icds_rendehr.rds`.

- icds_cause_path:

  Path to injury cause/etiology CSV. Defaults to package-embedded file.

- icds_proc_path:

  Path to SCI procedure classification CSV. Optional. When supplied,
  MIMIC sources also gain procedure columns and the
  `d_scid_operation_procedures_icds_rendehr` reference table. eICU has
  no ICD-coded procedure table, so it reports
  `proc_classification_status = "not_reported"` and typed-NA procedure
  columns regardless.

- icds_proc_dict_path:

  Path to ICD procedure description CSV. Optional.

- publish:

  Whether to publish the procedure dictionary to `raw_schema`. Set
  `FALSE` when the caller defers publication of its clinical tables, so
  a compute-only call does not write to the database.

## Value

Data frame keyed by group_col with SCI enrichment columns.

The SCI categories use the dictionary's `is_sci` (1 spinal cord, 3 cord
symptoms, 999 possible cord, 2 nerve root, 0 spinal control) and `is_tr`
(1 traumatic, 0 non-traumatic, 2 unspecified):

- `top_level`: "Spinal cord damage" if any code is `is_sci` 1, 3 or 999;
  else "Nerve roots injury and disease" if any is 2; else "Spinal
  control".

- `second_level`: for SCI stays, decided from the SCI codes only -
  "Traumatic SCI" if any is traumatic, else "Non-traumatic SCI" if any
  is non-traumatic, else "Unspecified SCI". Nerve-root stays are "Nerve
  root injury and disease". Control stays are "Spinal trauma control" if
  any code is traumatic, else "Spinal non-trauma control".

- `scid_trauma_types`: trauma statuses of the SCI codes, joined with `|`
  (e.g. `"non_traumatic|traumatic"`); `NA` for non-SCI stays.

- `scid_nontrauma_l1`, `scid_nontrauma_l2`: etiology of the SCI codes
  for non-traumatic SCI, joined with `" | "`; `"not_applicable"`
  otherwise.

## Details

Pass this as classify_fn to run_cohort_info(), with classify_args
supplying the file paths. All paths default to the package-embedded SCI
files.

## Examples

``` r
if (FALSE) { # \dontrun{
# used as the classify_fn of the SCI cohort annotations
build_cohort_annotations("mimic_iv_demo", db_name = "rendehr", classify_fn = scid_classify)
} # }
```

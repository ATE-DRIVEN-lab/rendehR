#' @importFrom stats setNames
#' @importFrom utils flush.console head
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".clinical_status", ".data", ".has_excluded", ".has_resolved",
    ".has_usable", ".icd_code", ".nontrauma_l1", ".nontrauma_l2", ".sci", ":=", "admittime", "anatomical_region", "base_rank",
    "body_system_section", "cohort_index", "comorbidity_status",
    "compact_code", "compact_versions", "concept_name", "conversion",
    "definition_filter_index", "dict", "ehr_id", "end_var", "exact_versions",
    "hadm_id",
    "hospitaldischargeyear", "icd9_code", "icd9code", "icdCodes",
    "icdCodes_split", "icd_code", "icd_code_compact",
    "icd_code_dotted", "icd_code_raw", "icd_scid_flg",
    "icd_version", "icustay_id", "index_var",
    "is_ambiguous", "is_multi_body_system", "is_only_hos",
    "is_sci", "is_tr", "iss_status", "itm_class", "long_title", "match_value",
    "n", "nontrauma_l1", "nontrauma_l2", "num_hos",
    "patient_context_index", "patienthealthsystemstayid", "patientunitstayid",
    "raw_code", "required_dedup", "review_status", "scope_index",
    "selected_version", "seq_hos", "specific_level", "stay_id", "sub_var", "subject_id",
    "table_name", "time_precision", "treatment_category", "typ", "uniquepid",
    "unit", "unit_var", "val_var", "version_resolution_status"
  ))
}

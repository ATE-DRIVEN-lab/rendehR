#' Build and optionally publish cohort information
#'
#' `cohort_info` is a hospitalization-based association table that contains generic
#' columns (hospitalization sequence, diagnostic ICD summary, composite visit
#' key, and the cohort membership flag `cohort_index`). When ICU identifiers
#' exist, admission-level values may appear on multiple ICU association rows;
#' source event tables are not duplicated by this function. Domain enrichment is
#' added by `classify_fn`.
#'
#' `cohort_index` is the single public membership flag. It is the cohort
#' definition recorded at import (`cohort_hierarchy.definition_filter_index`),
#' never the import scope: importing more context (patient rather than hospital
#' `import_scope`) adds nonmember rows without changing membership. ICD cohorts
#' select hospitalizations containing a matching diagnosis in every source, so
#' each ICU stay of a qualifying hospitalization is a member; SQL, direct-ID,
#' function and Sepsis-3 cohorts keep their selected grain. With
#' `analysis_level = "icu"` each row carries its own ICU stay's membership; with
#' `"admission"` every row of an admission carries `any()` over that
#' admission's rows. Membership of a hospitalization does not establish that
#' the diagnosis was present at the start of any ICU stay.
#'
#' If `cohort_hierarchy` is missing - data imported before the cohort hierarchy
#' existed, or a standalone raw schema - a `cohort_info` row has no hierarchy
#' row, or the cohort is an eICU ICD cohort built with the superseded unit-stay
#' selection, this function stops with an error; re-run the cohort import
#' ([import_cohort_general()] and [run_db()]). There is no fallback to ICD
#' matching or to the retired `hos_cohort_flg`.
#'
#' @param con DBI connection.
#' @param schema_names List from src_schema_names() - uses $raw.
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param icd_filter_path Deprecated and ignored: membership comes from the
#'   cohort definition recorded at import. Accepted so existing calls keep working;
#'   a note is printed when it is supplied.
#' @param icds_dict_path Path to ICD description CSV/RDS with icd_code,
#'   icd_version, long_title. Optional for MIMIC (long_title columns are then
#'   omitted); required for eICU, where it resolves ICD versions.
#' @param icd_adjudication_path Optional reviewed eICU ICD-version crosswalk.
#'   Used only for eICU after exact dictionary matching remains ambiguous.
#' @param classify_fn Optional R function for domain-specific enrichment.
#'   Called as: do.call(classify_fn, c(list(diag_exp, con, raw_schema,
#'   src_name, group_col), classify_args)). Must return a data frame keyed by
#'   group_col with enrichment columns to left-join onto cohort_info.
#'   Use scid_classify for the SCI reference implementation.
#'   Set NULL (default) to write generic columns only.
#' @param classify_args Named list of extra arguments forwarded to classify_fn.
#' @param analysis_level Grain of the composite `ehr_id` key and of
#'   `cohort_index`: `"admission"` (default) builds `ehr_id` from patient +
#'   admission ids; `"icu"` additionally appends the ICU stay id. The table
#'   still carries all id columns either way.
#' @param publish Whether to publish `cohort_info` to the raw schema. Set to
#'   `FALSE` when a caller will publish several clinical tables atomically.
#' @return Invisibly returns the cohort_info data frame.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' run_cohort_info(con, src_schema_names("mimic_iv_demo"), "mimic_iv_demo")
#' }
#' @export
run_cohort_info <- function(con,
                             schema_names,
                             src_name,
                             icd_filter_path = NULL,
                             icds_dict_path = NULL,
                             icd_adjudication_path = NULL,
                             classify_fn    = NULL,
                             classify_args  = list(),
                             analysis_level = c("admission", "icu"),
                             publish = TRUE) {
  analysis_level <- match.arg(analysis_level)
  message("Building cohort_info for ", src_name,
          " (analysis_level = ", analysis_level, ") ...")
  if (!is.null(icd_filter_path)) {
    message("run_cohort_info(): icd_filter_path is ignored; cohort membership ",
            "comes from the cohort definition recorded at import.")
  }
  .build_cohort_info_general_core(
    con                   = con,
    src_name              = src_name,
    schema_names          = schema_names,
    icds_dict_path        = icds_dict_path,
    icd_adjudication_path = icd_adjudication_path,
    classify_fn           = classify_fn,
    classify_args         = classify_args,
    analysis_level        = analysis_level,
    backend               = if (inherits(con, "duckdb_connection")) "duckdb"
                            else "postgres",
    publish               = publish
  )
}

# =============================================================================
# Internal file loaders (shared by generic and classify_fn implementations)
# =============================================================================

.load_icd_filter <- function(path, manifest_required = FALSE) {
  required <- c("icd_code")
  df <- .read_clinical_asset(path, required,
                             manifest_required = manifest_required)
  .normalize_icd_asset(df)
}

.load_icds_cause <- function(path) {
  df <- .read_clinical_asset(
    path, c("icd_code", "icd_version", "new_inj_category"),
    manifest_required = TRUE)
  .normalize_icd_asset(df)
}

.load_icds_dict <- function(path) {
  df <- .read_clinical_asset(
    path, c("icd_code", "icd_version", "long_title"),
    manifest_required = TRUE)
  .normalize_icd_asset(df)
}

# Resolve the ICD version of raw eICU diagnosis tokens. This is the single
# eICU resolver: .eicu_diag_versioned() uses it for clinical annotation and
# scoring, and cohort selection uses it for version-aware matching.
#
# Resolution is evidence-based, in order:
#   exact_dotted        the dotted token is an official diagnosis in exactly
#                       one version;
#   compact_unique      the token has no dot and its compact code exists in
#                       exactly one version;
#   expert_adjudicated  an approved row in the eICU adjudication crosswalk;
#   ambiguous           official in both versions (dotted, or compact for an
#                       undotted token);
#   unmatched           none of the above;
#   empty / malformed   no token, or not a well-formed code.
# Compact fallback is only for tokens written without a dot: a dotted token
# carries its own evidence, and stripping the dot must not turn a
# non-diagnosis into one (eICU records ICD-9 procedure 31.1, temporary
# tracheostomy, which would otherwise become diagnosis 311, depressive
# disorder). Only exact_dotted, compact_unique and expert_adjudicated tokens
# get an icd_version; all others stay explicit and unscoreable.
#
# `tokens` may repeat and contain NA; the result has one row per distinct
# normalized token (icd_code_raw), so joining it back never multiplies rows.
# The dictionary is required: a missing or unusable one is an error, never an
# empty result that would silently mark every token unmatched.
.eicu_resolve_icd_tokens <- function(tokens, icds_dict, adjudication = NULL) {
  if (is.null(icds_dict)) {
    stop("eICU ICD version resolution requires the diagnosis dictionary ",
         "(icds_dict / icds_dict_path); none was supplied.", call. = FALSE)
  }
  missing_cols <- setdiff(c("icd_code", "icd_version", "long_title"),
                          names(icds_dict))
  if (length(missing_cols)) {
    stop("eICU ICD version resolution: the diagnosis dictionary is missing ",
         "column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dict <- icds_dict %>%
    dplyr::transmute(
      icd_code_compact = .icd_compact_token(icd_code),
      icd_code_dotted = .icd_raw_token(
        if ("icd_code_dotted" %in% names(icds_dict)) icd_code_dotted
        else .icd_dotted_from_compact(icd_code, icd_version)),
      icd_version = as.character(icd_version),
      long_title = as.character(long_title)
    ) %>%
    dplyr::filter(!is.na(icd_code_compact), icd_version %in% c("9", "10")) %>%
    dplyr::distinct()
  if (!nrow(dict)) {
    stop("eICU ICD version resolution: the diagnosis dictionary has no ",
         "usable rows (a well-formed icd_code with icd_version 9 or 10).",
         call. = FALSE)
  }

  tok <- tibble::tibble(icd_code_raw = unique(.icd_raw_token(tokens))) %>%
    dplyr::mutate(icd_code_compact = .icd_compact_token(icd_code_raw))

  # exact_versions > 1 means the dotted form is official in both versions
  # (e.g. V42.7): ambiguous, not resolvable by the dot alone.
  exact <- tok %>%
    dplyr::filter(!is.na(icd_code_raw)) %>%
    dplyr::select(icd_code_raw) %>%
    dplyr::inner_join(dict, by = c("icd_code_raw" = "icd_code_dotted")) %>%
    dplyr::group_by(icd_code_raw) %>%
    dplyr::summarise(
      exact_versions = dplyr::n_distinct(icd_version),
      icd_version_exact = if (exact_versions == 1L)
        dplyr::first(icd_version) else NA_character_,
      long_title_exact = if (exact_versions == 1L)
        .collapse_sorted_values(long_title) else NA_character_,
      .groups = "drop")

  compact <- dict %>%
    dplyr::group_by(icd_code_compact) %>%
    dplyr::summarise(
      compact_versions = dplyr::n_distinct(icd_version),
      icd_version_compact = if (compact_versions == 1L)
        dplyr::first(icd_version) else NA_character_,
      long_title_compact = if (compact_versions == 1L)
        .collapse_sorted_values(long_title) else NA_character_,
      .groups = "drop"
    )

  expert <- if (is.null(adjudication)) {
    tibble::tibble(icd_code_raw = character(), icd_code_compact = character(),
                   expert_version = character())
  } else {
    adjudication %>%
      dplyr::filter(review_status == "approved") %>%
      dplyr::transmute(icd_code_raw = raw_code,
                       icd_code_compact = compact_code,
                       expert_version = selected_version)
  }

  tok %>%
    dplyr::left_join(exact, by = "icd_code_raw") %>%
    dplyr::left_join(compact, by = "icd_code_compact") %>%
    dplyr::left_join(expert,
                     by = c("icd_code_raw", "icd_code_compact")) %>%
    dplyr::mutate(
      undotted = !is.na(icd_code_raw) &
        !stringr::str_detect(icd_code_raw, stringr::fixed(".")),
      version_resolution_status = dplyr::case_when(
        is.na(icd_code_raw) ~ "empty",
        is.na(icd_code_compact) ~ "malformed",
        !is.na(icd_version_exact) ~ "exact_dotted",
        undotted & !is.na(icd_version_compact) ~ "compact_unique",
        !is.na(expert_version) ~ "expert_adjudicated",
        !is.na(exact_versions) & exact_versions > 1L ~ "ambiguous",
        undotted & !is.na(compact_versions) & compact_versions > 1L ~ "ambiguous",
        TRUE ~ "unmatched"
      ),
      icd_version = dplyr::case_when(
        version_resolution_status == "exact_dotted" ~ icd_version_exact,
        version_resolution_status == "compact_unique" ~ icd_version_compact,
        version_resolution_status == "expert_adjudicated" ~ expert_version,
        TRUE ~ NA_character_),
      long_title = dplyr::case_when(
        version_resolution_status == "exact_dotted" ~ long_title_exact,
        version_resolution_status == "compact_unique" ~ long_title_compact,
        TRUE ~ NA_character_)
    ) %>%
    dplyr::select(icd_code_raw, icd_code_compact, icd_version, long_title,
                  version_resolution_status)
}

# Parse the eICU diagnosis table without discarding the source token, and
# resolve each token's ICD version with .eicu_resolve_icd_tokens(). One output
# row per diagnosis token, in source order. A regex suggestion is diagnostic
# only and never makes a code scoreable. `icds_dict` is required.
.eicu_diag_versioned <- function(con, raw_schema, icds_dict = NULL,
                                 adjudication = NULL,
                                 include_regex_diagnostic = TRUE) {

  diag <- get_table_from_db(con, raw_schema, "diagnosis")
  pati_ids <- get_table_from_db(con, raw_schema, "patient") %>%
    dplyr::select(patientunitstayid, patienthealthsystemstayid) %>%
    dplyr::distinct()

  diag_exp <- diag %>%
    dplyr::left_join(pati_ids, by = "patientunitstayid") %>%
    dplyr::mutate(icdCodes = dplyr::na_if(as.character(icd9code), "")) %>%
    dplyr::mutate(icdCodes_split = stringr::str_split(icdCodes, "\\s*,\\s*")) %>%
    tidyr::unnest(icdCodes_split) %>%
    dplyr::mutate(
      icd_code_raw = .icd_raw_token(icdCodes_split),
      icd_code_compact = .icd_compact_token(icd_code_raw)
    ) %>%
    dplyr::select(patienthealthsystemstayid, patientunitstayid,
                  icd_code_raw, icd_code_compact)

  resolved <- .eicu_resolve_icd_tokens(diag_exp$icd_code_raw, icds_dict,
                                       adjudication) %>%
    dplyr::select(icd_code_raw, icd_version, long_title,
                  version_resolution_status)

  # `resolved` has one row per distinct icd_code_raw, so this join keeps the
  # diagnosis rows exactly as they are.
  out <- diag_exp %>%
    dplyr::left_join(resolved, by = "icd_code_raw") %>%
    dplyr::mutate(icd_code = icd_code_compact) %>%
    dplyr::select(patienthealthsystemstayid, patientunitstayid,
                  icd_code_raw, icd_code_compact, icd_code, icd_version,
                  version_resolution_status, long_title)

  if (isTRUE(include_regex_diagnostic)) {
    out <- out %>% dplyr::mutate(
      regex_version_suggestion = dplyr::case_when(
        is.na(icd_code_compact) ~ NA_character_,
        stringr::str_detect(icd_code_compact, "^[A-DF-UW-Z]") ~ "10",
        TRUE ~ "9"
      ))
  }
  out
}

# Load the SCID procedure-classification lookup.
#
# The packaged asset is a code-pattern expansion, not a per-code table: its
# manifest declares the key (icd_code, icd_version, treatment_category,
# body_system_section) and `key_unique: false`. The two ICD sides behave
# differently, verified against the shipped file:
#
#   ICD-10  4,667 codes, each with exactly one treatment_category and one
#           body_system_section. Already 1:1; the duplicate rows are exact
#           repeats produced by the pattern expansion.
#   ICD-9     304 codes from 5,760 rows, because each ICD-9 code is repeated
#           once per ICD-10-PCS code it back-maps to. 47 codes therefore carry
#           several body_system_section values and 4 carry contradictory
#           treatment_category values.
#
# `collapse = TRUE` reduces the file to one row per (icd_code, icd_version) so
# joins cannot fan out (measured 4.6x on mimic_iii without it). Multi-valued
# body systems are preserved, pipe-separated, because an ICD-9 code that
# back-maps across PCS sections genuinely is ambiguous about site. Contradictory
# treatment_category values are recorded as NA with is_ambiguous = TRUE rather
# than resolved by sort order: this is a clinical call the asset does not make,
# and the summary treats such codes as not-Operative instead of guessing.
.load_icds_proc <- function(path, collapse = TRUE) {
  manifest <- .load_clinical_asset_manifest()
  entry <- .clinical_manifest_entry(
    manifest, path, asset_id = "procedure_classification")
  df <- .read_clinical_asset(
    path, c("icd_code", "icd_version", "treatment_category",
            "body_system_section"),
    asset_id = "procedure_classification", manifest_required = TRUE)

  missing_code <- .clinical_missing_value(df$icd_code)
  missing_version <- .clinical_missing_value(df$icd_version)
  if (is.null(entry) && any(missing_code | missing_version)) {
    stop("Procedure-classification overrides require nonmissing `icd_code` ",
         "and `icd_version` values.", call. = FALSE)
  }
  if (!is.null(entry) && any(missing_code) &&
      is.null(entry$known_missing_counts$icd_code)) {
    stop("The packaged procedure-classification asset may contain missing ",
         "`icd_code` values only when their exact count is declared in the ",
         "manifest.", call. = FALSE)
  }

  if (!is.null(entry)) df <- df[!missing_code, , drop = FALSE]
  df <- .normalize_icd_asset(df)
  if (any(.clinical_missing_value(df$icd_code) |
          .clinical_missing_value(df$icd_version))) {
    stop("The runtime procedure-classification lookup contains a missing or ",
         "invalid ICD code/version key.", call. = FALSE)
  }
  df <- tibble::as_tibble(df)
  if (isTRUE(collapse)) df <- .collapse_icds_proc(df)
  df
}

# Reduce the procedure lookup to one row per (icd_code, icd_version). See
# .load_icds_proc() for why the packaged asset is not already keyed that way.
.collapse_icds_proc <- function(df) {
  df %>%
    dplyr::group_by(icd_code, icd_version) %>%
    dplyr::summarise(
      # Contradictory Operative/Nonoperative values cannot be reconciled from
      # the asset, so the code is marked ambiguous and left unclassified.
      is_ambiguous = dplyr::n_distinct(treatment_category, na.rm = TRUE) > 1L,
      treatment_category = if (dplyr::n_distinct(treatment_category, na.rm = TRUE) > 1L)
        NA_character_ else .paste_summ(treatment_category),
      body_system_section = .paste_summ(body_system_section),
      is_multi_body_system = stringr::str_detect(
        dplyr::coalesce(body_system_section, ""), stringr::fixed(" | ")),
      .groups = "drop"
    )
}

# =============================================================================
# Internal summary helpers (used by scid_classify and any other classify_fn)
# =============================================================================

.summ_categories <- function(x) {
  u <- unique(x[!is.na(x)])
  if (length(u) == 0) return(character(0))
  # Placeholder values in both label styles (legacy title case, snake_case).
  placeholders <- c("Not applicable", "Unspecified", "not_applicable", "unspecified")
  if (length(u) > 1) sort(u[!u %in% placeholders]) else sort(u)
}

.paste_summ <- function(x) {
  v <- .summ_categories(x)
  if (length(v) == 0) NA_character_ else paste(v, collapse = " | ")
}

# Category labels for .sci_cat_summ(); change names here only.
.sci_cat_labels <- list(
  top_sci            = "Spinal cord damage",
  top_nerve_root     = "Nerve roots injury and disease",
  top_control        = "Spinal control",
  sci_traumatic      = "Traumatic SCI",
  sci_non_traumatic  = "Non-traumatic SCI",
  sci_unspecified    = "Unspecified SCI",
  nerve_root         = "Nerve root injury and disease",
  control_trauma     = "Spinal trauma control",
  control_non_trauma = "Spinal non-trauma control"
)

# SCI-specific: build classification summary per group_col.
#
# top_level:    any SCI code (is_sci 1, 3 or 999) -> SCI; else any nerve-root
#               code (is_sci 2) -> nerve root; else control (is_sci 0).
# second_level: SCI stays are classified from their SCI codes only - any
#               traumatic -> traumatic; else any non-traumatic -> non-traumatic;
#               else (only unspecified trauma) -> unspecified. Controls: any
#               traumatic code -> trauma control, else non-trauma control.
# scid_trauma_types lists the trauma statuses of the SCI codes; the nontrauma
# etiology columns also come from the SCI codes only.
.sci_cat_summ <- function(diag_exp, group_col) {
  has_long_title <- "long_title"        %in% names(diag_exp)
  has_region     <- "anatomical_region" %in% names(diag_exp)
  has_level      <- "specific_level"    %in% names(diag_exp)
  lab <- .sci_cat_labels
  tr_lab <- c("0" = "non_traumatic", "1" = "traumatic", "2" = "unspecified")

  agg <- diag_exp %>%
    dplyr::filter(!is.na(.data[[group_col]]) & icd_scid_flg == 1) %>%
    dplyr::mutate(.sci = is_sci %in% c(1, 3, 999)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) %>%
    dplyr::summarise(
      .has_sci      = any(.sci),
      .has_nr       = any(is_sci == 2, na.rm = TRUE),
      .sci_tr_1     = any(.sci & is_tr == 1, na.rm = TRUE),
      .sci_tr_0     = any(.sci & is_tr == 0, na.rm = TRUE),
      .ctl_tr_1     = any(is_sci == 0 & is_tr == 1, na.rm = TRUE),
      scid_trauma_types = {
        v <- sort(unique(tr_lab[as.character(is_tr[.sci])]))
        if (length(v)) paste(v, collapse = "|") else NA_character_
      },
      .nontrauma_l1 = .paste_summ(nontrauma_l1[.sci]),
      .nontrauma_l2 = .paste_summ(nontrauma_l2[.sci]),
      # Coded level and region from the diagnosis titles; not a neurological level.
      scid_injury_location      = if (has_level) .paste_summ(specific_level) else NA_character_,
      scid_injury_level         = if (has_region) .paste_summ(anatomical_region) else NA_character_,
      scid_icd_codes            = .collapse_sorted_values(icd_code),
      scid_icd_codes_long_title = if (has_long_title)
        .collapse_sorted_values(long_title)
      else NA_character_,
      num_scid_icd_codes = dplyr::n_distinct(icd_code, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      top_level = dplyr::case_when(
        .has_sci ~ lab$top_sci,
        .has_nr  ~ lab$top_nerve_root,
        TRUE     ~ lab$top_control
      ),
      second_level = dplyr::case_when(
        .has_sci & .sci_tr_1 ~ lab$sci_traumatic,
        .has_sci & .sci_tr_0 ~ lab$sci_non_traumatic,
        .has_sci             ~ lab$sci_unspecified,
        .has_nr              ~ lab$nerve_root,
        .ctl_tr_1            ~ lab$control_trauma,
        TRUE                 ~ lab$control_non_trauma
      ),
      # Same snake_case style as the SCI dictionary's nontrauma values.
      scid_nontrauma_l1 = dplyr::if_else(
        second_level == lab$sci_non_traumatic, .nontrauma_l1, "not_applicable"),
      scid_nontrauma_l2 = dplyr::if_else(
        second_level == lab$sci_non_traumatic, .nontrauma_l2, "not_applicable")
    ) %>%
    dplyr::select(-dplyr::starts_with("."))

  agg
}

# SCI-specific: build injury cause summary per group_col
.cause_summ <- function(diag_exp, group_col) {
  diag_exp %>%
    dplyr::filter(!is.na(.data[[group_col]]) & icd_cause_flg == 1) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) %>%
    dplyr::summarise(
      cause_icd_codes = .collapse_sorted_values(icd_code[icd_cause_flg == 1]),
      cause_icd_codes_long_title = if ("long_title" %in% names(dplyr::pick(dplyr::everything())))
        .collapse_sorted_values(long_title[icd_cause_flg == 1])
      else NA_character_,
      inj_cats = .collapse_sorted_values(new_inj_category[icd_cause_flg == 1]),
      num_cause_icd_codes = dplyr::n_distinct(icd_code[icd_cause_flg == 1], na.rm = TRUE),
      .groups = "drop"
    )
}

# SCI-specific: build procedure summary per group_col.
#
# `proc_exp` is the source's full procedure table left-joined to the SCID
# procedure dictionary, so most rows are unmatched: the dictionary is a curated
# SCID-relevant list, not a general procedure vocabulary (measured coverage
# 23.2% of mimic_iii procedure rows, 13.1%/8.4% of mimic_iv ICD-9/ICD-10). That
# is by design, and it fixes what the columns mean:
#
#   proc_icd_codes / num_proc_icd_codes        every procedure the admission
#                                              had, classified or not
#   num_proc_icd_codes_classified              how many were SCID-relevant, so
#                                              the two counts reconcile
#   proc_treatment_categories / _body_systems  describe the classified subset
#                                              only
#   has_operative_proc                         1 = had a SCID-relevant
#                                              operative procedure; 0 = had
#                                              procedure records, none such.
#                                              Admissions with no procedure
#                                              records never reach this
#                                              function and get NA from the
#                                              caller's left join, which is the
#                                              wanted "no data" third state.
#
# Codes the dictionary classifies contradictorily carry treatment_category NA
# and is_ambiguous TRUE (see .collapse_icds_proc), so they cannot count as
# Operative here; proc_classification_status reports that rather than hiding it.
.proc_summ <- function(proc_exp, group_col) {
  has_long_title <- "long_title"          %in% names(proc_exp)
  has_treat_cat  <- "treatment_category"  %in% names(proc_exp)
  has_body_sys   <- "body_system_section" %in% names(proc_exp)
  has_ambig      <- "is_ambiguous"        %in% names(proc_exp)
  has_multi_bs   <- "is_multi_body_system" %in% names(proc_exp)

  proc_exp %>%
    dplyr::filter(!is.na(.data[[group_col]]), !is.na(icd_code), icd_code != "") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) %>%
    dplyr::summarise(
      proc_icd_codes            = .collapse_sorted_values(icd_code),
      proc_icd_long_titles      = if (has_long_title)
        .collapse_sorted_values(long_title)
      else NA_character_,
      proc_treatment_categories = if (has_treat_cat)
        .collapse_sorted_values(treatment_category)
      else NA_character_,
      proc_body_systems         = if (has_body_sys)
        .collapse_sorted_values(body_system_section)
      else NA_character_,
      num_proc_icd_codes        = dplyr::n_distinct(icd_code, na.rm = TRUE),
      # A row matched the dictionary iff it carries a classification outcome.
      # is_ambiguous is the reliable marker: it is non-NA for every matched
      # code, including the ambiguous ones whose treatment_category is NA.
      num_proc_icd_codes_classified = if (has_ambig)
        dplyr::n_distinct(icd_code[!is.na(is_ambiguous)], na.rm = TRUE)
      else if (has_treat_cat)
        dplyr::n_distinct(icd_code[!is.na(treatment_category)], na.rm = TRUE)
      else NA_integer_,
      has_operative_proc        = as.integer(
        has_treat_cat && any(treatment_category == "Operative", na.rm = TRUE)
      ),
      .has_ambiguous            = if (has_ambig)
        any(is_ambiguous, na.rm = TRUE) else FALSE,
      proc_body_systems_ambiguous = if (has_multi_bs)
        as.integer(any(is_multi_body_system, na.rm = TRUE)) else NA_integer_,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      proc_classification_status = dplyr::case_when(
        is.na(num_proc_icd_codes_classified)   ~ NA_character_,
        num_proc_icd_codes_classified == 0L    ~ "unclassified",
        .has_ambiguous                          ~ "ambiguous",
        TRUE                                    ~ "classified"
      )
    ) %>%
    dplyr::select(-dplyr::starts_with("."))
}

# Column set produced by .proc_summ(), excluding the grouping key. Every source
# family must emit all of these, because merge_sources() keeps only the columns
# common to all sources - a family-specific set would silently drop the whole
# procedure block from the merged cohort_info.
.proc_summ_columns <- function() {
  c("proc_icd_codes", "proc_icd_long_titles", "proc_treatment_categories",
    "proc_body_systems", "num_proc_icd_codes", "num_proc_icd_codes_classified",
    "has_operative_proc", "proc_body_systems_ambiguous",
    "proc_classification_status")
}

# Read a source's procedures_icd table and summarise it against the SCID
# procedure dictionary. Returns NULL when the source has no such table, so the
# caller can emit typed-NA columns instead of failing the whole cohort-info
# stage - run_one_source() accepts a bounded `tables` subset, and the ISS flat
# concepts already degrade the same way via their to_regclass() guard.
#
# The existence check runs only against a live connection; with con = NULL the
# read is expected to be supplied by a mocked get_table_from_db() in tests.
#
# `prepare` normalises the source's native procedure columns onto
# (icd_code, icd_version). No version filtering is needed: the join key carries
# the version, so an ICD-9-only source matches only ICD-9 dictionary rows.
.build_proc_summ <- function(con, raw_schema, group_col, icds_proc,
                             icds_proc_dict = NULL, prepare = identity) {
  if (!is.null(con) && DBI::dbIsValid(con) &&
      !DBI::dbExistsTable(con, DBI::Id(schema = raw_schema,
                                       table = "procedures_icd"))) {
    message("  [rendehR] ", raw_schema, ".procedures_icd not found; ",
            "procedure classification reported as not_reported.")
    return(NULL)
  }

  proc_exp <- prepare(get_table_from_db(con, raw_schema, "procedures_icd")) %>%
    dplyr::left_join(
      dplyr::select(icds_proc, dplyr::any_of(c(
        "icd_code", "icd_version", "treatment_category",
        "body_system_section", "is_ambiguous", "is_multi_body_system"))),
      by = c("icd_code", "icd_version")
    )
  if (!is.null(icds_proc_dict))
    proc_exp <- dplyr::left_join(
      proc_exp,
      dplyr::select(icds_proc_dict, icd_code, icd_version, long_title),
      by = c("icd_code", "icd_version"))
  .proc_summ(proc_exp, group_col)
}

# Add typed-NA procedure columns, with `status` recording why they are empty.
# Used for sources with no ICD-coded procedure table (eICU reports
# "not_reported") and when a source's procedures_icd table is absent.
.add_na_proc_cols <- function(result, status = NA_character_) {
  typed <- list(
    proc_icd_codes                = NA_character_,
    proc_icd_long_titles          = NA_character_,
    proc_treatment_categories     = NA_character_,
    proc_body_systems             = NA_character_,
    num_proc_icd_codes            = NA_integer_,
    num_proc_icd_codes_classified = NA_integer_,
    has_operative_proc            = NA_integer_,
    proc_body_systems_ambiguous   = NA_integer_,
    proc_classification_status    = as.character(status)
  )
  for (column in names(typed)) result[[column]] <- typed[[column]]
  result
}

# =============================================================================
# Generic source-specific builders
# =============================================================================

.build_cohort_info_mimic_iv <- function(con, raw_schema, src_name,
                                         icds_dict,
                                         classify_fn, classify_args,
                                         analysis_level = "admission") {
  diag <- get_table_from_db(con, raw_schema, "diagnoses_icd") %>%
    dplyr::mutate(icd_code    = .icd_compact_token(icd_code),
                  icd_version = as.character(icd_version))
  admi <- get_table_from_db(con, raw_schema, "admissions")
  icus <- get_table_from_db(con, raw_schema, "icustays") %>%
    dplyr::select(dplyr::any_of(c("subject_id", "hadm_id", "stay_id"))) %>%
    dplyr::distinct()

  # ---- Hospitalization sequence ----
  hos_summ <- admi %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(num_hos = dplyr::n_distinct(hadm_id), .groups = "drop") %>%
    dplyr::mutate(is_only_hos = as.integer(num_hos == 1))

  admi_info <- admi %>%
    dplyr::group_by(subject_id) %>%
    dplyr::arrange(admittime) %>%
    dplyr::mutate(seq_hos = dplyr::row_number()) %>%
    dplyr::left_join(hos_summ, by = "subject_id") %>%
    dplyr::ungroup() %>%
    dplyr::distinct(subject_id, hadm_id, is_only_hos, num_hos, seq_hos)

  diag_exp <- diag

  if (!is.null(icds_dict))
    diag_exp <- dplyr::left_join(diag_exp,
      dplyr::select(icds_dict, icd_code, icd_version, long_title),
      by = c("icd_code", "icd_version"))

  # ---- Diagnostic summary ----
  has_long_title <- "long_title" %in% names(diag_exp)
  diag_summ <- diag_exp %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::summarise(
      diag_icd_codes            = .collapse_sorted_values(icd_code),
      diag_icd_codes_long_title = if (has_long_title)
        .collapse_sorted_values(long_title)
      else NA_character_,
      num_diag_icd_codes        = dplyr::n_distinct(icd_code, na.rm = TRUE),
      .groups = "drop"
    )


  # ---- Assemble generic result ----
  result <- admi_info %>%
    dplyr::left_join(diag_summ,  by = "hadm_id") %>%
    dplyr::left_join(icus, by = c("subject_id", "hadm_id")) %>%
    dplyr::distinct() %>%
    tidyr::unite("ehr_id",
                 dplyr::all_of(.ehr_id_grain_parts(
                   c("subject_id", "hadm_id", "stay_id"), analysis_level)),
                 na.rm = TRUE, remove = FALSE) %>%
    dplyr::select(ehr_id, subject_id, hadm_id, stay_id, dplyr::everything())

  # ---- Domain-specific enrichment ----
  if (!is.null(classify_fn)) {
    enrichment <- do.call(classify_fn,
                          c(list(diag_exp   = diag_exp,
                                 con        = con,
                                 raw_schema = raw_schema,
                                 src_name   = src_name,
                                 group_col  = "hadm_id"),
                            classify_args))
    result <- dplyr::left_join(result, enrichment, by = "hadm_id")
  }

  result
}

.build_cohort_info_mimic_iii <- function(con, raw_schema, src_name,
                                          icds_dict,
                                          classify_fn, classify_args,
                                          analysis_level = "admission") {
  diag <- get_table_from_db(con, raw_schema, "diagnoses_icd") %>%
    dplyr::rename(icd_code = icd9_code) %>%
    dplyr::mutate(icd_code    = .icd_compact_token(icd_code),
                  icd_version = "9")
  admi <- get_table_from_db(con, raw_schema, "admissions")
  icus <- get_table_from_db(con, raw_schema, "icustays") %>%
    dplyr::select(dplyr::any_of(c("subject_id", "hadm_id", "icustay_id"))) %>%
    dplyr::distinct()

  # ---- Hospitalization sequence ----
  hos_summ <- admi %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(num_hos = dplyr::n_distinct(hadm_id), .groups = "drop") %>%
    dplyr::mutate(is_only_hos = as.integer(num_hos == 1))

  admi_info <- admi %>%
    dplyr::group_by(subject_id) %>%
    dplyr::arrange(admittime) %>%
    dplyr::mutate(seq_hos = dplyr::row_number()) %>%
    dplyr::left_join(hos_summ, by = "subject_id") %>%
    dplyr::ungroup() %>%
    dplyr::distinct(subject_id, hadm_id, is_only_hos, num_hos, seq_hos)

  diag_exp <- diag

  if (!is.null(icds_dict))
    diag_exp <- dplyr::left_join(diag_exp,
      dplyr::select(dplyr::filter(icds_dict, icd_version == "9"),
                    icd_code, long_title),
      by = "icd_code")

  # ---- Diagnostic summary ----
  has_long_title <- "long_title" %in% names(diag_exp)
  diag_summ <- diag_exp %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::summarise(
      diag_icd_codes            = .collapse_sorted_values(icd_code),
      diag_icd_codes_long_title = if (has_long_title)
        .collapse_sorted_values(long_title)
      else NA_character_,
      num_diag_icd_codes        = dplyr::n_distinct(icd_code, na.rm = TRUE),
      .groups = "drop"
    )


  # ---- Assemble generic result ----
  result <- admi_info %>%
    dplyr::left_join(diag_summ,  by = "hadm_id") %>%
    dplyr::left_join(icus, by = c("subject_id", "hadm_id")) %>%
    dplyr::distinct() %>%
    tidyr::unite("ehr_id",
                 dplyr::all_of(.ehr_id_grain_parts(
                   c("subject_id", "hadm_id", "icustay_id"), analysis_level)),
                 na.rm = TRUE, remove = FALSE) %>%
    dplyr::select(ehr_id, subject_id, hadm_id, icustay_id, dplyr::everything())

  # ---- Domain-specific enrichment ----
  if (!is.null(classify_fn)) {
    enrichment <- do.call(classify_fn,
                          c(list(diag_exp   = diag_exp,
                                 con        = con,
                                 raw_schema = raw_schema,
                                 src_name   = src_name,
                                 group_col  = "hadm_id"),
                            classify_args))
    result <- dplyr::left_join(result, enrichment, by = "hadm_id")
  }

  result
}

.build_cohort_info_eicu <- function(con, raw_schema, src_name,
                                     icds_dict, adjudication,
                                     classify_fn, classify_args,
                                     analysis_level = "admission") {
  pati <- get_table_from_db(con, raw_schema, "patient")

  # ---- Hospitalization sequence ----
  hos_summ <- pati %>%
    dplyr::group_by(uniquepid) %>%
    dplyr::summarise(
      num_hos = dplyr::n_distinct(patienthealthsystemstayid),
      .groups = "drop"
    ) %>%
    dplyr::mutate(is_only_hos = as.integer(num_hos == 1))

  pati_dt <- data.table::as.data.table(dplyr::left_join(pati, hos_summ, by = "uniquepid"))
  pati_dt[, base_rank := data.table::frank(hospitaldischargeyear, ties.method = "dense"),
          by = uniquepid]
  pati_dt[, seq_hos := base_rank]
  pati_dt[, base_rank := NULL]
  pati <- dplyr::as_tibble(pati_dt)

  admi_info <- pati %>%
    dplyr::distinct(uniquepid, patienthealthsystemstayid, patientunitstayid,
                    is_only_hos, num_hos, seq_hos)

  diag_exp <- .eicu_diag_versioned(
    con, raw_schema, icds_dict, adjudication,
    include_regex_diagnostic = TRUE)


  # ---- Diagnostic summary ----
  has_long_title <- "long_title" %in% names(diag_exp)
  diag_summ <- diag_exp %>%
    dplyr::group_by(patienthealthsystemstayid) %>%
    dplyr::summarise(
      diag_icd_codes            = .collapse_sorted_values(icd_code_raw),
      diag_icd_codes_long_title = if (has_long_title)
        .collapse_sorted_values(long_title)
      else NA_character_,
      num_diag_icd_codes        = dplyr::n_distinct(icd_code, na.rm = TRUE),
      .groups = "drop"
    )


  # ---- Assemble generic result ----
  result <- admi_info %>%
    dplyr::left_join(diag_summ,  by = "patienthealthsystemstayid") %>%
    dplyr::distinct() %>%
    tidyr::unite("ehr_id",
                 dplyr::all_of(.ehr_id_grain_parts(
                   c("uniquepid", "patienthealthsystemstayid", "patientunitstayid"),
                   analysis_level)),
                 na.rm = TRUE, remove = FALSE) %>%
    dplyr::select(ehr_id, uniquepid, patienthealthsystemstayid,
                  patientunitstayid, dplyr::everything())

  # ---- Domain-specific enrichment ----
  if (!is.null(classify_fn)) {
    enrichment <- do.call(classify_fn,
                          c(list(diag_exp   = diag_exp,
                                 con        = con,
                                 raw_schema = raw_schema,
                                 src_name   = src_name,
                                 group_col  = "patienthealthsystemstayid"),
                            classify_args))
    result <- dplyr::left_join(result, enrichment, by = "patienthealthsystemstayid")
  }

  result
}

# =============================================================================
# SCI reference implementation of classify_fn
# =============================================================================

#' SCI-specific classify_fn for use with run_cohort_info()
#'
#' Reference implementation of a classify_fn for spinal cord injury research.
#' Adds SCI classification (traumatic/non-traumatic, top/second level), injury
#' cause summary, procedure summary, and hos_eth_flg to cohort_info.
#'
#' Pass this as classify_fn to run_cohort_info(), with classify_args supplying
#' the file paths. All paths default to the package-embedded SCI files.
#'
#' @param diag_exp Expanded diagnostic data frame built by the generic builder.
#' @param con DBI connection.
#' @param raw_schema Raw Postgres schema name.
#' @param src_name Source name (e.g. "mimic_iv_demo").
#' @param group_col Visit grouping column (e.g. "hadm_id", "patienthealthsystemstayid").
#' @param icds_scid_path Path to the canonical SCI ICD classification (RDS or CSV).
#'   Defaults to the package-embedded `scid_icds_rendehr.rds`.
#' @param icds_cause_path Path to injury cause/etiology CSV. Defaults to
#'   package-embedded file.
#' @param icds_proc_path Path to SCI procedure classification CSV. Optional.
#'   When supplied, MIMIC sources also gain procedure columns and the
#'   `d_scid_operation_procedures_icds_rendehr` reference table. eICU has no
#'   ICD-coded procedure table, so it reports `proc_classification_status =
#'   "not_reported"` and typed-NA procedure columns regardless.
#' @param icds_proc_dict_path Path to ICD procedure description CSV. Optional.
#' @param publish Whether to publish the procedure dictionary to `raw_schema`.
#'   Set `FALSE` when the caller defers publication of its clinical tables, so
#'   a compute-only call does not write to the database.
#' @return Data frame keyed by group_col with SCI enrichment columns.
#'
#'   The SCI categories use the dictionary's `is_sci` (1 spinal cord, 3 cord
#'   symptoms, 999 possible cord, 2 nerve root, 0 spinal control) and `is_tr`
#'   (1 traumatic, 0 non-traumatic, 2 unspecified):
#'   - `top_level`: "Spinal cord damage" if any code is `is_sci` 1, 3 or 999;
#'     else "Nerve roots injury and disease" if any is 2; else "Spinal control".
#'   - `second_level`: for SCI stays, decided from the SCI codes only -
#'     "Traumatic SCI" if any is traumatic, else "Non-traumatic SCI" if any is
#'     non-traumatic, else "Unspecified SCI". Nerve-root stays are "Nerve root
#'     injury and disease". Control stays are "Spinal trauma control" if any
#'     code is traumatic, else "Spinal non-trauma control".
#'   - `scid_trauma_types`: trauma statuses of the SCI codes, joined with `|`
#'     (e.g. `"non_traumatic|traumatic"`); `NA` for non-SCI stays.
#'   - `scid_nontrauma_l1`, `scid_nontrauma_l2`: etiology of the SCI codes for
#'     non-traumatic SCI, joined with `" | "`; `"not_applicable"` otherwise.
#' @examples
#' \dontrun{
#' # used as the classify_fn of the SCI cohort annotations
#' build_cohort_annotations("mimic_iv_demo", db_name = "rendehr", classify_fn = scid_classify)
#' }
#' @export
scid_classify <- function(diag_exp, con, raw_schema, src_name, group_col,
                         icds_scid_path      = NULL,
                         icds_cause_path     = NULL,
                         icds_proc_path      = NULL,
                         icds_proc_dict_path = NULL,
                         publish             = TRUE) {
  fam <- src_family(src_name)

  icds_scid <- .load_icd_filter(
    icds_scid_path %||% ehr_config_path("scid_icds_rendehr.rds"),
    manifest_required = TRUE
  )
  icds_cause <- .load_icds_cause(
    icds_cause_path %||% ehr_config_path("injury_cause_icds_rendehr.csv")
  )
  icds_proc      <- if (!is.null(icds_proc_path))      .load_icds_proc(icds_proc_path)      else NULL
  icds_proc_dict <- if (!is.null(icds_proc_dict_path)) .load_icds_dict(icds_proc_dict_path) else NULL

  # Publish the procedure dictionary as reference data alongside the cohort.
  # Only the ICD versions the source can actually use are published, so the
  # per-schema table matches what was applied: MIMIC-III procedures are ICD-9
  # only, MIMIC-IV carries both.
  if (isTRUE(publish) && !is.null(icds_proc) &&
      fam %in% c("mimic_iv", "mimic_iii")) {
    dict <- if (identical(fam, "mimic_iii"))
      dplyr::filter(icds_proc, icd_version == "9") else icds_proc
    .clinical_publish_table(
      con, raw_schema, "d_scid_operation_procedures_icds_rendehr", dict,
      required_columns   = c("icd_code", "icd_version"),
      nonmissing_columns = c("icd_code", "icd_version"),
      unique_columns     = c("icd_code", "icd_version"),
      indexes = list(list(
        name = "idx_d_scid_operation_procedures_icds_rendehr_code",
        columns = c("icd_code", "icd_version")))
    )
    message(
      "Built ", raw_schema, ".d_scid_operation_procedures_icds_rendehr: ",
      nrow(dict), " ICD procedure code(s) as declared reference data"
    )
  }

  switch(fam,
    mimic_iv  = .sci_enrich_mimic_iv(diag_exp, con, raw_schema, group_col,
                                      icds_scid, icds_cause, icds_proc, icds_proc_dict),
    mimic_iii = .sci_enrich_mimic_iii(diag_exp, con, raw_schema, group_col,
                                       icds_scid, icds_cause, icds_proc, icds_proc_dict),
    eicu      = .sci_enrich_eicu(diag_exp, group_col,
                                  icds_scid, icds_cause),
    stop("scid_classify: unsupported source family: ", fam)
  )
}

# ---------------------------------------------------------------------------
# SCI enrichment per source
# ---------------------------------------------------------------------------

.sci_enrich_mimic_iv <- function(diag_exp, con, raw_schema, group_col,
                                   icds_scid, icds_cause,
                                   icds_proc = NULL, icds_proc_dict = NULL) {
  # Join SCI classification columns onto diag_exp
  scid_icds <- icds_scid %>%
    dplyr::select(icd_code, icd_version) %>%
    dplyr::distinct() %>%
    dplyr::mutate(icd_scid_flg = 1L)

  diag_exp <- diag_exp %>%
    dplyr::left_join(scid_icds, by = c("icd_code", "icd_version")) %>%
    dplyr::mutate(icd_scid_flg = tidyr::replace_na(icd_scid_flg, 0L)) %>%
    dplyr::left_join(
      dplyr::select(icds_scid, dplyr::any_of(c(
        "icd_code", "icd_version",
        "is_sci", "is_tr", "anatomical_region", "specific_level",
        "nontrauma_l1", "nontrauma_l2"))),
      by = c("icd_code", "icd_version")
    )

  sci_summ <- .sci_cat_summ(diag_exp, group_col)

  # Cause join
  cause_icds <- icds_cause %>%
    dplyr::select(icd_code, icd_version) %>%
    dplyr::distinct() %>%
    dplyr::mutate(icd_cause_flg = 1L)

  diag_exp <- diag_exp %>%
    dplyr::left_join(cause_icds, by = c("icd_code", "icd_version")) %>%
    dplyr::mutate(icd_cause_flg = tidyr::replace_na(icd_cause_flg, 0L)) %>%
    dplyr::left_join(
      dplyr::select(icds_cause, icd_code, icd_version, new_inj_category),
      by = c("icd_code", "icd_version")
    )

  cause_summ <- .cause_summ(diag_exp, group_col)

  cause_hos <- diag_exp %>%
    dplyr::filter(icd_cause_flg == 1) %>%
    dplyr::distinct(.data[[group_col]]) %>%
    dplyr::mutate(hos_eth_flg = 1L)

  # Procedure summary
  proc_summ <- NULL
  if (!is.null(icds_proc)) {
    proc_summ <- .build_proc_summ(
      con, raw_schema, group_col, icds_proc, icds_proc_dict,
      prepare = function(df) dplyr::mutate(df,
        icd_code    = as.character(icd_code),
        icd_version = as.character(icd_version)))
  }

  result <- sci_summ %>%
    dplyr::left_join(cause_summ, by = group_col) %>%
    dplyr::left_join(cause_hos,  by = group_col) %>%
    dplyr::mutate(hos_eth_flg = tidyr::replace_na(hos_eth_flg, 0L))

  result <- if (is.null(proc_summ)) .add_na_proc_cols(result, "not_reported")
            else dplyr::left_join(result, proc_summ, by = group_col)

  result
}

.sci_enrich_mimic_iii <- function(diag_exp, con, raw_schema, group_col,
                                    icds_scid, icds_cause,
                                    icds_proc = NULL, icds_proc_dict = NULL) {
  # MIMIC-III diagnoses are ICD-9 only and carry no version column, so the
  # join is on icd_code alone - which means the lists must be restricted to
  # their ICD-9 rows first. Many ICD-10 transport-accident codes (V01-V99) are
  # character-identical to ICD-9 V-codes (supplementary factors such as V103
  # history of breast cancer), and would otherwise be flagged as causes.
  icds_scid  <- dplyr::filter(icds_scid,  as.character(icd_version) == "9")
  icds_cause <- dplyr::filter(icds_cause, as.character(icd_version) == "9")

  scid_icds <- icds_scid %>%
    dplyr::select(icd_code) %>%
    dplyr::distinct() %>%
    dplyr::mutate(icd_scid_flg = 1L)

  diag_exp <- diag_exp %>%
    dplyr::left_join(scid_icds, by = "icd_code") %>%
    dplyr::mutate(icd_scid_flg = tidyr::replace_na(icd_scid_flg, 0L)) %>%
    dplyr::left_join(
      dplyr::select(icds_scid, dplyr::any_of(c(
        "icd_code",
        "is_sci", "is_tr", "anatomical_region", "specific_level",
        "nontrauma_l1", "nontrauma_l2"))),
      by = "icd_code"
    )

  sci_summ <- .sci_cat_summ(diag_exp, group_col)

  cause_icds <- icds_cause %>%
    dplyr::select(icd_code) %>%
    dplyr::distinct() %>%
    dplyr::mutate(icd_cause_flg = 1L)

  diag_exp <- diag_exp %>%
    dplyr::left_join(cause_icds, by = "icd_code") %>%
    dplyr::mutate(icd_cause_flg = tidyr::replace_na(icd_cause_flg, 0L)) %>%
    dplyr::left_join(dplyr::select(icds_cause, icd_code, new_inj_category), by = "icd_code")

  cause_summ <- .cause_summ(diag_exp, group_col)

  cause_hos <- diag_exp %>%
    dplyr::filter(icd_cause_flg == 1) %>%
    dplyr::distinct(.data[[group_col]]) %>%
    dplyr::mutate(hos_eth_flg = 1L)

  # MIMIC-III procedures are ICD-9 only (the table carries icd9_code, with no
  # version column), so stamping icd_version = "9" makes the join key match
  # only the dictionary's ICD-9 side. No explicit version filter is needed.
  proc_summ <- NULL
  if (!is.null(icds_proc)) {
    proc_summ <- .build_proc_summ(
      con, raw_schema, group_col, icds_proc, icds_proc_dict,
      prepare = function(df) df %>%
        dplyr::rename(icd_code = icd9_code) %>%
        dplyr::mutate(icd_code    = as.character(icd_code),
                      icd_version = "9"))
  }

  result <- sci_summ %>%
    dplyr::left_join(cause_summ, by = group_col) %>%
    dplyr::left_join(cause_hos,  by = group_col) %>%
    dplyr::mutate(hos_eth_flg = tidyr::replace_na(hos_eth_flg, 0L))

  result <- if (is.null(proc_summ)) .add_na_proc_cols(result, "not_reported")
            else dplyr::left_join(result, proc_summ, by = group_col)

  result
}

.sci_enrich_eicu <- function(diag_exp, group_col,
                               icds_scid, icds_cause) {
  # eICU: only evidence-resolved code/version pairs are scoreable.
  scid_icds <- icds_scid %>%
    dplyr::select(icd_code, icd_version) %>%
    dplyr::distinct() %>%
    dplyr::mutate(icd_scid_flg = 1L)

  diag_exp <- diag_exp %>%
    dplyr::left_join(scid_icds, by = c("icd_code", "icd_version")) %>%
    dplyr::mutate(icd_scid_flg = tidyr::replace_na(icd_scid_flg, 0L)) %>%
    dplyr::left_join(
      dplyr::select(icds_scid, dplyr::any_of(c(
        "icd_code", "icd_version",
        "is_sci", "is_tr", "anatomical_region", "specific_level",
        "nontrauma_l1", "nontrauma_l2"))),
      by = c("icd_code", "icd_version")
    )

  sci_summ <- .sci_cat_summ(diag_exp, group_col)

  cause_icds <- icds_cause %>%
    dplyr::select(icd_code, icd_version) %>%
    dplyr::distinct() %>%
    dplyr::mutate(icd_cause_flg = 1L)

  diag_exp <- diag_exp %>%
    dplyr::left_join(cause_icds, by = c("icd_code", "icd_version")) %>%
    dplyr::mutate(icd_cause_flg = tidyr::replace_na(icd_cause_flg, 0L)) %>%
    dplyr::left_join(
      dplyr::select(icds_cause, icd_code, icd_version, new_inj_category),
      by = c("icd_code", "icd_version"))

  cause_summ <- .cause_summ(diag_exp, group_col)

  cause_hos <- diag_exp %>%
    dplyr::filter(icd_cause_flg == 1) %>%
    dplyr::distinct(.data[[group_col]]) %>%
    dplyr::mutate(hos_eth_flg = 1L)

  result <- sci_summ %>%
    dplyr::left_join(cause_summ, by = group_col) %>%
    dplyr::left_join(cause_hos,  by = group_col) %>%
    dplyr::mutate(hos_eth_flg = tidyr::replace_na(hos_eth_flg, 0L))

  # eICU has no ICD-coded procedure table - its nearest equivalents are the
  # free-text `treatment.treatmentstring` hierarchy and `admissiondx`, neither
  # of which carries ICD procedure codes - so procedure classification is opted
  # out for this family rather than approximated. The columns are still emitted,
  # typed and flagged "not_reported", because merge_sources() keeps only the
  # columns common to every source: omitting them here would drop the procedure
  # block from the merged cohort_info for MIMIC too.
  .add_na_proc_cols(result, "not_reported")
}

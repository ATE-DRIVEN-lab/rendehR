# Optional spinal cord injury (SCI) pages of the cohort explorer.
#
# The SCI pages show a study's analysis set: SCI-D categories, neurological
# classification, complications, interventions, admission biomarkers and the
# study's result tables. Their data come from a directory of R data files
# prepared by the study (one row per flagged admission with the derived
# variables), not from the package pipeline, so they are optional.

# Second-level categories in precedence order, named as in .sci_cat_summ(); the
# first three are SCI (top level "Spinal cord damage").
.xp_sci_cat_levels <- unname(unlist(.sci_cat_labels[c(
  "sci_traumatic", "sci_non_traumatic", "sci_unspecified", "nerve_root",
  "control_trauma", "control_non_trauma")]))
.xp_sci_cat_colors <- stats::setNames(c("#8a5a44", "#b08968", "#d0b49a", "#a3a17a", "#78909c", "#4f6a72"),
                                      .xp_sci_cat_levels)
.xp_sci_colors <- c("SCI" = "#8a5a44", "Not SCI" = "#4f6a72")
.xp_sci_source_colors <- c("MIMIC-III" = "#78909c", "MIMIC-IV" = "#37474f", "eICU-CRD" = "#b0a58c")

.xp_sci_biomarkers <- tibble::tribble(
  ~concept, ~label, ~panel, ~unit, ~ref_lo, ~ref_hi,
  "hemoglobin", "Hemoglobin", "Blood count", "g/dL", 12, 17.5,
  "hematocrit", "Hematocrit", "Blood count", "%", 36, 51,
  "rbc", "RBC", "Blood count", "M/uL", 4.0, 5.9,
  "wbc", "WBC", "Blood count", "K/uL", 4.0, 11.0,
  "platelet", "Platelets", "Blood count", "K/uL", 150, 400,
  "mcv", "MCV", "Blood count", "fL", 80, 100,
  "mch", "MCH", "Blood count", "pg", 27, 33,
  "mchc", "MCHC", "Blood count", "g/dL", 32, 36,
  "rdw", "RDW", "Blood count", "%", 11.5, 14.5,
  "sodium", "Sodium", "Chemistry", "mEq/L", 135, 145,
  "potassium", "Potassium", "Chemistry", "mEq/L", 3.5, 5.1,
  "chloride", "Chloride", "Chemistry", "mEq/L", 98, 107,
  "bicarbonate", "Bicarbonate", "Chemistry", "mEq/L", 22, 29,
  "anion_gap", "Anion gap", "Chemistry", "mEq/L", 8, 16,
  "bun", "BUN", "Chemistry", "mg/dL", 7, 20,
  "creatinine", "Creatinine", "Chemistry", "mg/dL", 0.6, 1.3,
  "glucose", "Glucose", "Chemistry", "mg/dL", 70, 140,
  "calcium", "Calcium", "Chemistry", "mg/dL", 8.5, 10.5,
  "magnesium", "Magnesium", "Chemistry", "mg/dL", 1.7, 2.4,
  "phosphate", "Phosphate", "Chemistry", "mg/dL", 2.5, 4.5,
  "prothrombin_time_inr", "INR", "Coagulation", "ratio", 0.8, 1.2,
  "ptt", "PTT", "Coagulation", "sec", 25, 35)
# Sex-specific adult ranges where they differ materially; the pooled range above is the fallback.
.xp_sci_bio_sex_ref <- tibble::tribble(
  ~concept, ~sex, ~ref_lo, ~ref_hi,
  "hemoglobin", "male", 13.5, 17.5, "hemoglobin", "female", 12.0, 15.5,
  "hematocrit", "male", 41, 50, "hematocrit", "female", 36, 44,
  "rbc", "male", 4.5, 5.9, "rbc", "female", 4.1, 5.1)
# Reference range of each value: by sex where defined, pooled when sex is unknown.
.xp_sci_ref_range <- function(concept, sex) {
  b <- .xp_sci_biomarkers[match(concept, .xp_sci_biomarkers$concept), ]
  k <- match(paste(concept, sex), paste(.xp_sci_bio_sex_ref$concept, .xp_sci_bio_sex_ref$sex))
  list(lo = dplyr::coalesce(.xp_sci_bio_sex_ref$ref_lo[k], b$ref_lo), hi = dplyr::coalesce(.xp_sci_bio_sex_ref$ref_hi[k], b$ref_hi))
}

.xp_sci_interventions <- c(
  "Mechanical ventilation" = "mechanical_ventilation", "Tracheostomy" = "tracheostomy",
  "Any vasopressor" = "any_vasopressor", "Norepinephrine" = "norepinephrine", "Phenylephrine" = "phenylephrine",
  "Dopamine" = "dopamine", "Epinephrine" = "epinephrine", "Vasopressin" = "vasopressin",
  "Methylprednisolone (any)" = "methylprednisolone_any",
  "IV methylprednisolone, day 0-1" = "methylprednisolone_early_iv",
  "Pharmacological VTE prophylaxis" = "vte_prophylaxis", "Spine surgery" = "spine_surgery")

.xp_sci_age_levels <- .xp_age_levels
# Display order only (SCI dictionary anatomical_region values); other values are kept, after these.
.xp_sci_level_order <- c("cervical", "cervical_thoracic", "thoracic", "lumbar_thoracic", "lumbar",
                         "lumbar_sacral_thoracic", "lumbar_sacral", "sacral", "coccygeal_sacral", "coccygeal",
                         "multiple_regions_unspecified", "unspecified", "not_applicable",
                         # Title-case labels of analysis caches built with the previous dictionary.
                         "Cervical", "Cervicothoracic", "Thoracic", "Thoracolumbar", "Lumbar", "Lumbosacral",
                         "Sacral", "Conus/Cauda", "Multiple levels", "Unspecified", "Not applicable")

# Variable dictionary of the SCI analysis set.
.xp_sci_vars <- function(adm) {
  cx <- grep("^cx_", names(adm), value = TRUE)
  cx_lab <- stats::setNames(cx, stringr::str_to_sentence(gsub("_", " ", sub("^cx_", "", cx))))
  comp <- setdiff(grep("^(charlson|elixhauser)_", names(adm), value = TRUE), c("charlson_score", "elixhauser_score"))
  b <- .xp_sci_biomarkers
  v <- dplyr::bind_rows(
    tibble::tribble(~var, ~label, ~type, ~group,
      "age", "Age, years", "cont", "Demographics", "age_group", "Age group", "cat", "Demographics",
      "sex", "Sex", "cat", "Demographics", "race", "Race/ethnicity", "cat", "Demographics",
      "insurance", "Insurance (MIMIC only)", "cat", "Demographics", "language", "Language (MIMIC only)", "cat", "Demographics",
      "marital_status", "Marital status (MIMIC only)", "cat", "Demographics", "height_cm", "Height, cm", "cont", "Demographics",
      "weight_kg", "Weight at admission, kg", "cont", "Demographics", "bmi", "Body-mass index, kg/m2", "cont", "Demographics",
      "source_label", "Source", "cat", "Admission", "sci", "SCI vs Not SCI", "cat", "SCI-D classification",
      "second_level", "SCI-D category", "cat", "SCI-D classification", "top_level", "Top level", "cat", "SCI-D classification",
      "trauma", "Traumatic / non-traumatic", "cat", "SCI-D classification", "injury_level", "Injury level (code)", "cat", "SCI-D classification",
      "level_group", "Neurological level group", "cat", "SCI-D classification",
      "completeness", "Lesion completeness / cord syndrome", "cat", "SCI-D classification",
      "completeness3", "Completeness (3 groups)", "cat", "SCI-D classification", "coded_plegia", "Coded plegia", "cat", "SCI-D classification",
      "cause_main", "Cause of injury", "cat", "SCI-D classification", "icd_period", "Coding period", "cat", "SCI-D classification",
      "num_scid_icd_codes", "SCI-D codes per admission", "cont", "SCI-D classification",
      "admission_type", "Admission type", "cat", "Admission", "hospital_admission_location", "Admission origin", "cat", "Admission",
      "admission_service", "Admitting service", "cat", "Admission", "hospital_discharge_location", "Discharge destination", "cat", "Outcomes",
      "hospital_los", "Hospital length of stay, days", "cont", "Outcomes", "any_icu", "ICU admission", "bin", "ICU",
      "n_icu", "ICU stays per admission", "cont", "ICU", "unit_first", "First ICU unit (if admitted)", "cat", "ICU",
      "icu_los_total", "ICU length of stay, days (if admitted)", "cont", "ICU", "icu_death", "ICU mortality (if admitted)", "bin", "ICU",
      "hospital_mortality", "In-hospital mortality", "bin", "Outcomes", "death_90d", "90-day mortality (MIMIC only)", "bin", "Outcomes",
      "death_1y", "1-year mortality (MIMIC only)", "bin", "Outcomes", "readmission_30d", "30-day readmission", "bin", "Outcomes",
      "discharged_rehab", "Discharged to rehabilitation", "bin", "Outcomes", "discharged_home", "Discharged home", "bin", "Outcomes",
      "charlson", "Charlson comorbidity index", "cont", "Comorbidity", "elixhauser", "Elixhauser score", "cont", "Comorbidity",
      "iss_scored", "Injury Severity Score (if trauma-coded)", "cont", "Comorbidity",
      "operative", "Operative procedure (MIMIC only)", "bin", "Procedures",
      "num_diag_icd_codes", "Diagnosis codes per admission", "cont", "Admission"),
    tibble::tibble(var = unname(.xp_sci_interventions), label = names(.xp_sci_interventions), type = "bin", group = "Interventions"),
    tibble::tibble(var = unname(cx_lab), label = names(cx_lab), type = "bin", group = "Complications"),
    tibble::tibble(var = paste0("bio_", b$concept), label = paste0(b$label, " at admission (", b$unit, ")"), type = "cont", group = "Biomarkers"),
    tibble::tibble(var = comp, label = paste(stringr::str_to_sentence(stringr::str_extract(comp, "^[a-z]+")), sub("^[a-z]+_", "", comp)),
                   type = rep("bin", length(comp)), group = rep("Comorbidity components", length(comp))))
  v[v$var %in% names(adm), ]
}

.xp_sci_groups <- c("SCI vs Not SCI" = "sci", "SCI-D category" = "second_level", "Source" = "source_label",
                    "Neurological level group" = "level_group", "Completeness (3 groups)" = "completeness3",
                    "Lesion completeness / cord syndrome" = "completeness", "Traumatic / non-traumatic" = "trauma",
                    "Admission type" = "admission_type", "Discharge destination" = "hospital_discharge_location",
                    "Sex" = "sex", "Age group" = "age_group", "Cause of injury" = "cause_main", "Coding period" = "icd_period",
                    "Injury level (code)" = "injury_level", "ICU admission" = "any_icu",
                    "In-hospital mortality" = "hospital_mortality", "First ICU unit" = "unit_first", "None (overall)" = ".all")

#' Load the optional SCI pages of the cohort explorer
#'
#' Reads a spinal cord injury study's prepared explorer data: R data files with
#' the flagged admissions and their derived variables (SCI-D category,
#' neurological classification, complications, interventions, admission
#' biomarkers), the patient context, code lists, biomarker trajectories, MAP
#' values and surgery timing, plus optionally the study's concept categories.
#' Pass the result to [run_explorer()] as `sci`.
#'
#' @param app_data_dir Directory with the prepared files (`adm.rds`,
#'   `ctx_adm.rds`, `ctx_icu.rds`, `patients.rds`, `dx.rds`, `px.rds`,
#'   `codes.rds`, `bio_daily.rds`, `map_stay.rds`, `map_values.rds`,
#'   `surgery_timing.rds`; `dur.rds` is used for the medication library when
#'   present).
#' @param analysis_dir Optional analysis output directory; its
#'   `tables/T22a_ts_concept_summary.csv` groups the concepts into categories.
#' @return A list of class `rendehR_explorer_sci`.
#' @export
#'
#' @examples
#' \dontrun{
#' sci <- explorer_sci_data("analysis/outputs/explorer_data", "analysis/outputs")
#' run_explorer("rendehr", db_name = "rendehr", sci = sci)
#' }
explorer_sci_data <- function(app_data_dir, analysis_dir = NULL) {
  need <- c("adm.rds", "ctx_adm.rds", "ctx_icu.rds", "patients.rds", "dx.rds", "px.rds", "codes.rds",
            "bio_daily.rds", "map_stay.rds", "map_values.rds", "surgery_timing.rds")
  miss <- need[!file.exists(file.path(app_data_dir, need))]
  if (length(miss)) stop("Missing SCI explorer files in '", app_data_dir, "': ", paste(miss, collapse = ", "), call. = FALSE)
  rd <- function(f) readRDS(file.path(app_data_dir, f))
  opt_csv <- function(...) { f <- file.path(...); if (length(f) && file.exists(f)) readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c")) else NULL }
  tables_dir <- if (!is.null(analysis_dir)) file.path(analysis_dir, "tables") else NULL
  out <- list(
    adm = rd("adm.rds"), ctx_adm = rd("ctx_adm.rds"), ctx_icu = rd("ctx_icu.rds"), patients = rd("patients.rds"),
    dx = rd("dx.rds"), px = rd("px.rds"), codes = rd("codes.rds"), bio_daily = rd("bio_daily.rds"),
    map_stay = rd("map_stay.rds"), map_values = rd("map_values.rds"), surgery = rd("surgery_timing.rds"),
    dur = if (file.exists(file.path(app_data_dir, "dur.rds"))) rd("dur.rds") else NULL,
    ts_summary = if (!is.null(tables_dir)) opt_csv(tables_dir, "T22a_ts_concept_summary.csv"))
  class(out) <- c("rendehR_explorer_sci", "list")
  out
}

#' @rdname explorer_sci_data
#' @param data A bundle from [explorer_data()].
#' @param sci The result of `explorer_sci_data()`.
#' @return `explorer_add_sci()`: the bundle with the SCI analysis set as its
#'   cohort and the SCI pages enabled.
#' @export
explorer_add_sci <- function(data, sci) {
  `%>%` <- magrittr::`%>%`
  if (!inherits(data, "rendehR_explorer_data")) stop("`data` must be a bundle from explorer_data().", call. = FALSE)
  if (!inherits(sci, "rendehR_explorer_sci")) stop("`sci` must come from explorer_sci_data().", call. = FALSE)
  adm <- sci$adm
  vars <- .xp_sci_vars(adm)
  for (v in vars$var[vars$type == "bin"]) adm[[v]] <- as.integer(adm[[v]] > 0)
  adm$age_group <- factor(as.character(adm$age_group), .xp_sci_age_levels)
  lv <- as.character(adm$injury_level)
  adm$injury_level <- factor(lv, unique(c(intersect(.xp_sci_level_order, lv), sort(unique(lv)))))
  adm$in_cohort <- TRUE
  adm$patient_id <- as.character(adm$patient_id)
  adm$adm_id <- as.character(adm$adm_id)

  sctx <- sci$ctx_adm %>% dplyr::transmute(source, adm_id = as.character(adm_id), flagged, second_level,
                                           scid_icd_codes, level_group, completeness)
  ctx <- data$ctx_adm[, setdiff(names(data$ctx_adm), c("flagged", "second_level", "scid_icd_codes", "level_group",
                                                       "completeness", "tl_group"))] %>%
    dplyr::left_join(sctx, by = c("source", "adm_id"))
  ctx$in_cohort <- ctx$flagged %in% TRUE
  ctx$tl_group <- ifelse(ctx$in_cohort, ctx$second_level, "Not flagged (context)")

  pat <- sci$patients
  pat$n_cohort <- pat$n_flagged
  pat$patient_id <- as.character(pat$patient_id)

  smd <- list(sci = c("SCI", "Not SCI"), second_level = c("Traumatic SCI", "Spinal trauma control"),
              level_group = c("Tetraplegia level (cervical)", "Paraplegia level (thoracic-sacral)"),
              completeness3 = c("Complete", "Incomplete"), trauma = c("Traumatic", "Non-traumatic"),
              sex = c("male", "female"), admission_type = c("emergency", "elective"))
  lvl <- c("Tetraplegia level (cervical)", "Paraplegia level (thoracic-sacral)", "Level unknown", "(not classified)")
  comp <- c("Complete", "Incomplete", "Not specified", "(not classified)")

  data$adm <- adm
  data$ctx_adm <- ctx
  if (nrow(sci$ctx_icu)) data$icu <- sci$ctx_icu %>%
    dplyr::transmute(source, patient_id = as.character(patient_id), adm_id = as.character(adm_id),
                     icu_id = as.character(icu_id), icu_in_min, icu_out_min, icu_unit)
  data$patients <- pat
  data$vars <- vars
  data$groups <- .xp_sci_groups[.xp_sci_groups %in% c(names(adm), ".all")]
  data$filters <- list(
    list(var = "source", label = "Source", type = "checkbox"),
    list(var = "sci", label = "Group", type = "checkbox", choices = c("SCI", "Not SCI"), keep_na = FALSE, inline = TRUE),
    list(var = "second_level", label = "SCI-D category", type = "picker", choices = .xp_sci_cat_levels, keep_na = FALSE),
    list(var = "age", label = "Age, years", type = "slider", panel = "Patient"),
    list(var = "sex", label = "Sex", type = "checkbox", choices = c("female", "male"), panel = "Patient"),
    list(var = "admission_type", label = "Admission type", type = "picker", panel = "Admission and injury"),
    list(var = "level_group", label = "Neurological level group (cord categories)", type = "picker", choices = lvl,
         na_label = "(not classified)", keep_na = FALSE, panel = "Admission and injury"),
    list(var = "completeness3", label = "Completeness", type = "picker", choices = comp, na_label = "(not classified)",
         keep_na = FALSE, panel = "Admission and injury"),
    list(var = "any_icu", label = "ICU admission", type = "icu", panel = "Admission and injury"))
  data$defaults <- list(
    overview_group = "second_level", t1_group = "second_level",
    t1_vars = intersect(c("age", "sex", "admission_type", "hospital_admission_location", "any_icu", "hospital_los",
                          "hospital_mortality", "death_1y", "hospital_discharge_location", "charlson", "operative"), vars$var),
    d_group = "sci", o_group = "second_level", s_group = "second_level", x_row = "second_level", sc_col = "sci",
    o_strata = c("None" = "", "Source" = "source_label", "Sex" = "sex", "Age group" = "age_group"),
    smd = smd,
    data_cols = intersect(c("source_label", "patient_id", "adm_id", "second_level", "sci", "age", "sex", "admission_type",
                            "any_icu", "hospital_los", "hospital_mortality", "hospital_discharge_location", "level_group",
                            "completeness", "cause_main", "icd_period"), names(adm)))
  data$palettes <- list(second_level = .xp_sci_cat_colors, sci = .xp_sci_colors,
                        tl_group = c(.xp_sci_cat_colors, "Not flagged (context)" = "#cccccc"))
  data$meta$source_colors <- c(.xp_sci_source_colors, data$meta$source_colors[!names(data$meta$source_colors) %in% names(.xp_sci_source_colors)])
  data$meta$index_group <- "index_category"
  data$meta$index_group_label <- "Index category"
  data$meta$title <- "SCI-EHR Explorer"
  data$meta$mode <- "sci"
  if (!is.null(sci$ts_summary)) {
    ts <- sci$ts_summary[!duplicated(sci$ts_summary$concept), ]
    data$concept_categories <- stats::setNames(ts$category, ts$concept)
  }
  if (!is.null(sci$dur)) {
    den <- dplyr::count(adm, source, name = "den")
    data$med_prev <- sci$dur %>% dplyr::semi_join(adm, by = c("source", "adm_id")) %>%
      dplyr::distinct(source, adm_id, drug) %>% dplyr::count(source, drug) %>%
      dplyr::left_join(den, by = "source") %>% dplyr::mutate(p = n / den)
    sci$dur <- NULL                                        # the Subject Explorer reads exposures live
  }
  data$sci <- sci
  data
}

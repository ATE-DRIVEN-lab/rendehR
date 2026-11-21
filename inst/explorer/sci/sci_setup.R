# SCI pages: objects of the SCI analysis set (bundle$sci from explorer_sci_data()).
S <- D$sci
CAT_LEVELS <- .xp_sci_cat_levels
CAT_COLORS <- .xp_sci_cat_colors
SCI_COLORS <- .xp_sci_colors
LAB <- .sci_cat_labels
TOP_OF <- stats::setNames(c(LAB$top_sci, LAB$top_sci, LAB$top_sci, LAB$top_nerve_root, LAB$top_control, LAB$top_control),
                          CAT_LEVELS)
RULES <- tibble::tribble(~Precedence, ~Category, ~`Assignment rule (SCI codes = is_sci 1, 3 or 999)`,
  1, LAB$sci_traumatic, "≥1 SCI code, and ≥1 of the SCI codes is traumatic (is_tr = 1)",
  2, LAB$sci_non_traumatic, "≥1 SCI code, none of them traumatic, ≥1 non-traumatic (is_tr = 0)",
  3, LAB$sci_unspecified, "≥1 SCI code, all with traumatic status unknown (is_tr = 2)",
  4, LAB$nerve_root, "≥1 nerve-root code (is_sci = 2), no SCI code",
  5, LAB$control_trauma, "only column/spine codes (is_sci = 0), ≥1 traumatic",
  6, LAB$control_non_trauma, "only column/spine codes (is_sci = 0), none traumatic") %>%
  mutate(Group = if_else(Precedence <= 3, "SCI", "Not SCI"), `Top level` = TOP_OF[Category], .after = Precedence)
BIOMARKERS <- .xp_sci_biomarkers
BIO_CHOICES <- stats::setNames(BIOMARKERS$concept, paste0(BIOMARKERS$label, " (", BIOMARKERS$unit, ")"))
CX_COLS <- grep("^cx_", names(ADM), value = TRUE)
CX_LAB <- stats::setNames(CX_COLS, stringr::str_to_sentence(gsub("_", " ", sub("^cx_", "", CX_COLS))))
IV_LAB <- .xp_sci_interventions[.xp_sci_interventions %in% names(ADM)]

# Level order of groupings: categories by precedence, descriptors from best to unknown, age bands; others alphabetical.
LVL_ORDER <- c(CAT_LEVELS, "SCI", "Not SCI", unname(SRC_LABEL[SRC]),
               "Tetraplegia level (cervical)", "Paraplegia level (thoracic-sacral)", "Level unknown",
               "Complete", "Incomplete", "Complete lesion", "Central cord syndrome", "Anterior cord syndrome",
               "Brown-Séquard syndrome", "Posterior cord syndrome", "Other incomplete lesion", "Not specified",
               "Quadriplegia/tetraplegia coded", "Paraplegia coded", "No plegia code", .xp_sci_age_levels)
grp_levels <- function(x) {
  u <- unique(as.character(x[!is.na(x)]))
  if (is.factor(x)) intersect(levels(x), u) else c(intersect(LVL_ORDER, u), sort(setdiff(u, LVL_ORDER)))
}
# Sequential ramp for ordered descriptors; unknown / unspecified values in grey.
seq_colors <- function(lv) {
  unk <- lv %in% c("Not specified", "Level unknown", "No plegia code", "Unknown")
  cols <- stats::setNames(rep("#c3c9cf", length(lv)), lv)
  if (any(!unk)) cols[!unk] <- grDevices::colorRampPalette(c("#5d3a2c", "#dcc7b4"))(sum(!unk))
  cols
}
# Small-cell rule for hovers: a masked count (1-9) also hides its share (k = p * n otherwise).
mask_kp <- function(k, p, on = TRUE) ifelse(isTRUE(on) & k >= 1 & k < 10, "<10", sprintf("%s (%.1f%%)", fmt_n(k), 100 * p))

CODES <- S$codes; BIO_DAILY <- S$bio_daily; MAP_STAY <- S$map_stay; MAP_VALUES <- S$map_values
SURG <- S$surgery

# Diagnoses and procedures from the study's tables (SCI-D codes marked).
patient_codes <- function(source, patient_id) {
  dx <- S$dx[S$dx$source == source & S$dx$patient_id == patient_id, , drop = FALSE]
  px <- S$px[S$px$source == source & S$px$patient_id == patient_id, , drop = FALSE]
  dx$is_scid <- if_else(dx$is_scid %in% TRUE, "★", "")
  list(dx = dx[order(dx$adm_id, dplyr::coalesce(dx$seq_num, 0)), , drop = FALSE], px = px)
}

# Extra cards on the Subject Explorer's Admission tab.
sci_admission_cards <- function(s, a) {
  f <- ADM[ADM$source == s$source & ADM$adm_id == a$adm_id, , drop = FALSE]
  sc <- S$ctx_adm[S$ctx_adm$source == s$source & S$ctx_adm$adm_id == a$adm_id, , drop = FALSE]
  codes <- tibble(icd_code = stringr::str_trim(unlist(strsplit(dplyr::coalesce(sc$scid_icd_codes[1], ""), " | ", fixed = TRUE)))) %>%
    filter(nzchar(icd_code)) %>%
    left_join(CODES$scid %>% distinct(icd_code, .keep_all = TRUE) %>% select(icd_code, icd_version, code_category, long_title), by = "icd_code")
  kv <- function(l) tags$table(class = "table table-sm", tags$tbody(lapply(names(l), function(n)
    tags$tr(tags$th(n, style = "width:45%"), tags$td(as.character(l[[n]] %||% "–"))))))
  cls <- if (nrow(f)) list(Category = f$second_level, `Top level` = f$top_level, Group = f$sci, `Neurological level group` = f$level_group,
                           Completeness = f$completeness, `Coded plegia` = f$coded_plegia, `Injury level (codes)` = as.character(f$injury_level),
                           `Cause of injury` = f$cause_main, `Coding period` = f$icd_period,
                           `1-year mortality` = if (is.na(f$death_1y)) "n/a (eICU-CRD)" else if (f$death_1y %in% 1) "yes" else "no",
                           Charlson = f$charlson)
         else list(Status = "Not an SCI-D flagged admission (patient context only)")
  ivs <- if (nrow(f)) names(IV_LAB)[which(unlist(f[1, unname(IV_LAB)]) == 1)] else character(0)
  cxs <- if (nrow(f)) names(CX_LAB)[which(unlist(f[1, unname(CX_LAB)]) == 1)] else character(0)
  bio <- if (nrow(f)) BIOMARKERS %>% mutate(v = unlist(f[1, paste0("bio_", concept)]),
                                            ref_lo = .xp_sci_ref_range(concept, f$sex[1])$lo, ref_hi = .xp_sci_ref_range(concept, f$sex[1])$hi,
                                            flag = case_when(is.na(v) ~ "", v < ref_lo ~ "low", v > ref_hi ~ "high", TRUE ~ "")) %>%
    filter(!is.na(v)) else tibble()
  tagList(
    layout_columns(col_widths = c(6, 6),
      card(card_header("SCI-D classification"), kv(cls)),
      card(card_header("Interventions and complications"),
           tags$p(tags$b("Interventions: "), if (length(ivs)) paste(ivs, collapse = ", ") else "none recorded"),
           tags$p(tags$b("Complications: "), if (length(cxs)) paste(cxs, collapse = ", ") else "none coded"))),
    layout_columns(col_widths = c(7, 5),
      card(card_header(sprintf("SCI-D codes of this admission (%d)", nrow(codes))),
           if (nrow(codes)) dt_table(codes %>% rename(Code = icd_code, Version = icd_version, `Code category` = code_category, Title = long_title), page = 10, dom = .dt_dom_pages)
           else tags$p("No SCI-D code (context admission).")),
      card(card_header("Admission biomarkers (mean -6 h to +24 h)"),
           if (nrow(bio)) datatable(bio %>% transmute(Biomarker = label, Value = round(v, 2), Unit = unit, Reference = sprintf("%s-%s", ref_lo, ref_hi), Flag = flag),
                                    rownames = FALSE, options = list(dom = "t", pageLength = 25), class = "compact") %>%
             formatStyle("Flag", color = styleEqual(c("low", "high"), c("#4f6a72", "#8a5a44")), fontWeight = "bold")
           else tags$p("Not available (context admission or no values in the window)."))))
}

# Optional SCI pages of the cohort explorer against a study's prepared data and
# analysis outputs (read-only). Set RENDEHR_EXPLORER_SCI_DIR (prepared files),
# RENDEHR_EXPLORER_SCI_ANALYSIS (analysis outputs, used to check the numbers), RENDEHR_EXPLORER_SCI_DB and
# RENDEHR_EXPLORER_SCI_SCHEMA (the study's merged schema), plus PG* credentials.

.xp_sci_env <- local({
  cache <- NULL
  function() {
    dir <- Sys.getenv("RENDEHR_EXPLORER_SCI_DIR", ""); out <- Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS", "")
    db <- Sys.getenv("RENDEHR_EXPLORER_SCI_DB", ""); schema <- Sys.getenv("RENDEHR_EXPLORER_SCI_SCHEMA", "")
    skip_if(!nzchar(dir) || !nzchar(out) || !nzchar(db) || !nzchar(schema),
            "set RENDEHR_EXPLORER_SCI_DIR, _ANALYSIS, _DB and _SCHEMA to run the SCI explorer tests")
    for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival", "RPostgres"))
      skip_if_not_installed(p)
    if (is.null(cache)) {
      creds <- list(postgres = list(host = Sys.getenv("PGHOST", "localhost"), port = as.integer(Sys.getenv("PGPORT", "5432")),
                                    user = Sys.getenv("PGUSER", "postgres"), password = Sys.getenv("PGPASSWORD", "")))
      suppressMessages(ehr_connect(db, creds = creds))
      b <- explorer_add_sci(explorer_data(schema), explorer_sci_data(dir, out))
      cache <<- attr(explorer_app(b, live = TRUE), "explorer_env")
    }
    cache
  }
})
# Evaluate a test body inside the app environment (its objects and helpers).
in_app <- function(code) eval(substitute(code), new.env(parent = .xp_sci_env()))
ptab <- function(name) readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", paste0(name, ".csv")),
                                       show_col_types = FALSE)
render_all <- function(output, names) for (n in names) expect_false(is.null(output[[n]]), info = n)

test_that("the SCI analysis set reproduces the study's cohort", in_app({
  # the study's own counts, read independently of the app
  NUM <- jsonlite::read_json(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "numbers.json"), simplifyVector = TRUE)
  expect_equal(nrow(ADM), NUM$total_admissions)
  expect_equal(nrow(PATIENTS), NUM$total_patients)
  expect_equal(sum(ADM$sci == "SCI"), NUM$total_sci_adm)
  expect_equal(sort(unique(ADM$second_level)), sort(CAT_LEVELS))
  expect_equal(anyDuplicated(paste(ADM$source, ADM$adm_id)), 0L)
  expect_equal(sum(CTX_ADM$in_cohort), nrow(ADM))
  expect_equal(levels(ADM$age_group), c("<18", "18-44", "45-64", "65-79", ">=80"))
}))

test_that("Table 1 equals table T11a and SCI vs Not SCI SMDs equal T11c", in_app({
  pat <- ADM[ADM$is_index & ADM$sci == "SCI", ]
  vars <- VARS[VARS$var %in% c("age", "sex", "admission_type", "any_icu", "hospital_los", "hospital_mortality", "death_1y",
                               "hospital_discharge_location", "charlson"), ]
  app <- char_table(pat, "second_level", vars)
  ref <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T11a_characteristics_SCI.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  for (v in vars$label) for (lvl in unique(app$level[app$variable == v])) {
    a <- app[app$variable == v & app$level == lvl, ]; r <- ref[ref$variable == v & ref$level == lvl, ]
    if (!nrow(r)) next
    for (col in c("Traumatic SCI", "Non-traumatic SCI", "Overall")) expect_identical(a[[col]], r[[col]], info = paste(v, lvl, col))
  }
  pat <- ADM[ADM$is_index, ]
  vars <- VARS[VARS$var %in% c("age", "admission_type", "hospital_mortality", "death_1y", "operative", "bmi"), ]
  app <- compare2(pat, "sci", "SCI", "Not SCI", vars)
  ref <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T11c_SCI_vs_not_SCI.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  j <- merge(app, ref, by = c("variable", "level"), suffixes = c("", ".ref"))
  expect_gt(nrow(j), 8)
  expect_equal(as.numeric(j$smd), as.numeric(j$smd.ref), tolerance = 1e-9)
  expect_identical(j$SCI, j$SCI.ref)
}))

test_that("mortality, interventions and complications equal tables T13, T31a and T30a", in_app({
  ref <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T13_mortality.csv"), show_col_types = FALSE)
  ref <- ref[ref$source == "All sources" & ref$group %in% c("All SCI", "All Not SCI") & ref$outcome == "In-hospital mortality", ]
  app <- prop_by(ADM[ADM$is_index, ], "hospital_mortality", "sci")
  for (g in c("SCI", "Not SCI")) {
    a <- app[app$group == g, ]; r <- ref[ref$group == paste("All", g), ]
    expect_equal(a$k, r$k); expect_equal(a$n, r$n); expect_equal(a$lo, r$lo, tolerance = 1e-12); expect_equal(a$hi, r$hi, tolerance = 1e-12)
  }
  iv <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T31a_sci_interventions_long.csv"), show_col_types = FALSE)
  for (i in c("mechanical_ventilation", "any_vasopressor", "vte_prophylaxis", "methylprednisolone_early_iv", "tracheostomy"))
    for (g in c("Traumatic SCI", "Spinal trauma control")) {
      r <- iv[iv$intervention == i & iv$group == g, ]; a <- ADM[ADM$second_level == g, ]
      expect_equal(sum(a[[i]]), r$k, info = paste(i, g)); expect_equal(nrow(a), r$n)
    }
  cx <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T30a_sci_complications_long.csv"), show_col_types = FALSE)
  for (cc in c("Pneumonia", "Pressure injury", "Neurogenic bladder", "Respiratory failure")) {
    col <- paste0("cx_", gsub("[^a-z]+", "_", tolower(cc)))
    expect_equal(sum(ADM[[col]][ADM$second_level == "Traumatic SCI"]), cx$k[cx$complication == cc & cx$group == "Traumatic SCI"], info = cc)
  }
}))

test_that("MAP, surgery timing, classification, trends and biomarkers equal tables T32, T31c, T29a, T34a/d, T17n/p", in_app({
  t32 <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T32_map_first_72h.csv"), show_col_types = FALSE)
  cv <- map_target_curve(SRC, c("Traumatic SCI", "Spinal trauma control"), 0.5, 85)
  cv$source <- SRC_LABEL[cv$source]
  j <- merge(cv, t32, by = c("second_level", "source"))
  expect_equal(nrow(j), 6); expect_equal(j$p, j$stays_mostly_ge85, tolerance = 1e-12); expect_equal(j$stays, j$icu_stays)
  t31c <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T31c_spine_surgery_timing_mimic_iv.csv"), show_col_types = FALSE)
  s <- SURG[SURG$second_level == "Traumatic SCI", ]
  expect_equal(nrow(s), t31c$n[t31c$second_level == "Traumatic SCI"])
  ss <- merge(surgery_summary(SURG), t31c, by = "second_level")             # page table, procedures before admission included
  expect_equal(nrow(ss), nrow(t31c)); expect_equal(ss$n.x, ss$n.y); expect_identical(ss$med, ss$days_median_iqr)
  expect_equal(ss$by1, ss$same_or_next_day, tolerance = 1e-12); expect_equal(ss$by3, ss$within_3_days, tolerance = 1e-12)
  expect_gt(sum(ss$before), 0)
  t29 <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T29a_neurological_impairment.csv"), show_col_types = FALSE)
  a <- neuro_counts(ADM, "Traumatic SCI", "level_group", "second_level")
  r <- t29[t29$group == "Traumatic SCI" & t29$variable == "Neurological level group", ]
  j <- merge(a, r, by.x = "val", by.y = "value"); expect_equal(nrow(j), 3); expect_equal(j$n.x, j$n.y)
  t34a <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T34a_tsci_cause_by_age.csv"), show_col_types = FALSE)
  cb <- cause_by_age(ADM[ADM$second_level == "Traumatic SCI", ]); cb$age_group <- as.character(cb$age_group)
  j <- merge(cb, t34a, by.x = c("age_group", "c"), by.y = c("age_group", "cause"))
  expect_equal(nrow(j), nrow(t34a)); expect_equal(j$p, j$pct, tolerance = 1e-12)
  t34d <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T34d_mimic_iv_category_trends.csv"), show_col_types = FALSE)
  tr <- mimic_iv_trends(); tr$period <- as.character(tr$period)
  j <- merge(tr, t34d, by = c("period", "second_level")); expect_equal(nrow(j), nrow(t34d)); expect_equal(j$n.x, j$n.y)
  t17n <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T17n_biomarker_SMD.csv"), show_col_types = FALSE)
  t17n <- t17n[t17n$comparison == "SCI vs Not SCI", ]
  for (b in BIOMARKERS$concept)
    expect_equal(round(smd_cont(ADM[[paste0("bio_", b)]][ADM$sci == "SCI"], ADM[[paste0("bio_", b)]][ADM$sci == "Not SCI"]), 3),
                 t17n$smd[t17n$concept == b], tolerance = 1e-9, info = b)
}))

test_that("complication rates keep cord-only groupings to cord categories and leave out sources that do not capture a variable", in_app({
  t30a <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T30a_sci_complications_long.csv"), show_col_types = FALSE)
  lv <- c("Tetraplegia level (cervical)", "Paraplegia level (thoracic-sacral)", "Level unknown")
  r <- event_rates(sci_scope(ADM, "level_group"), "level_group", c("cx_pneumonia", "cx_neurogenic_bladder"))
  j <- merge(r[r$var == "cx_pneumonia", ], t30a[t30a$complication == "Pneumonia", ], by.x = "by", by.y = "group")
  expect_equal(sort(j$by), sort(lv)); expect_equal(j$k.x, j$k.y); expect_equal(j$n.x, j$n.y)
  # eICU-CRD codes no neurogenic bladder: its admissions leave the denominator instead of counting as 0%
  expect_true(any(NOT_CAPTURED$source == "eicu" & NOT_CAPTURED$var == "cx_neurogenic_bladder"))
  eicu <- sci_scope(ADM, "level_group") %>% filter(source == "eicu") %>% count(by = as.character(level_group), name = "ne")
  j <- merge(merge(r[r$var == "cx_neurogenic_bladder", ], t30a[t30a$complication == "Neurogenic bladder", ], by.x = "by", by.y = "group"), eicu, by = "by")
  expect_equal(j$k.x, j$k.y); expect_equal(j$n.x, j$n.y - j$ne)
  # completeness of the symptom and nerve-root categories is not pooled into the cord groups
  r3 <- event_rates(sci_scope(ADM, "completeness3"), "completeness3", "cx_pneumonia")
  expect_false("cx_pneumonia" %in% NOT_CAPTURED$var)
  expect_equal(sum(r3$n), sum(ADM$second_level %in% CORD & !is.na(ADM$completeness3)))
  expect_lte(sum(r3$n), sum(!is.na(ADM$completeness3)))
  expect_true(all(NOT_CAPTURED$var %in% c(CX_LAB, IV_LAB)))
}))

test_that("SCI helpers order groups, mask small cells and use sex-specific reference ranges", in_app({
  expect_equal(grp_levels(c("Not SCI", "SCI", NA)), c("SCI", "Not SCI"))
  expect_equal(grp_levels(c("Spinal trauma control", "Traumatic SCI")), c("Traumatic SCI", "Spinal trauma control"))
  expect_equal(grp_levels(c("Not specified", "Incomplete", "Complete")), c("Complete", "Incomplete", "Not specified"))
  expect_equal(mask_kp(c(0, 7, 12), c(0, 0.07, 0.12)), c("0 (0.0%)", "<10", "12 (12.0%)"))
  expect_equal(mask_kp(7, 0.07, FALSE), "7 (7.0%)")
  rr <- .xp_sci_ref_range("hemoglobin", c("male", "female", NA))
  expect_equal(rr$lo, c(13.5, 12, 12)); expect_equal(rr$hi, c(17.5, 15.5, 17.5))
  expect_equal(.xp_sci_ref_range("sodium", "male")$lo, 135)
  expect_false(any(duplicated(toupper(CAT_COLORS))))
}))

test_that("Kaplan-Meier 1-year mortality agrees with the study's 1-year mortality (MIMIC)", in_app({
  pat <- ADM[ADM$is_index & ADM$source != "eicu", ]
  st <- as.integer(!is.na(pat$time_to_death_from_admission) & pat$time_to_death_from_admission <= 365)
  tm <- ifelse(st == 1, pmax(pat$time_to_death_from_admission, 0.01), 365)
  s <- summary(survival::survfit(survival::Surv(tm, st) ~ pat$sci), times = 365)
  km <- stats::setNames(1 - s$surv, sub("^pat\\$sci=", "", as.character(s$strata)))
  ref <- readr::read_csv(file.path(Sys.getenv("RENDEHR_EXPLORER_SCI_ANALYSIS"), "tables", "T13_mortality.csv"), show_col_types = FALSE)
  ref <- ref[ref$source == "All sources" & ref$outcome == "1-year mortality", ]
  expect_equal(unname(km["SCI"]), ref$p[ref$group == "All SCI"], tolerance = 0.005)
  expect_equal(unname(km["Not SCI"]), ref$p[ref$group == "All Not SCI"], tolerance = 0.005)
}))

test_that("every SCI page renders", {
  e <- .xp_sci_env()
  expect_match(as.character(e$ui(NULL)), "SCI-specific", fixed = TRUE)
  shiny::testServer(e$cohort_server, {
    session$setInputs(cl_cat = "Traumatic SCI", cl_ver = "10", cl_q = "", unit = "pat", src = e$SRC, mask = TRUE,
                      tc_cat = "Non-traumatic SCI", tc_n = 20, tc_mask = TRUE)
    render_all(output, c("sankey", "rules", "cl_n", "codelist", "catplot", "cattab", "topcodes", "period", "multi"))
  })
  shiny::testServer(e$sci_server, {
    session$setInputs(src = e$SRC, mask = TRUE, minn = 20, nc_desc = "level_group", nc_by = "second_level", nc_cat = e$CORD,
                      cx_by = "level_group", cx_sel = unname(e$CX_LAB), cx_cat = e$CAT_LEVELS, cx_view = "heat", iv_by = "completeness3",
                      iv_sel = unname(e$IV_LAB), iv_cat = e$CAT_LEVELS, iv_icu = FALSE, map_target = 85, map_share = 0.5, map_cat = c("Traumatic SCI", "Spinal trauma control"),
                      sg_cat = c("Traumatic SCI", "Non-traumatic SCI"), sg_max = 10, oi_by = "completeness3", oi_cat = "Traumatic SCI",
                      oi_out = c("hospital_mortality", "death_1y", "tracheostomy", "any_icu", "discharged_rehab"), ep_unit = "pct", ep_lvlcat = "Traumatic SCI")
    render_all(output, c("nc_plot", "nc_tab", "cx_plot", "cx_note", "cx_tab", "iv_plot", "iv_note", "iv_tab", "map_box", "map_curve", "map_tab",
                         "sg_plot", "sg_tab", "oi_plot", "oi_note", "oi_tab", "ep_cause", "ep_cause_note", "ep_sex", "ep_lvl"))
    if (nrow(e$MIV_ANCHOR)) render_all(output, "ep_trend")
    expect_match(as.character(output$cx_note$html), "Not captured", fixed = TRUE)
    expect_match(as.character(output$oi_note$html), "known discharge location", fixed = TRUE)
    session$setInputs(cx_view = "forest", cx_by = "sci", nc_desc = "completeness", nc_by = "source_label", map_target = 90, iv_icu = TRUE)
    render_all(output, c("cx_plot", "nc_plot", "map_box", "map_tab", "iv_plot"))
    session$setInputs(cx_sel = character(0))
    expect_error(output$cx_plot, "Select at least one complication")
    session$setInputs(src = c("eicu", "mimic_iii"))
    expect_error(output$sg_plot, "MIMIC-IV only")
  })
  shiny::testServer(e$bio_server, {
    session$setInputs(src = e$SRC, cat = e$CAT_LEVELS, group = "sci", mask = TRUE, bm = "creatinine", geom = "violin",
                      tr_bm = c("hemoglobin", "wbc"), tr_days = c(0, 7), cor_grp = "All selected", cor_m = "spearman", g1 = "SCI", g2 = "Not SCI")
    render_all(output, c("dist", "dist_tab", "all22_note", "all22", "smd", "traj", "abn", "cor", "avail"))
    expect_match(as.character(output$all22_note$html), "SCI minus Not SCI", fixed = TRUE)
    session$setInputs(g2 = "SCI")
    expect_error(output$smd, "two different groups")
  })
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    # every filter at its starting value (everything selected, including "(missing)" levels)
    defs <- stats::setNames(lapply(e$EX_FILTERS, e$filter_default), paste0("f_", vapply(e$EX_FILTERS, `[[`, "", "var")))
    do.call(session$setInputs, defs)
    session$setInputs(unit = "adm", mask = TRUE,
                      t1_group = "second_level", t1_vars = c("age", "sex", "hospital_mortality", "bio_hemoglobin", "cx_pneumonia"),
                      d_var = "age", d_group = "sci", d_geom = "box", o_var = "hospital_mortality", o_group = "second_level", o_strat = "",
                      s_group = "sci", s_days = 365, s_ci = TRUE, x_row = "second_level", x_col = "hospital_discharge_location",
                      x_pct = "row", sc_x = "age", sc_y = "hospital_los", sc_col = "sci", sc_log = FALSE)
    expect_equal(nrow(df()), nrow(e$ADM))
    render_all(output, c("t1", "dplot", "oplot", "splot", "xplot", "scplot"))
    session$setInputs(f_source = "eicu", f_second_level = "Traumatic SCI", f_level_group = "(not classified)")
    expect_true(all(df()$source == "eicu" & df()$second_level == "Traumatic SCI" & is.na(df()$level_group)))
  })
})

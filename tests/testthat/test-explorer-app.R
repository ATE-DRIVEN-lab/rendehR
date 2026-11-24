# Cohort explorer: bundle assembly and the server logic of every page, on a
# synthetic merged schema (no database needed).

test_that("the bundle has one row per admission and consistent derived columns", {
  f <- .xp_fixture()
  b <- .xp_fixture_bundle()
  expect_s3_class(b, "rendehR_explorer_data")
  expect_equal(nrow(b$ctx_adm), nrow(f$ci))
  expect_equal(nrow(b$adm), sum(f$ci$cohort_index == "true"))
  expect_true(all(b$adm$in_cohort))
  expect_equal(anyDuplicated(paste(b$ctx_adm$source, b$ctx_adm$adm_id)), 0L)
  # one index admission per cohort patient, and it is the patient's earliest cohort admission
  expect_equal(sum(b$adm$is_index), nrow(b$patients))
  idx <- b$adm[b$adm$is_index & b$adm$source == "mimic_iv_demo", ]
  first <- b$adm[b$adm$source == "mimic_iv_demo", ] |> dplyr::group_by(patient_id) |> dplyr::slice_min(admittime, n = 1)
  expect_setequal(idx$adm_id, first$adm_id)
  expect_equal(b$adm$any_icu, as.integer(!is.na(f$ci$src_icu_id[match(b$adm$adm_id, f$ci$src_admission_id)])))
  expect_true(all(b$adm$hospital_mortality %in% c(0L, 1L)))
  expect_equal(levels(b$adm$age_group), .xp_age_levels)
  expect_equal(unname(b$meta$source_labels[c("eicu_demo", "mimic_iv_demo")]), c("eICU-CRD demo", "MIMIC-IV demo"))
  expect_true(all(b$checks$status %in% c("PASS", "N/A")), info = paste(b$checks$check[!b$checks$status %in% c("PASS", "N/A")], collapse = "; "))
  expect_true(all(c("age", "sex", "hospital_mortality", "any_icu", "source_label") %in% b$vars$var))
  expect_true("source_label" %in% b$groups && ".all" %in% b$groups)
  expect_false(any(c("patient_id", "adm_id", "ehr_id") %in% b$vars$var))
})

test_that("structural checks flag duplicated admissions", {
  f <- .xp_fixture()
  ci <- rbind(f$ci, f$ci[1, ])
  fl <- suppressMessages(readr::type_convert(tibble::as_tibble(f$fl), guess_integer = FALSE))
  b <- .xp_build(tibble::as_tibble(ci), fl, tibble::as_tibble(f$win))
  expect_equal(b$checks$status[grepl("^cohort_info has one row", b$checks$check)], "FAIL")
})

test_that("structural checks fail on an admission with two patients and are N/A without windows", {
  f <- .xp_fixture()
  fl <- suppressMessages(readr::type_convert(tibble::as_tibble(f$fl), guess_integer = FALSE))
  ci <- rbind(f$ci, f$ci[1, ]); ci$src_patient_id[nrow(ci)] <- "someone_else"
  b <- .xp_build(tibble::as_tibble(ci), fl, tibble::as_tibble(f$win))
  expect_equal(b$checks$status[b$checks$check == "every admission belongs to one patient"], "FAIL")
  b <- .xp_build(tibble::as_tibble(f$ci), fl, NULL)
  st <- stats::setNames(b$checks$status, b$checks$check)
  expect_equal(unname(st["visit windows found for every admission"]), "N/A")
  expect_equal(unname(st["ICU stays start within their admission (1 day tolerance)"]), "N/A")
})

test_that("the ICU check fails on stays starting outside the admission, not on late ends", {
  f <- .xp_fixture()
  fl <- suppressMessages(readr::type_convert(tibble::as_tibble(f$fl), guess_integer = FALSE))
  icu_status <- function(win) {
    b <- .xp_build(tibble::as_tibble(f$ci), fl, tibble::as_tibble(win))
    b$checks$status[b$checks$check == "ICU stays start within their admission (1 day tolerance)"]
  }
  stays <- which(!is.na(f$win$icu_id))
  late <- f$win; late$icu_out_min[stays] <- late$hosp_los_min[stays] + 3 * 1440
  expect_equal(icu_status(late), "PASS")
  early <- f$win; early$icu_in_min[stays] <- -3 * 1440
  expect_equal(icu_status(early), "FAIL")
})

test_that("a schema without cohort flags treats every admission as in the cohort", {
  f <- .xp_fixture()
  ci <- f$ci; ci$cohort_index <- NULL
  fl <- suppressMessages(readr::type_convert(tibble::as_tibble(f$fl), guess_integer = FALSE))
  b <- .xp_build(tibble::as_tibble(ci), fl, NULL)
  expect_equal(nrow(b$adm), nrow(ci))
  expect_true(all(is.na(b$ctx_adm$admittime)))
  expect_equal(b$ctx_adm$hosp_los_min, b$ctx_adm$hospital_los * 1440)
})

test_that("route and raw-lab comparisons", {
  a <- tibble::tibble(adm_id = c("1", "1", "2"), concept_raw = c("hr", "hr", "sbp"), value = c(80, 90, 120))
  r <- .xp_compare_routes(a, a)
  expect_true(all(r$match))
  r2 <- .xp_compare_routes(a, a[1:2, ])
  expect_equal(sum(!r2$match), 1L)
  ts <- tibble::tibble(adm_id = "1", concept = c("sodium", "sodium", "calcium_total"), value = c(140, 142, 9))
  raw <- tibble::tibble(adm_id = "1", concept = c("sodium", "sodium", "calcium", "glucose"), value = c(140, 142, 9.5, 100))
  cr <- .xp_compare_raw(ts, raw)
  expect_equal(cr$agreement[cr$concept == "sodium"], "agree (within 1%)")
  expect_equal(cr$agreement[cr$concept == "calcium"], "differs (> 1%)")
  expect_equal(cr$agreement[cr$concept == "glucose"], "raw only")
})

# ---- Shiny server logic ---------------------------------------------------------------------------------------
.xp_skip_if_no_shiny <- function() {
  for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival"))
    skip_if_not_installed(p)
}
render_all <- function(output, names) for (n in names) expect_false(is.null(output[[n]]), info = n)

test_that("the app builds and its page layout contains every page", {
  .xp_skip_if_no_shiny()
  app <- explorer_app(.xp_fixture_bundle(), live = FALSE)
  expect_s3_class(app, "shiny.appobj")
  html <- as.character(attr(app, "explorer_env")$ui(NULL))
  for (p in c("Overview", "Cohort Explorer", "Subject Explorer", "Concepts", "Cohorts", "Setup", "Package", "Data checks", "About"))
    expect_match(html, p, fixed = TRUE)
  expect_match(html, "rendehr-explorer/logo.png", fixed = TRUE)
  expect_no_match(html, "SCI-specific", fixed = TRUE)
  expect_error(explorer_app(list()), "bundle from explorer_data")
})

test_that("Overview, Cohort Explorer, Concepts and Data quality render offline", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  shiny::testServer(e$overview_server, {
    for (g in c("source_label", "sex")) for (u in c("adm", "pat")) {
      session$setInputs(group = g, unit = u, scale = "pct", los_log = TRUE)
      render_all(output, c("comp", "outc", "age", "pyramid", "losplot"))
    }
  })
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", f_source = e$SRC, f_sex = c("female", "male"), f_age = c(0, 100),
                      f_admission_type = c("emergency", "elective", "urgent"), f_any_icu = "any", mask = TRUE,
                      t1_group = "source_label", t1_vars = e$DEF$t1_vars, d_var = "age", d_group = "sex", d_geom = "box",
                      o_var = "hospital_mortality", o_group = "source_label", o_strat = "", s_group = ".all", s_days = 365,
                      s_ci = TRUE, x_row = "source_label", x_col = "hospital_discharge_location", x_pct = "row",
                      sc_x = "age", sc_y = "hospital_los", sc_col = "source_label", sc_log = FALSE)
    session$setInputs(t1_g1 = e$SRC_LABEL[[1]], t1_g2 = e$SRC_LABEL[[2]])
    render_all(output, c("nsel", "t1_smd_ui", "t1", "dplot", "dsum", "oplot", "otab", "splot", "stab", "xplot", "xtest",
                         "xtab", "scplot", "sccor", "data"))
    if ("year" %in% names(e$ADM) && any(!is.na(e$ADM$year))) {
      session$setInputs(tr_var = "hospital_mortality", tr_group = ".all", tr_scale = "pct")
      render_all(output, c("trplot", "trrate"))
    }
    expect_match(output$t1, "SMD eICU-CRD demo vs MIMIC-IV demo", fixed = TRUE)
    for (g in c("violin", "hist", "ecdf")) { session$setInputs(d_geom = g); render_all(output, "dplot") }
    # filters narrow the selection; the survival curve only uses sources with death times
    n_all <- nrow(df())
    session$setInputs(f_source = "mimic_iv_demo", f_any_icu = "1", unit = "pat", d_var = "race")
    expect_lt(nrow(df()), n_all)
    expect_true(all(df()$source == "mimic_iv_demo" & df()$any_icu == 1 & df()$is_index))
    render_all(output, c("t1", "dplot", "dsum"))
    expect_equal(unique(sdat()$source), "mimic_iv_demo")
  })
  shiny::testServer(e$concepts_server, { render_all(output, c("dict")) })
  shiny::testServer(e$quality_server, { render_all(output, c("checks")) })
})

test_that("Cohorts and Setup pages render and Setup creates projects and scripts", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  shiny::testServer(e$cohorts_server, { render_all(output, c("balance", "inv")) })
  shiny::testServer(e$setup_server, {
    form <- list(src = c("mimic_iv_demo", "eicu_demo"), engine = "postgres", db_name = "demo_db", cohort = "scid",
                 scope = "", raw_dir = "", icd_path = "", target = "rendehr", pg_host = "localhost", pg_port = "5432",
                 pg_user = "tester", pg_pass = "pw", pn_user = "", pn_pass = "", overwrite = TRUE)
    do.call(session$setInputs, c(form, list(base_dir = tempdir(), script = 1)))
    render_all(output, "ready")
    expect_match(paste(as.character(output$ready), collapse = " "), "Registered source families")
    expect_match(output$code, "Generated by ehr_setup_assistant", fixed = TRUE)
    # demo and full sources cannot share a project
    session$setInputs(src = c("mimic_iv_demo", "mimic_iv"), apply = 1)
    expect_match(paste(as.character(output$apply_result), collapse = " "), "either demo or full", fixed = TRUE)
    # Create project writes the configuration for every source without touching this session's config
    prev_cfg <- rendehR:::.ehr_env$config
    bd <- file.path(tempdir(), paste0("setup_apply_", as.integer(Sys.time())))
    session$setInputs(src = c("mimic_iv_demo", "eicu_demo"), base_dir = bd, apply = 2)
    expect_match(paste(as.character(output$apply_result), collapse = " "), "Project created", fixed = TRUE)
    expect_true(file.exists(file.path(bd, "config", "ehr_config.json")))
    expect_true(file.exists(file.path(bd, "config", "config_pass.json")))
    expect_identical(rendehR:::.ehr_env$config, prev_cfg)
    expect_equal(shiny::isolate(e$JOB_CTX$src), c("mimic_iv_demo", "eicu_demo"))
    expect_match(shiny::isolate(cohort_code()), "cohort_selection_options(cohort = \"scid\")", fixed = TRUE)
    # an explicit ID list belongs to one source and is written next to the configuration
    session$setInputs(src = "mimic_iv_demo", cohort = "ids", ids = "10000032\n10003400", key = "hadm_id", apply = 3)
    expect_match(paste(as.character(output$apply_result), collapse = " "), "Project created", fixed = TRUE)
    expect_true(file.exists(file.path(bd, "config", "cohort_ids.csv")))
    expect_match(shiny::isolate(cohort_code()), "direct_ids_path = ", fixed = TRUE)
    expect_match(shiny::isolate(cohort_code()), "key = \"hadm_id\"", fixed = TRUE)
  })
  shiny::testServer(e$pkg_server, {
    render_all(output, c("env", "copies", "families"))
  })
})

test_that("background jobs run a detached Rscript and report through their log", {
  .xp_skip_if_no_shiny()
  skip_if_not(file.exists(file.path(R.home("bin"), "Rscript")))
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  id <- shiny::isolate(e$job_start("unit smoke", "cat('hello from the job\\n')"))
  ok <- FALSE
  for (i in 1:80) {
    shiny::isolate(e$job_refresh(id))
    st <- shiny::isolate(e$.jobs$list[[id]]$status)
    if (!identical(st, "running")) { ok <- TRUE; break }
    Sys.sleep(0.5)
  }
  expect_true(ok)
  expect_identical(shiny::isolate(e$.jobs$list[[id]]$status), "done")
  expect_match(paste(readLines(shiny::isolate(e$.jobs$list[[id]]$log), warn = FALSE), collapse = " "), "hello from the job")
})

test_that("filters keep or drop missing values as specified", {
  b <- .xp_fixture_bundle()
  b$adm$sex[1:3] <- NA
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(b, live = FALSE), "explorer_env")
  f_keep <- list(var = "sex", type = "checkbox")
  f_drop <- list(var = "sex", type = "checkbox", keep_na = FALSE)
  f_lab <- list(var = "sex", type = "picker", na_label = "(missing)", keep_na = FALSE)
  expect_equal(nrow(e$apply_filter(b$adm, f_keep, "male")), sum(b$adm$sex %in% c("male", NA)))
  expect_equal(nrow(e$apply_filter(b$adm, f_drop, "male")), sum(b$adm$sex %in% "male"))
  expect_equal(nrow(e$apply_filter(b$adm, f_lab, "(missing)")), 3L)
  expect_equal(nrow(e$apply_filter(b$adm, list(var = "any_icu", type = "icu"), "0")), sum(b$adm$any_icu == 0))
})

test_that("an empty checkbox or picker keeps nothing and counts as an active filter", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  f <- list(var = "sex", type = "checkbox"); p <- list(var = "race", type = "picker")
  expect_equal(nrow(e$apply_filter(e$ADM, f, NULL)), 0L)
  expect_equal(nrow(e$apply_filter(e$ADM, p, NULL)), 0L)
  expect_equal(nrow(e$apply_filter(e$ADM, f, NULL, present = FALSE)), nrow(e$ADM))   # input not created yet
  expect_true(e$filter_active(f, NULL))
  expect_true(e$filter_active(p, NULL))
  expect_false(e$filter_active(f, NULL, present = FALSE))
  expect_false(e$filter_active(list(var = "any_icu", type = "icu"), NULL))
  expect_false(e$filter_active(f, c("female", "male")))
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, f_sex = c("female", "male"), t1_group = "source_label", t1_vars = e$DEF$t1_vars)
    expect_equal(n_active(), 0L)
    n0 <- nrow(df())
    session$setInputs(f_sex = NULL)
    expect_equal(nrow(df()), 0L)
    expect_equal(n_active(), 1L)
    expect_error(output$t1, "No admissions match the filters")
    session$setInputs(unit = "pat")
    expect_error(output$t1, "No patients match the filters")
    session$setInputs(f_sex = c("female", "male"), unit = "adm")
    expect_equal(nrow(df()), n0)
  })
})

test_that("missing values get their own filter level, and sliders drop them once moved", {
  .xp_skip_if_no_shiny()
  b <- .xp_fixture_bundle()
  b$adm$sex[1:3] <- NA; b$adm$age[4:5] <- NA
  e <- attr(explorer_app(b, live = FALSE), "explorer_env")
  f <- e$EX_FILTERS[[which(vapply(e$EX_FILTERS, `[[`, "", "var") == "sex")]]
  expect_equal(f$na_label, "(missing)")
  expect_true("(missing)" %in% e$filter_choices(f))
  expect_equal(nrow(e$apply_filter(b$adm, f, c("female", "male"))), nrow(b$adm) - 3L)
  expect_equal(nrow(e$apply_filter(b$adm, f, e$filter_default(f))), nrow(b$adm))
  a <- list(var = "age", type = "slider"); full <- e$filter_default(a)
  expect_equal(nrow(e$apply_filter(b$adm, a, full)), nrow(b$adm))
  expect_equal(nrow(e$apply_filter(b$adm, a, full + c(1, 0))), sum(b$adm$age >= full[1] + 1, na.rm = TRUE))
})

test_that("Kaplan-Meier uses index admissions and counts in-hospital deaths without a death time", {
  .xp_skip_if_no_shiny()
  b <- .xp_fixture_bundle()
  i <- which(b$adm$is_index & b$adm$source == "mimic_iv_demo")[1]
  b$adm$hospital_mortality[i] <- 1L; b$adm$time_to_death_from_admission[i] <- NA
  e <- attr(explorer_app(b, live = FALSE), "explorer_env")
  d <- e$km_data(e$ADM, ".all", 365)
  expect_true(all(d$is_index))
  expect_equal(anyDuplicated(paste(d$source, d$patient_id)), 0L)
  expect_true(all(d$source %in% e$SURV_SOURCES))
  r <- d[d$adm_id == b$adm$adm_id[i], ]
  expect_equal(r$status, 1L)
  expect_equal(r$time, max(b$adm$hospital_los[i], 0.01))
  expect_true(all(d$time > 0 & d$time <= 365))
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, s_group = ".all", s_days = 365, s_ci = TRUE)
    expect_true(nrow(df()) > nrow(sdat()))
    expect_true(all(sdat()$is_index))
    render_all(output, c("splot", "srisk", "stab", "scap"))
  })
})

test_that("Cohort Explorer outputs honour the mask, percent base and captions", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, t1_group = "source_label", t1_vars = e$DEF$t1_vars,
                      d_var = "race", d_group = "source_label", d_geom = "box", o_var = "hospital_mortality",
                      o_group = "source_label", o_strat = "source_label", x_row = "source_label",
                      x_col = "hospital_discharge_location", x_pct = "col", sc_x = "age", sc_y = "hospital_los",
                      sc_col = "sex", sc_log = TRUE, tr_var = "hospital_mortality", tr_group = "sex", tr_scale = "n")
    render_all(output, c("t1", "t1_note", "dplot", "dsum", "oplot", "ocap", "otab", "xplot", "xtest", "xtab", "scplot", "sccor"))
    expect_match(output$t1, "p (all groups)", fixed = TRUE)
    expect_equal(otab()$group, sort(unique(e$ADM$source_label)))     # stratum = group is ignored
    x <- xt(); expect_equal(as.vector(tapply(x$p, x$c, sum)), rep(1, length(unique(x$c))))
    if ("year" %in% names(e$ADM) && any(!is.na(e$ADM$year))) {
      render_all(output, c("trplot", "trrate", "trcap"))
      expect_match(paste(as.character(output$trcap), collapse = " "), "eICU-CRD demo", fixed = TRUE)
    }
  })
})

test_that("the cross-tab heatmap renders when the small-cell rule masks every cell", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(n_pat = 4), live = FALSE), "explorer_env")
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, x_row = "source_label", x_col = "hospital_discharge_location", x_pct = "row")
    expect_true(all(xt()$code > 0L))
    render_all(output, c("xplot", "xtab"))
  })
})

test_that("route and raw-lab comparisons catch shifted times, small value changes and count differences", {
  a <- tibble::tibble(adm_id = "1", concept_raw = c("hr", "hr"), value = c(80, 90), t_hadm = c(10, 20))
  expect_true(all(.xp_compare_routes(a, a)$match))
  b <- a; b$t_hadm <- b$t_hadm + 60
  expect_false(any(.xp_compare_routes(a, b)$match))
  b <- a; b$value[1] <- 80.001
  expect_false(any(.xp_compare_routes(a, b)$match))
  b <- a; b$value[1] <- 80 + 1e-9
  expect_true(all(.xp_compare_routes(a, b)$match))
  ts <- tibble::tibble(adm_id = "1", concept = "sodium", value = c(140, 140, 140))
  cr <- .xp_compare_raw(ts, ts[1:2, ])
  expect_equal(cr$agreement, "count differs")
  expect_equal(cr$n_ratio, 1.5)
})

test_that("patient time-series plots keep ICU shading and the ICU clock", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  ts <- tibble::tibble(adm_id = "1", t_hadm = c(-120, 60, 120, 600), t_icu = NA_real_,
                       concept = c("heart_rate", "heart_rate", "sbp", "temperature"), value = c(80, 90, 120, 37.2),
                       value_chr = c("80", "90", "120", "37.2"), unit = c("bpm", "NA", NA, "C"))
  icu <- tibble::tibble(icu_id = c("a", "b"), icu_in_min = c(30, 400), icu_out_min = c(300, 700))
  r <- e$ts_panels_plot(ts, e$VITAL_PANELS, icu = icu, axis = "hadm")
  sh <- r$p$x$layout$shapes
  expect_length(sh, 2)
  expect_equal(c(sh[[1]]$x0, sh[[1]]$x1), c(0.5, 5))
  expect_true("Temperature (°C)" %in% names(e$VITAL_PANELS))
  # ICU clock from the second stay: hospital-level values (before the stay) are kept
  r2 <- e$ts_panels_plot(ts, e$VITAL_PANELS, icu = icu, axis = "icu", icu0 = 400)
  expect_equal(r2$p$x$layout$shapes[[2]]$x0, 0)
  x <- suppressWarnings(as.numeric(unlist(lapply(r2$p$x$data, function(d) d$x))))
  expect_equal(min(x, na.rm = TRUE), (-120 - 400) / 60)
  pn <- e$VITAL_PANELS[c(1, 2, 4)]
  dur <- tibble::tibble(drug = c("x", "y"), start_min = c(0, 300), end_min = c(30, 200), duration_min = c(30, -100),
                        negative_duration = c(FALSE, TRUE))
  full <- e$ts_panels_plot(ts, pn, dur = dur)
  hov <- unlist(lapply(full$p$x$data, function(d) d$text))
  expect_true(any(grepl("negative duration", hov)))
  expect_true(any(grepl("5.0 to 3.3 h", hov, fixed = TRUE)))
  expect_equal(e$clean_unit(c("NA", NA, "mg")), c("", "", "mg"))
  g <- e$ts_grid_plot(ts, c("heart_rate", "sbp"))
  expect_equal(g$p$x$layout$xaxis$range, g$p$x$layout$xaxis2$range)
})

test_that("Distributions and Cross-tab mask the same cells as Table 1", {
  .xp_skip_if_no_shiny()
  b <- .xp_fixture_bundle()
  b$adm$race[b$adm$source == "eicu_demo"][1:4] <- "asian"      # a small cell in one group only
  e <- attr(explorer_app(b, live = FALSE), "explorer_env")
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, d_var = "race", d_group = "source_label",
                      x_row = "source_label", x_col = "race", x_pct = "row")
    vars <- e$VARS[e$VARS$var == "race", ]
    t1 <- char_table(dfv(), "source_label", vars, mask = TRUE)
    x <- dcat(); xx <- xt()
    for (g in unique(x$.gg)) for (v in as.character(unique(x$val))) {
      shown_t1 <- !t1[[g]][t1$level == v] %in% c("<10", "masked", "–")
      expect_identical(x$code[x$.gg == g & x$val == v] == 0L, shown_t1, info = paste(g, v))
      expect_identical(xx$code[xx$r == g & xx$c == v] == 0L, shown_t1, info = paste(g, v))
    }
  })
})

test_that("the concept builder writes the packaged contract, validates names and merges into the project file", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  # the writer round-trips the packaged file (header, order, literal NA and blank cells, quoted notes)
  tf <- tempfile(fileext = ".csv"); on.exit(unlink(tf), add = TRUE)
  e$cb_write(e$CB_BASE, tf)
  expect_identical(e$cb_read(tf), e$CB_BASE)
  expect_identical(readLines(tf, n = 1), readLines(e$CB_PKG_PATH, n = 1))
  expect_equal(nrow(suppressMessages(readr::read_csv(tf, show_col_types = FALSE))), nrow(e$CB_BASE))
  expect_match(e$cb_name_issue("", "heart_rate"), "Enter")
  expect_match(e$cb_name_issue("Heart_rate", "heart_rate"), "lower-case")
  expect_match(e$cb_name_issue("1hr", "heart_rate"), "lower-case")
  expect_match(e$cb_name_issue("heart_rate", "heart_rate"), "already exists")
  expect_null(e$cb_name_issue("heart_rate", "heart_rate", new = FALSE))
  expect_null(e$cb_name_issue("heart_rate_test", "heart_rate"))

  tmp <- tempfile("cbproj"); dir.create(file.path(tmp, "config"), recursive = TRUE); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  path <- file.path(tmp, "config", "user_concepts.csv")
  other <- e$CB_BASE[e$CB_BASE$concept_name == "hemoglobin", ][1, ]; other$concept_name <- "my_other"
  other$notes <- "kept, with a comma"
  e$cb_write(other, path)
  shiny::isolate({ e$JOB_CTX$base_dir <- tmp; e$JOB_CTX$config <- file.path(tmp, "config", "config_pass.json")
                   e$JOB_CTX$src <- c("mimic_iv_demo", "eicu_demo"); e$JOB_CTX$db_name <- "some_db" })
  on.exit(shiny::isolate({ e$JOB_CTX$base_dir <- NULL; e$JOB_CTX$config <- NULL; e$JOB_CTX$src <- NULL }), add = TRUE)
  pkg <- e$CB_BASE
  shiny::testServer(e$concept_builder_server, {
    # new concept
    session$setInputs(mode = "new", name = "heart_rate", desc = "heart rate test", category = "vitals", unit = "bpm", min = 0, max = 300)
    expect_match(name_issue(), "already exists")
    session$setInputs(name = "my_other")
    expect_match(name_issue(), "already exists")         # names in the project file count too
    session$setInputs(name = "heart_rate_test")
    expect_null(name_issue())
    expect_true(any(grepl("Switch on at least one source", issues())))
    session$setInputs(`s_mimic_iv-on` = TRUE)
    session$setInputs(`s_mimic_iv-tbl` = "chartevents")
    expect_true(any(grepl("Add at least one item ID", issues())))
    session$setInputs(`s_mimic_iv-ids` = c("220045", "x1"))
    expect_true(any(grepl("whole numbers", issues())))
    session$setInputs(`s_mimic_iv-ids` = "220045")
    session$setInputs(`s_eicu-on` = TRUE)
    session$setInputs(`s_eicu-method` = "rgx_exp_itm", `s_eicu-regex` = "heart (rate")
    expect_true(any(grepl("Invalid pattern", issues())))
    session$setInputs(`s_eicu-tbl` = "vitalperiodic")
    session$setInputs(`s_eicu-valcol` = "heartrate")
    expect_length(issues(), 0)
    d <- draft()
    expect_identical(names(d), names(pkg))
    expect_equal(nrow(d), 2)
    m <- d[d$source == "mimic_iv", ]
    expect_equal(unlist(m[c("concept_name", "match_value", "dict", "category", "unit", "min_value", "max_value", "table_name", "sub_var",
                            "val_var", "unit_var", "index_var", "itm_class", "check", "conversion", "method", "typ", "end_var")], use.names = FALSE),
                 c("heart_rate_test", "220045", "d_items", "vitals", "bpm", "0", "300", "chartevents", "itemid",
                   "valuenum", "valueuom", "charttime", "itm_itm", "NA", "none", "id", "ts", ""))
    ei <- d[d$source == "eicu", ]
    expect_equal(unlist(ei[c("table_name", "itm_class", "val_var", "match_value", "sub_var", "dict", "method")], use.names = FALSE),
                 c("vitalperiodic", "col_itm", "heartrate", "NA", "NA", "NA", "NA"))
    expect_equal(status(), "draft")
    session$setInputs(save = 1)                            # no overlap: saved directly, other rows kept
    f <- e$cb_read(path)
    expect_identical(names(f), names(pkg))
    expect_equal(f$concept_name, c("my_other", "heart_rate_test", "heart_rate_test"))
    expect_equal(f$notes[1], "kept, with a comma")
    expect_equal(status(), "saved")
    expect_true(any(grepl('run_concepts(src_name = "mimic_iv_demo"', build_lines(), fixed = TRUE)))
    expect_true(any(grepl('concept_name = "heart_rate_test"', build_lines(), fixed = TRUE)))
    expect_true(any(grepl(path, build_lines(), fixed = TRUE)))

    # edit the project concept: a definition change reaches every loaded row, saving replaces them
    session$setInputs(mode = "edit", pick = "heart_rate_test")
    expect_equal(nrow(draft()), 0)
    expect_equal(status(), "empty")
    session$setInputs(max = 250)
    expect_equal(nrow(draft()), 2)
    expect_setequal(e$cb_diff(loaded()$rows, per_src())$Field, "max_value")
    session$setInputs(save = 2)                            # overlap: confirmation first
    expect_equal(e$cb_read(path)$max_value[2], "300")
    session$setInputs(save_yes = 1)
    f <- e$cb_read(path)
    expect_equal(nrow(f), 3)
    expect_equal(f$max_value[f$concept_name == "heart_rate_test"], c("250", "250"))
    expect_equal(f$concept_name[1], "my_other")

    # edit a packaged concept: only the changed source is drafted, every other field as packaged
    session$setInputs(pick = "heart_rate")
    session$setInputs(desc = "heart rate", category = "vitals", unit = "bpm", min = 0, max = 300)  # what the inputs echo after loading
    expect_equal(nrow(draft()), 0)
    session$setInputs(`s_mimic_iv-ids` = c("220045", "220046"))
    d <- draft()
    p <- pkg[pkg$concept_name == "heart_rate" & pkg$source == "mimic_iv", ]
    expect_identical(names(d), names(pkg))
    expect_equal(nrow(d), 1)
    expect_equal(d$match_value, "220045|220046")
    expect_identical(unlist(d[setdiff(names(pkg), "match_value")]), unlist(p[setdiff(names(pkg), "match_value")]))
    dd <- e$cb_diff(loaded()$rows, per_src())
    expect_equal(dd$Field, "match_value")
    expect_equal(dd$After, "220045|220046")
  })
})

test_that("a link to a view keeps the page, filters and tab settings, never buttons or table state", {
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  keep <- c("nav", "ex-f_sex", "ex-f_age", "ex-unit", "ex-mask", "ex-tab", "ex-t1_vars", "ex-s_days", "ex-sc_log")
  drop <- c("ex-reset", "ex-open_subject", "ex-data_rows_all", "ex-data_state", "st-run", "sub-load", "help", "share",
            "cn-go", "pk-drop_yes")
  expect_true(all(grepl(e$BOOKMARK_KEEP, keep)))
  expect_false(any(grepl(e$BOOKMARK_KEEP, drop)))
})

test_that("every help link points at a section of the explorer article", {
  orig <- testthat::test_path("..", "..", "vignettes", "explorer.Rmd.orig")
  skip_if_not(file.exists(orig), "vignette sources are not in this tree")
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  heads <- sub("^#+ +", "", grep("^#{2,3} ", readLines(orig, warn = FALSE), value = TRUE))
  slugs <- gsub("[^a-z0-9]+", "-", tolower(heads))       # pkgdown section ids
  anchors <- unique(vapply(e$PAGE_HELP, `[`, "", 1))
  expect_true(all(anchors[nzchar(anchors)] %in% slugs), info = paste(setdiff(anchors, c("", slugs)), collapse = ", "))
})

test_that("trajectory statistics give n, median and IQR per group and bin, and mask small bins", {
  x <- data.frame(group = rep(c("a", "b"), c(24, 3)), bin = c(rep(0:1, 12), 0, 0, 1), value = c(1:24, 5, 7, 9))
  s <- traj_stats(x, 4)
  a0 <- x$value[x$group == "a" & x$bin == 0]
  r <- s[s$group == "a" & s$bin == 0, ]
  expect_equal(r$n, 12L)
  expect_equal(c(r$median, r$q25, r$q75), unname(c(median(a0), quantile(a0, c(.25, .75)))))
  expect_equal(s$start, c(0, 4, 0, 4))
  expect_equal(s$n[s$group == "b"], c(2L, 1L))
  m <- traj_stats(x, 4, mask = TRUE)
  expect_equal(m$group, c("a", "a"))
  expect_equal(attr(m, "dropped"), 2L)
})

test_that("the R code for a selection keeps the same rows as the filters", {
  .xp_skip_if_no_shiny()
  b <- .xp_fixture_bundle()
  b$adm$sex[1:3] <- NA; b$adm$age[4:5] <- NA; b$adm$race[6:7] <- NA
  e <- attr(explorer_app(b, live = FALSE), "explorer_env")
  fv <- function(v) e$EX_FILTERS[[which(vapply(e$EX_FILTERS, `[[`, "", "var") == v)]]
  same_rows <- function(fs, vals, unit = "adm") {
    d <- if (unit == "pat") e$ADM[e$ADM$is_index, ] else e$ADM
    for (i in seq_along(fs)) d <- e$apply_filter(d, fs[[i]], vals[[i]])
    env <- new.env()
    env$b <- list(adm = e$ADM)
    eval(parse(text = e$filter_code(fs, vals, unit)), env)
    expect_equal(paste(env$adm$source, env$adm$adm_id), paste(d$source, d$adm_id))
  }
  same_rows(list(), list())
  same_rows(list(), list(), "pat")
  same_rows(list(fv("sex")), list("male"))                       # "(missing)" not selected: missing dropped
  same_rows(list(fv("sex")), list(c("male", "(missing)")))
  same_rows(list(fv("sex")), list("(missing)"))
  same_rows(list(fv("sex")), list(NULL))                         # nothing selected keeps nothing
  same_rows(list(fv("age"), fv("source"), fv("any_icu")), list(c(30, 70), "mimic_iv_demo", "1"), "pat")
  same_rows(list(fv("any_icu"), fv("admission_type")), list("0", c("urgent", "elective")))
  same_rows(list(list(var = "race", type = "picker")), list("white"))                       # keep_na by default
  same_rows(list(list(var = "race", type = "picker", keep_na = FALSE)), list(c("white", "black")))
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, f_source = e$SRC, f_sex = "female")
    code <- code_text()
    expect_match(code, 'b <- rendehR::explorer_data("rendehr_admission")', fixed = TRUE)
    expect_match(code, 'sex %in% c("female")', fixed = TRUE)
    expect_no_match(code, "source %in%", fixed = TRUE)          # untouched filters are left out
    expect_no_match(code, "password = \"[^.]", perl = TRUE)
    session$setInputs(show_code = 1)
  })
})

test_that("Compare puts two selections side by side", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, f_source = "mimic_iv_demo", f_sex = c("female", "male"), cmp_vars = e$DEF$t1_vars)
    session$setInputs(cmp_set = 1)
    expect_equal(nrow(cmp_a()$d), sum(e$ADM$source == "mimic_iv_demo"))
    expect_match(cmp_a()$desc, "Source: MIMIC-IV demo", fixed = TRUE)
    session$setInputs(f_source = e$SRC, f_sex = "female")
    render_all(output, c("cmp_sizes", "cmp_t1", "cmp_note", "cmp_out"))
    expect_equal(nrow(cmp()$b), sum(e$ADM$sex %in% "female"))
    expect_match(output$cmp_t1, "SMD A vs B", fixed = TRUE)
    expect_match(output$cmp_out, "B: Rate (95% CI)", fixed = TRUE)
    expect_match(paste(as.character(output$cmp_sizes), collapse = " "), "In both", fixed = TRUE)
  })
})

test_that("Trajectories summarise a cached query over the filtered selection", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  e$LIVE <- TRUE
  calls <- 0
  e$trajectory_query <- function(cn, clock, hz, bw) {
    calls <<- calls + 1
    a <- e$ADM
    data.frame(source = rep(a$source, each = 2), adm_id = rep(a$adm_id, each = 2), bin = rep(0:1, nrow(a)),
               value = seq_len(2 * nrow(a)))
  }
  shiny::testServer(e$explorer_server, args = list(goto_subject = function(...) NULL), {
    session$setInputs(unit = "adm", mask = TRUE, tj_cn = "heart_rate", tj_clock = "hadm", tj_hz = "72", tj_bw = "4",
                      tj_group = "source_label")
    session$setInputs(tj_go = 1)
    render_all(output, c("tjplot", "tjnote", "tjtab"))
    s <- tj()$s
    expect_equal(sort(unique(s$group)), sort(unique(e$ADM$source_label)))
    expect_equal(sum(s$n[s$bin == 0]), nrow(df()))
    expect_equal(s$start[s$bin == 1], c(4, 4))
    session$setInputs(unit = "pat", f_source = "mimic_iv_demo", tj_go = 2)
    expect_equal(calls, 1)                                        # same query: served from the cache
    expect_equal(sum(tj()$s$n[tj()$s$bin == 0]), sum(e$ADM$is_index & e$ADM$source == "mimic_iv_demo"))
    session$setInputs(tj_bw = "6", tj_go = 3)
    expect_equal(calls, 2)
  })
})

test_that("concept availability and quality are shaped and masked by the small-cell rule", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  den <- tibble::tibble(source = c("a", "b"), den = c(100, 8))
  av <- e$availability_shape(data.frame(source = c("a", "a", "b", "z"), concept_name = c("hr", "sbp", "hr", "hr"), n = c(50, 5, 8, 3)), den)
  expect_equal(nrow(av), 4)                                     # hr/sbp x a/b; the unknown source z is dropped
  expect_equal(av$share[av$concept_name == "hr" & av$source == "a"], 0.5)
  expect_equal(av$n_lab[av$concept_name == "sbp" & av$source == "a"], "<10")
  expect_true(is.na(av$share[av$concept_name == "sbp" & av$source == "a"]))
  expect_equal(av$n[av$concept_name == "sbp" & av$source == "b"], 0)
  expect_true(all(is.na(av$share[av$source == "b"])))          # denominator below 10
  b <- tibble::tibble(concept_name = c("hr", "na_only"), lo = c(0, NA), hi = c(250, 5))
  q <- e$quality_shape(data.frame(source = "a", concept_name = c("hr", "sbp", "na_only", "tiny"), n_rows = c(200, 100, 50, 5),
                                  n_value = c(180, 100, 50, 5), n_out = c(9, 0, 1, 0), units = c("bpm|NA||bpm", "mmHg", "x|y", NA)), b)
  expect_equal(q$concept_name[1], "hr")                         # sorted by share outside the range
  expect_equal(q$units[q$concept_name == "hr"], "bpm")
  expect_equal(q$p_out[q$concept_name == "hr"], 9 / 180)
  expect_equal(q$p_missing[q$concept_name == "hr"], 0.1)
  expect_equal(q$status[q$concept_name == "hr"], "CHECK")
  expect_equal(q$status[q$concept_name == "na_only"], "CHECK")  # a one-sided bound still checks; two units
  expect_equal(q$status[q$concept_name == "sbp"], "N/A")        # no bound in the configuration
  expect_true(is.na(q$p_missing[q$concept_name == "tiny"]))
  rb <- e$range_bounds()
  expect_true(all(c("concept_name", "lo", "hi") %in% names(rb)) && "heart_rate" %in% rb$concept_name)
})

test_that("an exposure counts on each day its interval overlaps", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  on <- function(s, en) sapply(0:2, function(d) e$exposed_on_day(s, en, d))
  expect_equal(on(100, 1440), c(TRUE, FALSE, FALSE))            # ends exactly at the day boundary
  expect_equal(on(1440, 1440), c(FALSE, TRUE, FALSE))           # a point at the boundary belongs to the next day
  expect_equal(on(-300, 1500), c(TRUE, TRUE, FALSE))            # starts before admission
  expect_equal(on(3000, 2000), c(FALSE, FALSE, TRUE))           # negative duration: a point at the start
  expect_equal(on(500, NA), c(TRUE, FALSE, FALSE))              # missing end: a point at the start
  expect_equal(on(NA, 500), c(FALSE, FALSE, FALSE))
  x <- e$exposure_shape(data.frame(source = "a", drug = "abx", day = c(0, 1), n = c(40, 4)), c("abx", "vaso"), 3,
                        tibble::tibble(source = "a", den = 100))
  expect_equal(nrow(x), 6)
  expect_equal(x$share[x$drug == "abx"], c(0.4, NA, 0))
})

test_that("the Subject Explorer filters by age and admission type", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  P <- e$PATIENTS
  expect_equal(nrow(e$filter_patients(P, age = e$AGE_RANGE, atype = e$ADM_TYPES)), nrow(P))
  y <- e$filter_patients(P, age = c(18, 40))
  expect_true(nrow(y) > 0 && nrow(y) < nrow(P) && all(y$age >= 18 & y$age <= 40))
  el <- e$filter_patients(P, atype = "elective")
  ok <- e$CTX_ADM[e$CTX_ADM$in_cohort & e$CTX_ADM$admission_type == "elective", ]
  expect_setequal(paste(el$source, el$patient_id), unique(paste(ok$source, ok$patient_id)))
  shiny::testServer(e$subject_server, args = list(request = shiny::reactiveVal(NULL)), {
    session$setInputs(f_src = "eicu_demo", f_icu = FALSE, f_multi = FALSE, f_died = FALSE, f_age = c(50, 90), f_atype = "urgent")
    p <- cand()
    expect_true(nrow(p) > 0 && all(p$source == "eicu_demo" & p$age >= 50 & p$age <= 90))
    session$setInputs(random = 1)
    k <- strsplit(cur()$key, "::", fixed = TRUE)[[1]]
    expect_true(k[2] %in% p$patient_id)
  })
})

test_that("concept definitions come from the shipped configuration of the source family", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  d <- e$concept_definition("heart_rate", "mimic_iv_demo")
  expect_gt(nrow(d$config), 0)
  expect_true(all(c("table_name", "match_value", "unit", "conversion") %in% names(d$config)))
  expect_equal(nrow(e$concept_definition("no_such_concept", "eicu_demo")$config), 0)
  cfg <- e$concept_config()
  expect_equal(nrow(d$config), sum(cfg$concept_name == "heart_rate" & cfg$source == "mimic_iv"))
})

test_that("time-series traces carry their concept and draw a comparison patient dashed on the same panel", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  ts <- tibble::tibble(adm_id = "1", t_hadm = c(60, 120, 60), t_icu = NA_real_, concept = c("heart_rate", "heart_rate", "sbp"),
                       value = c(80, 90, 120), value_chr = c("80", "90", "120"), unit = "x")
  r <- e$ts_panels_plot(ts, e$VITAL_PANELS)
  cmp <- ts %>% dplyr::mutate(value = value + 1)
  p <- e$ts_decorate(r$p, c("heart_rate", "sbp"), cmp, c("A1", "B2"))
  hr <- Filter(function(t) identical(t$name, "heart_rate"), p$x$data)[[1]]
  expect_equal(unclass(hr$customdata), c("heart_rate", "heart_rate"))
  b <- Filter(function(t) identical(t$legendgroup, "cmp"), p$x$data)
  expect_length(b, 2)                                    # one dashed trace per drawn concept
  expect_equal(vapply(b, function(t) isTRUE(t$showlegend), logical(1)), c(TRUE, FALSE))   # one legend entry
  expect_equal(b[[1]]$name, "B2 (dashed)")
  expect_equal(b[[1]]$line$dash, "dash")
  expect_equal(c(b[[1]]$xaxis, b[[1]]$yaxis), c(hr$xaxis, hr$yaxis))
  expect_equal(length(e$ts_decorate(r$p, c("heart_rate", "sbp"))$x$data), length(r$p$x$data))
})

test_that("procedure times are placed on the hospital clock", {
  .xp_skip_if_no_shiny()
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  # procedures with times: eICU offsets from ICU admission, MIMIC-IV dates (not before admission)
  icus <- tibble::tibble(icu_id = "i1", icu_in_min = 120)
  adms <- tibble::tibble(adm_id = c("a1", "a2"), admittime = as.POSIXct(c("2150-01-01 10:00", "2150-02-01 08:00"), tz = "UTC"))
  pt <- e$procedure_times(tibble::tibble(adm_id = "a1", icu_id = "i1", offset_min = 60, title = "t"), adms, icus)
  expect_equal(pt$t_min, 180)
  pt <- e$procedure_times(tibble::tibble(adm_id = c("a1", "a2"), code = "x", date = as.Date(c("2150-01-01", "2150-02-03")), title = NA), adms, icus)
  expect_equal(pt$t_min, c(0, 2 * 1440 - 480))
  expect_equal(nrow(e$procedure_times(tibble::tibble(adm_id = "a1", code = "x", date = as.Date(NA)), adms, icus)), 0)
})

test_that("plots save as SVG from the toolbar and hide the plotly logo", {
  e <- attr(explorer_app(.xp_fixture_bundle(), live = FALSE), "explorer_env")
  cfg <- e$plot_config(e$blank_plot())$x$config
  expect_equal(cfg$toImageButtonOptions$format, "svg")
  expect_false(cfg$displaylogo)
})

# Cohort explorer against a real merged schema (read-only). Set
# EHR_TEST_EXPLORER_DB and EHR_TEST_EXPLORER_SCHEMA (plus PG* credentials) to a
# database built by the pipeline with merge_sources() at admission level.

.xp_live_bundle <- local({
  cache <- NULL
  function() {
    db <- Sys.getenv("EHR_TEST_EXPLORER_DB", "")
    schema <- Sys.getenv("EHR_TEST_EXPLORER_SCHEMA", "")
    skip_if(!nzchar(db) || !nzchar(schema), "set EHR_TEST_EXPLORER_DB and EHR_TEST_EXPLORER_SCHEMA to run the live explorer tests")
    skip_if_not_installed("RPostgres")
    if (is.null(cache)) {
      creds <- list(postgres = list(host = Sys.getenv("PGHOST", "localhost"), port = as.integer(Sys.getenv("PGPORT", "5432")),
                                    user = Sys.getenv("PGUSER", "postgres"), password = Sys.getenv("PGPASSWORD", "")))
      suppressMessages(ehr_connect(db, creds = creds))
      raw <- Sys.getenv("EHR_TEST_EXPLORER_RAW_ROOT", "")
      dirs <- NULL
      if (nzchar(raw)) dirs <- stats::setNames(file.path(raw, list.files(raw)), list.files(raw))
      cache <<- explorer_data(schema, raw_dirs = dirs)
    }
    cache
  }
})

test_that("explorer_data reads the merged schema and passes its structural checks", {
  b <- .xp_live_bundle()
  expect_gt(nrow(b$adm), 0)
  expect_true(all(b$checks$status %in% c("PASS", "N/A")), info = paste(b$checks$check[!b$checks$status %in% c("PASS", "N/A")], collapse = "; "))
  expect_gt(nrow(b$concepts), 0)
  expect_true(all(b$meta$sources %in% b$adm$source))
  expect_error(explorer_data("no_such_schema_xyz"), "not found")
})

test_that("explorer_data caches the bundle privately and reuses it while the database is unchanged", {
  b <- .xp_live_bundle()
  dir <- withr::local_tempdir()
  first <- explorer_data(b$meta$schema, cache_dir = dir)
  f <- list.files(dir, full.names = TRUE)
  expect_length(f, 1)
  expect_equal(as.character(file.info(f)$mode), "600")
  expect_identical(explorer_data(b$meta$schema, cache_dir = dir), first)
  expect_length(list.files(dir), 1)
  expect_identical(first$adm, b$adm)
  path_of <- function(schema) .with_connection(NULL, function(con) .xp_cache_file(con, dir, schema, NULL, NULL))
  expect_identical(path_of(b$meta$schema), f)
  expect_false(identical(path_of("another_schema"), f))
})

test_that("per-source and merged reads of a patient are identical, on one clock", {
  b <- .xp_live_bundle()
  set.seed(11)
  for (s in b$meta$sources) {
    pp <- b$patients[b$patients$source == s & b$patients$n_icu > 0, ]
    for (p in pp$patient_id[sample.int(nrow(pp), min(2, nrow(pp)))]) {
      a <- explorer_patient_ts(b, s, p, "source")
      m <- explorer_patient_ts(b, s, p, "merged")
      expect_gt(nrow(a), 0, label = paste(s, p))
      expect_true(all(.xp_compare_routes(a, m)$match), info = paste(s, p))
      expect_true(all(a$adm_id %in% b$ctx_adm$adm_id[b$ctx_adm$source == s & b$ctx_adm$patient_id == p]))
      # t_hadm - t_icu equals the ICU start relative to hospital admission
      j <- merge(a[!is.na(a$t_icu), ], b$icu[b$icu$source == s, c("icu_id", "icu_in_min")], by = "icu_id")
      if (nrow(j)) expect_gt(mean(abs((j$t_hadm - j$t_icu) - j$icu_in_min) <= 1), 0.99, label = paste(s, p))
      d <- explorer_patient_dur(b, s, p)
      expect_true(all(d$end_min >= d$start_min))
      expect_true(all(d$adm_id %in% b$ctx_adm$adm_id[b$ctx_adm$source == s]))
      expect_gt(nrow(.xp_patient_flat(b, s, p)), 0)
    }
  }
})

test_that("the Subject Explorer loads a patient from every source and verifies it", {
  b <- .xp_live_bundle()
  for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival")) skip_if_not_installed(p)
  e <- attr(explorer_app(b, live = TRUE), "explorer_env")
  req <- shiny::reactiveVal(NULL)
  picks <- do.call(rbind, lapply(e$SRC, function(s) { x <- e$PATIENTS[e$PATIENTS$source == s & e$PATIENTS$n_icu > 0, ]; x[1, ] }))
  shiny::testServer(e$subject_server, args = list(request = req), {
    for (i in seq_len(nrow(picks))) {
      session$setInputs(f_src = "all", f_icu = FALSE, f_multi = FALSE, f_died = FALSE, mode = "source",
                        pid = paste(picks$source[i], picks$patient_id[i], sep = "::"))
      session$setInputs(load = i)
      s <- subj(); expect_gt(nrow(s$ts), 0); expect_equal(s$source, picks$source[i])
      session$setInputs(adm = s$info$index_adm, axis = "hadm", show_meds = TRUE, show_icu = TRUE, grid_cols = 3, rp_speed = "1",
                        dt_scope = "adm", tl_mode = "sequence")
      a <- TS1()[!is.na(TS1()$value), ]
      session$setInputs(concepts = head(unique(a$concept), 8), win = c(-24, 240), grid_c = head(unique(a$concept), 9))
      for (n in c("header", "load_info", "adm_ui", "timeline", "adm_table", "adm_detail", "concept_ui", "win_ui", "ts_plot",
                  "gridc_ui", "grid", "dx_tab", "ts_tab", "flat_tab", "csum_tab", "v_struct"))
        expect_false(is.null(output[[n]]), info = paste(picks$source[i], n))
      session$setInputs(v_merged = i)
      expect_match(output$v_merged_sum$html, "PASS")
    }
    r <- e$ADM[e$ADM$any_icu == 1, ][1, ]
    req(list(source = r$source, patient_id = r$patient_id, adm_id = r$adm_id, t = Sys.time())); session$flushReact()
    expect_equal(subj()$patient_id, r$patient_id)
  })
  shiny::testServer(e$quality_server, {
    session$setInputs(nrand = 1, run_live = 1, run_rows = 1)
    expect_true(all(live()$status %in% c("PASS", "N/A")), info = paste(live()$detail, collapse = "; "))
    expect_equal(nrow(live()), length(e$SRC))
    expect_false(is.null(output$rows))
  })
  shiny::testServer(e$concepts_server, {
    cn <- intersect(c("heart_rate", "hemoglobin"), e$CONCEPT_NAMES)[1]
    session$setInputs(cn = cn, src = e$SRC, logx = FALSE, live_src = e$SRC[1])
    session$setInputs(go = 1, live = 1)
    for (n in c("inv_type", "inv", "cdist", "csum", "meds", "medplot")) expect_false(is.null(output[[n]]), info = n)
    expect_match(output$live_out$html, "row_count")
  })
})

test_that("Subject Explorer picks an admission with data, clears verify results and uses the chosen ICU stay as time zero", {
  b <- .xp_live_bundle()
  for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival")) skip_if_not_installed(p)
  e <- attr(explorer_app(b, live = TRUE), "explorer_env")
  pp <- e$PATIENTS[e$PATIENTS$n_icu > 0, ]
  keys <- paste(pp$source, pp$patient_id, sep = "::")
  shiny::testServer(e$subject_server, args = list(request = shiny::reactiveVal(NULL)), {
    session$setInputs(f_src = "all", f_icu = FALSE, f_multi = FALSE, f_died = FALSE, mode = "source", pid = keys[1])
    session$setInputs(load = 1)
    s <- subj(); d <- default_adm(s)
    if (any(!is.na(s$ts$value))) expect_gt(sum(s$ts$adm_id == d & !is.na(s$ts$value)), 0)
    st <- s$icus[order(s$icus$icu_in_min), ][1, ]
    session$setInputs(adm = st$adm_id, axis = "icu", show_meds = TRUE, show_icu = TRUE, win = c(-1e4, 1e5),
                      concepts = head(unique(s$ts$concept[!is.na(s$ts$value) & s$ts$adm_id == st$adm_id]), 6))
    expect_equal(icu0(), st$icu_in_min)
    expect_false(is.null(output$icu_pick))
    session$setInputs(v_merged = 1)
    expect_match(output$v_merged_sum$html, "PASS")
    session$setInputs(pid = keys[2], load = 2)
    expect_null(vm())
    session$setInputs(mode = "merged")
    expect_equal(subj()$mode, "merged")
    expect_equal(subj()$patient_id, pp$patient_id[2])
  })
  # a demo patient whose index admission has no time series opens on an admission with values
  k <- "mimic_iv_demo::10002930"
  skip_if_not(k %in% keys)
  shiny::testServer(e$subject_server, args = list(request = shiny::reactiveVal(NULL)), {
    session$setInputs(f_src = "all", f_icu = FALSE, f_multi = FALSE, f_died = FALSE, mode = "source", pid = k)
    session$setInputs(load = 1)
    s <- subj(); d <- default_adm(s)
    expect_gt(sum(s$ts$adm_id == d & !is.na(s$ts$value)), 0)
    session$setInputs(adm = s$info$index_adm)
    expect_match(paste(as.character(output$ts_hint$html), collapse = ""), "Switch to that admission")
  })
})

test_that("the concept builder searches item dictionaries and previews values read-only", {
  b <- .xp_live_bundle()
  for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival")) skip_if_not_installed(p)
  e <- attr(explorer_app(b, live = TRUE), "explorer_env")
  skip_if_not("mimic_iv" %in% names(e$CB_FAMS))
  src <- e$CB_FAMS[["mimic_iv"]]
  expect_equal(e$cb_db(function(con) DBI::dbGetQuery(con, "SHOW transaction_read_only")[[1]]), "on")
  d <- e$cb_search_dict(src, "mimic_iv", NULL, e$cb_terms("heart rate"))
  expect_true("220045" %in% d$id)
  expect_match(d$label[d$id == "220045"][1], "Heart Rate")
  expect_true("220045" %in% e$cb_search_dict(src, "mimic_iv", "d_items", e$cb_terms("hr"))$id)
  r <- e$cb_new_row(e$CB_BASE, "mimic_iv", "chartevents")
  r$concept_name <- "heart_rate_test"; r$match_value <- "220045"; r$unit <- "bpm"; r$min_value <- "0"; r$max_value <- "300"
  pv <- e$cb_preview(src, r)
  expect_gt(pv$s$n_rows, 0); expect_gt(pv$s$n_adm, 0); expect_gt(pv$s$n_pat, 0)
  expect_lte(pv$s$n_pat, pv$s$n_adm)
  expect_true("bpm" %in% pv$units$unit)
  expect_true(all(pv$items$item == "220045"))
  expect_gt(sum(pv$hist$n), 0)
  if ("eicu" %in% names(e$CB_FAMS)) {
    r2 <- e$cb_new_row(e$CB_BASE, "eicu", "nursecharting")
    r2$concept_name <- "x"; r2$itm_class <- "rgx_itm"; r2$sub_var <- "nursingchartcelltypevallabel"; r2$match_value <- "Heart Rate|no such name"
    m <- e$cb_name_matches(e$CB_FAMS[["eicu"]], r2)
    expect_gt(m$n[m$name == "heart rate"], 0); expect_equal(m$n[m$name == "no such name"], 0)
  }
  shiny::testServer(e$concept_builder_server, {
    session$setInputs(mode = "new", name = "heart_rate_test", unit = "bpm", min = 0, max = 300)
    session$setInputs(`s_mimic_iv-on` = TRUE)
    session$setInputs(`s_mimic_iv-tbl` = "chartevents")
    session$setInputs(`s_mimic_iv-q` = "heart rate", `s_mimic_iv-syn` = TRUE, `s_mimic_iv-go` = 1)
    expect_false(is.null(output[["s_mimic_iv-hits"]]))
    session$setInputs(`s_mimic_iv-ids` = "220045")
    expect_length(issues(), 0)
    session$setInputs(`s_mimic_iv-pv_go` = 1)
    expect_match(paste(as.character(output[["s_mimic_iv-pv_out"]]$html), collapse = ""), "Rows matched")
    expect_false(is.null(output[["s_mimic_iv-pv_hist"]]))
  })
})

test_that("trajectory query returns one mean per cohort admission and bin within the horizon", {
  b <- .xp_live_bundle()
  x <- .with_connection(NULL, function(con) .xp_trajectory(con, b$meta$schema, "heart_rate", "hadm", 24, 6))
  expect_gt(nrow(x), 0)
  expect_equal(anyDuplicated(x[c("source", "adm_id", "bin")]), 0L)
  expect_true(all(x$bin %in% 0:3))
  expect_true(all(paste(x$source, x$adm_id) %in% paste(b$adm$source, b$adm$adm_id)))
  i <- .with_connection(NULL, function(con) .xp_trajectory(con, b$meta$schema, "heart_rate", "icu", 24, 6))
  expect_true(all(i$bin %in% 0:3))
})

test_that("concept availability and quality compute in the database", {
  b <- .xp_live_bundle()
  for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival")) skip_if_not_installed(p)
  e <- attr(explorer_app(b, live = TRUE), "explorer_env")
  shiny::testServer(e$concepts_server, {
    session$setInputs(av_win = "1440", av_go = 1, q_go = 1)
    a <- av(); expect_true(is.data.frame(a) && nrow(a) > 0)
    expect_true(all(a$share[!is.na(a$share)] <= 1))
    q <- qr(); expect_true(is.data.frame(q) && nrow(q) > 0)
    expect_true(all(q$status %in% c("PASS", "CHECK", "N/A")))
    for (n in c("av_plot_ui", "av_plot", "av_tab", "q_tab")) expect_false(is.null(output[[n]]), info = n)
  })
})

test_that("exposure by day is computed in the database with the same day rule as exposed_on_day()", {
  b <- .xp_live_bundle()
  for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival")) skip_if_not_installed(p)
  e <- attr(explorer_app(b, live = TRUE), "explorer_env")
  shiny::testServer(e$concepts_server, {
    session$setInputs(ed_drugs = head(e$DRUGS, 3), ed_days = "7", ed_go = 1)
    x <- ed(); expect_equal(nrow(x), 3 * 7 * length(e$SRC))
    expect_false(is.null(output$ed_plot))
  })
  # the SQL day rule agrees with exposed_on_day() on the raw intervals of one drug
  d <- e$DRUGS[1]
  raw <- .with_connection(NULL, function(con) DBI::dbGetQuery(con, sprintf(
    "SELECT d.source_data AS source, d.src_admission_id AS adm_id, d.end_offset_min - COALESCE(d.duration_min, 0) AS s,
            d.end_offset_min AS e FROM %s d JOIN %s c ON c.source_data = d.source_data AND c.src_admission_id = d.src_admission_id
     WHERE d.concept_name IN ($1, $2)", .xp_quote_table(con, b$meta$schema, "dur"), e$cohort_sql(con)), params = list(d, paste0(d, "_exp"))))
  ref <- do.call(rbind, lapply(0:6, function(k) {
    on <- raw[e$exposed_on_day(raw$s, raw$e, k), ]
    if (!nrow(on)) return(NULL)
    stats::aggregate(adm_id ~ source, on, function(v) length(unique(v))) |> transform(day = k)
  }))
  sq <- e$exposure_by_day(d, 7)
  m <- merge(ref, sq, by = c("source", "day"))
  expect_equal(nrow(m), nrow(ref))
  expect_equal(as.numeric(m$n), as.numeric(m$adm_id))
})

test_that("the Subject Explorer draws a comparison patient and marks procedures on the timeline", {
  b <- .xp_live_bundle()
  for (p in c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival")) skip_if_not_installed(p)
  e <- attr(explorer_app(b, live = TRUE), "explorer_env")
  pp <- e$PATIENTS[e$PATIENTS$source == "eicu_demo" & e$PATIENTS$n_icu > 0, ]
  skip_if(nrow(pp) < 2)
  keys <- paste(pp$source, pp$patient_id, sep = "::")
  shiny::testServer(e$subject_server, args = list(request = shiny::reactiveVal(NULL)), {
    session$setInputs(f_src = "all", f_icu = FALSE, f_multi = FALSE, f_died = FALSE, mode = "source", pid = keys[1], tl_mode = "sequence")
    session$setInputs(load = 1)
    s <- subj(); session$setInputs(adm = default_adm(s), axis = "hadm", show_meds = FALSE, show_icu = TRUE, win = c(-24, 240),
                                   concepts = c("heart_rate", "resp_rate"), cmp = keys[2])
    expect_equal(cmp_ts()$patient_id, pp$patient_id[2])
    expect_false(is.null(output$ts_plot))
    expect_false(is.null(output$ts_prov))
    expect_false(is.null(output$timeline))
    if (nrow(e$procedure_times(s$codes$px, s$adms, s$icus))) expect_match(output$timeline, "Procedure", fixed = TRUE)
  })
})

# ---- Subject Explorer: one patient, all admissions, live time series, verification -------------------------------
AGE_RANGE <- if (any(!is.na(PATIENTS$age))) c(floor(min(PATIENTS$age, na.rm = TRUE)), ceiling(max(PATIENTS$age, na.rm = TRUE)))
ADM_TYPES <- if ("admission_type" %in% names(CTX_ADM)) sort(unique(stats::na.omit(CTX_ADM$admission_type[CTX_ADM$in_cohort %in% TRUE])))
# Patients matching the sidebar filters. Age (at the index admission) and admission type (of any cohort admission)
# only filter when narrowed, so patients with no age or type stay listed by default.
filter_patients <- function(p, src = "all", grp = NULL, icu = FALSE, multi = FALSE, died = FALSE, age = NULL, atype = NULL) {
  if (!is.null(src) && src != "all") p <- p[p$source == src, ]
  if (!is.null(INDEX_GROUP) && !is.null(grp)) p <- p[p[[INDEX_GROUP]] %in% grp, ]
  if (isTRUE(icu)) p <- p[p$n_icu > 0, ]
  if (isTRUE(multi)) p <- p[p$n_adm > 1, ]
  if (isTRUE(died)) p <- p[p$any_death %in% TRUE, ]
  if (length(age) == 2 && !identical(as.numeric(age), as.numeric(AGE_RANGE))) p <- p[!is.na(p$age) & p$age >= age[1] & p$age <= age[2], ]
  if (!is.null(atype) && !setequal(atype, ADM_TYPES)) {
    a <- CTX_ADM[CTX_ADM$in_cohort %in% TRUE & CTX_ADM$admission_type %in% atype, ]
    p <- p[paste(p$source, p$patient_id) %in% paste(a$source, a$patient_id), ]
  }
  p
}

# Definition of a concept for one source: the rows of the shipped concept configuration for its source family and the
# registered provenance (D$concepts), when any.
concept_config <- local({ cfg <- NULL
  function() { if (is.null(cfg)) cfg <<- readr::read_csv(ehr_config_path("rendehr_concepts_config.csv"), col_types = readr::cols(.default = "c")); cfg } })
concept_definition <- function(cn, source, cfg = concept_config()) {
  fam <- src_family(source)
  o <- D$concepts %||% tibble(concept_name = character(), source = character())
  list(config = cfg[cfg$concept_name %in% cn & cfg$source %in% fam, intersect(c("table_name", "match_value", "unit", "conversion"), names(cfg))],
       origin = o[o$concept_name %in% cn & o$source %in% source, intersect(c("schema", "origin_name", "origin_url", "origin_ref"), names(o))])
}

# Concept traces of a built ts_panels_plot() carry their concept as customdata (read back on click). With a comparison
# patient, that patient's values of each drawn concept are added dashed on the same panel (hours since admission).
ts_decorate <- function(p, concepts, cmp = NULL, labels = NULL) {
  if (is.null(p$x$data)) return(p)
  extra <- list()
  for (i in seq_along(p$x$data)) {
    tr <- p$x$data[[i]]; cn <- tr$name %||% ""
    if (!cn %in% concepts) next
    p$x$data[[i]]$customdata <- I(rep(cn, length(tr$x)))
    if (is.null(cmp)) next
    d <- cmp[cmp$concept == cn & !is.na(cmp$value), ]; d <- d[order(d$t_hadm), ]
    if (!nrow(d)) next
    # one legend entry for the whole comparison patient; hover names the concept
    extra[[length(extra) + 1]] <- list(type = "scattergl", mode = "lines+markers", x = I(d$t_hadm / 60), y = I(d$value),
      xaxis = tr$xaxis, yaxis = tr$yaxis, name = sprintf("%s (dashed)", labels[2]), legendgroup = "cmp",
      showlegend = !length(extra),
      line = list(dash = "dash", width = 1, color = tr$marker$color), marker = list(size = 3, color = tr$marker$color),
      hoverinfo = "text", text = I(sprintf("%s (%s): %s %s<br>%.1f h since admission", cn, labels[2], d$value_chr, clean_unit(d$unit), d$t_hadm / 60)))
  }
  p$x$data <- c(p$x$data, extra)
  p
}

# Procedures with a time, in minutes since hospital admission: eICU treatment offsets (minutes from ICU admission) and
# MIMIC-IV chart dates (day precision, placed at the start of the day, not before admission). MIMIC-III has no times.
procedure_times <- function(px, adms, icus) {
  px <- px %||% tibble()
  if (!nrow(px) || !"adm_id" %in% names(px)) return(tibble(adm_id = character(), t_min = numeric(), title = character()))
  t <- rep(NA_real_, nrow(px))
  if (all(c("offset_min", "icu_id") %in% names(px))) t <- px$offset_min + icus$icu_in_min[match(px$icu_id, icus$icu_id)]
  if ("date" %in% names(px)) {
    a0 <- adms$admittime[match(px$adm_id, adms$adm_id)]
    t <- coalesce(t, pmax(0, as.numeric(difftime(as.POSIXct(as.Date(px$date), tz = "UTC"), a0, units = "mins"))))
  }
  tibble(adm_id = px$adm_id, t_min = t, title = coalesce(as.character(px[["title"]] %||% NA), as.character(px[["code"]] %||% NA), "procedure")) %>%
    filter(!is.na(t_min))
}
# Start of each admission on the patient timeline (the same placement as patient_timeline_plot()).
timeline_x0 <- function(adms, mode) {
  if (mode == "calendar" && all(!is.na(adms$admittime))) return(tibble(adm_id = adms$adm_id, x0 = adms$admittime, in_cohort = adms$in_cohort))
  gap <- if (all(is.na(adms$admittime))) 30 else 3
  adms %>% arrange(coalesce(as.numeric(admittime), as.numeric(year)), seq_hos, adm_id) %>%
    transmute(adm_id, in_cohort, x0 = cumsum(lag(coalesce(hosp_los_min, 1440) / 1440 + gap, default = 0)))
}
subject_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(border = FALSE, border_radius = FALSE, fillable = TRUE, 
    sidebar = sidebar(width = 340, title = "Select a subject", open = "always",
      selectInput(ns("f_src"), "Source", c("All sources" = "all", stats::setNames(SRC, SRC_LABEL[SRC]))),
      if (!is.null(INDEX_GROUP)) pickerInput(ns("f_grp"), D$meta$index_group_label %||% "Index group", INDEX_GROUP_LEVELS,
                                             selected = INDEX_GROUP_LEVELS, multiple = TRUE, options = list(`actions-box` = TRUE)),
      checkboxInput(ns("f_icu"), "Has at least one ICU stay", FALSE),
      checkboxInput(ns("f_multi"), "More than one admission", FALSE),
      checkboxInput(ns("f_died"), "Died in hospital", FALSE),
      if (!is.null(AGE_RANGE)) sliderInput(ns("f_age"), "Age at index admission", AGE_RANGE[1], AGE_RANGE[2], AGE_RANGE, step = 1),
      if (length(ADM_TYPES)) pickerInput(ns("f_atype"), "Admission type (any cohort admission)", ADM_TYPES, ADM_TYPES, multiple = TRUE,
                                         options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      uiOutput(ns("n_match")),
      selectizeInput(ns("pid"), "Patient", choices = NULL, options = list(placeholder = "Type a patient id", maxOptions = 200)),
      button_row(actionButton(ns("load"), "Load", icon = icon("download"), class = "btn-primary"),
                 actionButton(ns("random"), "Random", icon = icon("shuffle"))),
      radioButtons(ns("mode"), "Time series via",
                   c("Per-source tables" = "source", stats::setNames("merged", sprintf("Merged schema (%s)", D$meta$schema)))),
      tags$div(class = "small text-muted", style = "margin-top:-0.5rem", "Changing this reloads the loaded patient."),
      uiOutput(ns("adm_ui")), uiOutput(ns("load_info"))),
    uiOutput(ns("header")),
    conditionalPanel("output.has_subject", ns = ns, navset_underline(id = ns("tabs"),
      nav_panel("Timeline", card(
        radioButtons(ns("tl_mode"), NULL, c("Admissions in sequence (lengths to scale)" = "sequence", "Calendar (de-identified dates)" = "calendar"), inline = TRUE),
        uiOutput(ns("timeline_ui")), DTOutput(ns("adm_table")))),
      nav_panel("Admission", uiOutput(ns("adm_detail"))),
      nav_panel("Time series", card(uiOutput(ns("ts_hint")),
        layout_columns(col_widths = c(3, 5, 4),
          tagList(radioButtons(ns("axis"), "X axis", c("Hours since admission" = "hadm", "Hours since ICU admission" = "icu", "Date/time (when available)" = "date")),
                  uiOutput(ns("icu_pick"))),
          uiOutput(ns("concept_ui")),
          tagList(checkboxInput(ns("show_meds"), "Medication lane", TRUE), checkboxInput(ns("show_icu"), "Shade ICU stays", TRUE), uiOutput(ns("win_ui")),
                  selectizeInput(ns("cmp"), "Compare with (hours since admission)", choices = NULL, options = list(placeholder = "Optional: type a patient id", maxOptions = 200)))),
        uiOutput(ns("ts_plot_ui")), uiOutput(ns("ts_prov")))),
      nav_panel("All concepts", card(uiOutput(ns("grid_hint")),
        layout_columns(col_widths = c(8, 4), uiOutput(ns("gridc_ui")), numericInput(ns("grid_cols"), "Columns", 3, 1, 6)),
        uiOutput(ns("grid_ui")))),
      nav_panel("Medications & codes",
        card(card_header("Medication exposures (merged duration table)"), uiOutput(ns("meds_plot_ui")), DTOutput(ns("meds_tab"))),
        layout_columns(col_widths = c(7, 5),
          card(card_header("Diagnoses"), DTOutput(ns("dx_tab"))),
          card(card_header("Procedures / treatments"), DTOutput(ns("px_tab"))))),
      nav_panel("Data tables", card(
        radioButtons(ns("dt_scope"), NULL, c("Selected admission" = "adm", "All admissions of the patient" = "all"), inline = TRUE),
        navset_pill(nav_panel("Time series (long)",
                              tags$div(style = "text-align:right", downloadButton(ns("dl_ts"), "Download time series (CSV)", class = "btn-outline-secondary btn-sm")),
                              DTOutput(ns("ts_tab"))),
                    nav_panel("rendehR::get_patient_flat()", DTOutput(ns("flat_tab"))),
                    nav_panel("Concept summary", DTOutput(ns("csum_tab")))))),
      nav_panel("Verify",
        tags$p(class = "text-muted", "The same patient read two ways must give the same values."),
        layout_columns(col_widths = c(6, 6),
          card(card_header("Per-source harmonized (get_patient_ts) vs merged schema"),
               actionButton(ns("v_merged"), "Run comparison", icon = icon("scale-balanced")), uiOutput(ns("v_merged_sum")), DTOutput(ns("v_merged_tab"))),
          card(card_header("Harmonized vs raw laboratory values"),
               actionButton(ns("v_raw"), "Run comparison", icon = icon("flask")), uiOutput(ns("v_raw_sum")), DTOutput(ns("v_raw_tab")))),
        card(card_header("Structural checks for this patient"), DTOutput(ns("v_struct"))))
    )))
}

subject_server <- function(id, request) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  key_of <- function(src, pid) paste(src, pid, sep = "::")
  cand <- reactive(filter_patients(PATIENTS, input$f_src, input$f_grp, input$f_icu, input$f_multi, input$f_died, input$f_age, input$f_atype))
  output$n_match <- renderUI(tags$div(class = "small text-muted", sprintf("%s patients match", fmt_n(nrow(cand())))))
  observe({ p <- cand(); updateSelectizeInput(session, "pid", choices = stats::setNames(key_of(p$source, p$patient_id), p$label), server = TRUE,
                                              selected = isolate(input$pid) %||% character(0)) })
  # Select a patient while keeping the filtered list (the patient is added when the filters exclude it).
  set_pid <- function(k, lab) {
    p <- isolate(cand()); ch <- stats::setNames(key_of(p$source, p$patient_id), p$label)
    if (!k %in% ch) ch <- c(stats::setNames(k, lab), ch)
    updateSelectizeInput(session, "pid", choices = ch, selected = k, server = TRUE)
  }
  cur <- reactiveVal(NULL); want_adm <- reactiveVal(NULL)
  output$has_subject <- reactive(!is.null(cur()))
  outputOptions(output, "has_subject", suspendWhenHidden = FALSE)
  observeEvent(input$load, { req(input$pid, nzchar(input$pid)); want_adm(NULL); cur(list(key = input$pid, stamp = Sys.time())) })
  observeEvent(input$random, { p <- cand(); req(nrow(p) > 0); r <- p[sample.int(nrow(p), 1), ]; k <- key_of(r$source, r$patient_id)
    set_pid(k, r$label); want_adm(NULL); cur(list(key = k, stamp = Sys.time())) })
  observeEvent(request(), { r <- request(); req(r$source); k <- key_of(r$source, r$patient_id)
    pr <- PATIENTS[PATIENTS$source == r$source & PATIENTS$patient_id == r$patient_id, ]
    set_pid(k, if (nrow(pr)) pr$label[1] else k); want_adm(r$adm_id); cur(list(key = k, stamp = Sys.time())) })
  # The route applies to the loaded patient: reload it, staying on the same admission.
  observeEvent(input$mode, { req(cur()); want_adm(input$adm); cur(list(key = cur()$key, stamp = Sys.time())) }, ignoreInit = TRUE)

  # ---- load (live) ------------------------------------------------------------------------------------------------
  subj <- eventReactive(cur(), {
    k <- strsplit(cur()$key, "::", fixed = TRUE)[[1]]; src <- k[1]; pid <- paste(k[-1], collapse = "::")
    mode <- input$mode %||% "source"
    t0 <- Sys.time()
    ts <- withProgress(message = sprintf("Loading %s patient %s...", SRC_LABEL[[src]], pid), value = 0.3,
                       tryCatch(patient_ts(src, pid, mode), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL }))
    req(!is.null(ts))
    flat <- tryCatch(patient_flat(src, pid), error = function(e) tibble())
    codes <- tryCatch(patient_codes(src, pid), error = function(e) list(dx = tibble(), px = tibble()))
    list(source = src, patient_id = pid, ts = ts, flat = flat, mode = mode, secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
         adms = CTX_ADM[CTX_ADM$source == src & CTX_ADM$patient_id == pid, ], icus = CTX_ICU[CTX_ICU$source == src & CTX_ICU$patient_id == pid, ],
         dur = tryCatch(patient_dur(src, pid), error = function(e) EMPTY_DUR), codes = codes,
         info = PATIENTS[PATIENTS$source == src & PATIENTS$patient_id == pid, ])
  })
  output$load_info <- renderUI({ s <- subj()
    tags$div(class = "small text-muted mt-2", sprintf("Loaded %s time-series rows (%d concepts) in %.1f s via %s.", fmt_n(nrow(s$ts)),
                                                     n_distinct(s$ts$concept), s$secs,
                                                     if (s$mode == "source") "rendehR::get_patient_ts()" else "the merged schema")) })
  adm_order <- function(a) a[order(as.numeric(a$admittime), a$year, a$seq_hos, a$adm_id, na.last = TRUE), ]
  index_of <- function(s) { i <- s$info$index_adm; if (length(i) && !is.na(i[1])) i[1] else NA_character_ }
  # Default admission: the index admission when it has values, else the cohort admission with most values.
  default_adm <- function(s) {
    a <- adm_order(s$adms); if (!nrow(a)) return(NULL)
    n <- table(s$ts$adm_id[!is.na(s$ts$value)]); nv <- as.integer(n[a$adm_id]); nv[is.na(nv)] <- 0L
    idx <- index_of(s); has_idx <- !is.na(idx) && idx %in% a$adm_id
    if (has_idx && nv[match(idx, a$adm_id)] > 0) return(idx)
    if (any(nv > 0)) { pool <- if (any(nv[a$in_cohort %in% TRUE] > 0)) a$in_cohort %in% TRUE else rep(TRUE, nrow(a))
      return(a$adm_id[pool][which.max(nv[pool])]) }
    if (has_idx) idx else a$adm_id[1]
  }
  output$adm_ui <- renderUI({ s <- subj(); a <- adm_order(s$adms); validate(need(nrow(a) > 0, "No admissions found for this patient."))
    lab <- sprintf("%s %s | %s | %s", if_else(a$in_cohort, "★", "·"), a$adm_id, a$tl_group,
                   if_else(!is.na(a$admittime), format(a$admittime, "%Y-%m-%d"), if_else(is.na(a$year), "no date", paste("year", a$year))))
    sel <- want_adm() %||% default_adm(s); if (!length(sel) || !sel[1] %in% a$adm_id) sel <- a$adm_id[1]
    selectInput(ns("adm"), "Admission (★ = cohort)", stats::setNames(a$adm_id, lab), sel[1]) })
  A1 <- reactive({ s <- subj(); req(input$adm); s$adms[s$adms$adm_id == input$adm, ] })
  TS1 <- reactive({ s <- subj(); req(input$adm); s$ts[s$ts$adm_id == input$adm, ] })
  ICU1 <- reactive({ s <- subj(); req(input$adm); s$icus[s$icus$adm_id == input$adm, ] })
  DUR1 <- reactive({ s <- subj(); req(input$adm); s$dur[s$dur$adm_id == input$adm, ] })
  admit1 <- reactive({ a <- A1(); if (nrow(a) && !is.na(a$admittime[1])) a$admittime[1] else NA })

  # ---- header ------------------------------------------------------------------------------------------------------
  nowrap <- function(x) tags$span(class = "rh-id", x)
  output$header <- renderUI({
    if (is.null(cur())) return(card(empty_state("person-bounding-box", "No patient loaded",
      "Pick a patient on the left and press Load, or press Random. The filters above the list narrow the choice; ",
      if (LIVE) "time series are then read live from the database." else "time series need a database connection, which this session does not have.")))
    s <- subj(); a <- s$adms
    idx <- a[a$adm_id %in% index_of(s), ]; if (!nrow(idx)) idx <- a; idx <- idx[1, ]
    died <- any(a$hospital_mortality %in% 1)
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 6, lg = 4), class = "rh-stat-row",
      stat_box("Patient", nowrap(s$patient_id), "person-vcard", sub = SRC_LABEL[[s$source]]),
      stat_box("Age at index", if (is.na(idx$age %||% NA)) "?" else sprintf("%s y", round(idx$age)), "person", sub = idx$sex %||% NULL),
      stat_box("Index admission", nowrap(idx$adm_id), "tag", sub = idx$tl_group),
      stat_box("Admissions", nrow(a), "hospital", sub = sprintf("%d in cohort", sum(a$in_cohort))),
      stat_box("ICU stays", nrow(s$icus), "heart-pulse"),
      stat_box("Died in hospital", if (died) "Yes" else "No", "clipboard2-pulse", if (died) "danger" else "neutral")) })

  # ---- timeline ----------------------------------------------------------------------------------------------------
  # The container is recreated only when the height changes; the plot itself re-renders in place.
  dyn <- function(name, spec) {
    h <- reactiveVal(400)
    output[[paste0(name, "_ui")]] <- renderUI(withSpinner(plotlyOutput(ns(name), height = paste0(h(), "px"))))
    output[[name]] <- renderPlotly({ s <- spec(); h(s$h); s$p })
  }
  dyn("timeline", reactive({ s <- subj(); mode <- input$tl_mode %||% "sequence"
    r <- patient_timeline_plot(s$adms, s$icus, mode)
    pt <- procedure_times(s$codes$px, s$adms, s$icus)
    if (!nrow(pt) || !nrow(s$adms)) return(r)
    pt <- pt %>% inner_join(timeline_x0(s$adms, mode), by = "adm_id") %>%
      mutate(x = if (inherits(x0, "POSIXct")) x0 + t_min * 60 else x0 + t_min / 1440, row = sprintf("%s adm %s", if_else(in_cohort, "★", " "), adm_id))
    r$p <- r$p %>% add_markers(inherit = FALSE, x = pt$x, y = pt$row, name = "Procedure", hoverinfo = "text",
                               marker = list(symbol = "line-ns-open", size = 14, color = "#a53f3f", line = list(width = 2)),
                               text = sprintf("%s<br>%.1f h since admission", pt$title, pt$t_min / 60))
    r }))
  output$adm_table <- renderDT({ s <- subj(); a <- adm_order(s$adms)
    dt_table(filter = "none", tibble(Admission = a$adm_id, Cohort = if_else(a$in_cohort, "yes", "no"), Group = a$tl_group,
                    Admitted = format(a$admittime, "%Y-%m-%d %H:%M"), Year = a$year, `LOS (d)` = round(a$hosp_los_min / 1440, 1),
                    Age = round(a$age %||% NA), `Admission type` = a$admission_type %||% NA,
                    Discharge = a$hospital_discharge_location %||% NA, `Died in hospital` = a$hospital_mortality %||% NA,
                    `ICU stays` = a$n_icu), page = 10) })

  # ---- admission detail --------------------------------------------------------------------------------------------
  kv <- function(l) tags$table(class = "table table-sm", tags$tbody(lapply(names(l), function(n)
    tags$tr(tags$th(n, style = "width:45%"), tags$td(as.character(l[[n]] %||% "–"))))))
  output$adm_detail <- renderUI({ a <- A1(); req(nrow(a)); s <- subj()
    main <- list(Admitted = if (!is.na(a$admittime)) format(a$admittime, "%Y-%m-%d %H:%M") else if (!is.na(a$year)) paste("year", a$year) else "no date",
                 `In cohort` = if (a$in_cohort) "yes" else "no (patient context)", Age = round(a$age %||% NA, 1), Sex = a$sex %||% NA,
                 `Admission type` = a$admission_type %||% NA, Origin = a$hospital_admission_location %||% NA,
                 `Hospital LOS (days)` = round(a$hosp_los_min / 1440, 2), `Discharge destination` = a$hospital_discharge_location %||% NA,
                 `Died in hospital` = if (is.na(a$hospital_mortality %||% NA)) "n/a" else if (a$hospital_mortality %in% 1) "yes" else "no",
                 `ICU stays` = nrow(ICU1()), `Diagnosis codes` = a$num_diag_icd_codes %||% NA)
    fl <- a[, setdiff(names(a), c(.xp_hidden_cols, "tl_group", "source_label", "age_group")), drop = FALSE]
    fl <- tibble(Variable = names(fl), Value = vapply(fl, function(x) if (is.na(x[1])) "" else as.character(x[1]), character(1))) %>%
      filter(nzchar(Value))
    tagList(
      if (SCI) sci_admission_cards(s, a),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Admission and outcomes"), kv(main)),
        card(card_header(sprintf("All flat variables of this admission (%d)", nrow(fl))), dt_table(filter = "none", fl, page = 12, dom = .dt_dom_pages)))) })

  # ---- "this admission has nothing to show" hints ----------------------------------------------------------------------
  rows_by_adm <- reactive(subj()$ts %>% filter(!is.na(value)) %>% count(adm_id, name = "n") %>% arrange(desc(n)))
  hint_ui <- function(v, btn, none, here) {
    req(input$adm); if (sum(v$n[v$adm_id == input$adm]) > 0) return(NULL)
    if (!nrow(v)) return(tags$div(class = "alert alert-info", none))
    tags$div(class = "alert alert-info d-flex align-items-center gap-3", sprintf(here, v$adm_id[1], fmt_n(v$n[1])),
             actionButton(ns(btn), "Switch to that admission", class = "btn-sm btn-primary"))
  }
  for (b in c("ts", "grid")) local({ b <- b
    output[[paste0(b, "_hint")]] <- renderUI(hint_ui(rows_by_adm(), paste0(b, "_switch"), "This patient has no numeric time-series values.",
                                                     "This admission has no numeric values. Admission %s has %s values."))
    observeEvent(input[[paste0(b, "_switch")]], { v <- rows_by_adm(); req(nrow(v)); updateSelectInput(session, "adm", selected = v$adm_id[1]) })
  })

  # ---- time series -------------------------------------------------------------------------------------------------
  avail <- reactive({ TS1() %>% filter(!is.na(value)) %>% count(concept, name = "n") %>% mutate(category = concept_category(concept)) %>% arrange(category, concept) })
  output$concept_ui <- renderUI({ a <- avail(); validate(need(nrow(a) > 0, "No numeric values for this admission."))
    ch <- split(stats::setNames(a$concept, sprintf("%s (%s)", a$concept, fmt_n(a$n))), a$category)
    def <- intersect(isolate(input$concepts), a$concept)
    if (!length(def)) def <- intersect(c(unlist(VITAL_PANELS), "hemoglobin", "creatinine", "sodium", "potassium", "wbc", "glucose", "lactate", "peep", "fio2"), a$concept)
    if (!length(def)) def <- head(a$concept, 6)
    pickerInput(ns("concepts"), sprintf("Concepts (%d available)", nrow(a)), ch, selected = def, multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 4")) })
  rng <- reactive({ t <- TS1()$t_hadm / 60; if (!length(t) || all(is.na(t))) c(0, 1) else c(floor(min(t, na.rm = TRUE)), ceiling(max(t, na.rm = TRUE))) })
  output$win_ui <- renderUI({ r <- rng(); sliderInput(ns("win"), "Window (hours since hospital admission)", r[1], max(r[2], r[1] + 1), r, step = 1) })
  # ICU clock: time zero is the start of the chosen ICU stay of this admission.
  icu_sorted <- reactive(ICU1() %>% arrange(icu_in_min))
  output$icu_pick <- renderUI({ req(input$axis == "icu"); st <- icu_sorted()
    if (!nrow(st)) return(tags$p(class = "small text-warning", "This admission has no ICU stay."))
    lab <- sprintf("%s (%s, starts %.1f h after admission)", st$icu_id, coalesce(st$icu_unit, "ICU"), st$icu_in_min / 60)
    if (nrow(st) == 1) return(tags$p(class = "small text-muted", "Time zero: start of ICU stay ", lab))
    selectInput(ns("icu_stay"), "ICU stay (time zero)", stats::setNames(st$icu_id, lab), c(intersect(isolate(input$icu_stay), st$icu_id), st$icu_id)[1]) })
  icu0 <- reactive({ st <- icu_sorted(); validate(need(nrow(st) > 0, "This admission has no ICU stay."))
    i <- match(input$icu_stay %||% "", st$icu_id); st$icu_in_min[if (is.na(i)) 1L else i] })
  build_panels <- function(sel) {
    vit <- lapply(VITAL_PANELS, function(cs) intersect(cs, sel)); vit <- vit[lengths(vit) > 0]
    other <- setdiff(sel, unlist(VITAL_PANELS)); c(vit, stats::setNames(as.list(other), other))
  }
  dyn("ts_plot", reactive({ req(input$concepts, input$win, input$axis)
    validate(need(!(input$axis == "date" && is.na(admit1())), "This source has no dates; use hours since admission."))
    i0 <- if (input$axis == "icu") icu0() else NULL
    d <- TS1() %>% filter(between(t_hadm / 60, input$win[1], input$win[2]))
    validate(need(nrow(d) > 0, "No values in this window."))
    pn <- build_panels(input$concepts); if (length(pn) > 24) { showNotification("Showing the first 24 panels; use 'All concepts' for more.", type = "warning"); pn <- pn[1:24] }
    du <- if (input$show_meds) DUR1() %>% filter(end_min / 60 >= input$win[1], start_min / 60 <= input$win[2]) else NULL
    r <- ts_panels_plot(d, pn, icu = if (input$show_icu) ICU1() else NULL, dur = du, axis = input$axis, admit = admit1(), icu0 = i0, height_per = 170)
    c2 <- if (input$axis == "hadm") cmp_ts() else NULL
    if (!is.null(c2)) c2$ts <- c2$ts %>% filter(between(t_hadm / 60, input$win[1], input$win[2]))
    r$p <- ts_decorate(r$p, input$concepts, c2$ts, c(subj()$patient_id, c2$patient_id))
    r$p$x$source <- ns("ts_plot")
    r }))
  # ---- comparison patient and provenance on click ------------------------------------------------------------------------
  observe({ s <- subj(); p <- PATIENTS[!(PATIENTS$source == s$source & PATIENTS$patient_id == s$patient_id), ]
    updateSelectizeInput(session, "cmp", choices = c("None" = "", stats::setNames(key_of(p$source, p$patient_id), p$label)), server = TRUE) })
  # The comparison patient's default admission (as for the loaded patient), on the same route.
  cmp_ts <- reactive({ if (!isTruthy(input$cmp)) return(NULL); k <- strsplit(input$cmp, "::", fixed = TRUE)[[1]]; src <- k[1]; pid <- paste(k[-1], collapse = "::")
    ts <- tryCatch(patient_ts(src, pid, input$mode %||% "source"), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(ts)) return(NULL)
    s <- list(ts = ts, adms = CTX_ADM[CTX_ADM$source == src & CTX_ADM$patient_id == pid, ], info = PATIENTS[PATIENTS$source == src & PATIENTS$patient_id == pid, ])
    list(patient_id = pid, ts = ts[ts$adm_id %in% default_adm(s), ]) })
  # Registered up front: the plot renders a flush after this output, and plotly warns about unregistered sources.
  session$userData$plotlyShinyEventIDs <- unique(c(session$userData$plotlyShinyEventIDs, paste0("plotly_click-", ns("ts_plot"))))
  output$ts_prov <- renderUI({ ev <- event_data("plotly_click", source = ns("ts_plot"))
    if (is.null(ev$customdata)) return(tags$p(class = "small text-muted mt-2", "Click a point to see how its concept is defined for this source."))
    s <- subj(); cn <- as.character(ev$customdata[1]); def <- concept_definition(cn, s$source)
    td <- function(x) tags$td(if (is.na(x) || !nzchar(x)) "–" else x)
    tags$div(class = "mt-2 small",
      tags$h6(sprintf("%s = %s at %s", cn, format(ev$y[1]), if (input$axis == "date") as.character(ev$x[1])
                      else sprintf("%.1f %s", as.numeric(ev$x[1]), tolower(axis_title(input$axis))))),
      if (nrow(def$config)) tags$table(class = "table table-sm",
        tags$thead(tags$tr(lapply(c("Table", "Items (match_value)", "Unit", "Conversion"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(def$config)), function(i) { r <- def$config[i, ]
          tags$tr(td(r$table_name), td(r$match_value), td(r$unit), td(r$conversion)) })))
      else tags$p(class = "text-muted", sprintf("Not in the shipped concept configuration for %s (a derived or external concept).", src_family(s$source))),
      if (nrow(def$origin) && any(!is.na(def$origin$origin_name %||% NA))) { o <- def$origin[1, ]
        nm <- if (!is.na(o$origin_url %||% NA)) as.character(tags$a(href = o$origin_url, target = "_blank", o$origin_name)) else htmlEscape(o$origin_name)
        tags$p(HTML(paste0("Origin: ", nm, htmlEscape(paste0(if (!is.na(o$origin_ref %||% NA)) paste0(" (", o$origin_ref, ")"), "; schema ", o$schema %||% "?", "."))))) }) })
  output$gridc_ui <- renderUI({ a <- avail(); validate(need(nrow(a) > 0, "No numeric values for this admission."))
    sel <- intersect(isolate(input$grid_c), a$concept); if (!length(sel)) sel <- head(a$concept, 12)
    pickerInput(ns("grid_c"), sprintf("Concepts (%d available; first 12 shown by default)", nrow(a)), split(a$concept, a$category), selected = sel, multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 4")) })
  dyn("grid", reactive({ req(input$grid_c); ts_grid_plot(TS1(), input$grid_c, "hadm", NULL, input$grid_cols %||% 3) }))

  # ---- medications / diagnoses / procedures ----------------------------------------------------------------------------
  dyn("meds_plot", reactive({ d <- DUR1(); validate(need(nrow(d) > 0, "No medication exposures for this admission."))
    r <- ts_panels_plot(TS1()[0, ], list(), icu = ICU1(), dur = d, axis = "hadm", height_per = 60); r$h <- 150 + 18 * n_distinct(d$drug); r }))
  output$meds_tab <- renderDT(dt_table(filter = "none", DUR1() %>% transmute(Drug = drug, `Start (h)` = round(start_min / 60, 2), `End (h)` = round(end_min / 60, 2),
                                                             `Duration (min)` = duration_min, Value = value, Unit = clean_unit(unit), `ICU stay` = icu_id,
                                                             `Negative duration` = if_else(negative_duration %in% TRUE, "yes", "")) %>% arrange(`Start (h)`), page = 10))
  # Codes of the selected admission; eICU offsets are minutes from ICU admission, also shown on the hospital clock.
  codes1 <- function(x, what) { s <- subj(); a <- A1()
    x <- x %||% tibble(); if ("adm_id" %in% names(x)) x <- x[x$adm_id %in% a$adm_id, , drop = FALSE] else x <- x[0, , drop = FALSE]
    validate(need(nrow(x) > 0, sprintf("No %s recorded for this admission.", what)))
    if (all(c("offset_min", "icu_id") %in% names(x)))
      x <- x %>% left_join(s$icus %>% select(icu_id, icu_in_min), by = "icu_id") %>%
        mutate(`Hours since admission` = round((offset_min + icu_in_min) / 60, 1), .before = 1) %>%
        rename(`Offset from ICU admission (min)` = offset_min) %>% select(-icu_in_min)
    x
  }
  output$dx_tab <- renderDT({ x <- codes1(subj()$codes$dx, "diagnoses")
    dt_table(filter = "none", x %>% select(any_of(c("is_scid", "seq_num", "Hours since admission", "Offset from ICU admission (min)", "icd_code", "icd_version", "title", "priority"))), page = 12) })
  output$px_tab <- renderDT({ x <- codes1(subj()$codes$px, "procedures or treatments"); a <- A1()
    if ("date" %in% names(x) && !is.na(a$admittime)) x$day <- as.integer(as.Date(x$date) - as.Date(format(a$admittime, "%Y-%m-%d")))
    dt_table(filter = "none", x %>% select(any_of(c("day", "Hours since admission", "Offset from ICU admission (min)", "code", "icd_version", "title"))), page = 12) })

  # ---- data tables -------------------------------------------------------------------------------------------------------
  scope_ts <- reactive(if (input$dt_scope == "adm") TS1() else subj()$ts)
  output$ts_tab <- renderDT(dt_table(scope_ts() %>% transmute(Admission = adm_id, `ICU stay` = icu_id, `Hours since admission` = round(t_hadm / 60, 3),
                                                              `Hours since ICU` = round(t_icu / 60, 3), Concept = concept,
                                                              Value = value_chr, Unit = unit, `Source table` = source_table), page = 20))
  output$flat_tab <- renderDT({ f <- subj()$flat; validate(need(nrow(f) > 0, "No flat rows (live connection unavailable?)."))
    if (input$dt_scope == "adm") { ac <- intersect(c(.source_entry(subj()$source)$select_id_cols[2]), names(f))
      if (length(ac)) f <- f[f[[ac[1]]] == input$adm, , drop = FALSE] }
    dt_table(filter = "none", f, page = 10) })
  output$csum_tab <- renderDT(dt_table(filter = "none", scope_ts() %>% group_by(Concept = concept, Category = concept_category(concept)) %>%
    summarise(Rows = n(), `With value` = sum(!is.na(value)), Min = safe_min(value), Median = stats::median(value, na.rm = TRUE),
              Max = safe_max(value), `First (h)` = round(safe_min(t_hadm) / 60, 1), `Last (h)` = round(safe_max(t_hadm) / 60, 1),
              Unit = first(stats::na.omit(unit)), .groups = "drop"), page = 25))

  # ---- verification ------------------------------------------------------------------------------------------------------
  # Results belong to the loaded patient: cleared on every load.
  vm <- reactiveVal(NULL); vr <- reactiveVal(NULL)
  observeEvent(cur(), { vm(NULL); vr(NULL) })
  observeEvent(input$v_merged, { s <- subj()
    other <- if (s$mode == "source") "merged" else "source"
    o <- withProgress(message = "Reading the other route...",
                      tryCatch(patient_ts(s$source, s$patient_id, other), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL }))
    req(!is.null(o)); vm(.xp_compare_routes(s$ts, o, s$mode, other)) })
  output$v_merged_sum <- renderUI({ v <- vm(); req(v); ok <- nrow(v) > 0 && all(v$match)
    tags$div(class = paste("alert mt-2", if (ok) "alert-success" else "alert-warning"),
             sprintf("%s: %d of %d admission x concept combinations identical (row and value counts, sums of values and of times).",
                     if (ok) "PASS" else "CHECK", sum(v$match), nrow(v))) })
  output$v_merged_tab <- renderDT({ req(vm()); dt_table(filter = "none", vm(), page = 10) })
  observeEvent(input$v_raw, { s <- subj(); r <- withProgress(message = "Reading raw laboratory values (DuckDB)...",
                                                             tryCatch(raw_labs(s$source, s$patient_id), error = function(e) NULL))
    vr(if (is.null(r) || !nrow(r)) "No raw laboratory file or values found for this patient (raw data directory not configured?)." else .xp_compare_raw(s$ts, r)) })
  vr_ok <- reactive({ v <- vr(); req(v); validate(need(!is.character(v), v)); v })
  output$v_raw_sum <- renderUI({ v <- vr_ok(); b <- v %>% filter(!is.na(n_raw), !is.na(n_harmonized))
    validate(need(nrow(b) > 0, "No laboratory test was found in both the harmonized and the raw data of this patient."))
    k <- sum(b$agreement == "agree (within 1%)"); ok <- k == nrow(b)
    tags$div(class = paste("alert mt-2", if (ok) "alert-success" else "alert-warning"),
             sprintf("%s: %d of %d admission x test pairs agree (same count, means within 1%%); %d differ in count, %d in mean. Differences can come from rendehR range cleaning, extra item ids or de-duplication.",
                     if (ok) "PASS" else "CHECK", k, nrow(b), sum(b$agreement == "count differs"), sum(b$agreement == "differs (> 1%)"))) })
  output$v_raw_tab <- renderDT(dt_table(filter = "none", vr_ok() %>% mutate(across(where(is.numeric), ~ round(.x, 3))), page = 10))
  output$v_struct <- renderDT({ s <- subj()
    inwin <- s$ts %>% left_join(s$adms %>% select(adm_id, hosp_los_min), by = "adm_id") %>%
      with(t_hadm >= -1440 & t_hadm <= hosp_los_min + 1440)
    inwin <- inwin[!is.na(inwin)]
    num <- s$ts$value[nzchar(clean_unit(s$ts$unit))]
    pct <- function(k, n) if (n) sprintf("%s of %s (%.1f%%)", fmt_n(k), fmt_n(n), 100 * k / n) else "none"
    ck <- tibble::tribble(~check, ~ok, ~detail,
      "Every time-series admission belongs to this patient", all(unique(s$ts$adm_id) %in% s$adms$adm_id), paste(n_distinct(s$ts$adm_id), "admissions with time series"),
      "Every time-series ICU stay belongs to this patient", all(stats::na.omit(unique(s$ts$icu_id)) %in% s$icus$icu_id), paste(n_distinct(stats::na.omit(s$ts$icu_id)), "ICU stays with time series"),
      "Medication exposures belong to this patient's admissions", all(unique(s$dur$adm_id) %in% s$adms$adm_id), paste(nrow(s$dur), "exposure rows"),
      "Values are numeric where a unit is given", !length(num) || mean(!is.na(num)) > 0.95, paste(pct(sum(!is.na(num)), length(num)), "values with a unit are numeric"),
      "Time since admission within the admission (-1 day to LOS + 1 day)", !length(inwin) || mean(inwin) > 0.95, paste(pct(sum(inwin), length(inwin)), "rows"))
    status_style(datatable(ck %>% mutate(status = if_else(ok, "PASS", "CHECK"), .before = 1) %>% select(-ok), rownames = FALSE,
                           colnames = c("Status", "Check", "Detail"),
                           options = list(dom = "t"), class = "compact")) })
  output$dl_ts <- downloadHandler(
    filename = function() sprintf("patient_%s_timeseries.csv", subj()$patient_id %||% "patient"),
    content = function(file) utils::write.csv(scope_ts() %>%
      transmute(admission = adm_id, icu_stay = icu_id, hours_since_admission = t_hadm / 60,
                concept = concept, value = value_chr, unit = unit), file, row.names = FALSE))

})

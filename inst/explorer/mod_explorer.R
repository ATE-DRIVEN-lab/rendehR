# ---- Cohort Explorer: filters + characteristics table, distributions, outcomes, survival, cross-tabs, scatter, data --------------------
# Filters come from FILTERS (bundle$filters): list(var, label, type = checkbox | picker | slider | icu,
# optional choices, keep_na = TRUE, na_label = NULL, panel = "Population").
# Checkbox and picker filters on a variable with missing values get an explicit
# "(missing)" level (unless the bundle sets na_label or fixed choices), so
# missing values can be filtered out like any other level.
# Filters without a panel are grouped by what they describe.
.ex_patient_vars <- c("sex", "age", "age_group", "race", "ethnicity", "language", "marital_status", "insurance")
.ex_admission_vars <- c("admission_type", "hospital_admission_location", "hospital_discharge_location", "year", "any_icu",
                        "admission_service", "unit_first")
EX_FILTERS <- lapply(FILTERS, function(f) {
  if (f$type %in% c("checkbox", "picker") && is.null(f$na_label) && is.null(f$choices) &&
      f$var %in% names(ADM) && anyNA(ADM[[f$var]])) f$na_label <- "(missing)"
  f$panel <- f$panel %||% (if (f$var == "source") "Sources" else if (f$var %in% .ex_patient_vars) "Patient"
                           else if (f$var %in% .ex_admission_vars) "Admission" else "Cohort")
  f
})
filter_choices <- function(f) {
  if (!is.null(f$choices)) return(f$choices)
  if (f$var == "source") return(stats::setNames(SRC, SRC_LABEL[SRC]))
  x <- ADM[[f$var]]
  lv <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(stats::na.omit(as.character(x))))
  if (!is.null(f$na_label) && any(is.na(x))) lv <- c(lv, f$na_label)
  lv
}
filter_input <- function(ns, f) {
  id <- ns(paste0("f_", f$var))
  ch <- if (f$type %in% c("checkbox", "picker")) filter_choices(f)
  switch(f$type,
    checkbox = checkboxGroupInput(id, f$label, ch, selected = unname(ch), inline = isTRUE(f$inline) || length(ch) <= 3),
    picker = pickerInput(id, f$label, ch, selected = unname(ch), multiple = TRUE,
                         options = list(`actions-box` = TRUE, `live-search` = length(ch) > 15,
                                        `selected-text-format` = "count > 2", `count-selected-text` = "{0} of {1} selected",
                                        `select-all-text` = "All", `deselect-all-text` = "None")),
    slider = { r <- range(ADM[[f$var]], na.rm = TRUE); nm <- sum(is.na(ADM[[f$var]]))
      tagList(sliderInput(id, f$label, floor(r[1]), ceiling(r[2]), c(floor(r[1]), ceiling(r[2])), ticks = FALSE, sep = ""),
              if (nm > 0) tags$div(class = "small text-muted mb-2", style = "margin-top: -0.5rem;",
                                   sprintf("%s without a value, included while the full range is selected.", mask_n(nm)))) },
    icu = radioButtons(id, f$label, c("Any" = "any", "With ICU stay" = "1", "Without ICU stay" = "0"), inline = TRUE))
}
# The value a filter starts with (everything kept), whether it has moved from
# it, and how to put it back. A checkbox or picker with nothing selected
# (NULL once the input exists, `present`) keeps nothing.
filter_default <- function(f) {
  switch(f$type,
    checkbox = , picker = unname(filter_choices(f)),
    slider = { r <- range(ADM[[f$var]], na.rm = TRUE); c(floor(r[1]), ceiling(r[2])) },
    icu = "any")
}
filter_active <- function(f, val, present = TRUE) {
  if (is.null(val)) return(present && f$type %in% c("checkbox", "picker"))
  def <- filter_default(f)
  if (f$type == "slider") return(!isTRUE(all.equal(as.numeric(val), def)))
  !setequal(as.character(val), as.character(def))
}
filter_reset <- function(session, f) {
  id <- paste0("f_", f$var); def <- filter_default(f)
  switch(f$type,
    checkbox = updateCheckboxGroupInput(session, id, selected = def),
    picker = shinyWidgets::updatePickerInput(session, id, selected = def),
    slider = updateSliderInput(session, id, value = def),
    icu = updateRadioButtons(session, id, selected = def))
}
apply_filter <- function(d, f, val, present = TRUE) {
  if (is.null(val)) return(if (present && f$type %in% c("checkbox", "picker")) d[0, , drop = FALSE] else d)
  x <- d[[f$var]]
  keep_na <- f$keep_na %||% TRUE
  if (f$type == "slider") {                       # missing values stay only while the slider spans the full range
    full <- !filter_active(f, val)
    return(d[(x >= val[1] & x <= val[2]) %in% TRUE | (full & keep_na & is.na(x)), , drop = FALSE])
  }
  if (f$type == "icu") return(if (val == "any") d else d[x %in% as.integer(val), , drop = FALSE])
  xs <- as.character(x)
  if (!is.null(f$na_label)) xs <- coalesce(xs, f$na_label)
  d[xs %in% val | (keep_na & is.na(xs)), , drop = FALSE]
}
# One line per active filter, for the Compare tab.
filter_desc <- function(a) {
  if (!length(a)) return("no filters")
  paste(vapply(a, function(x) {
    f <- x$f; v <- x$v
    val <- switch(f$type, slider = sprintf("%g-%g", v[1], v[2]), icu = if (v == "1") "with ICU stay" else "without ICU stay",
                  if (!length(v)) "none" else paste(if (f$var == "source") SRC_LABEL[v] else v, collapse = ", "))
    sprintf("%s: %s", f$label %||% f$var, val)
  }, ""), collapse = "; ")
}
# dplyr code equivalent to apply_filter() over the given filters and values
# (NULL keeps nothing). Expects a bundle `b` from explorer_data().
filter_code <- function(filters, values, unit = "adm") {
  conds <- unlist(Map(function(f, v) {
    x <- deparse(as.name(f$var), backtick = TRUE)
    if (f$type == "slider") return(sprintf("dplyr::between(%s, %s, %s)", x, v[1], v[2]))
    if (f$type == "icu") return(if (v == "any") NULL else sprintf("%s %%in%% %s", x, v))
    if (is.null(v)) return("FALSE")
    na <- if (!is.null(f$na_label)) f$na_label %in% v else f$keep_na %||% TRUE   # na_label turns NA into a level
    vals <- setdiff(v, f$na_label)
    paste(c(if (length(vals)) sprintf("%s %%in%% c(%s)", x, paste(encodeString(vals, quote = '"'), collapse = ", ")),
            if (na) sprintf("is.na(%s)", x)), collapse = " | ")
  }, filters, values))
  c(if (identical(unit, "pat")) "adm <- b$adm[b$adm$is_index, ]" else "adm <- b$adm",
    if (length(conds)) c("adm <- dplyr::filter(adm,", paste0("  ", conds, c(rep(",", length(conds) - 1), "")), ")"))
}
trajectory_query <- function(cn, clock, hz, bw) .with_connection(NULL, function(con) .xp_trajectory(con, D$meta$schema, cn, clock, hz, bw))
# Kaplan-Meier input: death is patient-level, so only index admissions (one per
# patient) from sources with death times. In-hospital deaths without a death
# time count as events at discharge (hospital_los). Censored at the horizon hz.
km_data <- function(d, g, hz) {
  d <- d[d$is_index %in% TRUE & d$source %in% SURV_SOURCES, , drop = FALSE]
  if (g != ".all") d <- d[!is.na(d[[g]]), , drop = FALSE]
  ttd <- d$time_to_death_from_admission
  if ("hospital_mortality" %in% names(d)) {
    miss <- is.na(ttd) & d$hospital_mortality %in% 1
    los <- if ("hospital_los" %in% names(d)) d$hospital_los[miss] else rep(NA_real_, sum(miss))
    ttd[miss] <- pmax(coalesce(los, 0.01), 0.01)
  }
  d$status <- as.integer(!is.na(ttd) & ttd <= hz)
  d$time <- if_else(d$status == 1L, pmax(ttd, 0.01), hz)
  d$gg <- if (g == ".all") rep("Overall", nrow(d)) else as.character(d[[g]])
  d
}
# Step-function coordinates (for stepped confidence ribbons).
step_xy <- function(x, y) {
  n <- length(x)
  if (n < 2) return(list(x = x, y = y))
  list(x = c(rbind(x[-n], x[-1]), x[n]), y = c(rbind(y[-n], y[-n]), y[n]))
}
hex_rgba <- function(h, a) { v <- grDevices::col2rgb(h); sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a) }
filter_panel <- function(ns) {
  panels <- vapply(EX_FILTERS, `[[`, "", "panel")
  order <- unique(c(intersect(c("Sources", "Cohort"), panels), panels))
  acc <- lapply(order, function(p) {
    fs <- EX_FILTERS[panels == p]
    if (length(fs) == 1) fs[[1]]$label <- NULL      # a one-filter panel is already named by its heading
    accordion_panel(p, lapply(fs, function(f) filter_input(ns, f)))
  })
  do.call(accordion, c(list(open = TRUE, multiple = TRUE, class = "rh-filters"), acc))
}
explorer_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(border = FALSE, border_radius = FALSE, fillable = TRUE, sidebar = sidebar(width = 340, title = "Filters",
      uiOutput(ns("nsel")),
      radioButtons(ns("unit"), tags$span("Count", tooltip(bs_icon("info-circle", title = "About counting patients", class = "ms-1 text-muted"),
                     "Patients: each patient's first cohort admission, chosen before the filters; a patient whose first admission is filtered out drops out.")),
                   c("Admissions" = "adm", "Patients" = "pat"), inline = TRUE),
      filter_panel(ns),
      input_switch(ns("mask"), "Hide counts below 10", TRUE),
      downloadButton(ns("dl_sel"), "Download selection (CSV)", class = "btn-outline-secondary w-100"),
      actionButton(ns("show_code"), "Show R code", icon = icon("code"), class = "btn-outline-secondary w-100")),
    navset_card_underline(id = ns("tab"), full_screen = TRUE,
      nav_panel("Characteristics",
        layout_columns(fill = FALSE, col_widths = c(3, 3, 6), selectInput(ns("t1_group"), "Columns (group by)", GROUP_CHOICES, DEF$t1_group),
                       pickerInput(ns("t1_vars"), "Rows (variables)", var_choices(), multiple = TRUE, selected = DEF$t1_vars,
                                   options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                       uiOutput(ns("t1_smd_ui"))),
        withSpinner(DTOutput(ns("t1"))), uiOutput(ns("t1_note"))),
      nav_panel("Distributions",
        layout_columns(fill = FALSE, col_widths = c(4, 4, 4), pickerInput(ns("d_var"), "Variable", var_choices(), selected = DEF$d_var, options = list(`live-search` = TRUE)),
                       selectInput(ns("d_group"), "Group by", GROUP_CHOICES, DEF$d_group),
                       selectInput(ns("d_geom"), "Plot (continuous variables)", c("Box plot" = "box", "Violin" = "violin", "Histogram" = "hist", "Cumulative (ECDF)" = "ecdf"))),
        withSpinner(plotlyOutput(ns("dplot"), height = "480px")), uiOutput(ns("dnote")), DTOutput(ns("dsum"))),
      nav_panel("Outcomes",
        layout_columns(fill = FALSE, col_widths = c(4, 4, 4), pickerInput(ns("o_var"), "Binary outcome / indicator", var_choices("bin"), selected = DEF$o_var, options = list(`live-search` = TRUE)),
                       selectInput(ns("o_group"), "Group by", GROUP_CHOICES, DEF$o_group), selectInput(ns("o_strat"), "Stratify by", DEF$o_strata)),
        withSpinner(plotlyOutput(ns("oplot"), height = "520px")), uiOutput(ns("ocap")), DTOutput(ns("otab"))),
      nav_panel("Trends",
        tags$p(class = "small text-muted", "Admissions per year and the yearly rate of an outcome (95% Wilson interval)."),
        layout_columns(fill = FALSE, col_widths = c(4, 4, 4), pickerInput(ns("tr_var"), "Binary outcome", var_choices("bin"), selected = DEF$o_var, options = list(`live-search` = TRUE)),
                       selectInput(ns("tr_group"), "Series", GROUP_CHOICES, ".all"),
                       conditionalPanel("input.tr_group != '.all'", ns = ns,
                         radioButtons(ns("tr_scale"), "Show", c("Count" = "n", "Share of the year" = "pct"), inline = TRUE))),
        withSpinner(plotlyOutput(ns("trplot"), height = "420px")),
        withSpinner(plotlyOutput(ns("trrate"), height = "380px")), uiOutput(ns("trcap"))),
      nav_panel("Trajectories",
        if (!LIVE) empty_state("database", "Needs the database", "Trajectories are computed from the merged time series in the live database.")
        else tagList(
          layout_columns(fill = FALSE, col_widths = c(3, 3, 2, 2, 2),
            pickerInput(ns("tj_cn"), "Concept", CONCEPT_NAMES, first_in(c("heart_rate", "hemoglobin"), CONCEPT_NAMES), options = list(`live-search` = TRUE)),
            selectInput(ns("tj_clock"), "Hours since", c("Hospital admission" = "hadm", "ICU admission" = "icu")),
            selectInput(ns("tj_hz"), "Horizon", c("24 h" = 24, "48 h" = 48, "72 h" = 72, "7 days" = 168), 72),
            selectInput(ns("tj_bw"), "Bin width", c("1 h" = 1, "4 h" = 4, "6 h" = 6, "12 h" = 12, "24 h" = 24), 4),
            selectInput(ns("tj_group"), "Group by", GROUP_CHOICES, DEF$d_group)),
          button_row(actionButton(ns("tj_go"), "Compute", icon = icon("chart-line"), class = "btn-primary")),
          conditionalPanel("!(input.tj_go > 0)", ns = ns, empty_state("graph-up", "No trajectory yet", "Choose a concept and press Compute.")),
          conditionalPanel("input.tj_go > 0", ns = ns, withSpinner(plotlyOutput(ns("tjplot"), height = "460px")), uiOutput(ns("tjnote")),
                           DTOutput(ns("tjtab"))))),
      nav_panel("Survival",
        layout_columns(fill = FALSE, col_widths = c(4, 4, 4), selectInput(ns("s_group"), "Group by", GROUP_CHOICES, DEF$s_group),
                       sliderInput(ns("s_days"), "Follow-up (days from admission)", 30, 365, 365, step = 5),
                       tags$div(class = "pt-4", checkboxInput(ns("s_ci"), "95% confidence bands", FALSE))),
        tags$p(class = "small text-muted",
               sprintf(paste("Kaplan-Meier from admission, one index admission per patient. In-hospital deaths without a time count at discharge;",
                             "others are censored at the horizon. Deaths after discharge are recorded up to about a year in MIMIC-IV and not in eICU.",
                             "Sources included: %s."),
                       if (length(SURV_SOURCES)) paste(SRC_LABEL[SURV_SOURCES], collapse = ", ") else "none")),
        withSpinner(plotlyOutput(ns("splot"), height = "500px")), uiOutput(ns("scap")),
        tags$h6(class = "mt-2", "Number at risk"), DTOutput(ns("srisk")), DTOutput(ns("stab"))),
      nav_panel("Cross-tab",
        layout_columns(fill = FALSE, col_widths = c(4, 4, 4), selectInput(ns("x_row"), "Rows", GROUP_CHOICES[GROUP_CHOICES != ".all"], DEF$x_row),
                       pickerInput(ns("x_col"), "Columns", var_choices(c("cat", "bin")), selected = DEF$x_col, options = list(`live-search` = TRUE)),
                       radioButtons(ns("x_pct"), "Percent of", c("Row" = "row", "Column" = "col", "Total" = "tot"), inline = TRUE)),
        withSpinner(plotlyOutput(ns("xplot"), height = "480px")), uiOutput(ns("xtest")), DTOutput(ns("xtab"))),
      nav_panel("Scatter",
        layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3), pickerInput(ns("sc_x"), "X", var_choices("cont"), selected = DEF$sc_x, options = list(`live-search` = TRUE)),
                       pickerInput(ns("sc_y"), "Y", var_choices("cont"), selected = DEF$sc_y, options = list(`live-search` = TRUE)),
                       selectInput(ns("sc_col"), "Colour", GROUP_CHOICES, DEF$sc_col), tags$div(class = "pt-4", checkboxInput(ns("sc_log"), "Log Y axis", FALSE))),
        withSpinner(plotlyOutput(ns("scplot"), height = "520px")), uiOutput(ns("sccor"))),
      nav_panel("Compare",
        layout_columns(fill = FALSE, col_widths = c(6, 6),
          pickerInput(ns("cmp_vars"), "Rows (variables)", var_choices(), multiple = TRUE, selected = DEF$t1_vars,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          tags$div(class = "pt-4", actionButton(ns("cmp_set"), "Set current selection as A", icon = icon("thumbtack"), class = "btn-outline-secondary"))),
        conditionalPanel("!(input.cmp_set > 0)", ns = ns,
          empty_state("layers", "No selection A yet", "Set the filters, press Set current selection as A, then change the filters: selection B follows them.")),
        conditionalPanel("input.cmp_set > 0", ns = ns, uiOutput(ns("cmp_sizes")), withSpinner(DTOutput(ns("cmp_t1"))), uiOutput(ns("cmp_note")),
                         tags$h6(class = "mt-3", "Outcomes"), DTOutput(ns("cmp_out")))),
      nav_panel("Cohort data",
        tags$p(class = "small text-muted", "Select a row to open the patient in the Subject Explorer."),
        actionButton(ns("open_subject"), "Open selected row in Subject Explorer", icon = icon("user")), withSpinner(DTOutput(ns("data"))))))
}
explorer_server <- function(id, goto_subject) moduleServer(id, function(input, output, session) {
  df <- reactive({
    req(input$unit)
    d <- if (input$unit == "pat") ADM[ADM$is_index, , drop = FALSE] else ADM
    keys <- names(input)                          # NULL from an existing input = nothing selected
    for (f in EX_FILTERS) { id <- paste0("f_", f$var); d <- apply_filter(d, f, input[[id]], id %in% keys) }
    d
  })
  unit_word <- reactive(if (identical(input$unit, "pat")) "patients" else "admissions")
  dfv <- reactive({ d <- df(); validate(need(nrow(d) > 0, sprintf("No %s match the filters.", unit_word()))); d })
  mask <- reactive(isTRUE(input$mask))
  active <- reactive({                             # list(f, v) per active filter
    keys <- names(input)
    Filter(Negate(is.null), lapply(EX_FILTERS, function(f) {
      id <- paste0("f_", f$var)
      if (filter_active(f, input[[id]], id %in% keys)) list(f = f, v = input[[id]])
    }))
  })
  n_active <- reactive(length(active()))
  output$nsel <- renderUI({
    k <- n_active()
    tags$div(class = "rh-selection",
      tags$div(tags$strong(fmt_n(nrow(df()))), paste0(" ", unit_word()),
               tags$div(class = "small text-muted", if (k == 0) "No filters applied" else sprintf("%d filter%s applied", k, if (k == 1) "" else "s"))),
      if (k > 0) actionButton(session$ns("reset"), "Reset", icon = icon("rotate-left"), class = "btn-sm"))
  })
  observeEvent(input$reset, for (f in EX_FILTERS) filter_reset(session, f))
  grp <- function(d, g) {
    if (g == ".all") return(d)
    d <- d[!is.na(d[[g]]), , drop = FALSE]
    d[[g]] <- if (is.factor(d[[g]])) droplevels(d[[g]]) else as.character(d[[g]])
    d
  }
  lv_of <- function(x) if (is.factor(x)) levels(droplevels(x)) else sort(unique(stats::na.omit(as.character(x))))
  need_rows <- function(d, what = "") validate(need(nrow(d) > 0, sprintf("No %s%s match the filters.", unit_word(), what)))
  # Small-cell codes for a levels x groups count matrix with the same pattern as Table 1
  # (Overall column, hidden groups with N < 10), so no tab reveals a cell another masks.
  group_codes <- function(m, ng) {
    m <- unclass(as.matrix(m)); ng <- as.numeric(ng[colnames(m)])
    cm <- small_cell_mask(cbind(m, rowSums(m)), hidden = c(ng < 10, FALSE), overall = TRUE)
    cm[, seq_len(ncol(m)), drop = FALSE]
  }
  note <- function(...) { x <- c(...); if (length(x)) tags$p(class = "small text-muted mt-1", paste(x, collapse = " ")) else tags$span() }

  # SMD column for groups g1 vs g2 (hide: a column is masked); continuous SMDs
  # come from means, so they go on the mean (SD) row.
  with_smd <- function(t1, d, g, g1, g2, vars, hide) {
    s <- compare2(d, g, g1, g2, vars) %>% select(variable, level, smd)
    if (hide) s$smd <- NA_real_
    t1 <- t1 %>% mutate(level2 = if_else(level == "mean (SD)", "median [IQR]", level)) %>%
      left_join(s, by = c("variable", level2 = "level")) %>% select(-level2) %>%
      mutate(smd = if_else(level == "median [IQR]", NA_real_, smd))
    names(t1)[names(t1) == "smd"] <- sprintf("SMD %s vs %s", g1, g2)
    t1
  }

  # Table 1
  output$t1_smd_ui <- renderUI({
    g <- input$t1_group; req(g, g != ".all")
    lv <- lv_of(df()[[g]])
    def <- DEF$smd[[g]]; if (is.null(def) || !all(def %in% lv)) def <- lv[1:2]
    if (length(lv) >= 2) layout_columns(col_widths = c(6, 6), selectInput(session$ns("t1_g1"), "SMD: group 1", lv, def[1]),
                                        selectInput(session$ns("t1_g2"), "vs group 2", lv, def[2]))
  })
  output$t1 <- renderDT({
    req(length(input$t1_vars) > 0, input$t1_group)
    g <- input$t1_group
    vars <- VARS %>% filter(var %in% input$t1_vars) %>% arrange(match(var, input$t1_vars))
    d <- grp(dfv(), g)
    need_rows(d, if (g != ".all") paste(" with a value for", var_label(g)) else "")
    if (g != ".all" && n_distinct(d[[g]]) > 25) validate(need(FALSE, "Too many groups (> 25) for a table."))
    t1 <- char_table(d, g, vars, mask = mask())
    g1 <- input$t1_g1; g2 <- input$t1_g2
    if (!is.null(g1) && !is.null(g2) && g1 != g2 && g != ".all" && all(c(g1, g2) %in% as.character(d[[g]])))
      t1 <- with_smd(t1, d, g, g1, g2, vars, hide = mask() && any(table(as.character(d[[g]]))[c(g1, g2)] < 10))
    t1 <- t1 %>% mutate(variable = if_else(duplicated(variable), "", variable)) %>%
      rename(Variable = variable, Level = level, `Missing (%)` = missing_pct, `p (all groups)` = p_value)
    summary_dt(t1)
  })
  output$t1_note <- renderUI({
    note(if (mask()) paste("Small-cell rule: <10 = count of 1-9 (no percentage; for 0/1 rows also when the complement is 1-9);",
                           "masked = suppressed so that a <10 cell cannot be recovered from the other cells or the Overall column;",
                           "groups with N < 10 are not described."),
         "p (all groups): Kruskal-Wallis or chi-square across all groups (Fisher when an expected count is below 5); the SMD compares the two selected groups.")
  })

  # distributions
  dcat <- reactive({                               # categorical counts with small-cell codes (bars and table)
    v <- input$d_var; g <- input$d_group; req(v, g)
    d <- grp(dfv(), g) %>% filter(!is.na(.data[[v]]))
    need_rows(d, paste(" with", var_label(v)))
    x <- d %>% count(.gg = as.character(.data[[g]]), val = as.character(.data[[v]])) %>%
      tidyr::complete(.gg, val, fill = list(n = 0L)) %>% group_by(.gg) %>% mutate(N = sum(n), pct = n / N) %>% ungroup()
    x$code <- 0L
    if (mask()) { m <- stats::xtabs(n ~ val + .gg, x); cm <- group_codes(m, table(as.character(grp(dfv(), g)[[g]])))
                  x$code <- cm[cbind(match(x$val, rownames(m)), match(x$.gg, colnames(m)))] }
    lv <- lv_of(d[[v]]); x$val <- factor(x$val, lv)
    x %>% arrange(.gg, val)
  })
  output$dplot <- renderPlotly({
    v <- input$d_var; req(v, input$d_group); ty <- VARS$type[VARS$var == v]; g <- input$d_group
    d <- grp(dfv(), g) %>% filter(!is.na(.data[[v]])) %>% mutate(.v = .data[[v]], .gg = as.character(.data[[g]]))
    need_rows(d, paste(" with", var_label(v)))
    cols <- group_colors(g, lv_of(d[[g]]))
    if (ty == "cont") {
      p <- switch(input$d_geom,
        box = plot_ly(d, x = ~.gg, y = ~.v, color = ~.gg, colors = cols, type = "box", boxpoints = FALSE),
        violin = plot_ly(d, x = ~.gg, y = ~.v, color = ~.gg, colors = cols, type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE)),
        hist = plot_ly(d, x = ~.v, color = ~.gg, colors = cols, type = "histogram", histnorm = "probability", opacity = 0.6) %>%
          layout(barmode = if (n_distinct(d$.gg) > 3) "group" else "overlay"),
        ecdf = { e <- d %>% group_by(.gg) %>% arrange(.v, .by_group = TRUE) %>% mutate(F = row_number() / n()) %>% ungroup()
                 plot_ly(e, x = ~.v, y = ~F, color = ~.gg, colors = cols, type = "scatter", mode = "lines", line = list(shape = "hv")) })
      p %>% layout(xaxis = list(title = if (input$d_geom %in% c("hist", "ecdf")) var_label(v) else ""),
                   yaxis = list(title = if (input$d_geom %in% c("hist", "ecdf")) "" else var_label(v)),
                   showlegend = input$d_geom %in% c("hist", "ecdf")) %>% plot_config()
    } else {
      x <- dcat() %>% filter(n > 0, code == 0L) %>% mutate(lab = sprintf("%s | %s: %s", .gg, val, n_pct(n, N)))
      if (!nrow(x)) return(empty_plot("Every cell is masked by the small-cell rule."))
      plot_ly(x, x = ~.gg, y = ~pct, color = ~val, colors = group_colors(v, levels(x$val)), type = "bar", text = ~lab,
              hoverinfo = "text", textposition = "none") %>%
        layout(barmode = "stack", yaxis = list(tickformat = ".0%", title = var_label(v)), xaxis = list(title = "")) %>% plot_config()
    }
  })
  output$dnote <- renderUI({
    v <- input$d_var; req(v, mask(), VARS$type[VARS$var == v] != "cont")
    if (any(dcat()$code > 0L)) note("Masked cells (<10, and the cells suppressed to protect them) are left out of the bars.")
  })
  output$dsum <- renderDT({
    v <- input$d_var; req(v, input$d_group); ty <- VARS$type[VARS$var == v]; g <- input$d_group
    s <- if (ty == "cont") {
      grp(dfv(), g) %>% group_by(Group = .data[[g]]) %>%
        summarise(n = sum(!is.na(.data[[v]])), Missing = sprintf("%.1f%%", 100 * mean(is.na(.data[[v]]))),
                  `Mean (SD)` = mean_sd(.data[[v]]), `Median [IQR]` = med_iqr(.data[[v]]),
                  Min = round(safe_min(.data[[v]]), 1), Max = round(safe_max(.data[[v]]), 1)) %>%
        mutate(hide = mask() & n < 10,                       # groups with < 10 values are not described
               across(c(`Mean (SD)`, `Median [IQR]`), ~ if_else(hide, "–", .x)),
               across(c(Min, Max), ~ if_else(hide, NA_real_, .x)),
               N = mask_n(n, mask())) %>%
        select(Group, N, Missing, `Mean (SD)`, `Median [IQR]`, Min, Max)
    } else {
      dcat() %>% filter(n > 0 | code > 0L) %>%
        transmute(Group = .gg, Value = val, `n (%)` = mask_fmt(code, n_pct(n, N)))
    }
    summary_dt(s)
  })

  # outcomes with Wilson CIs
  observeEvent(input$o_group, {                    # no stratum equal to the grouping variable
    ch <- DEF$o_strata[DEF$o_strata != input$o_group]
    updateSelectInput(session, "o_strat", choices = ch, selected = if (isTRUE(input$o_strat %in% ch)) input$o_strat else "")
  })
  otab <- reactive({ req(input$o_var, input$o_group)
    st <- input$o_strat %||% ""; if (!nzchar(st) || st == input$o_group) st <- NULL
    d <- grp(dfv(), input$o_group)
    if (!is.null(st)) d <- d[!is.na(d[[st]]), , drop = FALSE]
    need_rows(d)
    prop_by(d, input$o_var, input$o_group, st) %>% mutate(masked = mask_bin(k, n, mask())) })
  output$oplot <- renderPlotly({
    o <- otab() %>% filter(n >= 20, !masked)
    validate(need(nrow(o) > 0, "No groups with at least 20 observations and unmasked counts."))
    plot_ly(o, y = ~reorder(group, p), x = ~p, type = "scatter", mode = "markers", marker = list(size = 9, color = "#8a5a44"),
            error_x = list(type = "data", symmetric = FALSE, array = ~hi - p, arrayminus = ~p - lo, color = "#68727c"),
            text = ~sprintf("%s<br>%.1f%% (95%% CI %.1f-%.1f)<br>n = %s", group, 100 * p, 100 * lo, 100 * hi, mask_n(n, mask())), hoverinfo = "text") %>%
      layout(xaxis = list(title = var_label(input$o_var), tickformat = ".0%"), yaxis = list(title = ""), margin = list(l = 10)) %>% plot_config()
  })
  output$ocap <- renderUI({
    o <- otab(); few <- sum(o$n < 20); hid <- sum(o$n >= 20 & o$masked)
    note(if (few) sprintf("%d group%s with n < 20 not plotted.", few, if (few == 1) "" else "s"),
         if (hid) sprintf("%d group%s with masked counts (1-9 events or non-events) not plotted.", hid, if (hid == 1) "" else "s"))
  })
  output$otab <- renderDT(summary_dt(otab() %>% transmute(Group = group, Events = mask_k(k, n, mask()), N = mask_n(n, mask()),
                                                       `Proportion (95% CI)` = if_else(masked, "–", sprintf("%.1f%% (%.1f-%.1f)", 100 * p, 100 * lo, 100 * hi)))))

  # Kaplan-Meier (index admissions only, see km_data())
  sdat <- reactive({
    g <- input$s_group; hz <- input$s_days; req(g, hz)
    validate(need(length(SURV_SOURCES) > 0, "No death times (time_to_death_from_admission) in this schema."))
    km_data(grp(dfv(), g), g, hz) %>% group_by(gg) %>% filter(n() >= 20) %>%
      mutate(.masked = mask_bin(sum(status), n(), mask())) %>% ungroup()
  })
  sfit <- reactive({
    d <- sdat() %>% filter(!.masked)
    validate(need(nrow(d) > 0, "No groups with at least 20 patients and unmasked death counts from sources with death times."))
    list(d = d, fit = survival::survfit(survival::Surv(time, status) ~ gg, data = d))
  })
  strata_of <- function(s, d) if (is.null(s$strata)) rep(unique(d$gg), length(s$time)) else sub("^gg=", "", as.character(s$strata))
  output$splot <- renderPlotly({
    sf <- sfit(); d <- sf$d; hz <- input$s_days
    s <- summary(sf$fit, times = sort(unique(c(0, d$time[d$status == 1], hz))), extend = TRUE)
    k <- tibble(strata = strata_of(s, d), time = s$time, surv = s$surv, lo = coalesce(s$lower, 0), hi = coalesce(s$upper, 1))
    cols <- group_colors(input$s_group, sort(unique(k$strata)))
    p <- blank_plot()
    for (st in unique(k$strata)) { kk <- k %>% filter(strata == st); col <- unname(cols[st])
      if (input$s_ci) { lo <- step_xy(kk$time, kk$lo); hi <- step_xy(kk$time, kk$hi)
        p <- p %>% add_ribbons(inherit = FALSE, x = lo$x, ymin = lo$y, ymax = hi$y, legendgroup = st, showlegend = FALSE,
                               fillcolor = hex_rgba(col, 0.18), line = list(width = 0), hoverinfo = "skip") }
      p <- p %>% add_trace(inherit = FALSE, x = kk$time, y = kk$surv, type = "scatter", mode = "lines",
                           line = list(shape = "hv", color = col), name = st, legendgroup = st) }
    p %>% layout(xaxis = list(title = "Days from admission"), yaxis = list(title = "Survival", tickformat = ".0%", range = c(0, 1.02)),
                 legend = list(orientation = "h", y = -0.2)) %>% plot_config()
  })
  output$scap <- renderUI({
    hid <- unique(sdat()$gg[sdat()$.masked])
    note(if (length(hid)) sprintf("Not plotted (masked: 1-9 deaths or survivors): %s.", paste(hid, collapse = ", ")))
  })
  output$srisk <- renderDT({
    sf <- sfit(); hz <- input$s_days
    tt <- pretty(c(0, hz)); tt <- tt[tt <= hz]
    s <- summary(sf$fit, times = tt, extend = TRUE)
    r <- tibble(Group = strata_of(s, sf$d), time = sprintf("Day %g", s$time), n = mask_n(s$n.risk, mask())) %>%
      tidyr::pivot_wider(names_from = time, values_from = n)
    summary_dt(r)
  })
  output$stab <- renderDT({
    d <- sdat(); req(nrow(d) > 0)
    lr <- tryCatch(survival::survdiff(survival::Surv(time, status) ~ gg, data = d), error = function(e) NULL)
    pv <- if (!is.null(lr) && length(lr$n) > 1) fmt_p(1 - stats::pchisq(lr$chisq, length(lr$n) - 1)) else ""
    med <- tryCatch({
      f <- survival::survfit(survival::Surv(time, status) ~ gg, data = d)
      s <- summary(f)$table
      if (is.matrix(s)) setNames(ifelse(is.na(s[, "median"]), "-", sprintf("%.0f d", s[, "median"])), sub("^gg=", "", rownames(s)))
      else setNames(ifelse(is.na(s["median"]), "-", sprintf("%.0f d", s["median"])), unique(d$gg))
    }, error = function(e) NULL)
    t <- d %>% group_by(Group = gg) %>% summarise(n = n(), k = sum(status), masked = first(.masked)) %>%
      mutate(med = if (is.null(med)) "-" else unname(med[as.character(Group)])) %>%
      transmute(Group, N = mask_n(n, mask()), Deaths = mask_k(k, n, mask()),
                `Survival at horizon` = if_else(masked, "–", sprintf("%.1f%%", 100 * (1 - k / n))),
                `Median survival` = if_else(masked, "–", coalesce(med, "-")))
    summary_dt(t %>% mutate(`Log-rank p` = c(pv, rep("", n() - 1))))
  })

  # trends: MIMIC-IV anchor_year_group when available, else the (shifted) admission year
  tr_data <- reactive({
    d <- dfv()
    grouped <- "anchor_year_group" %in% names(d) && any(!is.na(d$anchor_year_group))
    if (grouped) d$.yr <- as.character(d$anchor_year_group)
    else if ("year" %in% names(d)) d$.yr <- d$year
    else return(NULL)
    list(d = d[!is.na(d$.yr), , drop = FALSE], grouped = grouped,
         dropped = setdiff(unique(d$source), unique(d$source[!is.na(d$.yr)])))
  })
  tr_axis <- function(tr) {
    if (tr$grouped) return(list(title = "Anchor year group", type = "category"))
    r <- range(tr$d$.yr)
    list(title = "Admission year (shifted per patient)", tickformat = "d", dtick = max(1, ceiling(diff(r) / 12)))
  }
  output$trplot <- renderPlotly({
    tr <- tr_data(); validate(need(!is.null(tr) && nrow(tr$d) > 0, "No admission years in this selection."))
    g <- input$tr_group; d <- tr$d; if (g != ".all") d <- d[!is.na(d[[g]]), , drop = FALSE]
    x <- d %>% count(.yr, gg = as.character(.data[[g]])) %>% tidyr::complete(.yr, gg, fill = list(n = 0L)) %>%
      group_by(.yr) %>% mutate(pct = n / sum(n)) %>% ungroup()
    x$code <- 0L
    if (mask()) { m <- stats::xtabs(n ~ gg + .yr, x); cm <- small_cell_mask(m, cols = TRUE)
                  x$code <- cm[cbind(match(x$gg, rownames(m)), match(as.character(x$.yr), colnames(m)))] }
    share <- identical(input$tr_scale, "pct") && g != ".all"
    x <- x %>% filter(n > 0, code == 0L) %>% mutate(y = if (share) pct else n)
    if (!nrow(x)) return(empty_plot("Every year is masked by the small-cell rule."))
    plot_ly(x, x = ~.yr, y = ~y, color = ~gg, colors = group_colors(g, lv_of(d[[g]])), type = "bar",
            text = ~sprintf("%s | %s<br>%s (%.1f%%)", gg, .yr, mask_n(n, mask()), 100 * pct), hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "stack", xaxis = tr_axis(tr),
             yaxis = list(title = if (share) "Share of the year" else tools::toTitleCase(unit_word()), tickformat = if (share) ".0%" else ","),
             legend = list(orientation = "h", y = -0.2)) %>% plot_config()
  })
  tr_rate <- reactive({
    req(input$tr_var)
    tr <- tr_data(); validate(need(!is.null(tr) && nrow(tr$d) > 0, "No admission years in this selection."))
    v <- tr$d[[input$tr_var]]; validate(need(sum(!is.na(v)) > 0, "No values for this outcome."))
    x <- tr$d %>% mutate(val = suppressWarnings(as.numeric(v))) %>% filter(!is.na(val)) %>%
      group_by(.yr) %>% summarise(n = n(), k = sum(val > 0), .groups = "drop") %>% mutate(masked = mask_bin(k, n, mask()))
    few <- sum(x$n < 20); hid <- sum(x$n >= 20 & x$masked)
    x <- x %>% filter(n >= 20, !masked)
    list(x = bind_cols(x, wilson(x$k, x$n)), few = few, hid = hid, tr = tr)
  })
  output$trrate <- renderPlotly({
    r <- tr_rate(); x <- r$x
    if (!nrow(x)) return(empty_plot(sprintf("No year with at least 20 %s and unmasked counts.", unit_word())))
    plot_ly(x, x = ~.yr, y = ~p, type = "scatter", mode = "lines+markers",
            marker = list(color = "#37474f"), line = list(color = "#37474f"),
            text = ~sprintf("%s | %s: %.1f%% (%.1f-%.1f), n = %s", .yr, var_label(input$tr_var), 100 * p, 100 * lo, 100 * hi, mask_n(n, mask())),
            hoverinfo = "text", error_y = list(type = "data", symmetric = FALSE, array = ~hi - p, arrayminus = ~p - lo, color = "#68727c")) %>%
      layout(xaxis = tr_axis(r$tr), yaxis = list(title = "Rate", tickformat = ".1%")) %>% plot_config()
  })
  output$trcap <- renderUI({
    tr <- tr_data(); req(!is.null(tr))
    r <- tryCatch(tr_rate(), error = function(e) NULL)
    note(if (tr$grouped) "Years are MIMIC-IV anchor_year_group windows (3-year calendar ranges)."
         else "Admission years are shifted per patient for de-identification, so the bars show the spread of shifted years, not a calendar trend.",
         if (length(tr$dropped)) sprintf("Not included (no admission year): %s.", paste(SRC_LABEL[tr$dropped], collapse = ", ")),
         if (!is.null(r) && r$few) sprintf("%d year%s with fewer than 20 %s not shown in the rate plot.", r$few, if (r$few == 1) "" else "s", unit_word()),
         if (!is.null(r) && r$hid) sprintf("%d year%s with masked counts not shown.", r$hid, if (r$hid == 1) "" else "s"))
  })
  output$dl_sel <- downloadHandler(
    filename = function() sprintf("cohort_selection_%s.csv", format(Sys.Date())),
    content = function(file) utils::write.csv(df() %>% select(any_of(DEF$data_cols)), file, row.names = FALSE)
  )

  # R code for the current selection: active filters, plus fixed-choice filters that drop missing values even untouched
  code_text <- reactive({
    keys <- names(input)
    fs <- Filter(function(f) { id <- paste0("f_", f$var)
      filter_active(f, input[[id]], id %in% keys) || (f$type %in% c("checkbox", "picker") && is.null(f$na_label) && isFALSE(f$keep_na)) }, EX_FILTERS)
    paste(c(sprintf("# Cohort Explorer selection: %s %s.", mask_n(nrow(df()), mask()), unit_word()),
            "# creds <- list(postgres = list(host = \"localhost\", port = 5432,",
            "#                               user = \"...\", password = \"...\"))",
            sprintf("rendehR::ehr_connect(db_name = \"%s\", creds = creds)", .ehr_env$db_name %||% "rendehr"),
            sprintf("b <- rendehR::explorer_data(\"%s\")", D$meta$schema),
            filter_code(fs, lapply(fs, function(f) input[[paste0("f_", f$var)]]), input$unit)), collapse = "\n")
  })
  observeEvent(input$show_code, showModal(modalDialog(
    title = "R code for this selection", size = "l", easyClose = TRUE, footer = modalButton("Close"),
    tags$p(class = "small text-muted", "Select the code and copy it (Ctrl+C or Cmd+C)."), tags$pre(code_text()))))

  # trajectories: per-admission bin means from the database (cached per query), summarised over the selection
  tj_cache <- list()
  tj_raw <- eventReactive(input$tj_go, {
    req(LIVE, input$tj_cn)
    q <- list(cn = input$tj_cn, clock = input$tj_clock, hz = as.numeric(input$tj_hz), bw = as.numeric(input$tj_bw))
    key <- paste(q, collapse = "|")
    if (is.null(tj_cache[[key]])) {
      x <- tryCatch(withProgress(message = sprintf("Computing %s...", q$cn), trajectory_query(q$cn, q$clock, q$hz, q$bw)),
                    error = function(e) e)
      if (inherits(x, "error")) return(c(q, error = conditionMessage(x)))
      tj_cache[[key]] <<- x
    }
    c(q, list(x = tj_cache[[key]]))
  })
  tj <- reactive({
    r <- tj_raw(); validate(need(is.null(r$error), r$error %||% ""))
    g <- input$tj_group; req(g)
    d <- grp(dfv(), g)
    k <- match(paste(r$x$source, r$x$adm_id), paste(d$source, d$adm_id))
    x <- r$x[!is.na(k), , drop = FALSE]
    x$group <- if (g == ".all") rep("Overall", nrow(x)) else as.character(d[[g]][k[!is.na(k)]])
    validate(need(nrow(x) > 0, sprintf("No values for %s in the selected %s.", r$cn, unit_word())))
    c(r, list(s = traj_stats(x, r$bw, mask()), g = g, lv = intersect(if (g == ".all") "Overall" else lv_of(d[[g]]), x$group)))
  })
  output$tjplot <- renderPlotly({
    t <- tj(); s <- t$s
    if (!nrow(s)) return(empty_plot("Every time bin has fewer than 10 admissions."))
    cols <- group_colors(t$g, t$lv)
    p <- blank_plot()
    for (gg in intersect(t$lv, s$group)) {
      z <- s[s$group == gg, ]; col <- unname(cols[gg]); x <- z$start + t$bw / 2
      p <- p %>% add_ribbons(inherit = FALSE, x = x, ymin = z$q25, ymax = z$q75, legendgroup = gg, showlegend = FALSE,
                             fillcolor = hex_rgba(col, 0.18), line = list(width = 0), hoverinfo = "skip") %>%
        add_trace(inherit = FALSE, x = x, y = z$median, type = "scatter", mode = "lines+markers", name = gg, legendgroup = gg,
                  line = list(color = col), marker = list(color = col, size = 5), hoverinfo = "text",
                  text = sprintf("%s | %g-%g h<br>median %s [%s-%s], n = %s", gg, z$start, z$start + t$bw, signif(z$median, 3),
                                 signif(z$q25, 3), signif(z$q75, 3), mask_n(z$n, mask())))
    }
    p %>% layout(xaxis = list(title = if (t$clock == "icu") "Hours since ICU admission" else "Hours since hospital admission",
                              range = c(0, t$hz)),
                 yaxis = list(title = t$cn), legend = list(orientation = "h", y = -0.2)) %>% plot_config()
  })
  output$tjnote <- renderUI({
    t <- tj(); k <- attr(t$s, "dropped")
    note(sprintf("Each admission's mean per %g-hour bin; lines are the median across admissions, bands the 25th-75th percentiles.", t$bw),
         if (k) sprintf("%d group-bin%s with fewer than 10 admissions not shown (small-cell rule).", k, if (k == 1) "" else "s"))
  })
  output$tjtab <- renderDT({
    t <- tj()
    w <- t$s %>% transmute(Hours = sprintf("%g-%g", start, start + t$bw), start, group = factor(group, t$lv),
                           cell = sprintf("%s [%s\u2013%s], n = %s", signif(median, 3), signif(q25, 3), signif(q75, 3), mask_n(n, mask()))) %>%
      arrange(group) %>% tidyr::pivot_wider(names_from = group, values_from = cell, values_fill = "") %>% arrange(start) %>% select(-start)
    summary_dt(w)
  })

  # compare: A is a stored selection, B the current one; an admission in both is counted in both columns
  one_line <- function(x) tags$span(class = "d-block text-truncate", title = x, x)   # equal box heights; full text on hover
  cmp_a <- reactiveVal(NULL)
  observeEvent(input$cmp_set, cmp_a(list(d = df(), desc = paste0(unit_word(), "; ", filter_desc(active())))))
  cmp <- reactive({
    a <- cmp_a(); req(a); b <- df()
    validate(need(nrow(a$d) > 0 && nrow(b) > 0, "Selection A or B is empty."))
    list(a = a$d, b = b, d = bind_rows(mutate(a$d, .sel = "A"), mutate(b, .sel = "B")))
  })
  output$cmp_sizes <- renderUI({
    a <- cmp_a(); req(a); b <- df()
    both <- sum(paste(a$d$source, a$d$adm_id) %in% paste(b$source, b$adm_id))
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      stat_box("Selection A", mask_n(nrow(a$d), mask()), "pin-angle", sub = one_line(a$desc)),
      stat_box("Selection B", mask_n(nrow(b), mask()), "funnel", sub = one_line(paste0(unit_word(), "; ", filter_desc(active())))),
      stat_box("In both", mask_n(both, mask()), "intersect", sub = "Same admission in A and B"))
  })
  output$cmp_t1 <- renderDT({
    req(length(input$cmp_vars) > 0)
    x <- cmp(); vars <- VARS %>% filter(var %in% input$cmp_vars) %>% arrange(match(var, input$cmp_vars))
    # Overall, missingness and p-values would count overlapping admissions twice
    t1 <- char_table(x$d, ".sel", vars, mask = mask()) %>% select(variable, level, A, B)
    t1 <- with_smd(t1, x$d, ".sel", "A", "B", vars, hide = mask() && min(nrow(x$a), nrow(x$b)) < 10)
    summary_dt(t1 %>% mutate(variable = if_else(duplicated(variable), "", variable)) %>% rename(Variable = variable, Level = level))
  })
  output$cmp_note <- renderUI(note(if (mask()) "Small-cell rule as in Characteristics.",
                                   "Admissions in both selections count in both columns, so no p-values are shown."))
  output$cmp_out <- renderDT({
    x <- cmp()
    ov <- VARS$var[VARS$type == "bin" & VARS$group == "Outcomes"]; if (!length(ov)) ov <- VARS$var[VARS$type == "bin"]
    validate(need(length(ov) > 0, "No binary outcomes."))
    r <- bind_rows(lapply(ov, function(v) prop_by(x$d, v, ".sel") %>% mutate(Outcome = var_label(v)))) %>%
      mutate(masked = mask_bin(k, n, mask()),
             `Events / N` = paste(mask_k(k, n, mask()), "/", mask_n(n, mask())),
             `Rate (95% CI)` = if_else(masked, "\u2013", sprintf("%.1f%% (%.1f-%.1f)", 100 * p, 100 * lo, 100 * hi)))
    summary_dt(tidyr::pivot_wider(r, id_cols = Outcome, names_from = group, values_from = c(`Events / N`, `Rate (95% CI)`),
                                  names_glue = "{group}: {.value}", names_vary = "slowest"))
  })

  # cross-tab
  xt <- reactive({ req(input$x_row, input$x_col, input$x_pct)
    d <- dfv() %>% filter(!is.na(.data[[input$x_row]]), !is.na(.data[[input$x_col]]))
    need_rows(d, " with both variables")
    x <- d %>% count(r = as.character(.data[[input$x_row]]), c = as.character(.data[[input$x_col]])) %>%
      tidyr::complete(r, c, fill = list(n = 0L))
    x <- switch(input$x_pct, row = group_by(x, r), col = group_by(x, c), tot = x) %>% mutate(den = sum(n), p = n / den) %>% ungroup()
    x$code <- 0L
    if (mask()) { m <- t(stats::xtabs(n ~ r + c, x)); cm <- group_codes(m, table(as.character(dfv()[[input$x_row]])))
                  x$code <- cm[cbind(match(x$c, rownames(m)), match(x$r, colnames(m)))] }
    x })
  output$xplot <- renderPlotly({
    x <- xt() %>% mutate(z = if_else(code > 0L, NA_real_, p),
                         lab = sprintf("%s | %s<br>%s", r, c, mask_fmt(code, n_pct(n, den))))
    if (all(is.na(x$z))) return(empty_plot("Every cell is masked by the small-cell rule."))
    plot_ly(x, x = ~c, y = ~r, z = ~z, type = "heatmap", zmin = 0, zmax = max(c(x$z, 0.01), na.rm = TRUE),
            colorscale = list(list(0, "#f4f6f8"), list(1, "#1f3b57")),
            colorbar = list(title = switch(input$x_pct, row = "Row %", col = "Column %", tot = "Share of total"), tickformat = ".0%"),
            text = ~lab, hoverinfo = "text") %>%
      layout(xaxis = list(title = var_label(input$x_col)), yaxis = list(title = ""), margin = list(l = 10)) %>% plot_config()
  })
  output$xtest <- renderUI({ x <- xt(); tb <- stats::xtabs(n ~ r + c, x)
    if (min(dim(tb)) < 2) return(NULL); ct <- suppressWarnings(stats::chisq.test(tb, correct = FALSE))
    tags$p(class = "small", sprintf("Chi-square = %.1f, df = %d, p = %s; Cramér's V = %.3f%s%s", ct$statistic, ct$parameter, fmt_p(ct$p.value),
                                    sqrt(ct$statistic / (sum(tb) * (min(dim(tb)) - 1))),
                                    if (any(ct$expected < 5)) " (some expected counts < 5: interpret with care)" else "",
                                    if (any(x$code > 0L)) ". Blank cells: masked by the small-cell rule" else "")) })
  output$xtab <- renderDT({ x <- xt() %>% mutate(cell = mask_fmt(code, n_pct(n, den))) %>% select(r, c, cell) %>%
    tidyr::pivot_wider(names_from = c, values_from = cell)
    names(x)[1] <- var_label(input$x_row)
    summary_dt(x) })

  # scatter: plot and correlation use the same rows
  scd <- reactive({
    g <- input$sc_col; req(g, input$sc_x, input$sc_y)
    grp(dfv(), g) %>% filter(!is.na(.data[[input$sc_x]]), !is.na(.data[[input$sc_y]])) %>%
      mutate(.x = .data[[input$sc_x]], .y = .data[[input$sc_y]], .gg = if (g == ".all") "Overall" else as.character(.data[[g]]))
  })
  output$scplot <- renderPlotly({
    d <- scd(); g <- input$sc_col
    if (input$sc_log) d <- d %>% filter(.y > 0)
    validate(need(nrow(d) > 1, sprintf("No %s with both values.", unit_word())))
    cols <- group_colors(g, sort(unique(d$.gg)))
    if (nrow(d) > 8000) d <- d %>% slice_sample(n = 8000)
    plot_ly(d, x = ~.x, y = ~.y, color = ~.gg, colors = cols, type = "scattergl", mode = "markers", marker = list(size = 4, opacity = 0.5)) %>%
      layout(xaxis = list(title = var_label(input$sc_x)), yaxis = list(title = var_label(input$sc_y), type = if (input$sc_log) "log" else "linear"),
             legend = list(orientation = "h", y = -0.2)) %>%
      plot_config()
  })
  output$sccor <- renderUI({
    d <- scd(); req(nrow(d) > 2)
    ct <- suppressWarnings(stats::cor.test(d$.x, d$.y, method = "spearman", exact = FALSE))
    nz <- if (input$sc_log) sum(d$.y <= 0) else 0
    pv <- fmt_p(ct$p.value)
    tags$p(class = "small text-muted", sprintf("Spearman rho = %.3f (%s), n = %s%s%s", ct$estimate, if (startsWith(pv, "<")) paste("p", pv) else paste("p =", pv),
                                    mask_n(nrow(d), mask()),
                                    if (nrow(d) - nz > 8000) "; 8,000 points plotted" else "",
                                    if (nz > 0) sprintf("; %s points with Y <= 0 not shown on the log scale", mask_n(nz, mask())) else "")) })

  # data table
  output$data <- renderDT(datatable(dfv() %>% select(all_of(DEF$data_cols)), rownames = FALSE, selection = "single", filter = "top",
                                    colnames = unname(dplyr::recode(var_label(DEF$data_cols), patient_id = "Patient", adm_id = "Admission",
                                                                    source_label = "Source")),
                                    extensions = "Buttons", options = list(pageLength = 15, scrollX = TRUE, dom = .dt_dom_data, buttons = c("copy", "csv")),
                                    class = "compact stripe hover nowrap"))
  observeEvent(input$open_subject, {
    i <- input$data_rows_selected
    if (!length(i)) { showNotification("Select a row first.", type = "warning"); return() }
    r <- df()[i, ]; goto_subject(r$source, r$patient_id, r$adm_id)
  })
})

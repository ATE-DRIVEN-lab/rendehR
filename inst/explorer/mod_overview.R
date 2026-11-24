# ---- Overview: headline numbers, cohort composition and outcomes by source,
# plus the cohort's shape: age pyramid, length of stay and age distribution.
# (No calendar accrual: MIMIC dates are shifted per patient.) All plots come
# from the bundle, so the page works
# offline; the live connection only enriches the other pages.

overview_ui <- function(id) {
  ns <- NS(id)
  ck <- D$checks
  n_icu <- sum(ADM$n_icu, na.rm = TRUE)
  sex_n <- if ("sex" %in% names(ADM)) ADM %>% count(sex) else tibble(sex = character(), n = integer())
  sex_sub <- if (nrow(sex_n)) paste(sprintf("%s %s", tools::toTitleCase(as.character(sex_n$sex)), fmt_n(sex_n$n)), collapse = " · ") else NULL
  med_los <- if ("hospital_los" %in% names(ADM) && any(!is.na(ADM$hospital_los)))
    sprintf("median %.1f d", stats::median(ADM$hospital_los, na.rm = TRUE)) else NULL
  tagList(
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 6, md = 4, xl = 2), class = "rh-stat-row",
      stat_box("Cohort patients", fmt_n(nrow(PATIENTS)), "people", sub = sex_sub),
      stat_box("Cohort admissions", fmt_n(nrow(ADM)), "hospital", sub = med_los),
      stat_box("All admissions", fmt_n(nrow(CTX_ADM)), "clock-history"),
      stat_box("Cohort ICU stays", fmt_n(n_icu), "heart-pulse"),
      stat_box("Sources", length(SRC), "database"),
      stat_box("Data checks passed", checks_summary(ck)$text, "check2-circle", checks_summary(ck)$tone)),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE, card_header("Admissions over time (dataset pulse)"),
           tags$p(class = "rh-pulse-note",
                  "Shifted admission years (or MIMIC-IV anchor year groups when present). Not a calendar accrual curve."),
           withSpinner(plotlyOutput(ns("pulse"), height = "220px"))),
      card(card_header("About this data"),
           tags$dl(class = "row small mb-2",
             tags$dt(class = "col-5 fw-normal text-muted", "Merged schema"), tags$dd(class = "col-7", D$meta$schema),
             tags$dt(class = "col-5 fw-normal text-muted", "Level"), tags$dd(class = "col-7", D$meta$level[1] %||% "-"),
             tags$dt(class = "col-5 fw-normal text-muted", "Sources"), tags$dd(class = "col-7", paste(unname(SRC_LABEL), collapse = ", ")),
             tags$dt(class = "col-5 fw-normal text-muted", "Loaded"), tags$dd(class = "col-7", D$meta$built),
             tags$dt(class = "col-5 fw-normal text-muted", "rendehR"), tags$dd(class = "col-7", D$meta$rendehR_version)),
           tags$p(class = "text-muted small mb-0",
                  "Individual-level clinical data: use it on this machine, under each source's data use agreement."))),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE, card_header("Cohort composition by source"),
           layout_columns(col_widths = c(5, 4, 3),
             selectInput(ns("group"), NULL, GROUP_CHOICES[!GROUP_CHOICES %in% c(".all", "source_label")], DEF$overview_group),
             radioButtons(ns("unit"), NULL, c("Admissions" = "adm", "Patients (index admission)" = "pat"), inline = TRUE),
             radioButtons(ns("scale"), NULL, c("Percentage" = "pct", "Count" = "n"), inline = TRUE)),
           withSpinner(plotlyOutput(ns("comp"), height = "420px"))),
      card(full_screen = TRUE, card_header("Outcomes by source"),
           tags$p(class = "small text-muted mb-1", "Share of cohort admissions, 95% Wilson interval. ICU mortality: admissions with an ICU stay."),
           withSpinner(plotlyOutput(ns("outc"), height = "440px")))),
    layout_columns(col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("Age pyramid (index admissions)"),
           withSpinner(plotlyOutput(ns("pyramid"), height = "360px"))),
      card(full_screen = TRUE, card_header("Hospital length of stay by source"),
           layout_columns(col_widths = c(8, 4),
             tags$p(class = "small text-muted mb-1", "Box: interquartile range; line: median. 0-day stays are left out on the log scale."),
             checkboxInput(ns("los_log"), "Log scale", TRUE)),
           withSpinner(plotlyOutput(ns("losplot"), height = "310px")))),
    layout_columns(col_widths = c(12),
      card(full_screen = TRUE, card_header("Age at admission by source"),
           tags$p(class = "small text-muted mb-1", "Share of each source's admissions per 5-year band."),
           withSpinner(plotlyOutput(ns("age"), height = "330px"))))
  )
}
overview_server <- function(id) moduleServer(id, function(input, output, session) {
  output$pulse <- renderPlotly({
    d <- ADM
    grouped <- "anchor_year_group" %in% names(d) && any(!is.na(d$anchor_year_group))
    if (grouped) {
      x <- d %>% filter(!is.na(anchor_year_group)) %>% count(yr = as.character(anchor_year_group), source_label)
      xlab <- "Anchor year group"
    } else if ("year" %in% names(d) && any(!is.na(d$year))) {
      x <- d %>% filter(!is.na(year)) %>% count(yr = as.character(year), source_label)
      xlab <- "Admission year (shifted)"
    } else return(empty_plot("No admission year in this schema."))
    plot_ly(x, x = ~yr, y = ~n, color = ~source_label, colors = SRC_COLORS, type = "bar",
            text = ~sprintf("%s<br>%s: %s", source_label, yr, fmt_n(n)), hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "stack", xaxis = list(title = xlab, categoryorder = "category ascending"),
             yaxis = list(title = "Admissions", tickformat = ","),
             legend = list(orientation = "h", y = -0.35), margin = list(b = 60)) %>% plot_config()
  })
  output$comp <- renderPlotly({
    g <- input$group; req(g)
    d <- if (input$unit == "pat") ADM %>% filter(is_index) else ADM
    d <- d %>% mutate(.v = coalesce(as.character(.data[[g]]), "(missing)")) %>% count(source_label, .v) %>%
      group_by(source_label) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
      mutate(y = if (input$scale == "pct") pct else n)
    cols <- group_colors(g, sort(unique(d$.v)))
    plot_ly(d, y = ~source_label, x = ~y, color = ~.v, colors = cols, type = "bar", orientation = "h",
            text = ~ifelse(n < 10, sprintf("%s<br>%s: <10", source_label, .v),
                           sprintf("%s<br>%s: %s (%.1f%%)", source_label, .v, fmt_n(n), 100 * pct)), hoverinfo = "text",
            textposition = "none") %>%
      layout(barmode = "stack", xaxis = list(title = if (input$scale == "pct") "Share" else "Count",
                                             tickformat = if (input$scale == "pct") ".0%" else ","),
             yaxis = list(title = ""), legend = list(orientation = "h", y = -0.2)) %>% plot_config()
  })
  output$outc <- renderPlotly({
    outs <- intersect(c("hospital_mortality", "icu_death", "readmission_30d"), names(ADM))
    validate(need(length(outs) > 0, "No binary outcomes in this schema."))
    d <- bind_rows(lapply(outs, function(o) prop_by(ADM, o, "source_label") %>% mutate(outcome = var_label(o)))) %>%
      filter(n > 0)
    validate(need(nrow(d) > 0, "No outcome values."))
    # sources side by side within each outcome instead of on top of each other
    lv <- unique(d$outcome); gl <- sort(unique(d$group))
    d <- d %>% mutate(y = match(outcome, lv) + (match(group, gl) - (length(gl) + 1) / 2) * 0.22)
    plot_ly(d, y = ~y, x = ~p, color = ~group, colors = SRC_COLORS, type = "scatter", mode = "markers",
            marker = list(size = 9), error_x = list(type = "data", symmetric = FALSE, array = ~hi - p, arrayminus = ~p - lo),
            text = ~sprintf("%s | %s<br>%.1f%% (95%% CI %.1f-%.1f), n = %s", group, outcome, 100 * p, 100 * lo, 100 * hi, fmt_n(n)),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Share of admissions (95% CI)", tickformat = ".0%", rangemode = "tozero"),
             yaxis = list(title = "", tickvals = seq_along(lv), ticktext = lv, autorange = "reversed"),
             legend = list(orientation = "h", y = -0.22)) %>% plot_config()
  })
  output$age <- renderPlotly({
    validate(need("age" %in% names(ADM) && any(!is.na(ADM$age)), "No age in this schema."))
    d <- ADM %>% filter(!is.na(age)) %>% mutate(band = 5 * floor(age / 5)) %>% count(source_label, band) %>%
      group_by(source_label) %>% mutate(p = n / sum(n)) %>% ungroup()
    plot_ly(d, x = ~band, y = ~p, color = ~source_label, colors = SRC_COLORS, type = "scatter", mode = "lines",
            line = list(shape = "hvh", width = 2),
            text = ~sprintf("%s, %d-%d years: %.1f%%", source_label, band, band + 4, 100 * p), hoverinfo = "text") %>%
      layout(xaxis = list(title = "Age, years"), yaxis = list(title = "Share of admissions", tickformat = ".0%", rangemode = "tozero"),
             legend = list(orientation = "h", y = -0.3)) %>% plot_config()
  })
  output$pyramid <- renderPlotly({
    validate(need(all(c("sex", "age_group") %in% names(ADM)), "No sex/age group in this schema."))
    d <- ADM %>% filter(is_index, sex %in% c("female", "male"), !is.na(age_group)) %>% count(sex, age_group)
    validate(need(nrow(d) > 0, "No index admissions with sex and age."))
    lv <- intersect(.xp_age_levels, unique(d$age_group))
    d <- d %>% mutate(age_group = factor(age_group, lv), signed = if_else(tolower(substr(sex, 1, 1)) == "f", -n, n)) %>%
      mutate(lab = sprintf("%s, %s: %s", sex, age_group, mask_n(n)))
    cols <- PALETTES$sex
    tk <- pretty(c(0, max(d$n)), 3); tk <- sort(unique(c(-tk, tk)))
    plot_ly(d, y = ~age_group, x = ~signed, color = ~sex, colors = cols, type = "bar", orientation = "h",
            text = ~lab, hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "overlay", bargap = 0.15,
             xaxis = list(title = "Index admissions (female left, male right)", tickvals = tk, ticktext = fmt_n(abs(tk))),
             yaxis = list(title = ""), legend = list(orientation = "h", x = 0, y = 1.08)) %>%
      plot_config()
  })
  output$losplot <- renderPlotly({
    validate(need("hospital_los" %in% names(ADM) && any(!is.na(ADM$hospital_los)), "No hospital length of stay in this schema."))
    d <- ADM %>% filter(!is.na(hospital_los), hospital_los > 0)
    validate(need(nrow(d) > 0, "No positive lengths of stay."))
    plot_ly(d, y = ~source_label, x = ~hospital_los, color = ~source_label, colors = SRC_COLORS, type = "box",
            boxpoints = "suspectedoutliers", marker = list(size = 3, opacity = 0.4), line = list(width = 1.2),
            hoverinfo = "y+x", showlegend = FALSE) %>%
      layout(xaxis = c(list(title = "Hospital length of stay, days"),
                       if (isTRUE(input$los_log)) list(type = "log", tickvals = c(0.1, 0.3, 1, 3, 10, 30, 100, 300),
                                                       ticktext = c("0.1", "0.3", "1", "3", "10", "30", "100", "300"))
                       else list(type = "linear")),
             yaxis = list(title = "")) %>% plot_config()
  })
})

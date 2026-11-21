# ---- Routine blood biomarkers: admission values, contrasts, trajectories, abnormal rates, correlations -------------------
bio_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(border = FALSE, border_radius = FALSE, fillable = TRUE, 
    sidebar = sidebar(width = 290, title = "Selection",
      checkboxGroupInput(ns("src"), "Source", stats::setNames(SRC, SRC_LABEL[SRC]), selected = SRC),
      pickerInput(ns("cat"), "SCI-D category", CAT_LEVELS, selected = CAT_LEVELS, multiple = TRUE, options = list(`actions-box` = TRUE)),
      selectInput(ns("group"), "Group by", GROUP_CHOICES, "sci"),
      tags$p(class = "small text-muted", "Admission value = mean of all values from 6 h before to 24 h after hospital admission (rendehR clean_by_range)."),
      checkboxInput(ns("mask"), "Mask counts < 10", TRUE)),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Admission values",
        layout_columns(col_widths = c(6, 6), pickerInput(ns("bm"), "Biomarker", BIO_CHOICES, "hemoglobin", options = list(`live-search` = TRUE)),
                       radioButtons(ns("geom"), NULL, c("Violin" = "violin", "Box" = "box", "Density" = "hist"), inline = TRUE)),
        withSpinner(plotlyOutput(ns("dist"), height = "460px")), DTOutput(ns("dist_tab"))),
      nav_panel("All 22 at a glance",
        uiOutput(ns("all22_note")), withSpinner(DTOutput(ns("all22")))),
      nav_panel("SMD contrast",
        layout_columns(col_widths = c(6, 6), selectInput(ns("g1"), "Group 1", NULL), selectInput(ns("g2"), "Group 2 (reference)", NULL)),
        withSpinner(plotlyOutput(ns("smd"), height = "560px"))),
      nav_panel("Trajectories",
        layout_columns(col_widths = c(6, 6), pickerInput(ns("tr_bm"), "Biomarkers", BIO_CHOICES, c("hemoglobin", "creatinine", "wbc", "sodium"), multiple = TRUE, options = list(`live-search` = TRUE)),
                       sliderInput(ns("tr_days"), "Days since admission", 0, 13, c(0, 7))),
        tags$p(class = "small text-muted", "Median and IQR of daily means across admissions; days with fewer than 30 admissions per group are hidden. Patients still in hospital later are sicker (selection)."),
        withSpinner(plotlyOutput(ns("traj"), height = "600px"))),
      nav_panel("Abnormal values",
        tags$p(class = "small text-muted", "Share of admissions with an admission value below / above the adult reference range (sex-specific for hemoglobin, hematocrit and RBC; pooled when sex is unknown)."), withSpinner(plotlyOutput(ns("abn"), height = "560px"))),
      nav_panel("Correlations",
        layout_columns(col_widths = c(6, 6), selectInput(ns("cor_grp"), "Within group", c("All selected", "SCI", "Not SCI", CAT_LEVELS)),
                       radioButtons(ns("cor_m"), "Method", c("Spearman" = "spearman", "Pearson" = "pearson"), inline = TRUE)),
        withSpinner(plotlyOutput(ns("cor"), height = "620px"))),
      nav_panel("Availability",
        withSpinner(plotlyOutput(ns("avail"), height = "560px")))))
}
bio_server <- function(id) moduleServer(id, function(input, output, session) {
  B <- reactive({ validate(need(length(input$src) > 0, "Select at least one source."), need(length(input$cat) > 0, "Select at least one category."))
    ADM %>% filter(source %in% input$src, second_level %in% input$cat) %>% mutate(.g = if (input$group == ".all") "Overall" else as.character(.data[[input$group]])) %>% filter(!is.na(.g)) })
  lab <- function(b) names(BIO_CHOICES)[match(b, BIO_CHOICES)]
  # Fixed colour per group over all its levels, so hiding a group does not shift the others.
  gcols <- reactive({ g <- input$group; group_colors(g, if (g == ".all") "Overall" else grp_levels(ADM[[g]])) })
  masked <- function(k) isTRUE(input$mask) & k >= 1 & k < 10
  output$dist <- renderPlotly({ v <- paste0("bio_", input$bm); d <- B() %>% filter(!is.na(.data[[v]])) %>% mutate(.v = .data[[v]])
    validate(need(nrow(d) > 0, "No values."))
    d <- d %>% group_by(.g) %>% filter(between(.v, quantile(.v, 0.01), quantile(.v, 0.99))) %>% ungroup()   # trimmed within each group
    lv <- intersect(names(gcols()), d$.g); d <- d %>% mutate(.g = factor(.g, lv)); cols <- gcols()[lv]
    r <- BIOMARKERS %>% filter(concept == input$bm)
    p <- switch(input$geom, violin = plot_ly(d, x = ~.g, y = ~.v, color = ~.g, colors = cols, type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE)),
                box = plot_ly(d, x = ~.g, y = ~.v, color = ~.g, colors = cols, type = "box", boxpoints = FALSE),
                hist = plot_ly(d, x = ~.v, color = ~.g, colors = cols, type = "histogram", histnorm = "probability density", opacity = 0.55) %>% layout(barmode = "overlay"))
    band <- if (r$concept %in% .xp_sci_bio_sex_ref$concept) "green band = pooled adult reference range (flags use sex-specific ranges)" else "green band = adult reference range"
    sh <- if (input$geom == "hist") list(list(type = "rect", xref = "x", yref = "paper", x0 = r$ref_lo, x1 = r$ref_hi, y0 = 0, y1 = 1, fillcolor = "rgba(47,125,79,0.09)", line = list(width = 0)))
          else list(list(type = "rect", xref = "paper", yref = "y", x0 = 0, x1 = 1, y0 = r$ref_lo, y1 = r$ref_hi, fillcolor = "rgba(47,125,79,0.09)", line = list(width = 0)))
    p %>% layout(shapes = sh, yaxis = list(title = if (input$geom == "hist") "" else lab(input$bm)), xaxis = list(title = if (input$geom == "hist") lab(input$bm) else ""),
                 showlegend = input$geom == "hist", annotations = list(list(text = paste0(band, "; 1st-99th percentile within each group shown"), xref = "paper", yref = "paper",
                                                                        x = 1, y = 1.06, showarrow = FALSE, xanchor = "right", font = list(size = 10)))) %>% plot_config() })
  output$dist_tab <- renderDT({ v <- paste0("bio_", input$bm)
    dt_table(B() %>% group_by(Group = .g) %>% summarise(Admissions = mask_n(n(), input$mask), `With value` = sprintf("%.1f%%", 100 * mean(!is.na(.data[[v]]))),
                                                        `Median [IQR]` = med_iqr(.data[[v]]), `Mean (SD)` = mean_sd(.data[[v]]), .groups = "drop") %>%
               arrange(match(Group, gl())), page = 20) })
  # Group order: the study's default contrast first (e.g. SCI before Not SCI), then categories by precedence.
  gl <- reactive({ u <- unique(B()$.g); pre <- intersect(DEF$smd[[input$group]] %||% character(0), u); c(pre, setdiff(intersect(names(gcols()), u), pre)) })
  observeEvent(gl(), {                                     # keep the chosen groups while they remain available
    g <- gl(); s1 <- if (isTruthy(input$g1) && input$g1 %in% g) input$g1 else g[1]
    s2 <- if (isTruthy(input$g2) && input$g2 %in% g && input$g2 != s1) input$g2 else c(setdiff(g, s1), g)[1]
    updateSelectInput(session, "g1", choices = g, selected = s1); updateSelectInput(session, "g2", choices = g, selected = s2) })
  output$all22_note <- renderUI({ g <- gl()
    tags$p(class = "small text-muted", sprintf("Median [IQR] of the admission value per group; SMD %s.%s",
                                               if (length(g) >= 2) sprintf("of %s minus %s", g[1], g[2]) else "needs two groups",
                                               if (length(g) > 8) sprintf(" Showing the first 8 of %d groups; not shown: %s.", length(g), paste(g[-(1:8)], collapse = ", ")) else "")) })
  output$all22 <- renderDT({ d <- B(); g <- gl()
    t <- bind_rows(lapply(seq_len(nrow(BIOMARKERS)), function(i) { v <- paste0("bio_", BIOMARKERS$concept[i])
      row <- tibble(Biomarker = paste0(BIOMARKERS$label[i], " (", BIOMARKERS$unit[i], ")"))
      for (gg in head(g, 8)) row[[gg]] <- med_iqr(d[[v]][d$.g == gg])
      if (length(g) >= 2) row[[sprintf("SMD %s vs %s", g[1], g[2])]] <- round(smd_cont(d[[v]][d$.g == g[1]], d[[v]][d$.g == g[2]]), 2)
      row }))
    dt_table(t, page = 25, ordering = FALSE) })
  output$smd <- renderPlotly({ d <- B()
    validate(need(isTruthy(input$g1) && isTruthy(input$g2), "Select two groups."), need(input$g1 != input$g2, "Select two different groups."))
    s <- tibble(concept = BIOMARKERS$concept, label = BIOMARKERS$label, panel = BIOMARKERS$panel,
                smd = sapply(BIOMARKERS$concept, function(b) smd_cont(d[[paste0("bio_", b)]][d$.g == input$g1], d[[paste0("bio_", b)]][d$.g == input$g2]))) %>% filter(!is.na(smd))
    validate(need(nrow(s) > 0, "No biomarker has at least two values in both groups."))
    plot_ly(s, y = ~reorder(label, smd), x = ~smd, color = ~panel, type = "bar", text = ~sprintf("%s: SMD %.2f", label, smd), hoverinfo = "text", textposition = "none") %>%
      layout(xaxis = list(title = sprintf("SMD (%s minus %s)", input$g1, input$g2)), yaxis = list(title = ""),
             shapes = list(list(type = "line", x0 = -0.1, x1 = -0.1, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "grey")),
                           list(type = "line", x0 = 0.1, x1 = 0.1, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "grey")))) %>% plot_config() })
  output$traj <- renderPlotly({ validate(need(length(input$tr_bm) > 0, "Select at least one biomarker."))
    k <- B() %>% select(source, adm_id, .g)
    d <- BIO_DAILY %>% filter(concept %in% input$tr_bm, between(day, input$tr_days[1], input$tr_days[2])) %>% inner_join(k, by = c("source", "adm_id")) %>%
      group_by(concept, .g, day) %>% summarise(n = n(), med = median(v), q1 = quantile(v, .25), q3 = quantile(v, .75), .groups = "drop") %>% filter(n >= 30)
    validate(need(nrow(d) > 0, "No daily values."))
    pl <- lapply(input$tr_bm, function(b) { x <- d %>% filter(concept == b); p <- blank_plot()
      for (gg in intersect(names(gcols()), x$.g)) { xx <- x %>% filter(.g == gg); col <- gcols()[[gg]]
        p <- p %>% add_ribbons(inherit = FALSE, x = xx$day, ymin = xx$q1, ymax = xx$q3, fillcolor = paste0(col, "22"), line = list(width = 0), name = gg, legendgroup = gg, showlegend = FALSE, hoverinfo = "skip") %>%
          add_lines(inherit = FALSE, x = xx$day, y = xx$med, line = list(color = col), name = gg, legendgroup = gg, showlegend = b == input$tr_bm[1],
                    text = sprintf("%s, day %d: %.2f [%.2f-%.2f], n = %s", gg, xx$day, xx$med, xx$q1, xx$q3, fmt_n(xx$n)), hoverinfo = "text") }
      p %>% layout(yaxis = list(title = list(text = lab(b), font = list(size = 10)))) })
    subplot(pl, nrows = ceiling(length(pl) / 2), shareX = TRUE, titleY = TRUE, margin = 0.05) %>% layout(xaxis = list(title = "Day of hospital admission"), legend = list(orientation = "h", y = -0.1)) %>% plot_config() })
  output$abn <- renderPlotly({ d <- B()
    a <- bind_rows(lapply(seq_len(nrow(BIOMARKERS)), function(i) { v <- paste0("bio_", BIOMARKERS$concept[i])
      x <- d %>% filter(!is.na(.data[[v]])); rr <- .xp_sci_ref_range(BIOMARKERS$concept[i], x$sex)
      x %>% mutate(.lo = rr$lo, .hi = rr$hi) %>% group_by(.g) %>% summarise(low = sum(.data[[v]] < .lo), high = sum(.data[[v]] > .hi), n = n(), .groups = "drop") %>%
        mutate(label = BIOMARKERS$label[i]) })) %>% pivot_longer(c(low, high), names_to = "dir", values_to = "k") %>%
      mutate(p = if_else(dir == "low", -k / n, k / n), label = factor(label, rev(BIOMARKERS$label)), .g = factor(.g, intersect(names(gcols()), .g)))
    plot_ly(a, y = ~label, x = ~p, color = ~.g, colors = gcols()[levels(a$.g)], type = "bar", orientation = "h",
            text = ~sprintf("%s, %s: %s %s", .g, label, dir, if_else(masked(k), "<10", sprintf("%.1f%% (%s of %s)", 100 * abs(p), fmt_n(k), fmt_n(n)))), hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "group", xaxis = list(title = "← below reference | above reference →", tickformat = ".0%"), yaxis = list(title = "")) %>% plot_config() })
  output$cor <- renderPlotly({ d <- B(); if (input$cor_grp == "SCI" || input$cor_grp == "Not SCI") d <- d %>% filter(sci == input$cor_grp) else if (input$cor_grp %in% CAT_LEVELS) d <- d %>% filter(second_level == input$cor_grp)
    m <- as.matrix(d[, paste0("bio_", BIOMARKERS$concept)]); validate(need(nrow(m) > 30, "Too few admissions."))
    cm <- suppressWarnings(cor(m, use = "pairwise.complete.obs", method = input$cor_m)); dimnames(cm) <- list(BIOMARKERS$label, BIOMARKERS$label)
    plot_ly(x = colnames(cm), y = rownames(cm), z = cm, type = "heatmap", colorscale = "RdBu", zmin = -1, zmax = 1, reversescale = TRUE,
            text = round(cm, 2), hovertemplate = "%{y} vs %{x}: %{z:.2f}<extra></extra>") %>% layout(xaxis = list(title = ""), yaxis = list(title = "", autorange = "reversed")) %>% plot_config() })
  output$avail <- renderPlotly({ d <- B()
    a <- bind_rows(lapply(seq_len(nrow(BIOMARKERS)), function(i) d %>% group_by(.g, source_label) %>% summarise(p = mean(!is.na(.data[[paste0("bio_", BIOMARKERS$concept[i])]])), .groups = "drop") %>%
      mutate(label = BIOMARKERS$label[i]))) %>% mutate(col = paste(source_label, .g, sep = " | "))
    plot_ly(a, x = ~col, y = ~label, z = ~p, type = "heatmap", colorscale = "Viridis", zmin = 0, zmax = 1, text = ~sprintf("%s | %s: %.0f%%", col, label, 100 * p), hoverinfo = "text") %>%
      layout(xaxis = list(title = "", tickangle = -30), yaxis = list(title = "")) %>% plot_config() })
})

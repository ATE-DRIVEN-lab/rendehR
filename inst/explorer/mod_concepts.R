# ---- Concepts and data dictionary ----------------------------------------------------------------------------------------
CONCEPT_NAMES <- sort(unique(D$concepts$concept_name %||% character(0)))
AVAIL_WINDOWS <- c("First 24 h" = "1440", "First 48 h" = "2880", "Whole stay" = "all")
DRUGS <- if (nrow(D$med_prev %||% tibble())) D$med_prev %>% group_by(drug) %>% summarise(n = sum(n)) %>% arrange(desc(n)) %>% pull(drug) else character(0)
concepts_ui <- function(id) {
  ns <- NS(id)
  navset_underline(nav_panel("Concept inventory",
      tags$p(class = "small text-muted", "Every concept table per source (rendehR::list_concepts()), with its provenance where registered."),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(4, 8)), fill = FALSE,
        card(card_header("Concepts by schema and source"), DTOutput(ns("inv_type"), fill = FALSE)),
        card(card_header("Concept tables and their provenance"), DTOutput(ns("inv"), fill = FALSE)))),
    nav_panel("Concept builder", concept_builder_ui(ns("cb"))),
    nav_panel("Concept viewer",
      layout_columns(col_widths = c(3, 9),
        card(pickerInput(ns("cn"), "Concept", CONCEPT_NAMES, first_in(c("heart_rate", "hemoglobin"), CONCEPT_NAMES), options = list(`live-search` = TRUE)),
             checkboxGroupInput(ns("src"), "Sources", stats::setNames(SRC, SRC_LABEL[SRC]), selected = SRC),
             checkboxInput(ns("logx"), "Log scale", FALSE),
             if (LIVE) actionButton(ns("go"), "Summarise cohort values", icon = icon("chart-column"), class = "btn-primary")
             else tags$p(class = "small text-muted", "Summaries need the live database connection."), hr(),
             tags$h6("Source concept table"), selectInput(ns("live_src"), NULL, stats::setNames(SRC, SRC_LABEL[SRC])),
             if (LIVE) actionButton(ns("live"), "Query source table", icon = icon("database")), uiOutput(ns("live_out"))),
        card(conditionalPanel("!(input.go > 0)", ns = ns,
               empty_state("bar-chart-line", "No summary yet", "Choose a concept and sources, then press Summarise cohort values.")),
             conditionalPanel("input.go > 0", ns = ns, withSpinner(plotlyOutput(ns("cdist"), height = "380px")), DTOutput(ns("csum")))))),
    nav_panel("Availability",
      layout_columns(fill = FALSE, col_widths = c(3, 2, 7),
        selectInput(ns("av_win"), "Window (hospital clock)", AVAIL_WINDOWS),
        if (LIVE) actionButton(ns("av_go"), "Compute", icon = icon("table-cells"), class = "btn-primary mt-4"),
        tags$p(class = "small text-muted", "Share of cohort admissions with at least one row of each concept in the window. Counts below 10 show as <10 and their share is hidden.")),
      if (!LIVE) empty_state("plug", "No database connection", "Availability is computed in the database.")
      else tagList(conditionalPanel("!(input.av_go > 0)", ns = ns, empty_state("grid-3x3", "Nothing computed yet", "Choose a window, then press Compute.")),
                   conditionalPanel("input.av_go > 0", ns = ns, uiOutput(ns("av_plot_ui")), DTOutput(ns("av_tab"))))),
    nav_panel("Quality",
      layout_columns(fill = FALSE, col_widths = c(2, 10),
        if (LIVE) actionButton(ns("q_go"), "Compute", icon = icon("magnifying-glass-chart"), class = "btn-primary mt-4"),
        tags$p(class = "small text-muted", "Per concept and source over cohort admissions: rows, rows without a numeric value, units seen (most common first) and values outside the plausible range of the shipped concept configuration. The pipeline keeps every value; range cleaning is opt-in (clean_by_range(), or clean = \"range\" in the wide loaders).")),
      if (!LIVE) empty_state("plug", "No database connection", "Quality is computed in the database.")
      else tagList(conditionalPanel("!(input.q_go > 0)", ns = ns, empty_state("clipboard-check", "Nothing computed yet", "Press Compute.")),
                   conditionalPanel("input.q_go > 0", ns = ns, withSpinner(DTOutput(ns("q_tab")))))),
    nav_panel("Flat variable dictionary",
      tags$p(class = "small text-muted", "Variables available in the explorers, their type and missingness by source (cohort admissions)."), withSpinner(DTOutput(ns("dict")))),
    nav_panel("Medication library",
      layout_columns(col_widths = c(6, 6), card(card_header("Exposure prevalence by source (cohort admissions)"), withSpinner(DTOutput(ns("meds")))),
                     card(card_header("Exposure prevalence: top 25"), withSpinner(plotlyOutput(ns("medplot"), height = "560px")))),
      card(card_header("Exposure by day"),
        layout_columns(fill = FALSE, col_widths = c(6, 2, 2),
          pickerInput(ns("ed_drugs"), "Drugs", DRUGS, head(DRUGS, 5), multiple = TRUE, options = list(`live-search` = TRUE, `actions-box` = TRUE, `selected-text-format` = "count > 4")),
          selectInput(ns("ed_days"), "Days", c(7, 14)),
          if (LIVE) actionButton(ns("ed_go"), "Compute", icon = icon("chart-line"), class = "btn-primary mt-4")),
        if (!LIVE) empty_state("plug", "No database connection", "Exposure by day is computed in the database.")
        else tagList(conditionalPanel("!(input.ed_go > 0)", ns = ns, empty_state("graph-up", "Nothing computed yet", "Choose drugs and days, then press Compute.")),
                     conditionalPanel("input.ed_go > 0", ns = ns, withSpinner(plotlyOutput(ns("ed_plot"), height = "420px")))))))
}

# Cohort admissions of the merged schema as a SQL subquery (source_data, src_admission_id).
cohort_sql <- function(con) .xp_cohort_adm(con, D$meta$schema)
COHORT_DEN <- count(ADM, source, name = "den")
share_hidden <- function(n, den) (n >= 1 & n < 10) | den < 10   # small-cell rule for shares

# Per-source summary and a binned distribution of one concept over the cohort
# admissions, computed in the database from the merged ts table (no rows are
# copied). Bins span the 0.5th-99.5th percentiles pooled over the sources.
concept_stats <- function(cn, srcs) {
  .with_connection(NULL, function(con) {
    tb <- .xp_cohort_ts(con, D$meta$schema)
    s <- DBI::dbGetQuery(con, sprintf(
      "SELECT source_data AS source, COUNT(*) AS n_rows, COUNT(value) AS n_value,
              COUNT(DISTINCT src_admission_id) AS n_adm,
              percentile_cont(ARRAY[0.005, 0.25, 0.5, 0.75, 0.995]) WITHIN GROUP (ORDER BY value) AS q,
              mode() WITHIN GROUP (ORDER BY unit) AS unit
       FROM %s WHERE concept_name = $1 GROUP BY 1", tb), params = list(cn))
    s <- s[s$source %in% srcs, , drop = FALSE]
    if (!nrow(s)) return(NULL)
    q <- lapply(s$q, function(x) as.numeric(strsplit(gsub("[{}]", "", x), ",")[[1]]))
    s$p005 <- vapply(q, `[`, numeric(1), 1); s$p25 <- vapply(q, `[`, numeric(1), 2); s$p50 <- vapply(q, `[`, numeric(1), 3)
    s$p75 <- vapply(q, `[`, numeric(1), 4); s$p995 <- vapply(q, `[`, numeric(1), 5); s$q <- NULL
    lo <- min(s$p005, na.rm = TRUE); hi <- max(s$p995, na.rm = TRUE)
    h <- if (is.finite(lo) && is.finite(hi) && hi > lo) DBI::dbGetQuery(con, sprintf(
      "SELECT source_data AS source, LEAST(width_bucket(value, $2, $3, 60), 60) AS b, COUNT(*) AS n
       FROM %s WHERE concept_name = $1 AND value BETWEEN $2 AND $3 GROUP BY 1, 2", tb), params = list(cn, lo, hi)) else NULL
    if (!is.null(h)) { h <- h[h$source %in% srcs, ]; h$x <- lo + (h$b - 0.5) * (hi - lo) / 60 }
    list(summary = tibble::as_tibble(s), hist = h)
  })
}

# ---- availability: admissions with at least one row of a concept, per source --------------------------------------------
concept_availability <- function(win) .with_connection(NULL, function(con) {
  w <- if (win == "all") "" else sprintf("WHERE t.time_since_hadm_min < %d", as.integer(win))
  DBI::dbGetQuery(con, sprintf(
    "SELECT t.source_data AS source, t.concept_name, COUNT(DISTINCT t.src_admission_id) AS n
     FROM %s t JOIN %s c ON c.source_data = t.source_data AND c.src_admission_id = t.src_admission_id %s GROUP BY 1, 2",
    .xp_quote_table(con, D$meta$schema, "ts"), cohort_sql(con), w))
})
# Every concept x source (absent = 0), share of the cohort admissions, small cells masked.
availability_shape <- function(x, den = COHORT_DEN) {
  tibble::as_tibble(x) %>% mutate(n = as.numeric(n)) %>% filter(source %in% den$source) %>%
    tidyr::complete(concept_name, source = den$source, fill = list(n = 0)) %>% left_join(den, by = "source") %>%
    mutate(share = if_else(share_hidden(n, den), NA_real_, n / den), n_lab = mask_n(n)) %>% arrange(concept_name, source)
}

# ---- quality: rows, missing values, units and out-of-range values per concept and source ----------------------------------
# Bounds as clean_by_range() and the wide loaders' clean = "range" resolve them (widest window over the config rows).
range_bounds <- function() {
  b <- Filter(function(x) !all(is.na(x)), .resolve_range_bounds(NULL, NULL, NULL))
  tibble(concept_name = names(b), lo = vapply(b, `[[`, 0, "min"), hi = vapply(b, `[[`, 0, "max"))
}
concept_quality <- function(bounds = range_bounds()) .with_connection(NULL, function(con) {
  vals <- paste(sprintf("(%s, CAST(%s AS float8), CAST(%s AS float8))", DBI::dbQuoteLiteral(con, bounds$concept_name),
                        DBI::dbQuoteLiteral(con, bounds$lo), DBI::dbQuoteLiteral(con, bounds$hi)), collapse = ", ")
  DBI::dbGetQuery(con, sprintf(
    "WITH b(concept_name, lo, hi) AS (VALUES %s),
     u AS (SELECT t.source_data, t.concept_name, t.unit, COUNT(*) AS n, COUNT(t.value) AS n_value,
                  COUNT(*) FILTER (WHERE t.value < b.lo OR t.value > b.hi) AS n_out
           FROM %s t JOIN %s c ON c.source_data = t.source_data AND c.src_admission_id = t.src_admission_id
           LEFT JOIN b ON b.concept_name = t.concept_name GROUP BY 1, 2, 3)
     SELECT source_data AS source, concept_name, SUM(n) AS n_rows, SUM(n_value) AS n_value, SUM(n_out) AS n_out,
            string_agg(COALESCE(unit, ''), '|' ORDER BY n DESC) AS units
     FROM u GROUP BY 1, 2", vals, .xp_quote_table(con, D$meta$schema, "ts"), cohort_sql(con)))
})
# One row per concept x source, worst share outside the range first. Shares from fewer than 10 rows are hidden.
quality_shape <- function(x, bounds) {
  tibble::as_tibble(x) %>% mutate(across(c(n_rows, n_value, n_out), as.numeric)) %>% left_join(bounds, by = "concept_name") %>%
    mutate(units = vapply(strsplit(coalesce(units, ""), "|", fixed = TRUE),
                          function(u) paste(setdiff(unique(clean_unit(u)), ""), collapse = ", "), ""),
           p_missing = if_else(n_rows < 10, NA_real_, 1 - n_value / n_rows),
           p_out = if_else(is.na(lo) & is.na(hi) | n_value < 10, NA_real_, n_out / n_value),
           status = case_when(is.na(p_out) ~ "N/A", p_out > 0.01 | grepl(",", units) ~ "CHECK", TRUE ~ "PASS")) %>%
    arrange(desc(coalesce(p_out, -1)), concept_name, source)
}

# ---- exposure by day ------------------------------------------------------------------------------------------------------
# Day d (0 = first 24 h) is [1440 d, 1440 (d + 1)) minutes since hospital admission. An exposure counts on a day its
# interval overlaps; a missing or negative end makes it a point at its start. The SQL in exposure_by_day() applies the
# same rule.
exposed_on_day <- function(start, end, day) {
  end <- ifelse(is.na(end) | end < start, start, end)
  !is.na(start) & start < 1440 * (day + 1) & (end > 1440 * day | start >= 1440 * day)
}
exposure_by_day <- function(drugs, days) .with_connection(NULL, function(con) {
  cn <- paste(DBI::dbQuoteLiteral(con, c(drugs, paste0(drugs, "_exp"))), collapse = ", ")
  DBI::dbGetQuery(con, sprintf(
    "WITH e AS (SELECT d.source_data, regexp_replace(d.concept_name, '_exp$', '') AS drug, d.src_admission_id,
                       d.end_offset_min - COALESCE(d.duration_min, 0) AS s,
                       CASE WHEN d.duration_min >= 0 THEN d.end_offset_min ELSE d.end_offset_min - COALESCE(d.duration_min, 0) END AS e
                FROM %s d JOIN %s c ON c.source_data = d.source_data AND c.src_admission_id = d.src_admission_id
                WHERE d.concept_name IN (%s))
     SELECT source_data AS source, drug, g.day, COUNT(DISTINCT src_admission_id) AS n
     FROM e CROSS JOIN generate_series(0, %d) AS g(day)
     WHERE s < 1440 * (g.day + 1) AND (e > 1440 * g.day OR s >= 1440 * g.day)
     GROUP BY 1, 2, 3", .xp_quote_table(con, D$meta$schema, "dur"), cohort_sql(con), cn, as.integer(days) - 1L))
})
exposure_shape <- function(x, drugs, days, den = COHORT_DEN) {
  tibble::as_tibble(x) %>% mutate(n = as.numeric(n), day = as.integer(day)) %>% filter(source %in% den$source) %>%
    tidyr::complete(drug = drugs, source = den$source, day = seq_len(days) - 1L, fill = list(n = 0)) %>%
    left_join(den, by = "source") %>% mutate(share = if_else(share_hidden(n, den), NA_real_, n / den)) %>% arrange(source, drug, day)
}

concepts_server <- function(id) moduleServer(id, function(input, output, session) {
  concept_builder_server("cb")
  output$inv_type <- renderDT({ validate(need(nrow(D$concepts %||% tibble()) > 0, "No concept inventory (live connection unavailable at start-up)."))
    x <- D$concepts %>% count(source, schema) %>% mutate(source = SRC_LABEL[source]) %>% tidyr::pivot_wider(names_from = source, values_from = n, values_fill = 0)
    datatable(x, rownames = FALSE, options = list(dom = "t"), class = "compact") })
  output$inv <- renderDT({ validate(need(nrow(D$concepts %||% tibble()) > 0, "No concept inventory."))
    dt_table(D$concepts %>% mutate(source = SRC_LABEL[source]) %>% select(any_of(c("concept_name", "source", "schema", "origin_name", "origin_url", "origin_ref"))) %>%
               rename(any_of(c(Concept = "concept_name", Source = "source", Schema = "schema", Origin = "origin_name",
                               `Origin URL` = "origin_url", Reference = "origin_ref"))), page = 20) })
  cs <- eventReactive(input$go, { req(LIVE, input$cn)
    withProgress(message = sprintf("Summarising %s...", input$cn), tryCatch(concept_stats(input$cn, input$src), error = function(e) list(error = conditionMessage(e)))) })
  output$cdist <- renderPlotly({ r <- cs(); validate(need(is.null(r$error), r[["error"]] %||% ""), need(!is.null(r$hist) && nrow(r$hist) > 0, "No values for this concept in the cohort."),
             need(!isTRUE(input$logx) || min(r$hist$x) > 0, "Log scale needs positive values; this concept has zero or negative values."))
    h <- r$hist %>% group_by(source) %>% mutate(d = n / sum(n)) %>% ungroup() %>% mutate(source_label = SRC_LABEL[source])
    plot_ly(h, x = ~x, y = ~d, color = ~source_label, colors = SRC_COLORS, type = "scatter", mode = "lines", line = list(shape = "hvh")) %>%
      layout(xaxis = list(title = sprintf("%s (0.5th-99.5th percentile, pooled over sources)", isolate(input$cn)), type = if (input$logx) "log" else "linear"),
             yaxis = list(title = "Share of values", tickformat = ".1%")) %>% plot_config() })
  output$csum <- renderDT({ r <- cs(); req(is.null(r$error), !is.null(r$summary))
    dt_table(r$summary %>% transmute(Source = SRC_LABEL[source], Rows = fmt_n(n_rows), `No value` = sprintf("%.1f%%", 100 * (1 - n_value / n_rows)),
                                     Admissions = fmt_n(n_adm), Unit = unit, P0.5 = signif(p005, 4), P25 = signif(p25, 4), Median = signif(p50, 4),
                                     P75 = signif(p75, 4), P99.5 = signif(p995, 4)), dom = "t") })
  live <- eventReactive(input$live, { req(LIVE)
    tryCatch(withProgress(message = "rendehR::concept_summary() ...", rendehR::concept_summary(input$live_src, input$cn)),
             error = function(e) list(error = conditionMessage(e))) })
  output$live_out <- renderUI({ r <- live()
    if (!is.null(r$error)) return(tags$div(class = "alert alert-warning small mt-2", r$error))
    tags$table(class = "table table-sm small mt-2", tags$tbody(lapply(setdiff(names(r), "columns"), function(n)
      tags$tr(tags$th(n), tags$td(format(r[[n]], big.mark = ","))))))
  })
  output$dict <- renderDT({
    vv <- setdiff(VARS$var, "source_label")
    icu_only <- intersect(c("icu_los_total", "unit_first", "icu_death"), vv)   # defined only for admissions with an ICU stay
    miss <- ADM %>% group_by(source_label) %>%
      summarise(across(all_of(vv), ~ if (cur_column() %in% icu_only) mean(is.na(.x[any_icu %in% 1])) else mean(is.na(.x)))) %>%
      tidyr::pivot_longer(-source_label, names_to = "var", values_to = "m") %>% mutate(m = sprintf("%.0f%%", 100 * m)) %>%
      tidyr::pivot_wider(names_from = source_label, values_from = m, names_prefix = "Missing ")
    dt_table(VARS %>% left_join(miss, by = "var") %>% rename(Variable = var, Label = label, Type = type, Group = group), page = 25) })
  output$meds <- renderDT({ m <- D$med_prev; validate(need(nrow(m) > 0, "No medication exposures (merged dur table) for the cohort."))
    dt_table(m %>% arrange(source, desc(n)) %>% transmute(Drug = drug, Source = SRC_LABEL[source], Admissions = mask_n(n),
                                                           Share = if_else(n < 10, "<10 admissions", sprintf("%.1f%%", 100 * p))), page = 20) })
  output$medplot <- renderPlotly({ m <- D$med_prev; validate(need(nrow(m) > 0, "No medication exposures."))
    top <- m %>% tidyr::complete(drug, source, fill = list(p = 0)) %>% group_by(drug) %>% summarise(s = mean(p)) %>%
      slice_max(s, n = 25, with_ties = FALSE) %>% pull(drug)
    plot_ly(m %>% filter(drug %in% top) %>% mutate(source_label = SRC_LABEL[source]), y = ~drug, x = ~p, color = ~source_label, colors = SRC_COLORS,
            type = "bar", orientation = "h") %>%
      layout(barmode = "group", legend = list(orientation = "h", y = -0.12), xaxis = list(tickformat = ".0%", title = "Cohort admissions exposed"), yaxis = list(title = "", categoryorder = "total ascending")) %>%
      plot_config() })

  run_live <- function(msg, expr) withProgress(message = msg, tryCatch(expr, error = function(e) list(error = conditionMessage(e))))
  av <- eventReactive(input$av_go, { req(LIVE); r <- run_live("Counting admissions per concept...", concept_availability(input$av_win))
    if (is.data.frame(r)) availability_shape(r) else r })
  output$av_plot_ui <- renderUI({ r <- av(); validate(need(is.data.frame(r), r[["error"]] %||% ""), need(nrow(r) > 0, "No time-series rows for the cohort."))
    withSpinner(plotlyOutput(session$ns("av_plot"), height = sprintf("%dpx", 120 + 16 * n_distinct(r$concept_name)))) })
  output$av_plot <- renderPlotly({ r <- av(); req(is.data.frame(r), nrow(r) > 0)
    ord <- r %>% group_by(concept_name) %>% summarise(m = mean(coalesce(share, 0))) %>% arrange(m) %>% pull(concept_name)
    plot_ly(r %>% mutate(source_label = SRC_LABEL[source]), x = ~source_label, y = ~concept_name, z = ~share, type = "heatmap",
            zmin = 0, zmax = 1, colorscale = list(list(0, "#f4f6f8"), list(1, "#1f3b57")),
            colorbar = list(title = "Admissions", tickformat = ".0%"), hoverinfo = "text",
            text = ~sprintf("%s | %s<br>%s admissions%s", concept_name, source_label, n_lab, if_else(is.na(share), "", sprintf(" (%.1f%%)", 100 * share)))) %>%
      layout(xaxis = list(title = "", side = "top"), yaxis = list(title = "", categoryorder = "array", categoryarray = ord, tickfont = list(size = 10)),
             margin = list(l = 10)) %>% plot_config() })
  output$av_tab <- renderDT({ r <- av(); req(is.data.frame(r))
    w <- r %>% transmute(Concept = concept_name, source = SRC_LABEL[source],
                         cell = if_else(is.na(share), n_lab, sprintf("%s (%.1f%%)", n_lab, 100 * share))) %>%
      tidyr::pivot_wider(names_from = source, values_from = cell, values_fill = "0")
    dt_table(w[, c("Concept", intersect(unname(SRC_LABEL), names(w)))], page = 25) })
  qb <- reactive(range_bounds())
  qr <- eventReactive(input$q_go, { req(LIVE); r <- run_live("Checking values per concept...", concept_quality(qb()))
    if (is.data.frame(r)) quality_shape(r, qb()) else r })
  pct1 <- function(p) if_else(is.na(p), "", sprintf("%.1f%%", 100 * p))
  output$q_tab <- renderDT({ r <- qr(); validate(need(is.data.frame(r), r[["error"]] %||% ""))
    status_style(dt_table(r %>% transmute(Status = status, Concept = concept_name, Source = SRC_LABEL[source], Rows = mask_n(n_rows),
                                          `No value` = pct1(p_missing), Units = units,
                                          Range = if_else(is.na(lo) & is.na(hi), "", sprintf("%s to %s", coalesce(as.character(lo), "–"), coalesce(as.character(hi), "–"))),
                                          `Outside range` = pct1(p_out)), page = 25), "Status") })
  ed <- eventReactive(input$ed_go, { req(LIVE); validate(need(length(input$ed_drugs) > 0, "Choose at least one drug."))
    d <- as.integer(input$ed_days); r <- run_live("Counting exposures per day...", exposure_by_day(input$ed_drugs, d))
    if (is.data.frame(r)) exposure_shape(r, input$ed_drugs, d) else r })
  # One panel per source, one line per drug (same colour in every panel).
  output$ed_plot <- renderPlotly({ r <- ed(); validate(need(is.data.frame(r), r[["error"]] %||% ""))
    dr <- unique(r$drug); col <- stats::setNames(rep_len(PAL, length(dr)), dr); ss <- intersect(SRC, r$source)
    ps <- lapply(seq_along(ss), function(i) {
      p <- blank_plot()
      for (g in dr) { d <- r %>% filter(source == ss[i], drug == g)
        p <- p %>% add_trace(inherit = FALSE, x = d$day + 1, y = d$share, type = "scatter", mode = "lines+markers", name = g, legendgroup = g,
                             showlegend = i == 1, line = list(color = col[[g]], width = 1.5), marker = list(color = col[[g]], size = 5),
                             hoverinfo = "text", text = sprintf("%s, day %d<br>%s admissions%s", g, d$day + 1, mask_n(d$n),
                                                                if_else(is.na(d$share), "", sprintf(" (%.1f%%)", 100 * d$share)))) }
      p %>% layout(xaxis = list(title = "Day since hospital admission", dtick = 1), annotations = list(list(text = sprintf("<b>%s</b>", SRC_LABEL[[ss[i]]]), x = 0.5, y = 1.02, xref = "paper", yref = "paper",
                                           showarrow = FALSE, xanchor = "center", yanchor = "bottom")))
    })
    subplot(ps, nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.03) %>%
      layout(yaxis = list(title = "Cohort admissions exposed", tickformat = ".0%", rangemode = "tozero"),
             legend = list(orientation = "h", y = -0.18), hovermode = "closest") %>% plot_config() })
})

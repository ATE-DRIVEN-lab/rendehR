# ---- Cohort definition, code list and categories ------------------------------------------------------------------------
cohort_ui <- function(id) {
  ns <- NS(id)
  navset_underline(nav_panel("Category rules",
      layout_columns(col_widths = c(7, 5),
        card(card_header("Six categories, assigned by precedence (rendehR .sci_cat_summ)"), DTOutput(ns("rules"))),
        card(card_header("Code flags"), tags$ul(
          tags$li(tags$b("is_sci"), ": 1 = spinal cord injury or disease; 2 = nerve root injury or disease; 3 = symptoms/syndromes of cord damage; 0 = spinal column injury or spinal disease without cord involvement; 999 = unspecified."),
          tags$li(tags$b("is_tr"), ": 1 = traumatic, 0 = non-traumatic, 2 = unknown."),
          tags$li("Selection grain: MIMIC hospital admissions (discharge diagnoses); eICU-CRD ICU stays (problem list)."),
          tags$li("rendehR extracts the full patient context; admissions with an SCI-D code are flagged (hos_cohort_flg = 1)."),
          tags$li(paste0("SCI = top level '", .sci_cat_labels$top_sci, "' (any code with is_sci 1, 3 or 999); ",
                         "nerve-root and control admissions = Not SCI. Trauma status of an SCI admission comes from its SCI codes only.")),
          tags$li("Neurological level group, completeness and coded plegia are derived from the titles and levels of the cord and symptom codes (code-based proxies, not ASIA).")))
      )),
    nav_panel("Source to category",
      card(full_screen = TRUE, card_header("From source to category (flagged admissions)"), withSpinner(plotlyOutput(ns("sankey"), height = "480px")))),
    nav_panel("SCI-D code list",
      layout_columns(col_widths = c(3, 9),
        card(selectInput(ns("cl_cat"), "Category", c("All", CAT_LEVELS)), selectInput(ns("cl_ver"), "ICD version", c("All", "9", "10")),
             textInput(ns("cl_q"), "Search code or title"), uiOutput(ns("cl_n"))),
        card(DTOutput(ns("codelist"))))),
    nav_panel("Categories",
      layout_columns(col_widths = c(3, 9),
        card(radioButtons(ns("unit"), "Unit", c("Patients (index admission)" = "pat", "Flagged admissions" = "adm")),
             checkboxGroupInput(ns("src"), "Sources", stats::setNames(SRC, SRC_LABEL[SRC]), selected = SRC), checkboxInput(ns("mask"), "Mask counts < 10", TRUE)),
        card(withSpinner(plotlyOutput(ns("catplot"), height = "380px")), DTOutput(ns("cattab"))))),
    nav_panel("Codes used per category",
      layout_columns(col_widths = c(3, 9),
        card(selectInput(ns("tc_cat"), "Category", CAT_LEVELS), sliderInput(ns("tc_n"), "Top codes", 5, 50, 20), checkboxInput(ns("tc_mask"), "Mask counts < 10", TRUE)),
        card(withSpinner(plotlyOutput(ns("topcodes"), height = "520px"))))),
    nav_panel("Coding period and multi-category patients",
      layout_columns(col_widths = c(6, 6),
        card(card_header("Category mix by coding period"), withSpinner(plotlyOutput(ns("period"), height = "420px"))),
        card(card_header("Patients with flagged admissions in more than one category"), DTOutput(ns("multi")))))
  )
}
cohort_server <- function(id) moduleServer(id, function(input, output, session) {
  output$sankey <- renderPlotly({
    d <- ADM %>% count(source_label, sci, second_level)
    nodes <- c(unname(SRC_LABEL[SRC]), "SCI", "Not SCI", CAT_LEVELS)
    l1 <- d %>% group_by(source_label, sci) %>% summarise(n = sum(n), .groups = "drop")
    l2 <- d %>% group_by(sci, second_level) %>% summarise(n = sum(n), .groups = "drop")
    links <- bind_rows(tibble(s = match(l1$source_label, nodes) - 1, t = match(l1$sci, nodes) - 1, v = l1$n),
                       tibble(s = match(l2$sci, nodes) - 1, t = match(l2$second_level, nodes) - 1, v = l2$n))
    tot <- pmax(tapply(links$v, factor(links$s, seq_along(nodes) - 1), sum, default = 0),
                tapply(links$v, factor(links$t, seq_along(nodes) - 1), sum, default = 0))   # node = max(in, out)
    plot_ly(type = "sankey", orientation = "h", valueformat = ",",
            node = list(label = nodes, pad = 12, thickness = 14, customdata = mask_n(unname(tot), TRUE),
                        hovertemplate = "%{label}: %{customdata} admissions<extra></extra>",
                        color = c(unname(SRC_COLORS[SRC_LABEL[SRC]]), unname(SCI_COLORS), unname(CAT_COLORS[CAT_LEVELS]))),
            link = list(source = links$s, target = links$t, value = links$v, color = "rgba(150,150,150,0.35)", customdata = mask_n(links$v, TRUE),
                        hovertemplate = "%{source.label} → %{target.label}: %{customdata}<extra></extra>")) %>% plot_config()
  })
  output$rules <- renderDT(datatable(RULES, rownames = FALSE, options = list(dom = "t", pageLength = 8), class = "compact"))
  cl <- reactive({
    x <- CODES$scid %>% transmute(icd_code, icd_version = as.character(icd_version), category = code_category, is_sci, is_tr,
                                  injury_location, injury_level, nontrauma_l1, nontrauma_l2, long_title)
    if (input$cl_cat != "All") x <- x %>% filter(category == input$cl_cat)
    if (input$cl_ver != "All") x <- x %>% filter(icd_version == input$cl_ver)
    if (nzchar(input$cl_q)) x <- x %>% filter(str_detect(tolower(paste(icd_code, long_title)), fixed(tolower(input$cl_q))))
    x
  })
  output$cl_n <- renderUI(tags$p(sprintf("%s of %s codes", fmt_n(nrow(cl())), fmt_n(nrow(CODES$scid)))))
  output$codelist <- renderDT(dt_table(cl(), page = 20))
  catd <- reactive({
    d <- if (input$unit == "pat") ADM %>% filter(is_index) else ADM
    d %>% filter(source %in% input$src) %>% count(source_label, second_level) %>% group_by(source_label) %>% mutate(pct = n / sum(n)) %>% ungroup()
  })
  output$catplot <- renderPlotly({
    d <- catd() %>% mutate(second_level = factor(second_level, CAT_LEVELS))
    plot_ly(d, x = ~second_level, y = ~pct, color = ~source_label, colors = SRC_COLORS, type = "bar",
            text = ~sprintf("%s | %s<br>%s", source_label, second_level, mask_kp(n, pct, input$mask)), hoverinfo = "text") %>%
      layout(barmode = "group", yaxis = list(title = "Share within source", tickformat = ".0%"), xaxis = list(title = "")) %>% plot_config()
  })
  output$cattab <- renderDT({
    d <- catd()
    tot <- d %>% group_by(second_level) %>% summarise(n = sum(n)) %>% mutate(pct = n / sum(n), source_label = "Total")
    w <- bind_rows(d, tot) %>% mutate(cell = if_else(input$mask & n < 10, "<10", sprintf("%s (%.1f%%)", fmt_n(n), 100 * pct))) %>%
      select(second_level, source_label, cell) %>% pivot_wider(names_from = source_label, values_from = cell, values_fill = "0 (0.0%)") %>%
      mutate(Group = if_else(second_level %in% CAT_LEVELS[1:3], "SCI", "Not SCI"), .before = 1) %>% arrange(match(second_level, CAT_LEVELS)) %>%
      rename(Category = second_level)
    datatable(w, rownames = FALSE, options = list(dom = "t", pageLength = 10), class = "compact")
  })
  output$topcodes <- renderPlotly({
    d <- ADM %>% filter(second_level == input$tc_cat) %>% mutate(c = str_split(scid_icd_codes, fixed(" | "))) %>% tidyr::unnest(c) %>%
      mutate(c = str_trim(c)) %>% count(c, sort = TRUE) %>% head(input$tc_n) %>%
      left_join(CODES$scid %>% distinct(icd_code, .keep_all = TRUE) %>% select(icd_code, long_title), by = c(c = "icd_code")) %>%
      mutate(pct = n / sum(ADM$second_level == input$tc_cat), lab = paste(c, str_trunc(coalesce(long_title, ""), 60)))
    if (input$tc_mask) d <- d %>% filter(n >= 10)
    plot_ly(d, y = ~reorder(lab, n), x = ~pct, type = "bar", marker = list(color = CAT_COLORS[[input$tc_cat]]),
            text = ~sprintf("%s<br>%s admissions (%.1f%%)", lab, fmt_n(n), 100 * pct), hoverinfo = "text") %>%
      layout(xaxis = list(title = "Share of the category's flagged admissions", tickformat = ".0%"), yaxis = list(title = "", tickfont = list(size = 9))) %>% plot_config()
  })
  output$period <- renderPlotly({
    d <- ADM %>% count(source_label, icd_period, second_level) %>% group_by(source_label, icd_period) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
      mutate(g = paste(source_label, icd_period, sep = " | "), second_level = factor(second_level, rev(CAT_LEVELS)))
    plot_ly(d, y = ~g, x = ~pct, color = ~second_level, colors = CAT_COLORS, type = "bar", orientation = "h",
            text = ~sprintf("%s<br>%s: %.1f%%", g, second_level, 100 * pct), hoverinfo = "text") %>%
      layout(barmode = "stack", xaxis = list(tickformat = ".0%", title = ""), yaxis = list(title = ""), legend = list(orientation = "h", y = -0.25, traceorder = "reversed")) %>%
      plot_config()
  })
  output$multi <- renderDT({
    m <- ADM %>% group_by(source, patient_id) %>% summarise(cats = paste(sort(unique(second_level)), collapse = " + "), k = n_distinct(second_level), .groups = "drop") %>%
      filter(k > 1) %>% count(cats, sort = TRUE, name = "patients") %>% mutate(patients = mask_n(patients, TRUE))
    datatable(m, rownames = FALSE, options = list(pageLength = 12, dom = .dt_dom_search), class = "compact")
  })
})

# ---- Cohorts: the cohort balance in this schema. New cohorts are new projects,
# built on the Setup page.

cohorts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      stat_box("Cohort admissions", fmt_n(nrow(ADM)), "hospital"),
      stat_box("Cohort patients", fmt_n(nrow(PATIENTS)), "people"),
      stat_box("Other admissions of these patients", fmt_n(sum(!CTX_ADM$in_cohort)), "clock-history")),
    layout_columns(col_widths = c(7, 5), fill = FALSE,
      card(full_screen = TRUE, card_header("Cohort and other admissions by source"),
           withSpinner(plotlyOutput(ns("balance"), height = "260px"))),
      card(card_header("Cohort table"),
           tags$p(class = "small text-muted",
                  "Cohort admissions are the rows flagged as cohort members in the merged cohort_info; the other admissions ",
                  "of the same patients are kept as context for longitudinal views."),
           DTOutput(ns("inv")))),
    card(card_header("Build a new cohort"),
      tags$p("A new cohort is a new project: choose its sources, cohort definition (bundled, an ICD list or an explicit ID list) ",
             "and database on the Setup page, then run the pipeline there. Its progress and logs appear under Background tasks."),
      tags$div(actionButton(ns("to_setup"), "Open Setup", icon = icon("arrow-right"), class = "btn-primary")))
  )
}

cohorts_server <- function(id, goto_nav = function(panel) NULL) moduleServer(id, function(input, output, session) {
  output$balance <- renderPlotly({
    coh <- ADM %>% count(source_label) %>% mutate(what = "Cohort admission")
    ctx <- CTX_ADM %>% filter(!in_cohort) %>% count(source_label) %>% mutate(what = "Context admission")
    d <- bind_rows(coh, ctx) %>% mutate(what = factor(what, c("Cohort admission", "Context admission")))
    cols <- TL_COLORS[c("Cohort admission", "Context admission")]
    plot_ly(d, y = ~source_label, x = ~n, color = ~what, colors = cols, type = "bar", orientation = "h",
            text = ~sprintf("%s<br>%s: %s", source_label, what, fmt_n(n)), hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "stack", xaxis = list(title = "Admissions"), yaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.25)) %>% plot_config()
  })
  output$inv <- renderDT(datatable({
    d <- ADM %>% group_by(Source = source_label) %>%
      summarise(Admissions = n(), Patients = n_distinct(patient_id), `Index admissions` = sum(is_index), .groups = "drop") %>%
      mutate(across(where(is.numeric), ~ fmt_n(.)))
    d
  }, rownames = FALSE, options = list(dom = "t", pageLength = 20), class = "compact"))
  observeEvent(input$to_setup, goto_nav("setup"))
})

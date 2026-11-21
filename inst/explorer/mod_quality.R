# ---- Data quality: structural checks of the bundle and live per-patient checks --------------------------------------------
quality_ui <- function(id) {
  ns <- NS(id)
  cs <- checks_summary(D$checks)
  tagList(
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      stat_box("Structural checks passed", cs$text, "check2-circle", cs$tone),
      stat_box("Merged schema", D$meta$schema, "diagram-3"),
      stat_box("Database connection", if (LIVE) "Connected" else "Offline (bundle only)", "plug", if (LIVE) "success" else "warning")),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Structural checks",
        tags$p(class = "small text-muted", "N/A: not possible with this schema (for example, no visit windows); not counted."),
        DTOutput(ns("checks"), fill = FALSE)),
      nav_panel("Live random-patient check",
        layout_columns(col_widths = c(3, 3, 6), numericInput(ns("nrand"), "Patients per source", 3, 1, 20), actionButton(ns("run_live"), "Run check", icon = icon("play"), class = "btn-primary mt-4"),
                       tags$p(class = "small text-muted", "Random patients read from the per-source tables and from the merged schema must give identical values.")),
        withSpinner(DTOutput(ns("live")))),
      nav_panel("Rows per source", tags$p(class = "small text-muted", "Rows of each merged table, per source."),
                actionButton(ns("run_rows"), "Count", icon = icon("calculator")), withSpinner(DTOutput(ns("rows")))))
  )
}
quality_server <- function(id) moduleServer(id, function(input, output, session) {
  output$checks <- renderDT(status_style(datatable(D$checks, rownames = FALSE, colnames = c("Check", "Status", "Detail"),
                                                   options = list(dom = "t", pageLength = 50), class = "compact")))
  live <- eventReactive(input$run_live, { req(LIVE)
    n <- suppressWarnings(as.integer(input$nrand))
    validate(need(!is.na(n) && n >= 1, "Choose at least one patient per source."))
    pats <- PATIENTS %>% group_by(source) %>% slice_sample(n = n) %>% ungroup()
    withProgress(message = "Checking random patients...", value = 0, bind_rows(lapply(seq_len(nrow(pats)), function(i) {
      incProgress(1 / nrow(pats)); s <- pats$source[i]; p <- pats$patient_id[i]
      tryCatch({
        a <- patient_ts(s, p, "source"); b <- patient_ts(s, p, "merged"); j <- .xp_compare_routes(a, b)
        tibble(source = SRC_LABEL[[s]], patient_id = p, rows_per_source = nrow(a), rows_merged = nrow(b),
               concepts = n_distinct(a$concept_raw),
               status = if (!nrow(a) && !nrow(b)) "N/A" else if (nrow(j) && all(j$match)) "PASS" else "FAIL",
               detail = if (!nrow(a) && !nrow(b)) "no time series for this patient" else "")
      }, error = function(e) tibble(source = SRC_LABEL[[s]], patient_id = p, rows_per_source = NA_integer_, rows_merged = NA_integer_,
                                     concepts = NA_integer_, status = "FAIL", detail = conditionMessage(e)))
    }))) })
  output$live <- renderDT(status_style(datatable(live(), rownames = FALSE, options = list(dom = "t", pageLength = 60), class = "compact")))
  rows <- eventReactive(input$run_rows, { req(LIVE)
    .with_connection(NULL, function(con) bind_rows(lapply(c("cohort_info", "flat", "ts", "dur"), function(t) {
      if (!.xp_table_exists(con, D$meta$schema, t)) return(NULL)
      x <- DBI::dbGetQuery(con, sprintf("SELECT source_data AS source, COUNT(*) AS n FROM %s GROUP BY 1", .xp_quote_table(con, D$meta$schema, t)))
      tibble(table = t, source = coalesce(unname(SRC_LABEL[x$source]), x$source), rows = as.numeric(x$n))
    }))) %>% tidyr::pivot_wider(names_from = source, values_from = rows) })
  output$rows <- renderDT(datatable(rows(), rownames = FALSE, options = list(dom = "t"), class = "compact") %>%
                            formatRound(columns = setdiff(names(rows()), "table"), digits = 0))
})

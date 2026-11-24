# ---- Cohorts: the cohort open in this session, other merged schemas in the
# database, and a short path to build a new cohort as a new project.

cohorts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      stat_box("Cohort admissions", fmt_n(nrow(ADM)), "hospital",
               sub = paste(c(XDB_NAME %||% .ehr_env$db_name, D$meta$schema), collapse = " · ")),
      stat_box("Cohort patients", fmt_n(nrow(PATIENTS)), "people"),
      stat_box("Other admissions of these patients", fmt_n(sum(!CTX_ADM$in_cohort)), "clock-history")),
    uiOutput(ns("available")),
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

cohorts_server <- function(id, goto_nav = function(panel) NULL, switch_cohort = function(schema) NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    schemas <- reactive({
      sch <- unique(c(D$meta$schema, XSCHEMAS %||% character()))
      if (LIVE) sch <- unique(c(sch, tryCatch(list_explorer_schemas(), error = function(e) character())))
      sch[nzchar(sch)]
    })
    output$available <- renderUI({
      sch <- schemas()
      if (!length(sch)) return(NULL)
      db_cur <- XDB_NAME; if (is.null(db_cur) || is.na(db_cur) || !nzchar(db_cur))
        db_cur <- .ehr_env$db_name %||% ""
      dbs <- unique(c(db_cur, XDBS %||% character()))
      if (LIVE) dbs <- unique(c(dbs, tryCatch(list_explorer_databases(), error = function(e) character())))
      dbs <- dbs[!is.na(dbs) & nzchar(dbs)]
      card(card_header("Database and cohort"),
           tags$p(class = "small text-muted mb-2",
                  "Only databases with an admission-level merged cohort are listed. ",
                  "Overview, Cohort Explorer, Subject Explorer and Concepts all follow the choice here or in the top bar. ",
                  "A failed switch restores the previous database."),
           layout_columns(col_widths = c(5, 5, 2), fill = FALSE,
             selectInput(ns("pick_db"), "Database", choices = dbs, selected = db_cur),
             selectInput(ns("pick"), "Cohort (merged schema)", choices = sch, selected = D$meta$schema),
             tags$div(class = "pt-4",
                      if (LIVE) actionButton(ns("open"), "Open", icon = icon("folder-open"),
                                             class = "btn-outline-primary w-100"))))
    })
    observeEvent(input$open, {
      req(input$pick_db, input$pick)
      db <- input$pick_db; sch <- input$pick
      cur_db <- XDB_NAME; if (is.null(cur_db) || is.na(cur_db) || !nzchar(cur_db))
        cur_db <- .ehr_env$db_name %||% ""
      if (!identical(db, cur_db)) {
        id <- "db_load"
        showNotification(
          sprintf("Opening '%s' and loading cohort — this can take up to a minute…", db),
          duration = NULL, id = id, type = "message")
        ok <- tryCatch(reload_explorer_database(db, schema = sch), error = function(e) e)
        removeNotification(id)
        if (inherits(ok, "error"))
          return(showNotification(conditionMessage(ok), type = "error", duration = 15))
        showNotification(sprintf("Loaded '%s'. Refreshing…", db), duration = 3, type = "message")
        return(session$reload())
      }
      switch_cohort(sch)
    }, ignoreInit = TRUE)
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
}

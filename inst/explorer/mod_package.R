# ---- Package: manage the installation around the data - libraries and
# versions, source catalogue and raw files, databases, exports - plus the full
# background-task manager. Everything that changes state asks first; nothing
# here touches data without a typed confirmation or an explicit button.

pkg_ui <- function(id) {
  ns <- NS(id)
  navset_underline(
    nav_panel("Libraries & versions",
      layout_columns(col_widths = c(6, 6),
        card(card_header("This session"), withSpinner(uiOutput(ns("env")))),
        card(card_header("Installed copies of rendehR"),
             tags$p(class = "small text-muted", "Each library on the search path with a copy, and the commit it was built from."),
             DTOutput(ns("copies"))))),
    nav_panel("Sources & raw data",
      layout_columns(col_widths = c(5, 7),
        card(card_header("Registered source families"),
             tags$p(class = "small text-muted", "Families the package knows how to read."),
             DTOutput(ns("families"))),
        card(card_header("Raw data and harmonized tables"),
             tags$p(class = "small text-muted", "Raw files need a configured project; harmonized tables need the database."),
             actionButton(ns("scan"), "Scan raw + tables", icon = icon("magnifying-glass"), class = "btn-outline-secondary"),
             tags$div(style = "margin-top:0.6rem", withSpinner(DTOutput(ns("srcs")))),
             tags$div(style = "margin-top:0.6rem", uiOutput(ns("verify")))))),
    nav_panel("Databases",
      card(card_header("PostgreSQL databases (rendehr*)"),
           tags$p(class = "small text-muted",
                  "Every rendehr* database on the server. The connected database cannot be dropped here."),
           actionButton(ns("dbs"), "Refresh list", icon = icon("rotate"), class = "btn-outline-secondary"),
           withSpinner(DTOutput(ns("dblist"))),
           tags$hr(),
           layout_columns(col_widths = c(5, 7),
             div(selectizeInput(ns("drop_name"), "Database to drop", choices = character(),
                                options = list(placeholder = "Choose a database")),
                 actionButton(ns("drop"), "Drop database…", icon = icon("trash"), class = "btn-outline-danger")),
             div(textInput(ns("create_name"), "New database name"),
                 actionButton(ns("create"), "Create database", icon = icon("plus"), class = "btn-outline-secondary")))),
      ),
    nav_panel("Export",
      card(card_header("Export a table"),
           tags$p(class = "small text-muted",
                  "Writes one table of the merged schema to a file in the background; the extension sets the format."),
           layout_columns(col_widths = c(4, 8),
             selectInput(ns("ex_table"), "Table", choices = character()),
             textInput(ns("ex_path"), "Destination file (.parquet or .csv.gz)",
                       value = file.path(getwd(), "export.parquet"))),
           actionButton(ns("ex_go"), "Start export", icon = icon("play"), class = "btn-primary"))),
    nav_panel("Tasks", jobs_ui(ns("jobs")))
  )
}

pkg_server <- function(id) moduleServer(id, function(input, output, session) {
  # ---- environment -------------------------------------------------------------------------------
  output$env <- renderUI({
    pk <- c("shiny", "bslib", "plotly", "DT", "survival", "ricu")
    pkv <- vapply(pk, function(p) if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "-", character(1))
    row <- function(k, ...) tagList(tags$dt(class = "col-sm-3 fw-normal text-muted", k), tags$dd(class = "col-sm-9", ...))
    tags$dl(class = "row small mb-0",
      row("rendehR", as.character(utils::packageVersion("rendehR"))),
      row("Loaded from", tags$code(dirname(find.package("rendehR")))),
      row("R", sub("^R version ", "", R.version.string)),
      row("Packages", paste(sprintf("%s %s", pk, pkv), collapse = ", ")),
      row("Schema", sprintf("%s (built %s)", D$meta$schema, D$meta$built)),
      row("Database", if (LIVE) sub(" on .*$", "", dbserver_version()) else "not connected"),
      row("Libraries", tags$ol(class = "ps-3 mb-0", lapply(.libPaths(), function(p) tags$li(tags$code(p))))))
  })
  dbserver_version <- function() tryCatch({
    .with_connection(NULL, function(con) DBI::dbGetQuery(con, "SELECT version()")[[1]][1])
  }, error = function(e) "unknown")
  output$copies <- renderDT({
    paths <- unique(c(.libPaths(), dirname(find.package("rendehR"))))
    rows <- lapply(paths, function(p) {
      d <- file.path(p, "rendehR")
      if (!dir.exists(d)) return(NULL)
      ver <- tryCatch(as.character(utils::packageVersion("rendehR", lib.loc = p)), error = function(e) "?")
      sf <- file.path(d, "INSTALLED_SHA")
      sha <- if (file.exists(sf)) tryCatch(trimws(readLines(sf, n = 1, warn = FALSE)), error = function(e) "") else ""
      data.frame(Library = p, Version = ver, Commit = if (nzchar(sha)) substr(sha, 1, 7) else "-")
    })
    d <- do.call(rbind, Filter(Negate(is.null), rows))
    if (is.null(d)) d <- data.frame(Library = character(), Version = character(), Commit = character())
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 20), class = "compact")
  })

  # ---- sources and raw data ----------------------------------------------------------------------
  output$families <- renderDT({
    fam <- tryCatch(rendehR::list_registered_sources(), error = function(e) NULL)
    validate(need(!is.null(fam), "Source catalogue unavailable."))
    d <- if (is.data.frame(fam)) fam
         else if (length(fam)) data.frame(family = unname(fam))
         else data.frame(family = character())
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 30), class = "compact")
  })
  scan_result <- eventReactive(input$scan, {
    rows <- lapply(unname(SRC), function(s) {
      raw_n <- tryCatch({
        p <- ehr_paths(s)
        if (dir.exists(p$raw_dir)) length(list.files(p$raw_dir, recursive = TRUE)) else NA_integer_
      }, error = function(e) NA_integer_)
      tabs <- tryCatch({
        if (!LIVE) NA_integer_
        else .with_connection(NULL, function(con) length(list_tables(s, "harmonized", con = con)))
      }, error = function(e) NA_integer_)
      data.frame(Source = unname(SRC_LABEL[[s]]), `Raw files` = raw_n, `Harmonized tables` = tabs)
    })
    do.call(rbind, rows)
  })
  output$srcs <- renderDT({
    d <- scan_result()
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 20), class = "compact") %>%
      formatStyle(columns = names(d), color = "inherit")
  })
  ver_src <- eventReactive(input$scan, {
    res <- vapply(unname(SRC), function(s) tryCatch({
      v <- rendehR::verify_download(ehr_paths(s))
      if (all(unlist(v$checks))) "ok" else paste(names(v$checks)[!v$checks], collapse = ", ")
    }, error = function(e) paste("not available:", conditionMessage(e))), character(1))
    paste(sprintf("%s: %s", unname(SRC_LABEL), res), collapse = " | ")
  })
  output$verify <- renderUI(tags$div(class = "small text-muted", ver_src()))

  # ---- databases ---------------------------------------------------------------------------------
  admin_con <- function() connect_postgres("postgres", .ehr_env$creds)
  db_refresh <- reactiveVal(0)
  connected_db <- function() .ehr_env$db_name %||% ""
  db_list <- eventReactive(list(input$dbs, db_refresh()), {
    req(LIVE)
    tryCatch({
      con <- admin_con(); on.exit(DBI::dbDisconnect(con))
      DBI::dbGetQuery(con, "SELECT datname AS database, pg_size_pretty(pg_database_size(datname)) AS size
                            FROM pg_database WHERE datname LIKE 'rendehr%' ORDER BY pg_database_size(datname) DESC")
    }, error = function(e) data.frame(database = paste("unavailable:", conditionMessage(e)), size = ""))
  })
  output$dblist <- renderDT(datatable(db_list(), rownames = FALSE, options = list(dom = "t", pageLength = 30), class = "compact"))
  observe({
    d <- db_list()
    # nothing is preselected: a drop always starts from an explicit choice
    updateSelectizeInput(session, "drop_name", choices = setdiff(d$database[!grepl("^unavailable:", d$database)], connected_db()),
                         selected = character(0))
  })
  observeEvent(input$drop, {
    nm <- input$drop_name
    req(nzchar(nm %||% ""), !is.null(.ehr_env$creds))
    showModal(modalDialog(title = sprintf("Drop %s?", nm),
      tags$p("This permanently removes the database ", tags$code(nm), " and everything in it. It cannot be undone."),
      textInput(session$ns("drop_confirm"), sprintf("Type %s to confirm", nm)),
      footer = tagList(modalButton("Cancel"),
                       actionButton(session$ns("drop_yes"), "Drop database", class = "btn-danger"))))
  })
  observeEvent(input$drop_yes, {
    nm <- input$drop_name; typed <- trimws(input$drop_confirm %||% "")
    if (!identical(typed, nm)) {
      showNotification("The typed name does not match; nothing was dropped.", type = "warning", duration = 6)
      return(invisible(NULL))
    }
    removeModal()
    if (identical(nm, connected_db())) {
      showNotification("This session is connected to that database; it was not dropped.", type = "error", duration = 8)
      return(invisible(NULL))
    }
    r <- tryCatch(isTRUE(suppressMessages(rendehR::drop_database(nm, creds = .ehr_env$creds, dry_run = FALSE, confirm = typed))),
                  error = function(e) conditionMessage(e))
    db_refresh(db_refresh() + 1)
    showNotification(if (isTRUE(r)) sprintf("Dropped %s.", nm) else sprintf("%s was not dropped: %s", nm, if (is.character(r)) r else "not confirmed"),
                     type = if (isTRUE(r)) "message" else "error", duration = 8)
  })
  observeEvent(input$create, {
    nm <- trimws(input$create_name); req(nzchar(nm))
    r <- tryCatch({ rendehR::ensure_database_exists(nm, .ehr_env$creds); "created" },
                  error = function(e) conditionMessage(e))
    db_refresh(db_refresh() + 1)
    showNotification(if (identical(r, "created")) sprintf("%s is ready.", nm) else sprintf("%s was not created: %s", nm, r),
                     type = if (identical(r, "created")) "message" else "error", duration = 8)
  })

  # ---- export ------------------------------------------------------------------------------------
  observe({
    tabs <- tryCatch({
      if (!LIVE) character() else .with_connection(NULL, function(con) DBI::dbGetQuery(con,
        sprintf("SELECT table_name FROM information_schema.tables WHERE table_schema = '%s' ORDER BY 1", D$meta$schema))[[1]])
    }, error = function(e) character())
    ex_tables(tabs)
    updateSelectInput(session, "ex_table", choices = tabs)
  })
  ex_tables <- reactiveVal(character())
  start_export <- function(path) {
    q <- .with_connection(NULL, function(con)
      sprintf("SELECT * FROM %s", DBI::dbQuoteIdentifier(con, DBI::Id(schema = D$meta$schema, table = input$ex_table))))
    job_start(sprintf("Export %s", input$ex_table),
      c(sprintf("export_query(%s, %s)", deparse(q), deparse(path)),
        sprintf('cat("written:", %s, "\\n")', deparse(path))),
      wd = JOB_CTX$base_dir, config = JOB_CTX$config)
    showNotification("Export started; its log is on the Tasks tab.", type = "message", duration = 6)
  }
  observeEvent(input$ex_go, {
    req(LIVE, nzchar(input$ex_table), input$ex_table %in% ex_tables(), nzchar(input$ex_path))
    if (is.null(JOB_CTX$config)) {
      showNotification("Exports run as background tasks against a configured project; apply one on the Setup page first.",
                       type = "warning", duration = 8)
      return(invisible(NULL))
    }
    path <- trimws(input$ex_path)
    if (!grepl("[.](parquet|csv[.]gz)$", path, ignore.case = TRUE)) {
      showNotification("Give the destination a .parquet or .csv.gz extension; the extension picks the format.",
                       type = "warning", duration = 8)
      return(invisible(NULL))
    }
    if (file.exists(path)) {
      showModal(modalDialog(title = "Replace the existing file?",
        tags$p(tags$code(path), " already exists. The export will overwrite it."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("ex_overwrite"), "Overwrite", class = "btn-danger"))))
      return(invisible(NULL))
    }
    start_export(path)
  })
  observeEvent(input$ex_overwrite, { removeModal(); start_export(trimws(input$ex_path)) })
  jobs_server("jobs")
})

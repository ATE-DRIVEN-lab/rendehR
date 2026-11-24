# ---- Setup: one workflow page. The project form (sources, backend, cohort,
# credentials) creates the configuration or writes the equivalent script; the
# pipeline card runs the package's own stages for every source of the project
# as background jobs; merging is a separate, confirmed step.

.setup_chip <- function(ok, label) {
  cls <- if (isTRUE(ok)) "success" else if (is.na(ok)) "secondary" else "danger"
  tags$span(class = sprintf("badge rounded-pill text-bg-%s", cls), label)
}

# Sources grouped for the picker: demo releases first, then the credentialed ones.
.setup_source_choices <- function() {
  s <- tryCatch(rendehR:::.setup_sources(),
                error = function(e) list(demo = c("mimic_iv_demo", "mimic_iii_demo", "eicu_demo"),
                                         full = c("mimic_iv", "mimic_iii", "eicu")))
  list(`Demo (open access)` = unname(s$demo), `Full (credentialed PhysioNet access)` = unname(s$full))
}
.setup_cohort_choices <- function() {
  ex <- tryCatch(rendehR::list_example_cohorts(), error = function(e) data.frame(name = character(), description = character()))
  c(stats::setNames(ex$name, sprintf("%s: %s", ex$name, sub("\\s+-.*$", "", ex$description))),
    "Custom ICD list (CSV)" = "custom", "Explicit ID list" = "ids", "All patients (no cohort filter)" = "none")
}
.setup_stage_labels <- c(
  cache_raw_parquet = "Cache raw files as Parquet", import_cohort = "Import cohort", run_db = "Build database",
  run_raw_mods = "Raw-table modifications", build_window_tables = "Visit and ICU windows",
  build_cohort_annotations = "Cohort annotations (ICD classification)", build_cohort_info = "Cohort info",
  run_comorbidities = "Comorbidities (Charlson, Elixhauser)", run_iss = "Injury severity score (ISS)",
  run_concepts = "Concepts", run_external_concepts = "External SQL concepts", run_harmonized_tables = "Harmonized tables")
.setup_default_stages <- c("import_cohort", "run_db", "run_raw_mods", "build_window_tables", "build_cohort_info",
                           "run_concepts", "run_harmonized_tables")
.setup_postgres_only <- c("build_cohort_annotations", "run_comorbidities", "run_iss", "run_external_concepts",
                          "run_harmonized_tables")

setup_ui <- function(id) {
  ns <- NS(id)
  full_src <- sprintf("input['%s'] && [].concat(input['%s']).some(function(s) { return !/_demo$/.test(s); })", ns("src"), ns("src"))
  tagList(
    layout_columns(col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      card(card_header("1. Project"), card_body(fillable = FALSE,
        textInput(ns("base_dir"), "Project folder", file.path(path.expand("~"), "rendehr_project"), width = "100%"),
        layout_columns(fill = FALSE, col_widths = c(6, 6),
          textInput(ns("db_name"), "Database name", "rendehr_project"),
          radioButtons(ns("engine"), "Backend", c("PostgreSQL" = "postgres", "DuckDB (local file)" = "duckdb"), inline = TRUE)),
        pickerInput(ns("src"), "Sources", .setup_source_choices(), selected = "mimic_iv_demo", multiple = TRUE, width = "100%",
                    options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3",
                                   `none-selected-text` = "Choose at least one source")),
        textInput(ns("raw_dir"), "Raw data root (one subfolder per source)", placeholder = "default: <project folder>/data/source_data", width = "100%"),
        layout_columns(fill = FALSE, col_widths = c(7, 5),
          selectInput(ns("cohort"), "Cohort", .setup_cohort_choices()),
          selectInput(ns("scope"), "Import around each cohort member",
                      c("Whole patient (default)" = "", "Hospital admission" = "hospital", "ICU stay" = "icu"))),
        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("cohort")),
                         textInput(ns("icd_path"), "ICD list CSV", placeholder = "columns: icd_code, optional icd_version (9/10)")),
        conditionalPanel(sprintf("input['%s'] == 'ids'", ns("cohort")),
          layout_columns(fill = FALSE, col_widths = c(8, 4),
            textAreaInput(ns("ids"), "IDs (one per line, or separated by commas)", rows = 3),
            selectInput(ns("key"), "ID column", choices = character()))),
        conditionalPanel(sprintf("input['%s'] == 'postgres'", ns("engine")),
          tags$h6(class = "mt-2", "PostgreSQL"),
          layout_columns(fill = FALSE, col_widths = c(4, 2, 3, 3),
            textInput(ns("pg_host"), "Host", "localhost"),
            textInput(ns("pg_port"), "Port", "5432"),
            textInput(ns("pg_user"), "User", "postgres"),
            passwordInput(ns("pg_pass"), "Password"))),
        conditionalPanel(full_src,
          tags$h6(class = "mt-2", "PhysioNet account (for downloading credentialed sources)"),
          layout_columns(fill = FALSE, col_widths = c(6, 6),
            textInput(ns("pn_user"), "Username"),
            passwordInput(ns("pn_pass"), "Password"))),
        tags$p(class = "small text-muted", "Saved to config/config_pass.json in the project folder, readable only by you."),
        checkboxInput(ns("overwrite"), "Replace an existing configuration in this folder", FALSE),
        button_row(actionButton(ns("apply"), "Create project", class = "btn-primary", icon = icon("folder-plus")),
                   actionButton(ns("script"), "Write as a script instead", icon = icon("code"))),
        uiOutput(ns("apply_result")))),
      tagList(
        card(card_header("2. Pipeline"), card_body(fillable = FALSE,
          uiOutput(ns("project")),
          uiOutput(ns("stages_ui")),
          checkboxInput(ns("redo"), "Rebuild stages whose outputs already exist", FALSE),
          button_row(actionButton(ns("run"), "Run selected stages", class = "btn-primary", icon = icon("play")),
                     actionButton(ns("download"), "Download raw data", icon = icon("cloud-arrow-down")),
                     actionButton(ns("status"), "Check progress", icon = icon("list-check"))),
          tags$p(class = "small text-muted mt-2 mb-0",
                 "Runs in the background; completed stages are skipped unless Rebuild is ticked."))),
        conditionalPanel(sprintf("input['%s'] == 'postgres'", ns("engine")),
          card(card_header("3. Merge sources"), card_body(fillable = FALSE,
            tags$p(class = "small text-muted", "One schema with the harmonized tables of all project sources. Without Append it is rebuilt from these sources only."),
            layout_columns(fill = FALSE, col_widths = c(6, 6),
              textInput(ns("target"), "Target schema", "rendehr"),
              checkboxInput(ns("append"), "Append to the existing schema", FALSE)),
            tags$div(actionButton(ns("merge"), "Merge sources…", icon = icon("code-merge")))))))),
    uiOutput(ns("script_card")),
    card(card_header("Background tasks"), jobs_ui(ns("jobs"))),
    card(card_header("This installation"), withSpinner(uiOutput(ns("ready")))))
}

setup_server <- function(id, goto_nav = function(panel) NULL) moduleServer(id, function(input, output, session) {
  ns <- session$ns

  # ---- the form, validated once for every action ---------------------------------------------
  form <- reactive({
    src <- input$src %||% character()
    validate(
      need(length(src) > 0, "Choose at least one source."),
      need(all(grepl("_demo$", src)) || !any(grepl("_demo$", src)),
           "Use either demo or full sources in one project, not both."),
      need(nzchar(trimws(input$base_dir)), "Set the project folder."),
      need(grepl("^[a-z_][a-z0-9_]*$", input$db_name), "Database names use lower-case letters, digits and _."),
      need(input$cohort != "custom" || (nzchar(input$icd_path) && file.exists(input$icd_path)),
           "Give an existing ICD list CSV for the custom cohort."),
      need(input$cohort != "ids" || length(src) == 1, "An ID list belongs to one source; choose a single source."),
      need(input$cohort != "ids" || length(id_list()) > 0, "Paste at least one ID."),
      need(input$engine == "duckdb" || !is.na(suppressWarnings(as.integer(input$pg_port))), "The PostgreSQL port must be a number."))
    list(src = src, base_dir = path.expand(trimws(input$base_dir)), db_name = input$db_name, engine = input$engine,
         raw_dir = if (nzchar(trimws(input$raw_dir))) path.expand(trimws(input$raw_dir)) else NULL,
         cohort = list(cohort = input$cohort, icd_path = input$icd_path, scope = input$scope,
                       ids = id_list(), key = input$key))
  })
  id_list <- reactive({ x <- trimws(unlist(strsplit(input$ids %||% "", "[[:space:],;]+"))); x[nzchar(x)] })
  # the ID column depends on the source (subject_id, hadm_id, ... or uniquepid, patientunitstayid, ...)
  observeEvent(input$src, {
    lad <- tryCatch(rendehR:::.cohort_id_ladder(input$src[1]), error = function(e) character())
    updateSelectInput(session, "key", choices = lad, selected = if (isTRUE(input$key %in% lad)) input$key else lad[min(2L, length(lad))])
  }, ignoreNULL = TRUE)

  # ---- create the project (no change to this app's own session) -------------------------------
  applied <- reactiveVal(NULL)
  observeEvent(input$apply, {
    f <- tryCatch(form(), error = function(e) conditionMessage(e))
    if (is.character(f)) return(applied(f))
    res <- tryCatch({
      cfg_dir <- file.path(f$base_dir, "config")
      cfg_path <- file.path(cfg_dir, "ehr_config.json"); cred_path <- file.path(cfg_dir, "config_pass.json")
      if ((file.exists(cfg_path) || file.exists(cred_path)) && !isTRUE(input$overwrite))
        stop("This folder already has a configuration. Tick \"Replace an existing configuration\" to overwrite it, ",
             "or choose another folder.")
      dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
      full <- !any(grepl("_demo$", f$src))
      rendehR::ehr_create_config_template(
        path = cfg_path, src_name = f$src[1], db_name = f$db_name, base_dir = f$base_dir,
        raw_data_root = f$raw_dir %||% file.path(f$base_dir, "data", "source_data"),
        cohort_parquet_root = file.path(f$base_dir, "data", "cohorts_data"),
        duckdb_root = file.path(f$base_dir, "data", "duckdb"), credentials_path = cred_path,
        cohort_profile = if (f$cohort$cohort %in% c("scid", "sepsis3")) f$cohort$cohort else "generic",
        overwrite = isTRUE(input$overwrite))
      rendehR::ehr_create_credentials_template(
        path = cred_path, postgres_host = input$pg_host, postgres_port = as.integer(input$pg_port),
        postgres_user = input$pg_user, postgres_password = input$pg_pass,
        physionet_username = if (full && nzchar(input$pn_user)) input$pn_user,
        physionet_password = if (full && nzchar(input$pn_pass)) input$pn_pass,
        overwrite = isTRUE(input$overwrite))
      Sys.chmod(cred_path, "0600")
      if (identical(f$cohort$cohort, "ids")) {                      # the import reads the IDs from a CSV
        f$cohort$ids_path <- file.path(cfg_dir, "cohort_ids.csv")
        utils::write.csv(stats::setNames(data.frame(f$cohort$ids), f$cohort$key), f$cohort$ids_path, row.names = FALSE)
      }
      chk <- tryCatch(rendehR::ehr_check_config(config_path = cfg_path, src_name = f$src[1], quiet = TRUE),
                      error = function(e) list(ok = FALSE, problems = conditionMessage(e)))
      JOB_CTX$config <- cfg_path; JOB_CTX$base_dir <- f$base_dir; JOB_CTX$db_name <- f$db_name
      JOB_CTX$src <- f$src; JOB_CTX$engine <- f$engine; JOB_CTX$cohort <- f$cohort; JOB_CTX$raw_dir <- f$raw_dir
      list(paths = c(cfg_path, cred_path), chk = chk)
    }, error = function(e) conditionMessage(e))
    applied(res)
  })
  output$apply_result <- renderUI({
    res <- applied(); req(res)
    if (is.character(res)) return(tags$div(class = "alert alert-danger small py-2 mt-3 mb-0", res))
    tags$div(class = paste("alert small py-2 mt-3 mb-0", if (isTRUE(res$chk$ok)) "alert-success" else "alert-warning"),
      tags$strong(if (isTRUE(res$chk$ok)) "Project created." else "Project created, with problems to fix:"),
      if (length(res$chk$problems)) tags$ul(class = "mb-1", lapply(res$chk$problems, tags$li)),
      if (length(res$chk$warnings)) tags$div(class = "text-muted", paste(res$chk$warnings, collapse = "; ")),
      tags$div(tags$code(res$paths[1])), tags$div(tags$code(res$paths[2])))
  })

  # ---- pipeline ------------------------------------------------------------------------------------
  ready <- reactive(!is.null(JOB_CTX$config) && length(JOB_CTX$src) > 0)
  output$project <- renderUI({
    if (!ready()) return(tags$div(class = "alert alert-secondary small py-2",
                                  "Create a project on the left first; the pipeline then runs for all of its sources."))
    tags$dl(class = "row small mb-2",
      tags$dt(class = "col-sm-3", "Project"), tags$dd(class = "col-sm-9", tags$code(JOB_CTX$base_dir)),
      tags$dt(class = "col-sm-3", "Sources"), tags$dd(class = "col-sm-9", paste(JOB_CTX$src, collapse = ", ")),
      tags$dt(class = "col-sm-3", "Database"), tags$dd(class = "col-sm-9",
        sprintf("%s (%s)", JOB_CTX$db_name %||% "from the configuration", if (identical(JOB_CTX$engine, "duckdb")) "DuckDB" else "PostgreSQL")))
  })
  output$stages_ui <- renderUI({
    st <- names(.setup_stage_labels)
    if (identical(input$engine, "duckdb")) st <- setdiff(st, .setup_postgres_only)
    sel <- intersect(isolate(input$stages) %||% .setup_default_stages, st)
    checkboxGroupInput(ns("stages"), "Stages, run in this order",
                       stats::setNames(st, .setup_stage_labels[st]), selected = sel)
  })
  observe({
    for (b in c("run", "download", "status", "merge")) updateActionButton(session, b, disabled = !ready())
  })

  ctx_lines <- function() c(
    sprintf("src_names <- %s", paste(deparse(JOB_CTX$src), collapse = "")),
    sprintf("db_name   <- %s", deparse(JOB_CTX$db_name)),
    sprintf("base_dir  <- %s", deparse(JOB_CTX$base_dir)))
  cohort_code <- function() {
    co <- JOB_CTX$cohort %||% list(cohort = "scid", scope = "")
    args <- switch(co$cohort,
      custom = sprintf("icd_filter_path = %s", deparse(co$icd_path)),
      ids = sprintf("direct_ids_path = %s, key = %s", deparse(co$ids_path), deparse(co$key)),
      none = "no_filter = TRUE",
      sprintf("cohort = %s", deparse(co$cohort)))
    if (nzchar(co$scope %||% "")) args <- paste0(args, sprintf(", import_scope = %s", deparse(co$scope)))
    sprintf("cohort_selection_options(%s)", args)
  }
  raw_arg <- function() if (!is.null(JOB_CTX$raw_dir)) sprintf(", raw_data_root = %s", deparse(JOB_CTX$raw_dir)) else ""
  start_job <- function(name, lines) {
    if (!ready()) return(invisible(NULL))
    job_start(name, c(ctx_lines(), lines), wd = JOB_CTX$base_dir, config = JOB_CTX$config)
    showNotification(sprintf("%s started; follow it under Background tasks.", name), type = "message", duration = 5)
  }
  observeEvent(input$run, {
    st <- intersect(names(.setup_stage_labels), input$stages)
    if (!length(st)) return(showNotification("Tick at least one stage.", type = "warning", duration = 5))
    if (identical(JOB_CTX$engine, "duckdb")) st <- setdiff(st, .setup_postgres_only)
    start_job(sprintf("Pipeline: %s", paste(.setup_stage_labels[st], collapse = ", ")),
      sprintf("run_sources(src_names, db_name = db_name, base_dir = base_dir, engine = %s, stages = %s%s,\n    cohort_selection = %s,\n    execution = execution_options(overwrite = %s))",
              deparse(JOB_CTX$engine %||% "postgres"), paste(deparse(st), collapse = ""), raw_arg(), cohort_code(), isTRUE(input$redo)))
  })
  observeEvent(input$download, start_job("Download raw data",
    sprintf("download_sources(src_names%s)", raw_arg())))
  observeEvent(input$status, start_job("Pipeline progress",
    sprintf("print(ehr_suggest_next(src_names, db_name = db_name, base_dir = base_dir%s))", raw_arg())))

  observeEvent(input$merge, {
    req(ready())
    tgt <- trimws(input$target)
    if (!grepl("^[a-z_][a-z0-9_]*$", tgt)) return(showNotification("Schema names use lower-case letters, digits and _.", type = "warning"))
    showModal(modalDialog(title = sprintf("Merge into %s?", tgt),
      if (isTRUE(input$append)) tags$p("The harmonized tables of ", tags$strong(paste(JOB_CTX$src, collapse = ", ")),
                                       " are appended to ", tags$code(tgt), ".")
      else tags$p(tags$code(tgt), " is rebuilt from exactly ", tags$strong(paste(JOB_CTX$src, collapse = ", ")),
                  ". Sources merged there before and not listed here are removed from it."),
      if (identical(tgt, D$meta$schema)) tags$p(class = "text-danger", "This is the schema the explorer is showing now."),
      footer = tagList(modalButton("Cancel"), actionButton(ns("merge_yes"), "Merge", class = "btn-primary"))))
  })
  observeEvent(input$merge_yes, {
    removeModal()
    start_job(sprintf("Merge into %s", trimws(input$target)),
      sprintf("merge_sources(src_names, db_name = db_name, base_dir = base_dir, target_schema = %s, append = %s)",
              deparse(trimws(input$target)), isTRUE(input$append)))
  })
  jobs_server("jobs")

  # ---- the same choices as a script (rendehR::ehr_setup_assistant) ------------------------------
  script <- reactiveVal(NULL)
  observeEvent(input$script, {
    f <- tryCatch(form(), error = function(e) conditionMessage(e))
    if (is.character(f)) return(script(list(error = f)))
    if (identical(f$cohort$cohort, "ids"))
      return(script(list(error = "The setup script covers cohort definitions, ICD lists and all patients; for an ID list use Create project.")))
    tf <- tempfile(fileext = ".R")
    err <- tryCatch({
      suppressMessages(utils::capture.output(rendehR::ehr_setup_assistant(
        src_name = f$src, source_size = if (all(grepl("_demo$", f$src))) "demo" else "full", engine = f$engine,
        db_name = f$db_name, base_dir = f$base_dir, config_setup = "manual", include_disk_check = FALSE,
        cohort = f$cohort$cohort, icd_filter_path = if (identical(f$cohort$cohort, "custom")) f$cohort$icd_path,
        import_scope = if (nzchar(f$cohort$scope)) f$cohort$scope, pipeline = "stages", include_download = TRUE,
        raw_data_dir = f$raw_dir, analysis_level = "admission", make_icu_timeline = FALSE, materialize_icu = FALSE,
        merge = if (identical(f$engine, "postgres") && length(f$src) > 1) "admission" else "none",
        target_schema = trimws(input$target), inspect_options = "tables", load_method = "library",
        save_script = TRUE, setup_script_path = tf, interactive = FALSE), type = "output"))
      NULL
    }, error = function(e) conditionMessage(e))
    script(if (!is.null(err) || !file.exists(tf)) list(error = sprintf("The setup assistant did not write a script: %s", err %||% "no file"))
           else list(text = paste(readLines(tf, warn = FALSE), collapse = "\n")))
  })
  output$script_card <- renderUI({
    s <- script(); req(s)
    card(card_header("Setup script"),
      if (!is.null(s$error)) tags$div(class = "alert alert-danger small py-2 mb-0", s$error)
      else tagList(tags$p(class = "small text-muted", "Review it and run it block by block in your own R session; nothing here has been run."),
                   downloadButton(ns("dl"), "Download .R", class = "btn-outline-secondary mb-2"),
                   verbatimTextOutput(ns("code"))))
  })
  output$code <- renderText(script()$text)
  output$dl <- downloadHandler(filename = function() "rendehr_setup.R", content = function(file) writeLines(script()$text, file))

  # ---- installation readiness ------------------------------------------------------------------
  output$ready <- renderUI({
    fam <- tryCatch(rendehR::list_registered_sources(), error = function(e) NULL)
    fam <- if (is.data.frame(fam)) fam[[1]] else unname(unlist(fam))
    row <- function(ok, label, detail) tags$li(class = "mb-1", .setup_chip(ok, if (isTRUE(ok)) "ok" else if (is.na(ok)) "n/a" else "missing"),
                                               " ", tags$strong(label), tags$span(class = "text-muted", " ", detail))
    tags$ul(class = "list-unstyled small mb-0",
      row(TRUE, "rendehR", as.character(utils::packageVersion("rendehR"))),
      row(if (LIVE) TRUE else NA, "Database connection", if (LIVE) sprintf("schema %s", D$meta$schema) else "offline: the saved bundle only"),
      row(length(fam) > 0, "Registered source families", paste(fam, collapse = ", ")),
      row(ready(), "Project for background tasks", JOB_CTX$config %||% "none yet"))
  })
})

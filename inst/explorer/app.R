# Cohort explorer: page layout and top-level server. Sourced last by
# rendehR::explorer_app(); defines `ui` and `server`.

about_ui <- function() {
  page <- function(name, text) tags$li(tags$strong(name), " ", tags$span(class = "text-muted", text))
  layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)),
    card(card_header(tags$span(tags$img(src = "rendehr-explorer/logo.png", height = "28px", class = "me-2"), "About the explorer")),
      tags$p("Explore a cohort built with rendehR: cohort tables come from the merged harmonized schema, and patient time series are read live from the database."),
      tags$ul(class = "list-unstyled mb-0",
        page("Overview", "headline numbers, composition and outcomes by source, age and length of stay."),
        page("Cohort Explorer", "filters, characteristics table with SMDs, distributions, outcomes, trends, survival, cross-tabs, scatter plots."),
        page("Subject Explorer", "one patient: admissions, time series, medications and codes, checks."),
        page("Concepts", "concept inventory and provenance, distributions, variable dictionary, medications, concept builder."),
        if (SCI) page("SCI pages", "category rules and code list, SCI-specific analyses, biomarkers."),
        page("Manage", "cohort inventory, project setup and pipeline, libraries, databases, exports, and data checks of the merged schema."))),
    card(card_header("Definitions"),
      tags$dl(class = "mb-3",
        tags$dt("Cohort admission"), tags$dd(class = "text-muted", "An admission flagged as a cohort member; the patient's other admissions are kept as context."),
        tags$dt("Index admission"), tags$dd(class = "text-muted", "A patient's first cohort admission; patient-level views use it."),
        tags$dt("Clock"), tags$dd(class = "text-muted", "Hours since hospital admission, unless a view says otherwise."),
        tags$dt("Small-cell rule"), tags$dd(class = "text-muted", "Counts of 1-9 are shown as <10, with the cells that would reveal them."),
        tags$dt("Dates"), tags$dd(class = "text-muted", "Shifted per patient in MIMIC; eICU-CRD has offsets only.")),
      tags$div(class = "alert alert-warning small mb-2",
               "Individual-level clinical data. Use it on this machine under each source's data use agreement; do not share screenshots of patients or deploy the app."),
      tags$p(class = "small text-muted mb-0", sprintf(
        "Loaded %s | rendehR %s | schema %s | database %s",
        D$meta$built, D$meta$rendehR_version, D$meta$schema,
        if (LIVE) (XDB_NAME %||% .ehr_env$db_name %||% "connected") else "offline"))))
}

# bslib's "shiny" preset with the package's own ink and one slate-blue accent;
# everything else is the preset's.
.ui_theme <- function() {
  bs_theme(
    version = 5, preset = "shiny",
    fg = "#1f2328", bg = "#ffffff",
    primary = "#2f5577", secondary = "#5c6670",
    success = "#2f7d4f", warning = "#9a6b1f", danger = "#a53f3f", info = "#2f5577",
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    code_font = font_collection("ui-monospace", "SFMono-Regular", "Menlo", "Consolas", "monospace"),
    "font-size-base" = "0.9375rem"
  )
}

ui <- function(request) {
  ic <- function(name) bs_icon(name, class = "me-1")
  pages <- list(
    nav_panel("Overview", icon = ic("grid-1x2"), overview_ui("ov")),
    if (SCI) nav_panel("Cohort & categories", icon = ic("diagram-3"), cohort_ui("co")),
    nav_panel("Cohort Explorer", icon = ic("bar-chart-line"), explorer_ui("ex")),
    if (SCI) nav_panel("SCI-specific", icon = ic("activity"), sci_ui("sci")),
    if (SCI) nav_panel("Biomarkers", icon = ic("droplet"), bio_ui("bio")),
    nav_panel("Subject Explorer", value = "subject", icon = ic("person"), subject_ui("sub")),
    nav_panel("Concepts", icon = ic("journal-text"), concepts_ui("cn")),
    nav_spacer(),
    nav_item(uiOutput("cohort_pick")),
    nav_menu("Manage", icon = ic("sliders"), align = "right",
      nav_panel("Cohorts", value = "cohorts", icon = ic("people"), cohorts_ui("ch")),
      nav_panel("Setup", value = "setup", icon = ic("gear"), setup_ui("st")),
      nav_panel("Package", value = "package", icon = ic("box-seam"), pkg_ui("pk")),
      nav_panel("Data checks", value = "quality", icon = ic("check2-circle"), quality_ui("dq"))),
    nav_panel("About", icon = ic("info-circle"), about_ui()),
    nav_item(actionLink("help", bs_icon("question-circle"), class = "nav-link", title = "Help for this page",
                        `aria-label` = "Help for this page")),
    nav_item(actionLink("share", bs_icon("link-45deg"), class = "nav-link", title = "Link to this view",
                        `aria-label` = "Link to this view")),
    if (!LIVE) nav_item(tags$span(class = "navbar-text small text-warning-emphasis",
                                  title = "No database connection: pages use the saved bundle only.",
                                  bs_icon("plug", class = "me-1"), "Offline")))
  do.call(page_navbar, c(list(
    title = tags$span(tags$img(src = "rendehr-explorer/logo.png", height = "30px", style = "margin-right:6px;vertical-align:middle"), APP_TITLE),
    # pages with a sidebar fill the window: the sidebar runs the full height beside the content
    id = "nav", fillable = c("Cohort Explorer", "subject", if (SCI) c("SCI-specific", "Biomarkers")), window_title = APP_TITLE,
    theme = .ui_theme(),
    header = tags$head(tags$link(rel = "icon", type = "image/png", href = "rendehr-explorer/logo.png"),
                       tags$link(rel = "stylesheet", href = "rendehr-explorer/rendehr.css"))),
    Filter(Negate(is.null), pages)))
}

# Short help per page (navbar value) with the matching section of the explorer article.
ARTICLE_URL <- "https://ate-driven-lab.github.io/rendehR/articles/explorer.html"
PAGE_HELP <- list(
  Overview = c("overview", "The whole cohort without filters: headline counts, composition, outcomes, age and length of stay by source."),
  `Cohort Explorer` = c("cohort-explorer", "Filter the cohort in the sidebar; every tab is computed from the same filtered admissions. Counts of 1-9 are shown as <10 while the small-cell switch is on."),
  subject = c("subject-explorer", "One patient at a time: pick or draw a patient, then read the timeline, time series, medications and codes. Verify compares the per-source and merged reads."),
  Concepts = c("concepts", "The concepts extracted per source, their provenance, distributions over the cohort, and the concept builder."),
  cohorts = c("cohorts", "Which merged cohort is open, other cohorts in this database, and a path to build a new one from Setup."),
  setup = c("setup", "Configure a project, run the pipeline stages and merge the sources. Jobs run in the background with a live log."),
  package = c("package", "Installed copies of rendehR, registered sources, databases and table export."),
  quality = c("data-checks", "Structural checks of the merged schema and a live check that per-source and merged reads agree."),
  About = c("", "What the explorer shows and the conventions every page uses."),
  `Cohort & categories` = c("study-pages", "The study's SCI category rules, code list and mapping from source to category."),
  `SCI-specific` = c("study-pages", "SCI analyses by category: neurological level, completeness and complications."),
  Biomarkers = c("study-pages", "Biomarker distributions and trajectories for the SCI categories.")
)
# Only the page, the Cohort Explorer filters and its tab settings go into a link:
# buttons would re-run actions on opening it, and table inputs would bloat it.
BOOKMARK_KEEP <- paste0("^(nav|ex-(f_.+|unit|mask|tab|t1_group|t1_vars|t1_g1|t1_g2|d_var|d_group|d_geom|o_var|o_group|o_strat|",
                        "tr_var|tr_group|tr_scale|s_group|s_days|s_ci|x_row|x_col|x_pct|sc_x|sc_y|sc_col|sc_log|",
                        "tj_cn|tj_clock|tj_hz|tj_bw|tj_group|cmp_vars))$")

server <- function(input, output, session) {
  observe({
    ids <- names(input)
    # filters left at their default and the default Table 1 rows add nothing to a link but length
    idle <- vapply(EX_FILTERS, function(f) { id <- paste0("ex-f_", f$var); id %in% ids && !filter_active(f, input[[id]]) }, logical(1))
    idle <- paste0("ex-f_", vapply(EX_FILTERS, `[[`, "", "var"))[idle]
    if (setequal(input[["ex-t1_vars"]] %||% DEF$t1_vars, DEF$t1_vars)) idle <- c(idle, "ex-t1_vars")
    setBookmarkExclude(c(grep(BOOKMARK_KEEP, ids, value = TRUE, invert = TRUE), idle,
                         "cohort_schema", "explorer_db"))
  })
  # Database + cohort pickers: every page follows the chosen Postgres database
  # and its merged schema (Overview / Cohort Explorer / Subject / Concepts).
  # XDBS is filled at launch with databases that already have an admission-level
  # merged cohort; unsuitable names are not offered.
  .xp_cur_db <- function() {
    db <- XDB_NAME
    if (is.null(db) || is.na(db) || !nzchar(db)) db <- .ehr_env$db_name %||% ""
    db
  }
  output$cohort_pick <- renderUI({
    db_cur <- .xp_cur_db()
    dbs <- unique(c(db_cur, XDBS %||% character()))
    dbs <- dbs[!is.na(dbs) & nzchar(dbs)]
    sch <- unique(c(D$meta$schema, XSCHEMAS %||% character()))
    if (LIVE) sch <- unique(c(sch, tryCatch(list_explorer_schemas(), error = function(e) character())))
    sch <- sch[!is.na(sch) & nzchar(sch)]
    tip <- "Only databases with an admission-level merged cohort (cohort_info + flat)."
    tags$div(class = "rh-cohort-pick d-flex align-items-center gap-2 flex-wrap",
             title = tip,
             tags$span(class = "small text-muted text-nowrap", "Database"),
             if (length(dbs)) selectInput("explorer_db", NULL, choices = dbs, selected = db_cur,
                                          width = "16rem", selectize = FALSE)
             else tags$span(class = "small", db_cur %||% "—"),
             tags$span(class = "small text-muted text-nowrap", "Cohort"),
             if (length(sch)) selectInput("cohort_schema", NULL, choices = sch, selected = D$meta$schema,
                                          width = "12rem", selectize = FALSE)
             else tags$span(class = "small", D$meta$schema %||% "—"))
  })
  observeEvent(input$explorer_db, {
    req(nzchar(input$explorer_db %||% ""))
    cur <- .xp_cur_db()
    if (identical(input$explorer_db, cur)) return()
    if (!LIVE) {
      showNotification("Switching database needs a live connection.", type = "warning")
      updateSelectInput(session, "explorer_db", selected = cur)
      return()
    }
    target <- input$explorer_db
    id <- "db_load"
    showNotification(
      sprintf("Opening '%s' and loading its cohort — this can take up to a minute…", target),
      duration = NULL, id = id, type = "message")
    ok <- tryCatch(reload_explorer_database(target), error = function(e) e)
    removeNotification(id)
    if (inherits(ok, "error")) {
      showNotification(conditionMessage(ok), type = "error", duration = 15)
      updateSelectInput(session, "explorer_db", selected = cur)
      return()
    }
    showNotification(sprintf("Loaded '%s'. Refreshing…", target), duration = 3, type = "message")
    session$reload()
  }, ignoreInit = TRUE)
  observeEvent(input$cohort_schema, {
    req(nzchar(input$cohort_schema %||% ""))
    if (identical(input$cohort_schema, D$meta$schema)) return()
    if (!LIVE) {
      showNotification("Switching cohort needs a live database connection.", type = "warning")
      updateSelectInput(session, "cohort_schema", selected = D$meta$schema)
      return()
    }
    target <- input$cohort_schema
    prev <- D$meta$schema
    id <- "cohort_load"
    showNotification(
      sprintf("Loading cohort '%s' — this can take up to a minute…", target),
      duration = NULL, id = id, type = "message")
    ok <- tryCatch(reload_cohort_bundle(target, force = TRUE), error = function(e) e)
    removeNotification(id)
    if (inherits(ok, "error")) {
      showNotification(
        paste0("Cohort '", target, "' could not be opened: ", conditionMessage(ok)),
        type = "error", duration = 15)
      updateSelectInput(session, "cohort_schema", selected = prev)
      return()
    }
    showNotification(sprintf("Loaded cohort '%s'. Refreshing…", target), duration = 3, type = "message")
    session$reload()
  }, ignoreInit = TRUE)
  observeEvent(input$share, session$doBookmark())
  onBookmarked(function(url) showModal(urlModal(url, title = "Link to this view",
    subtitle = "Opens the explorer on this page with the same Cohort Explorer filters and settings. The link holds no data.")))
  observeEvent(input$help, {
    h <- PAGE_HELP[[input$nav %||% "Overview"]] %||% PAGE_HELP$About
    showModal(modalDialog(title = "Help", easyClose = TRUE, footer = modalButton("Close"),
      tags$p(h[2]),
      tags$p(class = "small text-muted mb-0", "More in the article ",
             tags$a(href = paste0(ARTICLE_URL, if (nzchar(h[1])) paste0("#", h[1])), target = "_blank", "The cohort explorer"),
             " (or vignette(\"explorer\", package = \"rendehR\")).")))
  })
  subject_request <- reactiveVal(NULL)
  goto_subject <- function(source, patient_id, adm_id = NULL) {
    subject_request(list(source = source, patient_id = patient_id, adm_id = adm_id, t = Sys.time()))
    nav_select("nav", "subject")
  }
  jobs_poller(input, output, session)
  overview_server("ov"); explorer_server("ex", goto_subject); subject_server("sub", subject_request)
  concepts_server("cn"); quality_server("dq")
  cohorts_server("ch", goto_nav = function(panel) nav_select("nav", panel, session = session),
                 switch_cohort = function(schema) {
                   if (!LIVE) return(showNotification("Switching cohort needs a live database connection.", type = "warning"))
                   if (identical(schema, D$meta$schema)) return(nav_select("nav", "Overview", session = session))
                   id <- "cohort_load"
                   showNotification(sprintf("Loading cohort '%s' — this can take up to a minute…", schema),
                                    duration = NULL, id = id, type = "message")
                   ok <- tryCatch(reload_cohort_bundle(schema, force = TRUE), error = function(e) e)
                   removeNotification(id)
                   if (inherits(ok, "error"))
                     return(showNotification(
                       paste0("Cohort '", schema, "' could not be opened: ", conditionMessage(ok)),
                       type = "error", duration = 15))
                   showNotification(sprintf("Loaded cohort '%s'. Refreshing…", schema), duration = 3, type = "message")
                   session$reload()
                 })
  setup_server("st", goto_nav = function(panel) nav_select("nav", panel, session = session))
  pkg_server("pk")
  if (SCI) { cohort_server("co"); sci_server("sci"); bio_server("bio") }
}

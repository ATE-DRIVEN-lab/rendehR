# Shiny cohort explorer shipped with the package (inst/explorer).

.xp_app_packages <- c("shiny", "bslib", "bsicons", "plotly", "DT", "shinyWidgets", "shinycssloaders", "survival")

.xp_check_packages <- function() {
  miss <- .xp_app_packages[!vapply(.xp_app_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss))
    stop("The explorer needs these packages: ", paste(miss, collapse = ", "),
         ". Install them with install.packages(c(", paste0('"', miss, '"', collapse = ", "), ")).",
         call. = FALSE)
  invisible(TRUE)
}

# Environment the app files are sourced into: the exports of the UI packages
# (and the tidyverse verbs the modules use), on top of the rendehR namespace so
# the modules can call the package's internal helpers. Nothing is attached to
# the search path.
.xp_app_env <- function() {
  imports <- new.env(parent = asNamespace("rendehR"))
  for (p in c("stats", "dplyr", "tidyr", "tibble", "stringr", "readr", .xp_app_packages)) {
    for (n in getNamespaceExports(p)) {
      v <- tryCatch(getExportedValue(p, n), error = function(e) NULL)
      if (!is.null(v)) assign(n, v, envir = imports)
    }
  }
  assign("%>%", magrittr::`%>%`, envir = imports)
  new.env(parent = imports)
}

.xp_app_dir <- function() {
  d <- system.file("explorer", package = "rendehR")
  if (!nzchar(d)) stop("The explorer app files were not found in the rendehR installation.", call. = FALSE)
  d
}

#' Build the cohort explorer Shiny app
#'
#' Returns the Shiny app object for a data bundle from [explorer_data()],
#' without starting it. [run_explorer()] builds the bundle and runs the app;
#' use `explorer_app()` to embed or test the app.
#'
#' @param data A bundle from [explorer_data()] (optionally extended with
#'   [explorer_sci_data()]).
#' @param live Whether patient-level queries may reach the database. Defaults
#'   to TRUE when credentials have been stored with [ehr_connect()].
#' @return A `shiny.appobj`.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' app <- explorer_app(explorer_data("rendehr_admission"))
#' shiny::runApp(app)
#' }
explorer_app <- function(data, live = !is.null(.ehr_env$creds)) {
  .xp_check_packages()
  if (!inherits(data, "rendehR_explorer_data"))
    stop("`data` must be a bundle from explorer_data().", call. = FALSE)
  dir <- .xp_app_dir()
  env <- .xp_app_env()
  env$XD <- data
  env$XLIVE <- isTRUE(live)
  files <- c("setup.R", "plots.R", "mod_jobs.R", "mod_overview.R", "mod_explorer.R", "mod_subject.R",
             "mod_concepts.R", "mod_concept_builder.R", "mod_quality.R", "mod_cohorts.R", "mod_setup.R", "mod_package.R")
  if (!is.null(data$sci))
    files <- c(files, file.path("sci", c("sci_setup.R", "mod_cohort.R", "mod_sci.R", "mod_bio.R")))
  for (f in c(files, "app.R")) sys.source(file.path(dir, f), envir = env, keep.source = FALSE)
  shiny::addResourcePath("rendehr-explorer", file.path(dir, "www"))
  app <- shiny::shinyApp(env$ui, env$server, enableBookmarking = "url")
  attr(app, "explorer_env") <- env
  app
}

#' Run the cohort explorer
#'
#' Starts an interactive Shiny explorer for a cohort built with rendehR: an
#' overview, a cohort explorer (a characteristics table with SMDs,
#' distributions, outcomes with Wilson confidence intervals, Kaplan-Meier
#' survival, cross-tabs, scatter plots), a subject explorer that reads each
#' patient's harmonized time series live (timeline, time-series panels,
#' medications, diagnoses and per-patient verification), a concept browser
#' with a concept builder, project setup and data checks.
#' Optional spinal cord injury (SCI) pages are added when `sci` is given.
#'
#' The app serves individual-level data and listens on `127.0.0.1` by default.
#' Do not deploy it publicly.
#'
#' @param schema Merged schema to explore (the `target_schema` of
#'   [merge_sources()]). When NULL, `"rendehr_admission"` (the setup
#'   assistant's default) is used if it exists, otherwise `"rendehr"` (the
#'   default of [merge_sources()]).
#' @param db_name,creds,config_path Passed to [ehr_connect()] when credentials
#'   have not been stored yet (or when `db_name` is given).
#' @param data Optional prebuilt bundle from [explorer_data()]; `schema` is then
#'   ignored.
#' @param sci Optional SCI extension from [explorer_sci_data()].
#' @param raw_dirs Optional raw data directories per source, see
#'   [explorer_data()].
#' @param cache_dir Optional directory where the data bundle is cached between
#'   runs, see [explorer_data()]. Starting from the cache skips reading the
#'   merged tables; it holds individual-level data.
#' @param host,port Where the app listens.
#' @param launch.browser Open a browser window.
#' @return Called for its side effect (runs until stopped); returns the value of
#'   [shiny::runApp()] invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_use_config("config/ehr_config.json")
#' ehr_connect()
#' run_explorer("rendehr_admission")
#' }
run_explorer <- function(schema = NULL, db_name = NULL, creds = NULL, config_path = NULL, data = NULL,
                         sci = NULL, raw_dirs = NULL, cache_dir = NULL, host = "127.0.0.1", port = NULL,
                         launch.browser = interactive()) {
  .xp_check_packages()
  if (is.null(.ehr_env$creds) || !is.null(db_name) || !is.null(creds) || !is.null(config_path))
    ehr_connect(db_name = db_name, creds = creds, config_path = config_path)
  if (is.null(data)) {
    schema <- schema %||% .with_connection(NULL, function(con) {
      if (.xp_table_exists(con, "rendehr_admission", "cohort_info")) "rendehr_admission" else "rendehr"
    })
    message("rendehR explorer: loading '", schema, "' ...")
    data <- explorer_data(schema, raw_dirs = raw_dirs, cache_dir = cache_dir)
  }
  if (!is.null(sci)) data <- explorer_add_sci(data, sci)
  app <- explorer_app(data, live = TRUE)
  invisible(shiny::runApp(app, host = host, port = port %||% getOption("shiny.port"),
                          launch.browser = launch.browser))
}

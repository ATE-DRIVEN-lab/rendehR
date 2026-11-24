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
# Module files that bind cohort tables at source time; re-sourced when the
# navbar cohort picker loads another merged schema.
.xp_cohort_bind_files <- function(has_sci) {
  f <- c("setup.R", "mod_explorer.R", "mod_subject.R", "mod_concepts.R", "mod_cohorts.R",
         "mod_overview.R", "mod_quality.R", "mod_package.R")
  if (isTRUE(has_sci))
    f <- c(f, file.path("sci", c("sci_setup.R", "mod_cohort.R", "mod_sci.R", "mod_bio.R")))
  f
}

explorer_app <- function(data, live = !is.null(.ehr_env$creds)) {
  .xp_check_packages()
  if (!inherits(data, "rendehR_explorer_data"))
    stop("`data` must be a bundle from explorer_data().", call. = FALSE)
  dir <- .xp_app_dir()
  env <- .xp_app_env()
  env$XD <- data
  env$XLIVE <- isTRUE(live)
  env$XRAW_DIRS <- data$meta$raw_dirs
  env$XCACHE_DIR <- NULL
  # SCI extension from the launch bundle; re-applied only while still on that DB.
  env$XSCI <- data$sci
  env$XSCI_DB <- if (!is.null(data$sci)) (.ehr_env$db_name %||% NA_character_) else NA_character_
  env$XDB_NAME <- .ehr_env$db_name %||% NA_character_
  env$XDBS <- unique(c(stats::na.omit(env$XDB_NAME),
                       if (isTRUE(live)) tryCatch(list_explorer_databases(), error = function(e) character())))
  env$XSCHEMAS <- unique(c(data$meta$schema,
                           if (isTRUE(live)) tryCatch(list_explorer_schemas(), error = function(e) character())))
  # Switch the in-memory cohort bundle and rebind module globals (used by app.R).
  env$reload_cohort_bundle <- function(schema, force = FALSE) {
    schema <- as.character(schema)[1]
    same <- identical(schema, env$XD$meta$schema) &&
      identical(as.character(env$XDB_NAME %||% NA), as.character(.ehr_env$db_name %||% NA))
    if (!nzchar(schema) || (!isTRUE(force) && same)) return(invisible(FALSE))
    new <- explorer_data(schema, raw_dirs = env$XRAW_DIRS, cache_dir = env$XCACHE_DIR)
    if (!nrow(new$adm))
      stop("Cohort '", schema, "' has no admissions to show.", call. = FALSE)
    # Re-attach SCI only on the database the extension was built for.
    if (!is.null(env$XSCI) && !is.na(env$XSCI_DB) &&
        identical(as.character(.ehr_env$db_name %||% ""), as.character(env$XSCI_DB))) {
      new <- tryCatch(explorer_add_sci(new, env$XSCI), error = function(e) {
        warning("SCI pages not re-attached: ", conditionMessage(e), call. = FALSE)
        new
      })
    }
    env$XD <- new
    env$XRAW_DIRS <- new$meta$raw_dirs %||% env$XRAW_DIRS
    env$XDB_NAME <- .ehr_env$db_name %||% env$XDB_NAME
    env$XSCHEMAS <- unique(c(schema, tryCatch(list_explorer_schemas(), error = function(e) character())))
    for (f in .xp_cohort_bind_files(!is.null(new$sci)))
      sys.source(file.path(dir, f), envir = env, keep.source = FALSE)
    invisible(TRUE)
  }
  # Point credentials at another Postgres database, then load a cohort there.
  # On failure, reconnect to the previous database so the open session stays usable.
  env$reload_explorer_database <- function(db_name, schema = NULL) {
    db_name <- as.character(db_name)[1]
    if (!nzchar(db_name)) stop("Database name is empty.", call. = FALSE)
    if (is.null(.ehr_env$creds)) stop("No credentials stored; cannot switch database.", call. = FALSE)
    prev <- .ehr_env$db_name
    creds <- .ehr_env$creds
    restore_prev <- function() {
      if (!is.null(prev) && nzchar(prev) && !identical(prev, .ehr_env$db_name))
        tryCatch(ehr_connect(prev, creds = creds), error = function(e) NULL)
    }
    if (!identical(db_name, .ehr_env$db_name)) {
      ok_con <- tryCatch(ehr_connect(db_name, creds = creds), error = function(e) e)
      if (inherits(ok_con, "error"))
        stop("Cannot open database '", db_name, "': ", conditionMessage(ok_con), call. = FALSE)
    }
    schs <- tryCatch(list_explorer_schemas(), error = function(e) character())
    if (!length(schs)) {
      restore_prev()
      stop("Database '", db_name, "' is not suitable for the explorer: ",
           "no admission-level merged cohort (schema with cohort_info + flat). ",
           "Stay on the previous database or pick another from the list.", call. = FALSE)
    }
    pick <- if (!is.null(schema) && schema %in% schs) schema
    else if (!is.null(env$XD$meta$schema) && env$XD$meta$schema %in% schs) env$XD$meta$schema
    else if ("val_merged_adm" %in% schs) "val_merged_adm"
    else if ("rendehr_admission" %in% schs) "rendehr_admission"
    else if ("rendehr" %in% schs) "rendehr"
    else schs[[1]]
    loaded <- tryCatch(env$reload_cohort_bundle(pick, force = TRUE), error = function(e) e)
    if (inherits(loaded, "error")) {
      restore_prev()
      stop("Could not load cohort '", pick, "' from '", db_name, "': ",
           conditionMessage(loaded), " Previous database restored.", call. = FALSE)
    }
    env$XDB_NAME <- db_name
    env$XDBS <- unique(c(db_name, env$XDBS %||% character()))
    env$XSCHEMAS <- schs
    invisible(TRUE)
  }
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

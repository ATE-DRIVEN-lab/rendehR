# Current rendehR adapter. No package installation, pipeline execution, or writes.
.rh_call <- function(name, ...) {
  if (!requireNamespace("rendehR", quietly = TRUE))
    stop("Install the current rendehR package in this R library to use this tool.")
  if (!name %in% getNamespaceExports("rendehR"))
    stop("Installed rendehR does not export required API: ", name)
  do.call(getExportedValue("rendehR", name), list(...))
}
.rh_catalog <- function() {
  root <- Sys.getenv("RENDEHR_SOURCE_PATH")
  path <- if (nzchar(root)) file.path(root, "inst", "extdata", "rendehr_concepts_config.csv") else {
    if (!requireNamespace("rendehR", quietly = TRUE)) stop("Install rendehR or set RENDEHR_SOURCE_PATH for read-only source metadata.")
    system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR")
  }
  if (!file.exists(path)) stop("Current rendehR concept catalogue not found.")
  utils::read.csv(path, colClasses = "character", check.names = FALSE)
}
.rh_db <- function(fn) {
  path <- Sys.getenv("EHR_CONFIG")
  if (!nzchar(path)) stop("Set EHR_CONFIG to your reviewed project configuration before using database tools.")
  check <- .rh_call("ehr_check_config", config_path = path, quiet = TRUE)
  if (!isTRUE(check$ok)) stop("Project configuration has problems; run check_setup.")
  cfg <- .rh_call("ehr_use_config", config_path = path)
  creds <- .rh_call("load_credentials", path = check$paths$credentials_json)
  con <- tryCatch(.rh_call("connect_postgres", db_name = cfg$database$db_name, creds = creds),
                  error = function(e) stop("PostgreSQL connection failed. Review credentials and database availability locally."))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "SET default_transaction_read_only = on")
  DBI::dbExecute(con, "SET statement_timeout = '30s'")
  fn(con)
}
# cohort_summary() prints a report through message() and returns its counts.
.rh_cohort_report <- function(src, con) {
  report <- character()
  value <- withCallingHandlers(
    .rh_call("cohort_summary", src_name = src, con = con),
    message = function(m) {
      report <<- c(report, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  if (!length(report) && is.null(value)) stop("rendehR returned no cohort report.")
  count_fields <- c("icu_windows_rows", "cohort_info_rows", "flat_rows", "ts_rows",
                    "concept_tables")
  if (is.null(value) || !length(intersect(names(value), count_fields)))
    stop("No cohort tables were found for source '", src,
         "' in the configured database.")
  result <- list(source = src, report = paste(report, collapse = ""))
  result$counts <- value
  result
}
.rh_prop <- function(type, ...) c(list(type = type), list(...))
.rh_tool <- function(description, properties, required, fn) list(
  description = description,
  inputSchema = list(type = "object", properties = properties,
                     required = as.list(required), additionalProperties = FALSE),
  annotations = list(readOnlyHint = TRUE, destructiveHint = FALSE, openWorldHint = FALSE), fn = fn)
.rh_tools <- function() {
  str <- .rh_prop("string", minLength = 1L)
  src <- .rh_prop("string", enum = as.list(c("mimic_iii", "mimic_iii_demo", "mimic_iv", "mimic_iv_demo", "eicu", "eicu_demo")))
  limit <- .rh_prop("integer", minimum = 1L, maximum = 100L)
  terms <- list(oneOf = list(str, .rh_prop("array", items = str, minItems = 1L, maxItems = 20L)))
  list(
    rendehr_status = .rh_tool("Check installed rendehR and metadata mode without connecting to a database.", structure(list(), names = character()), character(), function(a) {
      installed <- requireNamespace("rendehR", quietly = TRUE)
      list(mcp_version = .rh_version(), installed = installed, version = if (installed) as.character(utils::packageVersion("rendehR")) else NULL,
           metadata_mode = if (nzchar(Sys.getenv("RENDEHR_SOURCE_PATH"))) "source checkout (may differ from installed package)" else "installed package",
           project_configured = nzchar(Sys.getenv("EHR_CONFIG")), database_tested = FALSE)
    }),
    check_setup = .rh_tool("Check project configuration. Does not prove database connectivity or run setup.", list(config_path = str), "config_path", function(a) {
      x <- .rh_call("ehr_check_config", config_path = a$config_path, quiet = TRUE)
      x[c("ok", "problems", "warnings")]
    }),
    search_concept_definitions = .rh_tool("Search packaged atomic concept definitions by literal name/description. Source coverage is not proof of semantic equivalence; this is not the full SQL or medication-duration catalogue.", list(term = str, src = src, limit = limit, offset = .rh_prop("integer", minimum = 0L)), "term", function(a) {
      d <- .rh_catalog()
      keep <- grepl(tolower(a$term), tolower(d$concept_name), fixed = TRUE) | grepl(tolower(a$term), tolower(d$description), fixed = TRUE)
      if (!is.null(a$src)) keep <- keep & d$source == sub("_demo$", "", a$src)
      d <- d[which(keep), , drop = FALSE]
      n <- nrow(d); offset <- a$offset %||% 0L; count <- a$limit %||% 20L
      list(total = n, offset = offset, rows = utils::head(d[seq_len(n) > offset, , drop = FALSE], count))
    }),
    draft_config_row = .rh_tool("Preview an atomic concept CSV row using one exact existing template. Returns all current CSV columns; writes nothing. Units, bounds, conversion and provenance are cleared for review. No clinical validity is inferred.", list(template_concept = str, template_match_value = str, src = src, event_table = str, concept_name = str, match_value = str, description = str), c("template_concept", "src", "event_table", "concept_name", "match_value", "description"), function(a) {
      d <- .rh_catalog(); family <- sub("_demo$", "", a$src)
      row <- d[which(d$concept_name == a$template_concept & d$source == family & d$table_name == a$event_table), , drop = FALSE]
      if (!is.null(a$template_match_value)) row <- row[which(row$match_value == a$template_match_value), , drop = FALSE]
      if (nrow(row) != 1L) stop("Template must match exactly one catalogue row; inspect search_concept_definitions and supply template_match_value to disambiguate.")
      if (!row$itm_class %in% c("itm_itm", "rgx_itm", "rgx_exp_itm", "col_itm")) stop("Unsupported template item class; manual review required.")
      if (any(d$concept_name == a$concept_name & d$source == family)) stop("Concept already exists for this source; choose a new name or review it manually.")
      if (!grepl("^[a-z][a-z0-9_]*$", a$concept_name)) stop("Use a lowercase SQL-safe concept name.")
      for (field in intersect(c("unit", "min_value", "max_value", "ref", "check", "conversion_note", "notes", "required_dedup", "basos_absolute", "requred_dedup", "basos_asbolute", "dedup_cross_reference"), names(row))) row[[field]] <- NA_character_
      row$conversion <- "none"; row$concept_name <- a$concept_name; row$match_value <- a$match_value; row$description <- a$description
      if (row$itm_class == "col_itm") row$val_var <- a$match_value
      csv <- paste(capture.output(utils::write.table(row, row.names = FALSE, col.names = TRUE, sep = ",", na = "NA", qmethod = "double")), collapse = "\n")
      list(status = "draft_requires_review", csv = csv, inherited_template = a$template_concept,
           review = "Verify source fields, matching syntax, timestamps, units, bounds, conversions and references before registering. No SQL was built or executed.")
    }),
    list_topics = .rh_tool("List locally cached synonym topics. No downloads or AI API calls.", list(limit = limit, offset = .rh_prop("integer", minimum = 0L)), character(), function(a) {
      topics <- sort(names(.rh_synonyms(TRUE))); offset <- a$offset %||% 0L
      list(total = length(topics), topics = as.list(utils::head(topics[seq_along(topics) > offset], a$limit %||% 20L)))
    }),
    resolve_term = .rh_tool("Show local alias expansions before searching. These are candidate search hints, not validated clinical equivalence.", list(term = terms, include_cached_topics = .rh_prop("boolean")), "term", function(a) .rh_expand(a$term, include_cached_topics = a$include_cached_topics %||% FALSE)),
    ehr_find = .rh_tool("Search one or several terms with optional local synonyms across source dictionaries. Includes diagnoses/procedures, item metadata and code versions. No external AI calls. Limit is global; select tables to narrow searches.", list(term = terms, src = src, limit = limit, expand_synonyms = .rh_prop("boolean"), include_cached_topics = .rh_prop("boolean", description = "Include old topic caches that may contain related rather than equivalent concepts. Default false."), tables = .rh_prop("array", items = str, minItems = 1L, maxItems = 6L)), c("term", "src"), function(a) .rh_db(function(con) .rh_find(a, con))),
    list_loaded_concepts = .rh_tool("List concepts present in a configured PostgreSQL source, with bounded output and the full count.", list(src = src, limit = limit), "src", function(a) .rh_db(function(con) {
      d <- .rh_call("list_concepts", src_name = a$src, con = con)
      lim <- a$limit %||% 20L
      list(total = nrow(d), returned = min(nrow(d), lim), rows = utils::head(d, lim))
    })),
    cohort_summary = .rh_tool("Return cohort aggregate counts from the configured PostgreSQL database; no patient records.", list(src = src), "src", function(a) .rh_db(function(con) .rh_cohort_report(a$src, con)))
  )
}

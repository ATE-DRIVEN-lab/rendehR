# Console assistant for adding a new source ----------------------------------
#
# ehr_add_source_assistant() is the "add a new source" counterpart to
# ehr_setup_assistant(): a console-first guide that interviews the user (when
# run interactively) and drives add_source_wizard() to scaffold the source. It
# reuses the setup-assistant primitives (.setup_box/.setup_choice/.setup_value/
# .setup_bool/.setup_number/.setup_call) so look and feel match.

# Split a comma/space-separated string (or pass a vector through) into a clean
# character vector. Interactive prompts return one string; programmatic callers
# may pass a vector directly.
.parse_id_vec <- function(x) {
  if (is.null(x)) return(NULL)
  if (length(x) > 1L) return(as.character(x))
  parts <- trimws(strsplit(as.character(x), "[,[:space:]]+")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) NULL else parts
}

# Render an R value as source text for the generated add_source_wizard() call:
# scalars, character vectors -> c(...), and named lists -> list(a = ..., b = ...).
.render_r <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.logical(x) && length(x) == 1L) return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.numeric(x) && length(x) == 1L)
    return(if (is.integer(x)) paste0(x, "L") else as.character(x))
  if (is.list(x)) {
    nm <- names(x)
    parts <- vapply(seq_along(x), function(i) {
      key <- if (!is.null(nm) && nzchar(nm[[i]])) paste0(nm[[i]], " = ") else ""
      paste0(key, .render_r(x[[i]]))
    }, character(1))
    return(paste0("list(", paste(parts, collapse = ", "), ")"))
  }
  if (is.character(x) && length(x) == 1L) return(.setup_quote(x))
  if (is.character(x)) return(.setup_r_vec(x))
  .setup_quote(as.character(x))
}

#' Interactive assistant for adding a new data source
#'
#' The "add a new source" counterpart to [ehr_setup_assistant()]. Run
#' interactively, it asks a short series of questions (ID columns, time origin,
#' cohort strategy, …), then either scaffolds the source with
#' [add_source_wizard()], prints the equivalent `add_source_wizard()` call for
#' you to run, or validates the assembled spec without writing anything. Every
#' input is also a keyword argument, so the same call works non-interactively in
#' a script.
#'
#' It collects choices and hands them to [add_source_wizard()]; it does not write
#' your source-specific SQL (`window_tables.sql`, schema DDL) or concept-config
#' rows — those are reported as next steps.
#'
#' @param name Source family name (lowercase, no `_demo`).
#' @param root Config directory to scaffold into. Defaults to
#'   `EHR_EXTERNAL_CONFIG_ROOT`.
#' @param select_id_cols ID columns carried into every concept table, coarse →
#'   fine. A vector, or a comma-separated string.
#' @param id_default Join key(s) for non-ICU tables. Defaults to
#'   `select_id_cols`.
#' @param id_icu,tables_with_icu Finer ICU-level join key and the tables that
#'   carry it. Omit for a single granularity.
#' @param time_origin `visit_windows` column used as t=0 (e.g. `"cw.admittime"`).
#' @param time_diff `"timestamp_minutes"` or `"offset_minutes"`.
#' @param cohort_key Patient/stay ID used to subset every table.
#' @param cohort_seed_tables Tables imported first so cohort IDs can be built.
#' @param diag_table,code_col,filter_key,split_commas,strip_dots Fields of the
#'   declarative `cohort_filter` that selects cohorts by ICD code (see
#'   `vignette("adding-a-new-source")`). `filter_key` defaults to `cohort_key`.
#' @param id_prefixes Advanced: source-name prefixes matched by `src_family()`.
#'   Defaults to `name`. Only needed when variant names don't start with `name`.
#' @param icu_id_type Advanced: SQL type of the ICU-level id (`"bigint"` default;
#'   use `"text"` for string stay IDs).
#' @param demo_row_limit Row cap for the `<name>_demo` variant. Default 1000.
#' @param action `"write"` (scaffold + register via `add_source_wizard()`),
#'   `"script"` (print the call, touch nothing), or `"dry_run"` (validate the
#'   spec only). Defaults to `"write"` interactively and to `"script"`
#'   otherwise, so a non-interactive call writes nothing unless asked.
#' @param overwrite Passed to `add_source_wizard()` when `action = "write"`.
#' @param interactive Ask questions on the console? Defaults to `interactive()`.
#' @return Invisibly, an `ehr_add_source_plan`: the collected `spec`, the
#'   rendered `call`, the chosen `action`, any `result` (wizard/validation
#'   output), and `next_steps`.
#' @seealso [add_source_wizard()], [register_source()], [validate_source_config()]
#' @examples
#' \dontrun{
#' ehr_add_source_assistant(name = "my_icu", root = "~/data/my_icu")
#' }
#' @export
ehr_add_source_assistant <- function(name = NULL, root = NULL,
                                     select_id_cols = NULL,
                                     id_default = NULL, id_icu = NULL,
                                     tables_with_icu = NULL,
                                     time_origin = NULL, time_diff = NULL,
                                     cohort_key = NULL, cohort_seed_tables = NULL,
                                     diag_table = NULL, code_col = NULL,
                                     filter_key = NULL, split_commas = NULL,
                                     strip_dots = NULL,
                                     id_prefixes = NULL, icu_id_type = NULL,
                                     demo_row_limit = NULL,
                                     action = NULL, overwrite = FALSE,
                                     interactive = base::interactive()) {
  .setup_box("rendehR add-source assistant", c(
    "I will ask a few questions and then scaffold a new source family.",
    "I collect choices and drive add_source_wizard(); I do not write your",
    "source-specific SQL (window_tables.sql, schema DDL) or concept rows.",
    "Pick action = 'script' to only print the call, or 'dry_run' to validate."
  ), interactive)

  # --- Identity -------------------------------------------------------------
  name <- .setup_value(name, "Source family name (lowercase, no _demo)",
                       interactive = interactive)
  if (is.null(name) || !nzchar(name))
    stop("ehr_add_source_assistant(): a source `name` is required.", call. = FALSE)
  if (grepl("_demo$", name))
    message("Note: register the family ('", sub("_demo$", "", name),
            "'); the _demo variant resolves automatically.")

  root <- .setup_value(root, "Config dir to scaffold into (EHR_EXTERNAL_CONFIG_ROOT)",
                       default = .ehr_config_root_value(),
                       interactive = interactive)
  if (!is.null(root) && !nzchar(root)) root <- NULL

  # --- ID columns -----------------------------------------------------------
  .setup_box("ID columns", c(
    "select_id_cols are carried into every concept table (coarse -> fine).",
    "id_cols$default is the join key for non-ICU tables."
  ), interactive)
  select_id_cols <- .parse_id_vec(.setup_value(
    select_id_cols, "select_id_cols (comma-separated)", interactive = interactive))
  id_default <- .parse_id_vec(.setup_value(
    id_default, "id_cols$default (comma-separated)",
    default = if (!is.null(select_id_cols)) paste(select_id_cols, collapse = ", ") else NULL,
    interactive = interactive))

  has_icu <- .setup_bool(if (is.null(id_icu) && is.null(tables_with_icu)) NULL else TRUE,
                         "Separate ICU-level join key?", default = FALSE,
                         interactive = interactive)
  id_cols <- list(default = id_default)
  if (isTRUE(has_icu)) {
    id_icu <- .parse_id_vec(.setup_value(
      id_icu, "id_cols$icu (comma-separated)",
      default = if (!is.null(id_default)) paste(id_default, collapse = ", ") else NULL,
      interactive = interactive))
    tables_with_icu <- .parse_id_vec(.setup_value(
      tables_with_icu, "tables_with_icu (comma-separated)", interactive = interactive))
    id_cols$icu <- id_icu
    id_cols$tables_with_icu <- tables_with_icu
  }

  # --- Time model -----------------------------------------------------------
  time_origin <- .setup_value(time_origin,
    "time_origin (visit_windows column, e.g. cw.admittime)",
    default = "cw.admittime", interactive = interactive)
  .setup_box("Time difference model", c(
    "timestamp_minutes: index/origin are timestamps; delta is computed.",
    "offset_minutes: index is already an integer minute offset (eICU-style)."
  ), interactive)
  time_diff <- .setup_choice(time_diff, "time_diff",
    c("timestamp_minutes", "offset_minutes"), interactive = interactive)

  # --- Cohort key + seed ----------------------------------------------------
  cohort_key <- .setup_value(cohort_key, "cohort_key (ID used to subset tables)",
    default = if (!is.null(id_default)) id_default[[length(id_default)]] else NULL,
    interactive = interactive)
  cohort_seed_tables <- .parse_id_vec(.setup_value(
    cohort_seed_tables, "cohort_seed_tables (comma-separated)",
    interactive = interactive))

  # --- Cohort selection -----------------------------------------------------
  .setup_box("Cohort selection", c(
    "cohort_filter: filter one diagnosis table by ICD code.",
    "See vignette(\"adding-a-new-source\")."
  ), interactive)

  diag_table <- .setup_value(diag_table,
    "cohort_filter$diag_table (parquet base name)", interactive = interactive)
  code_col <- .setup_value(code_col, "cohort_filter$code_col (ICD code column)",
    interactive = interactive)
  filter_key <- .setup_value(filter_key, "cohort_filter$key",
    default = cohort_key, interactive = interactive)
  split_commas <- .setup_bool(split_commas,
    "split_commas? (several codes joined in one cell)", default = FALSE,
    interactive = interactive)
  strip_dots <- .setup_bool(strip_dots, "strip_dots? (codes carry '.')",
    default = FALSE, interactive = interactive)
  cohort_filter <- list(diag_table = diag_table, code_col = code_col,
                        key = filter_key)
  if (isTRUE(split_commas)) cohort_filter$split_commas <- TRUE
  if (isTRUE(strip_dots))   cohort_filter$strip_dots   <- TRUE

  # Advanced (usually left at defaults; icu_id_type matters for text stay IDs).
  icu_id_type <- .setup_value(icu_id_type, "icu_id_type (bigint or text)",
    default = "bigint", interactive = interactive)
  demo_row_limit <- as.integer(.setup_number(demo_row_limit,
    "demo_row_limit (row cap for <name>_demo)", default = 1000L,
    interactive = interactive))

  # --- Assemble + render ----------------------------------------------------
  # the registry entry add_source_wizard() would write
  spec <- .source_wizard_spec(
    name = name, id_prefixes = id_prefixes, select_id_cols = select_id_cols,
    time_origin = time_origin, time_diff = time_diff,
    icu_id_type = icu_id_type %||% "bigint", id_cols = id_cols,
    cohort_key = cohort_key, cohort_seed_tables = cohort_seed_tables,
    cohort_filter = cohort_filter, demo_row_limit = demo_row_limit)

  call_text <- .setup_call("add_source_wizard", list(
    name               = .render_r(name),
    root               = if (!is.null(root)) .render_r(root) else NULL,
    select_id_cols     = .render_r(select_id_cols),
    id_cols            = .render_r(id_cols),
    time_origin        = .render_r(time_origin),
    time_diff          = .render_r(time_diff),
    cohort_key         = .render_r(cohort_key),
    cohort_seed_tables = if (!is.null(cohort_seed_tables)) .render_r(cohort_seed_tables) else NULL,
    cohort_filter      = if (!is.null(cohort_filter)) .render_r(cohort_filter) else NULL,
    id_prefixes        = if (!is.null(id_prefixes)) .render_r(id_prefixes) else NULL,
    icu_id_type        = if (!identical(icu_id_type, "bigint")) .render_r(icu_id_type) else NULL,
    demo_row_limit     = .render_r(demo_row_limit)
  ))

  action <- .setup_choice(action, "What should I do now?",
    c("write", "script", "dry_run"), default = if (interactive) "write" else "script",
    interactive = interactive)

  .setup_box("Planned add_source_wizard() call",
             strsplit(call_text, "\n", fixed = TRUE)[[1]], interactive)

  # --- Execute --------------------------------------------------------------
  result <- NULL
  if (identical(action, "write")) {
    result <- add_source_wizard(
      name = name, root = root, select_id_cols = select_id_cols,
      time_origin = time_origin, time_diff = time_diff, id_cols = id_cols,
      cohort_key = cohort_key, cohort_seed_tables = cohort_seed_tables,
      cohort_filter = cohort_filter,
      id_prefixes = id_prefixes, icu_id_type = icu_id_type %||% "bigint",
      demo_row_limit = demo_row_limit, overwrite = overwrite, register = TRUE)
    if (isTRUE(result$registered))
      result$validation <- validate_source_config(name = name, quiet = TRUE)
  } else if (identical(action, "dry_run")) {
    result <- list(validation = validate_source_config(config = spec, quiet = TRUE))
  }

  # --- Next steps -----------------------------------------------------------
  next_steps <- c(
    paste0("Fill sql/", name, "/db_setup_sqls/schema/create_tables.sql (raw DDL)."),
    paste0("Fill sql/", name, "/db_setup_sqls/window_tables.sql ",
           "(admission_windows, icu_windows, visit_windows)."),
    paste0("Add concept rows with source = \"", name,
           "\" to rendehr_concepts_config.csv."),
    paste0("Re-run validate_source_config(name = \"", name, "\"), then ",
           "run_one_source(\"", name, "_demo\", run_comorbidities = FALSE, ",
           "run_iss = FALSE, ...) (those stages are hardcoded for built-ins)."))
  if (identical(action, "script"))
    next_steps <- c(paste0("Run the printed call to scaffold '", name, "'."),
                    next_steps)

  out <- list(name = name, root = root, action = action, spec = spec,
              call = call_text, result = result, next_steps = next_steps)
  class(out) <- "ehr_add_source_plan"

  if (isTRUE(interactive)) {
    print(out)
    return(invisible(out))
  }
  out
}

#' Print an `rendehR` add-source assistant plan
#'
#' Displays the target source name, resolved config root, detected cohort
#' logic, the exact `add_source_wizard()` call the assistant would run, and,
#' once executed, whether scaffolding succeeded and how many files were
#' written.
#'
#' @param x An `ehr_add_source_plan` object, as returned by
#'   `ehr_add_source_assistant()`.
#' @param ... Ignored.
#' @return Invisibly, `x`.
#' @export
print.ehr_add_source_plan <- function(x, ...) {
  cat("rendehR Add-Source Assistant Plan\n")
  cat("Source   : ", x$name, "\n", sep = "")
  cat("Config   : ", x$root %||% "(EHR_EXTERNAL_CONFIG_ROOT / add_source_wizard default)",
      "\n", sep = "")
  cat("Cohort   : ",
      if (!is.null(x$spec$cohort_filter)) "declarative (cohort_filter)"
      else "none set", "\n", sep = "")
  cat("Action   : ", x$action, "\n\n", sep = "")

  cat("add_source_wizard() call:\n")
  cat(x$call, "\n\n", sep = "")

  if (identical(x$action, "write") && !is.null(x$result)) {
    cat("Result   : ", if (isTRUE(x$result$registered)) "scaffolded + registered"
        else "scaffolded (not registered - spec incomplete)", "\n", sep = "")
    if (length(x$result$created))
      cat("Files    : ", length(x$result$created), " written\n", sep = "")
  }

  v <- x$result$validation
  if (!is.null(v)) {
    if (length(v$problems))
      for (p in v$problems) cat("  problem: ", p, "\n", sep = "")
    if (length(v$warnings))
      for (w in v$warnings) cat("  warning: ", w, "\n", sep = "")
    if (isTRUE(v$ok) && !length(v$warnings)) cat("Validation: OK\n")
  }

  cat("\nNext steps:\n")
  for (step in x$next_steps) cat("- ", step, "\n", sep = "")
  invisible(x)
}

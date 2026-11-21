# Console setup assistant ----------------------------------------------------

.setup_line <- function(width) paste(rep("-", width), collapse = "")

.setup_wrap <- function(x, width = 74L) {
  unlist(strwrap(x, width = width, simplify = FALSE), use.names = FALSE)
}

.setup_box <- function(title, lines = character(), interactive = base::interactive()) {
  if (!isTRUE(interactive)) return(invisible(NULL))

  lines <- unlist(lapply(lines, .setup_wrap), use.names = FALSE)
  width <- max(nchar(c(title, lines)), 20L) + 4L
  width <- min(max(width, 50L), 82L)
  cat("\n+", .setup_line(width), "+\n", sep = "")
  cat("| ", title, paste(rep(" ", width - nchar(title) - 2L), collapse = ""), "|\n", sep = "")
  cat("+", .setup_line(width), "+\n", sep = "")
  for (line in lines) {
    pad <- paste(rep(" ", width - nchar(line) - 2L), collapse = "")
    cat("| ", line, pad, "|\n", sep = "")
  }
  cat("+", .setup_line(width), "+\n", sep = "")
  invisible(NULL)
}

.setup_choice <- function(value, prompt, choices, default = choices[[1]],
                          interactive = base::interactive()) {
  if (!is.null(value)) {
    hit <- if (length(value) == 1L) choices[pmatch(value, choices)] else NA_character_
    if (is.na(hit))
      stop(prompt, ": ", paste0("'", value, "'", collapse = ", "), " is not available here. ",
           "Valid choices are: ", paste(choices, collapse = ", "), ".", call. = FALSE)
    return(hit)
  }
  if (!isTRUE(interactive)) return(match.arg(default, choices))

  repeat {
    cat("\n", prompt, "\n", sep = "")
    for (i in seq_along(choices)) {
      marker <- if (identical(choices[[i]], default)) " [default]" else ""
      cat(sprintf("  %d. %s%s\n", i, choices[[i]], marker))
    }
    ans <- trimws(readline("Choose number or value: "))
    if (!nzchar(ans)) return(default)
    idx <- suppressWarnings(as.integer(ans))
    if (!is.na(idx) && idx >= 1L && idx <= length(choices)) return(choices[[idx]])
    if (ans %in% choices) return(ans)
    message("Invalid choice: ", ans,
            ". Valid choices are: ", paste(choices, collapse = ", "), ".")
  }
}

.setup_multi_choice <- function(value, prompt, choices, default = choices[[1]],
                                interactive = base::interactive()) {
  validate <- function(x) {
    x <- unique(as.character(x))
    bad <- setdiff(x, choices)
    if (length(bad) > 0L) {
      stop("Unknown choice(s): ", paste(bad, collapse = ", "),
           ". Valid choices are: ", paste(choices, collapse = ", "),
           call. = FALSE)
    }
    x
  }

  if (!is.null(value)) return(validate(value))
  if (!isTRUE(interactive)) return(validate(default))

  repeat {
    cat("\n", prompt, "\n", sep = "")
    for (i in seq_along(choices)) {
      marker <- if (choices[[i]] %in% default) " [default]" else ""
      cat(sprintf("  %d. %s%s\n", i, choices[[i]], marker))
    }
    ans <- trimws(readline("Choose one or more numbers/values, comma-separated, or 'all': "))
    if (!nzchar(ans)) return(validate(default))

    parts <- trimws(strsplit(ans, "[, ]+")[[1]])
    parts <- parts[nzchar(parts)]
    if (any(tolower(parts) == "all")) return(choices)

    idx <- suppressWarnings(as.integer(parts))
    out <- if (all(!is.na(idx)) && all(idx >= 1L) && all(idx <= length(choices))) {
      choices[idx]
    } else {
      parts
    }
    bad <- setdiff(unique(as.character(out)), choices)
    if (length(bad) == 0L) return(validate(out))
    message("Invalid choice(s): ", paste(bad, collapse = ", "),
            ". Valid choices are: ", paste(choices, collapse = ", "), ".")
  }
}

.setup_config_setup <- function(value, interactive = base::interactive(),
                                config_path_hint = file.path(".", "config", "ehr_config.json")) {
  choices <- c("manual", "interactive", "skip")
  if (!is.null(value)) {
    value <- match.arg(value, c(choices, "template"))
    if (identical(value, "template")) return("manual")
    return(value)
  }
  if (!isTRUE(interactive)) return("manual")

  .setup_box("Step 0 - Project config", c(
    paste0("The configuration JSON file will be generated at: ", config_path_hint),
    "If you choose a different project directory later, this path will update with that directory.",
    "Do you want to modify it manually yourself, set it up here interactively, or skip because it already exists?"
  ), interactive)
  .setup_choice(NULL,
                "How do you want to handle project config?",
                choices,
                default = "manual",
                interactive = interactive)
}

.setup_value <- function(value, prompt, default = NULL,
                         interactive = base::interactive()) {
  if (!is.null(value)) return(value)
  if (!isTRUE(interactive)) return(default)
  label <- if (!is.null(default)) paste0(prompt, " [", default, "]: ") else paste0(prompt, ": ")
  ans <- readline(label)
  if (nzchar(ans)) ans else default
}

.setup_bool <- function(value, prompt, default = FALSE,
                        interactive = base::interactive()) {
  if (!is.null(value)) return(isTRUE(value))
  if (!isTRUE(interactive)) return(isTRUE(default))
  ans <- readline(paste0(prompt, " [", if (default) "yes" else "no", "]: "))
  if (!nzchar(ans)) return(isTRUE(default))
  tolower(substr(ans, 1L, 1L)) %in% c("y", "t", "1")
}

.setup_number <- function(value, prompt, default = 0,
                          interactive = base::interactive()) {
  if (!is.null(value)) return(as.numeric(value))
  if (!isTRUE(interactive)) return(as.numeric(default))
  ans <- readline(paste0(prompt, " [", default, "]: "))
  if (!nzchar(ans)) return(as.numeric(default))
  out <- suppressWarnings(as.numeric(ans))
  if (is.na(out)) stop("Expected a numeric value for ", prompt, call. = FALSE)
  out
}

.setup_base_dir_problem <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return("base_dir must be a non-empty path string.")
  }
  if (grepl("[\r\n]", path)) {
    return("base_dir must not contain line breaks.")
  }
  if (!identical(.Platform$OS.type, "windows") && grepl("\\", path, fixed = TRUE)) {
    return("base_dir contains a backslash. On Unix/Linux/macOS, use '/' as the path separator.")
  }

  expanded <- path.expand(path)
  if (file.exists(expanded) && !dir.exists(expanded)) {
    return(paste0("base_dir exists but is not a directory: ", path))
  }

  parent <- expanded
  while (!file.exists(parent) && !dir.exists(parent)) {
    next_parent <- dirname(parent)
    if (identical(next_parent, parent)) break
    parent <- next_parent
  }
  if (!dir.exists(parent)) return(paste0("Path ancestor is not a directory: ", parent))
  if (file.access(parent, 2L) != 0L) return(paste0("Path ancestor is not writable: ", parent))

  NULL
}

.setup_base_dir_value <- function(value, prompt, default = ".",
                                  interactive = base::interactive()) {
  repeat {
    candidate <- .setup_value(value, prompt, default, interactive)
    problem <- .setup_base_dir_problem(candidate)
    if (is.null(problem)) {
      if (isTRUE(interactive) && !dir.exists(path.expand(candidate))) {
        message("Note: base_dir does not exist yet. The generated setup code can create config directories under it: ", candidate)
      }
      return(candidate)
    }

    if (!isTRUE(interactive) || !is.null(value)) {
      stop(problem, call. = FALSE)
    }
    message("Invalid project base directory: ", problem)
    value <- NULL
  }
}

.setup_scope_help <- c(
  icu = "ICU: retain selected ICU stays for focused analysis; excludes broader hospital history.",
  hospital = "Hospital: retain the hospitalization around selected encounters, including sibling ICU stays.",
  patient = "Patient: retain all available encounters for selected patients; uses more storage."
)

.setup_scope_choices <- function(src_names, cohort) {
  if (identical(cohort, "none")) return("patient")
  Reduce(intersect, lapply(src_names, function(src) {
    ladder <- .cohort_id_ladder(src)
    key <- if (identical(cohort, "sepsis3")) {
      .ehr_sepsis3_cohort_spec(src_family(src))$key
    } else if (src_family(src) %in% c("mimic_iii", "mimic_iv")) {
      "hadm_id"
    } else if (identical(src_family(src), "eicu")) {
      "patientunitstayid"
    } else {
      .source_entry(src)$cohort_key
    }
    level <- match(key, ladder)
    if (is.na(level)) stop("Cannot determine cohort selection grain for ", src, call. = FALSE)
    c(if (length(ladder) >= 3L && level == length(ladder)) "icu",
      if (length(ladder) >= 2L && level >= 2L) "hospital", "patient")
  }))
}

.setup_sources <- function() {
  families <- list_registered_sources()
  families <- c(intersect(.ehr_builtin_families, families), setdiff(families, .ehr_builtin_families))
  demos <- vapply(families, function(family) paste0(family, "_demo"), character(1))
  list(demo = demos, full = families)
}

.setup_storage_estimates <- function(src_names, source_size) {
  if (!identical(source_size, "full")) {
    return("Demo sources are small and intended for quick testing.")
  }
  src_map <- c(
    mimic_iv = "MIMIC-IV full: budget 100+ GB for downloaded files, parquet, and Postgres copies.",
    mimic_iii = "MIMIC-III full: budget 50+ GB for downloaded files, parquet, and Postgres copies.",
    eicu = "eICU full: budget 30+ GB for downloaded files, parquet, and Postgres copies."
  )
  c(unname(src_map[intersect(src_names, names(src_map))]),
    "These are conservative planning estimates, not exact PhysioNet file sizes.",
    "Check the current source documentation and your filesystem before a full run.")
}

.setup_check_icd_filter <- function(path, interactive = base::interactive()) {
  if (is.null(path) || !nzchar(path)) return(invisible(NULL))
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("csv", "rds")) {
    warning("Custom ICD filter should be a .csv or .rds file: ", path, call. = FALSE)
    return(invisible(NULL))
  }
  if (!file.exists(path)) {
    warning("Custom ICD filter file does not exist yet: ", path,
            ". The generated script will stop until this file exists.", call. = FALSE)
    return(invisible(NULL))
  }
  cols <- tryCatch({
    if (identical(ext, "rds")) {
      obj <- readRDS(path)
      names(obj)
    } else {
      names(utils::read.csv(path, nrows = 1L, check.names = FALSE))
    }
  }, error = function(e) {
    warning("Could not read ICD filter columns from ", path, ": ", conditionMessage(e), call. = FALSE)
    character(0)
  })
  needed <- c("icd_code", "icd_version")
  missing <- setdiff(needed, cols)
  if (length(missing) > 0L) {
    warning("Custom ICD filter is missing expected column(s): ",
            paste(missing, collapse = ", "), call. = FALSE)
  } else if (isTRUE(interactive)) {
    message("ICD filter check passed: found icd_code and icd_version.")
  }
  invisible(NULL)
}

# R-source-rendering helpers for the generated setup script (.setup_script()).
# R/pipeline.R's .ehr_chr_code()/.ehr_call_code()/.ehr_quote_code() render
# the same kinds of values for a different generator (ehr_suggest_next()'s
# recommended-code output); the two families are intentionally kept
# separate rather than sharing one implementation.
.setup_quote <- function(x) {
  if (is.null(x)) return("NULL")
  encodeString(as.character(x), quote = '"')
}

.setup_r_vec <- function(x) {
  paste0("c(", paste(vapply(x, .setup_quote, character(1)), collapse = ", "), ")")
}

.setup_indent <- function(x, n = 2L) {
  pad <- paste(rep(" ", n), collapse = "")
  lines <- unlist(strsplit(x, "\n", fixed = TRUE), use.names = FALSE)
  lines <- ifelse(nzchar(lines), paste0(pad, lines), "")
  unlist(split(lines, rep(seq_along(x), lengths(strsplit(x, "\n", fixed = TRUE)))),
         use.names = FALSE)
}

.setup_call <- function(fun, args, indent = 2L) {
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) == 0L) return(paste0(fun, "()"))
  pad <- paste(rep(" ", indent), collapse = "")
  body <- paste(sprintf("%s%s = %s", pad, names(args), unlist(args, use.names = FALSE)),
                collapse = ",\n")
  paste0(fun, "(\n", body, "\n)")
}

.setup_comment_block <- function(step, title, lines = character()) {
  header <- paste0("# Step ", step, " - ", title, " ",
                   paste(rep("-", max(1L, 62L - nchar(title))), collapse = ""))
  comments <- if (length(lines) == 0L) character(0) else paste0("# ", lines)
  c("", header, comments)
}

.setup_source_loop <- function(body, choices) {
  if (length(choices$src_names) <= 1L) {
    return(c("src <- default_src", body))
  }
  c("for (src in src_names) {", .setup_indent(body, 2L), "}")
}

.setup_inspect_choices <- c("tables", "cohort", "concepts", "patient_timeline", "descriptive")

.setup_inspect_script <- function(choices) {
  opts <- choices$inspect_options
  if (length(opts) == 0L || identical(opts, "none")) return(character(0))

  lines <- .setup_comment_block("7", "Inspect and analyze outputs", c(
    "These examples use default_src, the first selected source.",
    "They are meant as starting points for exploration after the build finishes."
  ))

  if ("tables" %in% opts) {
    lines <- c(lines,
      "",
      "# 7a. Show database sources and tables by schema.",
      "list_sources()",
      "tables_by_schema <- lapply(c('raw', 'concepts', 'derived', 'harmonized'), function(schema) {",
      "  local({ tables <- list_tables(default_src, schema = schema); data.frame(schema = rep(schema, length(tables)), table = tables) })",
      "})",
      "names(tables_by_schema) <- c('raw', 'concepts', 'derived', 'harmonized')",
      "tables_by_schema")
  }

  if ("cohort" %in% opts) {
    lines <- c(lines,
      "",
      "# 7b. Summarize the cohort and inspect admission/ICU windows.",
      "cohort_summary(default_src)",
      "load_admission_windows(default_src, n = 10)",
      "load_icu_windows(default_src, n = 10)")
  }

  if ("concepts" %in% opts) {
    lines <- c(lines,
      "",
      "# 7c. List/search concepts and summarize one available concept.",
      "concepts <- list_concepts(default_src)",
      "head(concepts, 20)",
      "search_concepts('lactate', src_name = default_src)",
      "concept_to_check <- if (nrow(concepts) > 0) concepts$concept_name[[1]] else NA_character_",
      "if (!is.na(concept_to_check)) concept_summary(default_src, concept_to_check)")
  }

  if ("patient_timeline" %in% opts) {
    level <- choices$analysis_level
    lines <- c(lines,
      "",
      "# 7d. Inspect one patient timeline. Replace the placeholder first.",
      "patient_id <- 'REPLACE_WITH_PATIENT_ID'",
      "if (!identical(patient_id, 'REPLACE_WITH_PATIENT_ID')) {",
      paste0("  patient_ts <- get_patient_ts(default_src, patient_id, analysis_level = '", level, "')"),
      "  head(patient_ts)",
      "  if (requireNamespace('ggplot2', quietly = TRUE) && nrow(patient_ts) > 0) {",
      "    ggplot2::ggplot(patient_ts, ggplot2::aes(time_since_hadm_min, suppressWarnings(as.numeric(value)), color = concept_name)) +",
      "      ggplot2::geom_line(na.rm = TRUE) +",
      "      ggplot2::labs(x = 'Minutes since hospital admission', y = 'Value', color = 'Concept') +",
      "      ggplot2::theme(legend.position = 'none')",
      "  }",
      "} else {",
      "  message('Set patient_id to a real source patient identifier before running the timeline example.')",
      "}")
  }

  if ("descriptive" %in% opts) {
    lines <- c(lines,
      "",
      "# 7e. Simple descriptive summaries from flat_tab and ts_tab.",
      "flat <- fetch_table(default_src, 'flat_tab', schema = 'harmonized', n = Inf)",
      "if ('sex' %in% names(flat)) table(flat$sex, useNA = 'ifany')",
      "if ('race' %in% names(flat)) sort(table(flat$race, useNA = 'ifany'), decreasing = TRUE)",
      "age_col <- intersect(c('age', 'age_hos_admit', 'age_icu_intime'), names(flat))[1]",
      "if (!is.na(age_col)) summary(suppressWarnings(as.numeric(flat[[age_col]])))",
      "mort_col <- intersect(c('hospital_mortality', 'death_in_hospital', 'icu_mortality', 'mortality'), names(flat))[1]",
      "if (!is.na(mort_col)) table(flat[[mort_col]], useNA = 'ifany')",
      "ts_sample <- fetch_table(default_src, 'ts_tab', schema = 'harmonized', n = 100000)",
      "if ('concept_name' %in% names(ts_sample)) sort(table(ts_sample$concept_name), decreasing = TRUE)",
      "if (all(c('concept_name', 'value') %in% names(ts_sample))) {",
      "  ts_sample$value_num <- suppressWarnings(as.numeric(ts_sample$value))",
      "  aggregate(value_num ~ concept_name, data = ts_sample, FUN = function(x) c(n = sum(!is.na(x)), median = median(x, na.rm = TRUE)))",
      "}")
  }

  lines
}

.setup_script <- function(choices) {
  lines <- character(0)

  lines <- c(lines,
    "# Generated by ehr_setup_assistant().",
    "# Review this script and run it block by block in your own R session.",
    "# The assistant printed this code; it did not run downloads or write to the database."
  )

  lines <- c(lines, .setup_comment_block("0", "Load rendehR", c(
    "Use library(rendehR) after installation, or devtools::load_all() while developing locally."
  )))
  if (identical(choices$load_method, "library")) {
    lines <- c(lines, "library(rendehR)")
  } else if (identical(choices$load_method, "load_all")) {
    lines <- c(lines, .setup_call("devtools::load_all", list(path = .setup_quote(choices$package_path))))
  } else {
    lines <- c(lines, "# rendehR is assumed to already be loaded.")
  }

  lines <- c(lines, .setup_comment_block("1", "Set parameters", c(
    "src_names can contain one source or several sources.",
    "Every pipeline call below passes src_name explicitly.",
    "base_dir is the project root (config, SQL, generated files); created here if missing.",
    "data_dir is for the large files (raw downloads, cohort parquet, DuckDB); point it at a large disk.",
    "raw_data_root, cohort_parquet_root, and duckdb_root are stored in the config so every stage uses them."
  )))
  data_dir <- choices$data_dir %||% file.path(choices$base_dir, "data")
  lines <- c(lines,
             paste0("src_names <- ", .setup_r_vec(choices$src_names)),
             "default_src <- src_names[[1]]",
             paste0("db_name <- ", .setup_quote(choices$db_name)),
             paste0("base_dir <- ", .setup_quote(choices$base_dir)),
             paste0("data_dir <- ", .setup_quote(data_dir)),
             paste0("raw_data_root <- ", if (is.null(choices$raw_data_dir)) "file.path(data_dir, 'physionet_data')" else .setup_quote(choices$raw_data_dir)),
             "cohort_parquet_root <- file.path(data_dir, 'cohorts_data')",
             "duckdb_root <- file.path(data_dir, 'duckdb')",
             paste0("import_scope <- ", .setup_quote(choices$import_scope)),
             paste0("# ", .setup_scope_help[[choices$import_scope]]),
             "# Imported context does not automatically become cohort membership.",
             "if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)",
             "if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)")
  if (isTRUE(choices$include_disk_check)) {
    lines <- c(lines,
               "# Optional read-only free-space check (data_dir holds the heavy files).",
               "if (.Platform$OS.type == 'unix') system2('df', c('-h', shQuote(data_dir))) else message('On Windows, check free space with File Explorer or PowerShell Get-PSDrive.')")
  }
  if (identical(choices$cohort, "custom")) {
    lines <- c(lines,
               paste0("icd_filter_path <- ", .setup_quote(choices$icd_filter_path)),
               "if (!file.exists(icd_filter_path)) stop('ICD filter file does not exist: ', icd_filter_path)",
               "icd_ext <- tolower(tools::file_ext(icd_filter_path))",
               "if (!icd_ext %in% c('csv', 'rds')) stop('ICD filter must be .csv or .rds: ', icd_filter_path)",
               "icd_cols <- if (icd_ext == 'rds') names(readRDS(icd_filter_path)) else names(utils::read.csv(icd_filter_path, nrows = 1L, check.names = FALSE))",
               "missing_icd_cols <- setdiff(c('icd_code', 'icd_version'), icd_cols)",
               "if (length(missing_icd_cols) > 0L) stop('ICD filter is missing required column(s): ', paste(missing_icd_cols, collapse = ', '))")
  }
  config_template_call <- .setup_call("ehr_create_config_template", list(
    path = "project_config_path",
    src_name = "default_src",
    db_name = "db_name",
    base_dir = "base_dir",
    raw_data_root = "raw_data_root",
    cohort_parquet_root = "cohort_parquet_root",
    duckdb_root = "duckdb_root",
    credentials_path = .setup_quote(file.path("config", "config_pass.json")),
    cohort_profile = .setup_quote(if (identical(choices$cohort, "scid")) "scid" else "generic"),
    overwrite = "FALSE"
  ))
  config_lines <- c(
    "project_config_path <- file.path(base_dir, 'config', 'ehr_config.json')",
    "credentials_config_path <- file.path(base_dir, 'config', 'config_pass.json')"
  )
  if (!identical(choices$config_setup, "skip")) {
    config_lines <- c(config_lines,
      "if (!file.exists(project_config_path)) {", .setup_indent(config_template_call), "}",
      "if (!file.exists(credentials_config_path)) ehr_create_credentials_template(credentials_config_path)",
      "# Existing configuration and credentials are preserved."
    )
    if (identical(choices$config_setup, "manual")) {
      config_lines <- c(config_lines,
        "# Edit the configuration and credentials before continuing with the next block.")
    }
  }
  expected_profile <- if (identical(choices$cohort, "scid")) "scid" else "generic"
  config_lines <- c(config_lines,
    "cfg <- ehr_load_config(project_config_path)",
    "# Stop before downloads or database stages when saved settings conflict with this plan.",
    "same_path <- function(a, b) identical(normalizePath(path.expand(a), winslash = '/', mustWork = FALSE), normalizePath(path.expand(b), winslash = '/', mustWork = FALSE))",
    "resolve_config_path <- function(p) { if (is.null(p)) return(NULL); if (grepl('^/|^[A-Za-z]:', p) || startsWith(p, '~')) path.expand(p) else file.path(cfg$project$base_dir, p) }",
    paste0("if (!identical(cfg$cohort$profile, ", .setup_quote(expected_profile), ")) stop('Config cohort profile conflicts with this plan; review the config or regenerate the plan.')"),
    "if (!identical(cfg$database$db_name, db_name)) stop('Config database conflicts with this plan.')",
    "if (!identical(cfg$source$src_name, default_src)) stop('Config default source conflicts with this plan.')",
    "if (!same_path(cfg$project$base_dir, base_dir)) stop('Config project directory conflicts with this plan.')",
    "expected_roots <- list(raw_data_root = raw_data_root, cohort_parquet_root = cohort_parquet_root, duckdb_root = duckdb_root)",
    "for (root in names(expected_roots)) {",
    "  actual <- resolve_config_path(cfg$paths[[root]])",
    "  if (is.null(actual) || !same_path(actual, expected_roots[[root]])) stop('Config storage path conflicts with this plan: ', root)",
    "}"
  )
  if (identical(choices$cohort, "custom")) {
    config_lines <- c(config_lines,
      "if (!is.null(cfg$cohort$icd_filter_path) && !same_path(resolve_config_path(cfg$cohort$icd_filter_path), icd_filter_path)) stop('Config ICD filter conflicts with this plan.')")
  } else if (!identical(choices$cohort, "scid")) {
    config_lines <- c(config_lines,
      "if (!is.null(cfg$cohort$icd_filter_path)) stop('This cohort requires a config without an ICD filter.')")
  }
  config_lines <- c(config_lines,
    "ehr_use_config(project_config_path)",
    "for (src in src_names) {",
    "  check <- ehr_check_config(src_name = src, quiet = FALSE)",
    "  if (!isTRUE(check$ok)) stop(paste(check$problems, collapse = '; '))",
    "}")
  lines <- c(lines, .setup_comment_block("1b", "Project config", c(
    "Only missing config/credentials files are created; existing files are never overwritten.",
    paste0("Config setup mode: ", choices$config_setup, ".")
  )), config_lines)

  if (isTRUE(choices$include_download)) {
    dl_lines <- .setup_call("download_source", list(src_name = "src"))
    lines <- c(lines, .setup_comment_block("2", "Download source data", c(
      if (identical(choices$source_size, "demo"))
        "Demo sources are recommended for testing because they are small and should not require PhysioNet credentials."
      else
        "Full sources can be large, slow, and may require PhysioNet credentials.",
      if (length(choices$src_names) > 1L)
        "The generated code loops over all selected sources."
      else
        "Only one source was selected, so the generated code runs directly."
    )))
    lines <- c(lines, .setup_source_loop(dl_lines, choices))
  } else {
    lines <- c(lines, .setup_comment_block("2", "Download source data", c(
      "Skipped in this generated script. Run download_source(src_name) later if data are not present."
    )))
  }

  raw_arg <- "raw_data_root"
  icd_arg <- if (identical(choices$cohort, "custom")) "icd_filter_path" else NULL

  import_call <- if (identical(choices$cohort, "sepsis3")) {
    .setup_call("build_sepsis_cohort", list(
      src_name = "src", base_dir = "base_dir", raw_data_root = raw_arg,
      bundle = .setup_quote(choices$sepsis_bundle), import = "TRUE",
      import_scope = "import_scope", memory_limit = .setup_quote(choices$memory_limit),
      threads = if (is.null(choices$threads)) "NULL" else as.character(choices$threads),
      overwrite = "FALSE"))
  } else {
    .setup_call("import_cohort_general", list(
      src_name = "src", base_dir = "base_dir", raw_data_root = raw_arg,
      icd_filter_path = icd_arg,
      no_filter = if (identical(choices$cohort, "none")) "TRUE" else NULL,
      import_scope = "import_scope", overwrite = "FALSE"))
  }

  build_lines <- "Stage-by-stage is recommended for a first run because each output can be inspected."
  if (length(choices$src_names) > 1L) {
    build_lines <- c(build_lines,
                     "The generated code loops over src_names because multiple sources were selected.")
  } else {
    build_lines <- c(build_lines,
                     "Only one source was selected, so the generated code runs directly without a loop.")
  }
  lines <- c(lines, .setup_comment_block("3", "Build selected source(s)", build_lines))

  if (identical(choices$pipeline, "full")) {
    body <- c(
      if (identical(choices$cohort, "sepsis3")) import_call,
      "# Run the selected stages for this source.",
      "# overwrite = FALSE reuses a complete current cohort artifact",
      "# when available; set TRUE to force a rebuild.",
      .setup_call("run_one_source", list(
        src_name = "src",
        db_name = "db_name",
        base_dir = "base_dir",
        icd_filter_path = icd_arg,
        raw_data_root = raw_arg,
        import_scope = "import_scope",
        no_filter = if (identical(choices$cohort, "none")) "TRUE" else NULL,
        stages = if (identical(choices$cohort, "sepsis3")) .setup_r_vec(c("run_db", "run_raw_mods", "build_cohort_info", if (isTRUE(choices$run_comorbidities)) "run_comorbidities", if (isTRUE(choices$run_iss)) "run_iss", "run_concepts", "run_harmonized_tables")) else NULL,
        run_cohort_annotations = "FALSE",
        run_comorbidities = if (isTRUE(choices$run_comorbidities)) "TRUE" else "FALSE",
        run_iss = if (isTRUE(choices$run_iss)) "TRUE" else "FALSE",
        enrichment = if (identical(choices$merge, "icu")) "enrichment_options(analysis_level = 'icu')" else NULL,
        overwrite = "FALSE"
      ))
    )
    lines <- c(lines, .setup_source_loop(body, choices))
  } else {
    body <- character(0)
    body <- c(body, "# 3a. Import/subset source files to cohort parquet.")
    body <- c(body,
              "# overwrite = FALSE reuses compatible cohort artifacts or reports a conflict;",
              "# set TRUE to force a rebuild.")
    body <- c(body, import_call)

    if (identical(choices$engine, "duckdb")) {
      body <- c(body, "", "# 3b. Load cohort parquet into a local DuckDB file (no Postgres server needed for this step).")
      body <- c(body, "db_path <- file.path(duckdb_root, paste0(src, '.duckdb'))")
      body <- c(body, .setup_call("run_db", list(
        src_name = "src",
        engine = .setup_quote("duckdb"),
        db_path = "db_path",
        base_dir = "base_dir"
      )))

      if (!isTRUE(choices$registered_only)) {
        body <- c(body, "", "# 3c. Apply source-specific raw table modifications (eICU views/renames; no-op for MIMIC).")
        body <- c(body, .setup_call("run_raw_mods_duckdb", list(
          src_name = "src",
          db_path = "db_path",
          base_dir = "base_dir"
        )))

        body <- c(body, "", "# 3e. Build generic cohort_info (SCI defaults are PostgreSQL-only).")
        body <- c(body, .setup_call("build_cohort_info_general_duckdb", list(
          src_name = "src",
          db_path = "db_path",
          base_dir = "base_dir",
          icd_filter_path = icd_arg
        )))
      }
      body <- c(body, "",
                "# This assistant stops its DuckDB plan here. run_concepts(engine = 'duckdb')",
                "# supports compatible concept layers separately. Harmonization, comorbidities,",
                "# ISS, clinical enrichment and merge_sources() still require PostgreSQL.",
                "# To continue in PostgreSQL, generate a postgres plan and start at run_db(); reuse the cohort parquet, do not re-import it.")
    } else {
      body <- c(body, "", "# 3b. Load cohort parquet to Postgres raw schema.")
      body <- c(body, .setup_call("run_db", list(
        src_name = "src",
        db_name = "db_name",
        base_dir = "base_dir"
      )))

      body <- c(body, "", "# 3c. Apply source-specific raw table modifications (eICU views/renames; no-op for MIMIC).")
      body <- c(body, .setup_call("run_raw_mods", list(
        src_name = "src",
        db_name = "db_name",
        base_dir = "base_dir"
      )))

      body <- c(body, "",
                "# 3d. admission_windows/icu_windows/visit_windows were already loaded by",
                "# run_db() above (with cohort_index from the cohort parquet). Do not call",
                "# build_window_tables() here - it would rebuild them from raw admissions/",
                "# icustays with no cohort filtering, overwriting the cohort-aware tables.")

      body <- c(body, "", "# 3e. Build cohort_info from cohort_index; SCI enrichment follows the profile.")
      body <- c(body, .setup_call("build_cohort_info_general", list(
        src_name = "src", db_name = "db_name", base_dir = "base_dir",
        icd_filter_path = icd_arg,
        # an ICU-level merge needs cohort membership resolved per ICU stay
        analysis_level = if (identical(choices$merge, "icu")) .setup_quote("icu") else NULL
      )))

      if (isTRUE(choices$run_comorbidities)) {
        body <- c(body, "", "# 3f. Compute Charlson and Elixhauser comorbidity indices.")
        body <- c(body, "# Requires the comorbidity package; set run_comorbidities = FALSE in ehr_setup_assistant() to omit.")
        body <- c(body, .setup_call("run_comorbidities", list(
          src_name = "src",
          db_name = "db_name",
          base_dir = "base_dir"
        )))
      }

      if (isTRUE(choices$run_iss)) {
        body <- c(body, "", "# 3g. Compute Injury Severity Score (ISS/NISS).")
        body <- c(body, "# Requires icdpicr/icdpicr2; set run_iss = FALSE in ehr_setup_assistant() to omit.")
        body <- c(body, .setup_call("run_iss", list(
          src_name = "src",
          db_name = "db_name",
          base_dir = "base_dir"
        )))
      }

      body <- c(body, "", "# 3h. Build source-specific concept tables.")
      body <- c(body, .setup_call("run_concepts", list(
        src_name = "src",
        db_name = "db_name",
        base_dir = "base_dir"
      )))

      body <- c(body, "", "# 3i. Combine concepts into harmonized ts_tab, flat_tab, and dur_tab.")
      body <- c(body, .setup_call("run_harmonized_tables", list(
        src_name = "src",
        db_name = "db_name",
        base_dir = "base_dir"
      )))
    }

    lines <- c(lines, .setup_source_loop(body, choices))
  }

  if (isTRUE(choices$make_icu_timeline)) {
    body <- .setup_call("make_icu_timeline", list(
      src_name = "src",
      db_name = "db_name",
      base_dir = "base_dir",
      window_before_hours = as.character(choices$window_before_hours),
      window_after_hours = as.character(choices$window_after_hours),
      include_other_icu_events = if (isTRUE(choices$include_other_icu_events)) "TRUE" else "FALSE",
      include_date_only_events = if (isTRUE(choices$include_date_only_events)) "TRUE" else "FALSE",
      materialize = if (isTRUE(choices$materialize_icu)) "TRUE" else "FALSE"
    ))
    lines <- c(lines, .setup_comment_block("4", "Optional ICU timeline projection", c(
      "ts_tab stores each event once at its native grain.",
      "make_icu_timeline() creates an ICU-analysis view/table with assignment metadata."
    )))
    lines <- c(lines, .setup_source_loop(body, choices))
  }

  if (!identical(choices$merge, "none")) {
    lines <- c(lines, .setup_comment_block("5", "Optional merge", c(
      "Use analysis_level = 'admission' for native ts_tab, or 'icu' for ts_icu.",
      "When multiple sources are selected, merge_sources() receives all selected src_names."
    )))
    lines <- c(lines, .setup_call("merge_sources", list(
      src_names = "src_names",
      db_name = "db_name",
      base_dir = "base_dir",
      target_schema = .setup_quote(choices$target_schema),
      analysis_level = .setup_quote(choices$merge)
    )))
  }

  needs_inspect_connection <- !identical(choices$inspect_options, "none") ||
    !identical(choices$merge, "none")
  if (isTRUE(needs_inspect_connection)) {
    lines <- c(lines, .setup_comment_block("6", "Connect for inspection", c(
      "Post-build inspection helpers open database queries using stored credentials.",
      "Run this after editing config/config_pass.json and after the database has been created."
    )))
    lines <- c(lines, .setup_call("ehr_connect", list(db_name = "db_name")))
  }

  lines <- c(lines, .setup_inspect_script(choices))
  if (!identical(choices$merge, "none")) {
    lines <- c(lines,
               "",
               "# 7f. Inspect merged time-series output.",
               .setup_call("load_merged_ts", list(
                 schema = .setup_quote(choices$target_schema),
                 n = "100"
               )))
  }

  paste(lines, collapse = "\n")
}

#' Generate a guided rendehR setup script
#'
#' `ehr_setup_assistant()` is a console-first guide. It explains the main setup
#' decisions, asks for choices when run interactively, and returns a runnable R
#' script. It does not download data, write schemas, edit credentials, or run the
#' pipeline itself. The generated script is the artifact to review, save, and run
#' block by block.
#'
#' @param src_name Optional source name or character vector of source names.
#'   When NULL, the assistant asks first whether to use demo or full sources,
#'   then lets the user choose one or more sources.
#' @param source_size Source size mode: `"demo"` or `"full"`. Demo is
#'   recommended for first tests because it is small and should not require
#'   PhysioNet credentials.
#' @param engine Database backend for the generated script: `"postgres"`
#'   (default) runs the complete pipeline, or `"duckdb"` for cohort
#'   selection and raw loading only (no Postgres server needed for that
#'   part). Compatible concept layers support DuckDB separately; this assistant
#'   does not generate them. Harmonization, comorbidities, ISS, clinical/
#'   SCI annotations, and `merge_sources()` all require PostgreSQL and are
#'   omitted from the generated script when `engine = "duckdb"`;
#'   `pipeline` is forced to `"stages"` (`run_one_source()` still uses
#'   PostgreSQL for loading and downstream stages), and `run_comorbidities`/`run_iss`/`merge`/`inspect_options`
#'   are forced off since they all require PostgreSQL.
#' @param db_name Database name stored in project config. DuckDB loading
#'   does not open a PostgreSQL connection.
#' @param base_dir Project root directory to place in generated code. Holds
#'   config, SQL, and generated files.
#' @param data_dir Directory for large data: raw PhysioNet downloads and cohort
#'   parquet outputs. When NULL, the assistant asks interactively and defaults
#'   to `file.path(base_dir, "data")`. Raw downloads go to its
#'   `physionet_data` subfolder and cohort parquet to its `cohorts_data`
#'   subfolder. Point this at a large disk to keep the heavy files off the
#'   project directory.
#' @param config_setup Project config handling mode: `"manual"` creates
#'   missing templates for the user to edit, `"interactive"` creates missing
#'   config from assistant answers, and `"skip"` requires existing config.
#'   Existing files are preserved; generated code stops on conflicting settings. The old
#'   value `"template"` is accepted as an alias for `"manual"`.
#' @param include_disk_check Include a read-only disk-space check snippet in
#'   the generated script? Mainly useful for full-data runs.
#' @param cohort Cohort mode: `"scid"`, `"sepsis3"`, `"custom"`, or `"none"`.
#' @param icd_filter_path Path to a custom ICD filter file when
#'   `cohort = "custom"`.
#' @param pipeline Pipeline style: `"stages"` emits stage-by-stage calls;
#'   `"full"` emits `run_one_source()` when possible.
#' @param include_download Include a `download_source()` block in the script?
#' @param raw_data_dir Optional parent raw-data storage root (source directories
#'   live underneath it). Used consistently for downloads, config and import.
#' @param run_comorbidities Include the Charlson/Elixhauser comorbidity stage in
#'   the generated build script? Defaults to TRUE for SCI, FALSE otherwise.
#' @param run_iss Include the Injury Severity Score (ISS/NISS) stage in the
#'   generated build script? Defaults to TRUE for SCI, FALSE otherwise.
#' @param analysis_level Desired unit of analysis: `"admission"` for hospital
#'   admission/native-grain analysis, or `"icu"` for ICU-stay analysis. This
#'   matches the `analysis_level` values used across the package (e.g.
#'   `merge_sources()`, `get_patient_ts()`). `"hadm"` is accepted as a
#'   deprecated alias for `"admission"`.
#' @param make_icu_timeline Deprecated/compatibility shortcut. If supplied and
#'   `analysis_level` is NULL, TRUE maps to `analysis_level = "icu"` and FALSE
#'   maps to `analysis_level = "admission"`.
#' @param window_before_hours Hours before ICU admission to include in the ICU
#'   projection block.
#' @param window_after_hours Hours after ICU discharge to include in the ICU
#'   projection block.
#' @param include_other_icu_events Include ICU-native events from other ICU stays
#'   when they fall inside the requested ICU window?
#' @param include_date_only_events Include date-only events in the ICU projection?
#' @param materialize_icu If TRUE, generate a table for `ts_icu`; if FALSE,
#'   generate a view.
#' @param merge Merge mode: `"none"`, `"admission"`, or `"icu"`.
#' @param target_schema Target schema for the optional merge block.
#' @param inspect_options Character vector of post-build inspect/analyze snippets
#'   to include. Choices are `"tables"`, `"cohort"`, `"concepts"`,
#'   `"patient_timeline"`, `"descriptive"`, and `"none"`.
#' @param load_method How the generated script should load rendehR: `"library"`,
#'   `"load_all"`, or `"none"`.
#' @param package_path Package path used when `load_method = "load_all"`.
#' @param save_script Save the generated script to disk? When NULL and
#'   interactive, the assistant asks at the end. When non-interactive, NULL is
#'   treated as FALSE.
#' @param setup_script_path Path where the generated script should be saved.
#'   Defaults to `file.path(base_dir, "ehr_setup_script.R")` when saving.
#' @param overwrite_setup_script Overwrite an existing saved script? Default
#'   FALSE.
#' @param interactive If TRUE, print the guide and prompt for missing values. If
#'   FALSE, use defaults and return the generated plan silently.
#' @param import_scope Surrounding data to retain: `"patient"`, `"hospital"`,
#'   or `"icu"`. Choices cannot be finer than any selected source's cohort key.
#'   Defaults to hospital for Sepsis-3, patient otherwise. Unfiltered mode only
#'   accepts patient. Context encounters do not automatically become members.
#' @param sepsis_bundle Sepsis-3 variant: `"sepsis3"`, `"first_sepsis3"`,
#'   `"sepsis3_admission"`, or `"first_sepsis3_admission"`. Only for sepsis3.
#' @param memory_limit Optional DuckDB memory size, e.g. `"4GB"`, for sepsis selection/import.
#' @param threads Optional positive integer worker count for sepsis selection/import.
#' @return An object of class `ehr_setup_plan` with `choices`, `script`, and
#'   `next_steps` fields.
#' @export
#'
#' @examples
#' project <- file.path(tempdir(), "rendehr-project")
#' plan <- ehr_setup_assistant(base_dir = project, interactive = FALSE)
#' cat(plan$script)
#' sepsis <- ehr_setup_assistant(base_dir = project, cohort = "sepsis3",
#'   import_scope = "hospital", sepsis_bundle = "first_sepsis3_admission",
#'   interactive = FALSE)
#' sci <- ehr_setup_assistant(base_dir = project, cohort = "scid",
#'   import_scope = "hospital", interactive = FALSE)
ehr_setup_assistant <- function(src_name = NULL,
                                source_size = NULL,
                                engine = NULL,
                                db_name = NULL,
                                base_dir = NULL,
                                data_dir = NULL,
                                config_setup = NULL,
                                include_disk_check = NULL,
                                cohort = NULL,
                                icd_filter_path = NULL,
                                pipeline = NULL,
                                include_download = NULL,
                                raw_data_dir = NULL,
                                run_comorbidities = NULL,
                                run_iss = NULL,
                                analysis_level = NULL,
                                make_icu_timeline = NULL,
                                window_before_hours = NULL,
                                window_after_hours = NULL,
                                include_other_icu_events = NULL,
                                include_date_only_events = NULL,
                                materialize_icu = NULL,
                                merge = NULL,
                                target_schema = NULL,
                                inspect_options = NULL,
                                load_method = NULL,
                                package_path = getwd(),
                                save_script = NULL,
                                setup_script_path = NULL,
                                overwrite_setup_script = FALSE,
                                interactive = base::interactive(),
                                import_scope = NULL,
                                sepsis_bundle = NULL,
                                memory_limit = NULL,
                                threads = NULL) {
  sources <- .setup_sources()
  demo_sources <- sources$demo
  full_sources <- sources$full

  .setup_box("rendehR setup assistant", c(
    "I will ask a few questions and then print a script you can run.",
    "The assistant only collects choices; it does not write config or credentials files, download data, create schemas, or run the pipeline.",
    "The generated script is the single place that creates config/credentials and runs everything.",
    "The safest workflow is to run the generated script block by block."
  ), interactive)

  config_setup <- .setup_config_setup(
    config_setup,
    interactive = interactive,
    config_path_hint = file.path(base_dir %||% ".", "config", "ehr_config.json")
  )

  .setup_box("Step 1 - Load method", c(
    "Choose how the generated script should load rendehR.",
    "Use load_all while developing locally; use library after installation."
  ), interactive)
  load_method <- .setup_choice(load_method, "How should the generated script load rendehR?",
                               c("load_all", "library", "none"),
                               default = "library", interactive = interactive)

  .setup_box("Step 2 - Demo or full data", c(
    "For a first test, demo is recommended: it is small, fast, and should not require a PhysioNet password.",
    "Full data can take a long time, use much more disk space, and may require credentialed access.",
    "After choosing demo or full, you can select one or multiple sources."
  ), interactive)
  if (is.null(src_name)) {
    source_size <- .setup_choice(source_size, "Would you like to use demo or full data?",
                                 c("demo", "full"), default = "demo",
                                 interactive = interactive)
    source_choices <- if (identical(source_size, "demo")) demo_sources else full_sources
    src_names <- .setup_multi_choice(NULL,
                                     "Which source(s) do you want to include?",
                                     source_choices,
                                     default = source_choices[[1]],
                                     interactive = interactive)
  } else {
    src_names <- unique(as.character(src_name))
    if (!length(src_names) || anyNA(src_names) || any(!nzchar(src_names))) stop("Select at least one source.", call. = FALSE)
    allowed <- c(demo_sources, full_sources)
    bad <- setdiff(src_names, allowed)
    if (length(bad) > 0L) {
      stop("Unknown source name(s): ", paste(bad, collapse = ", "),
           ". Valid choices are: ", paste(allowed, collapse = ", "),
           call. = FALSE)
    }
    if (is.null(source_size)) {
      if (all(src_names %in% demo_sources)) {
        source_size <- "demo"
      } else if (all(src_names %in% full_sources)) {
        source_size <- "full"
      } else {
        stop("Do not mix demo and full source names in one setup plan. ",
             "Choose demo sources or full sources.", call. = FALSE)
      }
    } else {
      source_size <- match.arg(source_size, c("demo", "full"))
      expected_sources <- if (identical(source_size, "demo")) demo_sources else full_sources
      if (any(!src_names %in% expected_sources)) {
        stop("source_size = '", source_size,
             "' does not match source name(s): ", paste(src_names, collapse = ", "),
             call. = FALSE)
      }
    }
  }

  .setup_box("Step 2b - Database backend", c(
    "postgres runs the complete pipeline: cohort loading, concept extraction, harmonization, and clinical annotations.",
    "This assistant's DuckDB plan covers cohort loading and generic cohort_info. run_concepts() separately supports compatible DuckDB layers; harmonization, clinical enrichment and merging still require PostgreSQL.",
    "Choose duckdb only if you specifically want a lightweight, Postgres-free cohort-loading step; choose postgres for the full pipeline."
  ), interactive)
  engine <- .setup_choice(engine, "Which database backend should the generated script target?",
                          c("postgres", "duckdb"), default = "postgres", interactive = interactive)
  is_duckdb <- identical(engine, "duckdb")

  project_lines <- c(
    "Choose where the project config, credentials template, SQL, and derived files should live.",
    "Next you also choose a data directory for the large files: raw PhysioNet downloads and cohort parquet outputs.",
    "Point the data directory at a large disk to keep heavy files off the project directory.",
    "The database name is stored in the project config alongside these paths.",
    "When multiple sources are selected, the generated config stores the first source as the default and passes src_name explicitly everywhere else.",
    .setup_storage_estimates(src_names, source_size)
  )
  .setup_box("Step 3 - Project location", project_lines, interactive)
  base_dir <- .setup_base_dir_value(base_dir, "Project base directory", ".", interactive)
  data_dir <- .setup_base_dir_value(
    data_dir,
    "Data directory for large raw downloads + cohort parquet",
    file.path(base_dir, "data"),
    interactive
  )
  db_name <- .setup_value(db_name, "Postgres database name", "rendehr", interactive)
  if (identical(source_size, "full")) {
    include_disk_check <- .setup_bool(include_disk_check,
                                      "Include a read-only disk-space check snippet?",
                                      default = TRUE,
                                      interactive = interactive)
  } else {
    include_disk_check <- isTRUE(include_disk_check)
  }

  .setup_box("Step 4 - Cohort", c(
    "SCI: select encounters using the bundled spinal cord injury ICD definition.",
    "Sepsis-3: select ICU stays using the packaged clinical SQL definition, not an ICD list.",
    "Custom: select encounters using your own ICD filter file.",
    "None: import the available population without filtering."
  ), interactive)
  cohort <- .setup_choice(cohort, "Cohort mode", c("scid", "sepsis3", "custom", "none"),
                          default = "scid", interactive = interactive)
  if (identical(cohort, "custom")) {
    icd_filter_path <- .setup_value(icd_filter_path, "Path to ICD filter file",
                                    "config/my_icd_filter.csv", interactive)
    .setup_check_icd_filter(icd_filter_path, interactive = interactive)
  }

  families <- vapply(src_names, src_family, character(1))
  registered_only <- any(!families %in% .ehr_builtin_families)
  if (registered_only && (!is_duckdb || !identical(cohort, "none"))) {
    stop("Registered families are available for DuckDB unfiltered raw loading in this assistant. ",
         "Their clinical SQL/enrichment support must be configured separately; use engine = 'duckdb', cohort = 'none'.", call. = FALSE)
  }
  if (identical(cohort, "sepsis3")) {
    .setup_box("Sepsis-3 definition", c(
      "sepsis3: all qualifying ICU stays; suspicion can occur anywhere in the stay.",
      "first_sepsis3: restrict to a patient's first-ever ICU stay, if that stay qualifies.",
      "sepsis3_admission: qualifying suspicion falls within +/-1 day of ICU admission.",
      "first_sepsis3_admission: apply both first-stay and admission-timing restrictions.",
      "These use the packaged source-specific SQL adaptations. eICU first-stay ordering across hospitalizations is approximate."
    ), interactive)
    sepsis_bundle <- .setup_choice(sepsis_bundle, "Sepsis-3 variant",
      c("sepsis3", "first_sepsis3", "sepsis3_admission", "first_sepsis3_admission"),
      default = "sepsis3", interactive = interactive)
    for (src in src_names) .ehr_sepsis3_cohort_spec(src_family(src), sepsis_bundle)
    .setup_box("Sepsis selection resources", c(
      "Sepsis selection scans raw clinical tables before subsetting; full sources can require substantial memory and temporary disk space.",
      "memory_limit bounds DuckDB memory (for example 4GB); threads controls parallel workers. Blank uses the package default."
    ), interactive)
    memory_limit <- .setup_value(memory_limit, "DuckDB memory limit", NULL, interactive)
    threads <- .setup_value(threads, "DuckDB worker threads", NULL, interactive)
  } else if (!is.null(sepsis_bundle)) {
    stop("sepsis_bundle requires cohort = 'sepsis3'.", call. = FALSE)
  }
  if (!is.null(memory_limit) && (!is.character(memory_limit) || length(memory_limit) != 1L ||
      is.na(memory_limit) || !grepl("^[0-9]+([.][0-9]+)?[ ]*(KB|MB|GB|TB)$", memory_limit, ignore.case = TRUE))) {
    stop("memory_limit must be a size such as '4GB'.", call. = FALSE)
  }
  if (!is.null(threads)) {
    threads <- suppressWarnings(as.numeric(threads))
    if (length(threads) != 1L || !is.finite(threads) || threads < 1 || threads != floor(threads))
      stop("threads must be a positive integer.", call. = FALSE)
  }
  scope_choices <- .setup_scope_choices(src_names, cohort)
  .setup_box("Import scope - how much surrounding data?", c(
    unname(.setup_scope_help),
    "Extra encounters provide context; they do not automatically become cohort members.",
    "Only scopes supported by every selected source's selection grain are offered. MIMIC ICD cohorts select admissions, so ICU-only import is unavailable.",
    if (identical(cohort, "none")) "Unfiltered import retains the available population; scope cannot narrow it."
  ), interactive)
  import_scope <- .setup_choice(import_scope, "Import scope", scope_choices,
    default = if (identical(cohort, "sepsis3") && "hospital" %in% scope_choices) "hospital" else "patient",
    interactive = interactive)
  # Freeze paths as absolute values so generated configuration resolves them once.
  base_dir <- normalizePath(path.expand(base_dir), winslash = "/", mustWork = FALSE)
  if (!.ehr_is_abs_path(base_dir)) base_dir <- file.path(getwd(), base_dir)
  data_dir <- normalizePath(path.expand(data_dir), winslash = "/", mustWork = FALSE)
  if (!.ehr_is_abs_path(data_dir)) data_dir <- file.path(getwd(), data_dir)
  if (!is.null(raw_data_dir)) {
    raw_data_dir <- .setup_base_dir_value(raw_data_dir, "Raw data storage root", interactive = FALSE)
    if (!.ehr_is_abs_path(raw_data_dir)) raw_data_dir <- file.path(getwd(), raw_data_dir)
    raw_data_dir <- path.expand(raw_data_dir)
  }
  if (!is.null(icd_filter_path)) {
    icd_filter_path <- path.expand(icd_filter_path)
    if (!.ehr_is_abs_path(icd_filter_path)) icd_filter_path <- file.path(getwd(), icd_filter_path)
  }

  if (!identical(cohort, "sepsis3") && (!is.null(memory_limit) || !is.null(threads))) {
    stop("memory_limit and threads in this assistant apply to cohort = 'sepsis3'.", call. = FALSE)
  }

  .setup_box("Step 5 - Project config", c(
    paste0("Config setup mode: ", config_setup, "."),
    "This choice is recorded; the generated script creates and loads the config accordingly."
  ), interactive)

  if (isTRUE(interactive)) {
    # Option 1: the assistant only collects and validates choices. The generated
    # script is the single owner that creates config/credentials and calls
    # ehr_use_config(); no files are written here.
    message("rendehR: no files written by the assistant. The generated script creates/loads config and credentials.")
  }


  .setup_box("Step 6 - Build style", c(
    "Stage-by-stage is the recommended first run because you can inspect each output.",
    "Full shortcut emits run_one_source(); it is better after you know the setup works.",
    "Comorbidities summarize chronic disease burden using Charlson/Elixhauser indices.",
    "ISS/NISS summarizes injury severity; generally useful for injury cohorts, not routine sepsis analysis.",
    "Both default on for SCI and off for other cohorts; you can override either choice."
  ), interactive)
  if (isTRUE(is_duckdb)) {
    pipeline <- "stages"
    if (isTRUE(interactive)) {
      message("rendehR: this assistant's DuckDB plan covers cohort loading and generic cohort_info as direct stages; ",
              "run_concepts() separately supports compatible DuckDB layers, while run_one_source() still uses PostgreSQL for database loading, harmonization, clinical enrichment and merging.")
    }
  } else {
    pipeline <- .setup_choice(pipeline, "Pipeline style", c("stages", "full"),
                              default = "stages", interactive = interactive)
  }
  if (isTRUE(is_duckdb)) {
    run_comorbidities <- FALSE
    run_iss <- FALSE
    if (isTRUE(interactive)) {
      message("rendehR: comorbidity and ISS stages require PostgreSQL and are skipped for engine = 'duckdb'.")
    }
  } else {
    run_comorbidities <- .setup_bool(run_comorbidities,
                                     "Include comorbidity indices (Charlson/Elixhauser)?",
                                     default = identical(cohort, "scid"),
                                     interactive = interactive)
    run_iss <- .setup_bool(run_iss,
                           "Include Injury Severity Score (ISS/NISS)?",
                           default = identical(cohort, "scid"),
                           interactive = interactive)
  }

  .setup_box("Step 7 - Download block", c(
    "The assistant can include a download_source() block, but the generated code will not run it for you.",
    if (identical(source_size, "demo"))
      "For demo sources, this is usually small and suitable for a first test."
    else
      "For full sources, review credentials, disk space, and time before running the download block."
  ), interactive)
  include_download <- .setup_bool(include_download, "Include download_source() block?",
                                  default = FALSE, interactive = interactive)

  if (registered_only && isTRUE(include_download)) {
    stop("Registered-source downloads are not configured by this assistant; provide local raw data and set include_download = FALSE.", call. = FALSE)
  }

  if (isTRUE(is_duckdb)) {
    analysis_level <- "admission"
    make_icu_timeline <- FALSE
    window_before_hours <- window_before_hours %||% 0
    window_after_hours <- window_after_hours %||% 0
    include_other_icu_events <- isTRUE(include_other_icu_events)
    include_date_only_events <- isTRUE(include_date_only_events)
    materialize_icu <- isTRUE(materialize_icu)
    if (isTRUE(interactive)) {
      message("rendehR: make_icu_timeline() requires PostgreSQL and is skipped for engine = 'duckdb' (analysis_level forced to 'admission').")
    }
  } else {
    .setup_box("Step 8 - Unit of analysis", c(
      "Choose admission for hospital-admission/native-grain analysis: ts_tab stores each event once.",
      "Choose icu for ICU-stay analysis: the generated script will derive ts_icu with assignment metadata.",
      "You can still merge at either admission or ICU level later."
    ), interactive)
    if (is.null(analysis_level)) {
      if (!is.null(make_icu_timeline)) {
        analysis_level <- if (isTRUE(make_icu_timeline)) "icu" else "admission"
      } else {
        analysis_level <- .setup_choice(analysis_level,
                                        "What is your desired unit of analysis?",
                                        c("admission", "icu"),
                                        default = "admission",
                                        interactive = interactive)
      }
    } else {
      # "hadm" is accepted as a deprecated alias for "admission".
      if (identical(analysis_level, "hadm")) analysis_level <- "admission"
      analysis_level <- match.arg(analysis_level, c("admission", "icu"))
    }
    make_icu_timeline <- identical(analysis_level, "icu")
    .setup_box("Merge outputs", c(
      "None: keep each source's outputs separate.",
      "Admission: combine source outputs at admission level.",
      "ICU: combine ICU projections; this adds timeline preparation even with admission analysis."
    ), interactive)
    merge <- .setup_choice(merge, "Merge outputs?", c("none", "admission", "icu"),
      default = "none", interactive = interactive)
    make_icu_timeline <- make_icu_timeline || identical(merge, "icu")
    if (isTRUE(make_icu_timeline)) {
      .setup_box("ICU timeline settings", c(
        "Before/after hours extends the ICU observation window beyond ICU entry/discharge.",
        "Other ICU events includes events linked to another stay when inside that window.",
        "Date-only events includes events without an exact time; their assignment is less precise.",
        "Materialize stores a table for repeated queries; a view computes the projection when queried."
      ), interactive)
      window_before_hours <- .setup_number(window_before_hours,
                                           "Hours before ICU to include",
                                           default = 0,
                                           interactive = interactive)
      window_after_hours <- .setup_number(window_after_hours,
                                          "Hours after ICU to include",
                                          default = 0,
                                          interactive = interactive)
      include_other_icu_events <- .setup_bool(include_other_icu_events,
                                              "Include events from other ICU stays if inside the window?",
                                              default = FALSE,
                                              interactive = interactive)
      include_date_only_events <- .setup_bool(include_date_only_events,
                                              "Include date-only events in ICU projection?",
                                              default = FALSE,
                                              interactive = interactive)
      materialize_icu <- .setup_bool(materialize_icu,
                                     "Materialize ts_icu as a table instead of a view?",
                                     default = FALSE,
                                     interactive = interactive)
    } else {
      window_before_hours <- window_before_hours %||% 0
      window_after_hours <- window_after_hours %||% 0
      include_other_icu_events <- isTRUE(include_other_icu_events)
      include_date_only_events <- isTRUE(include_date_only_events)
      materialize_icu <- isTRUE(materialize_icu)
    }
  }

  if (any(!is.finite(c(window_before_hours, window_after_hours))) ||
      any(c(window_before_hours, window_after_hours) < 0)) {
    stop("ICU window hours must be finite and non-negative.", call. = FALSE)
  }

  if (isTRUE(is_duckdb)) {
    merge <- "none"
    default_schema <- "rendehr_admission"
    target_schema <- target_schema %||% default_schema
    if (isTRUE(interactive)) {
      message("rendehR: merge_sources() requires PostgreSQL and is skipped for engine = 'duckdb'.")
    }
  } else {
    .setup_box("Step 9 - Merge", c(
      "Merge is optional. Admission merge uses native ts_tab.",
      "ICU merge uses ts_icu, so it requires the ICU timeline block first.",
      "Multiple selected sources can be merged together at this step."
    ), interactive)
    merge <- .setup_choice(merge, "Merge outputs?", c("none", "admission", "icu"),
                           default = "none", interactive = interactive)
    default_schema <- if (identical(merge, "icu")) "rendehr_icu" else "rendehr_admission"
    if (!identical(merge, "none")) {
      target_schema <- .setup_value(target_schema, "Target merged schema", default_schema, interactive)
    } else {
      target_schema <- target_schema %||% default_schema
    }
  }

  if (isTRUE(is_duckdb)) {
    inspect_options <- "none"
    if (isTRUE(interactive)) {
      message("rendehR: inspect/analyze snippets connect through PostgreSQL (ehr_connect()/fetch_*) and are skipped for engine = 'duckdb'.")
    }
  } else {
    .setup_box("Step 10 - Inspect and analyze", c(
      "After the build, many users want quick checks and small analysis examples.",
      "The assistant can add snippets for tables, cohort summaries, concepts, one patient timeline, and descriptive summaries.",
      "Tables: list available database tables. Cohort: summarize membership and encounter windows.",
      "Concepts: find available measurements. Patient timeline: inspect one patient after supplying an ID.",
      "Descriptive: summarize demographics and sample measurements; the flat table is loaded into memory.",
      "These snippets are optional and do not run inside the assistant."
    ), interactive)
    inspect_choices <- c(.setup_inspect_choices, "none")
    inspect_options <- .setup_multi_choice(inspect_options,
                                           "Which inspect/analyze snippets should be included?",
                                           inspect_choices,
                                           default = .setup_inspect_choices,
                                           interactive = interactive)
  }
  if ("none" %in% inspect_options) inspect_options <- "none"

  .setup_box("Step 11 - Save script", c(
    "The generated setup script can be saved as an R file now.",
    "Saving is optional; the plan object still contains the script either way."
  ), interactive)
  save_script <- .setup_bool(save_script, "Save generated setup script to disk?",
                             default = FALSE, interactive = interactive)
  if (isTRUE(save_script)) {
    setup_script_path <- .setup_value(
      setup_script_path,
      "Setup script path",
      file.path(base_dir, "ehr_setup_script.R"),
      interactive
    )
    overwrite_setup_script <- .setup_bool(
      overwrite_setup_script,
      "Overwrite setup script if it already exists?",
      default = FALSE,
      interactive = interactive
    )
  } else {
    setup_script_path <- setup_script_path %||% file.path(base_dir, "ehr_setup_script.R")
    overwrite_setup_script <- isTRUE(overwrite_setup_script)
  }

  choices <- list(
    source_size = source_size,
    src_name = src_names[[1]],
    src_names = src_names,
    engine = engine,
    db_name = db_name,
    base_dir = base_dir,
    data_dir = data_dir,
    config_setup = config_setup,
    include_disk_check = include_disk_check,
    cohort = cohort,
    import_scope = import_scope,
    sepsis_bundle = sepsis_bundle,
    memory_limit = memory_limit,
    threads = threads,
    registered_only = registered_only,
    icd_filter_path = icd_filter_path,
    pipeline = pipeline,
    include_download = include_download,
    raw_data_dir = raw_data_dir,
    run_comorbidities = run_comorbidities,
    run_iss = run_iss,
    analysis_level = analysis_level,
    make_icu_timeline = make_icu_timeline,
    window_before_hours = window_before_hours,
    window_after_hours = window_after_hours,
    include_other_icu_events = include_other_icu_events,
    include_date_only_events = include_date_only_events,
    materialize_icu = materialize_icu,
    merge = merge,
    target_schema = target_schema,
    inspect_options = inspect_options,
    load_method = load_method,
    package_path = package_path,
    save_script = save_script,
    setup_script_path = setup_script_path,
    overwrite_setup_script = overwrite_setup_script
  )

  next_steps <- c(
    "Review the generated script before running it.",
    unname(.setup_scope_help[[import_scope]]),
    "Import scope controls retained context; analysis level controls output organization. Context encounters are not automatically cohort members.",
    "Edit config/config_pass.json with Postgres and PhysioNet credentials if needed.",
    "Run generated blocks one at a time in your own R session."
  )
  if (isTRUE(is_duckdb)) {
    next_steps <- c(next_steps,
                    "This DuckDB plan covers cohort selection and raw loading plus generic cohort_info for built-ins. Compatible concept layers can run separately with run_concepts(engine = 'duckdb'); harmonization, clinical enrichment and merging require PostgreSQL. For PostgreSQL continuation, start the new script at run_db() and reuse cohort parquet instead of re-importing.")
    if (identical(cohort, "scid")) {
      next_steps <- c(next_steps,
                      "SCI enrichment (scid_classify()) is not available through the DuckDB path; build_cohort_info_general_duckdb() produces generic cohort_info only.")
    }
  }
  if (identical(source_size, "demo")) {
    next_steps <- c(next_steps,
                    "Demo mode is intended for a quick first test before full data.")
  } else {
    next_steps <- c(next_steps,
                    "Full-data mode needs substantial free disk space; run the generated disk check before downloading.")
  }
  if (length(src_names) > 1L) {
    next_steps <- c(next_steps,
                    "Multiple sources were selected; the generated script loops over src_names.")
  }
  if (isTRUE(run_comorbidities) || isTRUE(run_iss)) {
    next_steps <- c(next_steps,
                    "Clinical-score stages were included; review their relevance to your cohort before running them.")
  }
  if (identical(cohort, "none")) {
    next_steps <- c(next_steps,
                    "Unfiltered mode imports the available population and builds generic cohort information for supported built-in sources.")
  }
  if (!identical(inspect_options, "none")) {
    next_steps <- c(next_steps,
                    "Inspect/analyze snippets were added near the end of the generated script.")
  }

  out <- list(
    choices = choices,
    script = .setup_script(choices),
    next_steps = next_steps
  )
  class(out) <- "ehr_setup_plan"

  if (isTRUE(save_script)) {
    write_setup_script(out, setup_script_path,
                       overwrite = overwrite_setup_script)
    out$saved_script_path <- setup_script_path
    next_steps <- c(paste0("Generated setup script was saved to: ",
                           setup_script_path),
                    next_steps)
    out$next_steps <- next_steps
  } else {
    out$saved_script_path <- NULL
  }

  if (isTRUE(interactive)) {
    .setup_box("Generated script", c(
      "The script below is ready to review and run block by block.",
      if (isTRUE(save_script))
        paste0("A copy was saved to: ", setup_script_path)
      else
        "Use write_setup_script(plan, path) if you want to save it later."
    ), interactive)
    print(out)
    return(invisible(out))
  }

  out
}

#' Print an `rendehR` setup assistant plan
#'
#' Displays the chosen sources, mode, database/directory targets, cohort and
#' analysis-level choices, the generated setup script, and the recommended
#' next steps.
#'
#' @param x An `ehr_setup_plan` object, as returned by `ehr_setup_assistant()`.
#' @param ... Ignored.
#' @return Invisibly, `x`.
#' @export
print.ehr_setup_plan <- function(x, ...) {
  cat("rendehR Setup Assistant Plan\n")
  cat("Sources  : ", paste(x$choices$src_names, collapse = ", "), "\n", sep = "")
  cat("Mode     : ", x$choices$source_size, "\n", sep = "")
  cat("Engine   : ", x$choices$engine %||% "postgres", "\n", sep = "")
  cat("Database : ", x$choices$db_name, "\n", sep = "")
  cat("Base dir : ", x$choices$base_dir, "\n", sep = "")
  cat("Data dir : ", x$choices$data_dir %||% file.path(x$choices$base_dir, "data"), "\n", sep = "")
  cat("Cohort   : ", x$choices$cohort, "\n", sep = "")
  cat("Scope    : ", x$choices$import_scope, " - ", .setup_scope_help[[x$choices$import_scope]], "\n", sep = "")
  if (!is.null(x$choices$sepsis_bundle)) cat("Sepsis   : ", x$choices$sepsis_bundle, "\n", sep = "")
  cat("Analysis : ", x$choices$analysis_level, "\n", sep = "")
  cat("Pipeline : ", x$choices$pipeline, "\n", sep = "")
  cat("ICU ts   : ", if (isTRUE(x$choices$make_icu_timeline)) "yes" else "no", "\n", sep = "")
  cat("Merge    : ", x$choices$merge, "\n", sep = "")
  cat("Inspect  : ", paste(x$choices$inspect_options, collapse = ", "), "\n\n", sep = "")
  cat("Generated script:\n")
  cat(x$script, "\n\n", sep = "")
  cat("Next steps:\n")
  for (step in x$next_steps) cat("- ", step, "\n", sep = "")
  invisible(x)
}

#' Write a generated setup script to disk
#'
#' @param plan Object returned by `ehr_setup_assistant()`.
#' @param path Output file path.
#' @param overwrite Overwrite an existing file? Default FALSE.
#' @return Invisibly returns `path`.
#' @examples
#' \dontrun{
#' plan <- ehr_setup_assistant(interactive = FALSE)
#' write_setup_script(plan, "run_pipeline.R")
#' }
#' @export
write_setup_script <- function(plan, path, overwrite = FALSE) {
  if (!inherits(plan, "ehr_setup_plan"))
    stop("plan must be returned by ehr_setup_assistant().", call. = FALSE)
  if (file.exists(path) && !isTRUE(overwrite))
    stop("File exists: ", path, call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(plan$script, path)
  invisible(path)
}

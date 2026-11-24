# ---- Concept builder: draft a new time-series concept or edit an existing one --------------------------------------
# A concept is one or more rows of rendehr_concepts_config.csv per source family. Rows saved to the project's
# config/user_concepts.csv replace every packaged row for the same source + concept_name when run_concepts() gets
# that file as user_concepts. Database access here is read-only (READ ONLY transaction, statement timeout).

# ---- contract ---------------------------------------------------------------------------------------------------------
# Read every column as text and keep the literal "NA" so a written file matches the packaged one.
cb_read <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  x <- utils::read.csv(path, colClasses = "character", na.strings = character(), check.names = FALSE,
                       encoding = "UTF-8")
  .ehr_normalize_concept_cols(x)
}
CB_PKG_PATH <- tryCatch(ehr_config_path("rendehr_concepts_config.csv"),
                        error = function(e) system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"))
CB_BASE   <- cb_read(CB_PKG_PATH)
CB_HEADER <- names(CB_BASE)
# Columns the packaged file leaves blank (not "NA") when empty.
CB_BLANK  <- c("conversion_note", "end_var", "basos_absolute", "required_dedup", "notes",
               "dedup_cross_reference")
CB_METHODS <- c("Item IDs" = "itm_itm", "Exact names" = "rgx_itm", "Name pattern" = "rgx_exp_itm",
                "Value column" = "col_itm")
CB_METHOD_COL <- c(itm_itm = "id", rgx_itm = "regex", rgx_exp_itm = "regex", col_itm = "NA")
CB_CONV <- c("None: already in the target unit" = "none",
             "By source unit (unit conversion map)" = "map",
             "Fahrenheit to Celsius" = "f_to_c",
             "Fahrenheit to Celsius above 70 (no unit column)" = "cond_f_to_c",
             "FiO2 fraction to percent" = "fio2_fraction",
             "Ionized calcium: mmol/L or mg/dL by value" = "ca_ionized_by_value",
             "Urine dipstick grade to 0-3" = "dipstick_grade",
             "Exposure only: every row is 1" = "exposure")
CB_MAP_NAMES <- tryCatch(unique(.load_unit_conversion_map()$concept_name), error = function(e) character())
CB_SYN <- tryCatch(jsonlite::read_json(system.file("mcp", "inst", "synonyms", "base.json", package = "rendehR")),
                   error = function(e) list())
CB_DICTS <- tryCatch(jsonlite::read_json(system.file("mcp", "inst", "config", "mcp_dict_tables.json", package = "rendehR")),
                     error = function(e) list())

# Source families of this explorer that the packaged config covers, with one source to query for each.
CB_FAMS <- local({
  fam <- vapply(SRC, function(s) tryCatch(src_family(s), error = function(e) s), character(1))
  keep <- !duplicated(fam) & fam %in% CB_BASE$source
  stats::setNames(unname(SRC[keep]), fam[keep])
})
CB_FAM_LABEL <- stats::setNames(sub(" demo$", "", unname(SRC_LABEL[CB_FAMS])), names(CB_FAMS))

cb_na <- function(x) is.null(x) || !length(x) || is.na(x[1]) || trimws(x[1]) %in% c("", "NA")
cb_val <- function(x) if (cb_na(x)) NA_character_ else trimws(x[1])
cb_cell <- function(x, col = "") if (cb_na(x)) (if (col %in% CB_BLANK) "" else "NA") else trimws(as.character(x[1]))
cb_num <- function(x) if (is.null(x) || !length(x) || is.na(x)) "NA" else
  format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE, digits = 15)
cb_split <- function(x, sep = "|") {
  if (cb_na(x)) return(character())
  v <- trimws(strsplit(x, sep, fixed = TRUE)[[1]]); unique(v[nzchar(v)])
}
cb_empty <- function() CB_BASE[0, , drop = FALSE]
# Rows in the packaged column order; missing columns get the packaged empty value, extra columns are dropped.
cb_align <- function(df) {
  if (is.null(df)) return(cb_empty())
  out <- lapply(CB_HEADER, function(h) if (h %in% names(df)) as.character(df[[h]]) else rep(if (h %in% CB_BLANK) "" else "NA", nrow(df)))
  as.data.frame(stats::setNames(out, CB_HEADER), stringsAsFactors = FALSE, check.names = FALSE)
}

# CSV text as the packaged file writes it: "NA" unquoted, fields quoted only when they hold a comma, quote or newline.
cb_csv_text <- function(df) {
  q <- function(x) {
    x <- as.character(x); x[is.na(x)] <- "NA"
    k <- grepl("[\",\r\n]", x)
    x[k] <- paste0("\"", gsub("\"", "\"\"", x[k], fixed = TRUE), "\"")
    x
  }
  body <- if (nrow(df)) do.call(paste, c(lapply(df, q), sep = ",")) else character()
  paste(c(paste(q(names(df)), collapse = ","), body), collapse = "\n")
}
cb_write <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".csv")
  con <- file(tmp, open = "w", encoding = "UTF-8"); writeLines(cb_csv_text(df), con); close(con)
  if (!file.rename(tmp, path)) { unlink(tmp); stop("Could not write ", path) }
  invisible(path)
}
# Project rows with the draft's source + concept_name keys replaced by the draft; other rows are kept as they are.
cb_merge <- function(existing, draft) {
  if (is.null(existing) || !nrow(existing)) return(cb_align(draft))
  cols <- union(CB_HEADER, names(existing))
  fill <- function(df) { for (h in setdiff(cols, names(df))) df[[h]] <- if (h %in% CB_BLANK) "" else "NA"; df[, cols, drop = FALSE] }
  keep <- !paste(existing$source, existing$concept_name) %in% paste(draft$source, draft$concept_name)
  rbind(fill(existing[keep, , drop = FALSE]), fill(draft))
}
# What run_concepts() uses for a source family: the project rows if the project file has any, else the base rows.
cb_effective <- function(base, proj, fam, name) {
  p <- if (!is.null(proj) && nrow(proj)) proj[proj$source == fam & proj$concept_name == name, , drop = FALSE] else NULL
  if (!is.null(p) && nrow(p)) return(cb_align(p))
  cb_align(base[base$source == fam & base$concept_name == name, , drop = FALSE])
}
cb_name_issue <- function(name, existing = character(), new = TRUE) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) return("Enter a concept name.")
  if (!grepl("^[a-z][a-z0-9_]*$", name)) return("Use lower-case letters, digits and _, starting with a letter.")
  if (new && name %in% existing) return(sprintf("'%s' already exists. Choose another name, or edit it.", name))
  NULL
}
# Most common column set among the base rows of one source + table.
cb_template <- function(base, fam, tbl) {
  r <- base[base$source == fam & base$table_name == tbl, , drop = FALSE]
  if (!nrow(r)) { r <- cb_align(data.frame(source = fam, table_name = tbl, itm_class = "itm_itm", typ = "ts")); return(r) }
  key <- do.call(paste, c(r[c("sub_var", "val_var", "unit_var", "index_var", "dict", "itm_class")], sep = "\r"))
  cb_align(r[match(names(sort(table(key), decreasing = TRUE))[1], key), , drop = FALSE])
}
# A new row from a template: concept fields and provenance cleared for review.
cb_new_row <- function(base, fam, tbl) {
  r <- cb_template(base, fam, tbl)
  for (h in setdiff(CB_HEADER, c("source", "table_name", "sub_var", "val_var", "unit_var", "index_var", "dict", "itm_class", "method")))
    r[[h]] <- if (h %in% CB_BLANK) "" else "NA"
  r$conversion <- "none"; r$typ <- "ts"; r$date <- format(Sys.Date())
  r$method <- CB_METHOD_COL[[r$itm_class]] %||% "NA"
  r$notes <- "Drafted in the rendehR explorer concept builder; review before use."
  r
}
cb_tables <- function(base, fam) {
  t <- table(base$table_name[base$source == fam])
  names(sort(t, decreasing = TRUE))
}
# Field-level problems of one row, named by where the UI shows them (table, match, cols, conv).
cb_row_issues <- function(r, live_cols = NULL) {
  out <- character()
  add <- function(g, m) out <<- c(out, stats::setNames(m, g))
  cls <- r$itm_class; mv <- cb_val(r$match_value)
  ident <- function(x) cb_na(x) || grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)
  if (cb_na(r$table_name)) add("table", "Choose an event table.")
  bad <- Filter(function(f) !ident(r[[f]]), c("table_name", "sub_var", "val_var", "unit_var", "index_var", "dict"))
  if (length(bad)) add("cols", sprintf("Use plain column or table names: %s.", paste(bad, collapse = ", ")))
  if (identical(cls, "itm_itm")) {
    if (is.na(mv)) add("match", "Add at least one item ID.")
    else if (!all(grepl("^[0-9]+$", cb_split(mv)))) add("match", "Item IDs are whole numbers.")
  } else if (identical(cls, "rgx_itm")) {
    if (is.na(mv)) add("match", "Add at least one name.")
  } else if (identical(cls, "rgx_exp_itm")) {
    if (is.na(mv)) add("match", "Enter a name pattern.")
    else {
      err <- tryCatch({ grepl(mv, "", ignore.case = TRUE); NULL }, error = function(e) conditionMessage(e),
                      warning = function(w) conditionMessage(w))
      if (!is.null(err)) add("match", sprintf("Invalid pattern: %s", err))
    }
  }
  if (!identical(cls, "col_itm") && cb_na(r$sub_var)) add("cols", "The ID / name column is empty.")
  if (cb_na(r$val_var)) add(if (identical(cls, "col_itm")) "match" else "cols", "Choose the value column.")
  if (cb_na(r$index_var)) add("cols", "The time column is empty.")
  conv <- r$conversion %||% "none"
  if (identical(conv, "map")) {
    if (cb_na(r$unit_var)) add("conv", "Conversion by source unit needs a unit column.")
    if (!r$concept_name %in% CB_MAP_NAMES) add("conv", sprintf("The unit conversion map has no entries for '%s'.", r$concept_name))
  } else if (grepl("^[*/]\\s*[0-9.]+$", conv)) {
    if (!cb_na(r$unit_var)) add("conv", "A fixed factor applies to every unit; with a unit column use the unit conversion map.")
  } else if (!conv %in% CB_CONV) add("conv", sprintf("Unknown conversion '%s'.", conv))
  if (!is.null(live_cols) && !cb_na(r$table_name)) {
    if (!length(live_cols)) add("cols", sprintf("Table %s is not in the raw schema.", r$table_name))
    else {
      used <- c(if (!identical(cls, "col_itm")) r$sub_var, r$val_var, r$unit_var, r$index_var)
      miss <- setdiff(used[!vapply(used, cb_na, logical(1))], live_cols)
      if (length(miss)) add("cols", sprintf("Not in %s: %s.", r$table_name, paste(miss, collapse = ", ")))
    }
  }
  out
}
# Field-by-field changes per source; rows are compared in order.
cb_diff <- function(before, after) {
  out <- list()
  for (fam in names(after)) {
    b <- before[[fam]] %||% cb_empty(); a <- after[[fam]]
    for (i in seq_len(max(nrow(a), nrow(b)))) {
      lab <- CB_FAM_LABEL[[fam]] %||% fam
      if (i > nrow(b)) { out[[length(out) + 1]] <- data.frame(Source = lab, Row = i, Field = "(row)", Before = "", After = "added"); next }
      if (i > nrow(a)) { out[[length(out) + 1]] <- data.frame(Source = lab, Row = i, Field = "(row)", Before = "", After = "removed"); next }
      ch <- CB_HEADER[vapply(CB_HEADER, function(h) !identical(b[[h]][i], a[[h]][i]), logical(1))]
      if (length(ch)) out[[length(out) + 1]] <- data.frame(Source = lab, Row = i, Field = ch, Before = unlist(b[i, ch]), After = unlist(a[i, ch]))
    }
  }
  if (!length(out)) return(data.frame(Source = character(), Row = integer(), Field = character(), Before = character(), After = character()))
  x <- do.call(rbind, out); rownames(x) <- NULL; x
}

# ---- read-only database helpers ----------------------------------------------------------------------------------
cb_db <- function(fn, timeout_ms = 15000) {
  .with_connection(NULL, function(con) {
    DBI::dbBegin(con)
    on.exit(try(DBI::dbRollback(con), silent = TRUE), add = TRUE)
    DBI::dbExecute(con, "SET TRANSACTION READ ONLY")
    DBI::dbExecute(con, sprintf("SET LOCAL statement_timeout = %d", as.integer(timeout_ms)))
    fn(con)
  })
}
cb_err <- function(e) {
  m <- conditionMessage(e)
  if (grepl("statement timeout", m)) return("The query took too long and was stopped. Narrow it down.")
  trimws(sub("^.*?ERROR:\\s*", "", m))
}
cb_qt <- function(con, schema, table) paste0(DBI::dbQuoteIdentifier(con, schema), ".", DBI::dbQuoteIdentifier(con, table))
# Search terms: comma-separated, optionally widened by the local synonym list. Short terms match whole words.
cb_terms <- function(q, syn = TRUE) {
  t <- unique(tolower(trimws(strsplit(q %||% "", ",")[[1]]))); t <- t[nzchar(t)]
  if (syn && length(CB_SYN)) for (x in t) {
    k <- names(CB_SYN)[vapply(names(CB_SYN), function(n) x == tolower(n) || x %in% tolower(unlist(CB_SYN[[n]])), logical(1))]
    t <- unique(c(t, tolower(k), tolower(unlist(CB_SYN[k]))))
  }
  utils::head(t, 40)
}
cb_term_sql <- function(con, expr, terms, id_expr = NULL) {
  p <- vapply(terms, function(t) {
    if (!is.null(id_expr) && grepl("^[0-9]+$", t)) return(paste0(id_expr, " = ", DBI::dbQuoteString(con, t)))
    if (nchar(t) <= 4) return(paste0(expr, " ~ ", DBI::dbQuoteString(con, paste0("(^|[^a-z0-9/])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", t), "($|[^a-z0-9/])"))))
    paste0("POSITION(", DBI::dbQuoteString(con, t), " IN ", expr, ") > 0")
  }, character(1))
  paste0("(", paste(p, collapse = " OR "), ")")
}
# Item dictionaries of a family (id column itemid) from the MCP dictionary map.
cb_item_dicts <- function(fam) {
  m <- CB_DICTS[[fam]]$tables %||% list()
  m[vapply(m, function(x) identical(x$id, "itemid"), logical(1))]
}
cb_search_dict <- function(src, fam, tables, terms, limit = 100) {
  schema <- src_schema_names(src)$raw
  maps <- cb_item_dicts(fam)
  maps <- maps[intersect(tables %||% names(maps), names(maps))]
  if (!length(maps)) stop("No item dictionary for this source.")
  cb_db(function(con) {
    q <- function(x) DBI::dbQuoteIdentifier(con, x)
    res <- lapply(names(maps), function(t) {
      m <- maps[[t]]
      f <- DBI::dbListFields(con, DBI::Id(schema = schema, table = t))
      col <- function(x, a) paste0(if (!is.null(x) && x %in% f) paste0("CAST(", q(x), " AS TEXT)") else "NULL::text", " AS ", a)
      lab <- paste0("LOWER(CAST(", q(m$label), " AS TEXT))")
      d <- DBI::dbGetQuery(con, paste0("SELECT ", paste(c(col(m$id, "id"), col(m$label, "label"), col(m$category, "category"), col(m$unit, "unit")), collapse = ", "),
                                       " FROM ", cb_qt(con, schema, t), " WHERE ", cb_term_sql(con, lab, terms, paste0("CAST(", q(m$id), " AS TEXT)")),
                                       " ORDER BY ", q(m$id), " LIMIT ", limit))
      if (nrow(d)) d$dictionary <- t
      d
    })
    do.call(rbind, Filter(nrow, res)) %||% data.frame(id = character(), label = character(), category = character(), unit = character(), dictionary = character())
  })
}
# Distinct names of a name column (or dictionary labels) with their row counts.
cb_search_names <- function(src, table, col, terms, limit = 100) {
  schema <- src_schema_names(src)$raw
  cb_db(function(con) {
    e <- paste0("CAST(", DBI::dbQuoteIdentifier(con, col), " AS TEXT)")
    DBI::dbGetQuery(con, paste0("SELECT ", e, " AS name, COUNT(*)::float8 AS n FROM ", cb_qt(con, schema, table),
                                " WHERE ", cb_term_sql(con, paste0("LOWER(", e, ")"), terms), " GROUP BY 1 ORDER BY 2 DESC LIMIT ", limit))
  })
}
# Names an exact list or a pattern selects, as run_concepts() matches them (dictionary label when a dictionary is set).
cb_name_matches <- function(src, r, limit = 100) {
  schema <- src_schema_names(src)$raw
  dict <- cb_val(r$dict)
  tbl <- dict %|na|% r$table_name; col <- if (!is.na(dict)) "label" else r$sub_var
  cb_db(function(con) {
    e <- paste0("CAST(", DBI::dbQuoteIdentifier(con, col), " AS TEXT)")
    if (identical(r$itm_class, "rgx_itm")) {
      v <- tolower(cb_split(r$match_value)); if (!length(v)) return(NULL)
      d <- DBI::dbGetQuery(con, paste0("SELECT LOWER(TRIM(", e, ")) AS name, COUNT(*)::float8 AS n FROM ", cb_qt(con, schema, tbl),
                                       " WHERE LOWER(TRIM(", e, ")) IN (", paste(DBI::dbQuoteString(con, v), collapse = ", "), ") GROUP BY 1"))
      miss <- setdiff(v, d$name)
      rbind(d, data.frame(name = miss, n = rep(0, length(miss))))
    } else {
      DBI::dbGetQuery(con, paste0("SELECT ", e, " AS name, COUNT(*)::float8 AS n FROM ", cb_qt(con, schema, tbl), " WHERE ", e, " ~* ",
                                  DBI::dbQuoteString(con, r$match_value), " GROUP BY 1 ORDER BY 2 DESC LIMIT ", limit))
    }
  }, timeout_ms = 10000)
}
`%|na|%` <- function(a, b) if (is.na(a)) b else a
cb_columns <- function(src, table) {
  cb_db(function(con) DBI::dbGetQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_schema = $1 AND table_name = $2 ORDER BY ordinal_position",
                                      params = list(src_schema_names(src)$raw, table))$column_name, timeout_ms = 5000)
}
cb_item_labels <- function(src, fam, ids) {
  ids <- ids[grepl("^[0-9]+$", ids)]
  if (!length(ids)) return(character())
  schema <- src_schema_names(src)$raw
  cb_db(function(con) {
    d <- do.call(rbind, lapply(names(cb_item_dicts(fam)), function(t) tryCatch(DBI::dbGetQuery(con, paste0(
      "SELECT CAST(itemid AS TEXT) AS id, CAST(label AS TEXT) AS label FROM ", cb_qt(con, schema, t),
      " WHERE itemid IN (", paste(ids, collapse = ", "), ")")), error = function(e) NULL)))
    if (is.null(d) || !nrow(d)) character() else stats::setNames(d$label, d$id)
  }, timeout_ms = 5000)
}

# Values the concept would extract from the cohort's raw tables for one source, using the package's own SELECT
# (match, conversion and cohort spine as run_concepts() builds them; chart-copy deduplication is not applied).
cb_preview <- function(src, rows) {
  ids <- get_dataset_config(src)$select_id_cols
  raw <- src_schema_names(src)$raw
  part <- vapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    s <- build_select_unified(table_name = r$table_name, sub_var = cb_val(r$sub_var), match_value = cb_val(r$match_value),
                              val_var = r$val_var, unit_var = cb_val(r$unit_var), index_var = r$index_var, end_var = NULL,
                              conversion = r$conversion, source = src, dict = cb_val(r$dict), unit = cb_val(r$unit),
                              itm_class = r$itm_class, raw_schema = raw, concept_name = r$concept_name, typ = "ts")
    sprintf("SELECT CAST(s.%s AS TEXT) AS pat, CAST(s.%s AS TEXT) AS adm, s.value, CAST(s.unit_raw AS TEXT) AS unit_raw,
            CAST(s.source_itemid AS TEXT) AS item, CAST(s.source_label AS TEXT) AS label FROM (%s) s", ids[1], ids[2], s)
  }, character(1))
  cte <- paste0("WITH x AS (", paste(part, collapse = "\nUNION ALL\n"), ")\n")
  lo_lim <- suppressWarnings(as.numeric(rows$min_value[1])); hi_lim <- suppressWarnings(as.numeric(rows$max_value[1]))
  cb_db(function(con) {
    s <- DBI::dbGetQuery(con, paste0(cte,
      "SELECT COUNT(*)::float8 AS n_rows, COUNT(value)::float8 AS n_value, COUNT(DISTINCT pat)::float8 AS n_pat,
              COUNT(DISTINCT adm)::float8 AS n_adm, MIN(value)::float8 AS vmin, MAX(value)::float8 AS vmax,
              CAST(percentile_cont(ARRAY[0.01, 0.25, 0.5, 0.75, 0.99]) WITHIN GROUP (ORDER BY value) AS TEXT) AS q,
              COUNT(*) FILTER (WHERE value < $1)::float8 AS n_low, COUNT(*) FILTER (WHERE value > $2)::float8 AS n_high
       FROM x"), params = list(lo_lim, hi_lim))
    q <- if (is.na(s$q)) rep(NA_real_, 5) else as.numeric(strsplit(gsub("[{}]", "", s$q), ",")[[1]])
    units <- DBI::dbGetQuery(con, paste0(cte, "SELECT COALESCE(unit_raw, '') AS unit, COUNT(*)::float8 AS n, COUNT(value)::float8 AS n_value
                                               FROM x GROUP BY 1 ORDER BY 2 DESC LIMIT 25"))
    items <- DBI::dbGetQuery(con, paste0(cte, "SELECT COALESCE(item, '') AS item, COALESCE(label, '') AS label, COUNT(*)::float8 AS n
                                               FROM x GROUP BY 1, 2 ORDER BY 3 DESC LIMIT 25"))
    hist <- NULL; lo <- NA; hi <- NA
    if (s$n_value > 0) {
      lo <- max(s$vmin, min(c(q[1], lo_lim), na.rm = TRUE)); hi <- min(s$vmax, max(c(q[5], hi_lim), na.rm = TRUE))
      if (hi > lo) {
        hist <- DBI::dbGetQuery(con, paste0(cte, "SELECT LEAST(width_bucket(value, $1, $2, 40), 40) AS b, COUNT(*)::float8 AS n
                                                  FROM x WHERE value BETWEEN $1 AND $2 GROUP BY 1 ORDER BY 1"), params = list(lo, hi))
        hist$x <- lo + (hist$b - 0.5) * (hi - lo) / 40
      }
    }
    list(s = s, q = q, units = units, items = items, hist = hist, lo = lo, hi = hi, width = (hi - lo) / 40,
         lim = c(lo_lim, hi_lim), target = cb_val(rows$unit[1]), conv = unique(rows$conversion))
  }, timeout_ms = 30000)
}
cb_feedback <- function(msgs) if (length(msgs)) tags$div(class = "invalid-feedback d-block mb-2", lapply(unname(msgs), tags$div))

# ---- one source family: its mapping rows ---------------------------------------------------------------------------
cb_source_ui <- function(id, fam) {
  ns <- NS(id)
  tbls <- cb_tables(CB_BASE, fam)
  nav_panel(CB_FAM_LABEL[[fam]], value = fam,
    tags$div(class = "d-flex flex-wrap align-items-center gap-3 mb-2",
      input_switch(ns("on"), "Include this source", FALSE),
      uiOutput(ns("rows_ui"), inline = TRUE)),
    uiOutput(ns("off_hint")),
    conditionalPanel("input.on", ns = ns,
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(4, 8)), fill = FALSE,
        tags$div(
          selectInput(ns("tbl"), "Event table", tbls, tbls[1]),
          uiOutput(ns("tbl_msg")),
          radioGroupButtons(ns("method"), "Match rows by", CB_METHODS, "itm_itm", size = "sm"),
          selectInput(ns("conv"), "Unit conversion", CB_CONV, "none"),
          uiOutput(ns("conv_msg")),
          accordion(open = FALSE, accordion_panel("Source columns", icon = bs_icon("sliders"),
            selectizeInput(ns("sub_var"), "ID / name column", NULL, options = list(create = TRUE)),
            conditionalPanel("input.method != 'col_itm'", ns = ns,
              selectizeInput(ns("val_var"), "Value column", NULL, options = list(create = TRUE))),
            selectizeInput(ns("unit_var"), "Unit column", NULL, options = list(create = TRUE, placeholder = "none")),
            selectizeInput(ns("index_var"), "Time column", NULL, options = list(create = TRUE)),
            selectizeInput(ns("dict"), "Dictionary table", NULL, options = list(create = TRUE, placeholder = "none")),
            uiOutput(ns("cols_msg"))))),
        tags$div(
          conditionalPanel("input.method == 'itm_itm'", ns = ns,
            selectizeInput(ns("ids"), "Item IDs", NULL, multiple = TRUE, width = "100%",
                           options = list(create = TRUE, persist = FALSE, plugins = list("remove_button"),
                                          placeholder = "Type an ID and press Enter, or add from the search"))),
          conditionalPanel("input.method == 'rgx_itm'", ns = ns,
            selectizeInput(ns("names"), "Names (exact, case-insensitive)", NULL, multiple = TRUE, width = "100%",
                           options = list(create = TRUE, persist = FALSE, plugins = list("remove_button"),
                                          placeholder = "Type a name and press Enter, or add from the search"))),
          conditionalPanel("input.method == 'rgx_exp_itm'", ns = ns,
            textInput(ns("regex"), "Name pattern (regular expression, case-insensitive)", width = "100%",
                      placeholder = "e.g. ^heart rate$")),
          conditionalPanel("input.method == 'col_itm'", ns = ns,
            selectizeInput(ns("valcol"), "Value column", NULL, options = list(create = TRUE))),
          uiOutput(ns("match_msg")),
          if (LIVE) tagList(
            conditionalPanel("input.method == 'itm_itm' || input.method == 'rgx_itm'", ns = ns,
              tags$div(class = "d-flex flex-wrap align-items-end gap-2",
                tags$div(class = "flex-grow-1", textInput(ns("q"), "Search", width = "100%", placeholder = "heart rate, hr")),
                tags$div(class = "mb-3", checkboxInput(ns("syn"), "Synonyms", TRUE)),
                tags$div(class = "mb-3", actionButton(ns("go"), "Search", icon = icon("magnifying-glass")))),
              uiOutput(ns("hits_head")),
              DTOutput(ns("hits")),
              uiOutput(ns("add_ui"))),
            conditionalPanel("input.method == 'rgx_itm' || input.method == 'rgx_exp_itm'", ns = ns,
              tags$h6(class = "mt-3", "Names matched"), DTOutput(ns("matches"))))
          else tags$p(class = "small text-muted", bs_icon("plug", class = "me-1"),
                      "Dictionary search and value preview need the live database connection."))),
      if (LIVE) tagList(
        tags$hr(),
        tags$div(class = "d-flex flex-wrap align-items-center gap-2 mb-2",
          actionButton(ns("pv_go"), "Preview values", icon = icon("chart-column"), class = "btn-outline-primary"),
          tags$span(class = "small text-muted", "Cohort rows of this source in the raw schema, all mapping rows, before deduplication.")),
        uiOutput(ns("pv_out")))))
}

cb_source_server <- function(id, fam, def_all, def_changed, base) moduleServer(id, function(input, output, session) {
  src <- CB_FAMS[[fam]]
  internal <- c(".ids", ".names", ".regex")
  rv <- reactiveValues(rows = NULL, isnew = logical(), active = 1L, on = FALSE)
  labels <- reactiveVal(character())
  cols_cache <- reactiveVal(list())

  # store rows carry each method's entry separately so switching method keeps what was typed
  prep <- function(df) {
    df <- cb_align(df)
    df$.ids <- ifelse(df$itm_class == "itm_itm", df$match_value, "NA")
    df$.names <- ifelse(df$itm_class == "rgx_itm", df$match_value, "NA")
    df$.regex <- ifelse(df$itm_class == "rgx_exp_itm", df$match_value, "NA")
    df
  }
  set_field <- function(field, value) {
    if (is.null(rv$rows) || !nrow(rv$rows)) return(invisible())
    i <- rv$active
    if (!identical(rv$rows[[field]][i], value)) rv$rows[[field]][i] <- value
  }
  cur <- reactive({ req(rv$rows, nrow(rv$rows) >= rv$active); rv$rows[rv$active, , drop = FALSE] })
  col_cands <- function(tbl, field) {
    b <- base(); v <- b[[field]][b$source == fam & b$table_name == tbl]
    live <- if (LIVE) cols_cache()[[tbl]] else NULL
    sort(unique(c(v[!v %in% c("NA", "")], live)))
  }
  set_cols <- function(r) {
    up <- function(id, v, cands) updateSelectizeInput(session, id, choices = c("", unique(c(if (!cb_na(v)) v, cands))),
                                                      selected = if (cb_na(v)) "" else v)
    for (f in c("sub_var", "val_var", "unit_var", "index_var")) up(f, r[[f]], col_cands(r$table_name, f))
    up("valcol", r$val_var, col_cands(r$table_name, "val_var"))
    d <- base()$dict[base()$source == fam]; up("dict", r$dict, sort(unique(d[!d %in% c("NA", "")])))
  }
  id_choices <- function(ids) { l <- labels()[ids]; stats::setNames(ids, ifelse(is.na(l), ids, paste0(ids, " · ", l))) }
  load_inputs <- function(r) {
    updateSelectInput(session, "tbl", choices = unique(c(cb_tables(base(), fam), r$table_name)), selected = r$table_name)
    updateRadioGroupButtons(session, "method", selected = r$itm_class)
    updateSelectInput(session, "conv", choices = c(CB_CONV, stats::setNames(setdiff(r$conversion, CB_CONV), setdiff(r$conversion, CB_CONV))),
                      selected = r$conversion)
    ids <- cb_split(r$.ids)
    if (LIVE && length(setdiff(ids, names(labels()))))
      labels(c(labels(), tryCatch(cb_item_labels(src, fam, setdiff(ids, names(labels()))), error = function(e) character())))
    updateSelectizeInput(session, "ids", choices = id_choices(ids), selected = ids)
    nm <- cb_split(r$.names); updateSelectizeInput(session, "names", choices = nm, selected = nm)
    updateTextInput(session, "regex", value = if (cb_na(r$.regex)) "" else r$.regex)
    set_cols(r)
  }
  add_row <- function(tbl) {
    r <- prep(cb_new_row(base(), fam, tbl))
    rv$rows <- if (is.null(rv$rows)) r else rbind(rv$rows, r)
    rv$isnew <- c(rv$isnew, TRUE); rv$active <- nrow(rv$rows)
    load_inputs(r)
  }
  # called by the page: load rows (edit) or clear (new)
  load <- function(df = NULL, on = FALSE) {
    rv$rows <- if (!is.null(df) && nrow(df)) prep(df) else NULL
    rv$isnew <- rep(FALSE, NROW(rv$rows)); rv$active <- 1L; rv$on <- isTRUE(on); pv_res(NULL)
    update_switch("on", value = isTRUE(on), session = session)
    if (!is.null(rv$rows)) load_inputs(rv$rows[1, ])
  }

  observeEvent(input$on, {
    rv$on <- isTRUE(input$on)
    if (rv$on && is.null(rv$rows)) add_row(input$tbl %||% cb_tables(base(), fam)[1])
  }, ignoreInit = TRUE)
  output$off_hint <- renderUI(if (!rv$on) tags$p(class = "small text-muted mb-0",
    "Off: this source is not part of the draft; any current definition for it stays in effect."))
  # the row selector follows only the row tables, not every keystroke
  row_tbls <- reactiveVal(character())
  observe({ v <- rv$rows$table_name %||% character(); if (!identical(v, isolate(row_tbls()))) row_tbls(v) })
  output$rows_ui <- renderUI({
    req(rv$on, length(row_tbls()))
    n <- length(row_tbls())
    tags$div(class = "d-flex align-items-center gap-2",
      if (n > 1) radioGroupButtons(session$ns("row"), NULL, stats::setNames(seq_len(n), sprintf("Row %d: %s", seq_len(n), row_tbls())),
                                   selected = rv$active, size = "sm"),
      actionButton(session$ns("add_row"), "Add row", icon = icon("plus"), class = "btn-sm btn-outline-secondary",
                   title = "Another table or item group, with its own conversion"),
      if (n > 1) actionButton(session$ns("del_row"), "Remove row", icon = icon("trash"), class = "btn-sm btn-outline-danger"))
  })
  observeEvent(input$row, {
    i <- as.integer(input$row)
    if (!is.na(i) && i != rv$active && i <= NROW(rv$rows)) { rv$active <- i; load_inputs(rv$rows[i, ]) }
  })
  observeEvent(input$add_row, add_row(cur()$table_name))
  observeEvent(input$del_row, {
    req(NROW(rv$rows) > 1)
    i <- rv$active; rv$rows <- rv$rows[-i, , drop = FALSE]; rv$isnew <- rv$isnew[-i]
    rv$active <- max(1L, i - 1L); load_inputs(rv$rows[rv$active, ])
  })

  # inputs write into the active row
  observeEvent(input$tbl, {
    req(rv$rows); r <- cur()
    if (identical(input$tbl, r$table_name)) return()
    t <- cb_template(base(), fam, input$tbl)
    for (f in c("table_name", "sub_var", "val_var", "unit_var", "index_var", "dict")) set_field(f, t[[f]])
    if (!identical(t$itm_class, r$itm_class)) {
      set_field("itm_class", t$itm_class); set_field("method", CB_METHOD_COL[[t$itm_class]] %||% "NA")
      updateRadioGroupButtons(session, "method", selected = t$itm_class)
    }
    set_cols(cur())
  }, ignoreInit = TRUE)
  observeEvent(input$method, {
    req(rv$rows)
    if (identical(input$method, cur()$itm_class)) return()
    set_field("itm_class", input$method); set_field("method", CB_METHOD_COL[[input$method]])
  }, ignoreInit = TRUE)
  observeEvent(input$conv, set_field("conversion", input$conv), ignoreInit = TRUE)
  for (f in c("sub_var", "unit_var", "index_var", "dict")) local({
    f <- f
    observeEvent(input[[f]], set_field(f, cb_cell(input[[f]], f)), ignoreInit = TRUE, ignoreNULL = FALSE)
  })
  # one value column behind two inputs (main area for Value column, accordion otherwise)
  observeEvent(input$val_var, {
    req(rv$rows, !identical(cur()$itm_class, "col_itm"))
    set_field("val_var", cb_cell(input$val_var)); updateSelectizeInput(session, "valcol", selected = input$val_var)
  }, ignoreInit = TRUE)
  observeEvent(input$valcol, {
    req(rv$rows, identical(cur()$itm_class, "col_itm"))
    set_field("val_var", cb_cell(input$valcol))
    updateSelectizeInput(session, "val_var", choices = c("", unique(c(input$valcol, col_cands(cur()$table_name, "val_var")))), selected = input$valcol)
  }, ignoreInit = TRUE)
  observeEvent(input$ids, {
    v <- unique(trimws(unlist(strsplit(input$ids %||% character(), "[|,;[:space:]]+")))); v <- v[nzchar(v)]
    if (!identical(v, input$ids %||% character())) updateSelectizeInput(session, "ids", choices = id_choices(v), selected = v)
    set_field(".ids", if (length(v)) paste(v, collapse = "|") else "NA")
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(input$names, {
    v <- unique(trimws(unlist(strsplit(input$names %||% character(), "|", fixed = TRUE)))); v <- v[nzchar(v)]
    if (!identical(v, input$names %||% character())) updateSelectizeInput(session, "names", choices = v, selected = v)
    set_field(".names", if (length(v)) paste(v, collapse = "|") else "NA")
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(input$regex, set_field(".regex", cb_cell(input$regex)), ignoreInit = TRUE)

  # live column list of the active table (choices and a column check)
  observeEvent(list(rv$on, cur()$table_name), {
    req(LIVE, rv$on); t <- cur()$table_name
    if (cb_na(t) || !is.null(cols_cache()[[t]])) return()
    cc <- tryCatch(cb_columns(src, t), error = function(e) NULL)
    if (!is.null(cc)) { x <- cols_cache(); x[[t]] <- cc; cols_cache(x); set_cols(cur()) }
  })

  # rows as they go into the CSV: match value from the chosen method, definition fields applied
  rows <- reactive({
    if (!rv$on || is.null(rv$rows) || !nrow(rv$rows)) return(NULL)
    df <- rv$rows; da <- def_all(); dc <- def_changed()
    df$match_value <- ifelse(df$itm_class == "itm_itm", df$.ids, ifelse(df$itm_class == "rgx_itm", df$.names,
                       ifelse(df$itm_class == "rgx_exp_itm", df$.regex, "NA")))
    col <- df$itm_class == "col_itm"; df$sub_var[col] <- "NA"; df$dict[col] <- "NA"
    for (i in seq_len(nrow(df))) {
      d <- if (rv$isnew[i]) da else dc
      for (f in names(d)) df[[f]][i] <- d[[f]]
    }
    df$concept_name <- da$concept_name; df$source <- fam
    df[, CB_HEADER, drop = FALSE]
  })
  issues_of <- function(i) {
    r <- rows()[i, ]
    cb_row_issues(r, if (LIVE && !cb_na(r$table_name)) cols_cache()[[r$table_name]])
  }
  active_issues <- reactive({ req(rows(), nrow(rows()) >= rv$active); issues_of(rv$active) })
  output$tbl_msg <- renderUI(cb_feedback(active_issues()[names(active_issues()) == "table"]))
  output$match_msg <- renderUI(cb_feedback(active_issues()[names(active_issues()) == "match"]))
  output$cols_msg <- renderUI(cb_feedback(active_issues()[names(active_issues()) == "cols"]))
  output$conv_msg <- renderUI({
    r <- req(rows())[rv$active, ]
    tagList(cb_feedback(active_issues()[names(active_issues()) == "conv"]),
            if (r$conversion %in% c("cond_f_to_c", "fio2_fraction", "ca_ionized_by_value") && !cb_na(r$unit_var))
              tags$div(class = "form-text mb-2", "Meant for items without a unit column; the unit column is not used."))
  })
  issues <- reactive({
    d <- rows(); if (is.null(d)) return(character())
    unlist(lapply(seq_len(nrow(d)), function(i) {
      m <- unique(unname(issues_of(i)))
      if (length(m)) sprintf("%s%s: %s", CB_FAM_LABEL[[fam]], if (nrow(d) > 1) sprintf(" row %d", i) else "", m)
    }))
  })

  # ---- search: item dictionaries (Item IDs) or distinct names (Exact names)
  used_by <- function(keys, cls) {
    b <- base(); b <- b[b$source == fam & b$itm_class == cls, ]
    if (!nrow(b)) return(rep("", length(keys)))
    k <- lapply(b$match_value, function(m) tolower(cb_split(m)))
    vapply(tolower(keys), function(x) paste(unique(b$concept_name[vapply(k, function(v) x %in% v, logical(1))]), collapse = ", "), character(1))
  }
  hits <- eventReactive(input$go, {
    req(LIVE, rv$rows)
    terms <- cb_terms(input$q, isTRUE(input$syn))
    validate(need(length(terms) > 0, "Enter a search term."))
    r <- cur()
    tryCatch(withProgress(message = "Searching...", {
      if (identical(r$itm_class, "itm_itm")) {
        d <- cb_search_dict(src, fam, if (!cb_na(r$dict)) r$dict, terms)
        d$used_by <- used_by(d$id, "itm_itm"); list(kind = "ids", d = d)
      } else {
        dict <- cb_val(r$dict)
        d <- cb_search_names(src, dict %|na|% r$table_name, if (!is.na(dict)) "label" else r$sub_var, terms)
        d$used_by <- used_by(d$name, "rgx_itm"); list(kind = "names", d = d, dict = !is.na(dict))
      }
    }), error = function(e) list(error = cb_err(e)))
  })
  output$hits_head <- renderUI({
    h <- hits(); if (!is.null(h$error)) return(tags$div(class = "alert alert-warning small py-2", h$error))
    tags$div(class = "small text-muted mb-1", sprintf("%s result%s; select rows, then add them.", fmt_n(nrow(h$d)), if (nrow(h$d) == 1) "" else "s"))
  })
  output$hits <- renderDT({
    h <- hits(); req(is.null(h$error))
    d <- if (h$kind == "ids") h$d %>% transmute(ID = id, Label = label, Category = category, Unit = unit, Dictionary = dictionary, `Used by` = used_by)
         else stats::setNames(h$d %>% transmute(Name = name, n = mask_n(n), `Used by` = used_by),
                              c("Name", if (isTRUE(h$dict)) "Dictionary entries" else "Rows (raw table)", "Used by"))
    datatable(d, rownames = FALSE, selection = "multiple", class = "compact hover",
              options = list(dom = .dt_dom_pages, pageLength = 8, scrollX = TRUE, language = list(emptyTable = "No match")))
  })
  output$add_ui <- renderUI({
    n <- length(input$hits_rows_selected); req(n > 0)
    actionButton(session$ns("add"), sprintf("Add %d selected", n), icon = icon("plus"), class = "btn-sm btn-primary mb-2")
  })
  observeEvent(input$add, {
    h <- hits(); sel <- input$hits_rows_selected; req(is.null(h$error), length(sel))
    if (h$kind == "ids") {
      labels(c(labels(), stats::setNames(h$d$label[sel], h$d$id[sel])))
      v <- unique(c(cb_split(cur()$.ids), h$d$id[sel]))
      set_field(".ids", paste(v, collapse = "|")); updateSelectizeInput(session, "ids", choices = id_choices(v), selected = v)
    } else {
      v <- unique(c(cb_split(cur()$.names), h$d$name[sel]))
      set_field(".names", paste(v, collapse = "|")); updateSelectizeInput(session, "names", choices = v, selected = v)
    }
    DT::selectRows(DT::dataTableProxy("hits"), NULL)
  })

  # ---- names an exact list or pattern selects (debounced)
  match_key <- debounce(reactive({
    req(LIVE, rows(), nrow(rows()) >= rv$active); r <- rows()[rv$active, ]
    req(r$itm_class %in% c("rgx_itm", "rgx_exp_itm"))
    r[c("itm_class", "table_name", "sub_var", "dict", "match_value")]
  }), 800)
  output$matches <- renderDT({
    r <- match_key()
    validate(need(!cb_na(r$match_value), if (r$itm_class == "rgx_itm") "Add names to see which ones occur." else "Enter a pattern to see the names it selects."),
             need(!length(cb_row_issues(cbind(r, val_var = "x", index_var = "x", conversion = "none", concept_name = "x",
                                              unit_var = NA, stringsAsFactors = FALSE))), "Fix the mapping first."))
    d <- tryCatch(cb_name_matches(src, r), error = function(e) structure(list(), msg = cb_err(e)))
    validate(need(is.data.frame(d), attr(d, "msg") %||% "No result."))
    lab <- if (!cb_na(r$dict)) "Dictionary entries" else "Rows (raw table)"
    datatable(stats::setNames(d %>% transmute(Name = name, n = if_else(n == 0, "not found", mask_n(n))), c("Name", lab)), rownames = FALSE, class = "compact",
              options = list(dom = .dt_dom_pages, pageLength = 8, language = list(emptyTable = "No name matches")))
  })

  # ---- value preview
  pv_res <- reactiveVal(NULL)          # cleared whenever the page loads other rows
  observeEvent(input$pv_go, {
    req(LIVE)
    d <- rows(); req(d)
    m <- issues()
    pv_res(if (length(m)) list(error = paste("Fix the mapping first:", paste(m, collapse = " ")))
           else tryCatch(withProgress(message = sprintf("Previewing %s...", CB_FAM_LABEL[[fam]]), cb_preview(src, d)),
                         error = function(e) list(error = cb_err(e))))
  })
  pv <- reactive(req(pv_res()))
  output$pv_out <- renderUI({
    p <- pv(); ns <- session$ns
    if (!is.null(p$error)) return(tags$div(class = "alert alert-danger small py-2", p$error))
    s <- p$s
    norm <- function(x) tolower(trimws(x))
    raw_u <- unique(norm(p$units$unit[nzchar(trimws(p$units$unit))]))
    warn <- c(
      if (s$n_rows == 0) "No rows match in this cohort.",
      if (s$n_rows > 0 && identical(p$conv, "none") && !is.na(p$target) && length(setdiff(raw_u, norm(p$target))))
        sprintf("Source units differ from the target unit (%s): %s. Choose a conversion or check the items.", p$target,
                paste(setdiff(raw_u, norm(p$target)), collapse = ", ")),
      if (s$n_rows > 0 && s$n_value < s$n_rows)
        sprintf("%.0f%% of matched rows have no numeric value after conversion.", 100 * (1 - s$n_value / s$n_rows)),
      if (s$n_value > 0 && (s$n_low + s$n_high) / s$n_value > 0.05)
        sprintf("%.1f%% of values fall outside the plausible range.", 100 * (s$n_low + s$n_high) / s$n_value))
    tagList(
      lapply(warn, function(w) tags$div(class = "alert alert-warning small py-2 mb-2", bs_icon("exclamation-triangle", class = "me-1"), w)),
      layout_columns(col_widths = breakpoints(sm = 6, lg = 3), fill = FALSE,
        stat_box("Rows matched", mask_n(s$n_rows), "list-ol"),
        stat_box("Admissions", mask_n(s$n_adm), "hospital"),
        stat_box("Patients", mask_n(s$n_pat), "people"),
        stat_box("Outside limits", mask_n(s$n_low + s$n_high), "arrows-expand",
                 if (s$n_value > 0 && (s$n_low + s$n_high) / s$n_value > 0.05) "warning" else "neutral")),
      if (s$n_rows > 0) layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
        card(card_header("Distribution"), plotlyOutput(ns("pv_hist"), height = "300px")),
        tags$div(
          tags$h6("Quantiles"),
          tags$table(class = "table table-sm small mb-3", tags$tbody(tags$tr(lapply(c("P1", "P25", "Median", "P75", "P99"), tags$th)),
                                                                     tags$tr(lapply(signif(p$q, 4), tags$td)))),
          tags$h6("Source units"), DTOutput(ns("pv_units")),
          tags$h6(class = "mt-3", "Matched items"), DTOutput(ns("pv_items")))))
  })
  output$pv_hist <- renderPlotly({
    p <- pv(); req(is.null(p$error))
    validate(need(!is.null(p$hist) && nrow(p$hist) > 0, "No numeric values to plot."))
    h <- p$hist
    lim <- p$lim[!is.na(p$lim)]
    shapes <- lapply(lim, function(v) list(type = "line", xref = "x", yref = "paper", x0 = v, x1 = v, y0 = 0, y1 = 1,
                                           line = list(color = "#a53f3f", dash = "dash", width = 1.5)))
    plot_ly(h, x = ~x, y = ~n, type = "bar", width = p$width, marker = list(color = PAL[1]), hoverinfo = "text",
            text = ~sprintf("%.4g to %.4g: %s", x - p$width / 2, x + p$width / 2, mask_n(n)), textposition = "none") %>%
      layout(shapes = shapes, bargap = 0.05,
             xaxis = list(title = sprintf("Value%s (plausible range dashed)", if (!is.na(p$target)) paste0(" (", p$target, ")") else "")),
             yaxis = list(title = "Rows")) %>% plot_config()
  })
  output$pv_units <- renderDT({
    p <- pv(); req(is.null(p$error))
    datatable(p$units %>% transmute(Unit = if_else(nzchar(unit), unit, "(none)"), Rows = mask_n(n), `With value` = mask_n(n_value)),
              rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 25))
  })
  output$pv_items <- renderDT({
    p <- pv(); req(is.null(p$error))
    datatable(p$items %>% transmute(Item = item, Label = label, Rows = mask_n(n)), rownames = FALSE, class = "compact",
              options = list(dom = .dt_dom_pages, pageLength = 8))
  })

  list(rows = rows, enabled = reactive(rv$on), issues = issues, load = load)
})

# ---- page ------------------------------------------------------------------------------------------------------------
concept_builder_ui <- function(id) {
  ns <- NS(id)
  cats <- sort(unique(CB_BASE$category[!CB_BASE$category %in% c("NA", "")]))
  units <- sort(unique(CB_BASE$unit[!CB_BASE$unit %in% c("NA", "")]))
  tagList(
    layout_columns(col_widths = breakpoints(sm = 12, lg = c(4, 8)), fill = FALSE,
      card(card_header("Start"),
        radioGroupButtons(ns("mode"), NULL, c("New concept" = "new", "Edit an existing concept" = "edit"), "new", justified = TRUE),
        conditionalPanel("input.mode == 'edit'", ns = ns,
          pickerInput(ns("pick"), "Concept", list(Packaged = sort(unique(CB_BASE$concept_name))), selected = character(),
                      options = list(`live-search` = TRUE, title = "Choose a concept"))),
        uiOutput(ns("start_info"))),
      card(card_header("Definition"),
        layout_columns(col_widths = breakpoints(sm = 12, md = c(4, 8)), fill = FALSE,
          tags$div(
            conditionalPanel("input.mode == 'new'", ns = ns, textInput(ns("name"), "Name", placeholder = "e.g. heart_rate_art")),
            conditionalPanel("input.mode == 'edit'", ns = ns, tags$label(class = "form-label", "Name"), uiOutput(ns("name_ro"))),
            uiOutput(ns("name_msg"))),
          textInput(ns("desc"), "Description", width = "100%", placeholder = "e.g. heart rate, arterial line")),
        layout_columns(col_widths = breakpoints(sm = 6, md = 3), fill = FALSE,
          selectizeInput(ns("category"), "Category", c("", cats), "", options = list(create = TRUE, placeholder = "choose or type")),
          selectizeInput(ns("unit"), "Target unit", c("", units), "", options = list(create = TRUE, placeholder = "choose or type")),
          numericInput(ns("min"), "Plausible min", NA),
          numericInput(ns("max"), "Plausible max", NA)),
        uiOutput(ns("def_msg")))),
    do.call(navset_card_underline, c(list(id = ns("src_tabs"), title = "Source mappings"),
                                      lapply(names(CB_FAMS), function(f) cb_source_ui(ns(paste0("s_", f)), f)))),
    card(fillable = FALSE, card_header(class = "d-flex justify-content-between align-items-center", "Result", uiOutput(ns("status"), inline = TRUE)),
      uiOutput(ns("result_msg")),
      navset_underline(
        nav_panel("Rows", DTOutput(ns("rows"))),
        nav_panel("CSV", verbatimTextOutput(ns("csv"))),
        nav_panel("Changes", DTOutput(ns("diff")))),
      uiOutput(ns("actions"))))
}

concept_builder_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  proj_bump <- reactiveVal(0)
  built <- reactiveVal(NULL)
  loaded <- reactiveVal(NULL)          # edit mode: name, definition and rows per source as loaded

  base <- reactive({
    p <- if (!is.null(JOB_CTX$base_dir)) file.path(JOB_CTX$base_dir, "config", "rendehr_concepts_config.csv")
    if (!is.null(p) && file.exists(p)) cb_read(p) %||% CB_BASE else CB_BASE
  })
  proj_path <- reactive(if (!is.null(JOB_CTX$config) && !is.null(JOB_CTX$base_dir)) file.path(JOB_CTX$base_dir, "config", "user_concepts.csv"))
  proj <- reactive({ proj_bump(); p <- proj_path(); if (!is.null(p)) cb_read(p) })

  observe({
    pr <- proj(); b <- base()
    ch <- list(Packaged = sort(unique(b$concept_name)))
    if (!is.null(pr) && nrow(pr)) ch$`Project file` <- sort(unique(pr$concept_name))
    updatePickerInput(session, "pick", choices = ch, selected = isolate(input$pick) %||% character())
  })

  edit <- reactive(identical(input$mode, "edit"))
  name <- reactive(if (edit()) input$pick %||% "" else trimws(input$name %||% ""))
  def_all <- reactive(list(concept_name = name(), description = cb_cell(input$desc, "description"),
                           category = cb_cell(input$category), unit = cb_cell(input$unit),
                           min_value = cb_num(input$min), max_value = cb_num(input$max)))
  # edit: only the definition fields changed since loading go onto loaded rows (rows keep their own otherwise)
  def_changed <- reactive({
    d <- def_all(); l <- loaded()
    if (!edit() || is.null(l)) return(d)
    d[c("concept_name", names(d)[vapply(names(d), function(f) !identical(d[[f]], l$def[[f]]), logical(1))])]
  })
  mods <- stats::setNames(lapply(names(CB_FAMS), function(f) cb_source_server(paste0("s_", f), f, def_all, def_changed, base)), names(CB_FAMS))

  set_def <- function(d) {
    updateTextInput(session, "desc", value = if (cb_na(d$description)) "" else d$description)
    for (f in c("category", "unit")) {
      v <- if (cb_na(d[[f]])) "" else d[[f]]
      ch <- sort(unique(c(base()[[f]], v))); ch <- ch[!ch %in% c("NA", "")]
      updateSelectizeInput(session, f, choices = c("", ch), selected = v)
    }
    updateNumericInput(session, "min", value = suppressWarnings(as.numeric(d$min_value)))
    updateNumericInput(session, "max", value = suppressWarnings(as.numeric(d$max_value)))
  }
  reset <- function() {
    loaded(NULL); built(NULL)
    set_def(list(description = "", category = "", unit = "", min_value = NA, max_value = NA))
    for (m in mods) m$load(NULL, FALSE)
  }
  load_concept <- function(cn) {
    b <- base(); p <- proj()
    rows <- lapply(stats::setNames(names(CB_FAMS), names(CB_FAMS)), function(f) cb_effective(b, p, f, cn))
    all <- do.call(rbind, rows)
    first <- function(f) { v <- all[[f]][!all[[f]] %in% c("NA", "")]; if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else "NA" }
    d <- list(description = first("description"), category = first("category"), unit = first("unit"),
              min_value = first("min_value"), max_value = first("max_value"))
    d$min_value <- cb_num(suppressWarnings(as.numeric(d$min_value))); d$max_value <- cb_num(suppressWarnings(as.numeric(d$max_value)))
    d <- lapply(d, cb_cell)
    loaded(list(name = cn, def = c(list(concept_name = cn), d), rows = rows))
    built(NULL)
    set_def(d)
    for (f in names(CB_FAMS)) mods[[f]]$load(rows[[f]], nrow(rows[[f]]) > 0)
  }
  observeEvent(input$mode, if (edit()) { if (!cb_na(input$pick)) load_concept(input$pick) } else reset(), ignoreInit = TRUE)
  observeEvent(input$pick, { req(edit(), !cb_na(input$pick)); load_concept(input$pick) })

  output$name_ro <- renderUI(tags$p(class = "form-control-plaintext fw-semibold", if (nzchar(name())) name() else "—"))
  output$start_info <- renderUI({
    if (!edit()) return(tags$p(class = "small text-muted mb-0", "Draft a concept from source tables. Nothing is written until you save."))
    l <- loaded(); req(l)
    n <- vapply(l$rows, nrow, integer(1))
    p <- proj(); in_proj <- !is.null(p) && any(p$concept_name == l$name)
    others <- setdiff(unique(base()$source[base()$concept_name == l$name]), names(CB_FAMS))
    tags$div(class = "small",
      tags$div(lapply(names(n), function(f) tags$span(class = sprintf("badge rounded-pill me-1 %s", if (n[[f]]) "text-bg-secondary" else "text-bg-light"),
                                                      sprintf("%s: %d row%s", CB_FAM_LABEL[[f]], n[[f]], if (n[[f]] == 1) "" else "s")))),
      tags$div(class = "text-muted mt-1", if (in_proj) "Includes rows from the project file." else "Packaged definition."),
      if (length(others)) tags$div(class = "text-muted", sprintf("Also defined for %s (not in this explorer; left unchanged).", paste(others, collapse = ", "))))
  })

  # ---- validation
  all_names <- reactive(unique(c(base()$concept_name, proj()$concept_name)))
  name_issue <- reactive(if (edit()) (if (cb_na(input$pick)) "Choose a concept to edit.") else cb_name_issue(input$name, all_names(), TRUE))
  def_issues <- reactive({
    c(if (!is.na(input$min %||% NA) && !is.na(input$max %||% NA) && input$min > input$max) "The plausible min is above the max.",
      if (grepl("'", input$unit %||% "")) "The unit cannot contain a single quote.")
  })
  # nothing is flagged until the user has started a draft
  started <- reactive(edit() || nzchar(input$name %||% "") || any(vapply(mods, function(m) isTRUE(m$enabled()), logical(1))))
  output$name_msg <- renderUI(if (started() && (!edit() || cb_na(input$pick))) cb_feedback(name_issue()))
  output$def_msg <- renderUI(cb_feedback(def_issues()))

  # ---- draft rows
  # edit: sources whose rows are unchanged are left out, so their current definition stays in effect
  per_src <- reactive({
    out <- lapply(mods, function(m) m$rows()); out <- Filter(Negate(is.null), out)
    l <- loaded()
    if (edit() && !is.null(l)) out <- out[vapply(names(out), function(f) !identical(cb_csv_text(out[[f]]), cb_csv_text(l$rows[[f]])), logical(1))]
    out
  })
  draft <- reactive({ p <- per_src(); if (length(p)) cb_align(do.call(rbind, unname(p))) else cb_empty() })
  draft_text <- reactive(cb_csv_text(draft()))
  issues <- reactive({
    on <- vapply(mods, function(m) isTRUE(m$enabled()), logical(1))
    c(name_issue(), def_issues(),
      if (!any(on)) "Switch on at least one source under Source mappings.",
      unlist(lapply(mods, function(m) m$issues())))
  })
  status <- reactive({
    d <- draft(); if (!nrow(d)) return("empty")
    p <- proj()
    same <- !is.null(p) && identical(cb_csv_text(cb_align(p[paste(p$source, p$concept_name) %in% paste(d$source, d$concept_name), , drop = FALSE])),
                                     draft_text())
    if (same && identical(built(), draft_text())) "built" else if (same) "saved" else "draft"
  })
  output$status <- renderUI({
    s <- status()
    tags$span(class = sprintf("badge rounded-pill %s", switch(s, built = "text-bg-primary", saved = "text-bg-success", "text-bg-secondary")),
              switch(s, built = "Build started", saved = "Saved to project", empty = "No changes", "Draft"))
  })
  output$result_msg <- renderUI({
    if (!started()) return(tags$p(class = "text-muted small", "Name the concept and switch on a source to start."))
    m <- issues()
    l <- loaded()
    unchanged <- if (edit() && !is.null(l)) setdiff(names(Filter(isTRUE, lapply(mods, function(m) m$enabled()))), names(per_src())) else character()
    dedup <- edit() && !is.null(l) && any(vapply(names(per_src()), function(f) any(l$rows[[f]]$required_dedup %in% c("TRUE", "true")), logical(1)))
    tagList(
      if (length(m)) tags$div(class = "alert alert-danger small py-2", tags$ul(class = "mb-0 ps-3", lapply(m, tags$li))),
      if (length(unchanged)) tags$div(class = "small text-muted mb-2", bs_icon("info-circle", class = "me-1"),
        sprintf("Unchanged, left out: %s.", paste(CB_FAM_LABEL[unchanged], collapse = ", "))),
      if (dedup) tags$div(class = "alert alert-warning small py-2", bs_icon("exclamation-triangle", class = "me-1"),
        "Replacing this concept through the project file turns off its chart-copy deduplication for the replaced sources."))
  })
  output$rows <- renderDT({
    validate(need(nrow(draft()) > 0, if (edit()) "No changes yet." else "Switch on a source and map it to see the rows."))
    datatable(draft(), rownames = FALSE, class = "compact nowrap", options = list(dom = "t", scrollX = TRUE, pageLength = 50))
  })
  output$csv <- renderText({ validate(need(nrow(draft()) > 0, "No rows yet.")); draft_text() })
  output$diff <- renderDT({
    validate(need(edit() && !is.null(loaded()), "Changes are listed when you edit an existing concept."))
    d <- cb_diff(loaded()$rows, per_src())
    validate(need(nrow(d) > 0, "No changes yet."))
    datatable(d, rownames = FALSE, class = "compact", options = list(dom = .dt_dom_pages, pageLength = 25, scrollX = TRUE))
  })

  # ---- actions
  ok <- reactive(!length(issues()) && nrow(draft()) > 0)
  build_src <- reactive(Filter(function(s) tryCatch(src_family(s), error = function(e) "") %in% draft()$source, JOB_CTX$src %||% character()))
  output$actions <- renderUI({
    proj_ok <- !is.null(proj_path()); s <- status()
    dis <- function(b, off) if (off) tagAppendAttributes(b, disabled = NA, class = "disabled", `aria-disabled` = "true") else b
    hint <- if (!proj_ok) "No project set: choose one on the Setup page to save and build."
            else if (!ok()) NULL
            else if (s == "draft") sprintf("Saves to %s. Build after saving.", proj_path())
            else if (!length(build_src())) "The project has no source of these families to build."
            else sprintf("Builds with run_concepts() for %s.", paste(build_src(), collapse = ", "))
    tagList(
      button_row(
        dis(actionButton(ns("save"), "Save to project", icon = icon("floppy-disk"), class = "btn-primary"), !proj_ok || !ok() || s != "draft"),
        dis(downloadButton(ns("dl"), "Download CSV", class = "btn-outline-secondary"), !ok()),
        dis(actionButton(ns("build"), "Build concept", icon = icon("play"), class = "btn-outline-primary"),
            !proj_ok || !ok() || !s %in% c("saved", "built") || !length(build_src()))),
      if (!is.null(hint)) tags$p(class = "small text-muted mt-2 mb-0", hint))
  })
  do_save <- function() {
    removeModal()
    path <- proj_path(); req(path, ok())
    res <- tryCatch({ cb_write(cb_merge(proj(), draft()), path); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(res)) return(showNotification(res, type = "error"))
    proj_bump(proj_bump() + 1)
    showNotification(sprintf("Saved %d row%s to %s.", nrow(draft()), if (nrow(draft()) == 1) "" else "s", basename(path)), type = "message", duration = 5)
  }
  observeEvent(input$save, {
    req(ok(), proj_path())
    p <- proj(); d <- draft()
    hit <- if (!is.null(p)) p[paste(p$source, p$concept_name) %in% paste(d$source, d$concept_name), , drop = FALSE] else NULL
    if (is.null(hit) || !nrow(hit)) return(do_save())
    n <- hit %>% count(source, concept_name)
    showModal(modalDialog(title = "Replace rows in the project file?",
      tags$p(sprintf("%s already defines:", basename(proj_path()))),
      tags$ul(lapply(seq_len(nrow(n)), function(i) tags$li(sprintf("%s for %s (%d row%s)", n$concept_name[i], n$source[i], n$n[i], if (n$n[i] == 1) "" else "s")))),
      tags$p(sprintf("They are replaced by the %d draft row%s; other rows are kept.", nrow(d), if (nrow(d) == 1) "" else "s")),
      footer = tagList(modalButton("Cancel"), actionButton(ns("save_yes"), "Replace", class = "btn-primary"))))
  })
  observeEvent(input$save_yes, do_save())
  output$dl <- downloadHandler(filename = function() sprintf("%s_concept.csv", name() %||% "concept"),
                               content = function(file) writeLines(draft_text(), file))
  build_lines <- reactive(c(
    sprintf("base_dir <- %s", deparse(JOB_CTX$base_dir)), sprintf("db_name <- %s", deparse(JOB_CTX$db_name)),
    sprintf("user_concepts <- %s", deparse(proj_path())),
    sprintf("run_concepts(src_name = %s, db_name = db_name, base_dir = base_dir,\n    concepts = concept_options(user_concepts = user_concepts, layers = \"ts\", concept_name = %s))",
            vapply(build_src(), deparse, character(1)), deparse(name()))))
  observeEvent(input$build, {
    req(status() %in% c("saved", "built"), length(build_src()))
    showModal(modalDialog(title = sprintf("Build %s?", name()),
      tags$p("Runs as a background task; follow it under Manage > Setup > Background tasks."),
      tags$pre(class = "small", paste(build_lines(), collapse = "\n")),
      tags$p(class = "small text-muted", "Rebuild the harmonized tables afterwards to see the concept in the explorer."),
      footer = tagList(modalButton("Cancel"), actionButton(ns("build_yes"), "Build", class = "btn-primary"))))
  })
  observeEvent(input$build_yes, {
    removeModal()
    req(status() %in% c("saved", "built"))
    job_start(sprintf("Build concept %s", name()), build_lines(), wd = JOB_CTX$base_dir, config = JOB_CTX$config)
    built(draft_text())
    showNotification(sprintf("Building %s; follow it under Background tasks.", name()), type = "message", duration = 5)
  })
})

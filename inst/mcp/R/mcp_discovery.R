# Local synonym resources and bounded source dictionary discovery. No APIs.
.rh_asset <- function(...) {
  root <- getOption("rendehR.mcp_root", "")
  if (nzchar(root)) return(file.path(root, "inst", ...))
  # Bundled in rendehR under inst/mcp (installed as mcp/inst/...).
  system.file("mcp", "inst", ..., package = "rendehR")
}
# The MCP version, from the bundle's VERSION file.
.rh_version <- function() {
  root <- getOption("rendehR.mcp_root", "")
  path <- if (nzchar(root)) file.path(root, "VERSION") else system.file("mcp", "VERSION", package = "rendehR")
  if (nzchar(path) && file.exists(path)) trimws(readLines(path, n = 1L, warn = FALSE)) else "unknown"
}
# Short terms (abbreviations such as "hr" or "map") match whole words only:
# as substrings they would hit unrelated labels ("ml/hr", "mapping").
.rh_short_term <- function(term) nchar(term) <= 4L
.rh_word_regex <- function(term)
  paste0("(^|[^a-z0-9/])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", term), "($|[^a-z0-9/])")
.rh_term_matches <- function(term, label) {
  if (.rh_short_term(term)) grepl(.rh_word_regex(term), label) else grepl(term, label, fixed = TRUE)
}
.rh_synonyms <- function(include_cached_topics = FALSE) {
  files <- c(.rh_asset("synonyms", "base.json"),
    if (include_cached_topics) list.files(.rh_asset("synonyms", "user"), pattern = "[.]json$", full.names = TRUE))
  merged <- list()
  for (path in files) {
    if (!file.exists(path)) next
    x <- jsonlite::read_json(path, simplifyVector = FALSE)
    if (is.null(names(x))) stop("Synonym file must contain a named object: ", basename(path))
    for (name in names(x)) {
      values <- unlist(x[[name]], use.names = FALSE)
      if (!is.character(values)) stop("Invalid aliases in local synonym file: ", basename(path))
      key <- tolower(trimws(name))
      merged[[key]] <- unique(c(merged[[key]], tolower(trimws(values))))
    }
  }
  merged
}
.rh_expand <- function(term, expand = TRUE, include_cached_topics = FALSE) {
  terms <- unique(tolower(trimws(unlist(term, use.names = FALSE))))
  if (!length(terms) || any(!nzchar(terms))) stop("Provide nonempty search terms.")
  dictionary <- if (expand) .rh_synonyms(include_cached_topics) else list()
  expansions <- lapply(terms, function(term) {
    keys <- names(dictionary)[vapply(names(dictionary), function(key)
      term == key || term %in% dictionary[[key]], logical(1))]
    unique(c(term, keys, unlist(dictionary[keys], use.names = FALSE)))
  })
  names(expansions) <- terms
  all <- unique(unlist(expansions, use.names = FALSE))
  if (length(all) > 100L) stop("More than 100 expanded search terms; narrow the request or disable expand_synonyms.")
  list(input_terms = as.list(terms), search_terms = as.list(all), expansions = lapply(expansions, as.list),
       note = "Local aliases are search hints, not clinical equivalence; ambiguous abbreviations can broaden results.")
}
.rh_find <- function(a, con) {
  expanded <- .rh_expand(a$term, a$expand_synonyms %||% TRUE, a$include_cached_topics %||% FALSE)
  terms <- unlist(expanded$search_terms)
  family <- sub("_demo$", "", a$src)
  maps <- jsonlite::read_json(.rh_asset("config", "mcp_dict_tables.json"))[[family]]$tables
  if (is.null(maps)) stop("Unsupported source family.")
  selected <- if (is.null(a$tables)) names(maps) else unique(unlist(a$tables))
  if (length(setdiff(selected, names(maps)))) stop("Unknown dictionary table for this source. Available: ", paste(names(maps), collapse = ", "))
  schema <- .rh_call("src_schema_names", src_name = a$src)$raw
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  count <- a$limit %||% 20L
  rows <- list(); failures <- list(); truncated <- character()
  for (table in selected) {
    map <- maps[[table]]
    found <- tryCatch({
      fields <- DBI::dbListFields(con, DBI::Id(schema = schema, table = table))
      if (!all(c(map$id, map$label) %in% fields)) stop("Required identifier or label column is missing")
      col <- function(field, alias) paste0(if (!is.null(field) && field %in% fields)
        paste0("CAST(", q(field), " AS TEXT)") else "NULL::text", " AS ", q(alias))
      projection <- c(col(map$id, "candidate_id"), col(map$label, "label"), col(map$category, "category"),
        col(map$unit, "unit"), col(map$specimen, "specimen"),
        if (family == "mimic_iii" && grepl("^d_icd_", table)) "'9'::text AS code_version" else col(map$code_version, "code_version"))
      label_sql <- paste0("LOWER(CAST(", q(map$label), " AS TEXT))")
      predicates <- vapply(terms, function(term) if (.rh_short_term(term))
        paste0(label_sql, " ~ ", DBI::dbQuoteString(con, .rh_word_regex(term)))
        else paste0("POSITION(", DBI::dbQuoteString(con, term), " IN ", label_sql, ") > 0"), character(1))
      sql <- paste0("SELECT DISTINCT ", paste(projection, collapse = ", "), " FROM ", q(schema), ".", q(table),
        " WHERE ", paste(predicates, collapse = " OR "), " ORDER BY candidate_id, label, category, unit, specimen, code_version LIMIT ", count + 1L)
      DBI::dbGetQuery(con, sql)
    }, error = function(e) {failures[[table]] <<- "Table unavailable, incompatible columns, permission failure, or query timeout; inspect locally."; NULL})
    if (is.null(found)) next
    if (nrow(found) > count) truncated <- c(truncated, table)
    found <- utils::head(found, count)
    if (nrow(found)) {
      found$dictionary <- table
      found$matched_terms <- vapply(tolower(found$label), function(label)
        paste(terms[vapply(terms, function(term) .rh_term_matches(term, label), logical(1))], collapse = " | "), character(1))
      rows[[table]] <- found
    }
  }
  if (length(failures) == length(selected)) stop("All selected dictionary queries failed; inspect source preparation and permissions.")
  # Round-robin order avoids consuming the global limit in the first dictionary.
  if (length(rows)) {
    combined <- do.call(rbind, lapply(rows, function(d) {d$.rank <- seq_len(nrow(d)); d}))
    combined <- combined[order(combined$.rank, match(combined$dictionary, selected)), , drop = FALSE]
    limited <- nrow(combined) > count || length(truncated) > 0L
    candidates <- utils::head(combined, count); candidates$.rank <- NULL
    rownames(candidates) <- NULL
  } else {candidates <- data.frame(); limited <- FALSE}
  list(candidates = candidates, search = expanded, searched_tables = as.list(selected), failed_tables = failures,
       limit_reached = limited, note = "Units are returned only from declared unit columns; specimen fluid is separate. Narrow terms or select tables when results are limited. eICU searches distinct event names, not central dictionaries.")
}

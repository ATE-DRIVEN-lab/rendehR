# Canonical acquisition/table catalog helpers.

.ehr_source_catalog_path <- function() {
  system.file("extdata", "rendehr-data-source.json", package = "rendehR")
}

.ehr_load_source_catalog <- function(path = NULL) {
  path <- path %||% .ehr_source_catalog_path()
  if (!length(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("Canonical source catalog not found.", call. = FALSE)
  }
  cfg <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  names <- vapply(cfg, function(x) x$name %||% "", character(1L))
  if (!length(cfg) || any(!nzchar(names)) || anyDuplicated(names)) {
    stop("Canonical source catalog has missing or duplicate source names.",
         call. = FALSE)
  }
  stats::setNames(cfg, names)
}

.ehr_catalog_source <- function(src_name, path = NULL) {
  cfg <- .ehr_load_source_catalog(path)
  if (!src_name %in% names(cfg)) {
    stop("Source '", src_name, "' is not in the canonical source catalog.",
         call. = FALSE)
  }
  cfg[[src_name]]
}

.ehr_catalog_projection_plan <- function(actual_names, table_cfg) {
  catalog_names <- names(table_cfg$cols %||% list())
  actual_names <- as.character(actual_names)
  raw_names <- vapply(
    table_cfg$cols %||% list(),
    function(x) as.character(x$name %||% ""),
    character(1L)
  )
  if (any(!nzchar(raw_names))) {
    stop("Catalog column entries must declare a non-empty raw `name`.",
         call. = FALSE)
  }
  catalog_key <- tolower(catalog_names)
  raw_key <- tolower(raw_names)
  actual_key <- tolower(actual_names)
  if (anyDuplicated(catalog_key)) {
    stop("Catalog contains case-insensitive duplicate columns.", call. = FALSE)
  }
  if (anyDuplicated(raw_key)) {
    stop("Catalog contains case-insensitive duplicate raw column names.",
         call. = FALSE)
  }
  if (anyDuplicated(actual_key)) {
    stop("Actual input contains case-insensitive duplicate columns.", call. = FALSE)
  }
  pos <- match(raw_key, actual_key)
  missing <- catalog_names[is.na(pos)]
  unexpected <- actual_names[!actual_key %in% raw_key]
  matched_actual <- actual_names[pos[!is.na(pos)]]
  matched_catalog <- catalog_names[!is.na(pos)]
  specs <- vapply(table_cfg$cols, function(x) x$spec %||% "col_character",
                  character(1L))
  list(
    retained = catalog_names,
    raw_names = stats::setNames(raw_names, catalog_names),
    types = unname(specs),
    matched_actual = matched_actual,
    matched_catalog = matched_catalog,
    missing_catalog = missing,
    unexpected_actual = unexpected,
    case_or_order_only = !length(missing) && !length(unexpected) &&
      !identical(actual_names, raw_names)
  )
}

.ehr_catalog_validation_report <- function(actual_names, table_cfg,
                                           conversion_failures = NULL) {
  plan <- .ehr_catalog_projection_plan(actual_names, table_cfg)
  failures <- conversion_failures %||% stats::setNames(
    integer(length(plan$retained)), plan$retained
  )
  failures <- as.integer(failures[match(plan$retained, names(failures))])
  failures[is.na(failures)] <- 0L
  list(
    plan = plan,
    conversion_failures = stats::setNames(failures, plan$retained),
    should_stop = FALSE
  )
}

.ehr_catalog_table_contract <- function(table_cfg, ladder = NULL,
                                        table_name = NULL) {
  role <- tolower(as.character(table_cfg$cohort_role %||% ""))
  link <- table_cfg$cohort_link_key
  label <- if (is.null(table_name)) "Catalog table" else
    paste0("Catalog table '", table_name, "'")

  if (!role %in% c("clinical", "reference")) {
    stop(label, " must declare cohort_role = 'clinical' or 'reference'.",
         call. = FALSE)
  }
  if (identical(role, "reference")) {
    if (!is.null(link) && length(link) && !is.na(link) && nzchar(link)) {
      stop(label, " is a reference table and must not declare cohort_link_key.",
           call. = FALSE)
    }
    return(list(role = role, link_key = NULL))
  }

  if (is.null(link) || length(link) != 1L || is.na(link) || !nzchar(link)) {
    stop(label, " is clinical and must declare cohort_link_key.",
         call. = FALSE)
  }
  link <- as.character(link)
  if (!link %in% names(table_cfg$cols %||% list())) {
    stop(label, " links by '", link,
         "', which is not a canonical configured column.", call. = FALSE)
  }
  if (!is.null(ladder) && !link %in% ladder) {
    stop(label, " links by '", link,
         "', which is not in the source hierarchy (",
         paste(ladder, collapse = ", "), ").", call. = FALSE)
  }
  list(role = role, link_key = link)
}

.ehr_validate_catalog_linkage <- function(source_cfg, ladder) {
  tables <- source_cfg$tables %||% list()
  if (!length(tables)) {
    stop("Source catalog contains no tables.", call. = FALSE)
  }
  out <- lapply(names(tables), function(table) {
    .ehr_catalog_table_contract(tables[[table]], ladder, table)
  })
  stats::setNames(out, names(tables))
}

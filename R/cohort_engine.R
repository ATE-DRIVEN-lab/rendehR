# Canonical cohort selection and materialization engine.

.ehr_duckdb_type_map <- c(
  col_integer = "BIGINT",
  col_big_integer = "BIGINT",
  col_double = "DOUBLE",
  col_logical = "BOOLEAN",
  col_datetime = "TIMESTAMP",
  col_character = "VARCHAR"
)

.ehr_spec_duckdb_type <- function(spec) {
  type <- unname(.ehr_duckdb_type_map[as.character(spec %||% "col_character")])
  if (is.na(type)) "VARCHAR" else type
}

.ehr_normalize_id_values <- function(values, arg_name = "selection") {
  if (is.data.frame(values) || is.list(values) && !is.atomic(values)) {
    stop(arg_name, " must resolve to one atomic identifier vector.",
         call. = FALSE)
  }
  values <- as.character(values)
  if (!length(values)) {
    stop(arg_name, " contains no identifiers.", call. = FALSE)
  }
  invalid <- is.na(values) | !nzchar(trimws(values))
  if (any(invalid)) {
    stop(arg_name, " contains ", sum(invalid),
         " missing or blank identifier value(s); identifiers are not shown.",
         call. = FALSE)
  }
  unique(trimws(values))
}

.ehr_selection_plan <- function(source, route, key, evidence_key = key,
                                requested_scope = NULL,
                                effective_scope = NULL,
                                values = character(),
                                allow_unmatched_ids = FALSE,
                                metadata = list()) {
  structure(
    list(
      source = source,
      route = route,
      selection_key = key,
      selection_grain = key,
      evidence_key = evidence_key,
      evidence_grain = evidence_key,
      requested_scope = requested_scope,
      effective_scope = effective_scope,
      values = as.character(values),
      requested_count = length(unique(values)),
      matched_count = NA_integer_,
      unmatched_count = NA_integer_,
      allow_unmatched_ids = isTRUE(allow_unmatched_ids),
      metadata = metadata
    ),
    class = "ehr_selection_plan"
  )
}

.ehr_validate_selection_plan <- function(plan, ladder) {
  if (!inherits(plan, "ehr_selection_plan")) {
    stop("Internal selection result is not an ehr_selection_plan.",
         call. = FALSE)
  }
  if (!plan$selection_key %in% ladder) {
    stop("Selection key '", plan$selection_key,
         "' is not in the configured hierarchy (",
         paste(ladder, collapse = ", "), ").", call. = FALSE)
  }
  if (!plan$evidence_key %in% ladder) {
    stop("Evidence key '", plan$evidence_key,
         "' is not in the configured hierarchy (",
         paste(ladder, collapse = ", "), ").", call. = FALSE)
  }
  if (!identical(plan$route, "no_filter") && !length(plan$values)) {
    stop("Cohort selection returned no identifiers.", call. = FALSE)
  }
  plan
}

.ehr_infer_direct_id_key <- function(path, ladder, key = NULL) {
  if (!is.null(key)) {
    if (length(key) != 1L || is.na(key) || !nzchar(key)) {
      stop("`key` must be one non-empty hierarchy column.", call. = FALSE)
    }
    return(as.character(key))
  }

  lower <- tolower(path)
  cols <- if (grepl("\\.rds$", lower)) {
    object <- readRDS(path)
    if (is.data.frame(object)) names(object) else character()
  } else if (grepl("\\.csv(\\.gz)?$", lower)) {
    names(readr::read_csv(
      path, n_max = 0L,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    ))
  } else if (grepl("\\.parquet$", lower)) {
    names(arrow::open_dataset(path, format = "parquet"))
  } else {
    character()
  }

  hits <- intersect(ladder, cols)
  if (length(hits) == 1L) return(hits[[1L]])
  if (length(hits) > 1L) {
    stop("The direct-ID input contains multiple hierarchy columns; pass `key` ",
         "explicitly. Candidate columns: ", paste(hits, collapse = ", "),
         ".", call. = FALSE)
  }
  stop("Could not infer a hierarchy key from the direct-ID input. Pass `key`; ",
       "configured hierarchy columns are: ", paste(ladder, collapse = ", "),
       ".", call. = FALSE)
}

.ehr_atomic_write_ids <- function(values, key, path) {
  if (is.null(path)) return(invisible(NULL))
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`out_csv` must be one non-empty path.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(pattern = paste0(".", basename(path), ".stage-"),
                    tmpdir = dirname(path))
  on.exit(if (file.exists(stage)) unlink(stage, force = TRUE), add = TRUE)
  readr::write_csv(
    stats::setNames(data.frame(values, stringsAsFactors = FALSE), key),
    stage
  )
  if (file.exists(path) && !file.remove(path)) {
    stop("Could not replace existing ID export.", call. = FALSE)
  }
  if (!file.rename(stage, path)) {
    stop("Could not publish the ID export atomically.", call. = FALSE)
  }
  invisible(path)
}

.ehr_table_file_map <- function(cfg, paths) {
  out <- lapply(cfg$tables, function(table_cfg) {
    file.path(paths$raw_dir, unlist(table_cfg$files, use.names = FALSE))
  })
  stats::setNames(out, names(cfg$tables))
}

.ehr_projection_sql <- function(con, table_cfg, actual_names,
                                alias = "src") {
  plan <- .ehr_catalog_projection_plan(actual_names, table_cfg)
  alias_prefix <- paste0(alias, ".")
  pieces <- vapply(seq_along(plan$retained), function(i) {
    canonical <- plan$retained[[i]]
    raw <- unname(plan$raw_names[[canonical]])
    type <- .ehr_spec_duckdb_type(plan$types[[i]])
    pos <- match(tolower(raw), tolower(actual_names))
    output <- as.character(DBI::dbQuoteIdentifier(con, canonical))
    if (is.na(pos)) {
      sprintf("CAST(NULL AS %s) AS %s", type, output)
    } else {
      input <- paste0(
        alias_prefix,
        as.character(DBI::dbQuoteIdentifier(con, actual_names[[pos]]))
      )
      sprintf("TRY_CAST(%s AS %s) AS %s", input, type, output)
    }
  }, character(1L))
  paste(pieces, collapse = ", ")
}

.ehr_projection_report <- function(con, table, table_cfg, headers,
                                   raw_view, chunk_rows = NULL,
                                   bucket_raw_col = NULL,
                                   show_progress = TRUE) {
  plan <- .ehr_catalog_projection_plan(
    unique(unlist(headers, use.names = FALSE)), table_cfg
  )
  specs <- lapply(seq_along(plan$retained), function(i) {
    canonical <- plan$retained[[i]]
    raw <- unname(plan$raw_names[[canonical]])
    type <- .ehr_spec_duckdb_type(plan$types[[i]])
    present <- vapply(headers, function(x)
      tolower(raw) %in% tolower(x), logical(1L))
    actual <- unique(unlist(lapply(headers, function(x) {
      pos <- match(tolower(raw), tolower(x))
      if (is.na(pos)) character() else x[[pos]]
    }), use.names = FALSE))
    list(
      canonical = canonical,
      raw = raw,
      type = type,
      present = present,
      actual = actual,
      alias = paste0("conversion_failures_", i)
    )
  })

  failures <- numeric(length(specs))
  present_idx <- which(vapply(
    specs, function(spec) length(spec$actual) > 0L, logical(1L)
  ))
  if (length(present_idx)) {
    aggregates <- vapply(present_idx, function(i) {
      spec <- specs[[i]]
      rawq <- as.character(DBI::dbQuoteIdentifier(con, spec$actual[[1L]]))
      aliasq <- as.character(DBI::dbQuoteIdentifier(con, spec$alias))
      sprintf(
        paste0(
          "COALESCE(SUM(CASE WHEN %s IS NOT NULL ",
          "AND trim(CAST(%s AS VARCHAR)) <> '' ",
          "AND TRY_CAST(%s AS %s) IS NULL THEN 1 ELSE 0 END), 0) AS %s"
        ),
        rawq, rawq, rawq, spec$type, aliasq
      )
    }, character(1L))
    ref <- as.character(DBI::dbQuoteIdentifier(con, raw_view))
    select_sql <- sprintf(
      "SELECT %s FROM %s", paste(aggregates, collapse = ", "), ref
    )
    aliases <- vapply(present_idx, function(i) specs[[i]]$alias, character(1L))

    n_chunks <- 1L
    num_rows <- table_cfg$num_rows
    if (!is.null(chunk_rows) && !is.null(bucket_raw_col) && !is.null(num_rows)) {
      n_chunks <- max(1L, ceiling(num_rows / chunk_rows))
      if (show_progress) {
        message(
          table, ": ", format(num_rows, big.mark = ","),
          " configured raw row(s) -> ",
          if (n_chunks > 1L) paste0(n_chunks, " chunk(s)") else "single statement"
        )
      }
    }
    if (n_chunks <= 1L) {
      if (show_progress) message(table, ": running validation query (single statement)...")
      aggregate_result <- DBI::dbGetQuery(con, select_sql)
      failures[present_idx] <- vapply(aliases, function(a) {
        as.numeric(aggregate_result[[a]][[1L]])
      }, numeric(1L))
    } else {
      # raw_view is a lazy view over read_csv() - without materializing it
      # first, each chunk's WHERE-filtered query below forces DuckDB to
      # re-decompress and re-scan the entire source file from scratch (e.g.
      # 67 chunks over full CHARTEVENTS meant 67 full re-reads of the same
      # 4GB gzip file for what should be one pass). Materialize once, using
      # only the columns these aggregates and the bucket predicate actually
      # need, and point every chunk's query at that instead.
      needed_cols <- unique(c(
        vapply(specs[present_idx], function(spec) spec$actual[[1L]], character(1L)),
        bucket_raw_col
      ))
      mat_tbl <- as.character(DBI::dbQuoteIdentifier(
        con, paste0("_projection_report_", table)
      ))
      DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", mat_tbl))
      DBI::dbExecute(con, sprintf(
        "CREATE TEMP TABLE %s AS SELECT %s FROM %s",
        mat_tbl,
        paste(as.character(DBI::dbQuoteIdentifier(con, needed_cols)), collapse = ", "),
        ref
      ))
      select_sql <- sprintf("SELECT %s FROM %s", paste(aggregates, collapse = ", "), mat_tbl)

      bucketq <- as.character(DBI::dbQuoteIdentifier(con, bucket_raw_col))
      totals <- stats::setNames(numeric(length(aliases)), aliases)
      for (bucket in 0:(n_chunks - 1L)) {
        if (show_progress) {
          message(table, ": validating chunk ", bucket + 1L, "/", n_chunks, "...")
        }
        pred <- sprintf(
          "hash(CAST(%s AS VARCHAR)) %% %d = %d", bucketq, n_chunks, bucket
        )
        chunk_result <- DBI::dbGetQuery(con, paste(select_sql, "WHERE", pred))
        totals[aliases] <- totals[aliases] + vapply(aliases, function(a) {
          as.numeric(chunk_result[[a]][[1L]])
        }, numeric(1L))
      }
      failures[present_idx] <- unname(totals[aliases])
      DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", mat_tbl))
    }
  }

  rows <- lapply(seq_along(specs), function(i) {
    spec <- specs[[i]]
    data.frame(
      table = table,
      column = spec$canonical,
      raw_name = spec$raw,
      target_type = spec$type,
      present_in_all_files = all(spec$present),
      missing_file_count = sum(!spec$present),
      conversion_failures = failures[[i]],
      status = if (all(spec$present)) "configured" else "typed_missing",
      stringsAsFactors = FALSE
    )
  })

  extras <- unique(unlist(lapply(headers, function(header) {
    header[!tolower(header) %in% tolower(unname(plan$raw_names))]
  }), use.names = FALSE))
  if (length(extras)) {
    rows <- c(rows, lapply(extras, function(extra) data.frame(
      table = table,
      column = NA_character_,
      raw_name = extra,
      target_type = NA_character_,
      present_in_all_files = NA,
      missing_file_count = NA_integer_,
      conversion_failures = NA_real_,
      status = "excluded_extra",
      stringsAsFactors = FALSE
    )))
  }
  do.call(rbind, rows)
}

.ehr_assert_required_projection <- function(report, table_cfg, ladder, table) {
  required <- intersect(ladder, names(table_cfg$cols %||% list()))
  link <- table_cfg$cohort_link_key
  if (!is.null(link) && length(link) && !is.na(link) && nzchar(link)) {
    required <- unique(c(required, as.character(link)))
  }
  required_rows <- !is.na(report$column) & report$column %in% required
  missing <- report$column[
    required_rows & report$status == "typed_missing"
  ]
  failures <- report[
    required_rows & report$conversion_failures > 0,
    c("column", "conversion_failures"), drop = FALSE
  ]
  if (length(missing)) {
    stop("Table '", table, "' is missing required hierarchy/linkage ",
         "column(s): ", paste(unique(missing), collapse = ", "), ".",
         call. = FALSE)
  }
  if (nrow(failures)) {
    stop("Table '", table, "' has nonblank values that cannot be converted ",
         "in required hierarchy/linkage column(s): ",
         paste(unique(failures$column), collapse = ", "),
         ". Identifier values are not shown.", call. = FALSE)
  }
  invisible(TRUE)
}

.ehr_prepare_source_views <- function(con, cfg, paths, ladder,
                                      requested_tables = NULL,
                                      table_chunk_rows = NULL,
                                      validate_columns = FALSE,
                                      ignore_errors = TRUE,
                                      store_rejects = TRUE,
                                      use_parquet_cache = TRUE,
                                      show_progress = TRUE) {
  contracts <- .ehr_validate_catalog_linkage(cfg, ladder)
  file_map <- .ehr_table_file_map(cfg, paths)
  present <- vapply(file_map, function(files) all(file.exists(files)),
                    logical(1L))
  if (!is.null(requested_tables)) {
    unknown <- setdiff(requested_tables, names(file_map))
    if (length(unknown)) {
      stop("Tables not in the source catalog: ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    absent <- requested_tables[!present[requested_tables]]
    if (length(absent)) {
      stop("Raw input is missing for requested table(s): ",
           paste(absent, collapse = ", "), ".", call. = FALSE)
    }
  }
  file_map <- file_map[present]
  if (!length(file_map)) {
    stop("No configured raw source files are available under ",
         paths$raw_dir, ".", call. = FALSE)
  }

  reports <- list()
  cache_used <- stats::setNames(logical(0), character(0))
  for (table in names(file_map)) {
    files <- file_map[[table]]

    # A table whose raw CSV(.gz) has already been cached as parquet (via
    # cache_raw_parquet_duckdb()) is read from that parquet instead - same
    # column names/types, drop-in for the CSV read below. `headers` is kept
    # as a list-of-one (the cache's merged column set) so every downstream
    # consumer below (linkage check, validate_columns reporting) keeps
    # working unchanged regardless of which branch ran.
    cache_file <- file.path(paths$raw_parquet_cache_dir, paste0(table, ".parquet"))
    cache_ok <- isTRUE(use_parquet_cache) && parquet_has_rows(cache_file)
    cache_fresh <- cache_ok &&
      .ehr_cache_entry_fresh(paths$raw_parquet_cache_dir, table, files)
    if (cache_ok && !cache_fresh && show_progress) {
      message(table, ": cached parquet no longer matches its source file(s) ",
              "(size/mtime changed, or no manifest entry) - reading raw CSV instead.")
    }
    if (cache_fresh) {
      cache_used[[table]] <- TRUE
      actual <- DBI::dbGetQuery(con, sprintf(
        "DESCRIBE SELECT * FROM read_parquet(%s);",
        DBI::dbQuoteString(con, cache_file)
      ))$column_name
      headers <- list(actual)
      read_expr <- sprintf("read_parquet(%s)", DBI::dbQuoteString(con, cache_file))
    } else {
      cache_used[[table]] <- FALSE
      headers <- lapply(files, .csv_header_cols)
      if (any(!lengths(headers))) {
        stop("Could not read a raw header for table '", table, "'.",
             call. = FALSE)
      }
      actual <- unique(unlist(headers, use.names = FALSE))
      read_expr <- .duckdb_csv_read_expr(con, files, headers = headers,
                                         ignore_errors = ignore_errors,
                                         store_rejects = store_rejects)
    }
    if (anyDuplicated(tolower(actual))) {
      stop("Raw files for table '", table,
           "' contain case-insensitive duplicate column names.",
           call. = FALSE)
    }
    table_cfg <- cfg$tables[[table]]
    contract <- contracts[[table]]
    raw_link <- NULL
    if (identical(contract$role, "clinical")) {
      raw_link <- table_cfg$cols[[contract$link_key]]$name
      link_present <- vapply(
        headers, function(x) tolower(raw_link) %in% tolower(x), logical(1L)
      )
      if (!all(link_present)) {
        stop("Clinical table '", table, "' is missing its configured linkage ",
             "column in ", sum(!link_present), " raw file(s).", call. = FALSE)
      }
    }

    raw_view <- paste0(".__ehr_raw_", table)
    tryCatch(DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM %s AS src",
      DBI::dbQuoteIdentifier(con, raw_view), read_expr
    )), error = function(e) {
      # DuckDB cannot work out the file's CSV layout: a malformed line (such
      # as an unclosed quote) or a damaged file. Reading on regardless could
      # silently swallow rows, so stop and say where.
      if (!grepl("sniff", conditionMessage(e), ignore.case = TRUE)) stop(e)
      stop("Raw file(s) for table '", table, "' could not be parsed as CSV (",
           paste(basename(file_map[[table]]), collapse = ", "), "): a malformed line or a damaged ",
           "file. Check them against the release checksums (download_check_data()) or ",
           "re-download the source. DuckDB: ", strsplit(conditionMessage(e), "\n")[[1]][1],
           call. = FALSE)
    })
    projection <- .ehr_projection_sql(con, table_cfg, actual, alias = "src")
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT %s FROM %s AS src",
      DBI::dbQuoteIdentifier(con, table), projection,
      DBI::dbQuoteIdentifier(con, raw_view)
    ))
    if (isTRUE(validate_columns) &&
        (is.null(requested_tables) || table %in% requested_tables)) {
      if (show_progress) message(table, ": validating column types...")
      reports[[table]] <- .ehr_projection_report(
        con, table, table_cfg, headers, raw_view,
        chunk_rows = table_chunk_rows, bucket_raw_col = raw_link,
        show_progress = show_progress
      )
      .ehr_assert_required_projection(
        reports[[table]], table_cfg, ladder, table
      )
    }
  }

  empty_report <- data.frame(
    table = character(), column = character(), raw_name = character(),
    target_type = character(), present_in_all_files = logical(),
    missing_file_count = integer(), conversion_failures = numeric(),
    status = character(), stringsAsFactors = FALSE
  )
  list(
    file_map = file_map,
    contracts = contracts,
    cache_used = cache_used,
    projection_report = if (length(reports)) do.call(rbind, reports) else empty_report
  )
}

.ehr_read_icd_filter_file <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path)) {
    stop("The ICD filter file does not exist.", call. = FALSE)
  }
  filter <- if (grepl("\\.rds$", tolower(path))) {
    readRDS(path)
  } else {
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      name_repair = "unique_quiet"
    )
  }
  if (!is.data.frame(filter) || !"icd_code" %in% names(filter)) {
    stop("The ICD filter must contain `icd_code`.", call. = FALSE)
  }
  filter
}

# The ICD filter as normalized (icd_code, icd_version) keys for the matching
# join. Codes are trimmed, upper-cased and stripped of dots, exactly as the
# source tokens are. Version rules (D-3):
#   mimic_iv, eicu : icd_version is required and validated.
#   mimic_iii      : ICD-9 is assumed only when the column is absent; when it
#                    is present, values are validated and ICD-10 rows are
#                    excluded (MIMIC-III is ICD-9 only), with a message.
#   other families : version validated if present, otherwise NA; their
#                    declarative selector matches on code only.
.ehr_icd_filter_keys <- function(path, src_name) {
  filter <- .ehr_read_icd_filter_file(path)
  codes <- .ehr_normalize_id_values(filter$icd_code, "ICD filter")
  fam <- src_family(src_name)
  raw_codes <- trimws(as.character(filter$icd_code))
  has_version <- "icd_version" %in% names(filter)

  if (!has_version && fam %in% c("mimic_iv", "eicu")) {
    stop("The ICD filter for ", src_name, " must contain an `icd_version` ",
         "column (9 or 10): ", fam, " diagnosis codes can be ICD-9 or ",
         "ICD-10, and the same code string can mean different diagnoses in ",
         "each.", call. = FALSE)
  }
  version <- if (has_version) .icd_version_token(filter$icd_version,
                                                   "ICD filter icd_version")
             else if (identical(fam, "mimic_iii")) rep("9", nrow(filter))
             else rep(NA_character_, nrow(filter))

  keys <- unique(data.frame(
    icd_code = toupper(gsub(".", "", raw_codes, fixed = TRUE)),
    icd_version = version, stringsAsFactors = FALSE))

  if (identical(fam, "mimic_iii") && any(keys$icd_version == "10")) {
    message("ICD filter: excluded ", sum(keys$icd_version == "10"),
            " ICD-10 code(s); MIMIC-III diagnoses are ICD-9 only.")
    keys <- keys[keys$icd_version == "9", , drop = FALSE]
  }
  if (!nrow(keys)) {
    stop("The ICD filter has no codes usable for ", src_name, ".",
         call. = FALSE)
  }
  keys
}

# Full ICD filter content for publishing as a d_<cohort>_icds reference
# table: every real column the file declares (long_title, categories, etc.),
# not just icd_code - distinct from .ehr_icd_filter_keys()'s normalized
# (icd_code, icd_version) keys used for the raw-table matching join. Drops
# blank/auto-named index columns left over from CSV export (e.g. readr's
# "...1"). When the file declares a version it is normalized with
# .icd_version_token() - the same normalizer selection and annotation use - and
# rows are deduplicated by (icd_code, icd_version), so a code listed in both
# ICD-9 and ICD-10 keeps both rows; without a version, by icd_code alone.
.ehr_icd_filter_dict <- function(path) {
  filter <- .ehr_read_icd_filter_file(path)
  named <- nzchar(names(filter)) & !grepl("^\\.\\.\\.[0-9]+$", names(filter))
  filter <- filter[, named, drop = FALSE]
  if ("icd_version" %in% names(filter)) {
    filter$icd_version <- .icd_version_token(filter$icd_version,
                                             "ICD filter icd_version")
  }
  key <- intersect(c("icd_code", "icd_version"), names(filter))
  as.data.frame(filter[!duplicated(filter[key]), , drop = FALSE])
}

# Derives the reference-table label for the ICD codes that defined a cohort:
# the named example cohort itself (e.g. "scid") when one was used, or a
# sanitized label from the raw icd_filter_path's basename otherwise. The
# caller composes the actual table name as paste0("d_", name, "_icds").
.ehr_icd_dict_name <- function(named, icd_filter_path) {
  if (!is.null(named)) {
    return(.ehr_validate_db_identifier(as.character(named), "named cohort"))
  }
  candidate <- tolower(tools::file_path_sans_ext(basename(icd_filter_path)))
  candidate <- gsub("[^a-z0-9_]+", "_", candidate)
  candidate <- gsub("^_+|_+$", "", candidate)
  if (!nzchar(candidate) || grepl("^[0-9]", candidate)) {
    candidate <- paste0("x", candidate)
  }
  tryCatch(
    .ehr_validate_db_identifier(candidate, "ICD dictionary name"),
    error = function(e) "custom"
  )
}

# Informational (Step 5): filter codes whose compact string is an official
# diagnosis in both ICD-9 and ICD-10 (e.g. V103, E8801). Version-aware
# selection already matches each only in its listed version; this tells the
# user which codes that protects. Returns the colliding codes (possibly none);
# NULL when no dictionary is available to check against.
.ehr_icd_filter_collisions <- function(keys, icds_dict_path = NULL) {
  path <- icds_dict_path %||% ehr_config_path("d_icd_diagnoses.csv")
  if (is.null(path) || !file.exists(path)) return(NULL)
  dict <- .load_icds_dict(path)
  both <- intersect(dict$icd_code[dict$icd_version == "9"],
                    dict$icd_code[dict$icd_version == "10"])
  sort(intersect(unique(keys$icd_code), both))
}

# Version-aware ICD selection (D-1): a diagnosis matches the filter only when
# both its code and its ICD version match a filter row.
#   mimic_iv : join on (code, icd_version).
#   mimic_iii: filter restricted to ICD-9 by .ehr_icd_filter_keys(); join on
#              code.
#   eicu     : distinct raw tokens are resolved with .eicu_resolve_icd_tokens()
#              (the same resolver used for clinical annotation), then joined on
#              (compact code, resolved version). Tokens with no resolved version
#              (ambiguous, unmatched) select nothing and are reported. Matching
#              unit stays are the evidence; they are promoted to their
#              hospitalizations (patient.patienthealthsystemstayid), which are
#              the selection.
#   other    : a registered source's declarative selector, code only.
# ICD cohorts therefore select hospitalizations containing a matching
# diagnosis in both eICU and MIMIC. Returns the selection key and values, the
# evidence key and values (for eICU the matching unit stays, kept for audit
# only - never a membership flag), the published dictionary, the version
# policy actually applied, and a report of what the filter could not use.
.ehr_select_icd <- function(con, src_name, icd_filter_path,
                            icds_dict_path = NULL,
                            icd_adjudication_path = NULL) {
  keys <- .ehr_icd_filter_keys(icd_filter_path, src_name)
  DBI::dbWriteTable(con, "ehr_icd_filter", keys,
                    temporary = TRUE, overwrite = TRUE)
  family <- src_family(src_name)
  # Report counts are at the evidence grain (eICU: unit stays). A unit stay
  # without direct evidence can still be a cohort member through its
  # hospitalization, so these counts are not exclusions.
  # unresolved_affected_unit_count: units holding an unresolved token that
  #   matches a filter code (they may still match through another diagnosis).
  # unresolved_no_evidence_unit_count: of those, units with no direct matching
  #   evidence - the evidence actually lost to unresolved tokens.
  # other_version_unit_count: units without direct evidence whose only
  #   filter-code match is the same code string in the other ICD version.
  collisions <- .ehr_icd_filter_collisions(keys, icds_dict_path)
  if (length(collisions)) {
    message("ICD filter: ", length(collisions), " code(s) are valid diagnoses ",
            "in both ICD-9 and ICD-10 (e.g. ",
            paste(utils::head(collisions, 5L), collapse = ", "),
            "); each matches only in the version the filter lists.")
  }
  report <- list(filter_collision_count = if (is.null(collisions)) NA_real_
                                          else as.numeric(length(collisions)),
                 unresolved_token_count = NA_real_,
                 unresolved_affected_unit_count = NA_real_,
                 unresolved_no_evidence_unit_count = NA_real_,
                 unresolved_by_status = NULL,
                 other_version_unit_count = NA_real_)
  count <- function(sql) as.numeric(DBI::dbGetQuery(con, sql)$n[[1L]])
  evidence_key <- NULL
  evidence_query <- NULL

  if (identical(family, "mimic_iv")) {
    code <- "upper(replace(trim(CAST(d.icd_code AS VARCHAR)), '.', ''))"
    version <- "trim(CAST(d.icd_version AS VARCHAR))"
    query <- paste(
      "SELECT DISTINCT CAST(d.hadm_id AS VARCHAR) AS cohort_id",
      "FROM diagnoses_icd d INNER JOIN ehr_icd_filter f",
      "ON", code, "= f.icd_code AND", version, "= f.icd_version",
      "WHERE d.hadm_id IS NOT NULL"
    )
    # Admissions whose only match is a same-string code in the other version.
    report$other_version_unit_count <- count(paste(
      "SELECT COUNT(DISTINCT d.hadm_id) AS n FROM diagnoses_icd d",
      "WHERE d.hadm_id IS NOT NULL AND", code,
      "IN (SELECT icd_code FROM ehr_icd_filter)",
      "AND CAST(d.hadm_id AS VARCHAR) NOT IN (", query, ")"))
    key <- "hadm_id"
    policy <- "code_and_version"
  } else if (identical(family, "mimic_iii")) {
    query <- paste(
      "SELECT DISTINCT CAST(d.hadm_id AS VARCHAR) AS cohort_id",
      "FROM diagnoses_icd d INNER JOIN ehr_icd_filter f",
      "ON upper(replace(trim(CAST(d.icd9_code AS VARCHAR)), '.', '')) = f.icd_code",
      "WHERE d.hadm_id IS NOT NULL"
    )
    key <- "hadm_id"
    policy <- "icd9_only"
  } else if (identical(family, "eicu")) {
    token_sql <- paste(
      "FROM diagnosis d,",
      "UNNEST(string_split(COALESCE(CAST(d.icd9code AS VARCHAR), ''), ',')) AS token(code)"
    )
    raw <- DBI::dbGetQuery(con, paste(
      "SELECT DISTINCT upper(trim(token.code)) AS raw", token_sql))$raw
    dict_path <- icds_dict_path %||% ehr_config_path("d_icd_diagnoses.csv")
    if (is.null(dict_path) || !file.exists(dict_path)) {
      stop("eICU ICD selection requires the diagnosis dictionary to resolve ",
           "ICD versions; not found: ", dict_path %||% "<unset>",
           call. = FALSE)
    }
    resolved <- .eicu_resolve_icd_tokens(
      raw, .load_icds_dict(dict_path),
      .load_eicu_icd_adjudication(icd_adjudication_path))
    DBI::dbWriteTable(con, "ehr_icd_token_version",
                      as.data.frame(resolved[!is.na(resolved$icd_code_raw), ]),
                      temporary = TRUE, overwrite = TRUE)
    matched <- paste(
      token_sql,
      "INNER JOIN ehr_icd_token_version t ON upper(trim(token.code)) = t.icd_code_raw",
      "WHERE d.patientunitstayid IS NOT NULL")
    # Unit stays with direct diagnostic evidence. The diagnostic counts below
    # compare units with this, never with the promoted hospital ids.
    unit_query <- paste(
      "SELECT DISTINCT CAST(d.patientunitstayid AS VARCHAR) AS cohort_id",
      matched,
      "AND EXISTS (SELECT 1 FROM ehr_icd_filter f",
      "WHERE f.icd_code = t.icd_code_compact AND f.icd_version = t.icd_version)"
    )
    # Promote to hospitalizations. Every evidence unit must map to exactly one
    # hospitalization in `patient`; a missing or ambiguous mapping is an error,
    # not silently dropped evidence.
    mapping <- DBI::dbGetQuery(con, paste(
      "SELECT e.cohort_id AS unit_id,",
      "COUNT(DISTINCT p.patienthealthsystemstayid) AS n_hosp",
      "FROM (", unit_query, ") e LEFT JOIN patient p",
      "ON CAST(p.patientunitstayid AS VARCHAR) = e.cohort_id",
      "GROUP BY e.cohort_id",
      "HAVING COUNT(DISTINCT p.patienthealthsystemstayid) <> 1"))
    if (nrow(mapping)) {
      stop("eICU ICD selection: ", sum(mapping$n_hosp == 0), " matching unit ",
           "stay(s) have no patienthealthsystemstayid in `patient` and ",
           sum(mapping$n_hosp > 1), " map to more than one; each must map to ",
           "exactly one hospitalization (ids are not shown).", call. = FALSE)
    }
    query <- paste(
      "SELECT DISTINCT CAST(p.patienthealthsystemstayid AS VARCHAR) AS cohort_id",
      "FROM patient p WHERE CAST(p.patientunitstayid AS VARCHAR) IN (",
      unit_query, ")")
    evidence_key <- "patientunitstayid"
    evidence_query <- unit_query
    unresolved <- DBI::dbGetQuery(con, paste(
      "SELECT t.version_resolution_status AS status, COUNT(*) AS tokens,",
      "COUNT(DISTINCT d.patientunitstayid) AS units", matched,
      "AND t.icd_version IS NULL",
      "AND t.icd_code_compact IN (SELECT icd_code FROM ehr_icd_filter)",
      "GROUP BY 1 ORDER BY 1"))
    report$unresolved_by_status <- unresolved
    report$unresolved_token_count <- sum(as.numeric(unresolved$tokens))
    affected <- paste(
      "SELECT DISTINCT CAST(d.patientunitstayid AS VARCHAR) AS id", matched,
      "AND t.icd_version IS NULL",
      "AND t.icd_code_compact IN (SELECT icd_code FROM ehr_icd_filter)")
    report$unresolved_affected_unit_count <- count(paste(
      "SELECT COUNT(*) AS n FROM (", affected, ") a"))
    report$unresolved_no_evidence_unit_count <- count(paste(
      "SELECT COUNT(*) AS n FROM (", affected, ") a",
      "WHERE a.id NOT IN (", unit_query, ")"))
    report$other_version_unit_count <- count(paste(
      "SELECT COUNT(DISTINCT d.patientunitstayid) AS n", matched,
      "AND t.icd_version IS NOT NULL",
      "AND t.icd_code_compact IN (SELECT icd_code FROM ehr_icd_filter)",
      "AND CAST(d.patientunitstayid AS VARCHAR) NOT IN (", unit_query, ")"))
    key <- "patienthealthsystemstayid"
    policy <- "code_and_version"
  } else {
    entry <- .source_entry(src_name)
    spec <- entry$cohort_filter
    if (is.null(spec)) {
      stop("Source family '", family,
           "' has no canonical declarative ICD selector.", call. = FALSE)
    }
    tableq <- as.character(DBI::dbQuoteIdentifier(con, spec$diag_table))
    codeq <- as.character(DBI::dbQuoteIdentifier(con, spec$code_col))
    keyq <- as.character(DBI::dbQuoteIdentifier(con, spec$key))
    code_expr <- paste0("upper(trim(CAST(d.", codeq, " AS VARCHAR)))")
    if (isTRUE(spec$strip_dots)) {
      code_expr <- paste0("replace(", code_expr, ", '.', '')")
    }
    query <- sprintf(
      paste0(
        "SELECT DISTINCT CAST(d.%s AS VARCHAR) AS cohort_id ",
        "FROM %s d INNER JOIN (SELECT DISTINCT icd_code FROM ehr_icd_filter) f ",
        "ON %s = f.icd_code WHERE d.%s IS NOT NULL"
      ),
      keyq, tableq, code_expr, keyq
    )
    key <- spec$key
    policy <- "code_only"
  }
  values <- DBI::dbGetQuery(con, query)$cohort_id
  evidence_values <- if (is.null(evidence_query)) values
                     else DBI::dbGetQuery(con, evidence_query)$cohort_id
  evidence_key <- evidence_key %||% key

  if (isTRUE(report$unresolved_token_count > 0)) {
    by <- report$unresolved_by_status
    message("ICD selection (", src_name, "): ignored ",
            report$unresolved_token_count, " diagnosis token(s) that match a ",
            "filter code but have no resolved ICD version (",
            paste0(by$status, ": ", by$tokens, collapse = ", "), "). ",
            "They occur in ", report$unresolved_affected_unit_count,
            " ICU stay(s); ", report$unresolved_no_evidence_unit_count,
            " of those have no other matching diagnosis (they are still ",
            "members if their hospitalization qualifies).")
  }
  if (isTRUE(report$other_version_unit_count > 0)) {
    message("ICD selection (", src_name, "): ",
            report$other_version_unit_count, " ", evidence_key, " value(s) ",
            "have no direct evidence because their only match is the same ",
            "code string in the other ICD version.")
  }
  list(key = key, values = values,
       evidence_key = evidence_key, evidence_values = evidence_values,
       dict = .ehr_icd_filter_dict(icd_filter_path),
       policy = policy, report = report)
}

.ehr_resolve_selection <- function(con, src_name, paths, ladder,
                                   icd_filter_path = NULL,
                                   cohort_sql = NULL, setup_sql = NULL,
                                   cohort_fn = NULL,
                                   direct_ids_path = NULL,
                                   cohort = NULL, no_filter = FALSE,
                                   key = NULL, import_scope = NULL,
                                   allow_unmatched_ids = FALSE) {
  named <- cohort
  if (!is.null(cohort)) {
    resolved <- .apply_example_cohort(
      cohort, src_name, icd_filter_path, cohort_sql, setup_sql, cohort_fn,
      direct_ids_path, key, no_filter
    )
    icd_filter_path <- resolved$icd_filter_path
    cohort_sql <- resolved$cohort_sql
    setup_sql <- resolved$setup_sql
    key <- resolved$key
  }

  supplied <- c(
    icd = !is.null(icd_filter_path),
    sql = !is.null(cohort_sql),
    function_result = !is.null(cohort_fn),
    direct_ids = !is.null(direct_ids_path)
  )
  if (isTRUE(no_filter) && any(supplied)) {
    stop("Pass one selection route or `no_filter = TRUE`, not both.",
         call. = FALSE)
  }
  if (!isTRUE(no_filter) && !any(supplied)) {
    icd_filter_path <- paths$icd_filter_path
    if (is.null(icd_filter_path)) {
      stop("No cohort definition is configured. Supply a selection route or ",
           "set `no_filter = TRUE`.", call. = FALSE)
    }
    supplied[["icd"]] <- TRUE
  }
  if (sum(supplied) > 1L) {
    stop("Supply exactly one selection route.", call. = FALSE)
  }

  if (isTRUE(no_filter)) {
    return(.ehr_selection_plan(
      src_name, "no_filter", ladder[[1L]],
      requested_scope = import_scope,
      effective_scope = ladder[[1L]], values = character(),
      allow_unmatched_ids = allow_unmatched_ids
    ))
  }

  route <- names(supplied)[supplied][[1L]]
  metadata <- list(named_cohort = named)
  evidence_key <- NULL
  if (identical(route, "direct_ids")) {
    key <- .ehr_infer_direct_id_key(direct_ids_path, ladder, key)
    input <- .ehr_read_direct_ids(direct_ids_path, key, src_name)
    values <- input[[key]]
    metadata$ignored_column_count <-
      as.integer(attr(input, "ignored_column_count") %||% 0L)
  } else if (identical(route, "function_result")) {
    key <- key %||% ladder[[1L]]
    fn <- if (is.character(cohort_fn)) match.fun(cohort_fn) else cohort_fn
    if (!is.function(fn)) {
      stop("`cohort_fn` must be a function or function name.", call. = FALSE)
    }
    result <- fn(paths)
    if (is.data.frame(result)) {
      if (!key %in% names(result)) {
        stop("The cohort function result does not contain the selected key.",
             call. = FALSE)
      }
      values <- result[[key]]
      metadata$ignored_column_count <- max(0L, ncol(result) - 1L)
    } else {
      values <- result
    }
  } else if (identical(route, "sql")) {
    key <- key %||% ladder[[1L]]
    for (statement in setup_sql) {
      DBI::dbExecute(con, .duckdb_sql_text(statement), immediate = TRUE)
    }
    result <- DBI::dbGetQuery(con, .duckdb_sql_text(cohort_sql))
    if (!key %in% names(result)) {
      stop("The SQL cohort result does not contain the selected key.",
           call. = FALSE)
    }
    values <- result[[key]]
    metadata$ignored_column_count <- max(0L, ncol(result) - 1L)
  } else {
    selected <- .ehr_select_icd(
      con, src_name, icd_filter_path,
      icds_dict_path = paths$icds_dict_path,
      icd_adjudication_path = paths$icd_adjudication_path)
    key <- selected$key
    values <- selected$values
    # Evidence grain can be finer than the selection (eICU: unit stays promoted
    # to hospitalizations). The evidence ids are kept for audit only.
    evidence_key <- selected$evidence_key
    metadata$icd_evidence <- data.frame(
      evidence_key = rep(evidence_key, length(selected$evidence_values)),
      evidence_id = as.character(selected$evidence_values),
      stringsAsFactors = FALSE)
    metadata$icd_version_policy <- selected$policy
    metadata$icd_selection_report <- selected$report
    metadata$icd_dict <- list(
      name = .ehr_icd_dict_name(named, icd_filter_path),
      table = selected$dict
    )
  }

  values <- .ehr_normalize_id_values(values, paste0(route, " selection"))
  # Same default-to-patient-scope rule as every other route: NULL -> ladder[1]
  # (patient), an explicit import_scope is respected (and clamped if finer
  # than the selection grain - so an ICU request for an ICD cohort, which
  # selects hospitalizations, clamps to hospital scope in every source).
  # Previously the icd route bypassed this and always forced patient scope,
  # silently ignoring any import_scope the caller passed alongside an
  # icd_filter_path.
  effective_scope <- .resolve_import_scope(import_scope, key, ladder, show_progress = FALSE)
  plan <- .ehr_selection_plan(
    source = src_name,
    route = if (is.null(named)) route else paste0("named_", route),
    key = key,
    evidence_key = evidence_key %||% key,
    requested_scope = import_scope,
    effective_scope = effective_scope,
    values = values,
    allow_unmatched_ids = allow_unmatched_ids,
    metadata = metadata
  )
  .ehr_validate_selection_plan(plan, ladder)
}

.ehr_build_full_spine <- function(con, paths, window_sql = NULL,
                                  window_fn = NULL) {
  if (!is.null(window_fn)) {
    fn <- if (is.character(window_fn)) match.fun(window_fn) else window_fn
    if (!is.function(fn)) {
      stop("`window_fn` must be a function or function name.", call. = FALSE)
    }
    DBI::dbWriteTable(
      con, "ehr_visit_windows_full", as.data.frame(fn(con)),
      temporary = TRUE, overwrite = TRUE
    )
    return(invisible(TRUE))
  }

  path <- window_sql %||% file.path(paths$helper_sql_dir, "window_tables.sql")
  if (!file.exists(path)) {
    stop("Cohort hierarchy SQL was not found; provide `window_sql` or ",
         "`window_fn`.", call. = FALSE)
  }
  statements <- .ehr_split_sql(readLines(path, warn = FALSE))
  for (statement in statements) {
    DBI::dbExecute(con, adapt_sql_for_duckdb(statement), immediate = TRUE)
  }
  DBI::dbExecute(
    con,
    "CREATE OR REPLACE TEMP TABLE ehr_visit_windows_full AS
       SELECT * FROM visit_windows"
  )
  for (table in c("visit_windows", "admission_windows", "icu_windows")) {
    DBI::dbExecute(
      con,
      sprintf("DROP TABLE IF EXISTS %s", DBI::dbQuoteIdentifier(con, table))
    )
  }
  invisible(TRUE)
}

.ehr_match_plan_to_spine <- function(con, plan, ladder) {
  columns <- DBI::dbGetQuery(con, "DESCRIBE ehr_visit_windows_full")
  if (!all(ladder %in% columns$column_name)) {
    stop("The cohort hierarchy is missing configured identifier column(s): ",
         paste(setdiff(ladder, columns$column_name), collapse = ", "),
         ".", call. = FALSE)
  }
  if (identical(plan$route, "no_filter")) {
    rootq <- as.character(DBI::dbQuoteIdentifier(con, ladder[[1L]]))
    values <- DBI::dbGetQuery(con, sprintf(
      "SELECT DISTINCT CAST(%s AS VARCHAR) AS value
         FROM ehr_visit_windows_full WHERE %s IS NOT NULL",
      rootq, rootq
    ))$value
    plan$values <- values
    plan$requested_count <- length(values)
    plan$matched_count <- length(values)
    plan$unmatched_count <- 0L
    return(plan)
  }

  key <- plan$selection_key
  type <- columns$column_type[match(key, columns$column_name)]
  DBI::dbWriteTable(
    con, "ehr_selection_values",
    data.frame(value = plan$values, stringsAsFactors = FALSE),
    temporary = TRUE, overwrite = TRUE
  )
  casted <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT CAST(TRY_CAST(value AS %s) AS VARCHAR) AS value,
            TRY_CAST(value AS %s) IS NULL AS invalid
       FROM ehr_selection_values",
    type, type
  ))
  invalid_count <- sum(casted$invalid)
  if (invalid_count) {
    stop("Selection contains ", invalid_count,
         " identifier value(s) that cannot be converted to the configured ",
         "hierarchy type; values are not shown.", call. = FALSE)
  }
  canonical <- unique(casted$value)
  keyq <- as.character(DBI::dbQuoteIdentifier(con, key))
  DBI::dbWriteTable(
    con, "ehr_selection_values",
    data.frame(value = canonical, stringsAsFactors = FALSE),
    temporary = TRUE, overwrite = TRUE
  )
  matched <- DBI::dbGetQuery(con, sprintf(
    paste0(
      "SELECT DISTINCT CAST(%s AS VARCHAR) AS value ",
      "FROM ehr_visit_windows_full ",
      "WHERE %s IS NOT NULL AND CAST(%s AS VARCHAR) IN ",
      "(SELECT value FROM ehr_selection_values)"
    ),
    keyq, keyq, keyq
  ))$value
  unmatched <- length(canonical) - length(matched)
  if (unmatched > 0L && !isTRUE(plan$allow_unmatched_ids)) {
    stop(unmatched, " of ", length(canonical),
         " selected identifier value(s) were not found in the source ",
         "hierarchy (values are not shown to avoid exposing identifiers). ",
         "Set `allow_unmatched_ids = TRUE` to proceed using only the ",
         "identifiers that did match.", call. = FALSE)
  }
  if (!length(matched)) {
    stop("No selected identifier matched the source hierarchy.",
         call. = FALSE)
  }
  plan$values <- matched
  plan$requested_count <- length(canonical)
  plan$matched_count <- length(matched)
  plan$unmatched_count <- unmatched
  DBI::dbWriteTable(
    con, "ehr_selection_values",
    data.frame(value = matched, stringsAsFactors = FALSE),
    temporary = TRUE, overwrite = TRUE
  )
  plan
}

.ehr_build_cohort_hierarchy <- function(con, schema, plan, ladder) {
  plan <- .ehr_match_plan_to_spine(con, plan, ladder)
  root <- ladder[[1L]]
  rootq <- as.character(DBI::dbQuoteIdentifier(con, root))
  keyq <- as.character(DBI::dbQuoteIdentifier(con, plan$selection_key))
  scopeq <- as.character(DBI::dbQuoteIdentifier(con, plan$effective_scope))
  schemaq <- as.character(DBI::dbQuoteIdentifier(con, schema))
  hierarchy <- as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = "cohort_hierarchy")
  ))
  visits <- as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = "visit_windows")
  ))

  definition <- if (identical(plan$route, "no_filter")) {
    "TRUE"
  } else {
    sprintf(
      "COALESCE(CAST(%s AS VARCHAR) IN
       (SELECT value FROM ehr_selection_values), FALSE)",
      keyq
    )
  }
  context <- if (identical(plan$route, "no_filter")) {
    "TRUE"
  } else {
    sprintf(
      paste0(
        "%s IN (SELECT DISTINCT %s FROM ehr_visit_windows_full ",
        "WHERE %s)"
      ),
      rootq, rootq, definition
    )
  }
  scope <- if (identical(plan$route, "no_filter")) {
    "TRUE"
  } else {
    sprintf(
      paste0(
        "COALESCE(%s IN (SELECT DISTINCT %s FROM ehr_visit_windows_full ",
        "WHERE %s), FALSE)"
      ),
      scopeq, scopeq, definition
    )
  }

  DBI::dbExecute(con, sprintf("CREATE SCHEMA IF NOT EXISTS %s", schemaq))
  # The windows keep the whole patient context (readmissions, other stays) so
  # concepts can look back; in_scope marks the rows whose data was imported.
  DBI::dbExecute(con, sprintf(
    paste0(
      "CREATE OR REPLACE TABLE %s AS ",
      "SELECT *, (%s) AS cohort_index, (%s) AS in_scope ",
      "FROM ehr_visit_windows_full WHERE %s"
    ),
    visits, definition, scope, context
  ))
  ids <- paste(
    as.character(DBI::dbQuoteIdentifier(con, ladder)),
    collapse = ", "
  )
  # patient_context_index is intentionally hardcoded TRUE, not computed: the
  # row set here is already restricted to context rows by the trailing WHERE
  # %s (context), so every row that exists satisfies it by construction. This
  # is the enforced contract, not a placeholder - .ehr_hierarchy_contract_valid()
  # explicitly counts `NOT patient_context_index` as a violation, and
  # definition_filter_index/scope_index are only ever meaningful as subsets
  # of it (scope_index <= patient_context_index, checked in
  # test-cohort-hierarchy.R). Do not change this to a computed expression
  # without also relaxing the WHERE clause and updating that contract check.
  DBI::dbExecute(con, sprintf(
    paste0(
      "CREATE OR REPLACE TABLE %s AS ",
      "SELECT DISTINCT %s, TRUE AS patient_context_index, ",
      "(%s) AS definition_filter_index, (%s) AS scope_index ",
      "FROM ehr_visit_windows_full WHERE %s"
    ),
    hierarchy, ids, definition, scope, context
  ))
  for (view in c("patient_context", "definition_filter", "scope")) {
    index <- paste0(view, "_index")
    viewq <- as.character(DBI::dbQuoteIdentifier(
      con, DBI::Id(schema = schema, table = paste0("cohort_", view))
    ))
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM %s WHERE %s",
      viewq, hierarchy, DBI::dbQuoteIdentifier(con, index)
    ))
  }
  list(plan = plan, hierarchy_ref = hierarchy, visit_ref = visits)
}

.ehr_table_filter_sql <- function(con, contract, hierarchy_ref,
                                  alias = "v", no_filter = FALSE) {
  if (identical(contract$role, "reference")) {
    return(list(where = "", filtered = FALSE, link_key = NA_character_))
  }
  link <- contract$link_key
  linkq <- as.character(DBI::dbQuoteIdentifier(con, link))
  # no_filter imports every row. Filtering on the link key would still drop
  # rows whose link is NULL (e.g. MIMIC-III ward prescriptions with no
  # icustay_id), which no_filter promises to keep. The link key is still
  # returned so large tables keep building in hash chunks.
  if (isTRUE(no_filter)) {
    return(list(where = "WHERE TRUE", filtered = FALSE, link_key = link))
  }
  list(
    where = sprintf(
      paste0(
        "WHERE CAST(%s.%s AS VARCHAR) IN ",
        "(SELECT DISTINCT CAST(%s AS VARCHAR) FROM %s WHERE scope_index)"
      ),
      alias, linkq, linkq, hierarchy_ref
    ),
    filtered = TRUE,
    link_key = link
  )
}

.ehr_write_metadata <- function(con, schema, cfg, plan, projection_report,
                                table_report, validate_columns = FALSE) {
  meta <- data.frame(
    source = plan$source,
    source_family = src_family(plan$source),
    source_version = as.character(cfg$release %||% cfg$version %||% NA),
    route = plan$route,
    selection_key = plan$selection_key,
    selection_grain = plan$selection_grain,
    evidence_key = plan$evidence_key,
    evidence_grain = plan$evidence_grain,
    requested_scope = as.character(plan$requested_scope %||% NA),
    effective_scope = plan$effective_scope,
    requested_count = as.numeric(plan$requested_count),
    matched_count = as.numeric(plan$matched_count),
    unmatched_count = as.numeric(plan$unmatched_count),
    allow_unmatched_ids = plan$allow_unmatched_ids,
    icd_version_policy = as.character(
      plan$metadata$icd_version_policy %||% NA
    ),
    # What version-aware ICD selection could not use (NA when not applicable).
    icd_filter_collision_count = as.numeric(
      plan$metadata$icd_selection_report$filter_collision_count %||% NA),
    icd_unresolved_token_count = as.numeric(
      plan$metadata$icd_selection_report$unresolved_token_count %||% NA),
    icd_unresolved_affected_unit_count = as.numeric(
      plan$metadata$icd_selection_report$unresolved_affected_unit_count %||% NA),
    # Units whose only matching token was unresolved. Not exclusions: a unit
    # without direct evidence is still a member through its hospitalization.
    icd_unresolved_no_evidence_unit_count = as.numeric(
      plan$metadata$icd_selection_report$unresolved_no_evidence_unit_count %||% NA),
    icd_other_version_count = as.numeric(
      plan$metadata$icd_selection_report$other_version_unit_count %||% NA),
    # Records whether validate_columns = TRUE was used for this build, so an
    # empty cohort_projection_report can be told apart after the fact:
    # "validation ran, nothing failed" (validate_columns = TRUE here) vs
    # "validation was never run" (FALSE) - the two look identical in the
    # report table alone.
    validate_columns = isTRUE(validate_columns),
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "cohort_meta"),
    meta, overwrite = TRUE
  )
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "cohort_projection_report"),
    projection_report, overwrite = TRUE
  )
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "cohort_table_report"),
    table_report, overwrite = TRUE
  )
  # ICD evidence at its own grain (eICU: matching unit stays), for audit. It is
  # not a membership flag: membership is the selection (hospitalizations).
  if (!is.null(plan$metadata$icd_evidence)) {
    DBI::dbWriteTable(
      con, DBI::Id(schema = schema, table = "cohort_icd_evidence"),
      plan$metadata$icd_evidence, overwrite = TRUE
    )
  }
  invisible(meta)
}

.ehr_artifact_schemas <- function(con, schema, tables) {
  stats::setNames(lapply(tables, function(table) {
    ref <- as.character(DBI::dbQuoteIdentifier(
      con, DBI::Id(schema = schema, table = table)
    ))
    description <- DBI::dbGetQuery(con, paste("DESCRIBE", ref))
    list(
      columns = as.character(description$column_name),
      types = toupper(as.character(description$column_type))
    )
  }), tables)
}

.ehr_schema_contract_matches <- function(actual, expected) {
  identical(as.character(actual$column_name), unname(unlist(expected$columns))) &&
    identical(
      toupper(as.character(actual$column_type)),
      toupper(unname(unlist(expected$types)))
    )
}

.ehr_manifest_data <- function(source, schema, tables, schemas, row_counts,
                               hierarchy_rows, output_kind) {
  list(
    format_version = 1L,
    status = "success",
    source = source,
    schema = schema,
    output_kind = output_kind,
    tables = as.character(tables),
    schemas = schemas,
    row_counts = row_counts,
    table_count = length(tables),
    hierarchy_rows = as.numeric(hierarchy_rows),
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

.EHR_BUILD_PROGRESS_TABLE <- "__ehr_build_progress"

# Idempotent - safe to call before every build.
.ehr_ensure_progress_table <- function(con) {
  DBI::dbExecute(con, sprintf(
    paste0(
      "CREATE TABLE IF NOT EXISTS %s (table_name VARCHAR, chunk_index INTEGER, ",
      "total_chunks INTEGER, completed_at TIMESTAMP)"
    ),
    DBI::dbQuoteIdentifier(con, .EHR_BUILD_PROGRESS_TABLE)
  ))
}

# Chunks already committed for `label` in a prior attempt at this same staging
# DB, or NULL if the progress table doesn't exist yet (nothing has ever been
# recorded). Never inferred from row counts - only from what was explicitly
# logged as done immediately after it committed.
.ehr_completed_chunks <- function(con, label) {
  tryCatch(
    DBI::dbGetQuery(con, sprintf(
      "SELECT chunk_index, total_chunks FROM %s WHERE table_name = %s ORDER BY chunk_index",
      DBI::dbQuoteIdentifier(con, .EHR_BUILD_PROGRESS_TABLE),
      DBI::dbQuoteString(con, label)
    )),
    error = function(e) NULL
  )
}

.ehr_mark_chunk_done <- function(con, label, chunk_index, total_chunks) {
  DBI::dbExecute(con, sprintf(
    "INSERT INTO %s VALUES (%s, %d, %d, now())",
    DBI::dbQuoteIdentifier(con, .EHR_BUILD_PROGRESS_TABLE),
    DBI::dbQuoteString(con, label), as.integer(chunk_index), as.integer(total_chunks)
  ))
}

.ehr_clear_chunk_progress <- function(con, label) {
  DBI::dbExecute(con, sprintf(
    "DELETE FROM %s WHERE table_name = %s",
    DBI::dbQuoteIdentifier(con, .EHR_BUILD_PROGRESS_TABLE), DBI::dbQuoteString(con, label)
  ))
}

# Builds one cohort table, splitting into multiple smaller INSERTs when the
# filtered row count exceeds `chunk_rows` so peak memory per statement stays
# bounded even for huge tables (e.g. MIMIC-III `chartevents`) where DuckDB's
# own out-of-core spilling isn't enough for a single `CREATE TABLE AS
# SELECT`. Chunks are split by `hash(link_key) % N`, so no sort order or
# known value range is needed. Reference (unfiltered) tables, and any table
# when `chunk_rows` is NULL, always build in one shot - unchanged behavior.
.ehr_build_cohort_table <- function(con, target, source, filter, chunk_rows,
                                    table = NULL, show_progress = TRUE,
                                    num_rows = NULL) {
  label <- table %||% target
  select_body <- sprintf("SELECT v.* FROM %s v %s", source, filter$where)
  n_chunks <- 1L
  if (!is.na(filter$link_key %||% NA_character_) && !is.null(chunk_rows) &&
      !is.null(num_rows)) {
    n_chunks <- max(1L, ceiling(num_rows / chunk_rows))
    if (show_progress) {
      message(
        label, ": ", format(num_rows, big.mark = ","),
        " configured raw row(s) (upper bound) -> ",
        if (n_chunks > 1L) paste0(n_chunks, " chunk(s)") else "single statement"
      )
    }
  }

  done <- .ehr_completed_chunks(con, label)
  # A prior attempt's chunk plan only means something if it used the same
  # total_chunks; otherwise the bucket boundaries don't line up and any
  # partial progress must be discarded rather than trusted.
  if (!is.null(done) && nrow(done) && !all(done$total_chunks == n_chunks)) {
    done <- NULL
  }
  already <- if (!is.null(done)) done$chunk_index else integer()

  if (length(already) == n_chunks) {
    if (show_progress) {
      message(label, ": already complete (", n_chunks, " chunk(s)), skipping.")
    }
    return(invisible(n_chunks))
  }

  if (n_chunks <= 1L) {
    if (show_progress) message(label, ": building (single statement)...")
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS %s", target, select_body
    ))
    .ehr_mark_chunk_done(con, label, 1L, 1L)
    return(invisible(n_chunks))
  }

  linkq <- as.character(DBI::dbQuoteIdentifier(con, filter$link_key))
  if (!length(already)) {
    # No usable prior progress for this table - (re)start it clean. If the
    # table already exists from an incompatible/prior partial attempt, this
    # replaces it; .ehr_clear_chunk_progress keeps the log consistent.
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TABLE %s AS SELECT v.* FROM %s v WHERE FALSE",
      target, source
    ))
    .ehr_clear_chunk_progress(con, label)
  }
  # Each chunk filtered straight from `source` would re-read the whole raw
  # input (for a CSV view: decompress the file again) once per chunk. Stream
  # the cohort's rows to a temporary parquet file in one pass instead - COPY
  # is memory-bounded like the chunks - and fill the chunks from that file.
  stage_file <- .ehr_chunk_stage_file(con, label)
  on.exit(unlink(stage_file), add = TRUE)
  if (show_progress) message(label, ": staging the cohort's rows (one pass over the source)...")
  DBI::dbExecute(con, sprintf(
    "COPY (%s) TO %s (FORMAT PARQUET)", select_body, DBI::dbQuoteString(con, stage_file)
  ))
  staged <- sprintf("read_parquet(%s)", DBI::dbQuoteString(con, stage_file))
  for (bucket in 0:(n_chunks - 1L)) {
    chunk_index <- bucket + 1L
    if (chunk_index %in% already) next
    # COALESCE so rows with a NULL link key fall into a bucket too.
    bucket_pred <- function(alias) sprintf(
      "hash(COALESCE(CAST(%s%s AS VARCHAR), '')) %% %d = %d",
      if (nzchar(alias)) paste0(alias, ".") else "", linkq, n_chunks, bucket
    )
    if (show_progress) {
      message(label, ": chunk ", chunk_index, "/", n_chunks, "...")
    }
    if (length(already)) {
      # A kill during an INSERT can leave part of that chunk committed in the
      # staging database; clear the chunk before writing it again.
      DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE %s", target,
                                  bucket_pred("")))
    }
    DBI::dbExecute(con, sprintf(
      "INSERT INTO %s SELECT v.* FROM %s v WHERE %s",
      target, staged, bucket_pred("v")
    ))
    .ehr_mark_chunk_done(con, label, chunk_index, n_chunks)
  }
  invisible(n_chunks)
}

# Where a chunked build stages its rows: DuckDB's own spill directory when one
# is set (it is chosen for space), otherwise the R session's temporary folder.
.ehr_chunk_stage_file <- function(con, label) {
  dir <- tryCatch(DBI::dbGetQuery(con, "SELECT current_setting('temp_directory') AS d")$d,
                  error = function(e) "")
  if (!length(dir) || is.na(dir) || !nzchar(dir)) dir <- tempdir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tempfile(paste0("ehr_chunk_stage_", gsub("[^A-Za-z0-9_]", "_", label), "_"),
           tmpdir = dir, fileext = ".parquet")
}

.ehr_build_artifact <- function(db_path, parquet_dir = NULL,
                                src_name, base_dir = NULL,
                                raw_data_root = NULL,
                                icd_filter_path = NULL,
                                cohort_sql = NULL, setup_sql = NULL,
                                cohort_fn = NULL,
                                direct_ids_path = NULL,
                                cohort = NULL, no_filter = FALSE,
                                key = NULL, import_scope = NULL,
                                allow_unmatched_ids = FALSE,
                                window_sql = NULL, window_fn = NULL,
                                schema = NULL, tables = NULL,
                                memory_limit = NULL, threads = NULL,
                                temp_directory = NULL,
                                table_chunk_rows = NULL,
                                validate_columns = FALSE,
                                ignore_errors = TRUE,
                                store_rejects = TRUE,
                                use_parquet_cache = TRUE,
                                show_progress = TRUE) {
  .duckdb_require()
  paths <- ehr_paths(
    src_name = src_name, base_dir = base_dir,
    raw_data_root = raw_data_root
  )
  src_name <- paths$src_name
  cfg <- load_src_cfg(paths$data_config_json, src_name)
  ladder <- .cohort_id_ladder(src_name)
  schema <- schema %||% src_schema_names(src_name)$raw
  all_tables <- names(cfg$tables)
  build_tables <- tables %||% all_tables

  con <- connect_duckdb(db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (!is.null(memory_limit)) {
    DBI::dbExecute(
      con, sprintf("SET memory_limit=%s", DBI::dbQuoteString(con, memory_limit))
    )
  }
  if (!is.null(threads)) {
    DBI::dbExecute(con, sprintf("SET threads=%d", as.integer(threads)))
  } else {
    .duckdb_configure_raw_csv_connection(
      con, .duckdb_is_demo_source(src_name)
    )
  }
  if (!is.null(temp_directory)) {
    DBI::dbExecute(
      con, sprintf(
        "SET temp_directory=%s",
        DBI::dbQuoteString(con, temp_directory)
      )
    )
  }
  DBI::dbExecute(
    con, sprintf(
      "CREATE SCHEMA IF NOT EXISTS %s",
      DBI::dbQuoteIdentifier(con, schema)
    )
  )
  .ehr_ensure_progress_table(con)

  prepared <- .ehr_prepare_source_views(
    con, cfg, paths, ladder, requested_tables = build_tables,
    table_chunk_rows = table_chunk_rows, validate_columns = validate_columns,
    ignore_errors = ignore_errors, store_rejects = store_rejects,
    use_parquet_cache = use_parquet_cache, show_progress = show_progress
  )
  plan <- .ehr_resolve_selection(
    con, src_name, paths, ladder,
    icd_filter_path = icd_filter_path,
    cohort_sql = cohort_sql, setup_sql = setup_sql,
    cohort_fn = cohort_fn, direct_ids_path = direct_ids_path,
    cohort = cohort, no_filter = no_filter, key = key,
    import_scope = import_scope,
    allow_unmatched_ids = allow_unmatched_ids
  )
  icd_dict <- plan$metadata$icd_dict
  .ehr_build_full_spine(con, paths, window_sql, window_fn)
  hierarchy <- .ehr_build_cohort_hierarchy(con, schema, plan, ladder)
  plan <- hierarchy$plan
  .duckdb_create_window_derivatives(con, schema, src_name)

  report <- vector("list", length(build_tables))
  for (i in seq_along(build_tables)) {
    table <- build_tables[[i]]
    if (!table %in% names(prepared$file_map)) {
      stop("Raw input is unavailable for requested table '", table, "'.",
           call. = FALSE)
    }
    contract <- prepared$contracts[[table]]
    filter <- .ehr_table_filter_sql(
      con, contract, hierarchy$hierarchy_ref, alias = "v",
      no_filter = identical(plan$route, "no_filter")
    )
    target <- as.character(DBI::dbQuoteIdentifier(
      con, DBI::Id(schema = schema, table = table)
    ))
    source <- as.character(DBI::dbQuoteIdentifier(con, table))
    started <- Sys.time()
    n_chunks <- .ehr_build_cohort_table(
      con, target, source, filter, table_chunk_rows, table, show_progress,
      num_rows = cfg$tables[[table]]$num_rows
    )
    rows <- .duckdb_row_count(con, schema, table)
    report[[i]] <- data.frame(
      table = table,
      rows = rows,
      cohort_role = contract$role,
      cohort_link_key = filter$link_key,
      filtered = filter$filtered,
      seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      used_parquet_cache = isTRUE(prepared$cache_used[[table]]),
      n_chunks = as.integer(n_chunks),
      stringsAsFactors = FALSE
    )
    if (show_progress) {
      message(
        "Built ", table, ": ", rows, " row(s)",
        if (filter$filtered) paste0(" via ", filter$link_key) else
          " as declared reference data",
        "  (", round(report[[i]]$seconds, 1), "s)"
      )
    }
  }
  table_report <- do.call(rbind, report)
  projection_report <- prepared$projection_report[
    prepared$projection_report$table %in% build_tables, , drop = FALSE
  ]
  .ehr_write_metadata(
    con, schema, cfg, plan, projection_report, table_report,
    validate_columns = validate_columns
  )

  dict_table <- NULL
  if (!is.null(icd_dict)) {
    dict_table <- paste0("d_", icd_dict$name, "_icds")
    DBI::dbWriteTable(
      con, DBI::Id(schema = schema, table = dict_table),
      icd_dict$table,
      overwrite = TRUE
    )
    if (show_progress) {
      message(
        "Built ", dict_table, ": ", nrow(icd_dict$table),
        " ICD code(s) as declared reference data"
      )
    }
  }

  hierarchy_rows <- .duckdb_row_count(con, schema, "cohort_hierarchy")
  special <- c(
    "admission_windows", "icu_windows", "visit_windows",
    "cohort_hierarchy", "cohort_meta", "cohort_projection_report",
    "cohort_table_report", dict_table,
    if (!is.null(plan$metadata$icd_evidence)) "cohort_icd_evidence"
  )
  output_tables <- unique(c(build_tables, special))
  artifact_schemas <- .ehr_artifact_schemas(con, schema, output_tables)
  artifact_row_counts <- stats::setNames(lapply(
    output_tables,
    function(table) .duckdb_row_count(con, schema, table)
  ), output_tables)
  manifest <- .ehr_manifest_data(
    src_name, schema, output_tables, artifact_schemas, artifact_row_counts,
    hierarchy_rows,
    if (is.null(parquet_dir)) "duckdb" else "parquet"
  )

  if (is.null(parquet_dir)) {
    manifest_row <- data.frame(
      format_version = manifest$format_version,
      status = manifest$status,
      source = manifest$source,
      schema = manifest$schema,
      output_kind = manifest$output_kind,
      tables_json = jsonlite::toJSON(
        manifest$tables, auto_unbox = FALSE
      ),
      schemas_json = jsonlite::toJSON(
        manifest$schemas, auto_unbox = FALSE
      ),
      row_counts_json = jsonlite::toJSON(
        manifest$row_counts, auto_unbox = TRUE
      ),
      table_count = manifest$table_count,
      hierarchy_rows = manifest$hierarchy_rows,
      completed_at = manifest$completed_at,
      stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(
      con, DBI::Id(schema = schema, table = "cohort_manifest"),
      manifest_row, overwrite = TRUE
    )
  } else {
    dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
    for (table in output_tables) {
      ref <- as.character(DBI::dbQuoteIdentifier(
        con, DBI::Id(schema = schema, table = table)
      ))
      .duckdb_export_table_parquet(
        con, ref, file.path(parquet_dir, paste0(table, ".parquet")),
        overwrite = TRUE, show_progress = show_progress
      )
    }
    manifest$files <- paste0(output_tables, ".parquet")
    jsonlite::write_json(
      manifest, file.path(parquet_dir, "cohort_manifest.json"),
      auto_unbox = TRUE, pretty = TRUE, null = "null"
    )
  }
  # DuckDB's reject_errors/reject_scans (store_rejects = TRUE on the CSV
  # readers) are populated as a side effect of scanning, not at view-creation
  # time, and repeated scans of the same view duplicate entries - so this is
  # captured once, here, after every table has definitely been scanned at
  # least once (the .ehr_build_cohort_table() loop above), via a DISTINCT
  # query rather than trying to track per-table scan boundaries.
  csv_reject_report <- data.frame(
    table = character(), file = character(), line = integer(),
    column_name = character(), error_type = character(),
    csv_line = character(), error_message = character(),
    stringsAsFactors = FALSE
  )
  if (isTRUE(store_rejects)) {
    rejects <- tryCatch(
      DBI::dbGetQuery(con, "
        SELECT DISTINCT s.file_path, e.line, e.column_name, e.error_type,
                        e.csv_line, e.error_message
        FROM reject_errors e JOIN reject_scans s USING (scan_id)
      "),
      error = function(e) NULL
    )
    if (!is.null(rejects) && nrow(rejects) > 0L) {
      file_to_table <- stats::setNames(
        rep(names(prepared$file_map), lengths(prepared$file_map)),
        unlist(prepared$file_map, use.names = FALSE)
      )
      csv_reject_report <- data.frame(
        table = unname(file_to_table[rejects$file_path]),
        file = rejects$file_path,
        line = rejects$line,
        column_name = rejects$column_name,
        error_type = rejects$error_type,
        csv_line = rejects$csv_line,
        error_message = rejects$error_message,
        stringsAsFactors = FALSE
      )
      if (show_progress) {
        message(
          nrow(csv_reject_report), " row(s) rejected while reading raw CSV ",
          "(malformed beyond null_padding/strict_mode tolerance) across ",
          length(unique(csv_reject_report$table)), " table(s). ",
          "See the returned csv_reject_report for details."
        )
      }
    }
  }

  invisible(list(
    plan = plan,
    table_report = table_report,
    projection_report = projection_report,
    manifest = manifest,
    csv_reject_report = csv_reject_report
  ))
}

.ehr_hierarchy_contract_valid <- function(con, relation, expected_rows) {
  result <- tryCatch(DBI::dbGetQuery(con, sprintf(
    paste0(
      "SELECT count(*) AS rows, ",
      "count(*) FILTER (WHERE patient_context_index IS NULL ",
      "OR definition_filter_index IS NULL OR scope_index IS NULL) AS nulls, ",
      "count(*) FILTER (WHERE NOT patient_context_index ",
      "OR (definition_filter_index AND NOT patient_context_index) ",
      "OR (scope_index AND NOT patient_context_index)) AS violations ",
      "FROM %s"
    ),
    relation
  )), error = function(e) NULL)
  !is.null(result) &&
    identical(as.numeric(result$rows[[1L]]), as.numeric(expected_rows)) &&
    identical(as.numeric(result$nulls[[1L]]), 0) &&
    identical(as.numeric(result$violations[[1L]]), 0)
}

# ICD cohorts select hospitalizations containing a matching diagnosis. eICU
# ICD artifacts that instead selected unit stays (selection_key =
# patientunitstayid) encode unit membership in their hierarchy, so they must
# be rebuilt, never reused or relabelled. MIMIC ICD artifacts select
# admissions and stay valid.
# Returns NULL for a current cohort_meta, otherwise the reason to rebuild.
.ehr_stale_selection_reason <- function(meta) {
  if (is.null(meta) || !is.data.frame(meta) || !nrow(meta) ||
      !all(c("source_family", "route", "selection_key") %in% names(meta)))
    return(NULL)
  if (identical(as.character(meta$source_family[[1L]]), "eicu") &&
      as.character(meta$route[[1L]]) %in% c("icd", "named_icd") &&
      identical(as.character(meta$selection_key[[1L]]), "patientunitstayid"))
    return(paste0(
      "eICU ICD cohort built with the superseded unit-stay selection; ICD ",
      "cohorts now select hospitalizations. Rebuild it with ",
      "import_cohort_general(..., overwrite = TRUE), then run_db(..., ",
      "overwrite = TRUE)"))
  NULL
}

.ehr_check_parquet_artifact <- function(path) {
  invalid <- function(reason) list(ok = FALSE, reason = reason)
  manifest_path <- file.path(path, "cohort_manifest.json")
  if (!dir.exists(path) || !file.exists(manifest_path)) {
    return(invalid("artifact directory or success manifest is missing"))
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(manifest) || !identical(manifest$status, "success")) {
    return(invalid("success manifest is unreadable or incomplete"))
  }
  tables <- unname(unlist(manifest$tables))
  files <- unname(unlist(manifest$files))
  schemas <- manifest$schemas
  row_counts <- unlist(manifest$row_counts)
  if (!length(files) || !identical(length(files), length(tables))) {
    return(invalid("manifest table/file counts are inconsistent"))
  }
  if (!identical(sort(names(schemas)), sort(tables))) {
    return(invalid("manifest schema entries do not match its table list"))
  }
  if (!identical(sort(names(row_counts)), sort(tables))) {
    return(invalid("manifest row-count entries do not match its table list"))
  }
  missing <- files[!file.exists(file.path(path, files))]
  if (length(missing)) {
    return(invalid(paste0(
      "manifest-listed Parquet file is missing: ", basename(missing[[1L]])
    )))
  }

  con <- tryCatch(connect_duckdb(":memory:"), error = function(e) NULL)
  if (is.null(con)) {
    return(invalid("temporary read-only validation connection failed"))
  }
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  table_results <- lapply(seq_along(files), function(i) {
    file <- file.path(path, files[[i]])
    table <- tables[[i]]
    tryCatch({
      relation <- sprintf(
        "read_parquet(%s)",
        DBI::dbQuoteString(con, normalizePath(file, mustWork = TRUE))
      )
      actual <- DBI::dbGetQuery(
        con,
        paste("DESCRIBE SELECT * FROM", relation)
      )
      rows <- DBI::dbGetQuery(
        con, paste("SELECT count(*) AS n FROM", relation)
      )$n[[1L]]
      if (!.ehr_schema_contract_matches(actual, schemas[[table]])) {
        return(invalid(paste0(
          "Parquet schema mismatch for table '", table, "'"
        )))
      }
      if (!identical(
        as.numeric(rows), as.numeric(row_counts[[table]])
      )) {
        return(invalid(paste0(
          "Parquet row-count mismatch for table '", table, "'"
        )))
      }
      list(ok = TRUE, reason = "valid")
    }, error = function(e) invalid(paste0(
      "Parquet file is unreadable for table '", table, "'"
    )))
  })
  first_invalid <- which(!vapply(
    table_results, function(x) isTRUE(x$ok), logical(1L)
  ))
  if (length(first_invalid)) {
    return(table_results[[first_invalid[[1L]]]])
  }
  hierarchy_position <- match("cohort_hierarchy", tables)
  if (is.na(hierarchy_position)) {
    return(invalid("cohort_hierarchy is absent from the manifest"))
  }
  hierarchy_file <- file.path(
    path, files[[hierarchy_position]]
  )
  hierarchy_relation <- sprintf(
    "read_parquet(%s)",
    DBI::dbQuoteString(
      con, normalizePath(hierarchy_file, mustWork = TRUE)
    )
  )
  if (!.ehr_hierarchy_contract_valid(
    con, hierarchy_relation, manifest$hierarchy_rows
  )) {
    return(invalid("cohort_hierarchy index contract failed"))
  }
  meta_position <- match("cohort_meta", tables)
  if (!is.na(meta_position)) {
    meta <- tryCatch(DBI::dbGetQuery(con, sprintf(
      "SELECT * FROM read_parquet(%s)", DBI::dbQuoteString(
        con, normalizePath(file.path(path, files[[meta_position]]),
                           mustWork = TRUE)))), error = function(e) NULL)
    stale <- .ehr_stale_selection_reason(meta)
    if (!is.null(stale)) return(invalid(stale))
  }
  list(ok = TRUE, reason = "valid")
}

.ehr_validate_parquet_artifact <- function(path) {
  isTRUE(.ehr_check_parquet_artifact(path)$ok)
}

.ehr_validate_duckdb_artifact <- function(path, schema) {
  if (!file.exists(path)) return(FALSE)
  con <- tryCatch(connect_duckdb(path, read_only = TRUE),
                  error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (!.duckdb_table_exists(con, schema, "cohort_manifest")) return(FALSE)
  manifest <- tryCatch(DBI::dbGetQuery(
    con, sprintf(
      "SELECT * FROM %s",
      DBI::dbQuoteIdentifier(
        con, DBI::Id(schema = schema, table = "cohort_manifest")
      )
    )
  ), error = function(e) NULL)
  if (is.null(manifest) || nrow(manifest) != 1L ||
      !identical(manifest$status[[1L]], "success")) {
    return(FALSE)
  }
  tables <- tryCatch(
    unlist(jsonlite::fromJSON(manifest$tables_json[[1L]])),
    error = function(e) character()
  )
  schemas <- tryCatch(
    jsonlite::fromJSON(
      manifest$schemas_json[[1L]], simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  row_counts <- tryCatch(
    unlist(jsonlite::fromJSON(manifest$row_counts_json[[1L]])),
    error = function(e) numeric()
  )
  meta <- if ("cohort_meta" %in% tables) tryCatch(DBI::dbGetQuery(
    con, sprintf("SELECT * FROM %s", DBI::dbQuoteIdentifier(
      con, DBI::Id(schema = schema, table = "cohort_meta")))),
    error = function(e) NULL)
  length(tables) > 0L &&
    is.null(.ehr_stale_selection_reason(meta)) &&
    !is.null(schemas) &&
    identical(sort(names(schemas)), sort(tables)) &&
    identical(sort(names(row_counts)), sort(tables)) &&
    all(vapply(tables, function(table) {
      if (!.duckdb_table_exists(con, schema, table)) return(FALSE)
      ref <- as.character(DBI::dbQuoteIdentifier(
        con, DBI::Id(schema = schema, table = table)
      ))
      actual <- tryCatch(
        DBI::dbGetQuery(con, paste("DESCRIBE", ref)),
        error = function(e) NULL
      )
      rows <- tryCatch(
        .duckdb_row_count(con, schema, table),
        error = function(e) NA_real_
      )
      !is.null(actual) &&
        .ehr_schema_contract_matches(actual, schemas[[table]]) &&
        identical(as.numeric(rows), as.numeric(row_counts[[table]]))
    }, logical(1L))) &&
    .ehr_hierarchy_contract_valid(
      con,
      as.character(DBI::dbQuoteIdentifier(
        con, DBI::Id(schema = schema, table = "cohort_hierarchy")
      )),
      manifest$hierarchy_rows[[1L]]
    )
}

.ehr_available_bytes <- function(path) {
  override <- getOption("ehr.cohort.available_bytes")
  if (!is.null(override)) return(as.numeric(override))
  output <- tryCatch(
    system2("df", c("-Pk", path), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(output) < 2L) return(NA_real_)
  fields <- strsplit(trimws(utils::tail(output, 1L)), "[[:space:]]+")[[1L]]
  if (length(fields) < 4L) return(NA_real_)
  suppressWarnings(as.numeric(fields[[4L]]) * 1024)
}

.ehr_check_staging_space <- function(raw_files, destination_parent) {
  bytes <- sum(file.info(unlist(raw_files, use.names = FALSE))$size,
               na.rm = TRUE)
  needed <- max(1024^2, ceiling(bytes * 1.25))
  available <- .ehr_available_bytes(destination_parent)
  if (!is.na(available) && available < needed) {
    stop("Insufficient free space for safe cohort staging: estimated ",
         needed, " bytes required and ", available,
         " bytes available.", call. = FALSE)
  }
  invisible(list(required = needed, available = available))
}

.ehr_remove_owned_path <- function(path) {
  if (!file.exists(path) && !dir.exists(path)) return(invisible(FALSE))
  name <- basename(path)
  if (!grepl("[.](stage|recovery|work)-", name)) {
    stop("Refusing to remove a path not owned by cohort staging.",
         call. = FALSE)
  }
  unlink(path, recursive = dir.exists(path), force = TRUE)
  invisible(TRUE)
}

.ehr_publish_cohort_artifact <- function(stage, final) {
  backup <- tempfile(
    pattern = paste0(".", basename(final), ".recovery-"),
    tmpdir = dirname(final)
  )
  had_final <- file.exists(final) || dir.exists(final)
  if (had_final && !file.rename(final, backup)) {
    stop("Could not move the current cohort to temporary recovery storage.",
         call. = FALSE)
  }
  published <- file.rename(stage, final)
  if (!published) {
    if (had_final) file.rename(backup, final)
    stop("Could not publish the staged cohort; the previous cohort was restored.",
         call. = FALSE)
  }
  if (had_final) .ehr_remove_owned_path(backup)
  invisible(final)
}

# Deterministic string fingerprint of the inputs that define what a staged
# build actually contains (selection, tables, chunking) - NOT execution
# tuning knobs like memory_limit/threads/show_progress, which don't affect
# output. Used to decide whether a leftover staging DB from a prior attempt
# is safe to resume from, or must be discarded because the request changed.
.ehr_build_fingerprint <- function(fields) {
  safe <- lapply(fields, function(x) {
    if (is.function(x)) paste(deparse(x), collapse = "\n") else x
  })
  paste(deparse(safe), collapse = "")
}

# Identity of the cohort a build selects: only the inputs that decide which
# rows end up in the artifact, so tuning knobs (chunk size, column
# validation, output location) never make a valid artifact look foreign.
# Files named by path are identified by normalized path plus content hash,
# so editing an ICD list or ID file in place also counts as a new selection.
.ehr_selection_identity <- function(fields) {
  for (nm in c("icd_filter_path", "direct_ids_path", "cohort_sql",
               "setup_sql", "window_sql")) {
    x <- fields[[nm]]
    if (is.character(x) && length(x) == 1L && !is.na(x) &&
        file.exists(x) && !dir.exists(x)) {
      fields[[nm]] <- normalizePath(x, winslash = "/", mustWork = TRUE)
      fields[[paste0(nm, "_md5")]] <- unname(tools::md5sum(x))
    }
  }
  .ehr_build_fingerprint(fields)
}

# Sidecar next to a published artifact recording the selection it was built
# from; read back before an existing artifact is reused.
.ehr_selection_file <- function(final) {
  file.path(dirname(final), paste0(".", basename(final), ".selection"))
}

# Appends one row per successful build to a durable, ever-growing log at
# {base_dir}/data/build_metrics_log.csv. Unlike cohort_manifest.json /
# cohort_table_report (both scoped to one cohort artifact and overwritten by
# every rebuild), this survives across rebuilds and across different
# sources/cohorts in one place, so build time and cache effectiveness can
# actually be compared over time instead of only ever showing the latest
# build's snapshot - e.g. "did adopting the parquet cache actually reduce
# build time run over run" becomes a question the log can answer directly.
# Never allowed to fail the build itself: logging is instrumentation, not a
# build requirement.
.ehr_append_build_log <- function(base_dir, src_name, output, plan, table_report) {
  log_path <- file.path(base_dir, "data", "build_metrics_log.csv")
  used_cache <- if ("used_parquet_cache" %in% names(table_report)) {
    table_report$used_parquet_cache
  } else {
    rep(NA, nrow(table_report))
  }
  n_chunks <- if ("n_chunks" %in% names(table_report)) {
    table_report$n_chunks
  } else {
    rep(NA_integer_, nrow(table_report))
  }
  row <- data.frame(
    logged_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    src_name          = src_name,
    output_kind       = output,
    route             = as.character(plan$route %||% NA_character_),
    table_count       = nrow(table_report),
    total_rows        = sum(table_report$rows, na.rm = TRUE),
    total_seconds     = round(sum(table_report$seconds, na.rm = TRUE), 1),
    tables_from_cache = sum(used_cache, na.rm = TRUE),
    tables_from_csv   = sum(!used_cache, na.rm = TRUE),
    max_chunks        = suppressWarnings(max(c(1L, n_chunks), na.rm = TRUE)),
    stringsAsFactors  = FALSE
  )
  tryCatch({
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    write_header <- !file.exists(log_path)
    readr::write_csv(row, log_path, append = !write_header,
                     col_names = write_header)
  }, error = function(e) {
    warning("Could not append to the build metrics log at ", log_path, ": ",
            conditionMessage(e), call. = FALSE)
  })
  invisible(row)
}

.ehr_run_cohort_engine <- function(output = c("parquet", "duckdb"),
                                   output_path = NULL,
                                   src_name = NULL, base_dir = NULL,
                                   raw_data_root = NULL,
                                   icd_filter_path = NULL,
                                   direct_ids_path = NULL,
                                   cohort = NULL,
                                   cohort_sql = NULL, setup_sql = NULL,
                                   cohort_fn = NULL, no_filter = FALSE,
                                   key = NULL, import_scope = NULL,
                                   allow_unmatched_ids = FALSE,
                                   window_sql = NULL, window_fn = NULL,
                                   schema = NULL, tables = NULL,
                                   memory_limit = NULL, threads = NULL,
                                   temp_directory = NULL,
                                   table_chunk_rows = NULL,
                                   validate_columns = FALSE,
                                   ignore_errors = TRUE,
                                   store_rejects = TRUE,
                                   use_parquet_cache = TRUE,
                                   overwrite = FALSE,
                                   show_progress = TRUE) {
  output <- match.arg(output)
  paths <- ehr_paths(
    src_name = src_name, base_dir = base_dir,
    raw_data_root = raw_data_root
  )
  src_name <- paths$src_name
  schema <- schema %||% src_schema_names(src_name)$raw
  final <- output_path %||% if (identical(output, "parquet"))
    paths$cohort_parquet_dir else paths$duckdb_path
  dir.create(dirname(final), recursive = TRUE, showWarnings = FALSE)

  # Under the SCI profile, the project default is the bundled SCI reference
  # list. Treat it as the named "scid" cohort for selection identity, so an
  # explicit import_cohort_general(cohort = "scid") and a later default
  # run_one_source() reuse one artifact instead of reporting a conflict.
  identity_cohort <- cohort
  if (is.null(icd_filter_path) && is.null(direct_ids_path) &&
      is.null(cohort_sql) && is.null(cohort_fn) && is.null(cohort) &&
      !isTRUE(no_filter) && identical(paths$cohort_profile, "scid")) {
    pkg_scid <- system.file("extdata", "scid_icds_rendehr.rds", package = "rendehR")
    if (nzchar(pkg_scid) && file.exists(pkg_scid) &&
        !is.null(paths$icd_filter_path) && file.exists(paths$icd_filter_path) &&
        identical(unname(tools::md5sum(paths$icd_filter_path)),
                  unname(tools::md5sum(pkg_scid)))) {
      identity_cohort <- "scid"
    }
  }
  selection_fields <- list(
    src_name = src_name, icd_filter_path = icd_filter_path,
    direct_ids_path = direct_ids_path, cohort = identity_cohort,
    cohort_sql = cohort_sql, setup_sql = setup_sql, cohort_fn = cohort_fn,
    no_filter = no_filter, key = key, import_scope = import_scope,
    allow_unmatched_ids = allow_unmatched_ids, window_sql = window_sql,
    window_fn = window_fn, schema = schema, tables = tables
  )
  if (identical(src_family(src_name), "eicu"))
    selection_fields$selection_semantics <- "icd_hospital_grain_2026_09_25"
  selection_id <- .ehr_selection_identity(selection_fields)
  selection_file <- .ehr_selection_file(final)

  valid_existing <- if (identical(output, "parquet")) {
    .ehr_validate_parquet_artifact(final)
  } else {
    .ehr_validate_duckdb_artifact(final, schema)
  }
  # An empty directory (ehr_init_project() creates one) holds no cohort to protect.
  empty_dir <- dir.exists(final) && !length(list.files(final, all.files = TRUE, no.. = TRUE))
  if ((dir.exists(final) || file.exists(final)) && !empty_dir && !isTRUE(overwrite)) {
    if (valid_existing) {
      prior_selection <- if (file.exists(selection_file))
        readLines(selection_file, warn = FALSE, n = 1L) else character(0)
      if (length(prior_selection) && !identical(prior_selection, selection_id)) {
        stop("The existing cohort artifact at ", final, " was built from a ",
             "different cohort selection than this call requests. Set ",
             "`overwrite = TRUE` to rebuild it, or use a separate base_dir.",
             call. = FALSE)
      }
      if (!length(prior_selection)) {
        warning("The existing cohort artifact at ", final, " predates ",
                "selection tracking, so it cannot be confirmed to match this ",
                "call's cohort selection. Set `overwrite = TRUE` to rebuild ",
                "it if the selection has changed.", call. = FALSE)
      }
      if (show_progress) message("Current cohort is complete; no rebuild needed.")
      return(invisible(list(status = "existing", path = final)))
    }
    reason <- if (identical(output, "parquet"))
      .ehr_check_parquet_artifact(final)$reason else NULL
    stop("The current cohort artifact is incomplete or invalid",
         if (!is.null(reason)) paste0(" (", reason, ")"), ". Set ",
         "`overwrite = TRUE` to replace it safely.", call. = FALSE)
  }

  cfg <- load_src_cfg(paths$data_config_json, src_name)
  raw_files <- .ehr_table_file_map(cfg, paths)
  .ehr_check_staging_space(raw_files, dirname(final))

  # Stable (not random-tempfile) staging path so a later call can find the
  # same location and resume it. Never deleted on failure - only on a
  # successful publish, or when the fingerprint below shows it belongs to a
  # different request and must be discarded.
  stage <- file.path(
    dirname(final),
    paste0(".", basename(final), ".stage-checkpoint",
           if (identical(output, "duckdb")) ".duckdb" else "")
  )
  fingerprint_file <- paste0(stage, ".fingerprint")
  fingerprint_fields <- list(
    output = output, output_path = output_path, src_name = src_name,
    icd_filter_path = icd_filter_path, direct_ids_path = direct_ids_path,
    cohort = cohort, cohort_sql = cohort_sql, setup_sql = setup_sql,
    cohort_fn = cohort_fn, no_filter = no_filter, key = key,
    import_scope = import_scope, allow_unmatched_ids = allow_unmatched_ids,
    window_sql = window_sql, window_fn = window_fn, schema = schema,
    tables = tables, table_chunk_rows = table_chunk_rows,
    validate_columns = validate_columns
  )
  # eICU ICD selection uses hospitalization grain, not unit grain; this field
  # keeps a unit-grain staging checkpoint from being resumed. It is set for
  # eICU only, so other families' fingerprints (and checkpoints) are unchanged.
  if (identical(src_family(src_name), "eicu"))
    fingerprint_fields$selection_semantics <- "icd_hospital_grain_2026_09_25"
  fingerprint <- .ehr_build_fingerprint(fingerprint_fields)
  if (file.exists(stage) || dir.exists(stage)) {
    prior_fingerprint <- tryCatch(
      readLines(fingerprint_file, warn = FALSE, n = 1L),
      error = function(e) NA_character_, warning = function(w) NA_character_
    )
    if (!length(prior_fingerprint) || !identical(prior_fingerprint, fingerprint)) {
      if (show_progress) {
        message(
          "Discarding an incompatible in-progress staging build (different ",
          "selection/settings than this call) and starting fresh."
        )
      }
      .ehr_remove_owned_path(stage)
      if (file.exists(fingerprint_file)) unlink(fingerprint_file, force = TRUE)
    } else if (show_progress) {
      message("Resuming an in-progress staging build from the last attempt.")
    }
  }
  writeLines(fingerprint, fingerprint_file)

  if (identical(output, "parquet")) {
    dir.create(stage, recursive = TRUE, showWarnings = FALSE)
    work_db <- file.path(stage, "cohort.work.duckdb")
    result <- .ehr_build_artifact(
      work_db, parquet_dir = stage,
      src_name = src_name, base_dir = base_dir,
      raw_data_root = raw_data_root,
      icd_filter_path = icd_filter_path,
      cohort_sql = cohort_sql, setup_sql = setup_sql,
      cohort_fn = cohort_fn, direct_ids_path = direct_ids_path,
      cohort = cohort, no_filter = no_filter, key = key,
      import_scope = import_scope,
      allow_unmatched_ids = allow_unmatched_ids,
      window_sql = window_sql, window_fn = window_fn,
      schema = schema, tables = tables,
      memory_limit = memory_limit, threads = threads,
      temp_directory = temp_directory,
      table_chunk_rows = table_chunk_rows,
      validate_columns = validate_columns,
      ignore_errors = ignore_errors,
      store_rejects = store_rejects,
      use_parquet_cache = use_parquet_cache,
      show_progress = show_progress
    )
    staged_check <- .ehr_check_parquet_artifact(stage)
    if (!isTRUE(staged_check$ok)) {
      stop(
        "Staged Parquet cohort failed completion validation: ",
        staged_check$reason, ".",
        call. = FALSE
      )
    }
    # Only discard the working DuckDB (and its checkpoint log) once we know
    # the staged parquet output is actually complete.
    unlink(c(work_db, paste0(work_db, ".wal")), force = TRUE)
  } else {
    result <- .ehr_build_artifact(
      stage, parquet_dir = NULL,
      src_name = src_name, base_dir = base_dir,
      raw_data_root = raw_data_root,
      icd_filter_path = icd_filter_path,
      cohort_sql = cohort_sql, setup_sql = setup_sql,
      cohort_fn = cohort_fn, direct_ids_path = direct_ids_path,
      cohort = cohort, no_filter = no_filter, key = key,
      import_scope = import_scope,
      allow_unmatched_ids = allow_unmatched_ids,
      window_sql = window_sql, window_fn = window_fn,
      schema = schema, tables = tables,
      memory_limit = memory_limit, threads = threads,
      temp_directory = temp_directory,
      table_chunk_rows = table_chunk_rows,
      validate_columns = validate_columns,
      ignore_errors = ignore_errors,
      store_rejects = store_rejects,
      use_parquet_cache = use_parquet_cache,
      show_progress = show_progress
    )
    if (!.ehr_validate_duckdb_artifact(stage, schema)) {
      stop("Staged DuckDB cohort failed completion validation.",
           call. = FALSE)
    }
  }

  .ehr_publish_cohort_artifact(stage, final)
  writeLines(selection_id, selection_file)
  unlink(fingerprint_file, force = TRUE)
  .ehr_append_build_log(paths$base_dir, src_name, output, result$plan,
                        result$table_report)
  if (show_progress) message("Published current ", output, " cohort: ", final)
  invisible(c(result, list(status = "built", path = final)))
}

.ehr_select_cohort_plan <- function(src_name, base_dir = NULL,
                                    raw_data_root = NULL,
                                    icd_filter_path = NULL,
                                    cohort_sql = NULL, setup_sql = NULL,
                                    cohort_fn = NULL,
                                    direct_ids_path = NULL,
                                    cohort = NULL, no_filter = FALSE,
                                    key = NULL, import_scope = NULL,
                                    allow_unmatched_ids = FALSE,
                                    window_sql = NULL, window_fn = NULL,
                                    out_csv = NULL) {
  paths <- ehr_paths(
    src_name = src_name, base_dir = base_dir,
    raw_data_root = raw_data_root
  )
  cfg <- load_src_cfg(paths$data_config_json, paths$src_name)
  ladder <- .cohort_id_ladder(paths$src_name)
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .duckdb_configure_raw_csv_connection(
    con, .duckdb_is_demo_source(paths$src_name)
  )
  .ehr_prepare_source_views(con, cfg, paths, ladder)
  plan <- .ehr_resolve_selection(
    con, paths$src_name, paths, ladder,
    icd_filter_path, cohort_sql, setup_sql, cohort_fn,
    direct_ids_path, cohort, no_filter, key, import_scope,
    allow_unmatched_ids
  )
  .ehr_build_full_spine(con, paths, window_sql, window_fn)
  plan <- .ehr_match_plan_to_spine(con, plan, ladder)
  .ehr_atomic_write_ids(plan$values, plan$selection_key, out_csv)
  plan
}

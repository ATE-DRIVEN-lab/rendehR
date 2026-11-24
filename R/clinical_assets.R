# Internal clinical-asset and diagnosis-normalization helpers.

.clinical_manifest_filename <- "rendehr-clinical-assets.json"

.clinical_asset_manifest_path <- function(path = NULL) {
  path %||% ehr_config_path(.clinical_manifest_filename)
}

.clinical_sha256 <- function(path) {
  if (!file.exists(path)) stop("Clinical asset not found: ", path, call. = FALSE)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  paste0(openssl::sha256(con))
}

.load_clinical_asset_manifest <- function(path = NULL) {
  path <- .clinical_asset_manifest_path(path)
  if (!file.exists(path)) {
    stop("Clinical-asset manifest not found: ", path, call. = FALSE)
  }
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(manifest$format_version, 1L) || !is.list(manifest$assets)) {
    stop("Clinical-asset manifest has an unsupported schema: ", path,
         call. = FALSE)
  }
  manifest
}

.clinical_manifest_entry <- function(manifest, path, asset_id = NULL) {
  filename <- basename(path)
  hits <- Filter(function(x) {
    (is.null(asset_id) || identical(x$id, asset_id)) &&
      identical(x$filename, filename)
  }, manifest$assets)
  if (length(hits) > 1L) {
    stop("Clinical-asset manifest contains duplicate entries for ", filename,
         call. = FALSE)
  }
  if (length(hits) == 0L) return(NULL)
  hits[[1L]]
}

.clinical_missing_value <- function(x) {
  is.na(x) | (!is.na(x) & !nzchar(trimws(as.character(x))))
}

.validate_clinical_asset <- function(df, path, required_columns,
                                     asset_id = NULL,
                                     manifest = NULL,
                                     manifest_required = TRUE) {
  missing <- setdiff(required_columns, names(df))
  if (length(missing)) {
    stop("Clinical asset ", basename(path), " is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  if (is.null(manifest)) {
    manifest_path <- .clinical_asset_manifest_path()
    if (!file.exists(manifest_path) && !isTRUE(manifest_required)) {
      return(invisible(df))
    }
    manifest <- .load_clinical_asset_manifest(manifest_path)
  }
  entry <- .clinical_manifest_entry(manifest, path, asset_id)
  if (is.null(entry)) {
    if (isTRUE(manifest_required)) {
      warning("Clinical asset ", path,
              " is an explicit override not listed in the packaged manifest; ",
              "schema validation was applied but provenance/hash remain unverified.",
              call. = FALSE)
    }
    return(invisible(df))
  }

  required_metadata <- c(
    "id", "filename", "source", "edition", "citation", "license", "sha256",
    "generation_method", "required_columns",
    "required_nonmissing", "key", "key_unique", "multiplicity", "domains",
    "limitations", "verification_status")
  missing_metadata <- required_metadata[!vapply(
    required_metadata, function(field) !is.null(entry[[field]]), logical(1))]
  if (length(missing_metadata)) {
    stop("Clinical-asset manifest entry `", entry$id,
         "` is missing metadata field(s): ",
         paste(missing_metadata, collapse = ", "), call. = FALSE)
  }

  declared <- unlist(entry$required_columns, use.names = FALSE)
  missing_declared <- setdiff(declared, names(df))
  if (length(missing_declared)) {
    stop("Clinical asset ", entry$id, " violates its manifest schema: missing ",
         paste(missing_declared, collapse = ", "), call. = FALSE)
  }

  nonmissing <- unlist(entry$required_nonmissing, use.names = FALSE)
  for (column in intersect(nonmissing, names(df))) {
    if (any(.clinical_missing_value(df[[column]]))) {
      stop("Clinical asset ", entry$id, " has missing values in required column `",
           column, "`.", call. = FALSE)
    }
  }

  known_missing <- entry$known_missing_counts %||% list()
  if (length(known_missing)) {
    if (is.null(names(known_missing)) || any(!nzchar(names(known_missing)))) {
      stop("Clinical asset ", entry$id,
           " has an invalid known_missing_counts declaration.", call. = FALSE)
    }
    missing_known_columns <- setdiff(names(known_missing), names(df))
    if (length(missing_known_columns)) {
      stop("Clinical asset ", entry$id,
           " declares known missing counts for absent column(s): ",
           paste(missing_known_columns, collapse = ", "), call. = FALSE)
    }
    for (column in names(known_missing)) {
      expected <- unlist(known_missing[[column]], use.names = FALSE)
      valid_expected <- length(expected) == 1L && is.numeric(expected) &&
        is.finite(expected) && expected >= 0 &&
        identical(as.numeric(expected), as.numeric(as.integer(expected)))
      if (!valid_expected) {
        stop("Clinical asset ", entry$id,
             " has an invalid known missing count for column `", column,
             "`.", call. = FALSE)
      }
      observed <- sum(.clinical_missing_value(df[[column]]))
      if (!identical(as.integer(observed), as.integer(expected))) {
        stop("Clinical asset ", entry$id,
             " known-missing count for column `", column,
             "` differs from its manifest declaration (expected ",
             as.integer(expected), ", observed ", observed, ").",
             call. = FALSE)
      }
    }
  }

  key <- unlist(entry$key, use.names = FALSE)
  missing_key <- setdiff(key, names(df))
  if (length(missing_key)) {
    stop("Clinical asset ", entry$id, " is missing declared key column(s): ",
         paste(missing_key, collapse = ", "), call. = FALSE)
  }
  if (isTRUE(entry$key_unique) && anyDuplicated(df[key])) {
    stop("Clinical asset ", entry$id,
         " violates its declared unique key: ", paste(key, collapse = ", "),
         call. = FALSE)
  }

  domains <- entry$domains %||% list()
  for (column in intersect(names(domains), names(df))) {
    allowed <- as.character(unlist(domains[[column]], use.names = FALSE))
    observed <- unique(as.character(df[[column]][!is.na(df[[column]])]))
    invalid <- setdiff(observed, allowed)
    if (length(invalid)) {
      stop("Clinical asset ", entry$id, " has out-of-domain `", column,
           "` value(s): ", paste(utils::head(invalid, 5L), collapse = ", "),
           call. = FALSE)
    }
  }

  expected_hash <- as.character(entry$sha256 %||% "")
  if (nzchar(expected_hash)) {
    actual_hash <- .clinical_sha256(path)
    if (!identical(tolower(actual_hash), tolower(expected_hash))) {
      stop("Clinical asset ", entry$id, " failed SHA-256 validation.",
           call. = FALSE)
    }
  }
  invisible(df)
}

.read_clinical_asset <- function(path, required_columns, asset_id = NULL,
                                 manifest_required = TRUE) {
  ext <- tolower(tools::file_ext(path))
  df <- if (identical(ext, "rds")) {
    readRDS(path)
  } else {
    # ICD codes are text: guessing would read an all-digit column (ICD-9
    # procedures, a user list) as numbers and drop leading zeros ("0331").
    hdr <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE,
                                 name_repair = "unique_quiet"))
    code_cols <- grep("code", hdr, value = TRUE, ignore.case = TRUE)
    readr::read_csv(path, show_col_types = FALSE, name_repair = "unique_quiet",
                    col_types = do.call(readr::cols, c(
                      stats::setNames(rep(list(readr::col_character()), length(code_cols)), code_cols),
                      list(.default = readr::col_guess()))))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  .validate_clinical_asset(df, path, required_columns, asset_id,
                           manifest_required = manifest_required)
  tibble::as_tibble(df)
}

.icd_raw_token <- function(x) {
  value <- toupper(trimws(as.character(x)))
  value[is.na(x) | !nzchar(value)] <- NA_character_
  value
}

.icd_compact_token <- function(x) {
  raw <- .icd_raw_token(x)
  out <- stringr::str_replace_all(raw, "\\.", "")
  out[!is.na(out) & !stringr::str_detect(out, "^[A-Z0-9]+$")] <- NA_character_
  out
}

.icd_dotted_from_compact <- function(code, version) {
  code <- .icd_compact_token(code)
  version <- as.character(version)
  out <- code
  needs_dot <- !is.na(code) & !stringr::str_detect(code, "\\.")
  split_at <- ifelse(
    version == "9" & stringr::str_detect(code %||% "", "^E"), 4L, 3L)
  add <- needs_dot & nchar(code) > split_at
  out[add] <- paste0(substr(code[add], 1L, split_at[add]), ".",
                     substr(code[add], split_at[add] + 1L, nchar(code[add])))
  out
}

# Validate and normalize ICD version labels to "9" / "10". Each value is
# checked against the explicitly accepted formats *before* normalizing: 9, 10,
# ICD9, ICD10, ICD-9, ICD-10, in any case, with surrounding whitespace. Anything
# else - missing, blank, negative (-9), doubled hyphens (ICD--10), other
# numbers - is an error; offending values are counted, not shown. This is the
# one version normalizer for ICD filters (selection and annotation), published
# filter dictionaries, and packaged clinical assets.
.icd_version_token <- function(x, arg_name = "icd_version") {
  v <- toupper(trimws(as.character(x)))
  ok <- !is.na(v) & grepl("^(ICD-?)?(9|10)$", v)
  if (any(!ok)) {
    stop(arg_name, " has ", sum(!ok), " missing, blank or unsupported ",
         "value(s); accepted: 9, 10, ICD9, ICD10, ICD-9, ICD-10.",
         call. = FALSE)
  }
  sub("^ICD-?", "", v)
}

.normalize_icd_asset <- function(df) {
  raw_code <- .icd_raw_token(df$icd_code)
  df$icd_code <- .icd_compact_token(raw_code)
  if ("icd_version" %in% names(df)) {
    df$icd_version <- .icd_version_token(df$icd_version, "ICD asset icd_version")
    df$icd_code_compact <- df$icd_code
    df$icd_code_dotted <- ifelse(
      !is.na(raw_code) & stringr::str_detect(raw_code, "\\."), raw_code,
      .icd_dotted_from_compact(df$icd_code, df$icd_version))
  }
  df
}

.load_eicu_icd_adjudication <- function(path = NULL) {
  path <- path %||% ehr_config_path(
    "eicu_icd_version_adjudication_rendehr.csv")
  df <- .read_clinical_asset(
    path,
    c("raw_code", "compact_code", "selected_version", "review_status",
      "rationale"),
    "eicu_icd_version_adjudication"
  )
  df <- df %>%
    dplyr::mutate(
      raw_code = .icd_raw_token(raw_code),
      compact_code = .icd_compact_token(compact_code),
      selected_version = as.character(selected_version),
      review_status = tolower(trimws(as.character(review_status)))
    )
  approved <- df$review_status == "approved"
  if (any(approved & !df$selected_version %in% c("9", "10"), na.rm = TRUE)) {
    stop("Approved eICU ICD adjudications must select version 9 or 10.",
         call. = FALSE)
  }
  if (anyDuplicated(df[c("raw_code", "compact_code")])) {
    stop("eICU ICD adjudication keys must be unique.", call. = FALSE)
  }
  df
}

.collapse_sorted_values <- function(x) {
  value <- trimws(as.character(x))
  value <- sort(unique(value[!is.na(value) & nzchar(value)]))
  if (length(value)) paste(value, collapse = " | ") else NA_character_
}

.clinical_diag_status <- function(diag, id_col) {
  if (!id_col %in% names(diag)) {
    stop("Diagnosis input is missing id column `", id_col, "`.", call. = FALSE)
  }
  raw <- if ("icd_code_raw" %in% names(diag)) diag$icd_code_raw else diag$icd_code
  usable <- !is.na(.icd_raw_token(raw))
  compact <- if ("icd_code" %in% names(diag)) {
    .icd_compact_token(diag$icd_code)
  } else .icd_compact_token(raw)
  resolved <- usable & !is.na(compact) &
    as.character(diag$icd_version) %in% c("9", "10")
  excluded <- usable & !resolved
  status_input <- data.frame(
    id = diag[[id_col]], usable = usable, resolved = resolved,
    excluded = excluded, stringsAsFactors = FALSE
  )
  names(status_input)[[1L]] <- id_col
  status_input %>%
    dplyr::filter(!is.na(.data[[id_col]])) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      .has_usable = any(usable),
      .has_resolved = any(resolved),
      .has_excluded = any(excluded),
      .groups = "drop"
    ) %>%
    dplyr::mutate(.clinical_status = dplyr::case_when(
      .has_resolved & .has_excluded ~ "scored_with_exclusions",
      .has_resolved ~ "scored",
      .has_usable ~ "unresolved_diagnosis",
      TRUE ~ "missing_diagnosis"
    ))
}

.clinical_status_for_universe <- function(universe, diag, id_col) {
  status <- .clinical_diag_status(diag, id_col)
  universe %>%
    dplyr::distinct(.data[[id_col]]) %>%
    dplyr::left_join(status, by = id_col) %>%
    dplyr::mutate(
      .has_usable = tidyr::replace_na(.has_usable, FALSE),
      .has_resolved = tidyr::replace_na(.has_resolved, FALSE),
      .has_excluded = tidyr::replace_na(.has_excluded, FALSE),
      .clinical_status = tidyr::replace_na(.clinical_status,
                                            "missing_diagnosis")
    )
}

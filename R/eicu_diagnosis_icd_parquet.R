#' Build a normalized eICU diagnosis ICD parquet table from CSV files
#'
#' eICU stores one or more comma-separated ICD codes in `diagnosis.icd9code`
#' and does not provide an explicit ICD version. This utility expands those
#' codes to one row per diagnosis-code pair, joins patient-level identifiers,
#' resolves each code's ICD version, and writes a standalone parquet file for
#' audit and inspection.
#'
#' Versions are resolved by the package's single eICU resolver - the same one
#' used for cohort selection, `cohort_info`, comorbidities and ISS - so every
#' code gets exactly the version the pipeline gives it: an exact match of the
#' dotted token against the dictionary, a compact match only for tokens written
#' without a dot, then an approved expert adjudication. Codes that are official
#' in both versions stay `ambiguous`, and codes that match nothing stay
#' `unmatched`; neither gets a version. There is no regular-expression
#' fallback: a regex guess is reported separately, in
#' `regex_version_suggestion`, for diagnosis only. This utility is not the
#' canonical eICU cohort selector and does not determine cohort membership.
#'
#' Output columns include `icd_code_raw` (the source token), `icd_code`
#' (compact), `icd_version` (`"9"`, `"10"` or NA), `version_source` (the
#' resolution status: `exact_dotted`, `compact_unique`, `expert_adjudicated`,
#' `ambiguous`, `unmatched` or `malformed`), `is_ambiguous`, `match_n` and
#' `long_titles_all` (the dictionary candidates the evidence points to, so
#' ambiguity stays visible), `long_title`, and `regex_version_suggestion`.
#'
#' @param diagnosis_csv Path to eICU `diagnosis.csv` or `diagnosis.csv.gz`.
#' @param patient_csv Path to eICU `patient.csv` or `patient.csv.gz`.
#' @param icd_dict_path Path to diagnosis dictionary CSV/RDS with `icd_code`,
#'   `icd_version` (9 or 10), and optionally `long_title`.
#' @param output_path Destination parquet file, or a directory where
#'   `diagnosis_icd_mod.parquet` should be written.
#' @param overwrite Overwrite an existing parquet file? Default `FALSE`.
#' @param icd_adjudication_path Optional reviewed eICU ICD-version crosswalk.
#'   Defaults to the packaged crosswalk, as in the pipeline.
#' @return Invisibly returns the written parquet file path.
#' @examples
#' \dontrun{
#' build_eicu_diagnosis_icd_parquet(
#'   diagnosis_csv = "data/source_data/eicu/diagnosis.csv.gz",
#'   patient_csv = "data/source_data/eicu/patient.csv.gz",
#'   icd_dict_path = system.file("extdata", "d_icd_diagnoses.csv", package = "rendehR"),
#'   output_path = "data/eicu_diagnosis_icd.parquet")
#' }
#' @export
build_eicu_diagnosis_icd_parquet <- function(diagnosis_csv,
                                             patient_csv,
                                             icd_dict_path,
                                             output_path,
                                             overwrite = FALSE,
                                             icd_adjudication_path = NULL) {
  if (!file.exists(diagnosis_csv))
    stop("diagnosis_csv does not exist: ", diagnosis_csv, call. = FALSE)
  if (!file.exists(patient_csv))
    stop("patient_csv does not exist: ", patient_csv, call. = FALSE)
  if (!file.exists(icd_dict_path))
    stop("icd_dict_path does not exist: ", icd_dict_path, call. = FALSE)

  out_file <- .eicu_diagnosis_icd_output_file(output_path)
  if (file.exists(out_file) && !isTRUE(overwrite)) {
    stop("Output parquet already exists: ", out_file,
         "\nUse overwrite = TRUE to replace it.", call. = FALSE)
  }
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

  diag <- readr::read_csv(
    diagnosis_csv,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  pati <- readr::read_csv(
    patient_csv,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  .eicu_require_cols(diag, c("diagnosisid", "patientunitstayid", "icd9code"),
                     "diagnosis_csv")
  .eicu_require_cols(pati, c("patientunitstayid",
                             "patienthealthsystemstayid",
                             "uniquepid"),
                     "patient_csv")

  diag_exp <- diag |>
    dplyr::mutate(
      icd_codes = dplyr::na_if(as.character(.data$icd9code), "")
    ) |>
    dplyr::filter(!is.na(.data$icd_codes)) |>
    dplyr::mutate(icd_code_raw = stringr::str_split(.data$icd_codes, "\\s*,\\s*")) |>
    tidyr::unnest(cols = dplyr::all_of("icd_code_raw")) |>
    dplyr::mutate(
      icd_code_raw = stringr::str_squish(as.character(.data$icd_code_raw)),
      icd_code_raw = dplyr::na_if(.data$icd_code_raw, ""),
      icd_code_raw_norm = toupper(.data$icd_code_raw),
      icd_code = stringr::str_replace_all(.data$icd_code_raw_norm, "\\.", "")
    ) |>
    dplyr::filter(!is.na(.data$icd_code), .data$icd_code != "") |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::left_join(
      pati |>
        dplyr::select(dplyr::all_of(c("patientunitstayid",
                                      "patienthealthsystemstayid",
                                      "uniquepid"))) |>
        dplyr::distinct(),
      by = "patientunitstayid"
    )

  dict <- .eicu_read_icd_dict(icd_dict_path)
  resolved <- .eicu_resolve_icd_tokens(
    diag_exp$icd_code_raw_norm, dict,
    .load_eicu_icd_adjudication(icd_adjudication_path))

  # The dictionary candidates behind each token, from the same evidence the
  # resolver uses (the dotted form for dotted tokens, the compact code for
  # undotted ones), so the versions and titles in play stay visible.
  cand_dict <- dict |>
    dplyr::transmute(icd_code_compact = .data$icd_code,
                     icd_code_dotted = .data$icd_code_dotted,
                     icd_version = .data$icd_version,
                     long_title = .data$long_title) |>
    dplyr::distinct()
  tokens <- resolved |>
    dplyr::filter(!is.na(.data$icd_code_raw)) |>
    dplyr::mutate(undotted = !grepl(".", .data$icd_code_raw, fixed = TRUE))
  candidates <- dplyr::bind_rows(
    tokens |>
      dplyr::filter(!.data$undotted) |>
      dplyr::select(dplyr::all_of("icd_code_raw")) |>
      dplyr::inner_join(cand_dict, by = c("icd_code_raw" = "icd_code_dotted")),
    tokens |>
      dplyr::filter(.data$undotted) |>
      dplyr::select(dplyr::all_of(c("icd_code_raw", "icd_code_compact"))) |>
      dplyr::inner_join(cand_dict, by = "icd_code_compact")
  ) |>
    dplyr::group_by(.data$icd_code_raw) |>
    dplyr::summarise(
      match_n = dplyr::n_distinct(paste(.data$icd_version, .data$long_title,
                                        sep = "\r")),
      long_titles_all = paste(
        sort(unique(paste0(.data$icd_version, ": ", .data$long_title))),
        collapse = " | "),
      .groups = "drop")

  resolved <- resolved |>
    dplyr::select(dplyr::all_of(c("icd_code_raw", "icd_version", "long_title",
                                  "version_resolution_status"))) |>
    dplyr::left_join(candidates, by = "icd_code_raw")

  # `resolved` has one row per distinct token, so the join keeps every
  # diagnosis-code row exactly once.
  out <- diag_exp |>
    dplyr::left_join(resolved, by = c("icd_code_raw_norm" = "icd_code_raw")) |>
    dplyr::mutate(
      is_ambiguous = .data$version_resolution_status == "ambiguous",
      version_source = .data$version_resolution_status,
      match_n = as.integer(tidyr::replace_na(.data$match_n, 0L)),
      long_title = dplyr::na_if(.data$long_title, ""),
      long_titles_all = dplyr::na_if(.data$long_titles_all, ""),
      regex_version_suggestion = dplyr::case_when(
        !stringr::str_detect(.data$icd_code, "^[A-Z0-9]+$") ~ NA_character_,
        stringr::str_detect(.data$icd_code, "^[A-DF-UW-Z]") ~ "10",
        TRUE ~ "9")
    ) |>
    dplyr::transmute(
      diagnosisid = readr::parse_integer(.data$diagnosisid),
      patientunitstayid = readr::parse_integer(.data$patientunitstayid),
      patienthealthsystemstayid =
        readr::parse_integer(.data$patienthealthsystemstayid),
      uniquepid = .data$uniquepid,
      activeupondischarge = .data$activeupondischarge,
      diagnosisoffset = readr::parse_integer(.data$diagnosisoffset),
      diagnosispriority = .data$diagnosispriority,
      diagnosisstring = .data$diagnosisstring,
      icd_code_raw = .data$icd_code_raw,
      icd_code = .data$icd_code,
      icd_version = .data$icd_version,
      is_ambiguous = .data$is_ambiguous,
      version_source = .data$version_source,
      match_n = .data$match_n,
      long_title = .data$long_title,
      long_titles_all = .data$long_titles_all,
      regex_version_suggestion = .data$regex_version_suggestion
    )

  stage <- tempfile(
    pattern = paste0(".", basename(out_file), ".stage-"),
    tmpdir = dirname(out_file)
  )
  on.exit({
    if (file.exists(stage)) .ehr_remove_owned_path(stage)
  }, add = TRUE)
  arrow::write_parquet(out, stage)
  valid <- tryCatch({
    check <- arrow::open_dataset(stage, format = "parquet")
    identical(names(check), names(out))
  }, error = function(e) FALSE)
  if (!isTRUE(valid)) {
    stop("The staged eICU diagnosis ICD parquet failed validation.",
         call. = FALSE)
  }
  .ehr_publish_cohort_artifact(stage, out_file)
  message("diagnosis_icd_mod: ", format(nrow(out), big.mark = ","),
          " rows written to ", out_file)
  invisible(out_file)
}

.eicu_diagnosis_icd_output_file <- function(output_path) {
  if (missing(output_path) || is.null(output_path) || !nzchar(output_path))
    stop("output_path is required.", call. = FALSE)
  if (grepl("\\.parquet$", output_path, ignore.case = TRUE)) output_path
  else file.path(output_path, "diagnosis_icd_mod.parquet")
}

.eicu_require_cols <- function(x, cols, arg_name) {
  missing_cols <- setdiff(cols, names(x))
  if (length(missing_cols) > 0L) {
    stop(arg_name, " is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# Read an export dictionary (CSV or RDS; no manifest required, since callers
# pass their own file) and normalize it with the shared ICD asset normalizer:
# compact codes, validated 9/10 versions, and official dotted forms.
.eicu_read_icd_dict <- function(icd_dict_path) {
  ext <- tolower(tools::file_ext(icd_dict_path))
  dict <- if (ext == "rds") {
    readRDS(icd_dict_path)
  } else {
    readr::read_csv(
      icd_dict_path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  }
  .eicu_require_cols(dict, c("icd_code", "icd_version"), "icd_dict_path")
  if (!"long_title" %in% names(dict)) dict$long_title <- NA_character_
  dict$long_title <- as.character(dict$long_title)
  .normalize_icd_asset(as.data.frame(dict))
}

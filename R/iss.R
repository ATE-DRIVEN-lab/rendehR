#' Compute hospital-level injury severity measures
#'
#' ICD-9 diagnoses are scored with `icdpicr`; ICD-10 diagnoses are scored with
#' an isolated adapter for the `icdpicr2` 2.1 API (2.1.0 or later). Diagnoses are scored
#' once per hospital admission and then projected to optional ICU identifiers.
#' Admissions without an ICU remain present. Ambiguous eICU ICD versions are
#' excluded and represented by `iss_status` rather than inferred by regex.
#'
#' Both scorers run on every admission, and the reported `iss`/`niss`/`maxais`
#' are the element-wise maximum of the two: when an admission has both an ICD-9
#' and an ICD-10 scoring the higher value is reported, and when it has only one
#' that one is reported. `mechmaj`/`mechmin`/`intent` are categorical and cannot
#' be maximised, so they take the ICD-9 value when present and the ICD-10 value
#' otherwise. The per-version columns (`*_icd9`, `*_i10`) are always published
#' alongside, so either scoring can be recovered or compared.
#'
#' `maxais` is the highest *known* AIS (1-6) across the two scorings. AIS 9 -
#' "injury present, severity not specified" - is never treated as a severity:
#' it cannot outrank a known AIS, and an admission whose injuries all have
#' unknown severity gets `maxais`, `iss` and `niss` as NA with
#' `iss_status = "severity_unknown"`, distinct from `"no_mapped_condition"`
#' (no code maps to an injury; scores reported as 0). The per-version columns
#' keep the scorers' raw values, including 9.
#'
#' This is deliberately a maximum and not a per-admission choice of coding
#' system. It matters most for eICU, which codes the same stay in both systems
#' (93.6% of stays in the reference SCID cohort carry both, at a median 50/50
#' split), so the two scorings are two representations of the same clinical
#' content rather than two eras; taking the maximum keeps an injury that is
#' documented in only one of them.
#'
#' @param src_name Source name (for example, `"mimic_iii_demo"`).
#' @param db_name Optional Postgres database name.
#' @param base_dir Project root. Defaults to `getwd()`.
#' @param con Optional explicit DBI connection.
#' @param icd_adjudication_path Optional reviewed eICU ICD-version crosswalk.
#' @param publish Whether to transactionally publish the result.
#' @return Invisibly returns the ISS data frame.
#' @examples
#' \dontrun{
#' run_iss("mimic_iv_demo", db_name = "rendehr")
#' }
#' @export
#' @param enrichment Optional overrides from `enrichment_options()` or a named list.
run_iss <- function(src_name, db_name = NULL, base_dir = NULL, con = NULL,
                    icd_adjudication_path = NULL, publish = TRUE,
                    enrichment = NULL) {
  .ehr_apply_grouped_options(
    list(enrichment = enrichment),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(run_iss)), frame = environment())
  fam <- src_family(src_name)
  schema_names <- src_schema_names(src_name)
  base_dir <- base_dir %||% getwd()

  icds_dict <- adjudication <- NULL
  if (identical(fam, "eicu")) {
    paths <- ehr_paths(src_name = src_name, base_dir = base_dir)
    dict_path <- paths$icds_dict_path
    if (is.null(dict_path) || !file.exists(dict_path)) {
      stop("eICU ISS scoring requires the validated ICD dictionary: ",
           dict_path %||% "<unset>", call. = FALSE)
    }
    icds_dict <- .load_icds_dict(dict_path)
    adjudication <- .load_eicu_icd_adjudication(icd_adjudication_path)
  }

  .do_run <- function(connection) {
    message("Computing hospital-level ISS for ", src_name, " ...")
    result <- switch(fam,
      mimic_iv = .iss_mimic_iv(connection, schema_names$raw),
      mimic_iii = .iss_mimic_iii(connection, schema_names$raw),
      eicu = .iss_eicu(connection, schema_names$raw, icds_dict, adjudication),
      stop("Unsupported source family: ", fam, call. = FALSE)
    )
    if (isTRUE(publish)) {
      id_columns <- intersect(
        c("subject_id", "hadm_id", "stay_id", "icustay_id", "uniquepid",
          "patienthealthsystemstayid", "patientunitstayid"), names(result))
      .clinical_publish_table(
        connection, schema_names$raw, "iss", result,
        required_columns = c(id_columns, "iss_status", .iss_metric_columns()),
        nonmissing_columns = "iss_status"
      )
    }
    message("iss: ", format(nrow(result), big.mark = ","), " rows ",
            if (isTRUE(publish)) "published to " else "built for ",
            schema_names$raw, ".iss")
    invisible(result)
  }

  if (!is.null(con)) return(.do_run(con))
  paths <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name <- .ehr_resolve_db_name(db_name)
  creds <- load_credentials(paths$credentials_json)
  con2 <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  .do_run(con2)
}

.iss_metric_columns <- function() {
  c("iss_icd9", "niss_icd9", "maxais_icd9", "pmort_icd9",
    "iss_i10", "niss_i10", "maxais_i10", "pmort_tqp", "pmort_nis",
    "iciss_mult_intl", "iciss_min_intl", "iciss_mult_tqp",
    "iciss_min_tqp", "iciss_mult_nis", "iciss_min_nis", "frame_cell",
    "frame_survival", "iss", "niss", "maxais", "mechmaj", "mechmin",
    "intent")
}

.empty_iss_scores <- function(id_col, id = NULL) {
  id <- if (is.null(id)) character() else id
  out <- data.frame(setNames(list(id), id_col),
                    stringsAsFactors = FALSE, check.names = FALSE)
  character_columns <- c("frame_cell", "mechmaj", "mechmin", "intent")
  for (column in .iss_metric_columns()) {
    out[[column]] <- if (column %in% character_columns) {
      rep(NA_character_, nrow(out))
    } else rep(NA_real_, nrow(out))
  }
  out
}

.iss_num_exact <- function(df, column, label, required = TRUE) {
  if (!column %in% names(df)) {
    if (isTRUE(required)) {
      stop(label, " returned an incompatible schema; missing `", column,
           "`.", call. = FALSE)
    }
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[column]]))
}

.iss_pick_slots <- function(df, prefix) {
  columns <- grep(paste0("^", prefix, "_?[0-9]+$"), names(df), value = TRUE)
  if (!length(columns)) return(rep(NA_character_, nrow(df)))
  apply(df[columns], 1L, function(row) {
    value <- as.character(row)
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value)) value[[1L]] else NA_character_
  })
}

# Report the higher of the two version-specific scorings when an admission has
# both, and the only available one when it has just one. `na.rm = TRUE` is what
# makes the second case work; it also turns "neither scoring exists" into -Inf,
# which is mapped back to NA so an unscored admission stays missing rather than
# becoming an extreme value.
.iss_pmax <- function(a, b) {
  value <- pmax(a, b, na.rm = TRUE)
  value[is.infinite(value)] <- NA_real_
  value
}

# Combined maxais over the two version-specific scorings. AIS 9 means "injury
# present, severity not specified" - a flag, not a severity (real AIS runs
# 1-6) - so it must never win a numeric maximum against a known AIS. Returns the
# highest known AIS (1-6) from either version; otherwise 0 when a version found
# no mapped injury, and NA when there is no known severity to report (neither
# scored, or only unknown-severity injuries). .finalize_iss() labels the last
# case iss_status = "severity_unknown", distinct from "no_mapped_condition".
# The per-version maxais_icd9 / maxais_i10 keep the scorers' raw values, 9
# included.
.iss_max_known_ais <- function(a, b) {
  known <- function(x) ifelse(x %in% 1:6, x, NA_real_)
  value <- .iss_pmax(known(a), known(b))
  none_mapped <- is.na(value) & !(a %in% 9 | b %in% 9) & (a %in% 0 | b %in% 0)
  value[none_mapped] <- 0
  value
}

# TRUE where injuries were found but none has a known severity: no known AIS
# in either version, and at least one version returned AIS 9.
.iss_severity_unknown <- function(maxais, maxais_icd9, maxais_i10) {
  is.na(maxais) & (maxais_icd9 %in% 9 | maxais_i10 %in% 9)
}

.iss_pivot_wide <- function(diag_df, id_col) {
  diag_df %>%
    dplyr::filter(!is.na(.data[[id_col]]), !is.na(icd_code)) %>%
    dplyr::distinct(.data[[id_col]], icd_code) %>%
    dplyr::arrange(.data[[id_col]], icd_code) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::mutate(.dx = paste0("dx", dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(id_cols = dplyr::all_of(id_col), names_from = ".dx",
                       values_from = "icd_code") %>%
    as.data.frame()
}

.iss_score_icd9 <- function(diag_df, id_col) {
  if (!nrow(diag_df)) return(NULL)
  if (!requireNamespace("icdpicr", quietly = TRUE)) {
    stop("ICD-9 ISS scoring requires the declared `icdpicr` dependency.",
         call. = FALSE)
  }
  wide <- .iss_pivot_wide(diag_df, id_col)
  scored <- as.data.frame(icdpicr::cat_trauma(wide, "dx", icd10 = FALSE))
  if (!id_col %in% names(scored)) {
    stop("icdpicr returned an incompatible schema; missing visit ID.",
         call. = FALSE)
  }
  out <- data.frame(setNames(list(scored[[id_col]]), id_col),
                    stringsAsFactors = FALSE, check.names = FALSE)
  out$iss_icd9 <- .iss_num_exact(scored, "riss", "icdpicr::cat_trauma")
  out$niss_icd9 <- .iss_num_exact(scored, "niss", "icdpicr::cat_trauma")
  out$maxais_icd9 <- .iss_num_exact(scored, "maxais", "icdpicr::cat_trauma")
  out$pmort_icd9 <- .iss_num_exact(
    scored, "Pmort", "icdpicr::cat_trauma", required = FALSE)
  out$mechmaj9 <- .iss_pick_slots(scored, "mechmaj")
  out$mechmin9 <- .iss_pick_slots(scored, "mechmin")
  out$intent9 <- .iss_pick_slots(scored, "intent")
  out
}

.icdpicr2_adapter_environment <- function() {
  if (!requireNamespace("icdpicr2", quietly = TRUE)) {
    stop("ICD-10 ISS scoring requires the declared `icdpicr2` dependency.",
         call. = FALSE)
  }
  version <- as.character(utils::packageVersion("icdpicr2"))
  # 2.1.0 and 2.1.2 give identical scores; the schema checks below stop on an
  # incompatible later release.
  if (utils::compareVersion(version, "2.1.0") < 0) {
    stop("The ICD-10 ISS adapter needs icdpicr2 2.1.0 or later; found ",
         version, ".", call. = FALSE)
  }
  namespace <- asNamespace("icdpicr2")
  adapter <- new.env(parent = namespace)
  assets <- c("i10_map_frame", "i10_map_iciss", "i10_map_mech", "i10_map_sev")
  for (asset in assets) {
    utils::data(list = asset, package = "icdpicr2", envir = adapter)
    if (!exists(asset, envir = adapter, inherits = FALSE)) {
      stop("icdpicr2 mapping asset `", asset,
           "` could not be isolated.", call. = FALSE)
    }
  }
  for (name in c("cat_trauma2", "iciss", "framework")) {
    fn <- getExportedValue("icdpicr2", name)
    environment(fn) <- adapter
    assign(name, fn, envir = adapter)
  }
  adapter
}

.iss_score_icd10 <- function(diag_df, id_col, adapter = NULL) {
  if (!nrow(diag_df)) return(NULL)
  wide <- .iss_pivot_wide(diag_df, id_col)
  adapter <- adapter %||% .icdpicr2_adapter_environment()
  ct <- as.data.frame(adapter$cat_trauma2(wide, "dx", messages = FALSE))
  ic <- as.data.frame(adapter$iciss(wide, "dx", messages = FALSE))
  fr <- as.data.frame(adapter$framework(wide, "dx", messages = FALSE))
  align <- function(frame, label) {
    if (!id_col %in% names(frame)) {
      stop("icdpicr2 returned an incompatible visit-ID schema.",
           call. = FALSE)
    }
    index <- match(wide[[id_col]], frame[[id_col]])
    if (anyNA(index) || anyDuplicated(frame[[id_col]])) {
      stop("icdpicr2::", label,
           " did not return exactly one row per input visit.", call. = FALSE)
    }
    frame[index, , drop = FALSE]
  }
  ct <- align(ct, "cat_trauma2")
  ic <- align(ic, "iciss")
  fr <- align(fr, "framework")

  out <- data.frame(setNames(list(wide[[id_col]]), id_col),
                    stringsAsFactors = FALSE, check.names = FALSE)
  out$iss_i10 <- .iss_num_exact(ct, "riss", "icdpicr2::cat_trauma2")
  out$niss_i10 <- .iss_num_exact(ct, "niss", "icdpicr2::cat_trauma2")
  out$maxais_i10 <- .iss_num_exact(ct, "maxais", "icdpicr2::cat_trauma2")
  out$pmort_tqp <- .iss_num_exact(ct, "PmortTQP", "icdpicr2::cat_trauma2")
  out$pmort_nis <- .iss_num_exact(ct, "PmortNIS", "icdpicr2::cat_trauma2")
  out$mechmaj10 <- .iss_pick_slots(ct, "mech")
  out$mechmin10 <- rep(NA_character_, nrow(out))
  out$intent10 <- .iss_pick_slots(ct, "intent")

  out$iciss_mult_intl <- .iss_num_exact(ic, "PS_int_prod", "icdpicr2::iciss")
  out$iciss_min_intl <- .iss_num_exact(ic, "PS_int_min", "icdpicr2::iciss")
  out$iciss_mult_tqp <- .iss_num_exact(ic, "PS_TQP_prod", "icdpicr2::iciss")
  out$iciss_min_tqp <- .iss_num_exact(ic, "PS_TQP_min", "icdpicr2::iciss")
  out$iciss_mult_nis <- .iss_num_exact(ic, "PS_NIS_prod", "icdpicr2::iciss")
  out$iciss_min_nis <- .iss_num_exact(ic, "PS_NIS_min", "icdpicr2::iciss")

  cell_columns <- grep("^cell_[0-9]+$", names(fr), value = TRUE)
  survival_columns <- grep("^PsCell_[0-9]+$", names(fr), value = TRUE)
  if (!length(cell_columns) || !length(survival_columns)) {
    stop("icdpicr2::framework returned an incompatible cell schema.",
         call. = FALSE)
  }
  out$frame_cell <- apply(fr[cell_columns], 1L, .collapse_sorted_values)
  out$frame_survival <- apply(fr[survival_columns], 1L, function(row) {
    value <- suppressWarnings(as.numeric(row))
    if (all(is.na(value))) NA_real_ else min(value, na.rm = TRUE)
  })

  core <- out[c("iss_i10", "niss_i10", "maxais_i10")]
  if (!nrow(out) || all(vapply(core, function(x) all(is.na(x)), logical(1)))) {
    stop("icdpicr2 returned unusable all-missing ICD-10 severity output.",
         call. = FALSE)
  }
  out
}

.run_iss_all <- function(diag_df, id_col, icdpicr2_adapter = NULL) {
  resolved <- diag_df %>%
    dplyr::filter(icd_version %in% c("9", "10"), !is.na(icd_code)) %>%
    dplyr::distinct(.data[[id_col]], icd_code, icd_version)
  ids <- unique(resolved[[id_col]])
  if (!length(ids)) return(.empty_iss_scores(id_col, resolved[[id_col]]))
  r9 <- .iss_score_icd9(dplyr::filter(resolved, icd_version == "9"), id_col)
  r10 <- .iss_score_icd10(dplyr::filter(resolved, icd_version == "10"),
                          id_col, adapter = icdpicr2_adapter)
  out <- data.frame(setNames(list(ids), id_col), stringsAsFactors = FALSE)
  if (!is.null(r9)) out <- dplyr::left_join(out, r9, by = id_col)
  if (!is.null(r10)) out <- dplyr::left_join(out, r10, by = id_col)

  value <- function(name, character = FALSE) {
    if (name %in% names(out)) return(out[[name]])
    if (character) rep(NA_character_, nrow(out)) else rep(NA_real_, nrow(out))
  }
  out$iss <- .iss_pmax(value("iss_icd9"), value("iss_i10"))
  out$niss <- .iss_pmax(value("niss_icd9"), value("niss_i10"))
  out$maxais <- .iss_max_known_ais(value("maxais_icd9"), value("maxais_i10"))
  out$mechmaj <- dplyr::coalesce(value("mechmaj9", TRUE),
                                 value("mechmaj10", TRUE))
  out$mechmin <- dplyr::coalesce(value("mechmin9", TRUE),
                                 value("mechmin10", TRUE))
  out$intent <- dplyr::coalesce(value("intent9", TRUE), value("intent10", TRUE))
  scratch <- c("mechmaj9", "mechmin9", "intent9", "mechmaj10",
               "mechmin10", "intent10")
  out <- out[setdiff(names(out), scratch)]
  template <- .empty_iss_scores(id_col, out[[id_col]])
  for (column in setdiff(names(template), names(out))) out[[column]] <- template[[column]]
  out[c(id_col, .iss_metric_columns())]
}

.finalize_iss <- function(universe, diag, id_col, icdpicr2_adapter = NULL) {
  status <- .clinical_status_for_universe(universe, diag, id_col)
  scores <- .run_iss_all(diag, id_col, icdpicr2_adapter)
  hospital <- status %>% dplyr::left_join(scores, by = id_col)
  unresolved <- !hospital$.has_resolved
  hospital[unresolved, .iss_metric_columns()] <- NA
  # Injuries found but no severity known (AIS 9 only): the severity is unknown,
  # not zero, so iss / niss / maxais are NA and the status says so. Kept
  # separate from "no_mapped_condition", where no code maps to an injury.
  severity_unknown <- hospital$.clinical_status %in%
    c("scored", "scored_with_exclusions") &
    .iss_severity_unknown(hospital$maxais, hospital$maxais_icd9,
                          hospital$maxais_i10)
  no_map <- hospital$.clinical_status == "scored" & !severity_unknown &
    (is.na(hospital$iss) | hospital$iss == 0) &
    (is.na(hospital$niss) | hospital$niss == 0) &
    (is.na(hospital$maxais) | hospital$maxais == 0)
  primary <- c("iss", "niss", "maxais")
  for (column in primary) {
    hospital[[column]][no_map] <- 0
    hospital[[column]][severity_unknown] <- NA
  }
  hospital$iss_status <- dplyr::case_when(
    severity_unknown ~ "severity_unknown",
    no_map ~ "no_mapped_condition",
    TRUE ~ hospital$.clinical_status)
  hospital <- hospital %>% dplyr::select(-dplyr::starts_with("."))
  universe %>%
    dplyr::left_join(hospital, by = id_col) %>%
    dplyr::select(dplyr::all_of(names(universe)), iss_status,
                  dplyr::all_of(.iss_metric_columns()))
}

.iss_mimic_iv <- function(con, raw_schema) {
  diag <- get_table_from_db(con, raw_schema, "diagnoses_icd") %>%
    dplyr::transmute(subject_id, hadm_id,
                     icd_code_raw = .icd_raw_token(icd_code),
                     icd_code = .icd_compact_token(icd_code),
                     icd_version = as.character(icd_version))
  admissions <- get_table_from_db(con, raw_schema, "admissions") %>%
    dplyr::select(subject_id, hadm_id) %>% dplyr::distinct()
  icus <- get_table_from_db(con, raw_schema, "icustays") %>%
    dplyr::select(subject_id, hadm_id, stay_id) %>% dplyr::distinct()
  universe <- admissions %>% dplyr::left_join(icus, by = c("subject_id", "hadm_id"))
  .finalize_iss(universe, diag, "hadm_id")
}

.iss_mimic_iii <- function(con, raw_schema) {
  diag <- get_table_from_db(con, raw_schema, "diagnoses_icd") %>%
    dplyr::transmute(subject_id, hadm_id,
                     icd_code_raw = .icd_raw_token(icd9_code),
                     icd_code = .icd_compact_token(icd9_code),
                     icd_version = "9")
  admissions <- get_table_from_db(con, raw_schema, "admissions") %>%
    dplyr::select(subject_id, hadm_id) %>% dplyr::distinct()
  icus <- get_table_from_db(con, raw_schema, "icustays") %>%
    dplyr::select(subject_id, hadm_id, icustay_id) %>% dplyr::distinct()
  universe <- admissions %>% dplyr::left_join(icus, by = c("subject_id", "hadm_id"))
  .finalize_iss(universe, diag, "hadm_id")
}

.iss_eicu <- function(con, raw_schema, icds_dict, adjudication) {
  diag <- .eicu_diag_versioned(con, raw_schema, icds_dict, adjudication) %>%
    dplyr::distinct(patienthealthsystemstayid, patientunitstayid,
                    icd_code_raw, icd_code, icd_version,
                    version_resolution_status, .keep_all = TRUE)
  universe <- get_table_from_db(con, raw_schema, "patient") %>%
    dplyr::select(uniquepid, patienthealthsystemstayid, patientunitstayid) %>%
    dplyr::distinct()
  .finalize_iss(universe, diag, "patienthealthsystemstayid")
}

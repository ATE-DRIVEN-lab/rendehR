#' Compute hospital-level Charlson and Elixhauser comorbidities
#'
#' Diagnoses are scored once per hospital admission. ICU identifiers are an
#' optional projection: admissions without an ICU remain in the output with a
#' missing ICU identifier, while admissions with multiple ICU stays inherit the
#' same hospital-level result on each ICU row.
#'
#' Flag columns record whether any code of the category is present, with ICD-9
#' and ICD-10 evidence combined. Scores apply the standard hierarchy to those
#' combined flags (`comorbidity::score(assign0 = TRUE)`): when both the mild and
#' the severe form of a condition are present (for example diabetes with and
#' without complications, mild and severe liver disease, solid tumour and
#' metastatic cancer), only the severe form is weighted.
#'
#' eICU ICD versions are resolved from exact dictionary evidence and the
#' reviewed adjudication asset. Ambiguous or unmatched codes are excluded from
#' scoring and represented by `comorbidity_status`; regex suggestions never
#' determine a scoreable version.
#'
#' @param src_name Source name (for example, `"mimic_iii_demo"`).
#' @param db_name Optional Postgres database name.
#' @param base_dir Project root. Defaults to `getwd()`.
#' @param maps One or both of `"charlson"` and `"elixhauser"`.
#' @param con Optional explicit DBI connection.
#' @param icd_adjudication_path Optional reviewed eICU ICD-version crosswalk.
#' @param publish Whether to transactionally publish the result.
#' @return Invisibly returns the comorbidities data frame.
#' @examples
#' \dontrun{
#' run_comorbidities("mimic_iv_demo", db_name = "rendehr", maps = c("charlson", "elixhauser"))
#' }
#' @export
#' @param enrichment Optional overrides from `enrichment_options()` or a named list.
run_comorbidities <- function(src_name,
                              db_name = NULL,
                              base_dir = NULL,
                              maps = c("charlson", "elixhauser"),
                              con = NULL,
                              icd_adjudication_path = NULL,
                              publish = TRUE,
                              enrichment = NULL) {
  .ehr_apply_grouped_options(
    list(enrichment = enrichment),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(run_comorbidities)), frame = environment())
  maps <- match.arg(maps, c("charlson", "elixhauser"), several.ok = TRUE)
  fam <- src_family(src_name)
  schema_names <- src_schema_names(src_name)
  base_dir <- base_dir %||% getwd()

  icds_dict <- adjudication <- NULL
  if (identical(fam, "eicu")) {
    paths <- ehr_paths(src_name = src_name, base_dir = base_dir)
    dict_path <- paths$icds_dict_path
    if (is.null(dict_path) || !file.exists(dict_path)) {
      stop("eICU comorbidity scoring requires the validated ICD dictionary: ",
           dict_path %||% "<unset>", call. = FALSE)
    }
    icds_dict <- .load_icds_dict(dict_path)
    adjudication <- .load_eicu_icd_adjudication(icd_adjudication_path)
  }

  .do_run <- function(connection) {
    message("Computing hospital-level comorbidities for ", src_name, " ...")
    result <- switch(fam,
      mimic_iv = .comorbidities_mimic_iv(connection, schema_names$raw, maps),
      mimic_iii = .comorbidities_mimic_iii(connection, schema_names$raw, maps),
      eicu = .comorbidities_eicu(connection, schema_names$raw, maps,
                                 icds_dict, adjudication),
      stop("Unsupported source family: ", fam, call. = FALSE)
    )
    if (isTRUE(publish)) {
      id_columns <- intersect(
        c("subject_id", "hadm_id", "stay_id", "icustay_id", "uniquepid",
          "patienthealthsystemstayid", "patientunitstayid"), names(result))
      .clinical_publish_table(
        connection, schema_names$raw, "comorbidities", result,
        required_columns = c(id_columns, "comorbidity_status"),
        nonmissing_columns = "comorbidity_status"
      )
    }
    message("comorbidities: ", format(nrow(result), big.mark = ","), " rows ",
            if (isTRUE(publish)) "published to " else "built for ",
            schema_names$raw, ".comorbidities")
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

.comorbidity_flag_columns <- function(map_prefix) {
  switch(map_prefix,
    charlson = c("mi", "chf", "pvd", "cevd", "dementia", "cpd",
                 "rheumd", "pud", "mld", "diab", "diabwc", "hp", "rend",
                 "canc", "msld", "metacanc", "aids"),
    elixhauser = c("chf", "carit", "valv", "pcd", "pvd", "hypunc",
                   "hypc", "para", "ond", "cpd", "diabunc", "diabc",
                   "hypothy", "rf", "ld", "pud", "aids", "lymph",
                   "metacanc", "solidtum", "rheumd", "coag", "obes",
                   "wloss", "fed", "blane", "dane", "alcohol", "drug",
                   "psycho", "depre"),
    stop("Unsupported comorbidity map: ", map_prefix, call. = FALSE)
  )
}

.empty_comorbidity_map <- function(id_col, map_prefix, id = character()) {
  flags <- .comorbidity_flag_columns(map_prefix)
  out <- data.frame(stringsAsFactors = FALSE)
  out[[id_col]] <- id
  for (column in paste0(map_prefix, "_", flags)) out[[column]] <- integer()
  out[[paste0(map_prefix, "_score")]] <- integer()
  out
}

# Run one map over already resolved diagnosis rows. Scorer failures are fatal:
# they cannot be converted into plausible-looking missing values.
.comorbidity_one_map <- function(diag_df, id_col, map_prefix) {
  flags <- .comorbidity_flag_columns(map_prefix)
  if (!nrow(diag_df)) {
    return(.empty_comorbidity_map(id_col, map_prefix, diag_df[[id_col]]))
  }

  run_cm <- function(df, version) {
    if (!nrow(df)) return(NULL)
    out <- comorbidity::comorbidity(
      x = as.data.frame(df), id = id_col, code = "icd_code",
      map = paste0(map_prefix, "_icd", version, "_quan"), assign0 = FALSE)
    missing <- setdiff(c(id_col, flags), names(out))
    if (length(missing)) {
      stop("The comorbidity package returned an unexpected ", map_prefix,
           " schema; missing: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    as.data.frame(out[c(id_col, flags)])
  }

  cm9 <- run_cm(dplyr::filter(diag_df, icd_version == "9"), "9")
  cm10 <- run_cm(dplyr::filter(diag_df, icd_version == "10"), "10")
  ids <- unique(diag_df[[id_col]])
  base <- data.frame(setNames(list(ids), id_col), stringsAsFactors = FALSE)

  for (column in flags) {
    values <- rep.int(0L, length(ids))
    for (frame in Filter(Negate(is.null), list(cm9, cm10))) {
      match_index <- match(ids, frame[[id_col]])
      observed <- frame[[column]][match_index]
      values <- pmax(values, tidyr::replace_na(as.integer(observed), 0L))
    }
    base[[column]] <- values
  }

  # comorbidity::score(assign0 = TRUE) edits its input by reference (data.table).
  score_obj <- as.data.frame(lapply(base, function(v) v[seq_along(v)]),
                             stringsAsFactors = FALSE)
  class(score_obj) <- c("comorbidity", "data.frame")
  attr(score_obj, "id") <- id_col
  attr(score_obj, "map") <- paste0(map_prefix, "_icd10_quan")
  attr(score_obj, "assign0") <- FALSE
  weights <- if (identical(map_prefix, "charlson")) "quan" else "vw"
  score <- comorbidity::score(score_obj, weights = weights, assign0 = TRUE)
  if (length(score) != nrow(base)) {
    stop("The comorbidity package returned an invalid ", map_prefix,
         " score length.", call. = FALSE)
  }
  base[[paste0(map_prefix, "_score")]] <- as.integer(score)
  names(base)[names(base) %in% flags] <- paste0(map_prefix, "_", flags)
  base
}

.run_all_maps <- function(diag_df, id_col, maps) {
  resolved <- diag_df %>%
    dplyr::filter(icd_version %in% c("9", "10"), !is.na(icd_code)) %>%
    dplyr::distinct(.data[[id_col]], icd_code, icd_version)
  results <- lapply(maps, function(map) {
    .comorbidity_one_map(resolved, id_col, map)
  })
  if (length(results) == 1L) results[[1L]] else
    Reduce(function(a, b) dplyr::full_join(a, b, by = id_col), results)
}

.finalize_comorbidities <- function(universe, diag, id_col, maps) {
  status <- .clinical_status_for_universe(universe, diag, id_col)
  scores <- .run_all_maps(diag, id_col, maps)
  metric_columns <- setdiff(names(scores), id_col)
  flag_columns <- unlist(lapply(maps, function(map) {
    paste0(map, "_", .comorbidity_flag_columns(map))
  }), use.names = FALSE)

  hospital <- status %>% dplyr::left_join(scores, by = id_col)
  for (column in intersect(metric_columns, names(hospital))) {
    hospital[[column]] <- ifelse(
      hospital$.has_resolved,
      tidyr::replace_na(hospital[[column]], 0L), NA_integer_)
  }
  any_mapped <- if (length(flag_columns)) {
    rowSums(as.data.frame(hospital[intersect(flag_columns, names(hospital))]),
            na.rm = TRUE) > 0L
  } else rep(FALSE, nrow(hospital))
  hospital <- hospital %>%
    dplyr::mutate(comorbidity_status = dplyr::case_when(
      .clinical_status == "scored" & !any_mapped ~ "no_mapped_condition",
      TRUE ~ .clinical_status
    )) %>%
    dplyr::select(-dplyr::starts_with("."))

  universe %>%
    dplyr::left_join(hospital, by = id_col) %>%
    dplyr::select(dplyr::all_of(names(universe)), comorbidity_status,
                  dplyr::all_of(metric_columns))
}

.comorbidities_mimic_iv <- function(con, raw_schema, maps) {
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
  .finalize_comorbidities(universe, diag, "hadm_id", maps)
}

.comorbidities_mimic_iii <- function(con, raw_schema, maps) {
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
  .finalize_comorbidities(universe, diag, "hadm_id", maps)
}

.comorbidities_eicu <- function(con, raw_schema, maps, icds_dict,
                                adjudication) {
  diag <- .eicu_diag_versioned(con, raw_schema, icds_dict, adjudication) %>%
    dplyr::distinct(patienthealthsystemstayid, patientunitstayid,
                    icd_code_raw, icd_code, icd_version,
                    version_resolution_status, .keep_all = TRUE)
  universe <- get_table_from_db(con, raw_schema, "patient") %>%
    dplyr::select(uniquepid, patienthealthsystemstayid, patientunitstayid) %>%
    dplyr::distinct()
  .finalize_comorbidities(
    universe, diag, "patienthealthsystemstayid", maps)
}

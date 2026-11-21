# Data layer of the cohort explorer (run_explorer()).
#
# explorer_data() reads one admission-level merged schema (built by
# merge_sources()) plus the per-source visit windows into a "bundle": a list of
# plain data frames that the Shiny app works on. Patient time series are not
# copied; the Subject Explorer reads them per patient with the explorer_patient_*
# helpers below.

utils::globalVariables(c(
  "adm_id", "any_icu", "bin", "completeness", "concept", "concept_raw", "diag_icd_codes", "drug", "eicu_labname",
  "flagged", "group", "hosp_los_min", "icu_id", "icu_in_min", "icu_los_total", "icu_out_min", "icu_unit", "in_cohort",
  "is_index", "level_group", "mean_harmonized", "mean_raw", "n_cohort", "n_harmonized", "n_icu", "n_raw",
  "n_ratio", "num_diag_icd_codes", "patient_id", "rel_diff", "scid_icd_codes", "second_level", "t_hadm", "value"))

.xp_family_labels <- c(mimic_iii = "MIMIC-III", mimic_iv = "MIMIC-IV", eicu = "eICU-CRD")
.xp_palette <- c("#37474f", "#78909c", "#b0a58c", "#7f9a8f", "#9b8aa6", "#b08968",
                 "#6f6f6f", "#c3c9cf")
.xp_age_levels <- c("<18", "18-44", "45-64", "65-79", ">=80")

# Display label per source name: the family label, with the remaining part of
# the name added when it is not the family itself (e.g. "MIMIC-IV demo").
.xp_source_labels <- function(sources) {
  lab <- vapply(sources, function(s) {
    fam <- tryCatch(src_family(s), error = function(e) s)
    base <- unname(.xp_family_labels[fam])
    if (is.na(base)) return(s)
    rest <- sub(paste0("^", fam, "_?"), "", s)
    if (nzchar(rest) && rest != s) paste(base, gsub("_", " ", rest)) else base
  }, character(1))
  stats::setNames(make.unique(unname(lab), sep = " "), sources)
}

.xp_age_group <- function(age) {
  factor(as.character(cut(age, c(-Inf, 18, 45, 65, 80, Inf), right = FALSE,
                          labels = .xp_age_levels)), .xp_age_levels)
}

.xp_as_flag <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(x))
  out[x %in% c("1", "true", "t", "yes", "y")] <- 1L
  out[x %in% c("0", "false", "f", "no", "n")] <- 0L
  out
}

.xp_query <- function(con, sql, ...) tibble::as_tibble(DBI::dbGetQuery(con, sql, ...))

.xp_quote_table <- function(con, schema, table) {
  DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table))
}

.xp_table_exists <- function(con, schema, table) {
  nrow(DBI::dbGetQuery(con,
    "SELECT 1 FROM information_schema.tables WHERE table_schema = $1 AND table_name = $2",
    params = list(schema, table))) > 0
}

# Minutes between two time columns that are either timestamps or numeric
# minute offsets (eICU windows are already minutes since hospital admission).
.xp_minutes <- function(t, origin) {
  tm <- function(x) if (inherits(x, "Date")) as.POSIXct(x) else x   # a Date difference would be in days
  if (inherits(t, c("POSIXt", "Date")) || inherits(origin, c("POSIXt", "Date")))
    return(as.numeric(difftime(tm(t), tm(origin), units = "mins")))
  as.numeric(t) - as.numeric(origin)
}

# Visit windows of one source on one clock (minutes since hospital admission).
.xp_windows <- function(con, src) {
  sn <- src_schema_names(src)
  if (!.xp_table_exists(con, sn$raw, "visit_windows")) return(NULL)
  ids <- tryCatch(.source_entry(src)$select_id_cols, error = function(e) NULL)
  w <- .xp_query(con, sprintf("SELECT * FROM %s", .xp_quote_table(con, sn$raw, "visit_windows")))
  if (!nrow(w)) return(NULL)
  ids <- ids %||% names(w)[1:3]
  if (!all(ids %in% names(w)) || !all(c("admittime", "dischtime") %in% names(w))) return(NULL)
  admit <- w$admittime
  tibble::tibble(
    source = src,
    patient_id = as.character(w[[ids[1]]]),
    adm_id = as.character(w[[ids[2]]]),
    icu_id = as.character(w[[ids[3]]]),
    admittime = if (inherits(admit, "POSIXt")) admit else as.POSIXct(NA),
    hosp_los_min = .xp_minutes(w$dischtime, admit),
    icu_in_min = if ("icu_intime" %in% names(w)) .xp_minutes(w$icu_intime, admit) else NA_real_,
    icu_out_min = if ("icu_outtime" %in% names(w)) .xp_minutes(w$icu_outtime, admit) else NA_real_
  )
}

# Labels for the variables a merged flat table usually carries. Columns not
# listed here are still offered, labelled by their name.
.xp_known_vars <- tibble::tribble(
  ~var, ~label, ~type, ~group,
  "age", "Age, years", "cont", "Demographics",
  "age_group", "Age group", "cat", "Demographics",
  "sex", "Sex", "cat", "Demographics",
  "race", "Race/ethnicity", "cat", "Demographics",
  "insurance", "Insurance", "cat", "Demographics",
  "language", "Language", "cat", "Demographics",
  "marital_status", "Marital status", "cat", "Demographics",
  "height", "Height", "cont", "Demographics",
  "weight_admit", "Weight at admission", "cont", "Demographics",
  "source_label", "Source", "cat", "Admission",
  "admission_type", "Admission type", "cat", "Admission",
  "admission_service", "Admitting service", "cat", "Admission",
  "hospital_admission_location", "Admission origin", "cat", "Admission",
  "care_site", "Care site", "cat", "Admission",
  "num_diag_icd_codes", "Diagnosis codes per admission", "cont", "Admission",
  "hospital_discharge_location", "Discharge destination", "cat", "Outcomes",
  "hospital_los", "Hospital length of stay, days", "cont", "Outcomes",
  "hospital_mortality", "In-hospital mortality", "bin", "Outcomes",
  "readmission_30d", "30-day readmission", "bin", "Outcomes",
  "any_icu", "ICU admission", "bin", "ICU",
  "n_icu", "ICU stays per admission", "cont", "ICU",
  "unit_first", "First ICU unit (if admitted)", "cat", "ICU",
  "icu_last_careunit", "Last ICU unit (if admitted)", "cat", "ICU",
  "icu_los_total", "ICU length of stay, days (if admitted)", "cont", "ICU",
  "icu_death", "ICU mortality (if admitted)", "bin", "ICU",
  "iss", "Injury Severity Score", "cont", "Injury",
  "niss", "New Injury Severity Score", "cont", "Injury",
  "maxais", "Maximum AIS", "cont", "Injury",
  "iss_status", "ISS status", "cat", "Injury",
  "injury_mechanism", "Injury mechanism", "cat", "Injury",
  "injury_intent", "Injury intent", "cat", "Injury",
  "injury_cause_status", "Injury cause status", "cat", "Injury"
)

# Columns that are identifiers, times or free text, never offered as variables.
.xp_hidden_cols <- c("ehr_id", "source", "source_data", "patient_id", "adm_id", "icu_id",
                     "src_patient_id", "src_admission_id", "src_icu_id", "admittime",
                     "hosp_los_min", "diag_icd_codes", "diag_icd_codes_long_title",
                     "in_cohort", "is_index", "year", "time_to_death_from_admission",
                     "time_to_death_from_discharge", "icu_first_careunit", "icu_los",
                     "icu_mortality", "seq_hos", "num_hos", "is_only_hos", "cohort_index",
                     ".all")

# Variable dictionary for the explorers: known labels first, then every other
# usable column, typed from its values.
.xp_vars <- function(adm) {
  known <- .xp_known_vars[.xp_known_vars$var %in% names(adm), ]
  other <- setdiff(names(adm), c(known$var, .xp_hidden_cols))
  typ <- vapply(other, function(v) {
    x <- adm[[v]]
    u <- unique(stats::na.omit(x))
    if (!length(u)) return(NA_character_)
    if (is.numeric(x) && all(u %in% c(0, 1))) return("bin")
    if (is.numeric(x)) return("cont")
    if (length(u) <= 40) return("cat")
    NA_character_
  }, character(1))
  extra <- tibble::tibble(var = other, label = other, type = unname(typ), group = "Other")
  extra <- extra[!is.na(extra$type), ]
  out <- dplyr::bind_rows(known, extra)
  usable <- vapply(out$var, function(v) any(!is.na(adm[[v]])), logical(1))
  out[usable, ]
}

# Group-by choices: every categorical or binary variable with 2-25 levels.
.xp_groups <- function(adm, vars) {
  cand <- vars[vars$type %in% c("cat", "bin"), ]
  keep <- vapply(cand$var, function(v) {
    k <- length(unique(stats::na.omit(adm[[v]])))
    k >= 2 && k <= 25
  }, logical(1))
  cand <- cand[keep, ]
  c(stats::setNames(cand$var, cand$label), "None (overall)" = ".all")
}

#' Build the data bundle for the cohort explorer
#'
#' Reads an admission-level merged schema (built by [merge_sources()]) and the
#' per-source visit windows into plain data frames for [run_explorer()]. Patient
#' time series are not loaded here: the explorer reads them per patient.
#'
#' @param schema Name of the merged schema (the `target_schema` given to
#'   [merge_sources()]).
#' @param con Optional open DBI connection. If NULL, the credentials stored by
#'   [ehr_connect()] are used.
#' @param sources Optional character vector of source names to keep.
#' @param raw_dirs Optional named character vector of raw data directories per
#'   source, used by the raw-versus-harmonized laboratory check. When NULL the
#'   directories configured with [ehr_paths()] are tried.
#' @param cache_dir Optional directory for a cached copy of the bundle
#'   (PostgreSQL only). The cache is reused while no table in the database has
#'   been created, dropped or re-analysed since, and the package build is the
#'   same; otherwise the bundle is rebuilt and the cache replaced. The file
#'   holds individual-level data: keep the directory private (it is written with
#'   owner-only permissions).
#' @return A list of class `rendehR_explorer_data` with elements `meta`, `adm`
#'   (cohort admissions), `ctx_adm` (every admission in the schema), `icu`,
#'   `patients`, `vars`, `groups`, `filters`, `concepts`, `med_prev` and
#'   `checks`.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", config_path = "config/config_pass.json")
#' d <- explorer_data("rendehr_admission")
#' nrow(d$adm)
#' }
explorer_data <- function(schema, con = NULL, sources = NULL, raw_dirs = NULL, cache_dir = NULL) {
  schema <- .ehr_validate_db_identifier(schema, "schema name")
  .with_connection(con, function(con) {
    cache <- if (!is.null(cache_dir)) .xp_cache_file(con, cache_dir, schema, sources, raw_dirs)
    if (!is.null(cache) && file.exists(cache)) {
      b <- tryCatch(readRDS(cache), error = function(e) NULL)
      if (inherits(b, "rendehR_explorer_data")) return(b)
    }
    for (t in c("cohort_info", "flat"))
      if (!.xp_table_exists(con, schema, t))
        stop("Table '", schema, ".", t, "' not found. Build the merged schema with ",
             "merge_sources() first.", call. = FALSE)
    level <- if (.xp_table_exists(con, schema, "cohort_info_meta")) {
      m <- .xp_query(con, sprintf("SELECT * FROM %s", .xp_quote_table(con, schema, "cohort_info_meta")))
      lv <- intersect(c("level", "merge_level", "analysis_level"), names(m))
      if (length(lv) && nrow(m)) unique(as.character(m[[lv[1]]])) else NA_character_
    } else NA_character_

    ci <- .xp_query(con, sprintf("SELECT * FROM %s", .xp_quote_table(con, schema, "cohort_info")))
    fl <- .xp_query(con, sprintf("SELECT * FROM %s", .xp_quote_table(con, schema, "flat")))
    if (!is.null(sources)) {
      ci <- ci[ci$source_data %in% sources, ]
      fl <- fl[fl$source_data %in% sources, ]
    }
    if (!nrow(ci)) stop("No rows in '", schema, ".cohort_info' for the selected sources.", call. = FALSE)
    ids <- intersect(c("ehr_id", "src_patient_id", "src_admission_id", "src_icu_id"), names(fl))
    fl[ids] <- lapply(fl[ids], .xp_id)
    fl <- suppressMessages(readr::type_convert(fl, guess_integer = FALSE,
                                               col_types = do.call(readr::cols, c(stats::setNames(rep(list(readr::col_character()), length(ids)), ids),
                                                                                  list(.default = readr::col_guess())))))
    srcs <- sort(unique(ci$source_data))
    win <- dplyr::bind_rows(lapply(srcs, function(s) tryCatch(.xp_windows(con, s), error = function(e) NULL)))

    b <- .xp_build(ci, fl, win, schema = schema, level = level, con = con, raw_dirs = raw_dirs)
    if (!is.null(cache)) {
      unlink(Sys.glob(sub("[0-9a-f]{16}\\.rds$", paste0(strrep("?", 16), ".rds"), cache)))   # older copies, same db + schema
      saveRDS(b, cache)
      Sys.chmod(cache, "0600")
    }
    b
  })
}

# Cache file for explorer_data(): its name changes whenever a table in the
# database is created, dropped or re-analysed (a pipeline rebuild recreates its
# tables, so their OIDs change), or the package is reinstalled. NULL (no cache)
# off PostgreSQL.
.xp_cache_file <- function(con, cache_dir, schema, sources, raw_dirs) {
  if (!inherits(con, "PqConnection")) return(NULL)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  fp <- DBI::dbGetQuery(con, paste(
    "SELECT current_database() AS db, COUNT(*) AS n, MAX(c.oid::bigint) AS max_oid, SUM(c.reltuples) AS tuples",
    "FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace",
    "WHERE c.relkind IN ('r', 'm', 'p') AND n.nspname NOT IN ('pg_catalog', 'information_schema')",
    "AND n.nspname NOT LIKE 'pg_toast%' AND n.nspname NOT LIKE 'pg_temp%'"))
  key <- paste(c(unlist(lapply(fp, as.character)), schema, sort(sources), sort(unname(raw_dirs)), names(raw_dirs),
                 utils::packageDescription("rendehR")[["Built"]]), collapse = "|")
  file.path(cache_dir, sprintf("explorer_%s_%s_%s.rds", fp$db, schema, substr(as.character(openssl::md5(key)), 1, 16)))
}

# Identifiers as text without scientific notation (a double 200000 would
# otherwise become "2e+05" and miss its join).
.xp_id <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  out <- format(x, scientific = FALSE, trim = TRUE, digits = 15)
  out[is.na(x)] <- NA_character_
  out
}

# Assemble the bundle from the three source tables. Split from explorer_data()
# so the tests can build a bundle from in-memory tables.
.xp_build <- function(ci, fl, win, schema = NA_character_, level = NA_character_,
                      con = NULL, raw_dirs = NULL) {
  `%>%` <- magrittr::`%>%`
  srcs <- sort(unique(ci$source_data))
  labels <- .xp_source_labels(srcs)

  col <- function(n) if (n %in% names(ci)) ci[[n]] else rep(NA, nrow(ci))
  # Cohort membership: cohort_index; schemas merged by older versions carry
  # hos_cohort_flg instead.
  flag <- if ("cohort_index" %in% names(ci)) ci$cohort_index else col("hos_cohort_flg")
  ci2 <- tibble::tibble(source = ci$source_data, patient_id = .xp_id(ci$src_patient_id),
                        adm_id = .xp_id(ci$src_admission_id),
                        in_cohort = .xp_as_flag(flag) %in% 1L,
                        seq_hos = suppressWarnings(as.numeric(col("seq_hos"))),
                        num_diag_icd_codes = suppressWarnings(as.numeric(col("num_diag_icd_codes"))),
                        diag_icd_codes = as.character(col("diag_icd_codes"))) %>%
    dplyr::group_by(source, adm_id) %>%
    dplyr::summarise(patient_id = dplyr::first(patient_id), in_cohort = any(in_cohort),
                     seq_hos = min(seq_hos), num_diag_icd_codes = dplyr::first(num_diag_icd_codes),
                     diag_icd_codes = dplyr::first(diag_icd_codes), .groups = "drop")
  if (!any(ci2$in_cohort)) ci2$in_cohort <- TRUE      # no cohort flag: everyone is in

  fl2 <- fl
  fl2$source <- fl2$source_data
  fl2$adm_id <- .xp_id(fl2$src_admission_id)
  fl2$icu_id_flat <- .xp_id(fl2$src_icu_id)
  fl2 <- fl2[!duplicated(fl2[, c("source", "adm_id")]), ]
  drop <- c("ehr_id", "source_data", "src_patient_id", "src_admission_id", "src_icu_id")
  fl2 <- fl2[, setdiff(names(fl2), drop)]

  w <- if (is.null(win) || !nrow(win)) NULL else win
  wa <- if (is.null(w)) NULL else w %>%
    dplyr::group_by(source, adm_id) %>%
    dplyr::summarise(admittime = dplyr::first(admittime), hosp_los_min = dplyr::first(hosp_los_min),
                     n_icu = dplyr::n_distinct(icu_id[!is.na(icu_id)]),
                     icu_los_total = sum((icu_out_min - icu_in_min)[!duplicated(icu_id) & !is.na(icu_id)],
                                         na.rm = TRUE) / 1440,
                     .groups = "drop")

  ctx <- ci2 %>% dplyr::left_join(fl2, by = c("source", "adm_id"))
  if (!is.null(wa)) ctx <- ctx %>% dplyr::left_join(wa, by = c("source", "adm_id"))
  if (!"admittime" %in% names(ctx)) ctx$admittime <- as.POSIXct(NA)
  if (!"hosp_los_min" %in% names(ctx)) ctx$hosp_los_min <- NA_real_
  if (!"n_icu" %in% names(ctx)) ctx$n_icu <- NA_integer_
  if (!"icu_los_total" %in% names(ctx)) ctx$icu_los_total <- NA_real_
  # Whether the length of stay comes from a visit window; the structural checks
  # must not count the hospital_los fallback below as a window.
  ctx$has_window <- !is.na(ctx$hosp_los_min)
  if (all(is.na(ctx$hosp_los_min)) && "hospital_los" %in% names(ctx))
    ctx$hosp_los_min <- ctx$hospital_los * 1440

  ctx <- ctx %>% dplyr::mutate(
    source_label = unname(labels[source]),
    n_icu = dplyr::coalesce(as.integer(n_icu), as.integer(!is.na(.data$icu_id_flat))),
    any_icu = as.integer(n_icu > 0),
    icu_los_total = dplyr::if_else(any_icu == 1L,
                                   dplyr::coalesce(dplyr::na_if(icu_los_total, 0),
                                                   suppressWarnings(as.numeric(.data$icu_los %||% NA))),
                                   NA_real_),
    year = as.integer(format(admittime, "%Y")))
  if ("age" %in% names(ctx)) ctx$age_group <- .xp_age_group(ctx$age)
  if ("icu_first_careunit" %in% names(ctx))
    ctx$unit_first <- dplyr::if_else(ctx$any_icu == 1L, as.character(ctx$icu_first_careunit), NA_character_)
  if ("icu_mortality" %in% names(ctx))
    ctx$icu_death <- dplyr::if_else(ctx$any_icu == 1L, .xp_as_flag(ctx$icu_mortality), NA_integer_)
  for (b in intersect(c("hospital_mortality", "readmission_30d"), names(ctx)))
    ctx[[b]] <- .xp_as_flag(ctx[[b]])
  if ("sex" %in% names(ctx)) ctx$sex <- tolower(as.character(ctx$sex))
  ctx$icu_id_flat <- NULL

  # Index admission: the first cohort admission of each patient (in time when
  # times are known, then by the hospitalization sequence, then by id).
  ord <- order(ctx$source, ctx$patient_id, !ctx$in_cohort, as.numeric(ctx$admittime),
               ctx$seq_hos, ctx$adm_id, na.last = TRUE)
  ctx <- ctx[ord, ]
  first_cohort <- ctx$in_cohort & !duplicated(paste(ctx$source, ctx$patient_id, ctx$in_cohort))
  ctx$is_index <- first_cohort

  adm <- ctx[ctx$in_cohort, ]

  icu <- if (is.null(w)) {
    tibble::tibble(source = character(), patient_id = character(), adm_id = character(),
                   icu_id = character(), icu_in_min = numeric(), icu_out_min = numeric(),
                   icu_unit = character())
  } else {
    w %>% dplyr::filter(!is.na(icu_id)) %>% dplyr::distinct(source, icu_id, .keep_all = TRUE) %>%
      dplyr::semi_join(ctx, by = c("source", "adm_id")) %>%
      dplyr::select(source, patient_id, adm_id, icu_id, icu_in_min, icu_out_min) %>%
      dplyr::left_join(ctx %>% dplyr::select(source, adm_id, icu_unit = dplyr::any_of("unit_first")),
                       by = c("source", "adm_id"))
  }
  if (!"icu_unit" %in% names(icu)) icu$icu_unit <- NA_character_

  patients <- ctx %>% dplyr::group_by(source, patient_id) %>%
    dplyr::summarise(n_adm = dplyr::n(), n_cohort = sum(in_cohort),
                     any_death = any(.data$hospital_mortality %in% 1L),
                     n_icu = sum(dplyr::coalesce(n_icu, 0L)), .groups = "drop") %>%
    dplyr::filter(n_cohort > 0) %>%
    dplyr::left_join(adm %>% dplyr::filter(is_index) %>%
                       dplyr::select(source, patient_id, index_adm = adm_id,
                                     dplyr::any_of(c("age", "sex"))),
                     by = c("source", "patient_id"))
  if (!"age" %in% names(patients)) patients$age <- NA_real_
  if (!"sex" %in% names(patients)) patients$sex <- NA_character_
  patients$source_label <- unname(labels[patients$source])
  patients$label <- sprintf("%s | %s | %s, %s y | %d adm (%d in cohort)", patients$source_label,
                            patients$patient_id, dplyr::coalesce(patients$sex, "?"),
                            ifelse(is.na(patients$age), "?", round(patients$age)),
                            patients$n_adm, patients$n_cohort)

  vars <- .xp_vars(adm)
  bundle <- list(
    meta = list(schema = schema, level = level, sources = srcs, source_labels = labels,
                source_colors = stats::setNames(rep_len(.xp_palette, length(labels)), unname(labels)),
                built = format(Sys.time(), "%Y-%m-%d %H:%M"),
                rendehR_version = as.character(utils::packageVersion("rendehR")),
                raw_dirs = raw_dirs, mode = "generic"),
    adm = adm, ctx_adm = ctx, icu = icu, patients = patients, vars = vars,
    groups = .xp_groups(adm, vars),
    filters = .xp_default_filters(adm),
    concepts = .xp_concepts(con, srcs),
    med_prev = .xp_med_prev(con, schema, adm),
    sci = NULL
  )
  bundle$checks <- .xp_checks(bundle, ci, fl)
  class(bundle) <- c("rendehR_explorer_data", "list")
  bundle
}

# Default Cohort Explorer filters: source, sex, age, admission type, ICU, and
# every other low-cardinality categorical the flat table carries (kept to <= 25
# distinct values so the pickers stay usable).
.xp_default_filters <- function(adm) {
  f <- list(list(var = "source", label = "Source", type = "checkbox"))
  add_picker <- function(var, label) {
    if (!var %in% names(adm)) return(invisible(NULL))
    v <- adm[[var]]; v <- v[!is.na(v)]
    if (!length(v) || length(unique(v)) > 25) return(invisible(NULL))
    f[[length(f) + 1]] <<- list(var = var, label = label, type = "picker")
    invisible(NULL)
  }
  if ("sex" %in% names(adm)) f[[length(f) + 1]] <- list(var = "sex", label = "Sex", type = "checkbox")
  if ("age" %in% names(adm) && any(!is.na(adm$age)))
    f[[length(f) + 1]] <- list(var = "age", label = "Age, years", type = "slider")
  add_picker("admission_type", "Admission type")
  add_picker("race", "Race")
  add_picker("hospital_admission_location", "Admission location")
  add_picker("hospital_discharge_location", "Discharge location")
  add_picker("insurance", "Insurance")
  add_picker("language", "Language")
  add_picker("marital_status", "Marital status")
  if ("year" %in% names(adm) && length(unique(stats::na.omit(adm$year))) > 1)
    f[[length(f) + 1]] <- list(var = "year", label = "Admission year", type = "slider")
  f[[length(f) + 1]] <- list(var = "any_icu", label = "ICU admission", type = "icu")
  f
}

.xp_concepts <- function(con, srcs) {
  if (is.null(con)) return(tibble::tibble(source = character(), concept_name = character(), schema = character()))
  dplyr::bind_rows(lapply(srcs, function(s) {
    x <- tryCatch(suppressMessages(list_concepts(s, con = con)), error = function(e) NULL)
    if (is.null(x) || !nrow(x)) return(NULL)
    x <- tibble::as_tibble(x) %>% dplyr::mutate(source = s)
    # known origins link to the package's current address, not the one stored when the registry was written
    if (all(c("origin_name", "origin_url") %in% names(x))) {
      lab <- .origin_labels
      url <- stats::setNames(vapply(lab, `[[`, "", "url"), vapply(lab, `[[`, "", "name"))
      known <- x$origin_name %in% names(url)
      x$origin_url[known] <- unname(url[x$origin_name[known]])
    }
    x
  }))
}

# Share of cohort admissions with each medication exposure, per source.
.xp_med_prev <- function(con, schema, adm) {
  empty <- tibble::tibble(source = character(), drug = character(), n = integer(), den = integer(), p = numeric())
  if (is.null(con) || !.xp_table_exists(con, schema, "dur")) return(empty)
  x <- .xp_query(con, sprintf(
    "SELECT source_data AS source, concept_name, src_admission_id AS adm_id FROM %s GROUP BY 1, 2, 3",
    .xp_quote_table(con, schema, "dur")))
  if (!nrow(x)) return(empty)
  x$drug <- sub("_exp$", "", x$concept_name)
  x <- dplyr::semi_join(x, adm, by = c("source", "adm_id"))
  den <- dplyr::count(adm, source, name = "den")
  dplyr::distinct(x, source, adm_id, drug) %>% dplyr::count(source, drug) %>%
    dplyr::left_join(den, by = "source") %>% dplyr::mutate(p = n / den)
}

# Structural checks on the bundle, shown on the Data quality page.
.xp_checks <- function(b, ci, fl) {
  adm <- b$adm; ctx <- b$ctx_adm
  # ok = NA means the check cannot be made with these data (status "N/A").
  ck <- function(check, ok, detail = "") tibble::tibble(check = check,
                                                        status = if (is.na(ok)) "N/A" else if (isTRUE(ok)) "PASS" else "FAIL",
                                                        detail = as.character(detail))
  flat_keys <- paste(fl$source_data, .xp_id(fl$src_admission_id))
  ci_keys <- paste(ci$source_data, .xp_id(ci$src_admission_id))
  win <- if ("has_window" %in% names(ctx)) ctx$has_window else !is.na(ctx$hosp_los_min)
  los <- if (all(c("hospital_los", "hosp_los_min") %in% names(ctx))) {
    d <- abs(ctx$hospital_los - ctx$hosp_los_min / 1440)[win]
    d[!is.na(d)]
  } else numeric(0)
  # A stay misattached to an admission starts outside it. Sources do record ICU discharges after
  # hospital discharge (MIMIC-IV: patients who died), so late ends are reported, not failed.
  icu_ok <- NA; icu_detail <- "no ICU stays with times"
  if (nrow(b$icu)) {
    j <- dplyr::left_join(b$icu, dplyr::select(ctx, source, adm_id, hosp_los_min), by = c("source", "adm_id"))
    starts <- with(j, icu_in_min >= -1440 & icu_in_min <= hosp_los_min + 1440)
    late <- with(j, icu_out_min > hosp_los_min + 1440)
    icu_ok <- mean(starts, na.rm = TRUE) >= 0.99
    icu_detail <- sprintf("%s ICU stays, %s start outside; %s end more than a day after discharge, as recorded",
                          fmt_n(nrow(j)), fmt_n(sum(!starts, na.rm = TRUE)), fmt_n(sum(late, na.rm = TRUE)))
  }
  pid <- .xp_id(ci$src_patient_id)
  dplyr::bind_rows(
    ck("cohort_info has one row per admission (admission-level schema)",
       !anyDuplicated(ci_keys), sprintf("%s rows, %s admissions", fmt_n(length(ci_keys)), fmt_n(length(unique(ci_keys))))),
    ck("flat has one row per admission", !anyDuplicated(flat_keys),
       sprintf("%s rows, %s admissions", fmt_n(length(flat_keys)), fmt_n(length(unique(flat_keys))))),
    ck("every cohort_info admission has a flat row", all(ci_keys %in% flat_keys),
       paste(sum(!ci_keys %in% flat_keys), "missing")),
    ck("every flat admission is in cohort_info", all(flat_keys %in% ci_keys),
       paste(sum(!flat_keys %in% ci_keys), "extra")),
    ck("every admission belongs to one patient",
       nrow(unique(data.frame(k = ci_keys, p = pid))) == length(unique(ci_keys)),
       paste(nrow(unique(data.frame(k = ci_keys, p = pid))) - length(unique(ci_keys)), "admissions with more than one patient")),
    ck("every admission has a patient id", all(!is.na(pid) & nzchar(pid)),
       paste(sum(is.na(pid) | !nzchar(pid)), "rows without one")),
    ck("visit windows found for every admission", if (any(win)) all(win) else NA,
       if (any(win)) paste(sum(!win), "admissions without a window") else "no visit windows in this schema"),
    ck("hospital_los agrees with the visit window", if (length(los)) mean(los <= 0.01) >= 0.99 else NA,
       if (length(los)) sprintf("%.1f%% of %s admissions within 0.01 days", 100 * mean(los <= 0.01), fmt_n(length(los)))
       else "no admissions with both"),
    ck("ICU stays start within their admission (1 day tolerance)", icu_ok, icu_detail),
    ck("ages are plausible (0-120)", if ("age" %in% names(adm)) all(adm$age >= 0 & adm$age <= 120, na.rm = TRUE) else NA,
       if ("age" %in% names(adm)) paste(sum(adm$age > 120 | adm$age < 0, na.rm = TRUE), "outside") else "no age")
  )
}

# ---------------------------------------------------------------------------
# Patient-level reads (live)
# ---------------------------------------------------------------------------

.xp_empty_ts <- function() {
  tibble::tibble(adm_id = character(), icu_id = character(), t_hadm = numeric(), t_icu = numeric(),
                 concept = character(), concept_raw = character(), value = numeric(),
                 value_chr = character(), unit = character(), source_table = character())
}

# Ids to pass to get_patient_ts()/get_patient_flat() for one patient: the
# values of the source's cohort key (subject_id, or every ICU stay for eICU).
.xp_key_ids <- function(bundle, source, patient_id) {
  key <- cohort_key(source)
  ids <- .source_entry(source)$select_id_cols
  pos <- match(key, ids)
  a <- bundle$ctx_adm[bundle$ctx_adm$source == source & bundle$ctx_adm$patient_id == patient_id, ]
  if (is.na(pos) || pos == 1) return(patient_id)
  if (pos == 2) return(unique(a$adm_id))
  unique(bundle$icu$icu_id[bundle$icu$source == source & bundle$icu$patient_id == patient_id])
}

#' Read one patient's harmonized time series for the explorer
#'
#' `explorer_patient_ts()` returns the series of one patient either through the
#' per-source harmonized tables ([get_patient_ts()]) or from the merged schema,
#' in one normalized long format (minutes since hospital admission).
#' `explorer_patient_dur()` returns the patient's medication exposures from the
#' merged duration table.
#'
#' @param bundle A bundle from [explorer_data()].
#' @param source Source name.
#' @param patient_id Patient identifier (`src_patient_id` in the merged schema).
#' @param mode `"source"` (per-source harmonized tables) or `"merged"`.
#' @param con Optional open DBI connection.
#' @return A tibble with columns adm_id, icu_id, t_hadm, t_icu (minutes),
#'   concept, concept_raw, value (numeric), value_chr, unit and source_table.
#' @export
#'
#' @examples
#' \dontrun{
#' d <- explorer_data("rendehr_admission")
#' p <- d$patients[1, ]
#' explorer_patient_ts(d, p$source, p$patient_id)
#' }
explorer_patient_ts <- function(bundle, source, patient_id, mode = c("source", "merged"), con = NULL) {
  mode <- match.arg(mode)
  patient_id <- as.character(patient_id)
  .with_connection(con, function(con) {
    if (mode == "merged") {
      x <- .xp_query(con, sprintf(
        "SELECT src_admission_id, src_icu_id, time_since_hadm_min, time_since_icu_min, concept_name,
                value, unit, source_table
         FROM %s WHERE source_data = $1 AND src_patient_id = $2",
        .xp_quote_table(con, bundle$meta$schema, "ts")), params = list(source, patient_id))
      if (!nrow(x)) return(.xp_empty_ts())
      out <- tibble::tibble(adm_id = .xp_id(x$src_admission_id), icu_id = .xp_id(x$src_icu_id),
                            t_hadm = as.numeric(x$time_since_hadm_min), t_icu = as.numeric(x$time_since_icu_min),
                            concept = x$concept_name, concept_raw = x$concept_name,
                            value = suppressWarnings(as.numeric(x$value)),
                            value_chr = as.character(x$value), unit = as.character(x$unit),
                            source_table = as.character(x$source_table))
    } else {
      ids <- .xp_key_ids(bundle, source, patient_id)
      ic <- .source_entry(source)$select_id_cols
      x <- dplyr::bind_rows(lapply(ids, function(i) {
        r <- tibble::as_tibble(get_patient_ts(source, i, con = con))
        if (!nrow(r)) return(NULL)
        tibble::tibble(adm_id = as.character(r[[ic[2]]]),
                       icu_id = if (ic[3] %in% names(r)) as.character(r[[ic[3]]]) else NA_character_,
                       t_hadm = as.numeric(r$time_since_hadm_min),
                       t_icu = if ("time_since_icu_min" %in% names(r)) as.numeric(r$time_since_icu_min) else NA_real_,
                       concept = r$concept_name, concept_raw = r$concept_name,
                       value = suppressWarnings(as.numeric(r$value)), value_chr = as.character(r$value),
                       unit = as.character(r$unit),
                       source_table = if ("source_table" %in% names(r)) as.character(r$source_table) else NA_character_)
      }))
      if (!nrow(x)) return(.xp_empty_ts())
      out <- x
    }
    # Keep only the patient's admissions in this bundle (per-source tables can
    # hold rows of admissions outside the merged scope).
    keep <- bundle$ctx_adm$adm_id[bundle$ctx_adm$source == source & bundle$ctx_adm$patient_id == patient_id]
    out <- out[out$adm_id %in% keep, ]
    out[order(out$adm_id, out$t_hadm), ]
  })
}

#' @rdname explorer_patient_ts
#' @export
explorer_patient_dur <- function(bundle, source, patient_id, con = NULL) {
  patient_id <- as.character(patient_id)
  empty <- tibble::tibble(adm_id = character(), icu_id = character(), drug = character(),
                          start_min = numeric(), end_min = numeric(), duration_min = numeric(),
                          value = numeric(), unit = character(), negative_duration = logical())
  .with_connection(con, function(con) {
    if (!.xp_table_exists(con, bundle$meta$schema, "dur")) return(empty)
    x <- .xp_query(con, sprintf(
      "SELECT src_admission_id, src_icu_id, concept_name, end_offset_min, duration_min, value, unit
       FROM %s WHERE source_data = $1 AND src_patient_id = $2",
      .xp_quote_table(con, bundle$meta$schema, "dur")), params = list(source, patient_id))
    if (!nrow(x)) return(empty)
    out <- tibble::tibble(adm_id = .xp_id(x$src_admission_id), icu_id = .xp_id(x$src_icu_id),
                          drug = sub("_exp$", "", x$concept_name),
                          start_min = x$end_offset_min - x$duration_min, end_min = x$end_offset_min,
                          duration_min = x$duration_min, value = suppressWarnings(as.numeric(x$value)),
                          unit = as.character(x$unit))
    # Exposures that end before they start are shown as a point at the start.
    out$negative_duration <- !is.na(out$duration_min) & out$duration_min < 0
    out$end_min[out$negative_duration] <- out$start_min[out$negative_duration]
    out[order(out$adm_id, out$start_min), ]
  })
}

# Flat rows of one patient through get_patient_flat() (all columns as text).
.xp_patient_flat <- function(bundle, source, patient_id, con = NULL) {
  ids <- .xp_key_ids(bundle, source, as.character(patient_id))
  dplyr::bind_rows(lapply(ids, function(i) {
    f <- tibble::as_tibble(get_patient_flat(source, i, con = con))
    dplyr::mutate(f, dplyr::across(dplyr::everything(), as.character))
  }))
}

# Diagnoses and procedures of one patient's admissions from the source cohort
# tables (MIMIC-III, MIMIC-IV, eICU). Other families return the merged
# cohort_info code list.
.xp_patient_codes <- function(bundle, source, patient_id, con) {
  fam <- src_family(source)
  sn <- src_schema_names(source)$raw
  a <- bundle$ctx_adm[bundle$ctx_adm$source == source & bundle$ctx_adm$patient_id == patient_id, ]
  q <- function(sql) tryCatch(.xp_query(con, sql), error = function(e) { message("explorer: ", conditionMessage(e)); NULL })
  tb <- function(t) .xp_quote_table(con, sn, t)
  ids <- a$adm_id
  dx <- px <- NULL
  if (fam %in% c("mimic_iii", "mimic_iv") && length(ids)) {
    iv <- fam == "mimic_iv"
    code <- if (iv) "icd_code" else "icd9_code"
    inl <- paste(DBI::dbQuoteLiteral(con, as.numeric(ids)), collapse = ", ")
    # MIMIC-IV codes are unique per (code, version); MIMIC-III codes are ICD-9 only.
    ver_sel <- function(a) if (iv) sprintf("CAST(%s.icd_version AS TEXT)", a) else "'9'"
    ver_join <- function(a) if (iv) sprintf(" AND CAST(t.icd_version AS TEXT) = CAST(%s.icd_version AS TEXT)", a) else ""
    dx <- q(sprintf(
      "SELECT CAST(d.hadm_id AS TEXT) AS adm_id, d.seq_num, TRIM(CAST(d.%1$s AS TEXT)) AS icd_code,
              %2$s AS icd_version, t.long_title AS title
       FROM %3$s d LEFT JOIN %4$s t ON TRIM(CAST(t.%1$s AS TEXT)) = TRIM(CAST(d.%1$s AS TEXT))%5$s
       WHERE d.hadm_id IN (%6$s) ORDER BY d.hadm_id, d.seq_num",
      code, ver_sel("d"), tb("diagnoses_icd"), tb("d_icd_diagnoses"), ver_join("d"), inl))
    px <- q(sprintf(
      "SELECT CAST(p.hadm_id AS TEXT) AS adm_id, TRIM(CAST(p.%1$s AS TEXT)) AS code, %2$s AS icd_version,
              %3$s AS date, t.long_title AS title
       FROM %4$s p LEFT JOIN %5$s t ON TRIM(CAST(t.%1$s AS TEXT)) = TRIM(CAST(p.%1$s AS TEXT))%6$s
       WHERE p.hadm_id IN (%7$s)",
      code, ver_sel("p"), if (iv) "CAST(p.chartdate AS DATE)" else "CAST(NULL AS DATE)",
      tb("procedures_icd"), tb("d_icd_procedures"), ver_join("p"), inl))
  } else if (fam == "eicu") {
    icu <- bundle$icu[bundle$icu$source == source & bundle$icu$patient_id == patient_id, ]
    if (nrow(icu)) {
      inl <- paste(DBI::dbQuoteLiteral(con, as.numeric(icu$icu_id)), collapse = ", ")
      dx <- q(sprintf("SELECT CAST(patientunitstayid AS TEXT) AS icu_id, diagnosisoffset AS offset_min,
                              icd9code AS icd_code, 'eICU problem list' AS icd_version,
                              diagnosisstring AS title, diagnosispriority AS priority
                       FROM %s WHERE patientunitstayid IN (%s)", tb("diagnosis"), inl))
      px <- q(sprintf("SELECT CAST(patientunitstayid AS TEXT) AS icu_id, treatmentoffset AS offset_min,
                              NULL AS code, 'eICU treatment' AS icd_version, treatmentstring AS title
                       FROM %s WHERE patientunitstayid IN (%s)", tb("treatment"), inl))
      lk <- dplyr::select(icu, icu_id, adm_id)
      if (!is.null(dx)) dx <- dplyr::left_join(dx, lk, by = "icu_id")
      if (!is.null(px)) px <- dplyr::left_join(px, lk, by = "icu_id")
    }
  }
  if (is.null(dx) || !nrow(dx)) {
    dx <- dplyr::bind_rows(lapply(seq_len(nrow(a)), function(i) {
      cc <- trimws(strsplit(a$diag_icd_codes[i] %||% "", " | ", fixed = TRUE)[[1]])
      cc <- cc[nzchar(cc) & !is.na(cc)]
      if (!length(cc)) return(NULL)
      tibble::tibble(adm_id = a$adm_id[i], icd_code = cc, icd_version = NA_character_, title = NA_character_)
    }))
  }
  list(dx = dx %||% tibble::tibble(), px = px %||% tibble::tibble())
}

# ---------------------------------------------------------------------------
# Raw laboratory values (verification against the PhysioNet files)
# ---------------------------------------------------------------------------

# Laboratory tests with their raw identifiers in MIMIC (itemid) and eICU (labname).
.xp_raw_items <- tibble::tribble(
  ~concept, ~mimic_itemid, ~eicu_labname,
  "hemoglobin", 51222, "Hgb", "hematocrit", 51221, "Hct", "wbc", 51301, "WBC x 1000",
  "platelet", 51265, "platelets x 1000", "sodium", 50983, "sodium", "potassium", 50971, "potassium",
  "chloride", 50902, "chloride", "bicarbonate", 50882, "bicarbonate", "bun", 51006, "BUN",
  "creatinine", 50912, "creatinine", "glucose", 50931, "glucose", "calcium", 50893, "calcium",
  "magnesium", 50960, "magnesium", "phosphate", 50970, "phosphate",
  "prothrombin_time_inr", 51237, "PT - INR", "ptt", 51275, "PTT", "anion_gap", 50868, "anion gap",
  "rbc", 51279, "RBC", "mcv", 51250, "MCV", "mch", 51248, "MCH", "mchc", 51249, "MCHC", "rdw", 51277, "RDW")

# Concept names the merged tables may use for the same tests.
.xp_lab_alias <- c(calcium_total = "calcium", bili = "bilirubin_total", bili_dir = "bilirubin_direct",
                   alp = "alkaline_phosphatase", alt = "alanine_aminotransferase",
                   ast = "aspartate_aminotransferase")

.xp_raw_file <- function(bundle, source, table) {
  dirs <- c(if (!is.null(bundle$meta$raw_dirs)) bundle$meta$raw_dirs[[source]],
            tryCatch({ p <- ehr_paths(source); c(p$raw_parquet_cache_dir, p$raw_dir) }, error = function(e) NULL))
  dirs <- unique(dirs[!is.na(dirs) & nzchar(dirs)])
  # MIMIC-IV keeps its tables under hosp/ and icu/; MIMIC-III file names are upper case.
  for (d in dirs) for (sub in c("", "hosp", "icu")) for (ext in c(".parquet", ".csv.gz", ".csv")) {
    for (nm in c(table, toupper(table))) {
      f <- file.path(d, sub, paste0(nm, ext))
      if (file.exists(f)) return(f)
    }
  }
  NULL
}

.xp_duck_reader <- function(f) {
  if (grepl("\\.parquet$", f)) sprintf("read_parquet('%s')", gsub("'", "''", f))
  else sprintf("read_csv_auto('%s', ignore_errors = true)", gsub("'", "''", f))
}

# Raw laboratory values of one patient, by admission, read with DuckDB.
.xp_raw_labs <- function(bundle, source, patient_id) {
  fam <- src_family(source)
  if (fam %in% c("mimic_iii", "mimic_iv")) {
    f <- .xp_raw_file(bundle, source, "labevents")
    if (is.null(f)) return(NULL)
    dcon <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(dcon, shutdown = TRUE), add = TRUE)
    r <- DBI::dbGetQuery(dcon, sprintf(
      "SELECT CAST(CAST(hadm_id AS BIGINT) AS VARCHAR) AS adm_id, CAST(itemid AS INTEGER) AS itemid,
              CAST(valuenum AS DOUBLE) AS value
       FROM %s WHERE CAST(subject_id AS BIGINT) = %s AND valuenum IS NOT NULL AND itemid IN (%s)",
      .xp_duck_reader(f), as.numeric(patient_id), paste(.xp_raw_items$mimic_itemid, collapse = ",")))
    r <- r[!is.na(r$adm_id), ]
    tibble::tibble(adm_id = r$adm_id, concept = .xp_raw_items$concept[match(r$itemid, .xp_raw_items$mimic_itemid)],
                   value = r$value)
  } else if (fam == "eicu") {
    f <- .xp_raw_file(bundle, source, "lab")
    stays <- bundle$icu[bundle$icu$source == source & bundle$icu$patient_id == patient_id, ]
    if (is.null(f) || !nrow(stays)) return(NULL)
    dcon <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(dcon, shutdown = TRUE), add = TRUE)
    r <- DBI::dbGetQuery(dcon, sprintf(
      "SELECT CAST(patientunitstayid AS VARCHAR) AS icu_id, labname, CAST(labresult AS DOUBLE) AS value
       FROM %s WHERE patientunitstayid IN (%s) AND labresult IS NOT NULL",
      .xp_duck_reader(f), paste(as.numeric(stays$icu_id), collapse = ",")))
    r <- dplyr::inner_join(tibble::as_tibble(r), dplyr::select(.xp_raw_items, concept, eicu_labname),
                           by = c(labname = "eicu_labname"))
    r <- dplyr::left_join(r, dplyr::select(stays, icu_id, adm_id), by = "icu_id")
    dplyr::select(r, adm_id, concept, value)
  } else NULL
}

# Per admission and test: harmonized versus raw counts and means; agreement needs equal counts.
.xp_compare_raw <- function(ts, raw) {
  `%>%` <- magrittr::`%>%`
  ts$concept <- ifelse(ts$concept %in% names(.xp_lab_alias), .xp_lab_alias[ts$concept], ts$concept)
  h <- ts %>% dplyr::filter(concept %in% .xp_raw_items$concept, !is.na(value)) %>%
    dplyr::group_by(adm_id, concept) %>%
    dplyr::summarise(n_harmonized = dplyr::n(), mean_harmonized = mean(value), .groups = "drop")
  r <- raw %>% dplyr::group_by(adm_id, concept) %>%
    dplyr::summarise(n_raw = dplyr::n(), mean_raw = mean(value), .groups = "drop")
  dplyr::full_join(h, r, by = c("adm_id", "concept")) %>%
    dplyr::mutate(n_ratio = n_harmonized / n_raw,
                  rel_diff = abs(mean_harmonized - mean_raw) / pmax(abs(mean_raw), 1e-9),
                  agreement = dplyr::case_when(is.na(n_raw) ~ "harmonized only",
                                               is.na(n_harmonized) ~ "raw only",
                                               n_harmonized != n_raw ~ "count differs",
                                               rel_diff <= 0.01 ~ "agree (within 1%)",
                                               TRUE ~ "differs (> 1%)")) %>%
    dplyr::arrange(adm_id, concept)
}

# Per admission and concept: row and value counts, sums of values and of
# t_hadm of two reads of the same patient (per-source harmonized versus merged schema).
.xp_compare_routes <- function(a, b, lab_a = "source", lab_b = "merged") {
  # sums of times catch shifted clocks; sums are compared unrounded with a relative tolerance
  st <- "t_hadm" %in% names(a) && "t_hadm" %in% names(b)
  stats <- c("n", "n_value", "sum_value", if (st) "sum_t_hadm")
  agg <- function(x, lab) {
    s <- dplyr::summarise(dplyr::group_by(x, adm_id, concept_raw), n = dplyr::n(),
                          n_value = sum(!is.na(value)), sum_value = sum(value, na.rm = TRUE),
                          sum_t_hadm = if (st) sum(t_hadm, na.rm = TRUE) else NA_real_, .groups = "drop")
    s <- s[, c("adm_id", "concept_raw", stats)]
    names(s)[-(1:2)] <- paste0(stats, "_", lab)
    s
  }
  j <- dplyr::full_join(agg(a, lab_a), agg(b, lab_b), by = c("adm_id", "concept_raw"))
  same <- function(s) {
    x <- j[[paste0(s, "_", lab_a)]]; y <- j[[paste0(s, "_", lab_b)]]
    dplyr::coalesce(abs(x - y) <= 1e-6 * pmax(abs(x), abs(y), 1), FALSE)
  }
  j$match <- Reduce(`&`, lapply(stats, same))
  j
}

# The merged ts rows of cohort admissions, as a FROM item aliased x (cohort
# membership from the cohort_index / hos_cohort_flg flag when present).
# Subquery of the cohort admissions (source_data, src_admission_id) of a merged schema.
.xp_cohort_adm <- function(con, schema) {
  ci <- .xp_quote_table(con, schema, "cohort_info")
  flag <- intersect(c("cohort_index", "hos_cohort_flg"),
                    DBI::dbGetQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_schema = $1 AND table_name = 'cohort_info'",
                                    params = list(schema))$column_name)
  if (length(flag)) sprintf("(SELECT DISTINCT source_data, src_admission_id FROM %s WHERE CAST(%s AS text) IN ('1', 'true', 't', 'TRUE'))",
                            ci, DBI::dbQuoteIdentifier(con, flag[1]))
  else sprintf("(SELECT DISTINCT source_data, src_admission_id FROM %s)", ci)
}
.xp_cohort_ts <- function(con, schema) {
  sprintf("(SELECT t.* FROM %s t JOIN %s c ON c.source_data = t.source_data AND c.src_admission_id = t.src_admission_id) x",
          .xp_quote_table(con, schema, "ts"), .xp_cohort_adm(con, schema))
}

# Mean value of one concept per cohort admission and time bin (bin_h hours
# wide) within horizon_h hours of hospital ("hadm") or ICU ("icu") admission.
.xp_trajectory <- function(con, schema, concept, clock = c("hadm", "icu"), horizon_h = 72, bin_h = 4) {
  col <- if (match.arg(clock) == "icu") "time_since_icu_min" else "time_since_hadm_min"
  x <- .xp_query(con, sprintf(
    "SELECT source_data AS source, src_admission_id AS adm_id, FLOOR(%1$s / ($2 * 60.0)) AS bin, AVG(value) AS value
     FROM %2$s WHERE concept_name = $1 AND value IS NOT NULL AND %1$s >= 0 AND %1$s < $3 * 60.0 GROUP BY 1, 2, 3",
    col, .xp_cohort_ts(con, schema)), params = list(concept, bin_h, horizon_h))
  dplyr::mutate(x, adm_id = as.character(adm_id), bin = as.integer(bin), value = as.numeric(value))
}

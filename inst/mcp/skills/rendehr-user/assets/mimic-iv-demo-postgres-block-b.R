# Skill output asset: read/adapt only; never execute when loading the skill.
# Live-run on the MIMIC-IV demo with a scratch database (development branch,
# October 2026): 100 patients, 22 flat and 13 time-series concepts, no scores.
# Running this continuation downloads data and builds database outputs.
# run_concepts writes generated SQL under the project's sql/generated/ tree.
# ---- Block B: validate the edited setup, then build ----
# Select this block separately after editing the local files.
library(rendehR)

project_dir <- "[project path]"  # Same values as Block A
data_dir <- "[data path]"
database_name <- "[database name]"
if (project_dir == "[project path]" || data_dir == "[data path]" ||
    database_name == "[database name]") {
  stop("Replace all three placeholders before running Block B.")
}
project_dir <- normalizePath(project_dir, mustWork = TRUE)
data_dir <- normalizePath(data_dir, mustWork = TRUE)
config_path <- file.path(project_dir, "config", "ehr_config.json")
if (!file.exists(config_path)) stop("Project configuration is missing. Run Block A first.")

check <- ehr_check_config(config_path = config_path, quiet = TRUE)
if (!isTRUE(check$ok) || length(check$problems) > 0L) {
  if (length(check$problems)) message(paste(check$problems, collapse = "\n"))
  stop("Correct the configuration problems before continuing.")
}
reviewed_warnings <- character() # Add only exact messages reviewed and accepted locally.
pending_warnings <- setdiff(check$warnings, reviewed_warnings)
if (length(pending_warnings)) {
  message(paste(pending_warnings, collapse = "\n"))
  stop("Review the configuration warnings before continuing.")
}

ehr_disconnect() # Clear credentials cached before the local files were edited.
cfg <- ehr_use_config(config_path)
if (!identical(cfg$source$src_name, "mimic_iv_demo") ||
    !identical(cfg$cohort$profile, "generic") ||
    !identical(cfg$database$db_name, database_name) ||
    !identical(normalizePath(cfg$project$base_dir, mustWork = TRUE), project_dir) ||
    !identical(normalizePath(cfg$paths$raw_data_root, mustWork = FALSE),
               file.path(data_dir, "physionet_data")) ||
    !identical(normalizePath(cfg$paths$cohort_parquet_root, mustWork = FALSE),
               file.path(data_dir, "cohorts_data"))) {
  stop("The edited source, profile, database, or paths differ from this script.")
}

creds <- load_credentials(path = check$paths$credentials_json)
con <- connect_postgres(db_name = database_name, creds = creds)
tryCatch({
  result <- DBI::dbGetQuery(con, "SELECT 1 AS connection_ok")
  stopifnot(nrow(result) == 1L, result$connection_ok[[1L]] == 1)

  # Harmonization auto-discovers existing concept tables. Require an empty
  # destination so a score from a previous run cannot enter the new outputs.
  schemas <- src_schema_names("mimic_iv_demo")
  existing <- DBI::dbGetQuery(con, "SELECT schemaname, tablename FROM pg_tables")
  previous <- existing[existing$schemaname %in%
                         unlist(schemas[c("concepts", "harmonized", "derived")]), ]
  previous_scores <- existing[
    existing$schemaname == schemas$raw &
      existing$tablename %in% c("iss", "comorbidities"), ]
  if (nrow(previous) || nrow(previous_scores)) {
    stop("Use a fresh source database: previous concept, harmonized, derived, or score tables exist.")
  }
}, finally = DBI::dbDisconnect(con))

paths <- ehr_paths(src_name = "mimic_iv_demo", base_dir = project_dir)
package_flat <- system.file("sql", "mimic_iv", "concepts_sqls", "flat",
                            package = "rendehR")
package_ts <- system.file("extdata", "rendehr_concepts_config.csv",
                          package = "rendehR")
package_dur <- system.file("extdata", "rendehr_duration_concepts_config.csv",
                           package = "rendehR")
if (!nzchar(package_flat) || !nzchar(package_ts) || !nzchar(package_dur) ||
    !identical(paths$pkg_flat_sql_dir, package_flat) ||
    !identical(paths$concept_config_csv_ts, package_ts) ||
    !identical(paths$concept_config_csv_dur, package_dur)) {
  stop("Concept definitions differ from the packaged definitions reviewed for this script.")
}
# NULL when no supplemental definitions are configured
user_files <- c(paths$user_concept_config_csv_ts, paths$user_concept_config_csv_dur)
if (length(user_files) && any(file.exists(user_files))) {
  stop("Supplemental concept definitions exist; review them before this fixed no-score workflow.")
}

# Fixed allowlist: run_concepts() otherwise enables flat, ts, dur, ehr, user,
# and external. The ehr layer builds SOFA, SAPS II, OASIS, Charlson, and other
# scores. The flat layer includes guarded ISS/NISS/max AIS and injury outputs.
flat_names <- c(
  "admission_service", "admission_type", "age", "care_site", "height",
  "hospital_admission_location", "hospital_discharge_location",
  "hospital_los", "hospital_mortality", "icu_first_careunit",
  "icu_last_careunit", "icu_los", "icu_mortality", "insurance",
  "language", "marital_status", "race", "readmission_30d", "sex",
  "time_to_death_from_admission", "time_to_death_from_discharge",
  "weight_admit"
)
flat_files <- flat_names  # flat SQL files are named after their concept
ts_names <- c(
  "heart_rate", "sbp", "dbp", "map", "resp_rate", "spo2",
  "temperature", "creatinine", "hemoglobin", "platelet", "glucose",
  "sodium", "lactate"
)
flat_available <- tools::file_path_sans_ext(list.files(paths$pkg_flat_sql_dir,
                                                        pattern = "\\.sql$"))
if (!all(flat_files %in% flat_available)) stop("A selected flat SQL file is unavailable.")
ts_config <- utils::read.csv(paths$concept_config_csv_ts, stringsAsFactors = FALSE)
if (!all(ts_names %in% ts_config$concept_name[
    ts_config$source == "mimic_iv" & ts_config$typ == "ts"])) {
  stop("A selected time-series concept is unavailable.")
}
dur_config <- utils::read.csv(paths$concept_config_csv_dur, stringsAsFactors = FALSE)
dur_rows <- dur_config[dur_config$source == "mimic_iv" &
                         dur_config$typ == "dur", ]
if (!nrow(dur_rows) || anyNA(dur_rows$category) ||
    !all(dur_rows$category == "medication exposure")) {
  stop("Duration definitions changed; review them before continuing.")
}
dur_names <- unique(dur_rows$concept_name)
selected_names <- c(flat_files, ts_names, dur_names)

message("Configuration reviewed and database connection verified.")
download_source(src_name = "mimic_iv_demo")
import_cohort_general(
  src_name = "mimic_iv_demo", base_dir = project_dir,
  no_filter = TRUE, overwrite = FALSE
)
run_db(
  src_name = "mimic_iv_demo", db_name = database_name,
  base_dir = project_dir, engine = "postgres"
)
# run_db() loads the cohort-aware window tables. MIMIC-IV has no packaged raw
# modifications. No enrichment score stage or project SQL layer runs.
run_concepts(
  src_name = "mimic_iv_demo", db_name = database_name,
  base_dir = project_dir, engine = "postgres",
  layers = c("flat", "ts", "dur"), concept_name = selected_names
)
run_harmonized_tables(
  src_name = "mimic_iv_demo", db_name = database_name,
  base_dir = project_dir, flat_concepts = flat_names
)

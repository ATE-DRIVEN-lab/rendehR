# Skill output asset: read/adapt only; never execute when loading the skill.
# Live-run on the MIMIC-IV demo with a scratch database (development branch,
# October 2026): 100 patients, 22 flat and 13 time-series concepts, no scores.
# MIMIC-IV demo / PostgreSQL / whole demo population / no clinical scores.
# Run Block A by itself. Edit the two local config files, then run Block B.
# The target PostgreSQL database must already exist.

# ---- Block A: create templates and stop ----
library(rendehR)

project_dir <- "[project path]"
data_dir <- "[data path]"
database_name <- "[database name]"
if (project_dir == "[project path]" || data_dir == "[data path]" ||
    database_name == "[database name]") {
  stop("Replace all three placeholders before running Block A.")
}

dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
project_dir <- normalizePath(project_dir, mustWork = TRUE)
data_dir <- normalizePath(data_dir, mustWork = TRUE)
dir.create(file.path(project_dir, "config"), showWarnings = FALSE)

ehr_create_config_template(
  path = file.path(project_dir, "config", "ehr_config.json"),
  src_name = "mimic_iv_demo", db_name = database_name, base_dir = project_dir,
  raw_data_root = file.path(data_dir, "physionet_data"),
  cohort_parquet_root = file.path(data_dir, "cohorts_data"),
  duckdb_root = file.path(data_dir, "duckdb"),
  credentials_path = file.path("config", "config_pass.json"),
  cohort_profile = "generic", overwrite = FALSE
)
ehr_create_credentials_template(
  path = file.path(project_dir, "config", "config_pass.json"),
  overwrite = FALSE
)
stop(
  "Templates are ready. Edit your local project and credential files, then run Block B separately.",
  call. = FALSE
)


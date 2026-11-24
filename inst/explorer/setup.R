# Cohort explorer: app-wide objects built from the data bundle.
# Sourced by rendehR::explorer_app() into the app environment, where XD is the
# bundle from explorer_data() and XLIVE says whether patient-level queries can
# reach the database. Every module file reads the objects defined here.

validate <- shiny::validate
need <- shiny::need
# Tables take the height of their rows: DT's default fill = TRUE squeezes them
# into the card's height and cuts off rows.
DTOutput <- function(outputId, width = "100%", height = "auto", fill = FALSE) DT::DTOutput(outputId, width, height, fill)
# Sliders show their two values; the tick labels underneath only crowd the axis.
sliderInput <- function(..., ticks = FALSE) shiny::sliderInput(..., ticks = ticks)
options(dplyr.summarise.inform = FALSE, spinner.type = 6, spinner.color = "#68727c", scipen = 999)

D        <- XD
LIVE     <- isTRUE(XLIVE)
ADM      <- D$adm              # cohort admissions (one row per admission)
CTX_ADM  <- D$ctx_adm          # every admission in the merged schema (patient context)
CTX_ICU  <- D$icu              # ICU stays, minutes since hospital admission
PATIENTS <- D$patients         # cohort patients (search index)
VARS     <- D$vars
GROUP_CHOICES <- D$groups
FILTERS  <- D$filters
SCI      <- !is.null(D$sci)

SRC        <- D$meta$sources
SRC_LABEL  <- D$meta$source_labels
SRC_COLORS <- D$meta$source_colors
# sex keeps the colours of the age pyramid on every chart
PALETTES   <- utils::modifyList(list(sex = c(female = "#78909c", male = "#37474f")), D$palettes %||% list())
APP_TITLE  <- D$meta$title %||% "rendehR Explorer"

# Shared state for the management pages (created once; setup.R is re-sourced on
# cohort/database switch, so never replace these reactiveValues — observers from
# the live session would keep the destroyed copies and hang the UI).
JOBS_DIR <- file.path(tempdir(), "rendehr-explorer-jobs")
if (!exists(".jobs", inherits = FALSE) || !inherits(.jobs, "reactivevalues"))
  .jobs <- shiny::reactiveValues(list = list())
# src holds every source of the project (a vector); engine is "postgres" or
# "duckdb"; cohort is the cohort selection the pipeline jobs pass on.
if (!exists("JOB_CTX", inherits = FALSE) || !inherits(JOB_CTX, "reactivevalues"))
  JOB_CTX <- shiny::reactiveValues(config = NULL, base_dir = NULL, db_name = NULL, src = NULL,
                                   engine = "postgres", cohort = NULL)
connected <- .ehr_env$db_name %||% ""
# Database switch: drop a Setup project aimed at another DB so jobs cannot run
# against stale paths; then rehydrate from the active ehr_config when present.
# isolate(): setup.R runs outside a reactive consumer (including on cohort rebind).
.prev_job_db <- isolate(JOB_CTX$db_name)
.prev_job_cfg <- isolate(JOB_CTX$config)
if (nzchar(connected) && !is.null(.prev_job_db) && nzchar(.prev_job_db) &&
    !identical(.prev_job_db, connected)) {
  JOB_CTX$config <- NULL
  JOB_CTX$base_dir <- NULL
  JOB_CTX$cohort <- NULL
  JOB_CTX$raw_dir <- NULL
  JOB_CTX$engine <- "postgres"
  .prev_job_cfg <- NULL
}
JOB_CTX$db_name <- if (nzchar(connected)) connected else .prev_job_db
JOB_CTX$src <- unname(SRC)
if (LIVE && is.null(.prev_job_cfg)) {
  cfg <- tryCatch(rendehR::ehr_config(), error = function(e) NULL)
  if (!is.null(cfg) && !is.null(cfg$config_path) && file.exists(cfg$config_path)) {
    JOB_CTX$config <- cfg$config_path
    JOB_CTX$base_dir <- cfg$base_dir %||% dirname(dirname(cfg$config_path))
  }
}

ADM$.all <- "Overall"
if (!"tl_group" %in% names(CTX_ADM)) CTX_ADM$tl_group <- ifelse(CTX_ADM$in_cohort, "Cohort admission", "Context admission")
TL_COLORS <- PALETTES$tl_group %||% c("Cohort admission" = "#37474f", "Context admission" = "#c8cdd3")
INDEX_GROUP <- D$meta$index_group          # optional patient grouping in the Subject Explorer
INDEX_GROUP_LEVELS <- if (!is.null(INDEX_GROUP)) sort(unique(stats::na.omit(PATIENTS[[INDEX_GROUP]]))) else NULL

var_choices <- function(types = c("cont", "cat", "bin"), groups = NULL) {
  v <- VARS[VARS$type %in% types & (is.null(groups) | VARS$group %in% groups), ]
  split(stats::setNames(v$var, v$label), factor(v$group, unique(v$group)))
}
var_label <- function(v) {
  l <- VARS$label[match(v, VARS$var)]
  g <- names(GROUP_CHOICES)[match(v, GROUP_CHOICES)]
  ifelse(!is.na(l), l, ifelse(!is.na(g), g, v))
}
first_in <- function(cands, pool, fallback = pool[1]) {
  x <- intersect(cands, pool)
  if (length(x)) x[1] else fallback
}

# Defaults for the explorer inputs (the bundle may override any of them).
.grp <- unname(GROUP_CHOICES[GROUP_CHOICES != ".all"])
.bins <- VARS$var[VARS$type == "bin"]
.conts <- VARS$var[VARS$type == "cont"]
DEF <- utils::modifyList(list(
  overview_group = first_in(c("sex", "admission_type", "age_group"), setdiff(.grp, "source_label"), ".all"),
  t1_group = first_in("source_label", .grp, ".all"),
  t1_vars = intersect(c("age", "sex", "admission_type", "hospital_admission_location", "any_icu", "hospital_los",
                        "hospital_mortality", "readmission_30d", "hospital_discharge_location"), VARS$var),
  d_var = first_in(c("age", "hospital_los"), VARS$var),
  d_group = first_in("source_label", .grp, ".all"),
  o_var = first_in(c("hospital_mortality", "readmission_30d", "any_icu"), .bins),
  o_group = first_in("source_label", .grp, ".all"),
  o_strata = c("None" = "", stats::setNames(intersect(c("source_label", "sex", "age_group"), names(ADM)),
                                            var_label(intersect(c("source_label", "sex", "age_group"), names(ADM))))),
  s_group = first_in("source_label", .grp, ".all"),
  x_row = first_in(c("source_label", "sex"), .grp),
  x_col = first_in(c("hospital_discharge_location", "admission_type"), VARS$var[VARS$type %in% c("cat", "bin")]),
  sc_x = first_in("age", .conts), sc_y = first_in(c("hospital_los", "icu_los_total"), .conts),
  sc_col = first_in("source_label", .grp, ".all"),
  smd = list(sex = c("male", "female"), hospital_mortality = c("1", "0"), any_icu = c("1", "0")),
  data_cols = intersect(c("source_label", "patient_id", "adm_id", "age", "sex", "admission_type", "any_icu",
                          "hospital_los", "hospital_mortality", "hospital_discharge_location"), names(ADM))
), D$defaults %||% list())

# Survival needs time_to_death_from_admission (days); sources without any
# recorded death time (for example eICU, which has no post-discharge follow-up)
# are left out.
SURV_SOURCES <- if ("time_to_death_from_admission" %in% names(ADM))
  sort(unique(ADM$source[!is.na(ADM$time_to_death_from_admission)])) else character(0)

# Concept categories for pickers: vital signs first, then the schema each
# concept comes from (concepts / derived / external).
VITAL_PANELS <- list("Heart rate and respiration" = c("heart_rate", "resp_rate"),
                     "Blood pressure (mmHg)" = c("sbp", "map", "dbp"),
                     "Oxygen saturation (%)" = c("spo2", "o2sat"),
                     "Temperature (°C)" = "temperature",
                     "Glasgow Coma Scale" = c("egcs", "mgcs", "vgcs"))
# Settings that stay constant until the next charted change (step / hvh lines).
STEP_CONCEPTS <- c("peep", "fio2", "tidal_volume", "vent_mode", "set_rr", "set_tv",
                   "norepinephrine", "epinephrine", "vasopressin", "phenylephrine",
                   "dopamine", "dobutamine", "propofol", "midazolam", "fentanyl",
                   "insulin", "heparin", "nitroglycerin", "nitroprusside")
CONCEPT_CAT <- D$concept_categories %||% {
  cc <- D$concepts
  if (!is.null(cc) && nrow(cc)) { cc <- cc[!duplicated(cc$concept_name), ]; stats::setNames(cc$schema, cc$concept_name) }
  else character(0)
}
concept_category <- function(cn) {
  x <- unname(CONCEPT_CAT[cn])
  x[cn %in% unlist(VITAL_PANELS)] <- "vital signs"
  need_step <- cn %in% STEP_CONCEPTS & (is.na(x) | x %in% c("other", ""))
  x[need_step] <- "settings / infusions"
  x[is.na(x) | x == ""] <- "other"
  x
}

# Patient-level reads (the app environment sees the package internals). SCI
# mode may replace patient_codes() with its own tables.
EMPTY_DUR <- tibble::tibble(adm_id = character(), icu_id = character(), drug = character(), start_min = numeric(),
                            end_min = numeric(), duration_min = numeric(), value = numeric(), unit = character(),
                            negative_duration = logical())
patient_ts <- function(source, patient_id, mode = c("source", "merged")) {
  if (!LIVE) stop("The database connection is not available.")
  explorer_patient_ts(D, source, patient_id, match.arg(mode))
}
patient_flat <- function(source, patient_id) {
  if (!LIVE) return(tibble::tibble())
  .xp_patient_flat(D, source, patient_id)
}
patient_dur <- function(source, patient_id) {
  if (!LIVE) return(EMPTY_DUR)
  explorer_patient_dur(D, source, patient_id)
}
patient_codes <- function(source, patient_id) {
  if (!LIVE) return(list(dx = tibble::tibble(), px = tibble::tibble()))
  .with_connection(NULL, function(con) .xp_patient_codes(D, source, patient_id, con))
}
raw_labs <- function(source, patient_id) .xp_raw_labs(D, source, patient_id)

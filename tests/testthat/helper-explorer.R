# Synthetic merged schema for the cohort explorer tests (no database needed).

.xp_fixture <- function(n_pat = 60, seed = 1) {
  set.seed(seed)
  one <- function(src, admit_is_time) {
    pid <- if (src == "eicu_demo") sprintf("002-%05d", seq_len(n_pat)) else as.character(10000000 + seq_len(n_pat))
    n_adm <- sample(1:3, n_pat, replace = TRUE, prob = c(.6, .3, .1))
    p <- rep(pid, n_adm)
    k <- length(p)
    adm <- as.character(200000 + seq_len(k) + if (src == "eicu_demo") 50000 else 0)
    icu <- ifelse(runif(k) < 0.6, as.character(300000 + seq_len(k) + if (src == "eicu_demo") 50000 else 0), NA)
    los_min <- round(runif(k, 600, 20000))
    admit <- as.POSIXct("2150-01-01", tz = "UTC") + cumsum(round(runif(k, 1e6, 1e7)))
    died <- rbinom(k, 1, 0.12)
    list(
      ci = data.frame(source_data = src, src_patient_id = p, src_admission_id = adm, src_icu_id = icu,
                      cohort_index = ifelse(runif(k) < 0.85, "true", "false"), seq_hos = ave(seq_len(k), p, FUN = seq_along),
                      num_diag_icd_codes = sample(1:20, k, TRUE), diag_icd_codes = "I10 | E119", stringsAsFactors = FALSE),
      fl = data.frame(source_data = src, src_patient_id = p, src_admission_id = adm, src_icu_id = icu,
                      age = as.character(round(runif(k, 18, 90))), sex = sample(c("female", "male"), k, TRUE),
                      race = sample(c("white", "black", "other"), k, TRUE),
                      admission_type = sample(c("emergency", "elective", "urgent"), k, TRUE),
                      hospital_los = as.character(round(los_min / 1440, 4)), hospital_mortality = as.character(died),
                      hospital_discharge_location = sample(c("home", "rehab", "snf"), k, TRUE),
                      icu_first_careunit = ifelse(is.na(icu), NA, sample(c("micu", "sicu"), k, TRUE)),
                      icu_los = ifelse(is.na(icu), NA, "1.5"), icu_mortality = ifelse(is.na(icu), NA, as.character(died)),
                      readmission_30d = as.character(rbinom(k, 1, 0.1)),
                      time_to_death_from_admission = if (src == "eicu_demo") NA else ifelse(runif(k) < 0.3, as.character(round(runif(k, 1, 400), 2)), NA),
                      stringsAsFactors = FALSE),
      win = data.frame(source = src, patient_id = p, adm_id = adm, icu_id = icu,
                       admittime = if (admit_is_time) admit else as.POSIXct(NA),
                       hosp_los_min = los_min, icu_in_min = ifelse(is.na(icu), NA, 60), icu_out_min = ifelse(is.na(icu), NA, 60 + 2160),
                       stringsAsFactors = FALSE))
  }
  a <- one("mimic_iv_demo", TRUE); b <- one("eicu_demo", FALSE)
  list(ci = rbind(a$ci, b$ci), fl = rbind(a$fl, b$fl), win = rbind(a$win, b$win))
}
.xp_fixture_bundle <- function(...) {
  f <- .xp_fixture(...)
  fl <- suppressMessages(readr::type_convert(tibble::as_tibble(f$fl), guess_integer = FALSE))
  .xp_build(tibble::as_tibble(f$ci), fl, tibble::as_tibble(f$win), schema = "rendehr_admission")
}

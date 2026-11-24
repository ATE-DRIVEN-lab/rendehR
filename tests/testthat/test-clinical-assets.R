test_that("Clinical asset manifest validates all packaged runtime clinical assets", {
  manifest <- .load_clinical_asset_manifest()
  expect_identical(manifest$format_version, 1L)
  expect_length(manifest$assets, 6L)
  for (entry in manifest$assets) {
    path <- ehr_config_path(entry$filename)
    expect_true(file.exists(path), info = entry$filename)
    expect_identical(tolower(.clinical_sha256(path)),
                     tolower(entry$sha256), info = entry$filename)
  }
})

test_that("canonical SCI asset carries coded location columns and obsolete names are absent", {
  sci <- .load_icd_filter(ehr_config_path("scid_icds_rendehr.rds"))
  expect_true(all(c("anatomical_region", "specific_level") %in% names(sci)))
  expect_identical(system.file("extdata", "rendehr_scid_icds.csv",
                               package = "rendehR"), "")
  expect_identical(system.file("extdata", "icd_master2_modified.rds",
                               package = "rendehR"), "")
  expect_identical(system.file("extdata", "add_injury_level_to_icds.R",
                               package = "rendehR"), "")
})

test_that("the injury cause list is no longer shipped", {
  # Injury mechanism and intent come from run_iss() (external-cause codes).
  expect_identical(system.file("extdata", "injury_cause_icds_rendehr.csv",
                               package = "rendehR"), "")
  ids <- vapply(jsonlite::fromJSON(ehr_config_path("rendehr-clinical-assets.json"),
                                   simplifyVector = FALSE)$assets,
                function(a) a$id, character(1))
  expect_false("injury_cause_classification" %in% ids)
})
test_that("MIMIC-III NICU stays are neonatal, not neurological", {
  for (side in c("first", "last")) {
    sql <- paste(readLines(system.file("sql", "mimic_iii", "concepts_sqls", "flat",
                                       sprintf("icu_%s_careunit.sql", side),
                                       package = "rendehR")), collapse = "\n")
    expect_match(sql, "IN ('NICU') THEN 'neonatal'", fixed = TRUE)
    expect_false(grepl("'NICU',\\s*'Neurology'", sql))
  }
})

test_that("reviewed V42.7 collision remains unresolved", {
  adjudication <- .load_eicu_icd_adjudication()
  row <- adjudication[adjudication$raw_code == "V42.7", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$review_status, "ambiguous")
  expect_true(is.na(row$selected_version))
})

test_that("manifest-declared unique keys are enforced", {
  entry <- .load_clinical_asset_manifest()$assets[[1L]]
  path <- tempfile(fileext = ".csv")
  entry$filename <- basename(path)
  entry$sha256 <- ""
  entry$required_columns <- c("icd_code", "icd_version")
  entry$required_nonmissing <- c("icd_code", "icd_version")
  entry$key <- c("icd_code", "icd_version")
  entry$key_unique <- TRUE
  manifest <- list(assets = list(entry))
  duplicate <- data.frame(icd_code = c("A", "A"),
                          icd_version = c("10", "10"))
  expect_error(.validate_clinical_asset(
    duplicate, path, c("icd_code", "icd_version"), manifest = manifest),
    "unique key")
})

test_that("packaged procedure lookup filters only declared missing keys", {
  path <- ehr_config_path("scid_procedure_classification_rendehr.csv")
  before_hash <- .clinical_sha256(path)
  raw <- readr::read_csv(path, show_col_types = FALSE)
  manifest <- .load_clinical_asset_manifest()
  entry <- .clinical_manifest_entry(
    manifest, path, asset_id = "scid_procedure_classification")

  expect_identical(tolower(before_hash), tolower(entry$sha256))
  expect_identical(
    as.integer(entry$known_missing_counts$icd_code), 173L)
  expect_identical(sum(.clinical_missing_value(raw$icd_code)), 173L)

  # collapse = FALSE isolates the missing-key filtering from the per-code
  # reduction, which is exercised separately below.
  lookup <- .load_icds_proc(path, collapse = FALSE)
  expect_equal(nrow(lookup), nrow(raw) - 173L)
  expect_false(any(.clinical_missing_value(lookup$icd_code)))
  expect_false(any(.clinical_missing_value(lookup$icd_version)))
  expect_identical(.clinical_sha256(path), before_hash)
})

test_that("the procedure lookup collapses to one row per ICD code and version", {
  path <- ehr_config_path("scid_procedure_classification_rendehr.csv")
  raw <- .load_icds_proc(path, collapse = FALSE)
  lookup <- .load_icds_proc(path)

  # The packaged asset is a code-pattern expansion whose manifest declares
  # key_unique = false; joining on it unreduced fans out the procedure table
  # (measured 4.6x on mimic_iii), so the runtime lookup must be keyed.
  expect_gt(nrow(raw), nrow(lookup))
  expect_equal(nrow(lookup),
               nrow(dplyr::distinct(lookup, icd_code, icd_version)))

  # ICD-10 is already 1:1 in the asset; ICD-9 is a back-mapping from ICD-10-PCS
  # and is where every ambiguity lives.
  expect_true(all(lookup$icd_version[lookup$is_ambiguous] == "9"))
  expect_equal(sum(lookup$is_ambiguous), 4L)
  expect_equal(sum(lookup$is_multi_body_system), 47L)

  # An ambiguous code is left unclassified rather than resolved by sort order,
  # so it can never be counted as Operative downstream.
  expect_true(all(is.na(lookup$treatment_category[lookup$is_ambiguous])))
  expect_setequal(lookup$icd_code[lookup$is_ambiguous],
                  c("9716", "9729", "9739", "9787"))
})

test_that("manifest-declared known missing counts are enforced exactly", {
  path <- ehr_config_path("scid_procedure_classification_rendehr.csv")
  raw <- as.data.frame(readr::read_csv(path, show_col_types = FALSE))
  entry <- .clinical_manifest_entry(
    .load_clinical_asset_manifest(), path,
    asset_id = "scid_procedure_classification")
  entry$known_missing_counts$icd_code <- 172L
  manifest <- list(assets = list(entry))

  expect_error(
    .validate_clinical_asset(
      raw, path,
      c("icd_code", "icd_version", "treatment_category",
        "body_system_section"),
      asset_id = "scid_procedure_classification", manifest = manifest),
    "known-missing count"
  )
})

test_that("procedure lookup overrides fail closed on missing keys", {
  path <- tempfile(fileext = ".csv")
  override <- data.frame(
    icd_code = NA_character_,
    icd_version = "9",
    icd_title = "unkeyed snapshot row",
    treatment_category = "Nonoperative",
    body_system_section = "Test",
    stringsAsFactors = FALSE
  )
  utils::write.csv(override, path, row.names = FALSE, na = "")

  expect_error(
    suppressWarnings(.load_icds_proc(path)),
    "overrides require nonmissing"
  )
})

test_that("an all-digit ICD list keeps its leading zeros", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("icd_code,icd_version", "0331,9", "8051,9", "0389,9"), f)
  df <- .load_icd_filter(f)
  expect_identical(df$icd_code, c("0331", "8051", "0389"))
})

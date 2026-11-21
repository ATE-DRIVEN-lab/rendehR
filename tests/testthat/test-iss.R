.s006_fake_icdpicr2 <- function(all_missing = FALSE, value = 4) {
  value <- if (all_missing) NA_real_ else value
  list(
    cat_trauma2 = function(df, dx_pre, messages) data.frame(
      hadm_id = df$hadm_id, maxais = value, riss = value,
      niss = value + 1, PmortTQP = 0.1, PmortNIS = 0.2,
      mech_1 = "fall", intent_1 = "unintentional"),
    iciss = function(df, dx_pre, messages) data.frame(
      hadm_id = df$hadm_id, PS_int_prod = .9, PS_int_min = .8,
      PS_TQP_prod = .7, PS_TQP_min = .6, PS_NIS_prod = .5,
      PS_NIS_min = .4),
    framework = function(df, dx_pre, messages) data.frame(
      hadm_id = df$hadm_id, cell_1 = "head", cell_2 = "torso",
      PsCell_1 = .8, PsCell_2 = .6)
  )
}

test_that("ICD-10 ISS adapter maps the icdpicr2 2.1 fields", {
  diag <- data.frame(hadm_id = 1L, icd_code = "S14109A",
                     icd_version = "10")
  got <- .iss_score_icd10(diag, "hadm_id", .s006_fake_icdpicr2())
  expect_identical(got$iss_i10, 4)
  expect_identical(got$iciss_mult_intl, .9)
  expect_identical(got$frame_cell, "head | torso")
  expect_identical(got$frame_survival, .6)
  expect_identical(got$mechmaj10, "fall")
})

test_that("ICD-10 ISS adapter refuses all-missing severity output", {
  diag <- data.frame(hadm_id = 1L, icd_code = "S14109A",
                     icd_version = "10")
  expect_error(
    .iss_score_icd10(diag, "hadm_id", .s006_fake_icdpicr2(TRUE)),
    "all-missing")
})

test_that("empty ISS output has the fixed documented schema", {
  got <- .run_iss_all(data.frame(hadm_id = integer(), icd_code = character(),
                                 icd_version = character()), "hadm_id")
  expect_named(got, c("hadm_id", .iss_metric_columns()))
  expect_type(got$hadm_id, "integer")
})

test_that("ISS fixed-schema template preserves nonempty typed IDs", {
  ids <- c(9L, 4L)
  got <- .empty_iss_scores("hadm_id", ids)

  expect_named(got, c("hadm_id", .iss_metric_columns()))
  expect_identical(got$hadm_id, ids)
  expect_identical(nrow(got), length(ids))
  expect_true(all(vapply(got[.iss_metric_columns()],
                         function(value) all(is.na(value)), logical(1L))))
})

test_that("the reported score is the max when both versions score, else the one present", {
  # Both present -> higher wins, regardless of which version supplied it.
  # Only one present -> that one. Neither -> NA, never -Inf.
  expect_identical(
    .iss_pmax(c(4, 16, 12, NA, 0), c(25, 9, NA, NA, 0)),
    c(25, 16, 12, NA, 0)
  )
})

test_that("a mixed-version visit reports the higher of its two scorings", {
  skip_if_not_installed("icdpicr")

  # "80500" (closed cervical vertebral fracture) scores 4 under the real
  # icdpicr. Visit 1 is mixed and the stub scores ICD-10 at 9, so the ICD-10
  # value must win; visit 2 is ICD-10 only and must report the stub's value
  # even though no ICD-9 scoring exists for it.
  diag <- data.frame(
    hadm_id     = c(1L, 1L, 2L),
    icd_code    = c("80500", "S14109A", "S14109A"),
    icd_version = c("9", "10", "10"),
    stringsAsFactors = FALSE
  )
  got <- .run_iss_all(diag, "hadm_id", .s006_fake_icdpicr2(value = 9))

  mixed <- got[got$hadm_id == 1L, ]
  i10_only <- got[got$hadm_id == 2L, ]

  expect_identical(mixed$iss_icd9, 4)
  expect_identical(mixed$iss_i10, 9)
  expect_identical(mixed$iss, 9)          # max of the two, not a version choice

  expect_true(is.na(i10_only$iss_icd9))
  expect_identical(i10_only$iss, i10_only$iss_i10)
})

test_that("combined maxais is the highest known AIS; AIS 9 never wins", {
  # AIS 9 = injury present, severity not specified: a flag, not a severity.
  icd9 <- c(9, 3, 9, 9, 0, NA, 4, 6, 0, 9)
  i10  <- c(3, 9, 0, NA, NA, NA, 5, 9, 0, 9)
  expect_identical(.iss_max_known_ais(icd9, i10),
                   c(3, 3, NA, NA, 0, NA, 5, 6, 0, NA))
  expect_identical(
    .iss_severity_unknown(.iss_max_known_ais(icd9, i10), icd9, i10),
    c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("unknown-severity injuries get severity_unknown, not a score or no_mapped_condition", {
  # Visit 1: ICD-9 unknown, ICD-10 real AIS 3 -> the known AIS is reported.
  # Visit 2: ICD-9 unknown, ICD-10 no mapped injury -> severity_unknown.
  # Visit 3: ICD-9 only, unknown -> severity_unknown.
  # Visit 4: nothing maps in either version -> no_mapped_condition, scores 0.
  universe <- data.frame(hadm_id = 1:4)
  diag <- data.frame(hadm_id = c(1L, 1L, 2L, 2L, 3L, 4L),
                     icd_code = c("9599", "S0990XA", "9599", "Z000", "9599", "4019"),
                     icd_version = c("9", "10", "9", "10", "9", "9"))
  local_mocked_bindings(.run_iss_all = function(diag, id_col, ...) {
    out <- .empty_iss_scores("hadm_id", 1:4)
    out$maxais_icd9 <- c(9, 9, 9, 0)
    out$maxais_i10  <- c(3, 0, NA, NA)
    out$iss_icd9    <- c(NA, NA, NA, 0)
    out$iss_i10     <- c(9, 0, NA, NA)
    out$iss  <- .iss_pmax(out$iss_icd9, out$iss_i10)
    out$niss <- out$iss
    out$maxais <- .iss_max_known_ais(out$maxais_icd9, out$maxais_i10)
    out
  })
  got <- .finalize_iss(universe, diag, "hadm_id")
  got <- got[order(got$hadm_id), ]

  expect_identical(got$iss_status, c("scored", "severity_unknown",
                                     "severity_unknown", "no_mapped_condition"))
  expect_identical(got$maxais, c(3, NA, NA, 0))
  expect_identical(got$iss, c(9, NA, NA, 0))
  expect_identical(got$niss, c(9, NA, NA, 0))
  # The scorers' raw per-version values, including 9, stay visible.
  expect_identical(got$maxais_icd9, c(9, 9, 9, 0))
})

# The injury_mechanism flat concept harmonises two label vocabularies that
# icdpicr and icdpicr2 do not share. That crosswalk is derived from the
# scorers' complete mapping tables, NOT from labels observed in a cohort - a
# cohort only exposes the categories it happens to contain. These tests pin the
# full vocabularies so a dependency upgrade that adds or renames a label fails
# here, instead of silently reintroducing duplicate categories downstream.

.s006_mech_vocab <- function() {
  icd9 <- sort(unique(stats::na.omit(
    as.data.frame(get("etab_s1", asNamespace("icdpicr")))$mechmaj)))
  env <- new.env()
  utils::data("i10_map_mech", package = "icdpicr2", envir = env)
  i10 <- sort(unique(stats::na.omit(as.data.frame(env$i10_map_mech)$mechmaj)))
  list(icd9 = as.character(icd9), i10 = as.character(i10))
}

test_that("the scorer mechanism vocabularies are unchanged", {
  skip_if_not_installed("icdpicr")
  skip_if_not_installed("icdpicr2")
  v <- .s006_mech_vocab()

  # 19 ICD-9 labels, 30 ICD-10 labels, 8 shared exactly. If this fails, the
  # crosswalk in inst/sql/*/concepts_sqls/flat/*_injury_mechanism.sql must be
  # revisited before the new label reaches a concept table.
  expect_length(v$icd9, 19L)
  expect_length(v$i10, 30L)
  expect_setequal(
    intersect(v$icd9, v$i10),
    c("Fall", "Firearm", "Machinery", "Overexertion", "Pedal cyclist, other",
      "Pedestrian, other", "Suffocation", "Unspecified")
  )
})

test_that("every mechanism label is either harmonised or a known passthrough", {
  skip_if_not_installed("icdpicr")
  skip_if_not_installed("icdpicr2")
  path <- system.file("sql/mimic_iv/concepts_sqls/flat/injury_mechanism.sql",
                      package = "rendehR")
  skip_if(!nzchar(path))

  sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # Quoted literals only - 'Other Specified' must not match inside
  # 'Other Specified, NEC'.
  literals <- unlist(regmatches(sql, gregexpr("'[^']*'", sql)))
  literals <- gsub("^'|'$", "", literals)

  # Labels deliberately left to the ELSE branch: they have no counterpart in
  # the other coding system, so there is nothing to merge them with.
  passthrough <- c(
    "Fall", "Firearm", "Machinery", "Overexertion", "Pedal cyclist, other",
    "Pedestrian, other", "Suffocation", "Unspecified",
    "Adverse effects", "Poisoning",
    "Bites and Stings, nonvenomous", "Bites and Stings, venomous",
    "Hot Object/Substance", "Other Land Transport", "Other Specified",
    "Other Specified, Child/Adult Abuse", "Other Specified, Foreign Body"
  )

  v <- .s006_mech_vocab()
  unaccounted <- setdiff(union(v$icd9, v$i10), union(literals, passthrough))
  expect_identical(unaccounted, character(0))
})

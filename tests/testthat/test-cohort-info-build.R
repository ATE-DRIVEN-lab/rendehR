test_that("Clinical summaries are deterministic", {
  x <- c(" B ", "A", "B", NA_character_, "")
  expect_identical(.collapse_sorted_values(x), "A | B")
  expect_identical(.collapse_sorted_values(rev(x)), "A | B")
})

test_that("SCI summary reports coded region and level, dropping placeholders", {
  diag_exp <- data.frame(
    hadm_id = c(1L, 1L, 2L), icd_scid_flg = 1L,
    icd_code = c("80600", "7230", "S14105A"), long_title = "t",
    is_sci = 1L, is_tr = c(1L, 0L, 1L),
    nontrauma_l1 = "not_applicable", nontrauma_l2 = "not_applicable",
    anatomical_region = c("cervical", "unspecified", "cervical_thoracic"),
    specific_level = c("c1_c4", "unspecified", "c5"),
    stringsAsFactors = FALSE)
  got <- .sci_cat_summ(diag_exp, "hadm_id")
  expect_identical(got$scid_injury_level, c("cervical", "cervical_thoracic"))
  expect_identical(got$scid_injury_location, c("c1_c4", "c5"))

  # A dictionary without the coded location columns yields NA, not an error.
  got <- .sci_cat_summ(diag_exp[, setdiff(names(diag_exp),
                                          c("anatomical_region", "specific_level"))],
                       "hadm_id")
  expect_true(all(is.na(got$scid_injury_level)))
  expect_true(all(is.na(got$scid_injury_location)))
})

test_that("SCI nontrauma summary uses snake_case values and placeholder", {
  diag_exp <- data.frame(
    hadm_id = c(1L, 1L, 2L), icd_scid_flg = 1L,
    icd_code = c("3361", "7230", "80600"), long_title = "t",
    is_sci = c(1L, 0L, 1L), is_tr = c(0L, 0L, 1L),
    nontrauma_l1 = c("acquired_abnormalities", "not_applicable", "not_applicable"),
    nontrauma_l2 = c("vascular_disorders", "not_applicable", "not_applicable"),
    stringsAsFactors = FALSE)
  got <- .sci_cat_summ(diag_exp, "hadm_id")
  expect_identical(got$second_level, c(.sci_cat_labels$sci_non_traumatic,
                                       .sci_cat_labels$sci_traumatic))
  expect_identical(got$scid_nontrauma_l1, c("acquired_abnormalities", "not_applicable"))
  expect_identical(got$scid_nontrauma_l2, c("vascular_disorders", "not_applicable"))
})

test_that("SCI categories are decided by the SCI codes of the stay", {
  # One row per (stay, code): is_sci 1/3/999 = SCI, 2 = nerve root, 0 = control.
  diag_exp <- data.frame(
    hadm_id = c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 6L, 6L, 7L, 8L, 9L, 9L),
    icd_scid_flg = 1L, long_title = "t",
    icd_code = c("1983", "3361", "8052", "3441", "3361", "S1410XA", "S1410XA",
                 "3532", "8052", "8052", "7230", "1983", "3532"),
    is_sci = c(999L, 1L, 0L, 3L, 1L, 1L, 1L, 2L, 0L, 0L, 0L, 999L, 2L),
    is_tr  = c(0L, 0L, 1L, 0L, 0L, 2L, 2L, 0L, 1L, 1L, 0L, 0L, 0L),
    nontrauma_l1 = c("acquired_abnormalities", "congenital", "trauma_only",
                     "not_applicable", "genetic_disorders", "not_applicable",
                     "not_applicable", "not_applicable", "not_applicable",
                     "not_applicable", "acquired_abnormalities",
                     "acquired_abnormalities", "not_applicable"),
    nontrauma_l2 = "not_applicable",
    stringsAsFactors = FALSE)
  got <- .sci_cat_summ(diag_exp, "hadm_id")
  lab <- .sci_cat_labels

  expect_identical(got$top_level, c(
    lab$top_sci, lab$top_sci, lab$top_sci, lab$top_sci, lab$top_sci,
    lab$top_nerve_root, lab$top_control, lab$top_control, lab$top_sci))
  expect_identical(got$second_level, c(
    lab$sci_non_traumatic,   # 1: code 999 alone, non-traumatic
    lab$sci_non_traumatic,   # 2: trauma from a control code does not count
    lab$sci_non_traumatic,   # 3: code 3 alone
    lab$sci_non_traumatic,   # 4: non-traumatic + unspecified SCI codes
    lab$sci_unspecified,     # 5: only unspecified-trauma SCI codes
    lab$nerve_root,          # 6: nerve root + traumatic control
    lab$control_trauma,      # 7
    lab$control_non_trauma,  # 8
    lab$sci_non_traumatic))  # 9: SCI wins over a nerve-root code
  expect_false(anyNA(got$second_level))
  expect_identical(got$scid_trauma_types, c(
    "non_traumatic", "non_traumatic", "non_traumatic",
    "non_traumatic|unspecified", "unspecified", NA, NA, NA, "non_traumatic"))
  # Etiology comes from the SCI codes only, never from control codes.
  expect_identical(got$scid_nontrauma_l1, c(
    "acquired_abnormalities", "congenital", "not_applicable",
    "genetic_disorders", "not_applicable", "not_applicable",
    "not_applicable", "not_applicable", "acquired_abnormalities"))
})

test_that("Any traumatic SCI code makes the stay traumatic SCI", {
  diag_exp <- data.frame(
    hadm_id = 1L, icd_scid_flg = 1L, long_title = "t",
    icd_code = c("3361", "80600"), is_sci = 1L, is_tr = c(0L, 1L),
    nontrauma_l1 = c("congenital", "not_applicable"),
    nontrauma_l2 = "not_applicable", stringsAsFactors = FALSE)
  got <- .sci_cat_summ(diag_exp, "hadm_id")
  expect_identical(got$second_level, .sci_cat_labels$sci_traumatic)
  expect_identical(got$scid_trauma_types, "non_traumatic|traumatic")
  expect_identical(got$scid_nontrauma_l1, "not_applicable")
})

test_that("eICU ICD resolution preserves raw tokens and official ambiguity", {
  local_mocked_bindings(
    get_table_from_db = function(con, schema, table) {
      if (identical(table, "diagnosis")) {
        data.frame(patientunitstayid = c(11, 12),
                   icd9code = c("250.00, V42.7, A01.0", "25000"))
      } else {
        data.frame(patientunitstayid = c(11, 12),
                   patienthealthsystemstayid = c(101, 102))
      }
    }
  )
  dict <- data.frame(
    icd_code = c("25000", "V427", "V427", "A010"),
    icd_version = c("9", "9", "10", "10"),
    long_title = c("diabetes", "transplant 9", "transplant 10", "typhoid")
  )
  dict <- .normalize_icd_asset(dict)
  adjudication <- data.frame(
    raw_code = "V42.7", compact_code = "V427", selected_version = NA_character_,
    review_status = "ambiguous"
  )

  got <- .eicu_diag_versioned(NULL, "raw", dict, adjudication)
  exact <- got[got$icd_code_raw == "250.00", ]
  compact <- got[got$icd_code_raw == "25000", ]
  collision <- got[got$icd_code_raw == "V42.7", ]

  expect_identical(exact$version_resolution_status, "exact_dotted")
  expect_identical(exact$icd_version, "9")
  expect_identical(compact$version_resolution_status, "compact_unique")
  expect_identical(collision$version_resolution_status, "ambiguous")
  expect_true(is.na(collision$icd_version))
  expect_true("regex_version_suggestion" %in% names(got))
})

test_that("eICU dotted tokens never resolve through compact fallback", {
  # 31.1 is ICD-9 procedure "temporary tracheostomy"; its compact form 311 is
  # ICD-9 diagnosis "depressive disorder". X99.9 stands in for any dotted
  # token that is not an official diagnosis but has an approved adjudication.
  local_mocked_bindings(
    get_table_from_db = function(con, schema, table) {
      if (identical(table, "diagnosis")) {
        data.frame(patientunitstayid = c(11, 12, 13, 14),
                   icd9code = c("31.1", "311", "V42.7", "X99.9"))
      } else {
        data.frame(patientunitstayid = c(11, 12, 13, 14),
                   patienthealthsystemstayid = c(101, 102, 103, 104))
      }
    }
  )
  dict <- .normalize_icd_asset(data.frame(
    icd_code = c("311", "V427", "V427"),
    icd_version = c("9", "9", "10"),
    long_title = c("depressive disorder", "transplant 9", "transplant 10")))
  adjudication <- data.frame(
    raw_code = c("V42.7", "X99.9"), compact_code = c("V427", "X999"),
    selected_version = c(NA_character_, "10"),
    review_status = c("ambiguous", "approved"))

  got <- .eicu_diag_versioned(NULL, "raw", dict, adjudication)
  by_raw <- function(x) got[got$icd_code_raw == x, ]

  expect_identical(by_raw("31.1")$version_resolution_status, "unmatched")
  expect_true(is.na(by_raw("31.1")$icd_version))
  expect_true(is.na(by_raw("31.1")$long_title))

  # Genuine 311 has no decimal part, so its official dotted form is "311".
  expect_identical(by_raw("311")$version_resolution_status, "exact_dotted")
  expect_identical(by_raw("311")$icd_version, "9")
  expect_identical(by_raw("311")$long_title, "depressive disorder")

  expect_identical(by_raw("V42.7")$version_resolution_status, "ambiguous")
  expect_true(is.na(by_raw("V42.7")$icd_version))

  expect_identical(by_raw("X99.9")$version_resolution_status,
                   "expert_adjudicated")
  expect_identical(by_raw("X99.9")$icd_version, "10")
})

test_that("eICU token resolver returns one row per distinct token", {
  dict <- .normalize_icd_asset(data.frame(
    icd_code = c("4019", "I10", "V427", "V427"),
    icd_version = c("9", "10", "9", "10"),
    long_title = c("hypertension 9", "hypertension 10", "tx 9", "tx 10")))
  tokens <- c("401.9", " 401.9 ", "i10", "I10", NA, "", "V42.7", "31.1", "4-01")

  got <- .eicu_resolve_icd_tokens(tokens, dict)

  expect_identical(names(got), c("icd_code_raw", "icd_code_compact",
                                 "icd_version", "long_title",
                                 "version_resolution_status"))
  # Whitespace and case variants collapse to one normalized token; NA and ""
  # both normalize to NA.
  expect_identical(sort(got$icd_code_raw, na.last = TRUE),
                   c("31.1", "4-01", "401.9", "I10", "V42.7", NA))
  status <- stats::setNames(got$version_resolution_status, got$icd_code_raw)
  expect_identical(unname(status[c("401.9", "I10", "V42.7", "31.1", "4-01")]),
                   c("exact_dotted", "exact_dotted", "ambiguous", "unmatched",
                     "malformed"))
  expect_identical(got$version_resolution_status[is.na(got$icd_code_raw)],
                   "empty")
  expect_identical(got$icd_version[got$icd_code_raw %in% "I10"], "10")
})

test_that("eICU token resolver fails clearly on a missing or unusable dictionary", {
  expect_error(.eicu_resolve_icd_tokens("401.9", NULL),
               "requires the diagnosis dictionary")
  expect_error(
    .eicu_resolve_icd_tokens("401.9", data.frame(icd_code = "4019",
                                                 icd_version = "9")),
    "missing column\\(s\\): long_title")
  expect_error(
    .eicu_resolve_icd_tokens("401.9", data.frame(
      icd_code = "4019", icd_version = "11", long_title = "x")),
    "no usable rows")
  expect_error(
    .eicu_resolve_icd_tokens("401.9", data.frame(
      icd_code = character(), icd_version = character(),
      long_title = character())),
    "no usable rows")
})

test_that("eICU diagnosis parsing keeps every diagnosis row", {
  # Stay 11 repeats the same token across and within rows; the output must
  # carry one row per token occurrence, all resolved identically.
  local_mocked_bindings(
    get_table_from_db = function(con, schema, table) {
      if (identical(table, "diagnosis")) {
        data.frame(patientunitstayid = c(11, 11, 12),
                   icd9code = c("401.9, 401.9", "401.9, I10", ""))
      } else {
        data.frame(patientunitstayid = c(11, 12),
                   patienthealthsystemstayid = c(101, 102))
      }
    }
  )
  dict <- .normalize_icd_asset(data.frame(
    icd_code = c("4019", "I10"), icd_version = c("9", "10"),
    long_title = c("hypertension 9", "hypertension 10")))

  got <- .eicu_diag_versioned(NULL, "raw", dict)

  expect_identical(nrow(got), 5L)
  expect_identical(sum(got$icd_code_raw %in% "401.9"), 3L)
  expect_identical(unique(got$icd_version[got$icd_code_raw %in% "401.9"]), "9")
  expect_identical(got$version_resolution_status[got$patientunitstayid == 12],
                   "empty")
  expect_error(.eicu_diag_versioned(NULL, "raw", NULL),
               "requires the diagnosis dictionary")
})

test_that("eICU hospitalization sequence preserves dense year ties", {
  patient <- data.frame(
    uniquepid = c("p", "p", "p"),
    patienthealthsystemstayid = c(1, 2, 3),
    patientunitstayid = c(11, 12, 13),
    hospitaldischargeyear = c(2019, 2019, 2020),
    hospitaldischargestatus = c("Alive", "Expired", "Alive")
  )
  local_mocked_bindings(
    get_table_from_db = function(con, schema, table) {
      if (identical(table, "patient")) patient else
        data.frame(patientunitstayid = integer(), icd9code = character())
    }
  )
  # The resolver requires a usable dictionary; one row is enough here, since
  # this test is about the hospitalization sequence, not diagnoses.
  min_dict <- .normalize_icd_asset(data.frame(
    icd_code = "4019", icd_version = "9", long_title = "hypertension"))
  got <- .build_cohort_info_eicu(
    NULL, "raw", "eicu_demo", min_dict,
    data.frame(raw_code = character(), compact_code = character(),
               selected_version = character(), review_status = character()),
    NULL, list(), "admission")
  expect_identical(got$seq_hos, c(1L, 1L, 2L))
})

test_that("hospitalization without procedure ICD retains missing classification", {
  local_mocked_bindings(
    get_table_from_db = function(con, schema, table) {
      expect_identical(table, "procedures_icd")
      data.frame(hadm_id = 1L, icd9_code = "0123")
    }
  )

  diag_exp <- data.frame(
    hadm_id = c(1L, 2L),
    icd_code = c("8060", "8060"),
    stringsAsFactors = FALSE
  )
  icds_scid <- data.frame(
    icd_code = "8060", icd_version = "9", is_sci = 1L, is_tr = 1L,
    anatomical_region = "cervical", specific_level = "c1",
    nontrauma_l1 = NA_character_, nontrauma_l2 = NA_character_,
    stringsAsFactors = FALSE
  )
  icds_proc <- data.frame(
    icd_code = "0123", icd_version = "9",
    treatment_category = "Operative",
    body_system_section = "Operations on the nervous system",
    stringsAsFactors = FALSE
  )

  got <- .sci_enrich_mimic_iii(
    diag_exp, NULL, "raw", "hadm_id", icds_scid, icds_proc)

  expect_setequal(got$hadm_id, c(1L, 2L))
  no_procedure <- got[got$hadm_id == 2L, ]
  expect_true(is.na(no_procedure$proc_treatment_categories))
  expect_true(is.na(no_procedure$has_operative_proc))
  expect_false(identical(no_procedure$proc_treatment_categories,
                         "Nonoperative"))
})

# ---------------------------------------------------------------------------
# Procedure block (block D).
#
# The SCID procedure dictionary is a curated SCID-relevant list, not a general
# procedure vocabulary, so most of a source's procedures never match it. These
# tests pin what the resulting columns are allowed to claim.
# ---------------------------------------------------------------------------

# Minimal mimic_iii-shaped inputs: hadm 1 has a procedure, hadm 2 has none.
.proc_test_inputs <- function(proc_codes = "0123") {
  list(
    diag_exp = data.frame(hadm_id = c(1L, 2L), icd_code = c("8060", "8060"),
                          stringsAsFactors = FALSE),
    icds_scid = data.frame(
      icd_code = "8060", icd_version = "9", is_sci = 1L, is_tr = 1L,
      anatomical_region = "cervical", specific_level = "c1",
      nontrauma_l1 = NA_character_, nontrauma_l2 = NA_character_,
      stringsAsFactors = FALSE),
    procedures = data.frame(hadm_id = 1L, icd9_code = proc_codes,
                            stringsAsFactors = FALSE)
  )
}

test_that("MIMIC-III SCI matching ignores ICD-10 codes identical to ICD-9 codes", {
  # MIMIC-III diagnoses carry no version column. V103 is ICD-9 "personal
  # history of breast cancer" but an ICD-10 transport code; an ICD-10 row of
  # the SCI list must not match it.
  diag_exp <- data.frame(hadm_id = c(1L, 1L, 2L),
                         icd_code = c("8060", "V103", "8060"),
                         stringsAsFactors = FALSE)
  icds_scid <- data.frame(
    icd_code = c("8060", "V103"), icd_version = c("9", "10"),
    is_sci = 1L, is_tr = 1L, anatomical_region = "cervical",
    specific_level = "c1", nontrauma_l1 = NA_character_,
    nontrauma_l2 = NA_character_, stringsAsFactors = FALSE)

  got <- .sci_enrich_mimic_iii(diag_exp, NULL, "raw", "hadm_id", icds_scid)
  codes <- stats::setNames(got$scid_icd_codes, got$hadm_id)

  expect_identical(unname(codes[c("1", "2")]), c("8060", "8060"))
  expect_false(any(c("hos_eth_flg", "inj_cats", "cause_icd_codes") %in% names(got)))
})

test_that("an admission whose procedures are all unclassified is not called operative", {
  inp <- .proc_test_inputs(proc_codes = "9999")   # absent from the dictionary
  local_mocked_bindings(
    get_table_from_db = function(con, schema, table) inp$procedures)

  icds_proc <- .collapse_icds_proc(data.frame(
    icd_code = "0123", icd_version = "9", treatment_category = "Operative",
    body_system_section = "Central Nervous System", stringsAsFactors = FALSE))

  got <- .sci_enrich_mimic_iii(inp$diag_exp, NULL, "raw", "hadm_id",
                               inp$icds_scid, icds_proc)
  had <- got[got$hadm_id == 1L, ]

  # It had a procedure record, so this is a real 0 - "no SCID-relevant
  # operation" - not the NA that means "no procedure data at all".
  expect_identical(had$has_operative_proc, 0L)
  expect_identical(had$num_proc_icd_codes, 1L)
  expect_identical(had$num_proc_icd_codes_classified, 0L)
  expect_identical(had$proc_classification_status, "unclassified")
  expect_true(is.na(had$proc_treatment_categories))

  # The admission with no procedure rows at all keeps NA throughout.
  none <- got[got$hadm_id == 2L, ]
  expect_true(is.na(none$has_operative_proc))
  expect_true(is.na(none$proc_classification_status))
})

test_that("a contradictorily classified code is reported ambiguous, never operative", {
  inp <- .proc_test_inputs(proc_codes = "9729")
  local_mocked_bindings(
    get_table_from_db = function(con, schema, table) inp$procedures)

  # 9729 "Other nonoperative replacements" carries both Operative and
  # Nonoperative rows in the packaged asset; the collapse must not pick one.
  icds_proc <- .collapse_icds_proc(data.frame(
    icd_code = c("9729", "9729"), icd_version = c("9", "9"),
    treatment_category = c("Nonoperative", "Operative"),
    body_system_section = c("Placement", "Central Nervous System"),
    stringsAsFactors = FALSE))
  expect_true(icds_proc$is_ambiguous)
  expect_true(is.na(icds_proc$treatment_category))

  got <- .sci_enrich_mimic_iii(inp$diag_exp, NULL, "raw", "hadm_id",
                               inp$icds_scid, icds_proc)
  had <- got[got$hadm_id == 1L, ]

  expect_identical(had$has_operative_proc, 0L)
  expect_identical(had$proc_classification_status, "ambiguous")
  # The code did match the dictionary, so it counts toward the classified total
  # even though its category is unusable.
  expect_identical(had$num_proc_icd_codes_classified, 1L)
  expect_identical(had$proc_body_systems_ambiguous, 1L)
})

test_that("a missing procedures_icd table degrades to not_reported, not an error", {
  inp <- .proc_test_inputs()
  local_mocked_bindings(
    dbIsValid = function(...) TRUE,
    dbExistsTable = function(...) FALSE,
    .package = "DBI")
  local_mocked_bindings(
    get_table_from_db = function(...) stop("must not be read when absent"))

  icds_proc <- .collapse_icds_proc(data.frame(
    icd_code = "0123", icd_version = "9", treatment_category = "Operative",
    body_system_section = "Central Nervous System", stringsAsFactors = FALSE))

  got <- NULL
  expect_message(
    got <- .sci_enrich_mimic_iii(
      inp$diag_exp, structure(list(), class = "fake_con"),
      "raw", "hadm_id", inp$icds_scid, icds_proc),
    "procedures_icd not found")

  expect_true(all(.proc_summ_columns() %in% names(got)))
  expect_true(all(got$proc_classification_status == "not_reported"))
  expect_true(all(is.na(got$has_operative_proc)))
})

test_that("eICU opts out of procedure classification but keeps the column set", {
  diag_exp <- data.frame(
    patienthealthsystemstayid = c(101, 101),
    patientunitstayid = c(11, 12),
    icd_code = c("8060", "8060"), icd_version = c("9", "9"),
    stringsAsFactors = FALSE)
  icds_scid <- .normalize_icd_asset(data.frame(
    icd_code = "8060", icd_version = "9", is_sci = 1L, is_tr = 1L,
    anatomical_region = "cervical", specific_level = "c1",
    nontrauma_l1 = NA_character_, nontrauma_l2 = NA_character_,
    stringsAsFactors = FALSE))

  got <- .sci_enrich_eicu(diag_exp, "patienthealthsystemstayid", icds_scid)

  # eICU has no ICD-coded procedure table, so the block is opted out - but the
  # columns must still exist, or merge_sources() would drop them for MIMIC too.
  expect_true(all(.proc_summ_columns() %in% names(got)))
  expect_true(all(got$proc_classification_status == "not_reported"))
  expect_true(all(is.na(got$has_operative_proc)))
  expect_true(all(is.na(got$proc_icd_codes)))
})

test_that("eICU SCI enrichment matches codes only in the dictionary's version", {
  diag_exp <- data.frame(
    patienthealthsystemstayid = c(201, 201, 202),
    patientunitstayid         = c(21, 22, 23),
    icd_code    = c("8060", "999", "8060"),
    icd_version = c("9", "9", "10"),
    stringsAsFactors = FALSE)
  icds_scid <- .normalize_icd_asset(data.frame(
    icd_code = "8060", icd_version = "9", is_sci = 1L, is_tr = 1L,
    anatomical_region = "cervical", specific_level = "c1",
    nontrauma_l1 = NA_character_, nontrauma_l2 = NA_character_,
    stringsAsFactors = FALSE))

  got <- .sci_enrich_eicu(diag_exp, "patienthealthsystemstayid", icds_scid)

  # Stay 201's ICD-9 8060 matches the dictionary; stay 202's ICD-10 8060 must
  # not - the pair is compared, not the bare code - so only 201 is summarised.
  expect_equal(nrow(got), 1L)
  expect_equal(got$patienthealthsystemstayid, 201)
  expect_identical(got$scid_icd_codes, "8060")
  expect_equal(got$num_scid_icd_codes, 1L)
  expect_identical(got$top_level, .sci_cat_labels$top_sci)
  expect_identical(got$second_level, .sci_cat_labels$sci_traumatic)
})

# Medication exposure definitions (inst/extdata/rendehr_duration_concepts_config.csv).

.med_cfg <- function() utils::read.csv(system.file("extdata", "rendehr_duration_concepts_config.csv", package = "rendehR"),
                                       stringsAsFactors = FALSE)

test_that("MIMIC-III CareVue rows list CareVue inputevents_cv items", {
  # inputevents_cv holds CareVue items (30000-49999); a MetaVision id there
  # matches nothing. Drugs CareVue did not chart as inputs are listed.
  cfg <- .med_cfg()
  cv <- cfg[cfg$source == "mimic_iii" & cfg$table_name == "inputevents_cv", ]
  ids <- lapply(strsplit(cv$match_value, "|", fixed = TRUE), as.integer)
  has_cv <- vapply(ids, function(x) any(x >= 30000 & x < 50000), logical(1))
  no_carevue_input <- c("acetaminophen", "acyclovir", "ampicillin", "azithromycin", "cefazolin", "cefepime", "ceftazidime",
                        "ceftriaxone", "ciprofloxacin", "clindamycin", "eptifibatide", "erythromycin", "gentamicin",
                        "lansoprazole", "levetiracetam", "levofloxacin", "linezolid", "meropenem", "metronidazole",
                        "micafungin", "piperacillin", "ranitidine", "sulfamethoxazole", "vancomycin", "warfarin")
  expect_setequal(sub("_exp$", "", cv$concept_name[!has_cv]), no_carevue_input)
  key <- list(norepinephrine_exp = c(30047, 30120), epinephrine_exp = c(30044, 30119, 30309),
              dopamine_exp = c(30043, 30307), dobutamine_exp = c(30042, 30306), phenylephrine_exp = c(30127, 30128))
  for (k in names(key)) expect_true(all(key[[k]] %in% ids[[which(cv$concept_name == k)]]), info = k)
})

test_that("eICU drug patterns do not match other drugs", {
  cfg <- .med_cfg()
  ei <- unique(cfg[cfg$source == "eicu", c("concept_name", "match_value", "itm_class")])
  pat <- stats::setNames(ei$match_value, ei$concept_name)
  cls <- stats::setNames(ei$itm_class, ei$concept_name)
  # rgx_exp_itm: case-insensitive regex; rgx_itm: exact list of labels
  m <- function(k, x) if (cls[[k]] == "rgx_exp_itm") grepl(pat[[k]], x, ignore.case = TRUE, perl = TRUE)
                      else tolower(trimws(x)) %in% tolower(trimws(strsplit(pat[[k]], "|", fixed = TRUE)[[1]]))
  # names a looser pattern would capture under the wrong concept
  expect_false(any(m("epinephrine_exp", c("Norepinephrine (mcg/min)", "NOREPINEPHRINE 8 MG in 250mL NS", "<<norepinephrine 4 mg/4 mL Inj"))))
  expect_true(all(m("epinephrine_exp", c("Epinephrine (mcg/min)", "EPINEPHrine", "LIDOCAINE-EPINEPHRINE"))))
  expect_false(any(m("phenylephrine_exp", c("TYLENOL", "acetaminophen (TYLENOL) tab 650 mg", "VICKS VAPORUB"))))
  expect_false(any(m("fluconazole_exp", c("1000 ML FLEX CONT : SODIUM CHLORIDE 0.9 % IV SOLN", "150 ML FLEX CONT : LEVOFLOXACIN IN D5W 5 MG/ML IV SOLN"))))
  expect_false(m("micafungin_exp", "MEROPENEM 1 G IV SOLR"))
  expect_true(m("micafungin_exp", "micafungin 100 mg in NS 100 mL"))
  expect_false(m("nitroglycerin_exp", "insulin aspart Corrective dosing"))
  expect_true(m("nitroglycerin_exp", "NITROSTAT"))
  expect_false(any(m("thiamine_exp", c("VITAMIN B-12", "VITAMIN B-6", "VITAMIN B12-FOLIC ACID"))))
  expect_true(all(m("thiamine_exp", c("VITAMIN B-1", "VITAMIN B COMPLEX", "thiamine 100 mg"))))
  expect_false(m("lepirudin_exp", "glucagon (human recombinant) 1 mg/ml solution for injection"))
  expect_false(any(m("lidocaine_exp", c("OS-CAL 500+D3", "CENTRUM COMPLETE", "ASPIRIN, CHILDREN'S", "JEVITY 1.5 CAL"))))
  expect_false(m("tramadol_exp", "TRACE ELEMENTS"))
  expect_false(m("ranitidine_exp", "MYLANTA"))
  expect_false(m("lansoprazole_exp", "LANCET DEVICE"))
  expect_false(m("methylprednisolone_exp", "DIANEAL PD"))
  expect_false(m("ciprofloxacin_exp", "TUSSIN CF"))
  # a drug's own name is matched by its pattern and by no other single-drug pattern;
  # antibiotics_exp is a drug class, so it matches its members by design
  combo_ok <- c("dextrose_exp", "nacl_exp", "antibiotics_exp")
  for (k in names(pat)) {
    g <- gsub("_", " ", sub("_exp$", "", k))
    if (k %in% c("kcl_exp", "nacl_exp", "glycopyrronium_exp", "sulfamethoxazole_exp", "antibiotics_exp") ||
        cls[[k]] == "rgx_itm") next
    expect_true(m(k, g), info = k)
    others <- setdiff(names(pat), c(k, combo_ok))
    hit <- others[vapply(others, function(o) m(o, g), logical(1))]
    expect_length(hit, 0)
    if (length(hit)) message(k, " matched by ", paste(hit, collapse = ", "))
  }
})

test_that("aspirin and acetaminophen are separate exposures", {
  cfg <- .med_cfg()
  # MIMIC has no aspirin input item; 228315 is IV acetaminophen
  expect_false(any(cfg$concept_name == "aspirin_exp" & cfg$source %in% c("mimic_iii", "mimic_iv")))
  expect_true(all(cfg$match_value[cfg$concept_name == "acetaminophen_exp" & cfg$source != "eicu"] == "228315"))
  ei <- function(k) unique(cfg$match_value[cfg$concept_name == k & cfg$source == "eicu"])
  asp <- function(x) grepl(ei("aspirin_exp"), x, ignore.case = TRUE, perl = TRUE)
  apap <- function(x) grepl(ei("acetaminophen_exp"), x, ignore.case = TRUE, perl = TRUE)
  expect_false(any(asp(c("ACETAMINOPHEN 325 MG PO TABS", "HYDROCODONE-ACETAMINOPHEN 5-325 MG PO TABS", "IOPAMIDOL", "EXCEDRIN PM"))))
  expect_true(all(asp(c("ASPIRIN 81 MG PO CHEW", "ECOTRIN", "AGGRENOX", "EXCEDRIN EXTRA STRENGTH"))))
  expect_true(all(apap(c("ACETAMINOPHEN 325 MG PO TABS", "TYLENOL", "HYDROCODONE-ACETAMINOPHEN 5-325 MG PO TABS", "OFIRMEV"))))
  expect_false(apap("ASPIRIN 81 MG PO CHEW"))
})

test_that("vasopressin exposures are defined for all three sources", {
  cfg <- .med_cfg()
  v <- cfg[cfg$concept_name == "vasopressin_exp", ]
  expect_setequal(unique(v$source), c("mimic_iii", "mimic_iv", "eicu"))
  expect_true("222315" %in% unlist(strsplit(v$match_value[v$source == "mimic_iv"], "|", fixed = TRUE)))
  expect_true(all(c("30051", "42273", "42802") %in% unlist(strsplit(v$match_value[v$table_name == "inputevents_cv"], "|", fixed = TRUE))))
  p <- unique(v$match_value[v$source == "eicu"])
  expect_true(all(grepl(p, c("Vasopressin (units/min)", "VASOPRESSIN 60 UNITS in 100 mL NS", "PITRESSIN"), ignore.case = TRUE, perl = TRUE)))
  expect_false(grepl(p, "DESMOPRESSIN ACETATE", ignore.case = TRUE, perl = TRUE))
})

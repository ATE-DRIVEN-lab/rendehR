test_that("src_family strips _demo suffix", {
  expect_equal(src_family("mimic_iv_demo"), "mimic_iv")
  expect_equal(src_family("mimic_iii_demo"), "mimic_iii")
  expect_equal(src_family("eicu_demo"), "eicu")
})

test_that("src_family is idempotent on full source names", {
  expect_equal(src_family("mimic_iv"), "mimic_iv")
  expect_equal(src_family("mimic_iii"), "mimic_iii")
  expect_equal(src_family("eicu"), "eicu")
})

test_that("src_family handles materialized subcohort names", {
  expect_equal(src_family("mimic_iv_demo_cervical"), "mimic_iv")
  expect_equal(src_family("mimic_iv_ais_a"),         "mimic_iv")
  expect_equal(src_family("eicu_demo_thoracic"),      "eicu")
  expect_equal(src_family("mimic_iii_demo_complete"), "mimic_iii")
})

test_that("src_family matches the historical regex across a name battery", {
  old <- function(x) sub("^(mimic_iv|mimic_iii|eicu).*", "\\1", x)
  names <- c("mimic_iv", "mimic_iv_demo", "mimic_iv_demo_cervical_a",
             "mimic_iii", "mimic_iii_demo", "eicu", "eicu_demo",
             "amsterdam", "amsterdam_demo", "hirid", "")
  for (n in names) expect_equal(src_family(n), old(n), info = n)
})

test_that("src_family returns unknown sources unchanged", {
  expect_equal(src_family("amsterdam_demo"), "amsterdam_demo")
  expect_equal(src_family("hirid"),          "hirid")
})

test_that("src_family is vectorized and preserves NA", {
  expect_equal(src_family(c("mimic_iv_demo", "eicu", "amsterdam")),
               c("mimic_iv", "eicu", "amsterdam"))
  expect_equal(src_family(NA_character_), NA_character_)
})

test_that("src_family maps release identity and rejects retired SCID sources", {
  expect_equal(src_family("miiv"), "mimic_iv")
  expect_error(src_family("eicu_scid"), "cohort.profile")
  expect_error(src_family("miii_scid"), "cohort.profile")
  expect_error(src_family("miiv_scid"), "cohort.profile")
})

test_that("src_family resolves a newly registered family", {
  old_reg <- .ehr_env$source_registry
  on.exit(.ehr_env$source_registry <- old_reg, add = TRUE)

  .register_source_family("amsterdam", "amsterdam")
  expect_equal(src_family("amsterdam_demo_cervical"), "amsterdam")
  # Existing families still resolve after registration.
  expect_equal(src_family("mimic_iv_demo"), "mimic_iv")
})

test_that("ehr_config_root uses only EHR_EXTERNAL_CONFIG_ROOT", {
  vars <- c("EHR_EXTERNAL_CONFIG_ROOT", "EHR_CONFIG_DIR",
            "SCIEHR_CONFIG_PATH")
  old <- Sys.getenv(vars, unset = NA)
  names(old) <- vars
  on.exit({
    for (v in vars) {
      if (is.na(old[[v]])) Sys.unsetenv(v)
      else do.call(Sys.setenv, as.list(setNames(old[[v]], v)))
    }
  }, add = TRUE)

  Sys.unsetenv(vars)
  expect_null(ehr_config_root())

  d <- tempfile("cfgroot"); dir.create(d)
  Sys.setenv(EHR_EXTERNAL_CONFIG_ROOT = d)
  expect_equal(normalizePath(ehr_config_root()), normalizePath(d))

  Sys.unsetenv("EHR_EXTERNAL_CONFIG_ROOT")
  Sys.setenv(EHR_CONFIG_DIR = d)
  expect_error(ehr_config_root(), "Rename it to EHR_EXTERNAL_CONFIG_ROOT")
  Sys.unsetenv("EHR_CONFIG_DIR")
  Sys.setenv(SCIEHR_CONFIG_PATH = d)
  expect_error(ehr_config_root(), "Rename it to EHR_EXTERNAL_CONFIG_ROOT")

  # A path that does not exist resolves to NULL, not an error.
  Sys.setenv(EHR_EXTERNAL_CONFIG_ROOT = file.path(d, "nope"))
  Sys.unsetenv("SCIEHR_CONFIG_PATH")
  expect_null(ehr_config_root())
})

test_that(".ehr_pkg_or_local_cfg layers env dir between local and package", {
  vars <- c("EHR_EXTERNAL_CONFIG_ROOT", "EHR_CONFIG_DIR",
            "SCIEHR_CONFIG_PATH")
  old <- Sys.getenv(vars, unset = NA)
  names(old) <- vars
  on.exit({
    for (v in vars) {
      if (is.na(old[[v]])) Sys.unsetenv(v)
      else do.call(Sys.setenv, as.list(setNames(old[[v]], v)))
    }
  }, add = TRUE)

  root <- tempfile("envcfg"); dir.create(root)
  writeLines("{}", file.path(root, "hddl-data-sources.json"))
  Sys.unsetenv(vars)
  Sys.setenv(EHR_EXTERNAL_CONFIG_ROOT = root)

  empty_cfgdir <- tempfile("proj"); dir.create(empty_cfgdir)

  # Absent from project config_dir -> resolves to the EHR_CONFIG_DIR copy.
  got <- .ehr_pkg_or_local_cfg("hddl-data-sources.json", empty_cfgdir, tempdir())
  expect_equal(normalizePath(got),
               normalizePath(file.path(root, "hddl-data-sources.json")))

  # Concept CSVs opt out (use_config_root = FALSE): the env copy is NOT used,
  # so the base falls through to the packaged file (merge happens separately).
  writeLines("x", file.path(root, "rendehr_concepts_config.csv"))
  got2 <- .ehr_pkg_or_local_cfg("rendehr_concepts_config.csv", empty_cfgdir,
                                tempdir(), use_config_root = FALSE)
  expect_false(identical(
    normalizePath(got2, mustWork = FALSE),
    normalizePath(file.path(root, "rendehr_concepts_config.csv"))
  ))
})

test_that("ehr_load_config fills example defaults", {
  tmp <- tempfile(fileext = ".json")
  writeLines('{}', tmp)
  cfg <- ehr_load_config(tmp)

  expect_equal(cfg$source$src_name, "mimic_iv_demo")
  expect_equal(cfg$database$db_name, "rendehr")
  expect_equal(cfg$cohort$profile, "generic")
  expect_true(any(grepl("cohort.profile", cfg$defaults_used)))
  unlink(tmp)
})

test_that("ehr_load_config returns all fields from valid JSON", {
  tmp <- tempfile(fileext = ".json")
  writeLines(
    paste0('{"source":{"src_name":"mimic_iv_demo"},',
           '"database":{"db_name":"rendehrdov"},',
           '"project":{"base_dir":"."}}'),
    tmp
  )
  cfg <- ehr_load_config(tmp)
  expect_equal(cfg$source$src_name, "mimic_iv_demo")
  expect_equal(cfg$database$db_name,  "rendehrdov")
  expect_equal(cfg$project$base_dir, ".")
  unlink(tmp)
})

test_that("ehr_load_config errors on non-existent file", {
  expect_error(ehr_load_config("/no/such/file.json"))
})


test_that("ehr_write_config writes loadable project config", {
  tmp <- tempfile(fileext = ".json")
  cfg <- list(
    source = list(src_name = "mimic_iv_demo"),
    database = list(db_name = "rendehrdov"),
    project = list(base_dir = tempdir())
  )

  expect_invisible(ehr_write_config(cfg, tmp))
  loaded <- ehr_load_config(tmp)

  expect_equal(loaded$source$src_name, "mimic_iv_demo")
  expect_equal(loaded$database$db_name, "rendehrdov")
  unlink(tmp)
})

test_that("ehr_create_config_template writes standard roots", {
  tmp <- tempfile(fileext = ".json")
  expect_invisible(ehr_create_config_template(
    path = tmp,
    src_name = "eicu_demo",
    db_name = "ehr_test",
    overwrite = TRUE
  ))

  cfg <- ehr_load_config(tmp)
  expect_equal(cfg$source$src_name, "eicu_demo")
  expect_equal(cfg$database$db_name, "ehr_test")
  expect_equal(cfg$paths$raw_data_root, file.path("data", "source_data"))
  expect_equal(cfg$paths$duckdb_root, file.path("data", "duckdb"))
  unlink(tmp)
})

test_that("ehr_load_config rejects retired flat and path fields", {
  tmp <- tempfile(fileext = ".json")
  writeLines('{"src_name":"mimic_iv_demo"}', tmp)
  expect_error(ehr_load_config(tmp), "source.src_name")

  writeLines(
    '{"source":{"src_name":"mimic_iv_demo"},"paths":{"sql_root":"elsewhere"}}',
    tmp
  )
  expect_error(ehr_load_config(tmp), "paths.sql_root")
})

test_that("ehr_create_config_template defaults to rendehr database", {
  tmp <- tempfile(fileext = ".json")
  ehr_create_config_template(path = tmp, overwrite = TRUE)
  cfg <- ehr_load_config(tmp)

  expect_equal(cfg$database$db_name, "rendehr")
  unlink(tmp)
})

test_that("ehr_check_config returns structured result", {
  tmp <- tempfile(fileext = ".json")
  ehr_create_config_template(path = tmp, overwrite = TRUE)

  res <- ehr_check_config(config_path = tmp, quiet = TRUE)

  expect_true(is.list(res))
  expect_true(res$ok)
  expect_true("paths" %in% names(res))
  expect_true(length(res$warnings) >= 1L)
  unlink(tmp)
})

test_that(".ehr_profile_cfg_path respects profile and explicit override", {
  base <- tempfile("profile")
  dir.create(base)
  override <- file.path(base, "custom.csv")
  writeLines("x", override)

  expect_null(.ehr_profile_cfg_path(
    "scid_icds_rendehr.rds", file.path(base, "config"), base,
    profile = "custom", scid_only = TRUE
  ))
  expect_equal(
    .ehr_profile_cfg_path(
      "unused", file.path(base, "config"), base,
      override = "custom.csv", profile = "custom", scid_only = TRUE
    ),
    override
  )
})

test_that("SCI clinical asset defaults use canonical manifest names", {
  old <- .ehr_env$config
  on.exit(.ehr_env$config <- old, add = TRUE)
  .ehr_env$config <- list(
    source = list(src_name = "mimic_iv_demo"),
    project = list(base_dir = tempdir()),
    cohort = list(profile = "scid"))
  p <- ehr_paths(src_name = "mimic_iv_demo", base_dir = tempdir())
  expect_identical(basename(p$icd_filter_path), "scid_icds_rendehr.rds")
  expect_identical(basename(p$icds_cause_path),
                   "injury_cause_icds_rendehr.csv")
  expect_identical(basename(p$icds_proc_path),
                   "procedure_classification_icds_rendehr.csv")
  expect_identical(basename(p$icd_adjudication_path),
                   "eicu_icd_version_adjudication_rendehr.csv")
})

test_that("MIMIC-IV family release metadata matches the canonical catalog", {
  .register_builtin_sources()
  meta <- .source_entry("mimic_iv_demo")$release_metadata
  expect_equal(meta$mimic_iv, "3.1")
  expect_equal(meta$mimic_iv_demo, "2.2")
  expect_equal(meta$miiv, "2.2")
  catalog <- jsonlite::fromJSON(
    .ehr_extdata("extdata", "rendehr-data-source.json"),
    simplifyVector = FALSE
  )
  releases <- stats::setNames(
    vapply(catalog, function(x) as.character(x$release %||% NA), ""),
    vapply(catalog, function(x) x$name, "")
  )
  for (s in intersect(names(meta), names(releases))) {
    expect_identical(meta[[s]], releases[[s]], info = s)
  }
})

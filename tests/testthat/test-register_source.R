# Snapshot/restore the registry so tests don't leak registrations into others.
.with_clean_registry <- function(code) {
  old <- .ehr_env$source_registry
  on.exit(.ehr_env$source_registry <- old, add = TRUE)
  force(code)
}

.valid_source_spec <- function(name = "schema_ok") {
  list(
    schema_version = 1L,
    name = name,
    select_id_cols = "pid",
    time_origin = "cw.admittime",
    id_cols = list(default = "pid"),
    cohort_key = "pid"
  )
}

test_that("source-family schema is loaded and drives validation", {
  schema <- .load_source_family_schema()
  expect_identical(schema$title, "rendehR source-family definition")
  expect_false(schema$additionalProperties)

  valid <- .valid_source_spec()
  expect_length(.source_schema_problems(valid, schema = schema), 0L)

  schema$required <- c(schema$required, "schema_probe")
  expect_match(
    .source_schema_problems(valid, schema = schema),
    "\\$\\.schema_probe.*required"
  )
})

test_that("schema accepts valid definitions and rejects unknown properties", {
  .with_clean_registry({
    expect_silent(.register_source_spec(.valid_source_spec("schema_valid")))
    expect_true("schema_valid" %in% list_registered_sources())

    before <- .ehr_env$source_registry
    invalid <- .valid_source_spec("schema_unknown")
    invalid$totally_unexpected_typo_field <- "not in the schema"
    expect_error(
      .register_source_spec(invalid),
      "\\$\\.totally_unexpected_typo_field.*additionalProperties"
    )
    expect_equal(.ehr_env$source_registry, before)
  })
})

test_that("schema rejects malformed types and missing required fields atomically", {
  .with_clean_registry({
    before <- .ehr_env$source_registry

    malformed <- .valid_source_spec("schema_type")
    malformed$select_id_cols <- 123
    expect_error(
      .register_source_spec(malformed),
      "\\$\\.select_id_cols.*type.*string"
    )
    expect_equal(.ehr_env$source_registry, before)

    missing <- .valid_source_spec("schema_missing")
    missing$time_origin <- NULL
    expect_error(
      .register_source_spec(missing),
      "\\$\\.time_origin.*required"
    )
    expect_equal(.ehr_env$source_registry, before)

    explicit_null <- .valid_source_spec("schema_null")
    explicit_null["time_origin"] <- list(NULL)
    expect_error(
      .register_source_spec(explicit_null),
      "\\$\\.time_origin.*type.*string"
    )
    expect_equal(.ehr_env$source_registry, before)
  })
})

test_that("schema rejects unknown fields in restricted nested objects", {
  .with_clean_registry({
    before <- .ehr_env$source_registry
    cases <- list(
      id_cols = within(.valid_source_spec("nested_id"), {
        id_cols <- list(default = "pid", typo = "pid")
      }),
      cohort_filter = within(.valid_source_spec("nested_filter"), {
        cohort_filter <- list(
          diag_table = "dx", code_col = "code", key = "pid", typo = TRUE
        )
      }),
      variants = within(.valid_source_spec("nested_variant"), {
        variants <- list(demo = list(row_limit = 10L, typo = TRUE))
      })
    )

    for (field in names(cases)) {
      expect_error(
        .register_source_spec(cases[[field]]),
        paste0("\\$\\.", field, ".*typo.*additionalProperties")
      )
      expect_equal(.ehr_env$source_registry, before)
    }
  })
})

test_that("JSON registration applies schema before registry mutation", {
  .with_clean_registry({
    path <- tempfile(fileext = ".json")
    writeLines(paste0(
      '{"schema_version":1,"name":"json_unknown",',
      '"select_id_cols":["pid"],"time_origin":"cw.t",',
      '"id_cols":{"default":["pid"]},"cohort_key":"pid",',
      '"unknown_json_field":true}'
    ), path)
    before <- .ehr_env$source_registry
    expect_error(
      register_source(config = path),
      "\\$\\.unknown_json_field.*additionalProperties"
    )
    expect_equal(.ehr_env$source_registry, before)
  })
})

test_that("register_source rejects a cohort_fn registry field", {
  .with_clean_registry({
    path <- tempfile(fileext = ".json")
    writeLines(paste0(
      '{"schema_version":1,"name":"json_fn",',
      '"select_id_cols":["pid"],"time_origin":"cw.t",',
      '"id_cols":{"default":["pid"]},"cohort_key":"pid",',
      '"cohort_fn":"json_cohort_builder"}'
    ), path)
    before <- .ehr_env$source_registry
    expect_error(
      register_source(config = path),
      "\\$\\.cohort_fn.*additionalProperties"
    )
    expect_error(
      register_source("inline_fn", select_id_cols = "pid", time_origin = "cw.t",
                      id_cols = list(default = "pid"), cohort_key = "pid",
                      cohort_fn = "json_cohort_builder"),
      "\\$\\.cohort_fn.*additionalProperties"
    )
    expect_equal(.ehr_env$source_registry, before)
  })
})

test_that("register_source (inline) makes a source fully resolvable", {
  .with_clean_registry({
    register_source(
      "abcd",
      select_id_cols     = c("patientid", "admissionid"),
      time_origin        = "cw.admittime",
      time_diff          = "timestamp_minutes",
      id_cols            = list(default = c("patientid", "admissionid"),
                                icu     = c("patientid", "admissionid"),
                                tables_with_icu = c("numericitems")),
      cohort_key         = "admissionid",
      cohort_seed_tables = c("admissions")
    )

    expect_true("abcd" %in% list_registered_sources())
    expect_equal(src_family("abcd_demo_cervical"), "abcd")

    cfg <- get_dataset_config("abcd_demo")
    expect_equal(cfg$select_id_cols, c("patientid", "admissionid"))
    expect_equal(cfg$schema, "abcd_demo_coh")
    expect_equal(cfg$row_limit, 1000L)

    expect_equal(cohort_key("abcd"), "admissionid")
    expect_equal(cohort_seed_tables("abcd"), "admissions")
    expect_equal(get_id_cols("abcd", "numericitems"),
                 c("patientid", "admissionid"))
    # timestamp_minutes -> EXTRACT EPOCH form
    expect_match(build_time_diff("abcd", "charttime", "cw.admittime"),
                 "EXTRACT\\(EPOCH")
  })
})

test_that("register_source validates required fields", {
  .with_clean_registry({
    expect_error(
      register_source("x", time_origin = "cw.t", cohort_key = "id",
                      id_cols = list(default = "id")),
      "select_id_cols.*required"
    )
    expect_error(
      register_source("x", select_id_cols = "id", cohort_key = "id",
                      id_cols = list(default = "id")),
      "time_origin.*required"
    )
    expect_error(
      register_source("x", select_id_cols = "id", time_origin = "cw.t",
                      cohort_key = "id", time_diff = "weird",
                      id_cols = list(default = "id")),
      "time_diff.*enum"
    )
    expect_error(
      register_source("x", select_id_cols = "id", time_origin = "cw.t",
                      cohort_key = "id"),
      "id_cols.*required"
    )
  })
})

test_that("register_source protects built-in families", {
  .with_clean_registry({
    expect_error(
      register_source("mimic_iv", select_id_cols = "subject_id",
                      time_origin = "cw.admittime", cohort_key = "subject_id",
                      id_cols = list(default = "subject_id")),
      "built-in source"
    )
    # overwrite = TRUE allows it.
    expect_silent(
      register_source("mimic_iv", select_id_cols = "subject_id",
                      time_origin = "cw.admittime", cohort_key = "subject_id",
                      id_cols = list(default = "subject_id"), overwrite = TRUE)
    )
  })
})

test_that("register_source reads one versioned family and rejects arrays atomically", {
  .with_clean_registry({
    single <- tempfile(fileext = ".json")
    writeLines(paste0(
      '{"schema_version":1,"name":"abcd","select_id_cols":["patientid"],',
      '"time_origin":"cw.admittime","time_diff":"offset_minutes",',
      '"id_cols":{"default":["patientid"]},"cohort_key":"patientid",',
      '"cohort_seed_tables":["admissions"]}'), single)
    register_source(config = single)
    expect_true("abcd" %in% list_registered_sources())
    # offset_minutes -> addition form
    expect_match(build_time_diff("abcd", "off", "cw.icu_intime"), "\\+")

    arr <- tempfile(fileext = ".json")
    writeLines(paste0(
      '[{"name":"aa","select_id_cols":["id"],"time_origin":"cw.t",',
      '"id_cols":{"default":["id"]},"cohort_key":"id"},',
      '{"name":"bb","select_id_cols":["id"],"time_origin":"cw.t",',
      '"id_cols":{"default":["id"]},"cohort_key":"id"}]'), arr)
    before <- list_registered_sources()
    expect_error(register_source(config = arr), "exactly one source family")
    expect_equal(list_registered_sources(), before)
  })
})

test_that("EHR_EXTERNAL_CONFIG_ROOT sources are auto-registered on discovery", {
  .with_clean_registry({
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

    root <- tempfile("cfgroot"); dir.create(root)
    dir.create(file.path(root, "sources"))
    writeLines(paste0(
      '{"schema_version":1,"name":"dirsrc",',
      '"select_id_cols":["id"],"time_origin":"cw.t",',
      '"id_cols":{"default":["id"]},"cohort_key":"id"}'),
      file.path(root, "sources", "dirsrc.json"))

    Sys.unsetenv(vars)
    Sys.setenv(EHR_EXTERNAL_CONFIG_ROOT = root)
    .register_config_root_sources()

    expect_true("dirsrc" %in% list_registered_sources())
  })
})

# ---------------------------------------------------------------------------
# Declarative cohort_filter on the registry
# ---------------------------------------------------------------------------

test_that("register_source stores cohort_filter on the family", {
  .with_clean_registry({
    register_source(
      "flt", select_id_cols = "pid", time_origin = "cw.t",
      id_cols = list(default = "pid"), cohort_key = "pid",
      cohort_filter = list(diag_table = "dx", code_col = "code", key = "pid"))
    entry <- .ehr_env$source_registry[["flt"]]
    expect_equal(entry$cohort_filter$diag_table, "dx")
    expect_false("cohort_fn" %in% names(entry))
  })
})

test_that("validate_source_config warns when a family has no cohort_filter", {
  .with_clean_registry({
    register_source("nocohort", select_id_cols = "pid", time_origin = "cw.t",
                    id_cols = list(default = "pid"), cohort_key = "pid")
    res <- validate_source_config(name = "nocohort", root = "", quiet = TRUE)
    expect_true(res$ok)
    expect_true(any(grepl("no cohort_filter", res$warnings)))
    eicu <- validate_source_config(name = "eicu", root = "", quiet = TRUE)
    expect_false(any(grepl("no cohort_filter", eicu$warnings)))
  })
})

# ---------------------------------------------------------------------------
# add_source_wizard() + validate_source_config()
# ---------------------------------------------------------------------------

test_that("add_source_wizard scaffolds files and registers a complete spec", {
  .with_clean_registry({
    root <- tempfile("wiz");
    res <- add_source_wizard(
      "wizsrc", root = root,
      select_id_cols = c("pid", "aid"), time_origin = "cw.admittime",
      id_cols = list(default = c("pid", "aid")), cohort_key = "pid",
      cohort_seed_tables = "dx",
      cohort_filter = list(diag_table = "dx", code_col = "code", key = "pid"))

    expect_true(res$registered)
    expect_true("wizsrc" %in% list_registered_sources())
    expect_true(file.exists(file.path(root, "sources", "wizsrc.json")))
    expect_true(file.exists(file.path(root, "sql", "wizsrc",
                                      "db_setup_sqls", "window_tables.sql")))
    expect_true(file.exists(file.path(root, "sql", "wizsrc",
                                      "db_setup_sqls", "schema",
                                      "create_tables.sql")))
    expect_true(dir.exists(file.path(root, "sql", "wizsrc",
                                     "concepts_sqls", "ts")))
    expect_true(file.exists(file.path(root, "rendehr_concepts_config.csv")))

    # The re-read registry entry must resolve like any built-in.
    expect_equal(cohort_key("wizsrc"), "pid")
    expect_equal(src_family("wizsrc_demo"), "wizsrc")
  })
})

test_that("add_source_wizard uses canonical external config root", {
  .with_clean_registry({
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

    root <- file.path(tempdir(), paste0("external-wiz-",
                                        as.integer(runif(1, 1, 1e9))))
    Sys.unsetenv(vars)
    Sys.setenv(EHR_EXTERNAL_CONFIG_ROOT = root)

    res <- add_source_wizard(
      "legacywiz",
      select_id_cols = "pid", time_origin = "cw.t",
      id_cols = list(default = "pid"), cohort_key = "pid",
      cohort_filter = list(diag_table = "dx", code_col = "code", key = "pid"))

    expect_true(res$registered)
    expect_true(file.exists(file.path(root, "sources", "legacywiz.json")))
  })
})

test_that("source validation rejects unsafe routing without mutation", {
  .with_clean_registry({
    before <- .ehr_env$source_registry
    expect_error(
      register_source(
        "unsafe",
        id_prefixes = "",
        select_id_cols = 1,
        time_origin = "cw.t",
        id_cols = list(default = "id"),
        cohort_key = "id",
        variants = list(demo = list(row_limit = Inf))
      ),
      "id_prefixes"
    )
    expect_equal(.ehr_env$source_registry, before)

    expect_error(
      register_source(
        "overlap",
        id_prefixes = "mimic",
        select_id_cols = "id",
        time_origin = "cw.t",
        id_cols = list(default = "id"),
        cohort_key = "id"
      ),
      "collide or overlap"
    )
    expect_equal(.ehr_env$source_registry, before)
  })
})

test_that("canonical source inventory maps miiv and rejects SCID names", {
  expect_equal(src_family("miiv"), "mimic_iv")
  expect_equal(.source_entry("miiv")$release_metadata$miiv, "2.2")
  expect_error(src_family("eicu_scid"), "cohort.profile")
  expect_error(src_family("miii_scid"), "cohort.profile")
  expect_error(src_family("miiv_scid"), "cohort.profile")
})

test_that("add_source_wizard writes a skeleton but skips registration when incomplete", {
  .with_clean_registry({
    root <- tempfile("wiz")
    expect_warning(
      res <- add_source_wizard("partial", root = root),  # missing required fields
      "not registered"
    )
    expect_false(res$registered)
    expect_false("partial" %in% list_registered_sources())
    expect_true(file.exists(file.path(root, "sources", "partial.json")))
    expect_true(length(res$problems) > 0L)
  })
})

test_that("add_source_wizard refuses to clobber an existing registry file", {
  .with_clean_registry({
    root <- tempfile("wiz")
    add_source_wizard("dup", root = root, select_id_cols = "pid",
                      time_origin = "cw.t", id_cols = list(default = "pid"),
                      cohort_key = "pid",
                      cohort_filter = list(diag_table = "dx", code_col = "c",
                                           key = "pid"))
    expect_error(
      add_source_wizard("dup", root = root, select_id_cols = "pid",
                        time_origin = "cw.t", id_cols = list(default = "pid"),
                        cohort_key = "pid"),
      "already exists"
    )
  })
})

test_that("validate_source_config reports all problems for a spec list", {
  res <- validate_source_config(
    config = list(name = "x", time_diff = "weird"), quiet = TRUE)
  expect_false(res$ok)
  expect_true(any(grepl("select_id_cols", res$problems)))
  expect_true(any(grepl("time_origin", res$problems)))
  expect_true(any(grepl("time_diff", res$problems)))
})

test_that("validate_source_config validates a registered family by name", {
  .with_clean_registry({
    register_source("okname", select_id_cols = "pid", time_origin = "cw.t",
                    id_cols = list(default = "pid"), cohort_key = "pid",
                    cohort_filter = list(diag_table = "dx", code_col = "c",
                                         key = "pid"))
    res <- validate_source_config(name = "okname", root = "", quiet = TRUE)
    expect_true(res$ok)
  })
})

# ---------------------------------------------------------------------------
# ehr_add_source_assistant() (non-interactive / scriptable path)
# ---------------------------------------------------------------------------

test_that("ehr_add_source_assistant scaffolds + registers non-interactively", {
  .with_clean_registry({
    root <- tempfile("asst")
    plan <- ehr_add_source_assistant(
      "asstsrc", root = root,
      select_id_cols = c("pid", "aid"), id_default = c("pid", "aid"),
      time_origin = "cw.admittime", time_diff = "timestamp_minutes",
      cohort_key = "pid", cohort_seed_tables = "dx",
      diag_table = "dx", code_col = "code",
      action = "write", interactive = FALSE)

    expect_s3_class(plan, "ehr_add_source_plan")
    expect_true(plan$result$registered)
    expect_true("asstsrc" %in% list_registered_sources())
    expect_true(file.exists(file.path(root, "sources", "asstsrc.json")))
    # filter_key defaulted to cohort_key
    expect_equal(plan$spec$cohort_filter$key, "pid")
    expect_equal(cohort_key("asstsrc"), "pid")
  })
})

test_that("ehr_add_source_assistant 'script' action writes nothing", {
  .with_clean_registry({
    root <- tempfile("asst")
    plan <- ehr_add_source_assistant(
      "scriptsrc", root = root,
      select_id_cols = "pid", id_default = "pid", time_origin = "cw.t",
      time_diff = "timestamp_minutes", cohort_key = "pid",
      diag_table = "dx", code_col = "code",
      action = "script", interactive = FALSE)

    expect_match(plan$call, "add_source_wizard\\(")
    expect_match(plan$call, "cohort_filter = ")
    expect_false(dir.exists(file.path(root, "sources")))
    expect_false("scriptsrc" %in% list_registered_sources())
  })
})

test_that("a non-interactive ehr_add_source_assistant without action only prints the call", {
  .with_clean_registry({
    root <- tempfile("asst")
    plan <- ehr_add_source_assistant(
      "quietsrc", root = root,
      select_id_cols = "pid", id_default = "pid", time_origin = "cw.t",
      time_diff = "timestamp_minutes", cohort_key = "pid",
      diag_table = "dx", code_col = "code", interactive = FALSE)
    expect_identical(plan$action, "script")
    expect_false(dir.exists(file.path(root, "sources")))
    expect_false("quietsrc" %in% list_registered_sources())
  })
})

test_that("ehr_add_source_assistant threads icu_id_type through the call", {
  .with_clean_registry({
    plan <- ehr_add_source_assistant(
      "textid", root = tempfile("asst"),
      select_id_cols = "pid", id_default = "pid", time_origin = "cw.t",
      time_diff = "timestamp_minutes", cohort_key = "pid",
      diag_table = "dx", code_col = "code",
      icu_id_type = "text", action = "script", interactive = FALSE)
    expect_match(plan$call, "icu_id_type = \"text\"")
    expect_equal(plan$spec$icu_id_type, "text")
  })
})

test_that("assistant write path writes the SAME JSON as direct add_source_wizard", {
  .with_clean_registry({
    common <- list(
      select_id_cols = c("pid", "aid"),
      id_cols        = list(default = c("pid", "aid")),
      time_origin    = "cw.admittime",
      time_diff      = "timestamp_minutes",
      cohort_key     = "pid",
      cohort_filter  = list(diag_table = "dx", code_col = "code", key = "pid"),
      id_prefixes    = c("parity", "parity_x"),
      icu_id_type    = "text")

    root_a <- tempfile("direct")
    do.call(add_source_wizard, c(list("parity", root = root_a), common))

    root_b <- tempfile("asst")
    ehr_add_source_assistant(
      "parity", root = root_b,
      select_id_cols = common$select_id_cols, id_default = c("pid", "aid"),
      time_origin = common$time_origin, time_diff = common$time_diff,
      cohort_key = common$cohort_key,
      diag_table = "dx", code_col = "code",
      id_prefixes = common$id_prefixes, icu_id_type = common$icu_id_type,
      action = "write", interactive = FALSE)

    j_a <- jsonlite::fromJSON(file.path(root_a, "sources", "parity.json"),
                              simplifyVector = TRUE, simplifyDataFrame = FALSE)
    j_b <- jsonlite::fromJSON(file.path(root_b, "sources", "parity.json"),
                              simplifyVector = TRUE, simplifyDataFrame = FALSE)

    # The advanced fields land identically, and the whole spec matches.
    expect_equal(j_b$icu_id_type, "text")
    expect_equal(j_b$id_prefixes, c("parity", "parity_x"))
    expect_equal(j_a, j_b)
  })
})

test_that("ehr_add_source_assistant 'dry_run' validates without writing", {
  .with_clean_registry({
    root <- tempfile("asst")
    plan <- ehr_add_source_assistant(
      "drysrc", root = root, action = "dry_run", interactive = FALSE)
    # No cohort strategy / ids supplied -> validation reports problems.
    expect_false(plan$result$validation$ok)
    expect_false(dir.exists(root))
  })
})

# print.ehr_add_source_plan(): direct output test.
test_that("print.ehr_add_source_plan renders the source, action, and generated call", {
  .with_clean_registry({
    plan <- ehr_add_source_assistant(
      "printsrc", root = tempfile("asst"),
      select_id_cols = "pid", id_default = "pid", time_origin = "cw.t",
      time_diff = "timestamp_minutes", cohort_key = "pid",
      diag_table = "dx", code_col = "code",
      action = "script", interactive = FALSE)

    expect_output(print(plan), "rendehR Add-Source Assistant Plan")
    expect_output(print(plan), "printsrc")
    expect_output(print(plan), "add_source_wizard\\(")
  })
})

test_that("ehr_add_source_assistant 'dry_run' accepts a complete spec the wizard would register", {
  .with_clean_registry({
    root <- tempfile("asst")
    args <- list(select_id_cols = c("patientid", "admissionid"), id_default = c("patientid", "admissionid"),
                 time_origin = "cw.admittime", time_diff = "timestamp_minutes", cohort_key = "admissionid",
                 diag_table = "diagnoses", code_col = "diagnosis_code",
                 interactive = FALSE)
    dry <- do.call(ehr_add_source_assistant, c(list("drycomplete", root = root, action = "dry_run"), args))
    # the dry run validates the completed spec, not the raw answers (no
    # schema_version, NULL placeholders), so a spec that registers cleanly
    # reports no problems
    expect_true(dry$result$validation$ok, info = paste(dry$result$validation$problems, collapse = "; "))
    expect_false(dir.exists(root))
    wrote <- do.call(ehr_add_source_assistant, c(list("drycomplete", root = root, action = "write"), args))
    expect_true(wrote$result$registered)
    expect_identical(dry$spec, wrote$spec)
  })
})

# Tests for R/concept_registry.R (identifier validation and execution).
# register_concept_provenance() always creates concept_registry
# inside the SOURCE'S RAW SCHEMA (e.g. "mimic_iv_demo_coh"), not the
# connection's default schema, so every execution test below creates that
# schema first and queries the table fully qualified.
#
# Identifier-validation tests supply con = NULL so they exercise the
# validation before any connection would be touched.

.s007_setup_raw_schema <- function(con, src_name = "mimic_iv_demo") {
  sn <- src_schema_names(src_name)
  DBI::dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", sn$raw))
  sn$raw
}

test_that("register_concept_provenance: rejects a malformed schema_name before touching the connection", {
  expect_error(
    register_concept_provenance(
      src_name     = "mimic_iv_demo",
      concept_name = "x",
      schema_name  = "bad; DROP TABLE important_stuff; --",
      con          = NULL
    ),
    "Invalid"
  )
})

test_that("register_concept_provenance: a known schema key resolves and validates cleanly", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  expect_no_error(
    register_concept_provenance(
      src_name     = "mimic_iv_demo",
      concept_name = c("heart_rate", "map"),
      schema_name  = "mimic_iv_demo_coh_concepts",
      origin_name  = "rendehR package",
      con          = con
    )
  )
  rows <- DBI::dbGetQuery(con, paste0(
    "SELECT concept_name, schema_name, origin_name FROM ", raw,
    ".concept_registry ORDER BY concept_name"
  ))
  expect_setequal(rows$concept_name, c("heart_rate", "map"))
  expect_true(all(rows$schema_name == "mimic_iv_demo_coh_concepts"))
})

test_that("register_concept_provenance: overwrite = TRUE updates an existing row's origin", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  register_concept_provenance(
    src_name = "mimic_iv_demo", concept_name = "heart_rate",
    schema_name = "mimic_iv_demo_coh_concepts", origin_name = "first", con = con
  )
  register_concept_provenance(
    src_name = "mimic_iv_demo", concept_name = "heart_rate",
    schema_name = "mimic_iv_demo_coh_concepts", origin_name = "second",
    overwrite = TRUE, con = con
  )
  rows <- DBI::dbGetQuery(con, paste0("SELECT origin_name FROM ", raw, ".concept_registry"))
  expect_identical(nrow(rows), 1L)
  expect_identical(rows$origin_name, "second")
})

test_that("register_concept_provenance: overwrite = FALSE does not clobber an existing row", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  register_concept_provenance(
    src_name = "mimic_iv_demo", concept_name = "heart_rate",
    schema_name = "mimic_iv_demo_coh_concepts", origin_name = "first", con = con
  )
  register_concept_provenance(
    src_name = "mimic_iv_demo", concept_name = "heart_rate",
    schema_name = "mimic_iv_demo_coh_concepts", origin_name = "second",
    overwrite = FALSE, con = con
  )
  rows <- DBI::dbGetQuery(con, paste0("SELECT origin_name FROM ", raw, ".concept_registry"))
  expect_identical(rows$origin_name, "first")
})

test_that(".resolve_origin: known ref keys map to their citation, unknown keys pass through", {
  blended <- .resolve_origin("blended")
  expect_identical(blended$name, "BlendedICU")
  expect_match(blended$ref, "doi:10.1016/j.jbi.2023.104502", fixed = TRUE)
  expect_identical(.resolve_origin("rendehr")$name, "rendehR package")
  expect_match(.resolve_origin("ehr_eicu")$url, "eicu-code", fixed = TRUE)

  unknown <- .resolve_origin("smith_2023")
  expect_identical(unknown$name, "smith_2023")
  expect_true(is.na(unknown$url))

  none <- .resolve_origin(NA)
  expect_identical(none$name, "rendehR package")
})

test_that("auto_register_atomic_provenance: groups by ref and registers only the requested concepts", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      source       = c("mimic_iv", "mimic_iv", "mimic_iv"),
      concept_name = c("heart_rate", "map", "not_built"),
      ref          = c(NA, "mimic_code", NA),
      stringsAsFactors = FALSE
    ),
    cfg
  )

  # Only heart_rate and map actually succeeded (e.g. per run_atomic_concepts_unified()'s
  # returned result); not_built must not be registered.
  auto_register_atomic_provenance(
    con, "mimic_iv_demo", "mimic_iv_demo_coh_concepts", cfg,
    concept_names = c("heart_rate", "map")
  )

  rows <- DBI::dbGetQuery(con, paste0("SELECT concept_name FROM ", raw, ".concept_registry"))
  expect_setequal(rows$concept_name, c("heart_rate", "map"))
  expect_false("not_built" %in% rows$concept_name)
})

test_that("auto_register_atomic_provenance: with no filter, registers every matching concept (unchanged default)", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(source = "mimic_iv", concept_name = c("a", "b"), stringsAsFactors = FALSE),
    cfg
  )
  auto_register_atomic_provenance(con, "mimic_iv_demo", "mimic_iv_demo_coh_concepts", cfg)
  rows <- DBI::dbGetQuery(con, paste0("SELECT concept_name FROM ", raw, ".concept_registry"))
  expect_setequal(rows$concept_name, c("a", "b"))
})

test_that(".register_pkg_layer_provenance: succeeded filter restricts registration to those files", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  dir <- file.path(tempdir(), paste0("s007_pkg_layer_", as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("", file.path(dir, "ok_concept.sql"))
  writeLines("", file.path(dir, "failed_concept.sql"))

  .register_pkg_layer_provenance(
    con, "mimic_iv_demo", dir, "mimic_iv_demo_coh_concepts",
    succeeded = "ok_concept.sql"
  )

  rows <- DBI::dbGetQuery(con, paste0("SELECT concept_name FROM ", raw, ".concept_registry"))
  expect_identical(rows$concept_name, "ok_concept")
})

test_that(".register_user_layer_provenance: succeeded filter restricts registration to those files", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  dir <- file.path(tempdir(), paste0("s007_user_layer_", as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("", file.path(dir, "ok.sql"))
  writeLines("", file.path(dir, "bad.sql"))

  .register_user_layer_provenance(
    con, "mimic_iv_demo", dir, "mimic_iv_demo_coh_concepts",
    succeeded = "ok.sql"
  )

  rows <- DBI::dbGetQuery(con, paste0("SELECT concept_name, origin_name FROM ", raw, ".concept_registry"))
  expect_identical(rows$concept_name, "ok")
  expect_identical(rows$origin_name, "user")
})

test_that(".snapshot_tables: returns character(0), not an error, when the query cannot be answered", {
  # con = NULL guarantees the underlying query fails regardless of backend
  # (Postgres pg_tables vs. DuckDB catalog details); this only exercises the
  # tryCatch fallback contract, not any specific backend's system catalog.
  before <- .snapshot_tables(NULL, "does_not_exist_schema")
  expect_identical(before, character(0))
})

test_that(".register_new_tables: registers only tables created after the snapshot", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  # .snapshot_tables() itself depends on backend-specific system catalogs
  # (Postgres pg_tables); mock it here so this test targets only
  # .register_new_tables()'s diff-and-register logic.
  local_mocked_bindings(
    .snapshot_tables = function(con, schema_name) c("old_tbl", "brand_new_tbl")
  )

  .register_new_tables(con, "mimic_iv_demo", before_tables = "old_tbl",
                       schema_name = "main", origin_key = "mimic_code")

  rows <- DBI::dbGetQuery(con, paste0("SELECT concept_name FROM ", raw, ".concept_registry"))
  expect_identical(rows$concept_name, "brand_new_tbl")
})

test_that(".register_new_tables: no-op when nothing new appeared", {
  local_mocked_bindings(.snapshot_tables = function(con, schema_name) c("old_tbl"))
  result <- .register_new_tables(NULL, "mimic_iv_demo", before_tables = "old_tbl",
                                 schema_name = "main", origin_key = "mimic_code")
  expect_null(result)
})

test_that("auto_register_external_provenance is retired", {
  expect_false(exists("auto_register_external_provenance", mode = "function"))
})

test_that("concept_unit_conversion_func_2 is retired", {
  expect_false(exists("concept_unit_conversion_func_2", mode = "function"))
})

test_that("provenance is registered under the table names the SQL creates", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- .s007_setup_raw_schema(con)

  dir <- tempfile("pkg_flat_")
  dir.create(dir)
  writeLines(c("DROP TABLE IF EXISTS {concepts_schema}.age;",
               "CREATE TABLE  {concepts_schema}.age AS SELECT 1 AS x;"),
             file.path(dir, "eicu_age.sql"))
  .register_pkg_layer_provenance(con, "mimic_iv_demo", dir,
                                 "mimic_iv_demo_coh_concepts")

  cfg <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(source = "mimic_iv", concept_name = "NaCl_exp"), cfg)
  auto_register_atomic_provenance(con, "mimic_iv_demo",
                                  "mimic_iv_demo_coh_concepts", cfg)

  rows <- DBI::dbGetQuery(con, paste0("SELECT concept_name FROM ", raw, ".concept_registry"))
  expect_setequal(rows$concept_name, c("age", "nacl_exp"))
})

test_that("every ref in the shipped concept configs names a known origin", {
  for (f in c("rendehr_concepts_config.csv", "rendehr_duration_concepts_config.csv")) {
    cfg <- readr::read_csv(system.file("extdata", f, package = "rendehR"),
                           col_types = readr::cols(.default = "c"))
    expect_true(all(cfg$ref %in% names(.origin_labels)), info = f)
  }
})

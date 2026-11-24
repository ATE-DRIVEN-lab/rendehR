test_that("database identifiers are validated before SQL execution", {
  expect_equal(.ehr_validate_db_identifier("valid_name_1"), "valid_name_1")
  for (x in c("", "has space", "x;DROP_DATABASE", "1starts_wrong", "a-b"))
    expect_error(.ehr_validate_db_identifier(x), "Invalid")
  expect_true(.ehr_protected_database("postgres"))
  expect_true(.ehr_protected_database("TEMPLATE1"))
  expect_false(.ehr_protected_database("rendehr_s004_validation_test"))
})

test_that("canonical catalog retires legacy MIMIC-IV and retains v3.1", {
  cfg <- jsonlite::read_json(
    system.file("extdata", "rendehr-data-source.json", package = "rendehR"),
    simplifyVector = FALSE
  )
  names <- vapply(cfg, `[[`, character(1), "name")
  expect_false("miiv" %in% names)
  miiv <- cfg[[match("mimic_iv", names)]]
  expect_equal(miiv$release, "3.1")
  expect_equal(miiv$url, "https://physionet.org/files/mimiciv/3.1")
  expect_false("loinc_code" %in% names(miiv$tables$d_labitems$cols))
  expect_true(all(c("poe_id", "poe_seq") %in%
                    names(miiv$tables$prescriptions$cols)))
})

test_that("MIMIC-IV DDL follows canonical v3.1 columns", {
  paths <- ehr_paths("mimic_iv", base_dir = tempdir())
  ddl <- .duckdb_schema_from_sql_dir(paths$schema_sql_dir)
  cfg <- load_src_cfg(paths$data_config_json, "mimic_iv")
  catalog <- .duckdb_schema_from_cfg(cfg)
  expect_setequal(names(ddl), names(catalog))
  for (table in names(catalog)) {
    expect_setequal(names(ddl[[table]]), names(catalog[[table]]))
    expect_equal(unname(ddl[[table]][names(catalog[[table]])]),
                 unname(catalog[[table]]))
  }
  expect_false("loinc_code" %in% names(ddl$d_labitems))
  expect_false("cancelreason" %in% names(ddl$inputevents))
  expect_true(all(c("poe_id", "poe_seq") %in% names(ddl$prescriptions)))
  expect_false(any(c("secondaryordercategoryname", "totalamount",
                     "totalamountuom", "cancelreason", "comments_date") %in%
                   names(ddl$procedureevents)))
})

test_that("all post-load indexes match their selected release schemas", {
  for (source in c("eicu", "mimic_iii", "mimic_iv")) {
    paths <- ehr_paths(source, base_dir = tempdir())
    cfg <- load_src_cfg(paths$data_config_json, source)
    schema <- .duckdb_schema_for_paths(paths, cfg)
    specs <- .ehr_validate_index_file(
      file.path(paths$helper_sql_dir, "indexes.sql"), schema
    )
    expect_gt(nrow(specs), 0L)
  }
})

test_that("post-load index validation rejects unknown columns", {
  sql <- tempfile(fileext = ".sql")
  writeLines(
    "CREATE INDEX IF NOT EXISTS patients_bad_idx ON patients (missing_col);",
    sql
  )
  expect_error(
    .ehr_validate_index_file(sql, list(patients = c(subject_id = "BIGINT"))),
    "unknown column"
  )
})

test_that("doubles bound for NUMERIC are formatted per value, not per batch", {
  f <- .ehr_pg_numeric_text
  expect_identical(f(c(36.7, 100, -0.300000011920929)),
                   c("36.7", "100", "-0.300000011920929"))
  expect_identical(f(36.7), f(c(36.7, 1e-12, 123456.789))[1])
  x <- c(0.1 + 0.2, 1 / 3, NA, NaN, Inf, -Inf)
  expect_no_warning(out <- f(x))   # missing and infinite values must not warn on every chunk
  expect_identical(as.numeric(out[1:2]), x[1:2])
  expect_identical(out[3:6], c(NA, "NaN", "Infinity", "-Infinity"))
  expect_identical(f(1:3), 1:3)
  d <- as.Date("2020-01-01")
  expect_identical(f(d), d)
})

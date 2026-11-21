# Pure SQL-string-construction helpers - no database connection needed.

test_that(".src_id_map: returns the correct native ID columns per source family", {
  expect_identical(unname(.src_id_map("mimic_iv_demo")),
                    c("subject_id", "hadm_id", "stay_id"))
  expect_identical(unname(.src_id_map("mimic_iii_demo")),
                    c("subject_id", "hadm_id", "icustay_id"))
  expect_identical(unname(.src_id_map("eicu_demo")),
                    c("uniquepid", "patienthealthsystemstayid", "patientunitstayid"))
})

test_that(".src_id_map: errors on an unknown source family", {
  expect_error(.src_id_map("unknown_src"), "Unknown src_family")
})

test_that(".sql_cast: builds a CAST expression, with and without an alias", {
  expect_equal(.sql_cast("x.col"), "CAST(x.col AS text)")
  expect_equal(.sql_cast("x.col", "numeric", "alias"),
              "CAST(x.col AS numeric) AS alias")
})

test_that(".null_safe_id_join: builds an IS NOT DISTINCT FROM join across all id_cols", {
  cond <- .null_safe_id_join("a", "b", c("id1", "id2"))
  expect_match(cond, "a\\.id1 IS NOT DISTINCT FROM b\\.id1")
  expect_match(cond, "a\\.id2 IS NOT DISTINCT FROM b\\.id2")
})

test_that(".merge_ts_table_name: resolves admission -> ts_tab, icu -> ts_icu", {
  expect_equal(.merge_ts_table_name("admission"), "ts_tab")
  expect_equal(.merge_ts_table_name("icu"), "ts_icu")
})

test_that(".merge_ts_table_name: errors on an unsupported analysis_level", {
  expect_error(.merge_ts_table_name("bad_level"))
})

test_that(".merge_col_expr: casts an existing column, NULL-casts a missing one", {
  expect_equal(.merge_col_expr(c("a", "b"), "a"), "CAST(ts.a AS text) AS a")
  expect_equal(.merge_col_expr(c("a", "b"), "z"), "CAST(NULL AS text) AS z")
})

test_that(".merge_col_expr: raises with the reason when a required column is missing", {
  expect_error(
    .merge_col_expr(c("a", "b"), "z", required = TRUE, reason = "in mimic_iv_demo.ts_tab."),
    "Required column 'z' is missing in mimic_iv_demo.ts_tab."
  )
})

test_that(".merge_raw_expr: returns the raw reference or NULL, mirroring merge_col_expr", {
  expect_equal(.merge_raw_expr(c("a", "b"), "a"), "ts.a")
  expect_equal(.merge_raw_expr(c("a", "b"), "z"), "NULL")
  expect_error(.merge_raw_expr(c("a", "b"), "z", required = TRUE), "Required column 'z'")
})

test_that(".merge_ehr_id_expr: builds a concat_ws over NULLIF-wrapped parts", {
  expr <- .merge_ehr_id_expr(c("p", "a", "i"))
  expect_match(expr, "^concat_ws\\('_', ")
  expect_match(expr, "NULLIF\\(CAST\\(p AS text\\), ''\\)")
  expect_match(expr, "NULLIF\\(CAST\\(a AS text\\), ''\\)")
  expect_match(expr, "NULLIF\\(CAST\\(i AS text\\), ''\\)")
})

test_that(".ehr_id_grain_parts: admission drops the last (icu) part; icu keeps all", {
  parts <- c("patient", "admission", "icu")
  expect_identical(.ehr_id_grain_parts(parts, "admission"), c("patient", "admission"))
  expect_identical(.ehr_id_grain_parts(parts, "icu"), parts)
})

# Identifier validation, exercised directly and via merge_sources()'s public
# entry point (which validates target_schema before opening any database
# connection or reading credentials, so this is testable without a live
# PostgreSQL target).

test_that(".ehr_validate_db_identifier: accepts a well-formed identifier unchanged", {
  expect_equal(.ehr_validate_db_identifier("rendehr", "schema name"), "rendehr")
  expect_equal(.ehr_validate_db_identifier("valid_table_1", "table name"), "valid_table_1")
})

test_that(".ehr_validate_db_identifier: rejects a malformed identifier", {
  expect_error(.ehr_validate_db_identifier("bad name", "schema name"), "Invalid schema name")
  expect_error(.ehr_validate_db_identifier("bad\"name", "table name"), "Invalid table name")
})

test_that(".ehr_validate_db_identifier: rejects a deliberately stacked-statement string", {
  expect_error(
    .ehr_validate_db_identifier("sciehr; DROP SCHEMA public CASCADE; --", "schema name"),
    "Invalid schema name"
  )
})

test_that("merge_sources: rejects a malformed target_schema before opening any connection", {
  expect_error(
    merge_sources(c("mimic_iv_demo", "mimic_iii_demo"),
                  target_schema = "sciehr; DROP SCHEMA public CASCADE; --"),
    "Invalid schema name"
  )
})

test_that("merge_sources: rejects invalid src_names/append arguments before any I/O", {
  expect_error(merge_sources(character(0)), "src_names must contain")
  expect_error(merge_sources(""), "src_names must contain")
  expect_error(merge_sources("mimic_iv_demo", append = "yes"), "append must be TRUE or FALSE")
  expect_error(merge_sources("mimic_iv_demo", append = c(TRUE, FALSE)),
              "append must be TRUE or FALSE")
})

# merge_sources()'s full happy path (including the merge_dur output and
# append-mode transaction handling) requires a real, populated PostgreSQL
# target and cannot be redirected to a test database because it calls
# connect_postgres() internally; it is covered by the validation harness
# (validation/) instead.
test_that("merge_sources: full execution requires a real PostgreSQL target", {
  skip("merge_sources() opens its own live PostgreSQL connection; covered by \
the validation harness, not a unit test.")
})

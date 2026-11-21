.make_mimic_iv_cohort_fixture <- function() {
  base <- tempfile("ehr_s003_")
  raw_root <- file.path(base, "raw")
  raw <- file.path(raw_root, "mimic_iv_demo")
  dir.create(file.path(raw, "hosp"), recursive = TRUE)
  dir.create(file.path(raw, "icu"), recursive = TRUE)

  patients <- data.frame(
    subject_id = 1:3,
    gender = c("F", "M", "F"),
    anchor_age = c("40", "dirty", "60"),
    anchor_year = 2020L,
    anchor_year_group = "2017 - 2019",
    dod = NA_character_,
    vendor_extra = c("excluded", "excluded", "excluded")
  )
  admissions <- data.frame(
    subject_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    hadm_id = c(10L, 11L, 20L, 21L, 30L, 31L),
    admittime = sprintf("2020-01-%02d 00:00:00", 1:6),
    dischtime = sprintf("2020-01-%02d 00:00:00", 2:7),
    admission_type = "URGENT"
  )
  icustays <- data.frame(
    subject_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L),
    hadm_id = c(10L, 10L, 11L, 11L, 20L, 20L, 20L, 21L, 31L, 31L),
    stay_id = c(100L, 101L, 102L, 103L, 200L, 201L, 202L, 203L, 300L, 301L),
    intime = sprintf("2020-01-%02d 02:00:00", c(1, 1, 2, 2, 3, 3, 3, 4, 6, 6)),
    outtime = sprintf("2020-01-%02d 12:00:00", c(1, 1, 2, 2, 3, 3, 3, 4, 6, 6))
  )
  diagnoses <- data.frame(
    subject_id = c(1L, 2L, 3L),
    hadm_id = c(10L, 20L, 31L),
    seq_num = 1L,
    icd_code = c("S14.0", "X00", "S140"),
    icd_version = c(10L, 10L, 10L)
  )
  readr::write_csv(patients, file.path(raw, "hosp", "patients.csv.gz"))
  readr::write_csv(admissions, file.path(raw, "hosp", "admissions.csv.gz"))
  readr::write_csv(icustays, file.path(raw, "icu", "icustays.csv.gz"))
  readr::write_csv(
    diagnoses, file.path(raw, "hosp", "diagnoses_icd.csv.gz")
  )

  ids <- file.path(base, "ids.csv")
  readr::write_csv(data.frame(stay_id = "100"), ids)
  list(base = base, raw_root = raw_root, raw = raw, ids = ids)
}

test_that("SQL, function, ICD, and no-filter routes share the normalized plan", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)

  sql <- select_cohort_ids(
    "mimic_iv_demo",
    cohort_sql = "SELECT stay_id FROM icustays WHERE stay_id = 100",
    key = "stay_id",
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )
  expect_s3_class(sql, "ehr_selection_plan")
  expect_identical(sql$route, "sql")
  expect_identical(sql$values, "100")

  fn <- select_cohort_ids(
    "mimic_iv_demo",
    cohort_fn = function(paths) data.frame(hadm_id = 10L),
    key = "hadm_id",
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )
  expect_identical(fn$route, "function_result")
  expect_identical(fn$selection_key, "hadm_id")

  filter <- file.path(fixture$base, "icd.csv")
  readr::write_csv(data.frame(icd_code = "S140", icd_version = "10"), filter)
  icd <- select_cohort_ids(
    "mimic_iv_demo",
    icd_filter_path = filter,
    import_scope = "icu",
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )
  expect_identical(icd$evidence_key, "hadm_id")
  # import_scope = "icu" is finer than the icd route's own selection grain
  # (hadm_id), so it's clamped up to "hadm_id" (.resolve_import_scope()) -
  # not silently forced to "subject_id" regardless of what was requested.
  expect_identical(icd$effective_scope, "hadm_id")
  expect_setequal(icd$values, c("10", "31"))

  named <- select_cohort_ids(
    "mimic_iv_demo",
    cohort = "scid",
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )
  expect_identical(named$route, "named_icd")
  expect_identical(named$evidence_key, "hadm_id")
  expect_identical(named$effective_scope, "subject_id")

  whole <- select_cohort_ids(
    "mimic_iv_demo",
    no_filter = TRUE,
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )
  expect_identical(whole$route, "no_filter")
  expect_equal(whole$matched_count, 3L)
})

test_that("projection reporting aggregates conversion failures per table", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  raw_view <- ".__ehr_raw_events"
  DBI::dbWriteTable(
    con, raw_view,
    data.frame(
      raw_subject = c("1", "bad", ""),
      raw_value = c("1.5", "bad", ""),
      label = c("ok", "still-ok", "blank"),
      vendor_extra = c("a", "b", "c"),
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )
  table_cfg <- list(
    cohort_role = "clinical",
    cohort_link_key = "subject_id",
    cols = list(
      subject_id = list(name = "raw_subject", spec = "col_integer"),
      value = list(name = "raw_value", spec = "col_double"),
      label = list(name = "label", spec = "col_character")
    )
  )
  headers <- list(
    c("raw_subject", "raw_value", "label", "vendor_extra"),
    c("raw_subject", "label")
  )

  report <- .ehr_projection_report(
    con, "events", table_cfg, headers, raw_view
  )
  configured <- report[!is.na(report$column), , drop = FALSE]
  expect_identical(
    configured$column, c("subject_id", "value", "label")
  )
  expect_equal(configured$conversion_failures, c(1, 1, 0))
  expect_identical(
    configured$status, c("configured", "typed_missing", "configured")
  )
  expect_equal(configured$missing_file_count, c(0L, 1L, 0L))
  expect_true(any(
    report$status == "excluded_extra" &
      report$raw_name == "vendor_extra"
  ))

  expect_error(
    .ehr_assert_required_projection(
      report, table_cfg, "subject_id", "events"
    ),
    "required hierarchy/linkage"
  )
  report$conversion_failures[report$column == "subject_id"] <- 0
  expect_silent(.ehr_assert_required_projection(
    report, table_cfg, "subject_id", "events"
  ))
})

test_that("persistent DuckDB materializer exposes one canonical hierarchy", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  db <- file.path(fixture$base, "outputs", "cohort.duckdb")

  result <- materialize_cohort_duckdb(
    "mimic_iv_demo",
    db_path = db,
    direct_ids_path = fixture$ids,
    key = "stay_id",
    import_scope = "patient",
    tables = c("patients", "admissions", "icustays"),
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    validate_columns = TRUE,
    overwrite = TRUE,
    show_progress = FALSE
  )
  expect_identical(result$status, "built")
  expect_true(file.exists(db))

  con <- connect_duckdb(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  hierarchy <- DBI::dbGetQuery(
    con,
    "SELECT * FROM mimic_iv_demo_coh.cohort_hierarchy
     ORDER BY hadm_id, stay_id"
  )
  expect_equal(nrow(hierarchy), 4L)
  expect_true(all(hierarchy$patient_context_index))
  expect_equal(sum(hierarchy$definition_filter_index), 1L)
  expect_true(all(hierarchy$scope_index))
  expect_equal(
    DBI::dbGetQuery(
      con, "SELECT count(*) n FROM mimic_iv_demo_coh.cohort_manifest"
    )$n,
    1
  )
  expect_true(DBI::dbExistsTable(
    con, DBI::Id(schema = "mimic_iv_demo_coh",
                 table = "cohort_definition_filter")
  ))
  patient_cols <- DBI::dbGetQuery(
    con, "DESCRIBE mimic_iv_demo_coh.patients"
  )$column_name
  expect_false("vendor_extra" %in% patient_cols)
  projection <- DBI::dbGetQuery(
    con,
    "SELECT \"column\", conversion_failures, status
       FROM mimic_iv_demo_coh.cohort_projection_report
      WHERE \"table\" = 'patients'"
  )
  expect_equal(
    projection$conversion_failures[projection$column %in% "anchor_age"],
    1
  )
  expect_true(any(projection$status == "excluded_extra"))
})

test_that("ICD-route selection publishes a d_<name>_icds reference table", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)

  # Raw icd_filter_path (no named cohort): table name derives from the
  # file's basename, and the dictionary preserves every real column (not
  # just icd_code) verbatim (raw casing/formatting, not the normalized form
  # used internally for matching), deduplicated on exact
  # (icd_code, icd_version) repeats.
  filter <- file.path(fixture$base, "my_test_codes.csv")
  readr::write_csv(
    data.frame(
      icd_code = c("S14.0", "S14.0", "s140"),
      icd_version = "10",
      long_title = c("Injury of spinal cord", "Injury of spinal cord", "lowercase variant")
    ),
    filter
  )
  db1 <- file.path(fixture$base, "outputs", "raw_icd.duckdb")
  materialize_cohort_duckdb(
    "mimic_iv_demo",
    db_path = db1,
    icd_filter_path = filter,
    import_scope = "patient",
    tables = c("patients", "admissions", "icustays"),
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE,
    show_progress = FALSE
  )
  con1 <- connect_duckdb(db1, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con1, shutdown = TRUE), add = TRUE)
  expect_true(DBI::dbExistsTable(
    con1, DBI::Id(schema = "mimic_iv_demo_coh", table = "d_my_test_codes_icds")
  ))
  dict1 <- DBI::dbGetQuery(
    con1,
    "SELECT icd_code, long_title FROM mimic_iv_demo_coh.d_my_test_codes_icds
     ORDER BY icd_code"
  )
  # Exact duplicate ("S14.0" twice) collapses to one row; the distinct raw
  # variant ("s140") survives as its own row with its own long_title.
  expect_setequal(dict1$icd_code, c("S14.0", "s140"))
  expect_equal(
    dict1$long_title[dict1$icd_code == "S14.0"], "Injury of spinal cord"
  )
  expect_equal(
    dict1$long_title[dict1$icd_code == "s140"], "lowercase variant"
  )

  # Named bundled cohort: table name is the cohort name itself, and the
  # full bundled dictionary (long_title, categories, etc.) is preserved,
  # not just bare codes.
  db2 <- file.path(fixture$base, "outputs", "named_icd.duckdb")
  materialize_cohort_duckdb(
    "mimic_iv_demo",
    db_path = db2,
    cohort = "scid",
    tables = c("patients", "admissions", "icustays"),
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE,
    show_progress = FALSE
  )
  con2 <- connect_duckdb(db2, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  expect_true(DBI::dbExistsTable(
    con2, DBI::Id(schema = "mimic_iv_demo_coh", table = "d_scid_icds")
  ))
  scid_dict <- DBI::dbGetQuery(
    con2, "SELECT * FROM mimic_iv_demo_coh.d_scid_icds"
  )
  expect_gt(nrow(scid_dict), 0L)
  expect_true(all(c(
    "icd_code", "long_title", "anatomical_region", "specific_level"
  ) %in% names(scid_dict)))
  expect_false("...1" %in% names(scid_dict))
  expect_true(all(nzchar(scid_dict$long_title)))

  # Non-ICD (direct-ids) route: no dictionary table at all.
  db3 <- file.path(fixture$base, "outputs", "direct_ids.duckdb")
  materialize_cohort_duckdb(
    "mimic_iv_demo",
    db_path = db3,
    direct_ids_path = fixture$ids,
    key = "stay_id",
    tables = c("patients", "admissions", "icustays"),
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE,
    show_progress = FALSE
  )
  con3 <- connect_duckdb(db3, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con3, shutdown = TRUE), add = TRUE)
  tables3 <- DBI::dbGetQuery(
    con3, "SELECT table_name FROM information_schema.tables
           WHERE table_schema = 'mimic_iv_demo_coh'"
  )$table_name
  expect_false(any(grepl("^d_.*_icds$", tables3)))
})

test_that("Parquet and persistent DuckDB materializers have logical parity", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("arrow")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  raw_before <- file.info(list.files(
    fixture$raw, recursive = TRUE, full.names = TRUE
  ))[, c("size", "mtime"), drop = FALSE]

  parquet_result <- import_cohort_general(
    "mimic_iv_demo",
    direct_ids_path = fixture$ids,
    key = "stay_id",
    import_scope = "patient",
    tables = c("patients", "admissions", "icustays"),
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE,
    show_progress = FALSE
  )
  db <- file.path(fixture$base, "outputs", "cohort.duckdb")
  duckdb_result <- materialize_cohort_duckdb(
    "mimic_iv_demo",
    db_path = db,
    direct_ids_path = fixture$ids,
    key = "stay_id",
    import_scope = "patient",
    tables = c("patients", "admissions", "icustays"),
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE,
    show_progress = FALSE
  )

  parquet_dir <- ehr_paths(
    "mimic_iv_demo", base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )$cohort_parquet_dir
  pq_hierarchy <- as.data.frame(arrow::read_parquet(
    file.path(parquet_dir, "cohort_hierarchy.parquet")
  ))
  con <- connect_duckdb(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  db_hierarchy <- DBI::dbGetQuery(
    con, "SELECT * FROM mimic_iv_demo_coh.cohort_hierarchy"
  )
  ordering <- c("subject_id", "hadm_id", "stay_id")
  pq_hierarchy <- pq_hierarchy[do.call(order, pq_hierarchy[ordering]), ]
  db_hierarchy <- db_hierarchy[do.call(order, db_hierarchy[ordering]), ]
  rownames(pq_hierarchy) <- NULL
  rownames(db_hierarchy) <- NULL
  expect_equal(pq_hierarchy, db_hierarchy)

  pq_projection <- as.data.frame(arrow::read_parquet(
    file.path(parquet_dir, "cohort_projection_report.parquet")
  ))
  db_projection <- DBI::dbGetQuery(
    con, "SELECT * FROM mimic_iv_demo_coh.cohort_projection_report"
  )
  order_projection <- function(x) {
    ord <- do.call(order, c(
      x[c("table", "status", "raw_name", "column")],
      list(na.last = TRUE)
    ))
    out <- x[ord, , drop = FALSE]
    rownames(out) <- NULL
    out
  }
  expect_equal(
    order_projection(pq_projection),
    order_projection(db_projection)
  )

  pq_manifest <- jsonlite::fromJSON(
    file.path(parquet_dir, "cohort_manifest.json"),
    simplifyVector = FALSE
  )
  db_manifest <- DBI::dbGetQuery(
    con, "SELECT * FROM mimic_iv_demo_coh.cohort_manifest"
  )
  expect_identical(
    pq_manifest$schemas,
    jsonlite::fromJSON(
      db_manifest$schemas_json[[1L]], simplifyVector = FALSE
    )
  )
  expect_identical(
    pq_manifest$row_counts,
    jsonlite::fromJSON(
      db_manifest$row_counts_json[[1L]], simplifyVector = FALSE
    )
  )
  expect_true(file.exists(file.path(parquet_dir, "cohort_manifest.json")))
  expect_false(any(grepl("\\.duckdb", list.files(
    parquet_dir, all.files = TRUE
  ))))
  expect_identical(parquet_result$status, "built")
  expect_identical(duckdb_result$status, "built")

  raw_after <- file.info(rownames(raw_before))[, c("size", "mtime"), drop = FALSE]
  expect_identical(raw_after$size, raw_before$size)
  expect_equal(as.numeric(raw_after$mtime), as.numeric(raw_before$mtime))
})

test_that("ICU-scope Parquet stays valid with a no-ICU admission in context", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("arrow")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  ids <- file.path(fixture$base, "stay-with-no-icu-context.csv")
  readr::write_csv(data.frame(stay_id = "300"), ids)

  result <- import_cohort_general(
    "mimic_iv_demo",
    direct_ids_path = ids,
    key = "stay_id",
    import_scope = "icu",
    tables = c("patients", "admissions", "icustays"),
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE,
    show_progress = FALSE
  )
  parquet_dir <- ehr_paths(
    "mimic_iv_demo",
    base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )$cohort_parquet_dir
  hierarchy <- as.data.frame(arrow::read_parquet(
    file.path(parquet_dir, "cohort_hierarchy.parquet")
  ))
  no_icu <- is.na(hierarchy$stay_id)

  expect_identical(result$status, "built")
  expect_true(.ehr_validate_parquet_artifact(parquet_dir))
  expect_true(any(no_icu))
  expect_false(anyNA(hierarchy$definition_filter_index))
  expect_false(anyNA(hierarchy$scope_index))
  expect_true(all(!hierarchy$scope_index[no_icu]))
})

test_that("tables bounds both materializers without losing core artifacts", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  db <- file.path(fixture$base, "cohort.duckdb")
  materialize_cohort_duckdb(
    "mimic_iv_demo", db_path = db,
    direct_ids_path = fixture$ids, key = "stay_id",
    tables = "patients", base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE, show_progress = FALSE
  )
  con <- connect_duckdb(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true(.duckdb_table_exists(con, "mimic_iv_demo_coh", "patients"))
  expect_false(.duckdb_table_exists(con, "mimic_iv_demo_coh", "admissions"))
  expect_true(.duckdb_table_exists(
    con, "mimic_iv_demo_coh", "cohort_hierarchy"
  ))

  import_cohort_general(
    "mimic_iv_demo",
    direct_ids_path = fixture$ids, key = "stay_id",
    tables = "patients", base_dir = fixture$base,
    raw_data_root = fixture$raw_root,
    overwrite = TRUE, show_progress = FALSE
  )
  parquet_dir <- ehr_paths(
    "mimic_iv_demo", base_dir = fixture$base,
    raw_data_root = fixture$raw_root
  )$cohort_parquet_dir
  files <- list.files(parquet_dir)
  expect_true("patients.parquet" %in% files)
  expect_false("admissions.parquet" %in% files)
  expect_true(all(c(
    "cohort_hierarchy.parquet", "cohort_meta.parquet",
    "cohort_manifest.json"
  ) %in% files))
})

# The annotation path (run_cohort_info) takes cohort_index from the cohort
# definition (cohort_hierarchy.definition_filter_index), whatever selection
# route built the cohort, and never from the import scope.
.cohort_info_matches_hierarchy <- function(fixture, db_name, ...) {
  db <- file.path(fixture$base, "outputs", db_name)
  suppressMessages(materialize_cohort_duckdb(
    "mimic_iv_demo", db_path = db,
    tables = c("patients", "admissions", "icustays", "diagnoses_icd"),
    base_dir = fixture$base, raw_data_root = fixture$raw_root,
    overwrite = TRUE, show_progress = FALSE, ...))
  con <- connect_duckdb(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ci <- suppressMessages(run_cohort_info(
    con, src_schema_names("mimic_iv_demo"), "mimic_iv_demo", publish = FALSE))
  hier <- DBI::dbGetQuery(con,
    "SELECT hadm_id, bool_or(definition_filter_index) AS member
       FROM mimic_iv_demo_coh.cohort_hierarchy GROUP BY hadm_id")
  expected <- stats::setNames(as.logical(hier$member), as.character(hier$hadm_id))
  got <- stats::setNames(ci$cohort_index, as.character(ci$hadm_id))
  list(got = got, expected = expected, ci = ci)
}

test_that("annotation cohort_index equals the cohort definition for every route", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  filter <- file.path(fixture$base, "icd.csv")
  readr::write_csv(data.frame(icd_code = "S140", icd_version = "10"), filter)

  routes <- list(
    # ICD evidence on hadm 10 and 31; patient scope adds hadm 11 and 30 as
    # patient-context admissions, which are not members.
    icd = list(icd_filter_path = filter, import_scope = "patient"),
    icd_hospital = list(icd_filter_path = filter, import_scope = "hospital"),
    sql = list(cohort_sql = "SELECT stay_id FROM icustays WHERE stay_id = 100",
               key = "stay_id"),
    direct_ids = list(direct_ids_path = fixture$ids),
    no_filter = list(no_filter = TRUE))

  for (nm in names(routes)) {
    r <- do.call(.cohort_info_matches_hierarchy,
                 c(list(fixture, paste0(nm, ".duckdb")), routes[[nm]]))
    expect_identical(unname(r$got[names(r$got)]),
                     unname(r$expected[names(r$got)]), info = nm)
    expect_true(all(names(r$got) %in% names(r$expected)), info = nm)
    expect_false(any(c("hos_cohort_flg", "scope_index", "scope_level") %in%
                       names(r$ci)), info = nm)
  }

  # Patient vs hospital import: context is added, membership is unchanged.
  pat <- .cohort_info_matches_hierarchy(fixture, "icd_pat.duckdb",
                                        icd_filter_path = filter,
                                        import_scope = "patient")
  hosp <- .cohort_info_matches_hierarchy(fixture, "icd_hosp.duckdb",
                                         icd_filter_path = filter,
                                         import_scope = "hospital")
  members <- function(ci) sort(unique(ci$hadm_id[ci$cohort_index]))
  expect_identical(as.integer(members(pat$ci)), c(10L, 31L))
  expect_identical(members(pat$ci), members(hosp$ci))
  expect_true(all(c(11L, 30L) %in% pat$ci$hadm_id))
  expect_false(any(pat$ci$cohort_index[pat$ci$hadm_id %in% c(11L, 30L)]))
  # hadm 10 has two ICU stays (100, 101): one flag per hospitalization.
  expect_identical(unique(pat$ci$cohort_index[pat$ci$hadm_id == 10L]), TRUE)
})

test_that("run_cohort_info stops when the cohort hierarchy is missing", {
  skip_if_not_installed("duckdb")
  con <- connect_duckdb(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA mimic_iv_demo_coh")
  DBI::dbExecute(con, paste(
    "CREATE TABLE mimic_iv_demo_coh.cohort_hierarchy",
    "(subject_id INTEGER, hadm_id INTEGER, stay_id INTEGER)"))
  local_mocked_bindings(
    .build_cohort_info_mimic_iv = function(...)
      data.frame(subject_id = 1L, hadm_id = 1L, stay_id = NA_integer_))
  expect_error(
    suppressMessages(run_cohort_info(con, src_schema_names("mimic_iv_demo"),
                                     "mimic_iv_demo", publish = FALSE)),
    "missing column\\(s\\): definition_filter_index")
  DBI::dbExecute(con, "DROP TABLE mimic_iv_demo_coh.cohort_hierarchy")
  expect_error(
    suppressMessages(run_cohort_info(con, src_schema_names("mimic_iv_demo"),
                                     "mimic_iv_demo", publish = FALSE)),
    "cohort_hierarchy not found")
})

test_that("an artifact from the superseded eICU ICD selection is not reused or loaded", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  filter <- file.path(fixture$base, "icd.csv")
  readr::write_csv(data.frame(icd_code = "S140", icd_version = "10"), filter)
  build <- function(...) suppressMessages(import_cohort_general(
    "mimic_iv_demo", icd_filter_path = filter, tables = "patients",
    base_dir = fixture$base, raw_data_root = fixture$raw_root,
    show_progress = FALSE, ...))
  build(overwrite = TRUE)
  dir <- ehr_paths("mimic_iv_demo", base_dir = fixture$base,
                   raw_data_root = fixture$raw_root)$cohort_parquet_dir
  expect_true(.ehr_check_parquet_artifact(dir)$ok)
  # A compatible (MIMIC ICD) artifact is reused as is.
  expect_identical(build()$status, "existing")

  # Rewrite its cohort_meta as an old eICU unit-stay ICD selection, keeping the
  # schema and row count so only the selection semantics differ.
  meta <- file.path(dir, "cohort_meta.parquet")
  con <- connect_duckdb(":memory:")
  DBI::dbExecute(con, sprintf(
    "COPY (SELECT * REPLACE ('eicu' AS source_family, 'named_icd' AS route,
                            'patientunitstayid' AS selection_key)
           FROM read_parquet(%s)) TO %s (FORMAT parquet)",
    DBI::dbQuoteString(con, meta), DBI::dbQuoteString(con, paste0(meta, ".tmp"))))
  DBI::dbDisconnect(con, shutdown = TRUE)
  file.rename(paste0(meta, ".tmp"), meta)

  check <- .ehr_check_parquet_artifact(dir)
  expect_false(check$ok)
  expect_match(check$reason, "superseded unit-stay selection")
  expect_error(build(), "superseded unit-stay selection.*overwrite = TRUE")
  paths <- list(cohort_parquet_dir = dir)
  expect_false(.ehr_validate_stage("import_cohort", "mimic_iv_demo", NULL,
                                   paths, require_log = FALSE)$ok)
  expect_identical(build(overwrite = TRUE)$status, "built")
})

test_that("a project made by ehr_init_project() imports without overwrite", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  old_active <- .ehr_env$config
  .ehr_env$config <- NULL
  withr::defer(.ehr_env$config <- old_active)
  withr::local_envvar(EHR_CONFIG = file.path(fixture$base, "config", "ehr_config.json"))
  suppressMessages(ehr_init_project("mimic_iv_demo", base_dir = fixture$base,
                                    raw_data_root = fixture$raw_root))
  cfg <- jsonlite::read_json(file.path(fixture$base, "config", "ehr_config.json"))
  expect_identical(cfg$paths$raw_data_root, fixture$raw_root)
  dir <- ehr_paths("mimic_iv_demo", base_dir = fixture$base,
                   raw_data_root = fixture$raw_root)$cohort_parquet_dir
  expect_true(dir.exists(dir))  # the empty directory init leaves behind
  res <- suppressMessages(import_cohort_general(
    "mimic_iv_demo", no_filter = TRUE, tables = "patients", base_dir = fixture$base,
    raw_data_root = fixture$raw_root, show_progress = FALSE))
  expect_identical(res$status, "built")
  expect_true(.ehr_check_parquet_artifact(dir)$ok)
})

test_that("an existing artifact is reused only for the selection that built it", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  filter <- file.path(fixture$base, "icd.csv")
  readr::write_csv(data.frame(icd_code = "S140", icd_version = "10"), filter)
  build <- function(...) suppressMessages(import_cohort_general(
    "mimic_iv_demo", tables = "patients",
    base_dir = fixture$base, raw_data_root = fixture$raw_root,
    show_progress = FALSE, ...))
  expect_identical(build(icd_filter_path = filter, overwrite = TRUE)$status, "built")
  expect_identical(build(icd_filter_path = filter)$status, "existing")

  # A different selection route, or the same path with edited contents,
  # must not be served from the artifact built for the first selection.
  expect_error(build(no_filter = TRUE), "different cohort selection")
  readr::write_csv(data.frame(icd_code = c("S140", "S141"),
                              icd_version = "10"), filter)
  expect_error(build(icd_filter_path = filter), "different cohort selection")
  expect_identical(build(icd_filter_path = filter, overwrite = TRUE)$status, "built")
  expect_identical(build(icd_filter_path = filter)$status, "existing")

  # Artifacts published before selection tracking are reused with a warning.
  dir <- ehr_paths("mimic_iv_demo", base_dir = fixture$base,
                   raw_data_root = fixture$raw_root)$cohort_parquet_dir
  unlink(.ehr_selection_file(dir))
  expect_warning(res <- build(no_filter = TRUE), "predates selection tracking")
  expect_identical(res$status, "existing")
})

test_that("no_filter keeps rows whose link key is NULL, single and chunked", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  dx <- file.path(fixture$raw_root, "mimic_iv_demo", "hosp", "diagnoses_icd.csv.gz")
  rows <- readr::read_csv(dx, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  rows <- rbind(rows, data.frame(subject_id = "1", hadm_id = NA, seq_num = "2",
                                 icd_code = "I10", icd_version = "10"))
  readr::write_csv(rows, dx, na = "")
  dir <- ehr_paths("mimic_iv_demo", base_dir = fixture$base,
                   raw_data_root = fixture$raw_root)$cohort_parquet_dir
  count <- function() nrow(arrow::read_parquet(file.path(dir, "diagnoses_icd.parquet")))
  build <- function(...) suppressMessages(import_cohort_general(
    "mimic_iv_demo", tables = "diagnoses_icd", base_dir = fixture$base,
    raw_data_root = fixture$raw_root, show_progress = FALSE, overwrite = TRUE, ...))
  build(no_filter = TRUE)
  expect_identical(count(), 4L)
  build(no_filter = TRUE, table_chunk_rows = 1000)
  expect_identical(count(), 4L)
  filter <- file.path(fixture$base, "icd.csv")
  readr::write_csv(data.frame(icd_code = "S140", icd_version = "10"), filter)
  build(icd_filter_path = filter)
  expect_lt(count(), 4L)
})

test_that("a resumed chunked build clears a partially written chunk", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE src AS SELECT i AS id, i AS link FROM range(1, 11) t(i)")
  .ehr_ensure_progress_table(con)
  # Chunk 1 is complete; chunk 2 has one row left by a killed INSERT.
  DBI::dbExecute(con, "CREATE TABLE tgt AS SELECT * FROM src
                        WHERE hash(CAST(link AS VARCHAR)) % 5 = 0")
  DBI::dbExecute(con, "INSERT INTO tgt SELECT * FROM src
                        WHERE hash(CAST(link AS VARCHAR)) % 5 = 1 LIMIT 1")
  .ehr_mark_chunk_done(con, "tgt", 1L, 5L)
  .ehr_build_cohort_table(
    con, target = "tgt", source = "src",
    filter = list(where = "", link_key = "link"),
    chunk_rows = 2, table = "tgt", show_progress = FALSE, num_rows = 10
  )
  expect_identical(sort(as.integer(DBI::dbGetQuery(con, "SELECT id FROM tgt")$id)), 1:10)
})

test_that("the SCI profile default reuses an artifact built with cohort = 'scid'", {
  skip_if_not_installed("duckdb")
  fixture <- .make_mimic_iv_cohort_fixture()
  on.exit(unlink(fixture$base, recursive = TRUE), add = TRUE)
  cfg_path <- file.path(fixture$base, "config", "ehr_config.json")
  dir.create(dirname(cfg_path), recursive = TRUE, showWarnings = FALSE)
  ehr_create_config_template(
    path = cfg_path, src_name = "mimic_iv_demo", db_name = "rendehr_val_never",
    base_dir = fixture$base, raw_data_root = fixture$raw_root,
    cohort_profile = "scid", overwrite = TRUE
  )
  prev <- rendehR:::.ehr_env$config
  on.exit(assign("config", prev, envir = rendehR:::.ehr_env), add = TRUE)
  ehr_use_config(cfg_path)
  build <- function(...) suppressMessages(import_cohort_general(
    "mimic_iv_demo", tables = "diagnoses_icd", base_dir = fixture$base,
    raw_data_root = fixture$raw_root, show_progress = FALSE, ...))
  expect_identical(build(cohort = "scid")$status, "built")
  expect_identical(build()$status, "existing")
})

test_that("printing a selection plan shows counts but never identifiers", {
  plan <- .ehr_selection_plan("mimic_iv_demo", "direct_ids", "stay_id",
                              requested_scope = "patient", effective_scope = "subject_id",
                              values = c("31269608", "39553978"))
  plan$matched_count <- 2L; plan$unmatched_count <- 0L
  out <- capture.output(res <- print(plan))
  expect_identical(res, plan)
  expect_true(any(grepl("2 requested, 2 matched, 0 unmatched", out, fixed = TRUE)))
  expect_false(any(grepl("31269608|39553978", out)))
})

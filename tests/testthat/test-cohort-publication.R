test_that("manifest-backed zero-row parquet is valid and corrupt output is not", {
  skip_if_not_installed("arrow")
  path <- tempfile("cohort.stage-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  arrow::write_parquet(
    data.frame(
      subject_id = integer(),
      patient_context_index = logical(),
      definition_filter_index = logical(),
      scope_index = logical()
    ),
    file.path(path, "cohort_hierarchy.parquet")
  )
  jsonlite::write_json(
    list(
      status = "success",
      tables = "cohort_hierarchy",
      files = "cohort_hierarchy.parquet",
      row_counts = list(cohort_hierarchy = 0),
      hierarchy_rows = 0,
      schemas = list(
        cohort_hierarchy = list(
          columns = c(
            "subject_id", "patient_context_index",
            "definition_filter_index", "scope_index"
          ),
          types = c("INTEGER", "BOOLEAN", "BOOLEAN", "BOOLEAN")
        )
      )
    ),
    file.path(path, "cohort_manifest.json"),
    auto_unbox = TRUE
  )
  expect_true(.ehr_validate_parquet_artifact(path))

  manifest_path <- file.path(path, "cohort_manifest.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$schemas$cohort_hierarchy$types[[1L]] <- "VARCHAR"
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
  expect_false(.ehr_validate_parquet_artifact(path))
  expect_match(
    .ehr_check_parquet_artifact(path)$reason,
    "schema mismatch.*cohort_hierarchy"
  )

  manifest$schemas$cohort_hierarchy$types[[1L]] <- "INTEGER"
  manifest$row_counts$cohort_hierarchy <- 1
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
  expect_false(.ehr_validate_parquet_artifact(path))
  expect_match(
    .ehr_check_parquet_artifact(path)$reason,
    "row-count mismatch.*cohort_hierarchy"
  )

  manifest$row_counts$cohort_hierarchy <- 0
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
  writeLines("not parquet", file.path(path, "cohort_hierarchy.parquet"))
  expect_false(.ehr_validate_parquet_artifact(path))
})

test_that("missing success manifest is incomplete even when parquet exists", {
  skip_if_not_installed("arrow")
  path <- tempfile("cohort_incomplete_")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  arrow::write_parquet(
    data.frame(subject_id = integer()),
    file.path(path, "patients.parquet")
  )
  expect_false(.ehr_validate_parquet_artifact(path))
})

test_that("one-current publication replaces atomically and removes recovery", {
  parent <- tempfile("publish_")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  final <- file.path(parent, "current")
  stage <- file.path(parent, ".current.stage-test")
  dir.create(final)
  dir.create(stage)
  writeLines("old", file.path(final, "marker"))
  writeLines("new", file.path(stage, "marker"))

  .ehr_publish_cohort_artifact(stage, final)
  expect_identical(readLines(file.path(final, "marker")), "new")
  expect_false(dir.exists(stage))
  expect_false(any(grepl("[.]recovery-", list.files(
    parent, all.files = TRUE
  ))))
})

test_that("failed publication restores the previous current artifact", {
  parent <- tempfile("publish_failure_")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  final <- file.path(parent, "current")
  dir.create(final)
  writeLines("old", file.path(final, "marker"))

  expect_error(suppressWarnings(
    .ehr_publish_cohort_artifact(
      file.path(parent, ".current.stage-missing"), final
    )),
    "previous cohort was restored"
  )
  expect_identical(readLines(file.path(final, "marker")), "old")
  expect_false(any(grepl("[.]recovery-", list.files(
    parent, all.files = TRUE
  ))))
})

test_that("failed first publication leaves no current artifact", {
  parent <- tempfile("publish_first_failure_")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  final <- file.path(parent, "current")
  expect_error(suppressWarnings(
    .ehr_publish_cohort_artifact(
      file.path(parent, ".current.stage-missing"), final
    )),
    "Could not publish"
  )
  expect_false(file.exists(final) || dir.exists(final))
})

test_that("staging space preflight fails before materialization", {
  raw <- tempfile()
  writeBin(as.raw(rep(1L, 1024L)), raw)
  old <- getOption("ehr.cohort.available_bytes")
  options(ehr.cohort.available_bytes = 1)
  on.exit(options(ehr.cohort.available_bytes = old), add = TRUE)
  expect_error(
    .ehr_check_staging_space(list(raw), dirname(raw)),
    "Insufficient free space"
  )
})

test_that(".ehr_split_sql splits on top-level semicolons only", {
  sql <- c(
    "-- header; with a semicolon",
    "CREATE TABLE a AS SELECT 'note; with -- chars' AS label, 'it''s' AS q;",
    "/* block; comment */ CREATE TABLE b AS SELECT \"odd;col\" FROM t;",
    "DO $$ BEGIN PERFORM 1; END $$;",
    "SELECT $tag$ x; y $tag$ AS z;",
    "   ;",
    "SELECT 1 -- trailing; comment",
    "")
  out <- .ehr_split_sql(sql)
  expect_length(out, 5)
  expect_identical(out[1], "CREATE TABLE a AS SELECT 'note; with -- chars' AS label, 'it''s' AS q")
  expect_match(out[2], "^CREATE TABLE b AS SELECT \"odd;col\" FROM t$")
  expect_identical(out[3], "DO $$ BEGIN PERFORM 1; END $$")
  expect_identical(out[4], "SELECT $tag$ x; y $tag$ AS z")
  expect_identical(out[5], "SELECT 1")
  expect_identical(.ehr_split_sql(c("-- only a comment", "")), character(0))
})

test_that("every packaged window_tables.sql splits into the same statements as before", {
  files <- list.files(.ehr_extdata("sql"), pattern = "^window_tables\\.sql$",
                      recursive = TRUE, full.names = TRUE)
  expect_gt(length(files), 0)
  for (f in files) {
    old <- trimws(strsplit(gsub("--[^\n]*", "",
      paste(readLines(f, warn = FALSE), collapse = "\n")), ";", fixed = TRUE)[[1]])
    old <- old[nzchar(old)]
    expect_identical(gsub("\\s+", " ", .ehr_split_sql(readLines(f, warn = FALSE))),
                     gsub("\\s+", " ", old), info = f)
  }
})

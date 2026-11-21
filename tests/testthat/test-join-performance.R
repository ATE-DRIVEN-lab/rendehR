# Run against a disposable PostgreSQL database with EHR_TEST_JOIN_DB set.
.join_test_connection <- function() {
  skip_if_not_installed("RPostgres")
  db <- Sys.getenv("EHR_TEST_JOIN_DB")
  skip_if(!nzchar(db), "Set EHR_TEST_JOIN_DB for PostgreSQL join regression tests")
  DBI::dbConnect(RPostgres::Postgres(), dbname = db)
}

test_that("hybrid ID joins preserve values and multiplicities and allow hashing", {
  con <- .join_test_connection()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TEMP TABLE join_base AS SELECT g AS subject_id,
    g AS hadm_id, CASE WHEN g % 6 = 0 THEN g END AS stay_id
    FROM generate_series(1, 2000) g")
  DBI::dbExecute(con, "CREATE TEMP TABLE join_concept AS
    SELECT *, subject_id::text AS value FROM join_base WHERE subject_id <> 2")
  DBI::dbExecute(con, "INSERT INTO join_concept SELECT * FROM join_concept WHERE subject_id = 1")
  DBI::dbExecute(con, "ANALYZE join_base")
  DBI::dbExecute(con, "ANALYZE join_concept")
  schema <- DBI::dbGetQuery(con,
    "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema()")$nspname
  keys <- c("subject_id", "hadm_id", "stay_id")
  non_null <- .non_null_id_cols(con, schema, "join_base", keys)
  expect_identical(non_null, keys[1:2])
  query <- function(cond) paste("SELECT b.*, c.value FROM join_base b
    LEFT JOIN join_concept c ON", cond)
  old <- query(.null_safe_id_join("b", "c", keys))
  new <- query(.null_safe_id_join("b", "c", keys, non_null))
  diff <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM
    ((%s EXCEPT ALL %s) UNION ALL (%s EXCEPT ALL %s)) d", old, new, new, old))
  expect_equal(as.numeric(diff$n), 0)
  plan <- DBI::dbGetQuery(con, paste("EXPLAIN", new))[[1]]
  expect_true(any(grepl("Hash Cond", plan)))
  DBI::dbExecute(con, "CREATE TEMP TABLE visit_windows AS TABLE join_base")
  # A flat concept with a duplicated key would fan flat_tab out, so
  # build_flat_table() skips it; with one row per key it joins exactly.
  expect_warning(build_flat_table(con, "mimic_iv", list(raw = schema,
    concepts = schema, harmonized = schema), "join_concept"), "join_concept.*skipped")
  DBI::dbExecute(con, "DELETE FROM join_concept WHERE ctid IN
    (SELECT ctid FROM join_concept WHERE subject_id = 1 LIMIT 1)")
  build_flat_table(con, "mimic_iv", list(raw = schema, concepts = schema,
    harmonized = schema), "join_concept")
  diff <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM
    ((SELECT subject_id, hadm_id, stay_id, join_concept FROM flat_tab EXCEPT ALL %s)
    UNION ALL (%s EXCEPT ALL SELECT subject_id, hadm_id, stay_id, join_concept FROM flat_tab)) d",
    old, old))
  expect_equal(as.numeric(diff$n), 0)
  DBI::dbExecute(con, "UPDATE join_base SET subject_id = NULL, hadm_id = NULL")
  expect_message(expect_identical(.non_null_id_cols(con, schema, "join_base", keys),
    character()), "retaining NULL-safe joins")
})

test_that("cohort conversion preserves text matching semantics for integer and text IDs", {
  con <- .join_test_connection()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TEMP TABLE _ehr_cohort (cohort_id text)")
  DBI::dbExecute(con, "INSERT INTO _ehr_cohort VALUES
    ('1'), ('1'), ('001'), ('+1'), (' 1'), ('1 '), ('-0'), ('0'), ('-1'),
    ('32767'), ('32768'), ('2147483647'), ('2147483648'),
    ('9223372036854775807'), ('9223372036854775808'),
    ('-9223372036854775808'), ('-9223372036854775809'),
    ('99999999999999999999999999999999999999999999'), ('bad'), (''), (NULL)")
  schema <- DBI::dbGetQuery(con,
    "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema()")$nspname
  for (type in c("smallint", "integer", "bigint", "text")) {
    DBI::dbExecute(con, paste0("CREATE TEMP TABLE join_target (id ", type, ")"))
    DBI::dbExecute(con, "INSERT INTO join_target VALUES (1), (0), (-1), (NULL)")
    if (type == "bigint") DBI::dbExecute(con, "INSERT INTO join_target VALUES
      (9223372036854775807), (-9223372036854775808)")
    if (type == "text") DBI::dbExecute(con, "INSERT INTO join_target VALUES ('001'), ('bad')")
    join <- .cohort_filter_join(con, schema, "join_target", "id")
    old <- "SELECT t.* FROM join_target t JOIN _ehr_cohort c ON t.id::text = c.cohort_id"
    new <- paste("SELECT t.* FROM join_target t", join)
    diff <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM
      ((%s EXCEPT ALL %s) UNION ALL (%s EXCEPT ALL %s)) d", old, new, new, old))
    expect_equal(as.numeric(diff$n), 0, info = type)
    if (type != "text") expect_false(grepl("::text", join, fixed = TRUE))
    DBI::dbExecute(con, "DROP TABLE join_target")
  }
  DBI::dbExecute(con, "TRUNCATE _ehr_cohort")
  DBI::dbExecute(con, "INSERT INTO _ehr_cohort VALUES ('1'), ('2')")
  DBI::dbExecute(con, "CREATE TEMP TABLE join_target AS SELECT g AS id FROM generate_series(1,100000) g")
  DBI::dbExecute(con, "CREATE INDEX ON join_target(id)")
  DBI::dbExecute(con, "ANALYZE join_target")
  join <- .cohort_filter_join(con, schema, "join_target", "id")
  plan <- DBI::dbGetQuery(con, paste("EXPLAIN SELECT t.* FROM join_target t", join))[[1]]
  expect_true(any(grepl("Index.*Scan", plan)))
  DBI::dbExecute(con, "TRUNCATE _ehr_cohort")
  join <- .cohort_filter_join(con, schema, "join_target", "id")
  expect_equal(nrow(DBI::dbGetQuery(con, paste("SELECT t.* FROM join_target t", join))), 0)
})

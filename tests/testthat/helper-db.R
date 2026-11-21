# The live-DB tests write fixture tables under real schema names
# (mimic_iv_demo_coh, rendehr) and drop those schemas afterwards. Pointed at
# a database that already holds a pipeline run, that would destroy it, so
# such a test only runs when its schema is absent or empty: give EHR_TEST_DB
# a disposable database.
skip_if_schema_populated <- function(con, schema) {
  n <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM information_schema.tables WHERE table_schema = $1",
    params = list(schema)
  )$n
  if (isTRUE(as.numeric(n) > 0))
    testthat::skip(sprintf(
      "schema %s in EHR_TEST_DB already holds %s table(s); destructive DB tests run only against a disposable database",
      schema, n))
}

# Read-only tests that need a database holding a mimic_iv_demo pipeline run;
# they skip on the disposable database the destructive tests use.
skip_unless_pipeline_table <- function(schema, table) {
  testthat::skip_if_not_installed("RPostgres")
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("EHR_TEST_DB"))
  on.exit(DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, DBI::Id(schema = schema, table = table)))
    testthat::skip(sprintf("EHR_TEST_DB has no %s.%s (needs a mimic_iv_demo pipeline run)", schema, table))
}

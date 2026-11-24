# The bundled MCP server (inst/mcp): JSON-RPC protocol, argument validation,
# every tool, and the stdio transport. Database tools run only when
# EHR_TEST_MCP_CONFIG names a project configuration whose database is built;
# the server opens it read-only.

.mcp_env <- function() {
  skip_if_not_installed("jsonlite")
  root <- system.file("mcp", package = "rendehR")
  skip_if(!nzchar(root), "the MCP bundle is not installed")
  e <- new.env(parent = globalenv())
  for (f in c("mcp_discovery.R", "rendehr_adapter.R", "mcp_server.R"))
    sys.source(file.path(root, "R", f), envir = e)
  e
}

.mcp_session <- function(e) {
  state <- new.env(parent = emptyenv()); state$initialized <- FALSE; state$ready <- FALSE
  reg <- e$.rh_tools()
  send <- function(method, params = NULL, id = 1L) {
    msg <- list(jsonrpc = "2.0", id = id, method = method)
    if (!is.null(params)) msg$params <- params
    e$.mcp_dispatch(msg, state, reg)
  }
  list(state = state, reg = reg, send = send,
       start = function() {
         send("initialize", list(protocolVersion = "2025-06-18"))
         e$.mcp_dispatch(list(jsonrpc = "2.0", method = "notifications/initialized"), state, reg)
       },
       call = function(name, args = structure(list(), names = character())) {
         r <- send("tools/call", list(name = name, arguments = args), id = 9L)
         if (!is.null(r$error)) return(r)
         list(isError = r$result$isError,
              value = jsonlite::fromJSON(r$result$content[[1]]$text, simplifyVector = FALSE))
       })
}

test_that("the MCP follows the JSON-RPC handshake", {
  e <- .mcp_env(); s <- .mcp_session(e)
  expect_equal(s$send("tools/list")$error$code, -32000L)            # before initialize
  init <- s$send("initialize", list(protocolVersion = "2025-03-26"))
  expect_equal(init$result$protocolVersion, "2025-03-26")
  expect_equal(init$result$serverInfo$name, "rendehR")
  expect_equal(init$result$serverInfo$version,
               trimws(readLines(system.file("mcp", "VERSION", package = "rendehR"), n = 1)))
  expect_equal(s$send("initialize", list(protocolVersion = "2025-06-18"))$error$code, -32600L)
  expect_null(e$.mcp_dispatch(list(jsonrpc = "2.0", method = "notifications/initialized"), s$state, s$reg))
  expect_true(s$state$ready)
  tools <- vapply(s$send("tools/list")$result$tools, `[[`, "", "name")
  expect_setequal(tools, c("rendehr_status", "check_setup", "search_concept_definitions", "draft_config_row",
                           "list_topics", "resolve_term", "ehr_find", "list_loaded_concepts", "cohort_summary"))
  for (t in s$send("tools/list")$result$tools) {
    expect_true(isTRUE(t$annotations$readOnlyHint), info = t$name)
    expect_false(isTRUE(t$annotations$destructiveHint), info = t$name)
  }
  expect_equal(s$send("no/such")$error$code, -32601L)
  expect_equal(s$send("ping")$result, structure(list(), names = character()))
  expect_equal(e$.mcp_dispatch(list(id = 1, method = "ping"), s$state, s$reg)$error$code, -32600L)
})

test_that("tool arguments are validated before anything runs", {
  e <- .mcp_env(); s <- .mcp_session(e); s$start()
  err <- function(name, args) s$call(name, args)$error$message
  expect_match(err("nope", list()), "Unknown tool")
  expect_match(err("search_concept_definitions", list()), "Missing required")
  expect_match(err("search_concept_definitions", list(term = "x", what = 1)), "Unknown arguments")
  expect_match(err("search_concept_definitions", list(term = "x", limit = 0)), "Below minimum")
  expect_match(err("search_concept_definitions", list(term = "x", limit = 101)), "Above maximum")
  expect_match(err("search_concept_definitions", list(term = "x", limit = 1.5)), "Invalid argument")
  expect_match(err("search_concept_definitions", list(term = "  ")), "Invalid argument")
  expect_match(err("search_concept_definitions", list(term = "x", src = "other_db")), "Unsupported value")
  expect_match(err("resolve_term", list(term = list())), "Invalid argument")
  expect_match(err("resolve_term", list(term = as.list(letters[1:21]))), "Invalid argument")
})

test_that("the offline tools answer from the installed catalogue", {
  e <- .mcp_env(); s <- .mcp_session(e); s$start()
  st <- s$call("rendehr_status")$value
  expect_true(st$installed); expect_equal(st$version, as.character(utils::packageVersion("rendehR")))
  expect_false(st$database_tested)

  hr <- s$call("search_concept_definitions", list(term = "heart rate", src = "mimic_iv_demo", limit = 5))$value
  expect_gt(hr$total, 0)
  expect_true(all(vapply(hr$rows, `[[`, "", "source") == "mimic_iv"))
  page <- s$call("search_concept_definitions", list(term = "a", limit = 2, offset = 2))$value
  expect_equal(page$offset, 2); expect_length(page$rows, 2)

  topics <- s$call("list_topics", list(limit = 100))$value
  expect_true(all(c("hr", "map") %in% unlist(topics$topics)))
  rt <- s$call("resolve_term", list(term = "hr"))$value
  expect_true("heart rate" %in% unlist(rt$search_terms))
  expect_false("cerebral perfusion pressure" %in% unlist(rt$search_terms))  # cached topics are opt-in

  draft <- s$call("draft_config_row", list(template_concept = "lactate", src = "mimic_iv_demo", event_table = "labevents",
                                           concept_name = "lactate_whole_blood", match_value = "52442", description = "lactate"))
  expect_false(draft$isError)
  row <- utils::read.csv(text = draft$value$csv, colClasses = "character")
  cfg <- utils::read.csv(system.file("extdata", "rendehr_concepts_config.csv", package = "rendehR"), colClasses = "character")
  expect_identical(names(row), names(cfg))                              # every current column
  expect_equal(row$concept_name, "lactate_whole_blood"); expect_equal(row$match_value, "52442")
  for (f in intersect(c("unit", "min_value", "max_value", "ref", "required_dedup", "dedup_cross_reference"), names(row)))
    expect_true(is.na(row[[f]]) || row[[f]] %in% c("", "NA"), info = f)  # left for review
  dup <- s$call("draft_config_row", list(template_concept = "lactate", src = "mimic_iv_demo", event_table = "labevents",
                                         concept_name = "lactate", match_value = "1", description = "x"))
  expect_true(dup$isError); expect_match(dup$value$error, "already exists")
})

test_that("tool arguments must be an object and numbers keep full precision", {
  e <- .mcp_env()
  state <- new.env(parent = emptyenv()); state$initialized <- TRUE; state$ready <- TRUE
  registry <- list(num = list(
    description = "test", inputSchema = list(type = "object", properties = list(),
                                             required = list()),
    annotations = list(),
    fn = function(a) list(value = 123.456789)))
  bad <- e$.mcp_dispatch(list(jsonrpc = "2.0", id = 1L, method = "tools/call",
                              params = list(name = "num", arguments = list(1, 2))),
                         state, registry)
  expect_equal(bad$error$code, -32602L)
  expect_match(bad$error$message, "arguments must be an object")
  ok <- e$.mcp_dispatch(list(jsonrpc = "2.0", id = 2L, method = "tools/call",
                             params = list(name = "num", arguments = list())),
                        state, registry)
  expect_match(ok$result$content[[1]]$text, "123.456789", fixed = TRUE)
})

test_that("short search aliases match whole words only", {
  e <- .mcp_env()
  expect_true(e$.rh_term_matches("hr", "hr (bpm)"))
  expect_true(e$.rh_term_matches("hr", "heart rate hr"))
  expect_false(e$.rh_term_matches("hr", "0.9% nacl (ml/hr)"))
  expect_false(e$.rh_term_matches("hr", "shrimp"))
  expect_false(e$.rh_term_matches("map", "mapping"))
  expect_true(e$.rh_term_matches("heart rate", "heart rate (bpm)"))
  expect_true(e$.rh_term_matches("c.d", "a c.d b"))
  expect_false(e$.rh_term_matches("c.d", "a cxd b"))                   # regex characters are literal
})

test_that("database tools refuse to run without a reviewed configuration", {
  e <- .mcp_env(); s <- .mcp_session(e); s$start()
  withr::local_envvar(EHR_CONFIG = "")
  r <- s$call("cohort_summary", list(src = "mimic_iv_demo"))
  expect_true(r$isError); expect_match(r$value$error, "EHR_CONFIG")
})

test_that("the stdio transport writes only JSON-RPC to stdout", {
  launcher <- system.file("mcp", "inst", "scripts", "rendehr_mcp.R", package = "rendehR")
  input <- tempfile(fileext = ".jsonl")
  writeLines(c(
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}',
    '{"jsonrpc":"2.0","method":"notifications/initialized"}',
    'this is not json',
    '',
    '{"jsonrpc":"2.0","id":2,"method":"tools/list"}',
    '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"resolve_term","arguments":{"term":"map"}}}'), input)
  out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(launcher)), stdin = input,
                 stdout = TRUE, stderr = FALSE, env = paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep)))
  msgs <- lapply(out, jsonlite::fromJSON, simplifyVector = FALSE)        # every line parses
  expect_length(msgs, 4)
  expect_equal(msgs[[1]]$result$serverInfo$name, "rendehR")
  expect_equal(msgs[[2]]$error$code, -32700L)                            # the bad line, reported
  expect_length(msgs[[3]]$result$tools, 9)
  expect_false(msgs[[4]]$result$isError)
})

test_that("database tools read a built project read-only", {
  cfg <- Sys.getenv("EHR_TEST_MCP_CONFIG")
  skip_if(!nzchar(cfg), "set EHR_TEST_MCP_CONFIG to a project configuration with a built database")
  skip_if_not_installed("RPostgres")
  e <- .mcp_env(); s <- .mcp_session(e); s$start()
  withr::local_envvar(EHR_CONFIG = cfg)
  # the tools activate the project configuration; restore the session's afterwards
  prev <- rendehR:::.ehr_env$config
  on.exit(assign("config", prev, envir = rendehR:::.ehr_env), add = TRUE)
  expect_true(s$call("check_setup", list(config_path = cfg))$value$ok)
  src <- Sys.getenv("EHR_TEST_MCP_SOURCE", "mimic_iv_demo")
  cs <- s$call("cohort_summary", list(src = src))
  expect_false(cs$isError)
  expect_gt(cs$value$counts$flat_rows, 0)
  sex_n <- sum(unlist(cs$value$counts$sex))
  expect_gt(cs$value$counts$flat_patients, 0)
  expect_identical(as.numeric(sex_n), as.numeric(cs$value$counts$flat_patients))
  expect_gte(sex_n, cs$value$counts$patients); expect_lte(sex_n, cs$value$counts$flat_rows)
  expect_match(cs$value$report, "sex (flat patients", fixed = TRUE)
  lc <- s$call("list_loaded_concepts", list(src = src, limit = 5))$value
  expect_length(lc$rows, 5)
  expect_gte(lc$total, 5)
  f <- s$call("ehr_find", list(term = "lactate", src = src, limit = 10))$value
  expect_gt(length(f$candidates), 0)
  expect_true(all(grepl("lactate", tolower(vapply(f$candidates, `[[`, "", "label")))))
  expect_null(f$candidates[[1]][["_row"]])                              # no row names leak into results
  # the session is read-only: a write through the same path fails
  expect_error(e$.rh_db(function(con) DBI::dbExecute(con, "CREATE TABLE mcp_write_probe (x int)")), "read-only")
})

test_that("the bundled skills name only exported functions and their templates parse", {
  skills <- system.file("mcp", "skills", package = "rendehR")
  skip_if(!nzchar(skills), "the skills are not installed")
  expect_setequal(list.dirs(skills, recursive = FALSE, full.names = FALSE),
                  c("rendehr-user", "rendehr-add-concept", "rendehr-add-source"))
  files <- list.files(skills, pattern = "\\.(md|R)$", recursive = TRUE, full.names = TRUE)
  for (f in files[grepl("\\.R$", files)]) expect_no_error(parse(f))
  exports <- getNamespaceExports("rendehR")
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    named <- unique(sub("^rendehR::", "", regmatches(txt, gregexpr("rendehR::[A-Za-z_.][A-Za-z0-9_.]*", txt))[[1]]))
    expect_true(all(named %in% exports), info = paste(basename(f), ":", paste(setdiff(named, exports), collapse = ", ")))
  }
})

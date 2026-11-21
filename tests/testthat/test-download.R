test_that("download_sources validates src_names", {
  expect_error(download_sources(character()), "src_names must contain")
})

test_that("get_cred follows the accepted canonical credential contract", {
  old <- Sys.getenv(c("PHYSIONET_USER", "RICU_PHYSIONET_USER"), unset = NA)
  on.exit({
    for (nm in names(old)) {
      if (is.na(old[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, setNames(list(old[[nm]]), nm))
    }
  })
  Sys.setenv(PHYSIONET_USER = "canonical-user",
             RICU_PHYSIONET_USER = "retired-user")
  expect_equal(get_cred(NULL, "PHYSIONET_USER", "unused"), "canonical-user")
  expect_error(get_cred("", "UNSET_S002_CRED", "unused"), "Valid PhysioNet")
  expect_error(get_cred(NA_character_, "UNSET_S002_CRED", "unused"), "Valid PhysioNet")
  expect_error(get_cred("YOUR_PHYSIONET_USERNAME", "UNSET_S002_CRED", "unused"),
               "Valid PhysioNet")
})

test_that("SHA256SUMS parser accepts standard text and binary formats", {
  hash_a <- paste(rep("a", 64), collapse = "")
  hash_b <- paste(rep("b", 64), collapse = "")
  local_mocked_bindings(
    download_physionet_file = function(...) {
      charToRaw(paste0(hash_a, "  hosp/a.csv.gz\n",
                       hash_b, " *icu/b.csv.gz\n"))
    }
  )
  got <- get_sha256("https://example.invalid/release")
  expect_equal(unname(got), c(hash_a, hash_b))
  expect_equal(names(got), c("hosp/a.csv.gz", "icu/b.csv.gz"))
})

test_that("SHA256 verification is exact", {
  path <- tempfile()
  writeBin(charToRaw("approved artifact"), path)
  con <- file(path, "rb")
  on.exit(close(con))
  expected <- as.character(openssl::sha256(con))
  expect_true(.ehr_verify_sha256(path, expected))
  expect_false(.ehr_verify_sha256(path, paste(rep("0", 64), collapse = "")))
})

test_that("load_src_config accepts only one canonical catalog", {
  catalog <- system.file("extdata", "rendehr-data-source.json", package = "rendehR")
  if (!nzchar(catalog)) {
    catalog <- file.path(test_path("..", ".."), "inst", "extdata",
                         "rendehr-data-source.json")
  }
  got <- load_src_config("mimic_iv", catalog)
  expect_equal(got$release, "3.1")
  expect_error(load_src_config("mimic_iv", c(catalog, catalog)),
               "Exactly one canonical")
})

test_that("verified staging publication is checksum gated and non-destructive", {
  root <- tempfile()
  dir.create(root)
  staged <- file.path(root, "table.csv.gz.part")
  final <- file.path(root, "table.csv.gz")
  writeBin(charToRaw("artifact"), staged)
  con <- file(staged, "rb")
  expected <- as.character(openssl::sha256(con))
  close(con)

  expect_invisible(.ehr_publish_verified_download(
    staged, final, expected, file_type_ok = function(path) TRUE
  ))
  expect_true(file.exists(final))
  expect_false(file.exists(staged))

  staged_bad <- paste0(final, ".part")
  writeBin(charToRaw("different"), staged_bad)
  expect_error(
    .ehr_publish_verified_download(
      staged_bad, paste0(final, ".new"), expected,
      file_type_ok = function(path) TRUE
    ),
    "SHA-256"
  )
  expect_true(file.exists(staged_bad))
})

.s002_sha_text <- function(text) {
  path <- tempfile()
  on.exit(unlink(path))
  writeBin(charToRaw(text), path)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  as.character(openssl::sha256(con))
}

.s002_head_response <- function(size) {
  list(
    status_code = 200L,
    headers = charToRaw(paste0(
      "HTTP/1.1 200 OK\r\nContent-Length: ", size, "\r\n\r\n"
    ))
  )
}

test_that("strict manifest downloads, publishes, skips, and accepts moved bytes", {
  payload <- "id,value\n1,ok\n"
  file <- "nested/table.csv"
  expected <- .s002_sha_text(payload)
  root <- tempfile()
  stream_calls <- 0L

  local_mocked_bindings(
    get_sha256 = function(...) stats::setNames(expected, file),
    download_physionet_file = function(..., head_only = FALSE) {
      expect_true(head_only)
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      stream_calls <<- stream_calls + 1L
      fun(charToRaw(payload))
      list(status_code = 200L)
    }
  )

  expect_invisible(download_check_data(
    root, file, "https://example.invalid/release", "example", verbose = FALSE
  ))
  final <- file.path(root, file)
  expect_true(file.exists(final))
  expect_false(file.exists(paste0(final, ".part")))
  expect_equal(readLines(final), c("id,value", "1,ok"))
  expect_equal(stream_calls, 1L)

  before <- file.info(final)[, c("size", "mtime"), drop = FALSE]
  expect_message(
    download_check_data(
      root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    ),
    "Skipping 1 file"
  )
  after <- file.info(final)[, c("size", "mtime"), drop = FALSE]
  expect_identical(before, after)
  expect_equal(stream_calls, 1L)

  moved_root <- tempfile()
  dir.create(file.path(moved_root, dirname(file)), recursive = TRUE)
  expect_true(file.copy(final, file.path(moved_root, file)))
  expect_message(
    download_check_data(
      moved_root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    ),
    "release-bound SHA-256 verified"
  )
  expect_equal(stream_calls, 1L)
})

test_that("strict manifest preserves corrupt existing and staged files", {
  payload <- "id,value\n1,ok\n"
  file <- "table.csv"
  expected <- .s002_sha_text(payload)
  root <- tempfile()
  dir.create(root)
  final <- file.path(root, file)
  writeLines("wrong bytes", final)

  local_mocked_bindings(
    get_sha256 = function(...) stats::setNames(expected, file),
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(...) {
      fail("artifact download must not overwrite a corrupt final file")
    }
  )

  expect_error(
    download_check_data(
      root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    ),
    "does not match"
  )
  expect_equal(readLines(final), "wrong bytes")

  unlink(final)
  local_mocked_bindings(
    get_sha256 = function(...) {
      stats::setNames(paste(rep("0", 64), collapse = ""), file)
    },
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      fun(charToRaw(payload))
      list(status_code = 200L)
    }
  )
  expect_error(
    download_check_data(
      root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    ),
    "SHA-256"
  )
  expect_false(file.exists(final))
  expect_true(file.exists(paste0(final, ".part")))
})

test_that("complete and partial staging files are safely resumed or published", {
  payload <- "id,value\n1,ok\n"
  partial <- substr(payload, 1L, 5L)
  remainder <- substr(payload, 6L, nchar(payload))
  file <- "table.csv"
  expected <- .s002_sha_text(payload)
  root <- tempfile()
  dir.create(root)
  staged <- file.path(root, paste0(file, ".part"))
  writeBin(charToRaw(partial), staged)

  local_mocked_bindings(
    get_sha256 = function(...) stats::setNames(expected, file),
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      fun(charToRaw(remainder))
      list(status_code = 206L)
    }
  )

  expect_invisible(download_check_data(
    root, file, "https://example.invalid/release", "example", verbose = FALSE
  ))
  expect_equal(readLines(file.path(root, file)), c("id,value", "1,ok"))
  expect_false(file.exists(staged))

  second_root <- tempfile()
  dir.create(second_root)
  second_staged <- file.path(second_root, paste0(file, ".part"))
  writeBin(charToRaw(payload), second_staged)
  local_mocked_bindings(
    get_sha256 = function(...) stats::setNames(expected, file),
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(...) {
      fail("a complete staged file must be validated without downloading")
    }
  )
  expect_message(
    download_check_data(
      second_root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    ),
    "complete by exact size"
  )
  expect_true(file.exists(file.path(second_root, file)))
  expect_false(file.exists(second_staged))
})

test_that("explicitly absent manifest uses disclosed exact-size fallback", {
  payload <- "id,value\n1,ok\n"
  file <- "nested/table.csv"
  root <- tempfile()
  stream_calls <- 0L

  local_mocked_bindings(
    get_sha256 = function(...) {
      stop("HTTP 404 for https://example.invalid/release/SHA256SUMS.txt")
    },
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      stream_calls <<- stream_calls + 1L
      fun(charToRaw(payload))
      list(status_code = 200L)
    }
  )

  expect_warning(
    expect_invisible(download_check_data(
      root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    )),
    "not be cryptographically verified"
  )
  final <- file.path(root, file)
  expect_true(file.exists(final))
  expect_false(file.exists(paste0(final, ".part")))
  expect_equal(stream_calls, 1L)

  before <- file.info(final)[, c("size", "mtime"), drop = FALSE]
  expect_warning(
    expect_message(
      download_check_data(
        root, file, "https://example.invalid/release", "example",
        verbose = FALSE
      ),
      "exact size and file format"
    ),
    "not be cryptographically verified"
  )
  expect_identical(before, file.info(final)[, c("size", "mtime"), drop = FALSE])
  expect_equal(stream_calls, 1L)
  expect_true(.ehr_manifest_is_absent(
    simpleError("HTTP 410 for https://example.invalid/SHA256SUMS.txt")
  ))
})

test_that("HTTP 410 executes the same disclosed manifest-absent fallback", {
  payload <- "id,value\n1,ok\n"
  file <- "table.csv"
  root <- tempfile()

  local_mocked_bindings(
    get_sha256 = function(...) {
      stop("HTTP 410 for https://example.invalid/release/SHA256SUMS.txt")
    },
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      fun(charToRaw(payload))
      list(status_code = 200L)
    }
  )

  expect_warning(
    expect_invisible(download_check_data(
      root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    )),
    "HTTP 410"
  )
  expect_equal(readLines(file.path(root, file)), c("id,value", "1,ok"))
  expect_false(file.exists(file.path(root, paste0(file, ".part"))))
})

test_that("manifest access denial retries canonical credentials without downgrade", {
  payload <- "id,value\n1,ok\n"
  file <- "table.csv"
  expected <- .s002_sha_text(payload)
  root <- tempfile()
  attempts <- 0L
  old <- Sys.getenv(c("PHYSIONET_USER", "PHYSIONET_PASS"), unset = NA)
  on.exit({
    for (nm in names(old)) {
      if (is.na(old[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, setNames(list(old[[nm]]), nm))
      }
    }
  })
  Sys.setenv(PHYSIONET_USER = "canonical-user",
             PHYSIONET_PASS = "canonical-pass")

  local_mocked_bindings(
    get_sha256 = function(url, user = NULL, pass = NULL) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        stop("Access denied. Set up account at https://physionet.org/")
      }
      expect_equal(user, "canonical-user")
      expect_equal(pass, "canonical-pass")
      stats::setNames(expected, file)
    },
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      fun(charToRaw(payload))
      list(status_code = 200L)
    }
  )

  expect_invisible(download_check_data(
    root, file, "https://example.invalid/release", "example", verbose = FALSE
  ))
  expect_equal(attempts, 2L)
  expect_true(file.exists(file.path(root, file)))
})

test_that("manifest HTTP 401 and 403 are both classified as access denial", {
  for (status in c(401L, 403L)) {
    local_mocked_bindings(
      download_file = function(...) list(status_code = status)
    )
    expect_error(
      download_physionet_file(
        "https://example.invalid/release/SHA256SUMS.txt"
      ),
      "Access denied"
    )
  }
})

test_that("manifest-absent size mismatch preserves existing and staged files", {
  payload <- "id,value\n1,ok\n"
  file <- "table.csv"
  root <- tempfile()
  dir.create(root)
  final <- file.path(root, file)
  writeLines("short", final)

  local_mocked_bindings(
    get_sha256 = function(...) {
      stop("HTTP 404 for https://example.invalid/release/SHA256SUMS.txt")
    },
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(...) {
      fail("an invalid existing file must not be overwritten")
    }
  )

  expect_warning(
    expect_error(
      download_check_data(
        root, file, "https://example.invalid/release", "example",
        verbose = FALSE
      ),
      "size differs"
    ),
    "not be cryptographically verified"
  )
  expect_equal(readLines(final), "short")

  unlink(final)
  short_payload <- substr(payload, 1L, nchar(payload) - 1L)
  local_mocked_bindings(
    get_sha256 = function(...) {
      stop("HTTP 404 for https://example.invalid/release/SHA256SUMS.txt")
    },
    download_physionet_file = function(..., head_only = FALSE) {
      .s002_head_response(nchar(payload, type = "bytes"))
    },
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      fun(charToRaw(short_payload))
      list(status_code = 200L)
    }
  )

  expect_warning(
    expect_error(
      download_check_data(
        root, file, "https://example.invalid/release", "example",
        verbose = FALSE
      ),
      "size differs"
    ),
    "not be cryptographically verified"
  )
  expect_false(file.exists(final))
  expect_true(file.exists(paste0(final, ".part")))
})

test_that("manifest fallback refuses unsafe downgrade and size ambiguity", {
  file <- "table.csv"
  root <- tempfile()

  messages <- c(
    "HTTP 500 for https://example.invalid/release/SHA256SUMS.txt",
    "Malformed SHA256SUMS.txt manifest.",
    "network timeout retrieving SHA256SUMS.txt"
  )
  patterns <- c("HTTP 500", "Malformed SHA256SUMS", "network timeout")
  for (i in seq_along(messages)) {
    message <- messages[[i]]
    local_mocked_bindings(
      get_sha256 = function(...) stop(message)
    )
    expect_error(
      download_check_data(
        root, file, "https://example.invalid/release", "example",
        verbose = FALSE
      ),
      patterns[[i]]
    )
  }

  local_mocked_bindings(
    get_sha256 = function(...) stats::setNames(.s002_sha_text("x"), "other.csv")
  )
  expect_error(
    download_check_data(
      root, file, "https://example.invalid/release", "example",
      verbose = FALSE
    ),
    "Files not in SHA256SUMS"
  )

  local_mocked_bindings(
    get_sha256 = function(...) {
      stop("HTTP 404 for https://example.invalid/release/SHA256SUMS.txt")
    },
    download_physionet_file = function(..., head_only = FALSE) {
      list(status_code = 200L,
           headers = charToRaw("HTTP/1.1 200 OK\r\n\r\n"))
    }
  )
  expect_warning(
    expect_error(
      download_check_data(
        root, file, "https://example.invalid/release", "example",
        verbose = FALSE
      ),
      "exact positive Content-Length"
    ),
    "not be cryptographically verified"
  )
})

test_that("refused PhysioNet credentials can be entered again interactively", {
  payload <- "id,value\n1,ok\n"
  file <- "table.csv"
  expected <- .s002_sha_text(payload)
  withr::local_envvar(PHYSIONET_USER = "stored-user", PHYSIONET_PASS = "wrong-pass")
  seen <- character()
  answers <- c("typed-user", "typed-pass")
  local_mocked_bindings(
    is_interactive = function() TRUE,
    .ehr_readline = function(prompt) { a <- answers[1]; answers <<- answers[-1]; a },
    get_sha256 = function(url, user = NULL, pass = NULL) {
      seen <<- c(seen, paste(user, pass))
      if (!identical(pass, "typed-pass")) stop("Access denied. Set up account at https://physionet.org/")
      stats::setNames(expected, file)
    },
    download_physionet_file = function(..., head_only = FALSE) .s002_head_response(nchar(payload, type = "bytes")),
    .ehr_curl_fetch_stream = function(url, fun, handle) { fun(charToRaw(payload)); list(status_code = 200L) }
  )
  root <- tempfile()
  expect_message(download_check_data(root, file, "https://example.invalid/release", "example", verbose = FALSE),
                 "Enter them again")
  expect_equal(tail(seen, 1), "typed-user typed-pass")
  expect_true(file.exists(file.path(root, file)))
})

test_that("refused PhysioNet credentials stop with a clear message", {
  withr::local_envvar(PHYSIONET_USER = "stored-user", PHYSIONET_PASS = "wrong-pass")
  local_mocked_bindings(
    get_sha256 = function(...) stop("Access denied. Set up account at https://physionet.org/")
  )
  # outside an interactive session: no prompt, an actionable error
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_error(download_check_data(tempfile(), "t.csv", "https://example.invalid/release", "example", verbose = FALSE),
               "data use agreement")
  # interactive, but the user gives up with an empty username
  local_mocked_bindings(is_interactive = function() TRUE, .ehr_readline = function(prompt) "")
  expect_error(suppressMessages(download_check_data(tempfile(), "t.csv", "https://example.invalid/release", "example",
                                                    verbose = FALSE)), "PhysioNet denied access")
})

test_that("download_source prefers PHYSIONET_USER/PASS over stored credentials, like load_credentials", {
  withr::local_envvar(PHYSIONET_USER = "env-user", PHYSIONET_PASS = "env-pass")
  old <- .ehr_env$creds
  on.exit(.ehr_env$creds <- old, add = TRUE)
  .ehr_env$creds <- list(physionet = list(username = "file-user", password = "file-pass"))
  got <- NULL
  local_mocked_bindings(download_check_data = function(..., user, pass) got <<- c(user, pass))
  download_source("mimic_iv_demo", tables = "admissions", raw_data_root = withr::local_tempdir())
  expect_equal(got, c("env-user", "env-pass"))
  withr::local_envvar(PHYSIONET_USER = NA, PHYSIONET_PASS = NA)
  download_source("mimic_iv_demo", tables = "admissions", raw_data_root = withr::local_tempdir())
  expect_equal(got, c("file-user", "file-pass"))
})

test_that("a 502 error page is not kept in the download, and the retry succeeds", {
  payload <- "id,value\n1,ok\n"
  file <- "table.csv"
  expected <- .s002_sha_text(payload)
  root <- tempfile()
  calls <- 0L
  local_mocked_bindings(
    get_sha256 = function(...) stats::setNames(expected, file),
    download_physionet_file = function(..., head_only = FALSE) .s002_head_response(nchar(payload, type = "bytes")),
    .ehr_curl_fetch_stream = function(url, fun, handle) {
      calls <<- calls + 1L
      if (calls == 1L) { fun(charToRaw("<html>502 Bad Gateway</html>")); return(list(status_code = 502L)) }
      fun(charToRaw(payload)); list(status_code = 200L)
    }
  )
  local_mocked_bindings(Sys.sleep = function(...) invisible(NULL), .package = "base")
  suppressMessages(download_check_data(root, file, "https://example.invalid/release", "example", verbose = FALSE))
  expect_equal(calls, 2L)
  expect_equal(readLines(file.path(root, file)), c("id,value", "1,ok"))
})

test_that("staged bytes are trimmed to a prefix, or replaced by a fresh full copy", {
  f <- withr::local_tempfile()
  writeBin(charToRaw("HEADERpartial<html>err</html>"), f)
  .ehr_drop_staged_bytes(f, keep = 13)
  expect_identical(rawToChar(readBin(f, "raw", 100)), "HEADERpartial")
  writeBin(charToRaw("oldpartFULLFILE"), f)
  .ehr_drop_staged_bytes(f, keep = 0, from = 7)
  expect_identical(rawToChar(readBin(f, "raw", 100)), "FULLFILE")
})

# Download utilities for PhysioNet data sources.

# ___________________________________________________________________________
# Internal helpers
# ___________________________________________________________________________

`%||%` <- function(x, y) if (is.null(x)) y else x

is_interactive <- function() {
  !isTRUE(getOption("knitr.in.progress")) && interactive()
}

# readline() behind a seam, so the credential prompts can be tested.
.ehr_readline <- function(prompt) readline(prompt)

ensure_dirs <- function(paths) {
  uq_paths  <- unique(paths)
  is_dir    <- file.info(uq_paths, extra_cols = FALSE)[["isdir"]]
  is_no_dir <- vapply(is_dir, identical, logical(1), FALSE)
  if (any(is_no_dir))
    stop("Paths exist but are not directories: ",
         paste(uq_paths[is_no_dir], collapse = ", "))
  dirs_to_create <- uq_paths[is.na(is_dir)]
  if (length(dirs_to_create) > 0L) {
    res <- vapply(dirs_to_create, dir.create, logical(1), recursive = TRUE)
    if (!all(res))
      stop("Could not create directories: ",
           paste(dirs_to_create[!res], collapse = ", "))
  }
  invisible(paths)
}

parse_headers <- function(headers) {
  if (is.raw(headers)) headers <- rawToChar(headers)
  strsplit(headers, "\r\n")[[1]]
}

warn_dots <- function(..., ok_args = NULL) {
  if (...length() > 0L) {
    args <- setdiff(names(match.call(expand.dots = FALSE)$`...`), ok_args)
    if (length(args) > 0)
      warning("Ignoring unexpected argument(s): ",
              paste(args, collapse = ", "), call. = FALSE)
  }
  invisible(NULL)
}

get_cred <- function(x, env, msg) {
  x <- .ehr_first_credential(x, Sys.getenv(env, unset = NA_character_))
  if (is.null(x) && is_interactive()) {
    x <- readline(msg)
  }
  if (.ehr_credential_missing(x)) {
    stop("Valid PhysioNet credentials are required. Configure canonical ",
         "PHYSIONET_USER and PHYSIONET_PASS values or the accepted ",
         "credentials file.", call. = FALSE)
  }
  x
}

format_bytes <- function(bytes) {
  if (is.na(bytes) || bytes <= 0) return("???")
  if (bytes >= 1e9) return(sprintf("%.1f GB", bytes / 1e9))
  if (bytes >= 1e6) return(sprintf("%.1f MB", bytes / 1e6))
  if (bytes >= 1e3) return(sprintf("%.1f KB", bytes / 1e3))
  paste0(bytes, " B")
}

# ___________________________________________________________________________
# Core PhysioNet download functions
# ___________________________________________________________________________

download_file <- function(url, handle = curl::new_handle(), dest = NULL) {
  if (is.null(dest)) return(curl::curl_fetch_memory(url, handle))
  curl::curl_fetch_disk(url, dest, handle = handle)
}

.ehr_curl_fetch_stream <- function(url, fun, handle) {
  curl::curl_fetch_stream(url, fun, handle = handle)
}

# Rewrite a staged download keeping its first `keep` bytes and, when `from`
# is given, the bytes from offset `from` on (appended after the kept prefix).
.ehr_drop_staged_bytes <- function(path, keep, from = NULL) {
  if (!file.exists(path)) return(invisible(NULL))
  size <- as.double(file.info(path)$size)
  tmp <- paste0(path, ".tmp")
  inp <- file(path, "rb"); out <- file(tmp, "wb")
  copy <- function(n) while (n > 0) {
    chunk <- readBin(inp, "raw", n = min(n, 8 * 1024^2))
    if (!length(chunk)) break
    writeBin(chunk, out); n <- n - length(chunk)
  }
  copy(min(keep, size))
  if (!is.null(from) && from < size) { seek(inp, from); copy(size - from) }
  close(inp); close(out)
  file.rename(tmp, path)
  invisible(NULL)
}


download_physionet_file <- function(url, dest = NULL, user = NULL,
                                    pass = NULL, head_only = FALSE) {
  h <- curl::new_handle(useragent = "Wget/")
  if (!is.null(user) && !is.null(pass))
    curl::handle_setopt(h, username = user, password = pass)
  if (is.null(dest) && head_only)
    curl::handle_setopt(h, nobody = TRUE)

  res    <- download_file(url, h, dest)
  status <- res[["status_code"]]

  if (status == 304L) {
    message("Skipped download of ", basename(url))
    return(invisible(NULL))
  } else if (status %in% c(401L, 403L)) {
    stop("Access denied. Set up account at https://physionet.org/", call. = FALSE)
  } else if (status != 200L) {
    stop("HTTP ", status, " for ", url, call. = FALSE)
  }

  if (head_only) res
  else if (is.null(dest)) res[["content"]]
  else invisible(NULL)
}

get_sha256 <- function(url, user = NULL, pass = NULL) {
  res <- download_physionet_file(
    paste(url, "SHA256SUMS.txt", sep = "/"),
    dest = NULL, user = user, pass = pass
  )
  con <- rawConnection(res)
  on.exit(close(con))
  lines <- readLines(con, warn = FALSE)
  pattern <- "^\\s*([[:xdigit:]]{64})\\s+\\*?(.+?)\\s*$"
  matched <- grepl(pattern, lines, perl = TRUE)
  if (!length(lines) || any(!matched)) {
    stop("Malformed SHA256SUMS.txt manifest.", call. = FALSE)
  }
  hashes <- tolower(sub(pattern, "\\1", lines, perl = TRUE))
  files <- sub(pattern, "\\2", lines, perl = TRUE)
  files <- sub("^\\./", "", files)
  if (any(!nzchar(files)) || anyDuplicated(files)) {
    stop("SHA256SUMS.txt contains missing or duplicate filenames.",
         call. = FALSE)
  }
  stats::setNames(hashes, files)
}

.ehr_http_status <- function(error) {
  msg <- conditionMessage(error)
  matched <- regexec("^HTTP ([0-9]{3}) for ", msg, perl = TRUE)
  parts <- regmatches(msg, matched)[[1L]]
  if (length(parts) != 2L) return(NA_integer_)
  as.integer(parts[[2L]])
}

.ehr_manifest_is_absent <- function(error) {
  .ehr_http_status(error) %in% c(404L, 410L)
}

.ehr_manifest_access_denied <- function(error) {
  grepl("^Access denied\\.", conditionMessage(error))
}

.ehr_verify_sha256 <- function(path, expected) {
  if (!file.exists(path)) return(FALSE)
  con <- file(path, open = "rb")
  on.exit(close(con))
  actual <- as.character(openssl::sha256(con))
  isTRUE(tolower(actual) == tolower(expected))
}

.ehr_publish_verified_download <- function(staged, final, expected,
                                           file_type_ok = function(path) TRUE) {
  if (!file.exists(staged)) {
    stop("Downloaded staging file is missing: ", staged, call. = FALSE)
  }
  if (!isTRUE(file_type_ok(staged))) {
    stop("Downloaded staging file failed file-type validation: ", staged,
         ". It was preserved.", call. = FALSE)
  }
  has_checksum <- length(expected) == 1L && !is.na(expected) &&
    nzchar(expected)
  if (has_checksum && !.ehr_verify_sha256(staged, expected)) {
    stop("Downloaded staging file failed release-bound SHA-256 validation: ",
         staged, ". It was preserved.", call. = FALSE)
  }
  if (file.exists(final)) {
    stop("Refusing to replace an existing final file: ", final,
         call. = FALSE)
  }
  if (!file.rename(staged, final)) {
    stop("Verified staging file could not be published: ", staged,
         call. = FALSE)
  }
  invisible(final)
}


# ___________________________________________________________________________
# download_check_data - resumable, progress-bar, checksum-verified download
# ___________________________________________________________________________

download_check_data <- function(dest_folder, files, url, src,
                                user = NULL, pass = NULL, verbose = TRUE, ...) {

  get_size <- function(file_url) {
    tryCatch({
      resp    <- download_physionet_file(file_url, NULL, user, pass, head_only = TRUE)
      hdrs    <- parse_headers(resp$headers)
      starts  <- grep("^HTTP/", hdrs)
      block   <- if (length(starts) > 0) hdrs[seq(starts[length(starts)], length(hdrs))] else hdrs
      cl      <- grep("^Content-Length:", block, ignore.case = TRUE, value = TRUE)
      if (length(cl) >= 1L) {
        val <- as.numeric(sub("^Content-Length:\\s*", "", cl[1L], ignore.case = TRUE))
        if (!is.na(val) && val > 0) return(val)
      }
      NA_real_
    }, error = function(e) NA_real_)
  }

  dl_one <- function(file_url, total_size, path,
                     max_retries = 5L, idle_timeout_sec = 600L) {
    fname <- basename(file_url)

    file_complete <- function() {
      on_disk <- if (file.exists(path)) as.double(file.info(path)$size) else 0
      if (!is.na(total_size) && total_size > 0)
        on_disk >= total_size
      else
        FALSE
    }

    # A staged .gz that does not start like gzip (e.g. an error page saved by
    # an earlier run) cannot be resumed; start that file over.
    if (grepl("\\.gz$", fname) && file.exists(path) && file.info(path)$size >= 2) {
      head <- readBin(path, "raw", n = 2L)
      if (!identical(head, as.raw(c(0x1f, 0x8b)))) {
        message("  ", fname, "  staged copy is not gzip data; downloading it again.")
        unlink(path)
      }
    }

    if (file_complete()) {
      message("  ", fname, "  staged file is complete by exact size; ",
              "validating before publication.")
      return(invisible(NULL))
    }

    t_start_all <- Sys.time()
    success     <- FALSE

    for (attempt in seq_len(max_retries)) {
      on_disk  <- if (file.exists(path)) as.double(file.info(path)$size) else 0
      resuming <- on_disk > 0

      if (attempt == 1L) {
        if (resuming)
          message("  ", fname, "  resuming from ", format_bytes(on_disk),
                  " / ", format_bytes(total_size))
        else
          message("  ", fname, "  [", format_bytes(total_size), "]")
      } else {
        message("  ", fname, "  retry ", attempt, "/", max_retries,
                if (resuming) paste0(" (resuming from ", format_bytes(on_disk), ")")
                else "")
      }

      h <- curl::new_handle(useragent = "Wget/")
      if (!is.null(user) && !is.null(pass))
        curl::handle_setopt(h, username = user, password = pass)
      # No overall time limit; abort only after idle_timeout_sec without data.
      curl::handle_setopt(h,
        timeout         = 0L,
        low_speed_limit = 1L,
        low_speed_time  = as.integer(idle_timeout_sec)
      )
      if (resuming)
        curl::handle_setopt(h, resume_from_large = on_disk)

      out_con    <- file(path, if (resuming) "ab" else "wb")
      downloaded <- on_disk
      last_print <- 0
      t_start    <- Sys.time()
      byte_start <- on_disk
      status     <- NA_integer_

      prog_fun <- function(x) {
        writeBin(x, out_con)
        downloaded <<- downloaded + as.double(length(x))
        now <- as.numeric(Sys.time())
        if ((now - last_print) < 0.5) return(invisible(NULL))
        last_print <<- now
        elapsed <- now - as.numeric(t_start)
        speed   <- if (elapsed > 0.3) (downloaded - byte_start) / elapsed else 0
        spd     <- if (is.finite(speed) && speed > 0)
          paste0(format_bytes(speed), "/s") else ""
        if (!is.na(total_size) && total_size > 0) {
          pct <- min(floor(downloaded / total_size * 100), 100)
          cat(sprintf("\r    [%-50s] %3.0f%%  %s / %s  %s   ",
                      paste(rep("#", max(pct %/% 2, 0)), collapse = ""),
                      pct, format_bytes(downloaded), format_bytes(total_size), spd))
        } else {
          cat(sprintf("\r    %s  %s   ", format_bytes(downloaded), spd))
        }
        flush.console()
      }

      res <- tryCatch(
        .ehr_curl_fetch_stream(file_url, prog_fun, handle = h),
        error = function(e) {
          message("\n    download interrupted: ", conditionMessage(e))
          NULL
        }
      )
      close(out_con)

      if (!is.null(res))
        status <- res[["status_code"]]

      if (status %in% c(401L, 403L))
        stop("Access denied. Set up account at https://physionet.org/", call. = FALSE)

      # An error response's body (an HTML page) must not stay in the staged
      # file, and a full 200 reply to a range request replaces the partial copy.
      if (!is.na(status) && !status %in% c(200L, 206L))
        .ehr_drop_staged_bytes(path, keep = byte_start)
      else if (identical(status, 200L) && resuming)
        .ehr_drop_staged_bytes(path, keep = 0, from = byte_start)

      fetch_ok <- !is.null(res) && status %in% c(200L, 206L)
      # A finished 200 reply is the whole file as the server has it; its size
      # and checksum are judged by the validation that follows.
      if (fetch_ok && (file_complete() || is.na(total_size) || identical(status, 200L)))
        success <- TRUE

      if (success || attempt >= max_retries)
        break

      transient <- !is.na(status) && (status == 429L || status >= 500L)
      message("\n    ", if (transient) paste0("server answered HTTP ", status) else "incomplete download",
              ", retrying...")
      if (transient) Sys.sleep(min(2^attempt, 30))
    }

    final_size <- if (file.exists(path)) as.double(file.info(path)$size) else 0
    elapsed    <- as.numeric(difftime(Sys.time(), t_start_all, units = "secs"))
    speed      <- if (elapsed > 0) final_size / elapsed else 0
    avg_spd    <- if (is.finite(speed)) format_bytes(speed) else "???"

    if (success) {
      cat(sprintf("\r    [%-50s] 100%%  %s  %s/s  done\n",
                  paste(rep("#", 50), collapse = ""),
                  format_bytes(final_size), avg_spd))
      return(invisible(NULL))
    }

    if (!is.na(status) && (status == 429L || status >= 500L))
      stop("PhysioNet answered HTTP ", status, " for ", file_url, " after ", max_retries,
           " attempts; the server is busy or down. Run the download again later; ",
           "files already verified are kept.", call. = FALSE)
    if (!is.na(status) && !status %in% c(200L, 206L))
      stop("HTTP ", status, " for ", file_url, call. = FALSE)

    stop("Download incomplete after ", max_retries, " attempts: ", fname,
         call. = FALSE)
  }

  warn_dots(..., ok_args = "token")

  fetch_manifest <- function(user, pass) {
    tryCatch(get_sha256(url, user, pass), error = identity)
  }

  denied <- function(m) inherits(m, "error") && .ehr_manifest_access_denied(m)
  manifest <- fetch_manifest(user, pass)
  if (denied(manifest)) {
    user    <- get_cred(user, "PHYSIONET_USER", "PhysioNet username: ")
    pass    <- get_cred(pass, "PHYSIONET_PASS", "PhysioNet password: ")
    manifest <- fetch_manifest(user, pass)
  }
  # Stored credentials can be wrong: in an interactive session ask again
  # (an empty username stops) instead of failing on the first refusal.
  tries <- 0L
  while (denied(manifest) && is_interactive() && tries < 3L) {
    tries <- tries + 1L
    message("PhysioNet refused these credentials, or the account has not signed ",
            "the data use agreement for this dataset. Enter them again ",
            "(empty username to stop).")
    user <- .ehr_readline("PhysioNet username: ")
    if (!nzchar(trimws(user))) break
    pass <- .ehr_readline("PhysioNet password: ")
    manifest <- fetch_manifest(user, pass)
  }
  if (denied(manifest)) {
    stop("PhysioNet denied access to ", url, ". Check the username and ",
         "password (credentials file, or PHYSIONET_USER / PHYSIONET_PASS) and ",
         "that the account has signed the data use agreement for this dataset.",
         call. = FALSE)
  }

  manifest_available <- !inherits(manifest, "error")
  if (!manifest_available && !.ehr_manifest_is_absent(manifest)) {
    stop(manifest)
  }
  if (manifest_available) {
    chksums <- manifest
  } else {
    chksums <- NULL
    warning(
      "SHA256SUMS.txt is explicitly unavailable (HTTP ",
      .ehr_http_status(manifest),
      "). Falling back to exact Content-Length and file-format validation; ",
      "downloaded artifacts will not be cryptographically verified.",
      call. = FALSE
    )
  }

  if (manifest_available) {
    avail_tbls <- names(chksums)
    miss <- setdiff(files, avail_tbls)
    if (length(miss) > 0L)
      stop("Files not in SHA256SUMS.txt: ", paste(miss, collapse = ", "))

    chksums_v <- unname(chksums[match(files, avail_tbls)])
  } else {
    chksums_v <- rep(NA_character_, length(files))
  }
  paths     <- file.path(dest_folder, files)
  part_paths <- paste0(paths, ".part")
  urls      <- paste(url, files, sep = "/")

  ensure_dirs(unique(dirname(c(paths, part_paths))))

  message("Fetching file sizes...")
  sizes <- vapply(urls, get_size, numeric(1))
  if (!manifest_available &&
      any(!is.finite(sizes) | is.na(sizes) | sizes <= 0)) {
    stop("SHA256SUMS.txt is unavailable and exact positive Content-Length ",
         "could not be established for: ",
         paste(files[!is.finite(sizes) | is.na(sizes) | sizes <= 0],
               collapse = ", "),
         call. = FALSE)
  }

  if (isTRUE(verbose)) {
    total <- sum(sizes, na.rm = TRUE)
    message(src, ": ", length(files), " release file(s)",
            if (total > 0) paste0("  [total: ~", format_bytes(total), "]") else "")
  }

  # Pre-check: skip files that exist, are the right size, and pass the
  # relevant integrity check. Gzip validation applies only to compressed files;
  # plain CSV files are accepted on size/existence because gzip -t will fail on
  # valid uncompressed CSVs.
  is_gzip <- function(path) {
    grepl("\\.gz(?:\\.part)?$", path, ignore.case = TRUE, perl = TRUE)
  }

  gzip_ok <- function(path) {
    if (!is_gzip(path)) return(TRUE)
    # system2(..., stdout = FALSE, stderr = FALSE) joins args into a
    # shell string internally, splitting paths that contain spaces (e.g. a flash
    # drive named "Flash Drive").  Using stdout = TRUE forces R into popen mode,
    # which passes each arg as a separate token and handles spaces correctly.
    # The captured output is discarded; only the 'status' attribute is used.
    gzip_bin <- Sys.which("gzip")
    if (gzip_bin == "") {
      candidates <- c("/usr/bin/gzip", "/bin/gzip", "/usr/local/bin/gzip")
      found      <- candidates[file.exists(candidates)]
      if (length(found) == 0L) {
        return(tryCatch({
          con <- gzfile(path, open = "rb")
          on.exit(close(con))
          repeat {
            chunk <- readBin(con, what = "raw", n = 1024L * 1024L)
            if (length(chunk) == 0L) break
          }
          TRUE
        }, error = function(e) FALSE))
      }
      gzip_bin <- found[[1L]]
    }
    # R's system2 passes args through sh -c as a joined string, so paths with
    # spaces must be shell-quoted.  shQuote() wraps the path in single quotes:
    #   '/usr/bin/gzip' -t '/media/mmussavi/Flash Drive/...' 2>&1
    out <- tryCatch(
      system2(gzip_bin, c("-t", shQuote(path)), stdout = TRUE, stderr = TRUE),
      error = function(e) structure(character(0L), status = 1L)
    )
    st <- attr(out, "status")
    is.null(st) || st == 0L   # NULL status means exit 0 (success)
  }

  readable_table_file <- function(path) {
    info <- file.info(path, extra_cols = FALSE)
    if (is.na(info$size) || info$size <= 0) return(FALSE)
    if (!grepl("\\.csv(?:\\.gz)?(?:\\.part)?$", path,
               ignore.case = TRUE, perl = TRUE)) {
      return(TRUE)
    }
    tryCatch({
      con <- if (is_gzip(path)) gzfile(path, open = "rt") else file(path, "rt")
      on.exit(close(con))
      header <- readLines(con, n = 1L, warn = FALSE)
      length(header) == 1L && nzchar(header[[1L]])
    }, error = function(e) FALSE)
  }

  file_type_ok <- function(path) {
    gzip_ok(path) && readable_table_file(path)
  }

  already_ok <- mapply(function(p, expected, expected_size) {
    if (!file.exists(p)) return(FALSE)
    if (manifest_available && !.ehr_verify_sha256(p, expected)) {
      stop("Existing file does not match the release-bound SHA-256 manifest: ",
           p, ". The file was preserved; choose a different destination or ",
           "resolve it explicitly.", call. = FALSE)
    }
    if (!manifest_available) {
      actual_size <- as.double(file.info(p, extra_cols = FALSE)$size)
      if (is.na(actual_size) || actual_size != expected_size) {
        stop("Existing file cannot be accepted without SHA-256 because its ",
             "size differs from exact Content-Length: ", p,
             ". The file was preserved.", call. = FALSE)
      }
    }
    if (!file_type_ok(p)) {
      stop("Existing file failed file-format/readability validation: ",
           p, ". The file was preserved.", call. = FALSE)
    }
    TRUE
  }, paths, chksums_v, sizes)

  if (any(already_ok))
    message("  Skipping ", sum(already_ok),
            if (manifest_available)
              " file(s) - release-bound SHA-256 verified."
            else
              paste0(" file(s) - exact size and file format verified; ",
                     "SHA-256 unavailable."))

  if (isTRUE(verbose) && any(!already_ok))
    message("Downloading ", sum(!already_ok), " file(s), then verifying them...")
  Map(dl_one, urls[!already_ok], sizes[!already_ok], part_paths[!already_ok])

  # Verify staged files, then publish by same-directory rename.
  pending <- which(!already_ok)
  for (i in pending) {
    if (!manifest_available) {
      staged_size <- if (file.exists(part_paths[[i]]))
        as.double(file.info(part_paths[[i]], extra_cols = FALSE)$size)
      else
        NA_real_
      if (is.na(staged_size) || staged_size != sizes[[i]]) {
        stop("Downloaded staging file cannot be accepted without SHA-256 ",
             "because its size differs from exact Content-Length: ",
             part_paths[[i]], ". It was preserved.", call. = FALSE)
      }
    }
    .ehr_publish_verified_download(
      part_paths[[i]], paths[[i]],
      if (manifest_available) chksums_v[[i]] else NA_character_,
      file_type_ok = file_type_ok
    )
  }

  invisible(NULL)
}

# ___________________________________________________________________________
# High-level download entry points
# ___________________________________________________________________________

#' Load a single source config block
#'
#' Reads the sole generated runtime catalog and returns one source entry.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo")
#' @param config_paths Path to the canonical runtime catalog.
#'
#' @return Named list containing source configuration.
#' @keywords internal
load_src_config <- function(
    src_name,
    config_paths
) {

    if (length(config_paths) != 1L) {
      stop("Exactly one canonical runtime catalog is supported.", call. = FALSE)
    }
    .ehr_catalog_source(src_name, config_paths)
}


# _________________________________________________________________________
# Download source data
# _________________________________________________________________________

#' Download raw EHR source tables
#'
#' Downloads one or more tables from a configured PhysioNet source.
#'
#' Releases, URLs, files, retained columns, and types come from the sole
#' generated runtime catalog `inst/extdata/rendehr-data-source.json`.
#'
#' @param src_name Source name (e.g. \code{"mimic_iii_demo"}, \code{"eicu_demo"}).
#'   Defaults to the active project config source, then \code{"mimic_iv_demo"}.
#' @param tables Optional character vector of table names to download.
#'   If NULL, all tables for the source are downloaded.
#' @param raw_data_root Directory to store raw downloaded files. This may be
#'   either a root directory, in which case files are saved under
#'   `{raw_data_root}/{src_name}`, or the exact source directory ending in
#'   `src_name`. Defaults to the raw data root implied by
#'   \code{ehr_paths(src_name)}.
#'
#' @details
#' Credentials use the accepted canonical `PHYSIONET_USER` and
#' `PHYSIONET_PASS` contract. Downloads resume in `.part` files. Existing and
#' new artifacts are verified against the release-bound SHA-256 manifest before
#' use or publication when the provider publishes one. An explicit HTTP 404 or
#' 410 for `SHA256SUMS.txt` selects a disclosed lower-assurance fallback that
#' requires exact positive `Content-Length` and file-format/readability checks.
#' Malformed, incomplete, authentication, transient-network, and server-error
#' manifest failures do not downgrade. Mismatching files are reported and
#' preserved.
#'
#' @return Invisibly returns the downloaded filenames.
#' @examples
#' \dontrun{
#' download_source("mimic_iv_demo")                       # open demo, no credentials
#' download_source("mimic_iv", tables = c("patients", "admissions"))
#' }
#' @export
download_source <- function(
    src_name       = NULL,
    tables         = NULL,
    raw_data_root  = NULL
) {
    paths <- ehr_paths(src_name = src_name)
    src_name <- paths$src_name
    if (is.null(raw_data_root))
        raw_data_root <- dirname(paths$raw_dir)

    # package default config
    pkg_config <- system.file(
        "extdata",
        "rendehr-data-source.json",
        package = "rendehR"
    )

    src_cfg <- load_src_config(
        src_name = src_name,
        config_paths = pkg_config
    )
    if (identical(src_cfg$status, "blocked")) {
      stop("Source '", src_name, "' is blocked: ",
           src_cfg$blocked_reason %||% "acquisition provenance is unverified.",
           call. = FALSE)
    }

    base_url    <- src_cfg$url
    tables_json <- src_cfg$tables

    # default: download all tables
    if (is.null(tables)) {
        tables <- names(tables_json)
    }

    # validate requested tables
    missing <- setdiff(tables, names(tables_json))

    if (length(missing) > 0L) {

        stop(
            "Tables not found for source '",
            src_name,
            "': ",
            paste(missing, collapse = ", "),
            call. = FALSE
        )
    }

    message(
        "\n*** Downloading source: ",
        src_name,
        " ***\n"
    )

    files_to_dl <- vapply(
        tables,
        function(tbl) tables_json[[tbl]]$files,
        character(1L)
    )

    creds <- .ehr_env$creds
    if (is.null(creds)) {
        credentials_path <- .ehr_default_credentials_path()
        if (!is.null(credentials_path) && file.exists(credentials_path))
            creds <- load_credentials(credentials_path)
    }

    physionet <- if (!is.null(creds)) .ehr_normalize_physionet(creds$physionet) else list()
    # Same order as load_credentials(): environment variables, then the stored file values.
    user <- .ehr_first_credential(
      Sys.getenv("PHYSIONET_USER", unset = NA_character_),
      physionet$username %||% NULL
    )
    pass <- .ehr_first_credential(
      Sys.getenv("PHYSIONET_PASS", unset = NA_character_),
      physionet$password %||% NULL
    )

    dest_folder <- .ehr_download_dest_dir(raw_data_root, src_name, paths$base_dir)
    message("rendehR: raw files will be saved/checked in '", dest_folder, "'.")

    download_check_data(
        dest_folder = dest_folder,
        files       = files_to_dl,
        url         = base_url,
        src         = src_name,
        user        = user,
        pass        = pass
    )

    invisible(files_to_dl)
}

#' Download raw EHR source tables for multiple sources
#'
#' Loops over `download_source()` for each source name. Source-specific folders
#' are still created internally as `{raw_data_root}/{src_name}`.
#'
#' @param src_names Character vector of source names. If NULL, uses the active
#'   project config source, then "mimic_iv_demo".
#' @param tables Optional character vector of table names to download for each
#'   source. If NULL, all tables for each source are downloaded.
#' @param raw_data_root Directory for raw downloaded files. This may be either
#'   a root directory or an exact source directory ending in `src_name`. If
#'   NULL, each source uses the root implied by `ehr_paths(src_name)`.
#' @return Invisibly returns a named list of downloaded filenames by source.
#' @examples
#' \dontrun{
#' download_sources(c("mimic_iii_demo", "mimic_iv_demo", "eicu_demo"))
#' }
#' @export
download_sources <- function(src_names = NULL, tables = NULL, raw_data_root = NULL) {
    if (is.null(src_names))
        src_names <- ehr_paths()$src_name
    src_names <- as.character(src_names)
    if (length(src_names) == 0L || any(!nzchar(src_names)))
        stop("src_names must contain at least one source name.", call. = FALSE)

    results <- setNames(vector("list", length(src_names)), src_names)
    for (i in seq_along(src_names)) {
        src_name <- src_names[[i]]
        paths <- ehr_paths(src_name = src_name)
        source_data_dir <- raw_data_root %||% dirname(paths$raw_dir)
        results[[i]] <- download_source(src_name = src_name,
                                        tables = tables,
                                        raw_data_root = source_data_dir)
    }
    invisible(results)
}

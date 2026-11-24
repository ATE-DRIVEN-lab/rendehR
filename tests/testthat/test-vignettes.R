# Static checks of the vignette sources (vignettes/*.Rmd.orig), so code that is
# never evaluated (eval = FALSE, ```r blocks) cannot drift from the package.
# Executing the chunks needs PostgreSQL and the PhysioNet demo data: that is
# vignettes/precompute.R, which stops on the first chunk error.

.vig_dir <- function() {
  d <- testthat::test_path("..", "..", "vignettes")
  if (!dir.exists(d) || !length(list.files(d, "\\.Rmd\\.orig$"))) {
    testthat::skip("vignette sources are not in this tree (installed-package check)")
  }
  d
}

# R code of every chunk, keyed "file:chunk-start-line".
.vig_chunks <- function(file) {
  lines <- readLines(file, warn = FALSE)
  open <- grep("^```\\s*(\\{r[ ,}]|\\{r\\}|r\\s*$)", lines)
  close <- grep("^```\\s*$", lines)
  out <- list()
  for (o in open) {
    e <- close[close > o][1]
    if (is.na(e)) stop(basename(file), ":", o, ": chunk is never closed", call. = FALSE)
    out[[paste0(basename(file), ":", o)]] <- lines[seq_len(e - o - 1) + o]
  }
  out
}

# Function names called in parsed code: bare calls and pkg::fun.
.vig_calls <- function(exprs) {
  calls <- character()
  walk <- function(x) {
    if (is.call(x)) {
      f <- x[[1]]
      if (is.name(f) && !as.character(f) %in% c("::", ":::")) calls <<- c(calls, as.character(f))
      if (is.call(f) && identical(f[[1]], as.name("::"))) {
        calls <<- c(calls, paste0(as.character(f[[2]]), "::", as.character(f[[3]])))
      }
      for (i in seq_along(x)[-1]) if (!identical(x[[i]], quote(expr = ))) walk(x[[i]])
      if (is.call(f) && !identical(f[[1]], as.name("::"))) walk(f)
    } else if (is.expression(x) || is.pairlist(x)) {   # pairlist: function formals and their defaults
      for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) walk(x[[i]])
    }
  }
  walk(exprs)
  unique(calls)
}

# Names assigned anywhere in parsed code (helpers and function arguments).
.vig_defined <- function(exprs) {
  out <- all.names(exprs)
  assigned <- character()
  walk <- function(x) {
    if (is.call(x)) {
      op <- as.character(x[[1]])[1]
      if (op %in% c("<-", "=", "<<-") && is.name(x[[2]])) assigned <<- c(assigned, as.character(x[[2]]))
      if (op == "function") assigned <<- c(assigned, names(x[[2]]))
      for (i in seq_along(x)[-1]) if (!identical(x[[i]], quote(expr = ))) walk(x[[i]])
    } else if (is.expression(x)) for (i in seq_along(x)) walk(x[[i]])
  }
  walk(exprs)
  intersect(unique(assigned), out)
}

# Packages a vignette attaches with library()/require().
.vig_calls_library <- function(exprs) {
  pk <- character()
  walk <- function(x) {
    if (is.call(x)) {
      if (is.name(x[[1]]) && as.character(x[[1]]) %in% c("library", "require") && length(x) > 1) {
        pk <<- c(pk, as.character(x[[2]]))
      }
      for (i in seq_along(x)[-1]) if (!identical(x[[i]], quote(expr = ))) walk(x[[i]])
    } else if (is.expression(x)) for (i in seq_along(x)) walk(x[[i]])
  }
  walk(exprs)
  unique(pk)
}

test_that("every vignette chunk parses", {
  d <- .vig_dir()
  bad <- character()
  for (f in list.files(d, "\\.Rmd\\.orig$", full.names = TRUE)) {
    for (k in names(ch <- .vig_chunks(f))) {
      res <- tryCatch({ parse(text = ch[[k]], keep.source = FALSE); NULL },
                      error = function(e) conditionMessage(e))
      if (!is.null(res)) bad <- c(bad, paste(k, res))
    }
  }
  expect_identical(bad, character())
})

test_that("every function a vignette calls exists", {
  d <- .vig_dir()
  setup <- parse(file.path(d, "precompute-setup.R"), keep.source = FALSE)
  base_pkgs <- c("base", "stats", "utils", "methods", "graphics", "grDevices", "tools")
  known <- c(getNamespaceExports("rendehR"), unlist(lapply(base_pkgs, getNamespaceExports)),
             .vig_defined(setup), "function", "if", "for", "while", "repeat", "{", "(", "[", "[[",
             "$", "@", "<-", "<<-", "=", "::", ":::", "~", "!", "-", "+", "*", "/", "^", "&", "&&",
             "|", "||", "==", "!=", "<", ">", "<=", ">=", "%in%", "%%", "%/%", "|>", "?")
  missing_fun <- character()
  for (f in list.files(d, "\\.Rmd\\.orig$", full.names = TRUE)) {
    ex <- parse(text = unlist(.vig_chunks(f)), keep.source = FALSE)
    calls <- .vig_calls(ex)
    attached <- unlist(lapply(intersect(.vig_calls_library(ex), rownames(utils::installed.packages())),
                              getNamespaceExports))
    ok <- c(known, attached, .vig_defined(ex))
    for (cl in calls) {
      if (grepl("::", cl, fixed = TRUE)) {
        p <- strsplit(cl, "::", fixed = TRUE)[[1]]
        if (!requireNamespace(p[1], quietly = TRUE)) next   # optional package not installed here
        if (!p[2] %in% getNamespaceExports(p[1])) missing_fun <- c(missing_fun, paste0(basename(f), ": ", cl))
      } else if (!cl %in% ok && !startsWith(cl, "%")) {
        missing_fun <- c(missing_fun, paste0(basename(f), ": ", cl))
      }
    }
  }
  expect_identical(unique(missing_fun), character())
})

test_that("knitted vignettes carry no chunk errors or warnings", {
  d <- .vig_dir()
  bad <- character()
  for (f in list.files(d, "\\.Rmd$", full.names = TRUE)) {
    hit <- grep("^#> (Error|Warning)", readLines(f, warn = FALSE))
    if (length(hit)) bad <- c(bad, paste0(basename(f), ":", hit))
  }
  expect_identical(bad, character())
})

test_that("every vignette source has a knitted output and is in the precompute order", {
  d <- .vig_dir()
  src <- sub("\\.Rmd\\.orig$", "", list.files(d, "\\.Rmd\\.orig$"))
  expect_true(all(file.exists(file.path(d, paste0(src, ".Rmd")))))
  order <- eval(parse(file.path(d, "precompute.R"), keep.source = FALSE)[[1]][[3]])
  expect_setequal(order, src)
})

test_that("knitted vignettes print no machine-specific paths", {
  d <- .vig_dir()
  for (f in list.files(d, "\\.Rmd$", full.names = TRUE)) {
    hits <- grep("/home/|/Users/|/tmp/Rtmp|[A-Z]:\\\\Users", readLines(f, warn = FALSE), value = TRUE)
    expect_identical(hits, character(), info = basename(f))
  }
})

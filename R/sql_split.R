# Split a SQL script into statements on top-level semicolons, dropping
# comments. A plain strsplit on ";" (after a regex that strips "--" to end of
# line) breaks as soon as a string literal contains either character, so scan
# the text instead, tracking single-quoted strings (with '' escapes),
# double-quoted identifiers, dollar-quoted bodies ($tag$ ... $tag$), and
# "--" / "/* */" comments.
.ehr_split_sql <- function(sql) {
  chars <- strsplit(paste(sql, collapse = "\n"), "", fixed = TRUE)[[1L]]
  n <- length(chars)
  out <- character(0)
  buf <- character(0)
  flush <- function() {
    s <- trimws(paste(buf, collapse = ""))
    if (nzchar(s)) out <<- c(out, s)
    buf <<- character(0)
  }
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    nx <- if (i < n) chars[i + 1L] else ""
    if (ch == "-" && nx == "-") {
      while (i <= n && chars[i] != "\n") i <- i + 1L
      next
    }
    if (ch == "/" && nx == "*") {
      i <- i + 2L
      while (i < n && !(chars[i] == "*" && chars[i + 1L] == "/")) i <- i + 1L
      i <- i + 2L
      buf <- c(buf, " ")
      next
    }
    if (ch == "'" || ch == "\"") {
      j <- i + 1L
      repeat {
        if (j > n) break
        if (chars[j] == ch) {
          if (j < n && chars[j + 1L] == ch) { j <- j + 2L; next }
          break
        }
        j <- j + 1L
      }
      buf <- c(buf, chars[i:min(j, n)])
      i <- j + 1L
      next
    }
    if (ch == "$") {
      rest <- paste(chars[i:min(n, i + 63L)], collapse = "")
      tag <- regmatches(rest, regexpr("^\\$[A-Za-z_]*\\$", rest))
      if (length(tag)) {
        k <- nchar(tag)
        tail <- paste(chars[(i + k):n], collapse = "")
        end <- regexpr(tag, tail, fixed = TRUE)
        stop_at <- if (end > 0) i + k + end + k - 2L else n
        buf <- c(buf, chars[i:stop_at])
        i <- stop_at + 1L
        next
      }
    }
    if (ch == ";") {
      flush()
      i <- i + 1L
      next
    }
    buf <- c(buf, ch)
    i <- i + 1L
  }
  flush()
  out
}

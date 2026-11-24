# Native stdio MCP transport. stdout is reserved for JSON-RPC.
`%||%` <- function(a, b) if (is.null(a)) b else a
.mcp_error <- function(id, code, message) list(jsonrpc = "2.0", id = id, error = list(code = code, message = message))
.mcp_validate <- function(args, schema) {
  if (!is.list(args) || (length(args) && is.null(names(args)))) stop("arguments must be an object")
  if (anyDuplicated(names(args))) stop("Duplicate argument names")
  if (length(setdiff(names(args), names(schema$properties)))) stop("Unknown arguments")
  if (length(setdiff(unlist(schema$required), names(args)))) stop("Missing required arguments")
  for (key in names(args)) .mcp_value(args[[key]], schema$properties[[key]], key)
}
.mcp_value <- function(x, p, key) {
  if (!is.null(p$oneOf)) {
    matches <- vapply(p$oneOf, function(choice) tryCatch({.mcp_value(x, choice, key); TRUE}, error = function(e) FALSE), logical(1))
    if (sum(matches) != 1L) stop("Invalid argument: ", key)
    return(invisible(NULL))
  }
  if (identical(p$type, "array")) {
    if (!is.list(x) || !is.null(names(x))) stop("Expected an array: ", key)
    if (!is.null(p$minItems) && length(x) < p$minItems) stop("Too few values: ", key)
    if (!is.null(p$maxItems) && length(x) > p$maxItems) stop("Too many values: ", key)
    for (item in x) .mcp_value(item, p$items, key)
    return(invisible(NULL))
  }
  ok <- length(x) == 1L && !is.list(x) && !is.na(x) && switch(p$type,
    string = is.character(x) && nzchar(trimws(x)),
    integer = is.numeric(x) && is.finite(x) && x == floor(x),
    boolean = is.logical(x), FALSE)
  if (!ok) stop("Invalid argument: ", key)
  if (!is.null(p$enum) && !x %in% unlist(p$enum)) stop("Unsupported value: ", key)
  if (!is.null(p$minimum) && x < p$minimum) stop("Below minimum: ", key)
  if (!is.null(p$maximum) && x > p$maximum) stop("Above maximum: ", key)
}
.mcp_dispatch <- function(msg, state, registry) {
  if (!is.list(msg) || is.null(names(msg)) || !identical(msg$jsonrpc, "2.0") ||
      !is.character(msg$method) || length(msg$method) != 1L)
    return(.mcp_error(NULL, -32600L, "Invalid Request"))
  if (!"id" %in% names(msg)) {
    if (identical(msg$method, "notifications/initialized") && state$initialized) state$ready <- TRUE
    return(NULL)
  }
  id <- msg$id
  if (is.null(id) || length(id) != 1L || !(is.character(id) || is.numeric(id))) return(.mcp_error(NULL, -32600L, "Invalid request id"))
  result <- switch(msg$method,
    initialize = {
      if (state$initialized) return(.mcp_error(id, -32600L, "Already initialized"))
      versions <- c("2024-11-05", "2025-03-26", "2025-06-18")
      requested <- msg$params$protocolVersion
      if (!is.character(requested) || length(requested) != 1L) return(.mcp_error(id, -32602L, "protocolVersion required"))
      state$initialized <- TRUE
      list(protocolVersion = if (requested %in% versions) requested else "2025-06-18",
           capabilities = list(tools = list(listChanged = FALSE)), serverInfo = list(name = "rendehR", version = .rh_version()))
    },
    ping = structure(list(), names = character()),
    {
      if (!state$ready) return(.mcp_error(id, -32000L, "Initialize the session first"))
      if (msg$method == "tools/list") {
        list(tools = unname(lapply(names(registry), function(n) c(list(name = n), registry[[n]][c("description", "inputSchema", "annotations")]))))
      } else if (msg$method == "tools/call") {
        name <- msg$params$name
        if (!is.character(name) || length(name) != 1L || !name %in% names(registry)) return(.mcp_error(id, -32602L, "Unknown tool"))
        tool <- registry[[name]]; args <- msg$params$arguments %||% list()
        invalid <- tryCatch({.mcp_validate(args, tool$inputSchema); NULL}, error = conditionMessage)
        if (!is.null(invalid)) return(.mcp_error(id, -32602L, invalid))
        failed <- FALSE
        value <- tryCatch(tool$fn(args), error = function(e) {failed <<- TRUE; list(error = conditionMessage(e))})
        list(content = list(list(type = "text", text = as.character(jsonlite::toJSON(value, auto_unbox = TRUE, na = "null", null = "null", dataframe = "rows", digits = NA)))), isError = failed)
      } else return(.mcp_error(id, -32601L, "Method not found"))
    })
  list(jsonrpc = "2.0", id = id, result = result)
}
# Start the current rendehR MCP server over stdio. Sourced by the bundled
# launcher only; this file is not part of the package build.
ehr_mcp_start <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite to start this server.")
  registry <- .rh_tools()
  state <- new.env(parent = emptyenv()); state$initialized <- FALSE; state$ready <- FALSE
  input <- file("stdin", open = "r"); on.exit(close(input), add = TRUE)
  repeat {
    line <- readLines(input, n = 1L, warn = FALSE)
    if (!length(line)) break
    if (!nzchar(trimws(line))) next
    parsed <- TRUE
    msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) {parsed <<- FALSE; NULL})
    # Capture dependency prints so they cannot corrupt protocol output.
    captured <- capture.output(response <- if (!parsed) .mcp_error(NULL, -32700L, "Parse error") else
      tryCatch(.mcp_dispatch(msg, state, registry), error = function(e) .mcp_error(if (is.list(msg)) msg$id else NULL, -32603L, "Internal error")))
    if (!is.null(response)) {
      cat(jsonlite::toJSON(response, auto_unbox = TRUE, null = "null", na = "null", digits = NA), "\n", sep = "")
      flush(stdout())
    }
  }
  invisible(NULL)
}

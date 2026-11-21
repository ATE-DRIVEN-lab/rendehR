#!/usr/bin/env Rscript
# Run with Rscript --vanilla /absolute/path/to/this/file.R
argv <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", argv[grepl("^--file=", argv)])
if (length(script) != 1L) stop("Launch this file with Rscript.")
root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
options(rendehR.mcp_root = root)
source(file.path(root, "R", "mcp_discovery.R"), local = .GlobalEnv)
source(file.path(root, "R", "rendehr_adapter.R"), local = .GlobalEnv)
source(file.path(root, "R", "mcp_server.R"), local = .GlobalEnv)
ehr_mcp_start()

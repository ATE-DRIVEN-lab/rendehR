# Rebuilds the precomputed vignettes: every <name>.Rmd.orig is knitted against
# the open PhysioNet demo data into <name>.Rmd, with its output and figures.
# R CMD build then only renders Markdown, so it needs no database.
#
# Needs this checkout installed, a PostgreSQL server you may create databases
# on (PGHOST, PGPORT, PGUSER, PGPASSWORD) and internet access to PhysioNet.
# The demo projects are written to RENDEHR_DOCS_DIR (default: a temp folder).
#
#   Rscript vignettes/precompute.R              # all, in order
#   Rscript vignettes/precompute.R cohorts mcp  # some
# Articles later in the order reuse the database "rendehr_demo" built earlier.

order <- c("rendehR", "projects", "cohorts", "pipeline", "harmonized-data",
           "concepts", "explorer", "sepsis-cohort-definitions",
           "guided-setup-assistant", "adding-a-new-source", "mcp")
args <- commandArgs(trailingOnly = TRUE)
todo <- if (length(args)) intersect(order, args) else order
stopifnot(length(todo) > 0)

# Printed paths show this neutral folder instead of the machine's own.
docs_root <- normalizePath(Sys.getenv("RENDEHR_DOCS_DIR", file.path(tempdir(), "rendehr-docs")),
                           mustWork = FALSE)
here <- if (dir.exists("vignettes")) "vignettes" else "."
old <- setwd(here)
on.exit(setwd(old), add = TRUE)
for (v in todo) {
  message("== ", v)
  knitr::knit(paste0(v, ".Rmd.orig"), paste0(v, ".Rmd"), quiet = TRUE,
              envir = new.env(parent = globalenv()))
  out <- readLines(paste0(v, ".Rmd"))
  writeLines(gsub(docs_root, "~/rendehr-docs", out, fixed = TRUE), paste0(v, ".Rmd"))
}

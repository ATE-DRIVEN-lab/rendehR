# Sourced by the hidden setup chunk of every <name>.Rmd.orig (see precompute.R).
name <- sub("\\.Rmd\\.orig$", "", knitr::current_input())
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", message = FALSE, error = FALSE,  # a failing chunk stops the build
  fig.path = file.path("figures", paste0(name, "-")),
  fig.width = 7, fig.height = 3.8, dpi = 110, out.width = "100%"
)
options(width = 88, pillar.width = 88)

docs_root <- normalizePath(Sys.getenv("RENDEHR_DOCS_DIR", file.path(tempdir(), "rendehr-docs")),
                           mustWork = FALSE)
dir.create(docs_root, recursive = TRUE, showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
# code runs in the demo folder; figures still land in vignettes/figures
figures_dir <- normalizePath("figures", mustWork = FALSE)
knitr::opts_knit$set(root.dir = docs_root, base.dir = normalizePath("."))

# The only adaptation the visible code does not show: real credentials.
demo_credentials <- function(project) {
  jsonlite::write_json(list(postgres = list(
    host = Sys.getenv("PGHOST", "localhost"), port = as.integer(Sys.getenv("PGPORT", "5432")),
    user = Sys.getenv("PGUSER", "postgres"), password = Sys.getenv("PGPASSWORD"))),
    file.path(project, "config", "config_pass.json"), auto_unbox = TRUE, pretty = TRUE)
}

library(rendehR)

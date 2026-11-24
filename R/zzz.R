# Package-level import declarations.
# The pipe is used throughout the package; importing it here ensures it is
# available in all functions without requiring library(magrittr) in the session.

#' rendehR: Multi-Source ICU EHR Harmonization Pipeline
#'
#' Downloads, imports, cohort-filters, and harmonizes ICU EHR data from
#' MIMIC-III, MIMIC-IV, and eICU.
#'
#' @references Mussavi Rizi M, Shein V, Torres-Espin A (2026). rendehR: a cohort-first, provenance-aware, agentic-ready pipeline for harmonizing multi-source electronic health records.
#'   medRxiv (preprint). \doi{10.64898/2026.11.19.26367722}
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
NULL

# Tell data.table that this package intentionally uses := and related syntax.
# See vignette("datatable-importing", package = "data.table").
.datatable.aware <- TRUE

# Seed the source family registry: first the built-in datasets, then any user
# sources discovered under EHR_EXTERNAL_CONFIG_ROOT (source-registry.json and/or
# sources/*.json). Runs on both library() and devtools::load_all(). See
# .register_builtin_sources() (env_config.R) and .register_config_root_sources()
# (register_source.R).
.onLoad <- function(libname, pkgname) {
  .register_builtin_sources()
  .register_config_root_sources()
}

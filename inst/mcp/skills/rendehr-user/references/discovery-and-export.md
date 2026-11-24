# Discover concepts and export analysis data

API checked against the development branch (October 2026). The R examples are guidance for generating a user script, not evidence of a live successful run.

## Starting conditions

Use an already prepared PostgreSQL demo database and an existing configured connection. No downloads, schema creation, harmonization, or ICU projection builds are included here. Confirm the installed package version and, when available, source revision. Obtain credentials through the user's configured environment; do not embed them in generated scripts or reports.

Establish source, requested variables, population, admission/ICU grain, time origin, window, aggregation, missing-bin policy, output format, and destination. Reuse explicit user choices. "Blood pressure" may mean systolic, diastolic, or mean pressure and may have distinct measurement variants; matching names do not resolve these choices.

## Discovery and availability

Public calls, using an explicit existing DBI connection `con`:

```r
rendehR::list_sources(con = con)
catalog <- rendehR::list_concepts(src_name = src, con = con)
matches <- rendehR::search_concepts(
  pattern = "heart|pressure|sbp|dbp", src_name = src, con = con
)
rendehR::list_tables(src_name = src, schema = "harmonized", con = con)
```

| Need | Public function | Interpretation |
| --- | --- | --- |
| Registered source families | list_registered_sources() | Supported/configured family definitions, not loaded data |
| Sources in the connected database | list_sources(con = con) | Detected source schemas, not proof of completed processing |
| Tables for one layer | list_tables(src_name, schema, con) | Use raw, concepts, derived, harmonized, or an explicit schema |
| Available concept tables | list_concepts(src_name, con) | Tables and available registry provenance |
| Candidate concepts by name | search_concepts(pattern, src_name, con) | Name-pattern matches, not semantic equivalence |
| Concept summary | concept_summary(src_name, concept, schema, con) | Observed table statistics; not a clinical validation |
| Cohort summary | cohort_summary(src_name, con) | Summary of prepared outputs, not a full exclusion flow |

Call `search_concepts()` with named arguments: its order is `pattern, src_name`, and it returns a data frame with source/concept/schema information (the help documents the data-frame return and the correct argument order). Inspect `names(matches)` and retain schema/provenance information when resolving candidates. Do not silently select one ambiguous match.

`list_concepts()` inventories atomic, derived, and external concept tables. `fetch_concepts_wide()` reads harmonized `ts_tab` (admission) or `ts_icu` (ICU). Inspect the exact target table's distinct concept names before pivoting; the pivot otherwise only warns and returns all-NA columns for missing concepts. Inspect relevant unit metadata as well: neither name matching nor averaging proves unit compatibility.

`list_sources()` discovers database schemas, not configured source support. A connection error is not an empty source list. Missing `ts_icu` means this workflow cannot yet produce ICU-wide output; report that prerequisite instead of switching to admission grain or creating tables automatically.

## Scope and time semantics

Inspect active filters using `active_cohort_info()` and `active_time_window_info()`. Capture their returned objects locally; the former contains identifiers, so do not dump it into the conversation.

`fetch_concepts_wide()` does not consult these active filters. It uses explicit `analysis_level`, `window`, and `cohort_only` arguments (`case_adm_only` and `scope_only` were removed; supplying either is an error). An active subgroup must not be assumed applied. The first example stops when an active subgroup exists for the source; a subgroup-aware route is a separate workflow to validate. Do not clear filters merely to make an example run.

For the first 48 hours from ICU admission:

- `analysis_level = "icu"` reads the ICU projection.
- `bin_min = 60` and `window = c(0, 47)` select minute offsets `[0, 2880)`.
- Returned `t_bin` is a zero-based bin index, not a minute offset.
- `complete_grid = FALSE` returns observed bins. It does not promise 48 rows per stay.
- `complete_grid = TRUE` adds missing bins only for identifiers represented in the filtered aggregate, not all eligible cohort members. It neither imputes measurements nor establishes 48-hour length of stay.
- `cohort_only = TRUE` (the default) keeps rows of cohort members only (`cohort_info.cohort_index`); `cohort_only = FALSE` reads all imported context. It is not a replacement for arbitrary subgroup membership.
- The pivot groups by configured identifier columns present in the table; `ts_icu` drops the native ICU id (kept as `source_icustay_id`) and exposes `assigned_icustay_id` as the ICU-stay key, so key the panel on `assigned_icustay_id` and verify output uniqueness on it.

An explicit ICU-relative window is not a promise of clipping every measurement to ICU discharge. If within-stay clipping or length-of-stay eligibility is requested, establish and validate that condition separately.

## First evaluation scenario

Prepared MIMIC-IV demo database, with harmonized ICU output. User selects exactly two existing, unit-compatible concepts: heart rate and a specified blood-pressure measure. User requests arithmetic means in hourly bins during the first 48 ICU hours, observed bins only, no active subgroup, and a local CSV export. Exact concept names and expected counts come from the chosen fixture; do not invent them.

The following function is a reviewable script example for a small demo result. Calling it queries the database and writes a file; defining or parsing it does neither. Supply an existing `con`, verified `src`, a two-element vector of exact `concepts`, and a new CSV `output_path`. Review units and concept meanings before calling it.

```r
export_demo_icu <- function(con, src, concepts, output_path) {
  stopifnot(length(concepts) == 2L, !anyNA(concepts),
            all(nzchar(concepts)), !anyDuplicated(concepts))
  if (file.exists(output_path)) stop("Choose a new output path.")
  if (!dir.exists(dirname(output_path))) stop("Output directory is missing.")
  if (!src %in% rendehR::list_sources(con = con)) stop("Source not found.")

  active <- rendehR::active_cohort_info()
  if (!is.null(active[[src]]))
    stop("Wide retrieval does not apply the active subgroup; resolve scope first.")
  # Inspect locally: the explicit ICU window below is the intended time scope.
  time_settings <- rendehR::active_time_window_info()

  if (!"ts_icu" %in% rendehR::list_tables(
      src_name = src, schema = "harmonized", con = con))
    stop("Prepared harmonized ts_icu is required.")
  schema <- rendehR::src_schema_names(src)$harmonized
  table_id <- DBI::Id(schema = schema, table = "ts_icu")
  fields <- DBI::dbListFields(con, table_id)
  required <- c("concept_name", "value", "time_since_icu_min", "assigned_icustay_id")
  if (!all(required %in% fields)) stop("Expected MIMIC-IV ICU fields missing.")
  table_sql <- as.character(DBI::dbQuoteIdentifier(con, table_id))
  available <- DBI::dbGetQuery(con, paste0(
    "SELECT DISTINCT concept_name FROM ", table_sql
  ))$concept_name
  if (!all(concepts %in% available)) stop("Requested concept absent from ts_icu.")

  spec <- stats::setNames(as.list(rep("mean", length(concepts))), concepts)
  data <- rendehR::fetch_concepts_wide(
    src_name = src, spec = spec, bin_min = 60,
    analysis_level = "icu", window = c(0, 47),
    complete_grid = FALSE, cohort_only = TRUE, n = Inf, con = con
  )
  if (!nrow(data)) stop("No matching observations in the requested window.")
  stopifnot(all(c("assigned_icustay_id", "t_bin", concepts) %in% names(data)),
            !anyNA(data$assigned_icustay_id), !anyNA(data$t_bin),
            all(data$t_bin >= 0 & data$t_bin <= 47),
            !anyDuplicated(data[c("assigned_icustay_id", "t_bin")]))
  if (any(vapply(data[concepts], function(x) all(is.na(x)), logical(1))))
    stop("A requested concept has no usable values in this window.")

  utils::write.csv(data, output_path, row.names = FALSE, na = "")
  reread <- utils::read.csv(output_path, colClasses = "character",
                           check.names = FALSE)
  stopifnot(nrow(reread) == nrow(data), identical(names(reread), names(data)))
  invisible(list(
    path = normalizePath(output_path), rows = nrow(data),
    stays = length(unique(data$assigned_icustay_id)), columns = names(data),
    missing = vapply(data[concepts], function(x) sum(is.na(x)), integer(1))
  ))
}
```

This example is specifically for MIMIC-IV ICU grain (keyed on `assigned_icustay_id`), not a universal source adapter. The result is collected into R; use it only after confirming demo-scale volume. `export_table_parquet()` and `export_table_csv_gz()` stream tables/queries, but they do not directly export the result of `fetch_concepts_wide()`. A large wide export requires a separately reviewed query/export route.

## Explore interactively

`run_explorer(schema)` opens a local Shiny explorer of a merged schema (`rendehr_admission` when it has a `cohort_info` table, otherwise `rendehr`): overview, cohort filters with Table 1, outcomes and survival, one patient's time series read live, concepts and data-quality checks, plus management pages that can apply a project's configuration, run pipeline steps or a cohort import as background tasks, and export tables. `explorer_app()` returns the app without starting it. It needs the Suggests packages (shiny, bslib, bsicons, plotly, DT, shinyWidgets, shinycssloaders, survival). It serves individual-level data and listens on 127.0.0.1: for a remote server, use an SSH tunnel; never expose it publicly. Starting it is not a verification of the data.

## Verification and reporting

The example checks structure, window bounds, duplicate ICU/bin keys, nonempty output, all-missing concepts, and CSV read-back dimensions. It does not independently prove arithmetic correctness, unit compatibility, or clinical validity.

For live evaluation, compare selected aggregates with the underlying measurements in a known demo fixture. Include observations at minutes 0, 59, 60, 2879, and 2880 to verify boundaries. Confirm exclusions and expected missingness. Report only aggregate evidence in the conversation.

Return the reproducible script, exact source/concept selections, aggregation and window, artifact location, row/stay counts, missingness, and any limitations. Label code-only output as unexecuted and live checks by what actually ran.

## Evidence locations in the reviewed package

- `R/access_core.R`: discovery, search return type, wide retrieval, and pivot implementation.
- `R/access_subcohort.R`: active filter state and inspection functions.
- `R/access_export.R`: streaming table/query export.
- `man/fetch_concepts_wide.Rd`, `man/list_concepts.Rd`, `man/search_concepts.Rd`.
- `tests/testthat/test-access.R`: aggregate and window validation tests.

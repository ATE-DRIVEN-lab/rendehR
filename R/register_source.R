# Public API for schema-first registration of source families. Family JSON
# files live under EHR_EXTERNAL_CONFIG_ROOT/sources and are discovered at load.

# Families that ship with the package. Overriding one requires overwrite = TRUE
# so a stray user config cannot silently shadow a supported source.
.ehr_builtin_families <- c("mimic_iv", "mimic_iii", "eicu")
.ehr_source_definition_version <- 1L

# Resolve and load the authoritative source-family JSON Schema. The local
# fallback supports pkgload/source-tree tests; installed packages use
# system.file().
.source_family_schema_path <- function() {
  candidates <- c(
    system.file(
      "schema", "source-family.schema.json",
      package = utils::packageName() %||% "rendehR"
    ),
    file.path("inst", "schema", "source-family.schema.json"),
    file.path("..", "..", "inst", "schema", "source-family.schema.json")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) {
    stop(
      "source-family schema is unavailable: expected installed or source-tree ",
      "`inst/schema/source-family.schema.json`.",
      call. = FALSE
    )
  }
  normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
}

.load_source_family_schema <- function() {
  path <- .source_family_schema_path()
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      stop(
        "source-family schema could not be loaded from '", path, "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

.schema_instance_type <- function(x, type) {
  switch(
    type,
    object = is.list(x) && (length(x) == 0L || !is.null(names(x))),
    array = (is.atomic(x) && !is.null(x) && length(x) > 0L) ||
      (is.list(x) && is.null(names(x))),
    string = is.character(x) && length(x) == 1L && !is.na(x),
    integer = is.numeric(x) && length(x) == 1L && !is.na(x) &&
      is.finite(x) && x == as.integer(x),
    number = is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x),
    boolean = is.logical(x) && length(x) == 1L && !is.na(x),
    null = is.null(x),
    FALSE
  )
}

.schema_resolve_ref <- function(schema, ref) {
  prefix <- "#/$defs/"
  if (!is.character(ref) || length(ref) != 1L ||
      !startsWith(ref, prefix)) {
    stop("unsupported source-family schema reference: ", ref, call. = FALSE)
  }
  key <- substring(ref, nchar(prefix) + 1L)
  resolved <- schema[["$defs"]][[key]]
  if (is.null(resolved)) {
    stop("unresolved source-family schema reference: ", ref, call. = FALSE)
  }
  resolved
}

.source_schema_node_problems <- function(instance, node, schema, path = "$") {
  problems <- character(0L)
  add <- function(keyword, message) {
    problems[[length(problems) + 1L]] <<- paste0(
      "schema violation at ", path, " [", keyword, "]: ", message
    )
  }

  if (!is.null(node[["$ref"]])) {
    return(.source_schema_node_problems(
      instance,
      .schema_resolve_ref(schema, node[["$ref"]]),
      schema,
      path
    ))
  }

  if (!is.null(node$type) && !.schema_instance_type(instance, node$type)) {
    add("type", paste0("expected ", node$type))
    return(problems)
  }
  if (!is.null(node$const) && !identical(instance, node$const) &&
      !(is.numeric(instance) && is.numeric(node$const) &&
        length(instance) == 1L && length(node$const) == 1L &&
        isTRUE(instance == node$const))) {
    add("const", paste0("expected ", deparse(node$const)))
  }
  if (!is.null(node$enum)) {
    allowed <- unlist(node$enum, recursive = FALSE, use.names = FALSE)
    if (length(instance) != 1L || !any(vapply(
      allowed,
      function(value) identical(instance, value),
      logical(1)
    ))) {
      add(
        "enum",
        paste0(
          "expected one of ",
          paste(vapply(allowed, deparse, character(1)), collapse = ", ")
        )
      )
    }
  }

  if (.schema_instance_type(instance, "string")) {
    if (!is.null(node$minLength) && nchar(instance) < node$minLength) {
      add("minLength", paste0("minimum length is ", node$minLength))
    }
    if (!is.null(node$pattern) &&
        !grepl(node$pattern, instance, perl = TRUE)) {
      add("pattern", paste0("value does not match `", node$pattern, "`"))
    }
  }

  if (.schema_instance_type(instance, "array")) {
    values <- if (is.list(instance)) instance else as.list(instance)
    if (!is.null(node$minItems) && length(values) < node$minItems) {
      add("minItems", paste0("expected at least ", node$minItems, " item(s)"))
    }
    if (!is.null(node$items)) {
      for (i in seq_along(values)) {
        problems <- c(
          problems,
          .source_schema_node_problems(
            values[[i]], node$items, schema, paste0(path, "[", i, "]")
          )
        )
      }
    }
  }

  if (.schema_instance_type(instance, "number") &&
      !is.null(node$minimum) && instance < node$minimum) {
    add("minimum", paste0("minimum value is ", node$minimum))
  }

  if (.schema_instance_type(instance, "object")) {
    instance_names <- names(instance) %||% character(0L)
    required <- unlist(node$required %||% list(), use.names = FALSE)
    for (field in setdiff(required, instance_names)) {
      problems <- c(
        problems,
        paste0(
          "schema violation at ", path, ".", field,
          " [required]: required property is missing"
        )
      )
    }

    properties <- node$properties %||% list()
    if (identical(node$additionalProperties, FALSE)) {
      for (field in setdiff(instance_names, names(properties))) {
        problems <- c(
          problems,
          paste0(
            "schema violation at ", path, ".", field,
            " [additionalProperties]: unrecognized property"
          )
        )
      }
    }
    for (field in intersect(instance_names, names(properties))) {
      problems <- c(
        problems,
        .source_schema_node_problems(
          instance[[field]],
          properties[[field]],
          schema,
          paste0(path, ".", field)
        )
      )
    }
  }

  unique(problems)
}

.source_schema_problems <- function(spec, schema = .load_source_family_schema()) {
  .source_schema_node_problems(spec, schema, schema)
}

# Collect every structural problem with a source spec (named list) as a
# character vector of human-readable messages (empty when the spec is valid).
# Kept separate from the throwing validator so validate_source_config() can
# report all problems at once.
.source_spec_problems <- function(
    spec,
    schema_spec = spec) {
  p <- .source_schema_problems(schema_spec)
  add <- function(msg) p[[length(p) + 1L]] <<- msg

  scalar_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  string_vector <- function(x) {
    is.character(x) && length(x) > 0L && !anyNA(x) &&
      all(nzchar(trimws(x)))
  }

  version <- spec$schema_version %||% .ehr_source_definition_version
  if (length(version) != 1L || is.na(version) ||
      !identical(as.integer(version), .ehr_source_definition_version)) {
    add(paste0(
      "`schema_version` must be ",
      .ehr_source_definition_version
    ))
  }
  if (!scalar_string(spec$name))
    add("`name` must be a single non-empty string")
  if (!string_vector(spec$id_prefixes %||% spec$name))
    add("`id_prefixes` must contain non-empty strings")
  if (!string_vector(spec$select_id_cols))
    add("`select_id_cols` is required and must contain non-empty strings")
  if (!scalar_string(spec$time_origin))
    add("`time_origin` is required and must be a string (e.g. \"cw.admittime\")")
  if (!scalar_string(spec$cohort_key))
    add("`cohort_key` is required and must be a string")

  td <- spec$time_diff %||% "timestamp_minutes"
  if (!scalar_string(td) ||
      !td %in% c("timestamp_minutes", "offset_minutes"))
    add("`time_diff` must be 'timestamp_minutes' or 'offset_minutes'")

  idc <- spec$id_cols
  if (!is.list(idc) || !string_vector(idc$default))
    add("`id_cols$default` is required and must contain non-empty strings")
  else {
    if (!is.null(idc$icu) && !string_vector(idc$icu))
      add("`id_cols$icu` must contain non-empty strings")
    if (!is.null(idc$tables_with_icu) &&
        !string_vector(idc$tables_with_icu))
      add("`id_cols$tables_with_icu` must contain non-empty strings")
    if (!is.null(idc$tables_with_icu) && is.null(idc$icu))
      add("`id_cols$icu` is required when `id_cols$tables_with_icu` is set")
  }

  if (!scalar_string(spec$icu_id_type %||% "bigint"))
    add("`icu_id_type` must be a single non-empty string")
  if (!is.null(spec$cohort_seed_tables) &&
      !string_vector(spec$cohort_seed_tables))
    add("`cohort_seed_tables` must contain non-empty strings")
  if (!is.null(spec$source_names) && !string_vector(spec$source_names))
    add("`source_names` must contain non-empty strings")

  row_limit <- spec$variants$demo$row_limit %||% 1000L
  if (length(row_limit) != 1L || is.na(row_limit) ||
      !is.numeric(row_limit) || !is.finite(row_limit) ||
      row_limit <= 0 || row_limit != as.integer(row_limit)) {
    add("`variants$demo$row_limit` must be one positive finite integer")
  }

  # Declarative ICD selector (optional at registration time, but if present
  # must be well-formed). A source without one registers fine, but the cohort
  # engine cannot select its cohorts by ICD code.
  cf <- spec$cohort_filter
  if (!is.null(cf)) {
    if (!is.list(cf)) {
      add("`cohort_filter` must be a list (diag_table, code_col, key)")
    } else {
      for (f in c("diag_table", "code_col", "key")) {
        v <- cf[[f]]
        if (is.null(v) || !is.character(v) || length(v) != 1L || !nzchar(v))
          add(paste0("`cohort_filter$", f, "` is required and must be a string"))
      }
    }
  }

  unique(p)
}

# Validate a source spec (named list) before it reaches the registry. Throws a
# clear, source-prefixed error on the first problem.
.validate_source_spec <- function(
    spec,
    schema_spec = spec) {
  nm      <- spec$name %||% "<unnamed>"
  probs   <- .source_spec_problems(spec, schema_spec = schema_spec)
  if (length(probs))
    stop("register_source('", nm, "'): ", probs[[1L]], call. = FALSE)
  invisible(TRUE)
}

# Register one source spec (named list). Enforces the built-in overwrite guard,
# validates, then writes to the registry via .register_source_family().
.source_prefix_problems <- function(spec, registry, replace_name = NULL) {
  prefixes <- as.character(spec$id_prefixes %||% spec$name)
  others <- setdiff(names(registry %||% list()), replace_name)
  existing <- unlist(
    lapply(others, function(nm) registry[[nm]]$id_prefixes),
    use.names = FALSE
  )
  collisions <- unique(c(
    intersect(prefixes, existing),
    prefixes[vapply(prefixes, function(x) {
      any(startsWith(existing, x) | startsWith(x, existing))
    }, logical(1))]
  ))
  if (!length(collisions)) return(character())
  paste0(
    "`id_prefixes` collide or overlap with registered routing: ",
    paste(collisions, collapse = ", ")
  )
}

.prepare_source_spec <- function(
    spec,
    overwrite = FALSE,
    schema_spec = spec,
                                 registry = .ehr_env$source_registry) {
  if (!is.list(spec)) {
    stop("register_source(): definition must be one JSON object.", call. = FALSE)
  }
  if (!is.null(spec$overwrite)) {
    stop(
      "register_source(): JSON `overwrite` is not supported. Replacement ",
      "requires the explicit R `overwrite = TRUE` argument.",
      call. = FALSE
    )
  }
  name <- spec$name

  if (!is.null(name) && name %in% .ehr_builtin_families &&
      !isTRUE(overwrite) && !is.null(.ehr_env$source_registry[[name]]))
    stop("register_source('", name, "'): '", name,
         "' is a built-in source; pass overwrite = TRUE to replace it.",
         call. = FALSE)

  .validate_source_spec(spec, schema_spec = schema_spec)
  if (!isTRUE(overwrite) && !is.null(registry[[name]])) {
    stop(
      "register_source('", name, "'): already registered; use the explicit ",
      "R `overwrite = TRUE` argument to replace it.",
      call. = FALSE
    )
  }
  prefix_problems <- .source_prefix_problems(
    spec,
    registry,
    replace_name = if (isTRUE(overwrite)) name else NULL
  )
  if (length(prefix_problems)) {
    stop(
      "register_source('", name, "'): ",
      paste(prefix_problems, collapse = "; "),
      call. = FALSE
    )
  }

  demo_row_limit <- spec$variants$demo$row_limit %||% 1000L
  list(
    name = name,
    values = list(
      name               = name,
      id_prefixes        = as.character(spec$id_prefixes %||% name),
      select_id_cols     = as.character(spec$select_id_cols),
      time_origin        = spec$time_origin,
      icu_id_type        = spec$icu_id_type %||% "bigint",
      time_diff          = spec$time_diff %||% "timestamp_minutes",
      id_cols            = spec$id_cols,
      cohort_key         = spec$cohort_key,
      cohort_seed_tables = spec$cohort_seed_tables,
      cohort_filter      = spec$cohort_filter,
      demo_row_limit     = as.integer(demo_row_limit),
      source_names       = as.character(spec$source_names %||%
                                          spec$id_prefixes %||% name),
      release_metadata   = spec$release_metadata
    )
  )
}

.commit_prepared_source <- function(prepared) {
  do.call(.register_source_family, prepared$values)
  invisible(prepared$name)
}

.register_source_spec <- function(spec, overwrite = FALSE) {
  prepared <- .prepare_source_spec(
    spec,
    overwrite = overwrite,
    schema_spec = spec
  )
  .commit_prepared_source(prepared)
}

# Parse one schema-versioned family definition and commit it atomically.
.register_source_from_json <- function(path, overwrite = FALSE) {
  if (!file.exists(path))
    stop("register_source(): config file not found: ", path, call. = FALSE)
  # Preserve JSON array/object distinctions for strict schema validation; the
  # simplified parse is retained separately for the package's R-level values.
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  parsed <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                               simplifyDataFrame = FALSE)
  if (!is.list(parsed) || is.null(parsed$name)) {
    stop(
      "register_source(): each JSON file must contain exactly one source ",
      "family object; multi-family arrays are retired.",
      call. = FALSE
    )
  }
  if (is.null(parsed$schema_version)) {
    stop("register_source(): JSON `schema_version` is required.", call. = FALSE)
  }
  prepared <- .prepare_source_spec(
    parsed,
    overwrite = overwrite,
    schema_spec = raw
  )
  .commit_prepared_source(prepared)
}

# Auto-discovery: register every source defined under EHR_EXTERNAL_CONFIG_ROOT
# sources/*.json. Called from .onLoad();
# a failing file warns and is skipped so it can never break package loading.
.register_config_root_sources <- function() {
  root <- ehr_config_root()
  if (is.null(root)) return(invisible(character(0L)))

  files <- character(0L)
  sdir <- file.path(root, "sources")
  if (dir.exists(sdir))
    files <- c(files, sort(list.files(sdir, pattern = "\\.json$", full.names = TRUE),
                           method = "radix"))

  registered <- character(0L)
  for (f in files) {
    res <- tryCatch(
      .register_source_from_json(f, overwrite = FALSE),
      error = function(e) {
        warning("Skipping source config '", f, "': ", conditionMessage(e),
                call. = FALSE)
        character(0L)
      }
    )
    registered <- c(registered, res)
  }
  invisible(registered)
}

#' Register a data source family
#'
#' Makes a new source known to the whole pipeline (cohort building, concept
#' extraction, harmonization, access) without editing package code. Supply the
#' configuration either inline or as a path to a schema-versioned family JSON.
#' Sources placed under `EHR_EXTERNAL_CONFIG_ROOT/sources` are registered
#' automatically when
#' the package loads; call this directly to register within a session.
#'
#' @param name Family name, e.g. `"amsterdam"`. Its demo/subcohort variants
#'   (`amsterdam_demo`, `amsterdam_demo_cervical`) resolve to it automatically.
#'   Omit when using `config`.
#' @param ... Inline configuration fields: `select_id_cols`, `time_origin`,
#'   `time_diff` (`"timestamp_minutes"` or `"offset_minutes"`), `id_cols`
#'   (list with `default`, optional `icu`/`tables_with_icu`), `cohort_key`,
#'   `cohort_seed_tables`, `cohort_filter`, `id_prefixes`, `icu_id_type`, and
#'   `variants` (e.g. `list(demo = list(row_limit = 1000L))`).
#' @param config Optional path to one schema-versioned family JSON object.
#'   Mutually exclusive with inline `...` fields.
#' @param overwrite Allow replacing a built-in source (`mimic_iv`, `mimic_iii`,
#'   `eicu`). Replacement is always explicit; JSON cannot request overwrite.
#' @return The registered source name(s), invisibly.
#' @examples
#' \dontrun{
#' register_source(config = "config/sources/my_icu.json")
#' list_registered_sources()
#' }
#' @export
register_source <- function(name = NULL, ..., config = NULL, overwrite = FALSE) {
  if (!is.null(config)) {
    if (length(list(...)) > 0L || !is.null(name))
      stop("register_source(): supply either `config` or inline fields, not both.",
           call. = FALSE)
    return(.register_source_from_json(config, overwrite = overwrite))
  }
  spec <- list(...)
  spec$name <- name
  spec$schema_version <- spec$schema_version %||%
    .ehr_source_definition_version
  .register_source_spec(spec, overwrite = overwrite)
}

#' List registered source families
#'
#' Returns the family names currently known to the source registry (built-ins
#' plus any registered via [register_source()] or discovered under
#' `EHR_EXTERNAL_CONFIG_ROOT`). Unlike [list_sources()], this does not require a
#' database connection and reflects registration, not materialization.
#'
#' @return Character vector of registered family names.
#' @examples
#' list_registered_sources()
#' @export
list_registered_sources <- function() {
  sort(names(.ehr_env$source_registry %||% list()))
}

# ---------------------------------------------------------------------------
# Source scaffolder: add_source_wizard() + validate_source_config()
# ---------------------------------------------------------------------------

# Canonical column header for rendehr_concepts_config.csv. Used as a fallback
# to scaffold a new source's concept config when the packaged copy (the source
# of truth) can't be located at scaffold time.
.ehr_concept_config_header <- paste(
  "concept_name", "source", "match_value", "dict", "category", "unit",
  "min_value", "max_value", "description", "table_name",
  "sub_var", "val_var", "unit_var", "index_var", "itm_class", "time_precision",
  "check", "conversion", "conversion_note", "method", "typ", "end_var", "ref",
  sep = ",")

# Prefer the packaged CSV's real header so a scaffolded config never drifts from
# what run_atomic_concepts_unified() expects; fall back to the constant above.
.ehr_concept_config_header_line <- function() {
  pkg <- tryCatch(ehr_config_path("rendehr_concepts_config.csv"),
                  error = function(e) NULL)
  if (!is.null(pkg) && file.exists(pkg)) {
    hdr <- tryCatch(readLines(pkg, n = 1L), error = function(e) NULL)
    if (length(hdr) == 1L && nzchar(hdr)) return(hdr)
  }
  .ehr_concept_config_header
}

# Directories created under <root>/sql/<family>/ for a new source. Mirrors the
# packaged inst/sql/{family}/ layout that ehr_paths() resolves against.
.ehr_source_sql_dirs <- c(
  "db_setup_sqls/schema",
  "raw_mods",
  "concepts_sqls/flat",
  "concepts_sqls/ts",
  "concepts_sqls/dur",
  "concepts_sqls/ehr"
)

# Write `lines` to `path` only if it does not already exist. Returns the path
# when created, else NULL (so callers can report what was actually scaffolded).
.ehr_write_if_absent <- function(path, lines) {
  if (file.exists(path)) return(NULL)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  path
}

# Skeleton contents for the three db_setup SQL files a source needs. Each uses
# `SET search_path TO {raw_schema}` + bare table names, matching the convention
# in inst/sql (schema is injected by the runner, never hardcoded).
.ehr_source_setup_sql <- function(name) {
  list(
    "db_setup_sqls/schema/create_tables.sql" = c(
      paste0("-- Raw-table DDL for source '", name, "'."),
      "-- Runs first, into the {src}_coh schema (search_path already set).",
      "-- Add one CREATE TABLE per raw parquet table this source imports.",
      "--",
      "-- CREATE TABLE example_table (",
      "--   patient_id  BIGINT,",
      "--   charttime   TIMESTAMP,",
      "--   value       DOUBLE PRECISION",
      "-- );"
    ),
    "db_setup_sqls/window_tables.sql" = c(
      paste0("-- Visit/ICU windows for source '", name, "'."),
      "-- REQUIRED: must create admission_windows, icu_windows, and",
      "-- visit_windows in the raw schema. visit_windows is the admission-to-ICU",
      "-- crosswalk and defines time_origin for every concept.",
      "--",
      "-- DROP TABLE IF EXISTS visit_windows;",
      "-- CREATE TABLE visit_windows AS",
      "-- SELECT ... ;   -- must expose the source's time_origin column"
    ),
    "db_setup_sqls/indexes.sql" = c(
      paste0("-- Optional indexes for source '", name, "' (safe to leave empty)."),
      "-- CREATE INDEX ON some_table (patient_id);"
    )
  )
}

# The registry entry add_source_wizard() writes; ehr_add_source_assistant()
# validates the same entry in its dry run.
.source_wizard_spec <- function(name, id_prefixes = NULL, select_id_cols = NULL,
                                time_origin = NULL, time_diff = "timestamp_minutes",
                                icu_id_type = "bigint", id_cols = NULL,
                                cohort_key = NULL, cohort_seed_tables = NULL,
                                cohort_filter = NULL, demo_row_limit = 1000L) {
  spec <- list(
    schema_version     = .ehr_source_definition_version,
    name               = name,
    id_prefixes        = id_prefixes,
    select_id_cols     = select_id_cols,
    time_origin        = time_origin,
    time_diff          = time_diff,
    icu_id_type        = icu_id_type,
    id_cols            = id_cols,
    cohort_key         = cohort_key,
    cohort_seed_tables = cohort_seed_tables,
    cohort_filter      = cohort_filter,
    variants           = list(demo = list(row_limit = as.integer(demo_row_limit)))
  )
  # Optional values that were not supplied are absent properties, not JSON
  # nulls. Required NULL placeholders remain in incomplete scaffolds so schema
  # validation identifies exactly what the user still needs to fill in.
  for (field in c("id_prefixes", "cohort_seed_tables", "cohort_filter")) {
    if (is.null(spec[[field]])) spec[[field]] <- NULL
  }
  spec
}

#' Scaffold a new data source
#'
#' One-call scaffolder for adding a source family without editing the package.
#' It writes, under `root` (default `EHR_EXTERNAL_CONFIG_ROOT`):
#'
#' * `sources/<name>.json` — a registry skeleton (fed to [register_source()]).
#' * `sql/<name>/…` — the SQL directory tree (`db_setup_sqls/schema`,
#'   `raw_mods`, `concepts_sqls/{flat,ts,dur,ehr}`) with skeleton
#'   `create_tables.sql`, `window_tables.sql`, and `indexes.sql`.
#' * `rendehr_concepts_config.csv` — a header-only concept config (created
#'   only if absent), where source-specific concept rows are added.
#'
#' Existing files are never clobbered unless `overwrite = TRUE`. When the
#' supplied fields form a complete, valid spec the source is also registered for
#' the current session (`register = TRUE`); an incomplete skeleton is written
#' but not registered, and the missing fields are reported.
#'
#' @param name Family name, e.g. `"amsterdam"`.
#' @param root Target config directory. Defaults to
#'   `EHR_EXTERNAL_CONFIG_ROOT`;
#'   created if it does not exist.
#' @param select_id_cols,time_origin,time_diff,id_cols,cohort_key,cohort_seed_tables,id_prefixes,icu_id_type
#'   Registry fields (see [register_source()]). Required fields left `NULL`
#'   (`select_id_cols`, `time_origin`, `id_cols`, `cohort_key`) are written as
#'   `null` placeholders for you to fill in; optional ones are omitted.
#' @param cohort_filter Declarative cohort spec (list with `diag_table`,
#'   `code_col`, `key`, optional `split_commas`/`strip_dots`).
#' @param demo_row_limit Row cap for the `<name>_demo` variant. Default 1000.
#' @param overwrite Overwrite an existing `sources/<name>.json`? Default FALSE.
#' @param register Register the source this session when the spec is valid?
#'   Default TRUE.
#' @return Invisibly, a list with `root`, `json` (registry path), `created`
#'   (paths written), `registered` (logical), and `problems` (spec problems, if
#'   any, that blocked registration).
#' @examples
#' \dontrun{
#' add_source_wizard("my_icu", root = "~/data/my_icu",
#'                   select_id_cols = c("patient_id", "admission_id", "stay_id"))
#' }
#' @export
add_source_wizard <- function(name,
                              root               = NULL,
                              select_id_cols     = NULL,
                              time_origin        = NULL,
                              time_diff          = "timestamp_minutes",
                              id_cols            = NULL,
                              cohort_key         = NULL,
                              cohort_seed_tables = NULL,
                              cohort_filter      = NULL,
                              id_prefixes        = NULL,
                              icu_id_type        = "bigint",
                              demo_row_limit     = 1000L,
                              overwrite          = FALSE,
                              register           = TRUE) {
  if (missing(name) || !is.character(name) || length(name) != 1L || !nzchar(name))
    stop("add_source_wizard(): `name` must be a single non-empty string.",
         call. = FALSE)

  root <- root %||% .ehr_config_root_value()
  if (!nzchar(root))
    stop("add_source_wizard(): no `root` given and ",
         "EHR_EXTERNAL_CONFIG_ROOT is unset. Pass root = ... or set ",
         "EHR_EXTERNAL_CONFIG_ROOT.",
         call. = FALSE)
  root <- path.expand(root)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  created <- character(0L)

  # 1. Registry skeleton. NULL fields are kept and rendered as JSON `null`
  # placeholders (via null = "null" below) so the file shows what to fill in.
  spec <- .source_wizard_spec(
    name = name, id_prefixes = id_prefixes, select_id_cols = select_id_cols,
    time_origin = time_origin, time_diff = time_diff, icu_id_type = icu_id_type,
    id_cols = id_cols, cohort_key = cohort_key,
    cohort_seed_tables = cohort_seed_tables, cohort_filter = cohort_filter,
    demo_row_limit = demo_row_limit)

  sources_dir <- file.path(root, "sources")
  dir.create(sources_dir, recursive = TRUE, showWarnings = FALSE)
  json_path <- file.path(sources_dir, paste0(name, ".json"))
  if (file.exists(json_path) && !isTRUE(overwrite))
    stop("add_source_wizard(): registry file already exists: ", json_path,
         "\nPass overwrite = TRUE to replace it.", call. = FALSE)
  writeLines(
    jsonlite::toJSON(spec, pretty = TRUE, auto_unbox = TRUE, null = "null"),
    json_path)
  created <- c(created, json_path)

  # 2. SQL directory tree + skeleton db_setup files.
  sql_base <- file.path(root, "sql", name)
  for (d in .ehr_source_sql_dirs)
    dir.create(file.path(sql_base, d), recursive = TRUE, showWarnings = FALSE)
  for (rel in names(.ehr_source_setup_sql(name))) {
    w <- .ehr_write_if_absent(file.path(sql_base, rel),
                              .ehr_source_setup_sql(name)[[rel]])
    if (!is.null(w)) created <- c(created, w)
  }

  # 3. Header-only concept config (only if none exists yet at root).
  cfg_csv <- file.path(root, "rendehr_concepts_config.csv")
  w <- .ehr_write_if_absent(cfg_csv, .ehr_concept_config_header_line())
  if (!is.null(w)) created <- c(created, w)

  # 4. Register now if the spec is complete; otherwise leave a skeleton to edit.
  problems   <- .source_spec_problems(spec)
  registered <- FALSE
  if (isTRUE(register)) {
    if (length(problems) == 0L) {
      .register_source_spec(spec, overwrite = TRUE)
      registered <- TRUE
    } else {
      warning("add_source_wizard(): '", name, "' scaffolded but not registered ",
              "- fill in: ", paste(problems, collapse = "; "), call. = FALSE)
    }
  }

  message("add_source_wizard: scaffolded '", name, "' under ", root,
          " (", length(created), " file(s) written",
          if (registered) ", registered" else "", ").")
  invisible(list(root = root, json = json_path, created = created,
                 registered = registered, problems = problems))
}

#' Validate a source configuration
#'
#' Checks that a source spec is structurally complete enough for the pipeline to
#' use, reporting every problem at once (unlike [register_source()], which stops
#' at the first). Accepts a registered family name, an in-memory spec list, or a
#' path to a `source-registry.json` file. When a config `root` is known, it also
#' warns about missing SQL scaffolding (`window_tables.sql`, schema DDL).
#'
#' @param config A spec list, or a path to a `source-registry.json` (single
#'   object or array). Mutually exclusive with `name`.
#' @param name A registered family name to validate from the live registry.
#' @param root Config root for SQL-presence checks. Defaults to
#'   `EHR_EXTERNAL_CONFIG_ROOT`.
#' @param check_sql Check for expected SQL scaffold files when `root` is
#'   available? Default TRUE.
#' @param quiet Suppress the summary message? Default FALSE.
#' @return Invisibly, a list with `ok`, `problems`, and `warnings`.
#' @examples
#' \dontrun{
#' validate_source_config(config = "config/sources/my_icu.json")
#' }
#' @export
validate_source_config <- function(config = NULL, name = NULL, root = NULL,
                                   check_sql = TRUE, quiet = FALSE) {
  if (!is.null(config) && !is.null(name))
    stop("validate_source_config(): supply either `config` or `name`, not both.",
         call. = FALSE)

  # Resolve one or more specs to check.
  if (!is.null(name)) {
    entry <- .source_entry(name)
    if (is.null(entry))
      stop("validate_source_config(): source '", name, "' is not registered.",
           call. = FALSE)
    entry$name <- src_family(name)
    specs <- list(entry)
    schema_entry <- entry
    schema_entry$schema_version <- .ehr_source_definition_version
    schema_entry$variants <- list(
      demo = list(row_limit = entry$demo_row_limit %||% 1000L)
    )
    schema_entry$demo_row_limit <- NULL
    schema_entry <- Filter(Negate(is.null), schema_entry)
    schema_specs <- list(schema_entry)
  } else if (is.character(config) && length(config) == 1L) {
    if (!file.exists(config))
      stop("validate_source_config(): config file not found: ", config,
           call. = FALSE)
    raw <- jsonlite::fromJSON(config, simplifyVector = FALSE)
    parsed <- jsonlite::fromJSON(config, simplifyVector = TRUE,
                                 simplifyDataFrame = FALSE)
    specs <- if (!is.null(parsed$name)) list(parsed) else parsed
    schema_specs <- if (!is.null(raw$name)) list(raw) else raw
  } else if (is.list(config)) {
    specs <- if (!is.null(config$name)) list(config) else config
    schema_specs <- specs
  } else {
    stop("validate_source_config(): provide `name`, a spec list, or a JSON path.",
         call. = FALSE)
  }

  root     <- root %||% ehr_config_root() %||% ""
  problems <- character(0L)
  warnings <- character(0L)
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    nm <- spec$name %||% "<unnamed>"
    for (msg in .source_spec_problems(
      spec,
      schema_spec = schema_specs[[i]] %||% spec
    ))
      problems <- c(problems, paste0("[", nm, "] ", msg))
    if (is.null(spec$cohort_filter) && !identical(nm, "eicu"))
      warnings <- c(warnings, paste0("[", nm, "] no cohort_filter; ",
                                     "cohorts cannot be selected by ICD code"))
    if (isTRUE(check_sql) && nzchar(root)) {
      base <- file.path(path.expand(root), "sql", nm)
      wt   <- file.path(base, "db_setup_sqls", "window_tables.sql")
      sc   <- file.path(base, "db_setup_sqls", "schema", "create_tables.sql")
      if (!file.exists(wt))
        warnings <- c(warnings, paste0("[", nm, "] missing ", wt))
      if (!file.exists(sc))
        warnings <- c(warnings, paste0("[", nm, "] missing ", sc))
    }
  }

  result <- list(ok = length(problems) == 0L, problems = problems,
                 warnings = warnings)
  if (!isTRUE(quiet)) {
    if (result$ok) message("validate_source_config: OK",
                           if (length(warnings)) paste0(" (", length(warnings),
                                                        " warning(s))") else "")
    else message("validate_source_config: ", length(problems), " problem(s).")
    for (p in problems) message("  problem: ", p)
    for (w in warnings) message("  warning: ", w)
  }
  invisible(result)
}

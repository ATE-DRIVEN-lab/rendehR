# Source name a SQL directory was written for: the path segment directly
# above "concepts_sqls", or `default` when the path has none.
.ehr_sql_template_src <- function(sql_dir, default) {
  parts <- strsplit(gsub("\\\\", "/", sql_dir), "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  i <- match("concepts_sqls", parts)
  if (!is.na(i) && i > 1L) parts[i - 1L] else default
}

#' Run MIMIC-code / eICU-code handcrafted SQL concepts.
#'
#' Executes the adapted SQL scripts in `sql_dir` (by default the project's
#' `concepts_sqls/external/` directory; `run_concepts()` also uses it for the
#' packaged `concepts_sqls/ehr/` layer) in dependency order. Tables are written to `{src_name}_coh_derived`.
#'
#' The SQL files were converted from mimic-code for a specific demo schema.
#' When `src_name` differs from the schema baked into the files, schema names
#' are substituted on-the-fly before execution (demo -> full, or vice versa).
#'
#' The "comorbidity" and "score" categories (e.g. `charlson.sql`,
#' `elixhauser_*.sql`, `apsiii.sql`, `sapsii.sql`, and the MIMIC-III
#' `sepsis/{angus,explicit,martin}.sql` and `diagnosis/ccs_dx.sql` files) are
#' unmodified, third-party clinical definitions from the public MIT-LCP
#' `mimic-code`/`eicu-code` repositories. They are executed unconditionally
#' whenever the source's packaged `mimic_code`/`eicu_code` directory exists.
#' This package does not independently re-validate their clinical content
#' (ICD code lists, scoring formulas), consistent with how `icdpicr`/
#' `icdpicr2` are treated as trusted external dependencies. In particular,
#' `charlson.sql`/`elixhauser_*.sql` here are a *separate*, independently
#' computed comorbidity output from `run_comorbidities()`'s accepted
#' `raw.comorbidities` table: they live in a different schema
#' (`{src}_coh_derived` vs. `raw`) under different table names and use
#' different underlying logic (community SQL vs. the `comorbidity` R
#' package). `raw.comorbidities` is this package's accepted comorbidity
#' output; the tables produced here are a community reference, not a
#' replacement for it.
#'
#' @param con DBI connection (search_path will be set via postgres-functions.sql).
#' @param paths List from ehr_paths(). Must include `external_sql_dir` when
#'   `sql_dir` is not given.
#' @param schema_names List from src_schema_names(). Must include `raw` and
#'   `derived`.
#' @param src_name Source name used for schema adaptation.
#' @param sql_dir Optional SQL directory. Defaults to `paths$external_sql_dir`.
#' @param categories Character vector of category subdirs to run. NULL = all.
#'   Valid values: "demographics", "measurement", "comorbidity", "medication",
#'   "treatment", "firstday", "organfailure", "score", "sepsis".
#' @param skip_on_error If TRUE, log errors and continue; if FALSE, stop on
#'   first failure (default FALSE).
#' @param execute If TRUE, execute adapted SQL. If FALSE, only adapt/save SQL.
#' @param save_adapted If TRUE, save adapted SQL files.
#' @param save_to Optional output directory for adapted SQL.
#' @param target_schema Optional schema to write user/external concepts to.
#' @return Invisibly returns a named logical vector: TRUE = success per file.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' run_external_concepts(con, paths = ehr_paths("mimic_iv_demo"),
#'   schema_names = src_schema_names("mimic_iv_demo"), src_name = "mimic_iv_demo")
#' }
#' @export
run_external_concepts <- function(con,
                                     paths,
                                     schema_names,
                                     src_name,
                                     sql_dir       = NULL,
                                     categories    = NULL,
                                     skip_on_error = FALSE,
                                     execute       = TRUE,
                                     save_adapted  = FALSE,
                                     save_to       = NULL,
                                     target_schema = NULL) {

  sql_dir <- sql_dir %||% paths$external_sql_dir
  if (!dir.exists(sql_dir)) {
    message("No external SQL directory for this source: ", sql_dir)
    return(invisible(logical(0)))
  }

  # Multi-module mode: if no top-level SQL files and no top-level manifest,
  # iterate each subdirectory as an independent module (each may have its own
  # postgres-make-concepts.sql). This handles the layout:
  #   external/
  #     mimic_code/postgres-make-concepts.sql  <-- manifest per module
  #     smith_2023/01_...sql
  top_sqls     <- list.files(sql_dir, pattern = "\\.sql$", recursive = FALSE)
  has_manifest <- file.exists(file.path(sql_dir, "postgres-make-concepts.sql"))
  subdirs      <- list.dirs(sql_dir, recursive = FALSE)

  if (!has_manifest && length(top_sqls) == 0L && length(subdirs) > 0L) {
    all_results <- lapply(subdirs, function(d) {
      message("\n-- External module: ", basename(d))
      run_external_concepts(
        con           = con,
        paths         = paths,
        schema_names  = schema_names,
        src_name      = src_name,
        sql_dir       = d,
        categories    = categories,
        skip_on_error = skip_on_error,
        execute       = execute,
        save_adapted  = save_adapted,
        save_to       = if (!is.null(save_to)) file.path(save_to, basename(d)) else NULL,
        target_schema = target_schema
      )
    })
    return(invisible(unlist(all_results)))
  }

  # Determine the schema baked into the SQL files (based on directory path).
  # The converted files in sql/mimic_iv_demo/... use mimic_iv_demo_coh[_derived].
  # The template src_name is the directory just above "concepts_sqls"
  # (sql/{src_name}/concepts_sqls/...), whatever the depth below it - a fixed
  # number of dirname() calls is only right for external/<module>. Without a
  # concepts_sqls segment, assume the files already target this source.
  template_src <- .ehr_sql_template_src(sql_dir, src_name)
  template_raw <- paste0(template_src, "_coh")
  template_drv <- paste0(template_src, "_coh_derived")

  actual_raw <- schema_names$raw
  actual_drv <- target_schema %||% schema_names$derived

  needs_substitution <- (template_raw != actual_raw)

  # Schema prefixes used in MIMIC-code / user SQL files.
  # Derived-schema prefixes are listed before raw-schema prefixes so that
  # e.g. "miiv_derived." is stripped before "miiv." (no conflict in fixed mode,
  # but explicit ordering avoids surprises).
  mimic_iv_prefixes  <- c(
    # official mimic-code prefixes
    "mimiciv_derived.", "mimiciv_hosp.", "mimiciv_icu.", "mimic_iv.",
    # physionet BigQuery-style backtick prefixes
    "`physionet-data.mimiciv_derived`.",
    "`physionet-data.mimiciv_hosp`.", "`physionet-data.mimiciv_icu`.",
    # common short aliases - derived/hosp/icu first, then raw
    "miiv_derived.", "miiv_hosp.", "miiv_icu.", "miiv.",
    "mimic4_derived.", "mimic4_hosp.", "mimic4_icu.", "mimic4.",
    "m4_derived.", "m4_hosp.", "m4_icu.", "m4."
  )
  mimic_iii_prefixes <- c(
    # official mimic-code prefixes
    "mimiciii.", "mimic_iii.",
    # common short aliases - derived first, then raw
    "miii_derived.", "miii.",
    "mimic3_derived.", "mimic3.",
    "m3_derived.", "m3."
  )
  eicu_prefixes      <- c(
    # BigQuery-style fully-backtick-quoted refs used by some eicu-code files
    # (`physionet-data.eicu_crd.table`, `physionet-data.eicu_crd_derived.table`).
    # Unlike MIMIC's `physionet-data.mimiciv_derived`.table pattern (backtick
    # closes right after the dataset, before a bare table name), eICU's
    # convention closes the backtick only after the table name, so strip the
    # project+dataset+dot here (longer patterns first, so the plain
    # "eicu_crd." below can't partially match inside "eicu_crd_derived.") -
    # the dangling closing backtick is stripped unconditionally below.
    "`physionet-data.eicu_crd_derived.",
    "`physionet-data.eicu_crd.",
    "eicu_crd.", "eicu."
  )

  src_prefixes <- switch(src_family(src_name),
    mimic_iv  = mimic_iv_prefixes,
    mimic_iii = mimic_iii_prefixes,
    eicu      = eicu_prefixes,
    character(0)
  )

  .adapt_sql <- function(sql_text) {
    for (p in src_prefixes)
      sql_text <- gsub(p, "", sql_text, fixed = TRUE)
    # Backticks are never valid Postgres syntax; any left after prefix
    # stripping are dangling BigQuery quote characters (e.g. the trailing
    # "`" left by the eicu_prefixes patterns above) - safe to drop globally.
    sql_text <- gsub("`", "", sql_text, fixed = TRUE)
    if (needs_substitution) {
      sql_text <- gsub(template_drv, actual_drv, sql_text, fixed = TRUE)
      sql_text <- gsub(template_raw, actual_raw, sql_text, fixed = TRUE)
    }
    sql_text
  }

  custom_dir <- save_to %||% paths$custom_sql_dir
  if (save_adapted)
    dir.create(custom_dir, recursive = TRUE, showWarnings = FALSE)

  if (execute) {
    DBI::dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS {actual_drv};"), immediate = TRUE)
    # Set search_path so bare table writes land in the derived schema and reads
    # fall back to the raw schema. Both mimic_iv (after prefix-stripping) and
    # mimic_iii (always bare) produce schema-less DDL that depends on this.
    .ehr_set_search_path(con, c(actual_drv, actual_raw))
  }

  .run_one <- function(sql_text, rel_path) {
    adapted <- .adapt_sql(sql_text)
    if (save_adapted) {
      out_path <- file.path(custom_dir, rel_path)
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      writeLines(adapted, out_path)
    }
    if (execute) {
      exec_text <- if (save_adapted) paste(readLines(file.path(custom_dir, rel_path), warn = FALSE), collapse = "\n")
                   else adapted
      DBI::dbExecute(con, exec_text, immediate = TRUE)
    } else {
      message("  (not executed)")
    }
  }

  # Run postgres-functions.sql first.
  funcs_sql <- file.path(sql_dir, "postgres-functions.sql")
  if (file.exists(funcs_sql)) {
    message(if (execute) "Running: " else "Adapting: ", "postgres-functions.sql")
    sql_text <- paste(readLines(funcs_sql, warn = FALSE), collapse = "\n")
    tryCatch(
      .run_one(sql_text, "postgres-functions.sql"),
      error = function(e) {
        if (skip_on_error) message("ERROR in postgres-functions.sql: ", conditionMessage(e))
        else stop("Failed: postgres-functions.sql\n", conditionMessage(e))
      }
    )
  }

  # Parse execution order from postgres-make-concepts.sql if present,
  # otherwise fall back to alphabetical order within each subdirectory.
  make_sql <- file.path(sql_dir, "postgres-make-concepts.sql")
  ordered_files <- if (file.exists(make_sql)) {
    lines <- readLines(make_sql, warn = FALSE)
    matches <- regmatches(lines, regexpr("^\\\\i\\s+(.+\\.sql)$", lines, perl = TRUE))
    paths   <- sub("^\\\\i\\s+", "", matches)
    paths[nzchar(paths)]
  } else {
    sort(list.files(sql_dir, pattern = "\\.sql$", recursive = TRUE,
                    full.names = FALSE), method = "radix")
  }

  # Never execute the manifest or functions bootstrap - these are control files.
  reserved <- c("postgres-make-concepts.sql", "postgres-functions.sql")
  ordered_files <- ordered_files[!basename(ordered_files) %in% reserved]

  # Filter by requested categories.
  if (!is.null(categories))
    ordered_files <- ordered_files[dirname(ordered_files) %in% categories]

  # Only keep files that actually exist in the directory.
  existing_files <- ordered_files[file.exists(file.path(sql_dir, ordered_files))]

  skipped <- setdiff(ordered_files, existing_files)
  if (length(skipped) > 0)
    message("Skipping (file not found): ", paste(skipped, collapse = ", "))

  results <- setNames(logical(length(existing_files)), existing_files)

  for (rel_path in existing_files) {
    message(if (execute) "Running: " else "Adapting: ", rel_path)
    sql_text <- paste(readLines(file.path(sql_dir, rel_path), warn = FALSE), collapse = "\n")
    ok <- tryCatch({
      .run_one(sql_text, rel_path)
      TRUE
    }, error = function(e) {
      if (skip_on_error) { message("ERROR in ", rel_path, ": ", conditionMessage(e)); FALSE }
      else stop("Failed: ", rel_path, "\n", conditionMessage(e))
    })
    results[rel_path] <- ok
  }

  n_ok  <- sum(results)
  n_tot <- length(results)
  status <- if (execute) "executed" else if (save_adapted) "adapted + saved" else "listed"
  message("Handcrafted concepts: ", n_ok, "/", n_tot, " files ", status, ".")

  invisible(results)
}

# ---------------------------------------------------------------------------
# Flat SQL concept runner
# ---------------------------------------------------------------------------

#' Execute flat SQL concept files for one source
#'
#' Reads every .sql file in `flat_sql_dir`, substitutes `{raw_schema}` and
#' `{concepts_schema}` with the actual schema names, then executes each file.
#' SQL files are expected to use `{raw_schema}` and `{concepts_schema}` as
#' placeholders - set by the bulk-replacement step during package setup.
#'
#' @param con DBI connection.
#' @param flat_sql_dir Path to the directory containing flat .sql files.
#' @param raw_schema Raw schema name (e.g. "mimic_iv_demo_coh").
#' @param concepts_schema Concepts schema name (e.g. "mimic_iv_demo_coh_concepts").
#' @param skip_on_error If TRUE, log errors and continue; stop on first failure
#'   if FALSE (default FALSE).
#' @param concept_name Optional character vector to restrict execution to
#'   specific concept file(s) (matched against the filename without `.sql`),
#'   instead of every file in `flat_sql_dir`.
#' @param dry_run If TRUE, adapt each matching file's SQL without executing
#'   it. `con` is not required in this mode. Default FALSE.
#' @return Invisibly returns a named logical vector (TRUE = success per file).
#'   When `dry_run = TRUE`, instead invisibly returns a named character vector
#'   of `file -> adapted SQL text`, and nothing is executed.
#' @examples
#' \dontrun{
#' con <- connect_postgres("rendehr", load_credentials("config/config_pass.json"))
#' run_flat_sql_concepts(con, flat_sql_dir = "sql/mimic_iv/flat",
#'   raw_schema = "mimic_iv_demo_coh", concepts_schema = "mimic_iv_demo_coh_concepts")
#' }
#' @export
run_flat_sql_concepts <- function(con = NULL, flat_sql_dir, raw_schema, concepts_schema,
                                  skip_on_error = FALSE, concept_name = NULL,
                                  dry_run = FALSE) {
  if (!dir.exists(flat_sql_dir)) {
    message("No flat SQL directory found: ", flat_sql_dir)
    return(invisible(logical(0)))
  }

  if (!isTRUE(dry_run) && is.null(con))
    stop("run_flat_sql_concepts(): `con` is required unless dry_run = TRUE.",
         call. = FALSE)

  files <- sort(list.files(flat_sql_dir, pattern = "\\.sql$", full.names = FALSE),
                method = "radix")
  if (!is.null(concept_name))
    files <- files[tools::file_path_sans_ext(files) %in% concept_name]
  if (length(files) == 0L) {
    message("No flat SQL files in: ", flat_sql_dir)
    return(invisible(logical(0)))
  }

  .adapt <- function(sql_text) {
    sql_text <- gsub("{concepts_schema}", concepts_schema, sql_text, fixed = TRUE)
    sql_text <- gsub("{raw_schema}",      raw_schema,      sql_text, fixed = TRUE)
    sql_text
  }

  if (isTRUE(dry_run)) {
    adapted <- vapply(files, function(f) {
      .adapt(paste(readLines(file.path(flat_sql_dir, f), warn = FALSE), collapse = "\n"))
    }, character(1L))
    message("Dry run: ", length(files), " flat concept file(s) adapted, not executed: ",
            paste(files, collapse = ", "))
    return(invisible(adapted))
  }

  results <- setNames(logical(length(files)), files)

  for (f in files) {
    path     <- file.path(flat_sql_dir, f)
    sql_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    adapted  <- .adapt(sql_text)

    message("Flat concept: ", f)
    guarded <- if (inherits(con, "duckdb_connection"))
      .ehr_unwrap_guarded_do(adapted) else NULL
    if (!is.null(guarded)) {
      present <- .ehr_table_exists(con, guarded$schema, guarded$table)
      if (!present)
        message("  ", guarded$schema, ".", guarded$table, " does not exist.")
      adapted <- paste(guarded$pre,
                       if (present) guarded$if_present else guarded$if_missing,
                       sep = "\n")
      if (.ehr_sql_is_empty(adapted)) {
        results[f] <- TRUE
        next
      }
    }
    ok <- tryCatch({
      DBI::dbExecute(con, adapted, immediate = TRUE)
      TRUE
    }, error = function(e) {
      if (skip_on_error) { message("  ERROR: ", conditionMessage(e)); FALSE }
      else stop("Failed flat concept: ", f, "\n", conditionMessage(e))
    })
    results[f] <- ok
  }

  n_ok <- sum(results)
  message("Flat concepts: ", n_ok, "/", length(files), " files executed.")
  invisible(results)
}

# The packaged flat concepts that read an optional table (run_iss() output,
# MIMIC-III noteevents) wrap it in a PL/pgSQL guard, in one of two forms:
#   DO $$ BEGIN IF to_regclass('<schema>.<table>') IS NULL THEN
#     RAISE NOTICE ...; RETURN; END IF; <statements> END $$;
#   DO $tag$ BEGIN IF to_regclass('<schema>.<table>') IS NULL THEN
#     <statements> ELSE EXECUTE $sql$ <statement> $sql$; END IF; END $tag$;
# DuckDB has no DO blocks, so there the guard is evaluated in R and only the
# plain statements of the chosen branch run, after whatever precedes the
# block. Returns NULL for any other SQL.
.ehr_unwrap_guarded_do <- function(sql_text) {
  head_rx <- paste0(
    "(?s)^(.*?)DO\\s+\\$(\\w*)\\$\\s*BEGIN\\s+IF\\s+",
    "to_regclass\\('([A-Za-z0-9_]+)\\.([A-Za-z0-9_]+)'\\)\\s+IS\\s+NULL\\s+THEN")
  notice <- regmatches(sql_text, regexec(paste0(head_rx,
    "\\s+RAISE\\s+NOTICE\\s+'[^']*';\\s*RETURN;\\s*END\\s+IF;",
    "(.*?)END\\s+\\$\\2\\$\\s*;?\\s*$"), sql_text, perl = TRUE))[[1]]
  if (length(notice))
    return(list(pre = trimws(notice[2]), schema = notice[4], table = notice[5],
                if_missing = "", if_present = trimws(notice[6])))
  branch <- regmatches(sql_text, regexec(paste0(head_rx,
    "(.*?)ELSE\\s+EXECUTE\\s+\\$sql\\$(.*?)\\$sql\\$\\s*;\\s*END\\s+IF;",
    "\\s*END\\s*\\$\\2\\$\\s*;?\\s*$"), sql_text, perl = TRUE))[[1]]
  if (length(branch))
    return(list(pre = trimws(branch[2]), schema = branch[4], table = branch[5],
                if_missing = trimws(branch[6]), if_present = trimws(branch[7])))
  NULL
}

# TRUE when SQL text holds nothing but whitespace and -- comments.
.ehr_sql_is_empty <- function(sql_text) {
  !nzchar(trimws(gsub("--[^\n]*", "", sql_text)))
}

# Run user/ SQL dir against a target schema.
# If manifest.csv is present, runs files in listed order.
# Otherwise runs alphabetically and prints a hint.
.run_user_sql_dir <- function(con, user_sql_dir, raw_schema, target_schema,
                              concept_name = NULL, dry_run = FALSE) {
  if (!dir.exists(user_sql_dir)) return(invisible(logical(0)))

  manifest_path <- file.path(user_sql_dir, "manifest.csv")
  has_manifest  <- file.exists(manifest_path)

  if (has_manifest) {
    manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
    files    <- manifest$sql_path
    message("  Using manifest.csv for execution order.")
  } else {
    # subfolders too: run_concepts() enables this layer when any .sql exists below user/
    files <- sort(list.files(user_sql_dir, pattern = "\\.sql$", recursive = TRUE,
                             full.names = FALSE), method = "radix")
    if (length(files) == 0L) return(invisible(logical(0)))
    message("  No manifest.csv found in user/ - running ", length(files),
            " file(s) alphabetically.\n",
            "  Tip: add a manifest.csv with a sql_path column (one file per",
            " row, in run order) to control execution order.")
  }

  if (!is.null(concept_name))
    files <- files[tools::file_path_sans_ext(basename(files)) %in% concept_name]
  if (length(files) == 0L) return(invisible(logical(0)))

  if (isTRUE(dry_run)) {
    adapted <- setNames(character(length(files)), files)
    for (f in files) {
      path <- file.path(user_sql_dir, f)
      adapted[f] <- if (file.exists(path)) {
        paste(readLines(path, warn = FALSE), collapse = "\n")
      } else {
        NA_character_
      }
    }
    message("Dry run: ", length(files), " user concept file(s), not executed: ",
            paste(files, collapse = ", "))
    return(invisible(adapted))
  }

  DBI::dbExecute(con,
    sprintf("CREATE SCHEMA IF NOT EXISTS %s", target_schema), immediate = TRUE)
  .ehr_set_search_path(con, c(target_schema, raw_schema))

  results <- setNames(logical(length(files)), files)
  for (f in files) {
    path <- if (has_manifest) file.path(user_sql_dir, f)
            else file.path(user_sql_dir, f)
    if (!file.exists(path)) {
      message("  SKIP (not found): ", f)
      next
    }
    sql_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    message("  User concept: ", basename(f))
    # The table is named after the file, so replace it: a plain CREATE TABLE
    # in the user's SQL must not make the second run fail.
    drop_sql <- sprintf(
      "DROP TABLE IF EXISTS %s.%s",
      DBI::dbQuoteIdentifier(con, target_schema),
      DBI::dbQuoteIdentifier(con, tools::file_path_sans_ext(basename(f)))
    )
    results[f] <- tryCatch({
      DBI::dbExecute(con, drop_sql, immediate = TRUE)
      DBI::dbExecute(con, sql_text, immediate = TRUE)
      TRUE
    }, error = function(e) {
      message("  ERROR: ", conditionMessage(e))
      FALSE
    })
  }
  invisible(results)
}

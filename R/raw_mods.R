#' Build modified (_mod) versions of raw tables for a source
#'
#' Runs SQL files from the source's `raw_mods` directory (package-embedded or
#' local override) to create preprocessed versions of raw tables. Each SQL file
#' produces one `{table}_mod` table in the raw schema, which the concept config
#' can reference directly.
#'
#' The package ships default mods for eICU:
#'   - `infusiondrug_mod` - parses the drug rate and its unit from `drugrate`
#'     and the bracket at the end of `drugname`
#'   - `medication_mod`   - parses the dose and unit from `dosage` and names
#'     orders with a blank `drugname` from their drug code
#'   - `admissiondrug_mod`- trims the home-medication name and unit, with the
#'     same `mod_*` column names as `medication_mod`
#'   - `lab_mod`, `respiratorycharting_mod` - clean FiO2 values to percent
#'
#' Before the SQL runs, the lookup tables it reads are written to the raw
#' schema: for eICU, `rendehr_eicu_hicl_drugname_lookup` (drug code to drug
#' name). A file of the same name in the project config directory replaces the
#' shipped one.
#'
#' MIMIC-IV and MIMIC-III have no default mods. Add your own SQL files to
#' `sql/{src_name}/raw_mods/` in your project directory to override or extend.
#'
#' @param src_name Source name (e.g. "eicu_demo").
#' @param db_name Optional Postgres database name. If NULL, uses the active
#'   project config, then "rendehr".
#' @param base_dir Project root directory. Defaults to getwd().
#' @param skip_on_error If TRUE, warn and continue evaluating the remaining SQL
#'   files, then fail the overall operation if any required file failed.
#'   Default FALSE.
#' @return Invisibly TRUE if mods were run, NULL if no mod directory found.
#' @examples
#' \dontrun{
#' run_raw_mods("eicu_demo", db_name = "rendehr")
#' }
#' @export
#' @param execution Optional overrides from `execution_options()` or a named list.
run_raw_mods <- function(src_name, db_name = NULL, base_dir = NULL,
                         skip_on_error = FALSE,
                         execution = NULL) {
  .ehr_apply_grouped_options(
    list(execution = execution),
    supplied = names(match.call(expand.dots = FALSE))[-1L],
    allowed = names(formals(run_raw_mods)), frame = environment())
  paths        <- ehr_paths(src_name = src_name, base_dir = base_dir)
  db_name      <- .ehr_resolve_db_name(db_name)
  schema_names <- src_schema_names(src_name)
  creds        <- load_credentials(paths$credentials_json)
  con          <- connect_postgres(db_name, creds)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql_dir <- paths$raw_mods_sql_dir

  if (!nzchar(sql_dir) || !dir.exists(sql_dir)) {
    message("No raw_mods SQL directory found for '", src_name, "' - skipping.")
    return(invisible(NULL))
  }

  files <- sort(list.files(sql_dir, pattern = "\\.sql$", full.names = TRUE),
                method = "radix")
  if (length(files) == 0) {
    message("raw_mods directory is empty for '", src_name, "' - skipping.")
    return(invisible(NULL))
  }

  raw_schema <- .ehr_validate_db_identifier(schema_names$raw, "raw schema")
  DBI::dbExecute(con,
    paste0("SET search_path TO ", DBI::dbQuoteIdentifier(con, raw_schema)),
    immediate = TRUE
  )
  .ehr_write_raw_mod_lookups(con, src_name, paths$config_dir, raw_schema)

  message("Building raw mods for '", src_name, "' (",
          length(files), " file(s)) ...")

  failed <- character()
  for (f in files) {
    tbl_name <- tools::file_path_sans_ext(basename(f))
    message("  ", tbl_name, " ...")
    if (skip_on_error) {
      tryCatch(
        run_sql_file(con, f),
        error = function(e) {
          failed <<- c(failed, tbl_name)
          warning("  FAILED: ", tbl_name, ": ", conditionMessage(e))
        }
      )
    } else {
      run_sql_file(con, f)
    }
  }

  if (length(failed))
    stop("Required raw modifications failed: ", paste(failed, collapse = ", "),
         call. = FALSE)
  message("Raw mods done: ", src_name)
  invisible(TRUE)
}

# Lookup tables read by a source family's raw-mod SQL. Each CSV is written to
# the raw schema as a table named after the file, before any raw-mod SQL runs,
# by run_raw_mods() and run_raw_mods_duckdb(). A file of the same name in the
# project config directory replaces the shipped one. The shipped eICU lookup
# names each drug code (drughiclseqno) from the eICU-CRD 2.0 medication rows
# that carry a name; name_rule says how the name was chosen and n_named_rows
# counts those rows.
.ehr_raw_mod_lookups <- list(
  eicu = "rendehr_eicu_hicl_drugname_lookup.csv"
)

.ehr_write_raw_mod_lookups <- function(con, src_name, config_dir, raw_schema) {
  for (f in .ehr_raw_mod_lookups[[src_family(src_name)]]) {
    path <- .ehr_pkg_or_local_cfg(f, config_dir, base_dir = NULL)
    if (!nzchar(path) || !file.exists(path))
      stop("Raw-mod lookup file not found: ", f, call. = FALSE)
    df <- utils::read.csv(path, colClasses = "character", na.strings = "")
    if ("drughiclseqno" %in% names(df))
      df$drughiclseqno <- as.integer(df$drughiclseqno)
    DBI::dbWriteTable(con,
      DBI::Id(schema = raw_schema, table = tools::file_path_sans_ext(f)),
      df, overwrite = TRUE)
  }
  invisible(NULL)
}

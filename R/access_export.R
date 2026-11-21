# ___________________________________________________________________________
# Large-table export - chunked streaming, no full-table in RAM
# ___________________________________________________________________________

# Internal: stream a DBI result set through a per-chunk callback.
# result  - DBI result set from dbSendQuery (NOT dbGetQuery)
# fn      - function(chunk_df, chunk_number) called for each chunk
# chunk_size - rows per chunk
# verbose - print progress messages
.stream_result <- function(result, fn, chunk_size = 1e5, verbose = TRUE) {
  chunk_n   <- 0L
  total_rows <- 0L

  tryCatch({
    repeat {
      chunk <- DBI::dbFetch(result, n = chunk_size)
      if (nrow(chunk) == 0L) break
      chunk_n    <- chunk_n + 1L
      total_rows <- total_rows + nrow(chunk)
      fn(chunk, chunk_n)
      if (verbose)
        message(sprintf("  chunk %d: %s rows written  (total: %s)",
                        chunk_n,
                        format(nrow(chunk),  big.mark = ","),
                        format(total_rows,   big.mark = ",")))
    }
  }, finally = {
    DBI::dbClearResult(result)
  })

  invisible(total_rows)
}

#' Export a table or query result to a Parquet dataset (chunked, low RAM)
#'
#' Reads `chunk_size` rows at a time and writes one `.parquet` file per chunk
#' into `output_dir`. The result is a multi-file Arrow dataset that can be
#' opened later with `arrow::open_dataset(output_dir)` - no size limit.
#'
#' When `output_dir` ends in `.parquet` (single-file mode), the chunk files
#' are merged into that one file with DuckDB (`COPY ... FROM read_parquet(...)`),
#' not by collecting them into an R data frame - peak memory stays bounded by
#' `chunk_size` throughout, including this final combine step, regardless of
#' table size.
#'
#' @param src_name Source name (e.g. "mimic_iv_demo"). Ignored when `query`
#'   is supplied directly.
#' @param table Table name to export. Ignored when `query` is supplied.
#' @param output_dir Directory to write `.parquet` files into. Created if
#'   it does not exist.
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string. Legacy aliases "ts", "flat", and "dur" resolve to "harmonized".
#'   Ignored when `query` is supplied.
#' @param query Optional: raw SQL string. Overrides src_name/table/schema.
#' @param chunk_size Rows per chunk. Default 100 000. Tune to your RAM.
#' @param overwrite If TRUE, delete output_dir contents first. Default FALSE.
#' @param verbose Print chunk progress. Default TRUE.
#' @param con Optional explicit DBI connection.
#' @return Invisibly returns the total number of rows written.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", creds)
#'
#' # Export a full table
#' export_table_parquet("mimic_iv_demo", "chartevents",
#'                      output_dir = "exports/chartevents_parquet")
#'
#' # Export a custom query
#' export_table_parquet(
#'   query      = "SELECT * FROM mimic_iv_demo_coh.chartevents WHERE itemid = 220045",
#'   output_dir = "exports/hr_parquet"
#' )
#'
#' # Read back without loading into RAM
#' arrow::open_dataset("exports/chartevents_parquet") |>
#'   dplyr::filter(value > 100) |>
#'   dplyr::collect()
#' }
export_table_parquet <- function(src_name = NULL, table = NULL,
                                 output_dir,
                                 schema = "raw",
                                 query = NULL,
                                 chunk_size = 1e5,
                                 overwrite = FALSE,
                                 verbose = TRUE,
                                 con = NULL) {
  if (is.null(query)) {
    if (is.null(src_name) || is.null(table))
      stop("Provide either (src_name + table) or query.")
    schema_names <- src_schema_names(src_name)
    schema_name  <- .ehr_schema_name(schema, schema_names)
    query <- sprintf("SELECT * FROM \"%s\".\"%s\"", schema_name, table)
  }

  single_file <- grepl("\\.parquet$", output_dir, ignore.case = TRUE)

  if (single_file) {
    if (overwrite && file.exists(output_dir)) unlink(output_dir)
    dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
    if (verbose) message("Exporting to parquet: ", output_dir)

    tmp_dir <- tempfile("ehr_parquet_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    .with_connection(con, function(con) {
      result <- DBI::dbSendQuery(con, query)
      .stream_result(result, chunk_size = chunk_size, verbose = verbose,
        fn = function(chunk, chunk_n) {
          arrow::write_parquet(chunk,
            file.path(tmp_dir, sprintf("part-%05d.parquet", chunk_n)))
        }
      )
    })

    # Merge the chunk files with DuckDB rather than
    # arrow::open_dataset() |> dplyr::collect() |> arrow::write_parquet():
    # collect() pulled the ENTIRE result into one R data frame before writing
    # it back out, which defeated the whole point of the bounded-memory
    # chunked streaming above for exactly the case (a huge table exported to
    # a single file) it mattered most for - measured at ~3x the on-disk
    # parquet size in R memory for the combine step alone. DuckDB's COPY
    # streams the read_parquet() glob directly to the output file with its
    # own out-of-core engine, so peak memory here no longer scales with the
    # total row count.
    if (verbose) message("Combining chunks into: ", output_dir)
    .duckdb_require()
    merge_con <- connect_duckdb(":memory:")
    on.exit(DBI::dbDisconnect(merge_con, shutdown = TRUE), add = TRUE)
    DBI::dbExecute(merge_con, sprintf(
      "COPY (SELECT * FROM read_parquet(%s)) TO %s (FORMAT PARQUET);",
      DBI::dbQuoteString(merge_con, file.path(tmp_dir, "*.parquet")),
      DBI::dbQuoteString(merge_con, output_dir)
    ))
  } else {
    if (overwrite && dir.exists(output_dir)) {
      unlink(list.files(output_dir, pattern = "\\.parquet$",
                        full.names = TRUE))
      if (verbose) message("Cleared existing parquet files in: ", output_dir)
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) message("Exporting to parquet directory: ", output_dir)

    .with_connection(con, function(con) {
      result <- DBI::dbSendQuery(con, query)
      .stream_result(result, chunk_size = chunk_size, verbose = verbose,
        fn = function(chunk, chunk_n) {
          out_file <- file.path(output_dir,
                                sprintf("part-%05d.parquet", chunk_n))
          arrow::write_parquet(chunk, out_file)
        }
      )
    })
  }
}

#' Export a table or query result to a gzip-compressed CSV (chunked, low RAM)
#'
#' Streams rows from Postgres directly into a `.csv.gz` file using a gzip
#' connection. Memory use is bounded by `chunk_size` regardless of table size.
#'
#' @param src_name Source name. Ignored when `query` is supplied.
#' @param table Table name to export. Ignored when `query` is supplied.
#' @param output_path Path for the output `.csv.gz` file.
#' @param schema "raw", "concepts", "derived", "harmonized", or a literal schema string. Legacy aliases "ts", "flat", and "dur" resolve to "harmonized".
#'   Ignored when `query` is supplied.
#' @param query Optional: raw SQL string. Overrides src_name/table/schema.
#' @param chunk_size Rows per chunk. Default 100 000.
#' @param verbose Print chunk progress. Default TRUE.
#' @param con Optional explicit DBI connection.
#' @return Invisibly returns the total number of rows written.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", creds)
#'
#' # Export a full table
#' export_table_csv_gz("mimic_iv_demo", "chartevents",
#'                     output_path = "exports/chartevents.csv.gz")
#'
#' # Export a filtered query
#' export_table_csv_gz(
#'   query       = "SELECT * FROM mimic_iv_demo_coh_harmonized.ts_tab
#'                  WHERE concept_name = 'heart_rate'",
#'   output_path = "exports/heart_rate.csv.gz"
#' )
#'
#' # Read back (arrow handles .gz natively)
#' arrow::read_csv_arrow("exports/chartevents.csv.gz")
#' }
export_table_csv_gz <- function(src_name = NULL, table = NULL,
                                output_path,
                                schema = "raw",
                                query = NULL,
                                chunk_size = 1e5,
                                verbose = TRUE,
                                con = NULL) {
  if (is.null(query)) {
    if (is.null(src_name) || is.null(table))
      stop("Provide either (src_name + table) or query.")
    schema_names <- src_schema_names(src_name)
    schema_name  <- .ehr_schema_name(schema, schema_names)
    query <- sprintf("SELECT * FROM \"%s\".\"%s\"", schema_name, table)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  if (verbose) message("Exporting to: ", output_path)

  gz_con <- gzfile(output_path, open = "wb")
  on.exit(close(gz_con), add = TRUE)

  .with_connection(con, function(con) {
    result <- DBI::dbSendQuery(con, query)
    .stream_result(result, chunk_size = chunk_size, verbose = verbose,
      fn = function(chunk, chunk_n) {
        readr::write_csv(chunk, gz_con,
                         col_names = (chunk_n == 1L),
                         append    = (chunk_n > 1L))
      }
    )
  })
}

#' Export any SQL query to parquet or csv.gz
#'
#' Convenience wrapper around export_table_parquet() / export_table_csv_gz()
#' that dispatches based on the output file extension.
#'
#' @param query SQL string to execute.
#' @param output_path Path for the output file or directory.
#'   - Ends in `.gz`       -> gzip CSV via export_table_csv_gz()
#'   - Ends in `.parquet`  -> single-chunk parquet file
#'   - Is a directory path -> multi-file parquet via export_table_parquet()
#' @param chunk_size Rows per chunk. Default 100 000.
#' @param verbose Print progress. Default TRUE.
#' @param con Optional explicit DBI connection.
#' @return Invisibly returns total rows written.
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_connect("rendehr", creds)
#'
#' export_query(
#'   "SELECT * FROM rendehr.ts WHERE concept_name = 'spo2'",
#'   "exports/spo2.csv.gz"
#' )
#'
#' export_query(
#'   "SELECT p.*, v.sci_level FROM mimic_iv_demo_coh.flat_tab p
#'    JOIN mimic_iv_demo_coh.cohort_info v USING (subject_id)",
#'   "exports/flat_with_sci"   # directory -> parquet dataset
#' )
#' }
export_query <- function(query, output_path,
                         chunk_size = 1e5,
                         verbose = TRUE,
                         con = NULL) {
  if (grepl("\\.gz$", output_path, ignore.case = TRUE)) {
    export_table_csv_gz(query = query, output_path = output_path,
                        chunk_size = chunk_size, verbose = verbose, con = con)
  } else {
    export_table_parquet(query = query, output_dir = output_path,
                         chunk_size = chunk_size, verbose = verbose, con = con)
  }
}


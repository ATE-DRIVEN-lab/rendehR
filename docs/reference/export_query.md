# Export any SQL query to parquet or csv.gz

Convenience wrapper around export_table_parquet() /
export_table_csv_gz() that dispatches based on the output file
extension.

## Usage

``` r
export_query(
  query,
  output_path,
  chunk_size = 1e+05,
  verbose = TRUE,
  con = NULL
)
```

## Arguments

- query:

  SQL string to execute.

- output_path:

  Path for the output file or directory.

  - Ends in `.gz` -\> gzip CSV via export_table_csv_gz()

  - Ends in `.parquet` -\> single-chunk parquet file

  - Is a directory path -\> multi-file parquet via
    export_table_parquet()

- chunk_size:

  Rows per chunk. Default 100 000.

- verbose:

  Print progress. Default TRUE.

- con:

  Optional explicit DBI connection.

## Value

Invisibly returns total rows written.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", creds)

export_query(
  "SELECT * FROM rendehr.ts WHERE concept_name = 'spo2'",
  "exports/spo2.csv.gz"
)

export_query(
  "SELECT p.*, v.sci_level FROM mimic_iv_demo_coh.flat_tab p
   JOIN mimic_iv_demo_coh.cohort_info v USING (subject_id)",
  "exports/flat_with_sci"   # directory -> parquet dataset
)
} # }
```

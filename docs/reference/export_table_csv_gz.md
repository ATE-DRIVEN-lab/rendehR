# Export a table or query result to a gzip-compressed CSV (chunked, low RAM)

Streams rows from Postgres directly into a `.csv.gz` file using a gzip
connection. Memory use is bounded by `chunk_size` regardless of table
size.

## Usage

``` r
export_table_csv_gz(
  src_name = NULL,
  table = NULL,
  output_path,
  schema = "raw",
  query = NULL,
  chunk_size = 1e+05,
  verbose = TRUE,
  con = NULL
)
```

## Arguments

- src_name:

  Source name. Ignored when `query` is supplied.

- table:

  Table name to export. Ignored when `query` is supplied.

- output_path:

  Path for the output `.csv.gz` file.

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string. Legacy aliases "ts", "flat", and "dur" resolve to
  "harmonized". Ignored when `query` is supplied.

- query:

  Optional: raw SQL string. Overrides src_name/table/schema.

- chunk_size:

  Rows per chunk. Default 100 000.

- verbose:

  Print chunk progress. Default TRUE.

- con:

  Optional explicit DBI connection.

## Value

Invisibly returns the total number of rows written.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", creds)

# Export a full table
export_table_csv_gz("mimic_iv_demo", "chartevents",
                    output_path = "exports/chartevents.csv.gz")

# Export a filtered query
export_table_csv_gz(
  query       = "SELECT * FROM mimic_iv_demo_coh_harmonized.ts_tab
                 WHERE concept_name = 'heart_rate'",
  output_path = "exports/heart_rate.csv.gz"
)

# Read back (arrow handles .gz natively)
arrow::read_csv_arrow("exports/chartevents.csv.gz")
} # }
```

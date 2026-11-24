# Export a table or query result to a Parquet dataset (chunked, low RAM)

Reads `chunk_size` rows at a time and writes one `.parquet` file per
chunk into `output_dir`. The result is a multi-file Arrow dataset that
can be opened later with `arrow::open_dataset(output_dir)` - no size
limit.

## Usage

``` r
export_table_parquet(
  src_name = NULL,
  table = NULL,
  output_dir,
  schema = "raw",
  query = NULL,
  chunk_size = 1e+05,
  overwrite = FALSE,
  verbose = TRUE,
  con = NULL
)
```

## Arguments

- src_name:

  Source name (e.g. "mimic_iv_demo"). Ignored when `query` is supplied
  directly.

- table:

  Table name to export. Ignored when `query` is supplied.

- output_dir:

  Directory to write `.parquet` files into. Created if it does not
  exist.

- schema:

  "raw", "concepts", "derived", "harmonized", or a literal schema
  string. Legacy aliases "ts", "flat", and "dur" resolve to
  "harmonized". Ignored when `query` is supplied.

- query:

  Optional: raw SQL string. Overrides src_name/table/schema.

- chunk_size:

  Rows per chunk. Default 100 000. Tune to your RAM.

- overwrite:

  If TRUE, delete output_dir contents first. Default FALSE.

- verbose:

  Print chunk progress. Default TRUE.

- con:

  Optional explicit DBI connection.

## Value

Invisibly returns the total number of rows written.

## Details

When `output_dir` ends in `.parquet` (single-file mode), the chunk files
are merged into that one file with DuckDB
(`COPY ... FROM read_parquet(...)`), not by collecting them into an R
data frame - peak memory stays bounded by `chunk_size` throughout,
including this final combine step, regardless of table size.

## Examples

``` r
if (FALSE) { # \dontrun{
ehr_connect("rendehr", creds)

# Export a full table
export_table_parquet("mimic_iv_demo", "chartevents",
                     output_dir = "exports/chartevents_parquet")

# Export a custom query
export_table_parquet(
  query      = "SELECT * FROM mimic_iv_demo_coh.chartevents WHERE itemid = 220045",
  output_dir = "exports/hr_parquet"
)

# Read back without loading into RAM
arrow::open_dataset("exports/chartevents_parquet") |>
  dplyr::filter(value > 100) |>
  dplyr::collect()
} # }
```

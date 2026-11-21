# Tables needed to resolve ICD cohort IDs for a source

These are the minimal raw tables an ICD cohort reads before cohort IDs
can be resolved;
[`cache_raw_parquet_duckdb()`](https://ate-driven-lab.github.io/rendehR/reference/cache_raw_parquet_duckdb.md)
caches them for named ICD cohorts.

## Usage

``` r
cohort_seed_tables(src_name)
```

## Arguments

- src_name:

  Source name.

## Value

Character vector of table names.

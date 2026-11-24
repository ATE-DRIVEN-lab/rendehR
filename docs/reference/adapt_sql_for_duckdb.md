# Adapt conservative PostgreSQL SQL syntax to DuckDB

This is intentionally small and rule-based. It covers syntax differences
we use in bundled schema/raw-mod/cohort SQL, while leaving query
structure and backend-specific semantics visible to reviewers.

## Usage

``` r
adapt_sql_for_duckdb(sql)
```

## Arguments

- sql:

  SQL text or character vector.

## Value

A single DuckDB-compatible SQL string.

## Examples

``` r
adapt_sql_for_duckdb("SELECT subject_id, valuenum::numeric FROM labevents")
#> [1] "SELECT subject_id, valuenum::numeric FROM labevents"
```

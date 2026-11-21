# Search for concept names matching a pattern

Grep-style search against the list of available concept tables.

## Usage

``` r
search_concepts(pattern, src_name = NULL, ignore_case = TRUE, con = NULL)
```

## Arguments

- pattern:

  Regex pattern (passed to `grepl`).

- src_name:

  Source name to restrict the search to; `NULL` searches every source
  the connection knows.

- ignore_case:

  Case-insensitive match. Default TRUE.

- con:

  Optional explicit DBI connection.

## Value

A data frame with one row per matching concept table: `source` plus the
columns
[`list_concepts()`](https://ate-driven-lab.github.io/rendehR/reference/list_concepts.md)
returns. When nothing matches, a zero-row data frame with `source`,
`concept_name` and `schema` (a message is emitted in that case).

## Examples

``` r
if (FALSE) { # \dontrun{
search_concepts("pressure", "mimic_iv_demo")   # sbp, dbp, map...
search_concepts("^spo", "mimic_iv_demo")        # spo2 variants
search_concepts("kb")                            # across every connected source
} # }
```

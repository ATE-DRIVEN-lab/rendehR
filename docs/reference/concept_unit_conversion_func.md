# Build SQL for a concept's unit conversion

Returns a SQL expression (string) that casts the source value to NUMERIC
and optionally applies a unit conversion when the source unit does not
match the target unit.

## Usage

``` r
concept_unit_conversion_func(
  conversion,
  val_var,
  unit_var,
  target_unit,
  concept_name = NA_character_
)
```

## Arguments

- conversion:

  Conversion type string from the concept config. Supported formats:

  - `"none"` - no conversion, cast to NUMERIC as-is.

  - `"*N"` or `"/N"` - multiply or divide every value by N. Only for
    items without a unit column; with a unit column this is an error,
    because unrecognised units would be converted too. Use `"map"`
    instead.

  - `"f_to_c"` - Fahrenheit to Celsius conversion. With a unit column,
    only recognised Fahrenheit labels (`F`, `Deg. F`, ...) are
    converted; values already in `target_unit` or with no unit are kept,
    and any other unit becomes `NULL`.

  - `"cond_f_to_c"` - for items without a unit: values above 70 are
    taken as Fahrenheit and converted.

  - `"fio2_fraction"` - for FiO2 items that mix fractions and percents:
    values in (0, 1\] are multiplied by 100, values in (1, 20) become
    `NULL`, and all others are kept as percents.

  - `"ca_ionized_by_value"` - for ionized calcium reported as mmol/L or
    mg/dL under one unit label (eICU): values up to 2 are taken as
    mmol/L, values of 3.5 or more as mg/dL and multiplied by 0.2495, and
    values in between become `NULL`. The unit column is ignored.

  - `"dipstick_grade"` - for qualitative urine dipstick results: `NEG`
    is 0, `SM`/`1+` is 1, `MOD`/`2+` is 2 and `LG`/`3+` is 3 (full words
    are accepted too). The unit column is ignored and any other text
    becomes `NULL`.

  - `"map"` - multi-unit lookup. Factors are read from
    `rendehr_concepts_unit_conversion_map.csv`, keyed by `concept_name`
    and `source_unit`. The target unit is always taken from the `unit`
    column of the concept config (not the map file). Any source unit not
    listed in the map that also does not match the target unit is set to
    `NULL`, and so is a row without a unit. A map row with an empty
    `factor` records a unit that is excluded on purpose (for example an
    absolute count reported under a percentage concept); its values are
    `NULL` too. Use this when a source column contains more than one
    unit across rows (e.g. CRP in mimic_iv stored as both mg/dL and
    ug/mL).

- val_var:

  Source column name holding the raw value.

- unit_var:

  Source column name holding the unit label (may be NA).

- target_unit:

  Target unit string (from the `unit` column in the concept config);
  used as the reference for conditional conversion.

- concept_name:

  Concept name. Required when `conversion = "map"` so the unit
  conversion map can be filtered to the correct concept.

## Value

SQL expression string.

## Details

The value is parsed as a number when it is plain or comma-grouped digits
with an optional sign, decimal point, exponent (`1e3`) or trailing `%`.
Anything else, including censored values such as `<0.5`, becomes `NULL`;
the original text stays in `value_raw`.

## Examples

``` r
cat(concept_unit_conversion_func("f_to_c", "valuenum", "valueuom", "C", "temperature"))
#> CASE
#>          WHEN src.valueuom IS NULL THEN CASE
#>      WHEN TRIM(src.valuenum::TEXT) ~ '^[+-]?((([0-9]+)|([0-9]{1,3}(,[0-9]{3})+))(\.[0-9]*)?|\.[0-9]+)([eE][+-]?[0-9]+)?%?$'
#>      THEN CASE
#>        WHEN ABS(CAST(REPLACE(REPLACE(TRIM(src.valuenum::TEXT), '%', ''), ',', '') AS DOUBLE PRECISION)) < 1e28
#>        THEN CAST(REPLACE(REPLACE(TRIM(src.valuenum::TEXT), '%', ''), ',', '') AS NUMERIC(38,9))
#>        ELSE NULL
#>      END
#>      ELSE NULL
#>    END
#>          WHEN LOWER(TRIM(CAST(src.valueuom AS TEXT))) = LOWER(TRIM('C')) THEN CASE
#>      WHEN TRIM(src.valuenum::TEXT) ~ '^[+-]?((([0-9]+)|([0-9]{1,3}(,[0-9]{3})+))(\.[0-9]*)?|\.[0-9]+)([eE][+-]?[0-9]+)?%?$'
#>      THEN CASE
#>        WHEN ABS(CAST(REPLACE(REPLACE(TRIM(src.valuenum::TEXT), '%', ''), ',', '') AS DOUBLE PRECISION)) < 1e28
#>        THEN CAST(REPLACE(REPLACE(TRIM(src.valuenum::TEXT), '%', ''), ',', '') AS NUMERIC(38,9))
#>        ELSE NULL
#>      END
#>      ELSE NULL
#>    END
#>          WHEN LOWER(TRIM(CAST(src.valueuom AS TEXT))) IN ('f', '°f', '?f', 'deg. f', 'deg.f', 'deg f', 'degf', 'degrees f', 'fahrenheit') THEN (CASE
#>      WHEN TRIM(src.valuenum::TEXT) ~ '^[+-]?((([0-9]+)|([0-9]{1,3}(,[0-9]{3})+))(\.[0-9]*)?|\.[0-9]+)([eE][+-]?[0-9]+)?%?$'
#>      THEN CASE
#>        WHEN ABS(CAST(REPLACE(REPLACE(TRIM(src.valuenum::TEXT), '%', ''), ',', '') AS DOUBLE PRECISION)) < 1e28
#>        THEN CAST(REPLACE(REPLACE(TRIM(src.valuenum::TEXT), '%', ''), ',', '') AS NUMERIC(38,9))
#>        ELSE NULL
#>      END
#>      ELSE NULL
#>    END - 32) * 5.0/9.0
#>          ELSE NULL
#>        END
```

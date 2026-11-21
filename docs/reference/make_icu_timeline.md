# Build an ICU-assigned timeline from a native-grain harmonized table

Projects a native-grain time-series table (usually `ts_tab`) onto ICU
stays without modifying the canonical store. Source rows keep their
native identifiers, including the source ICU stay ID when present. The
derived table adds an assigned ICU stay ID, event-origin labels,
ICU-time-relation labels, and a recomputed `time_since_icu_min` relative
to the assigned ICU stay.

## Usage

``` r
make_icu_timeline(
  src_name,
  db_name = NULL,
  base_dir = NULL,
  source_table = "ts_tab",
  out_table = "ts_icu",
  icustay_id = NULL,
  target_id = NULL,
  window_before_hours = 0,
  window_after_hours = 0,
  include_other_icu_events = FALSE,
  include_other_icu_stays = NULL,
  overlap = NULL,
  include_date_only_events = FALSE,
  date_time_since_icu = c("null", "zero", "event_anchor"),
  materialize = TRUE
)
```

## Arguments

- src_name:

  Source name (e.g. `"mimic_iv_demo"`).

- db_name:

  Optional Postgres database name; resolved from config if NULL.

- base_dir:

  Project root directory.

- source_table:

  Native-grain source table in the harmonized schema (default
  `"ts_tab"`).

- out_table:

  Output table/view name in the harmonized schema (default `"ts_icu"`).

- icustay_id:

  ICU stay identifier column. Defaults to the source ICU ID column from
  the dataset config (e.g. `"stay_id"` for MIMIC-IV).

- target_id:

  Deprecated alias for `icustay_id`.

- window_before_hours:

  Hours before ICU admission to include in each ICU analysis window.

- window_after_hours:

  Hours after ICU discharge to include in each ICU analysis window.

- include_other_icu_events:

  If `TRUE`, ICU-native events from other ICU stays in the same
  admission may be assigned to an ICU when they fall inside its analysis
  window. If `FALSE`, ICU-native events are only assigned to their own
  source ICU stay.

- include_other_icu_stays:

  Deprecated alias for `include_other_icu_events`.

- overlap:

  How to resolve a datetime event that lands in multiple ICU analysis
  windows: `"first"`, `"nearest"`, or `"duplicate"`. If omitted,
  defaults to `"duplicate"` when `include_other_icu_events = TRUE`,
  otherwise `"first"`. For `"first"` and `"nearest"`, an eligible source
  ICU is preferred before temporal ordering. `"first"` then orders by
  ICU-window start, while `"nearest"` orders by distance to the ICU
  interval. ICU stay ID is the final deterministic tie-break.

- include_date_only_events:

  If `TRUE`, date-only events are assigned to every ICU stay overlapping
  the same 24-hour admission-relative date.

- date_time_since_icu:

  How to populate `time_since_icu_min` for assigned date-only events:
  `"null"` (default), `"zero"`, or `"event_anchor"`.

- materialize:

  `TRUE` (default) builds a TABLE; `FALSE` builds a VIEW.

## Value

Invisibly, the output object name (`{schema}.{out_table}`).

## Details

This is intended for ICU-stay analysis and machine-learning datasets.
For example, a hospital-level lab with no source ICU stay ID can be
assigned to an ICU stay whose analysis window contains the lab time. If
`include_other_icu_events = TRUE`, ICU-native events from neighboring
ICU stays in the same admission can also be assigned when they fall
inside the analysis window; the original source ICU stay ID is retained.

Date-only events are conservative by default. If
`include_date_only_events = TRUE`, a date-only row is assigned to every
ICU stay in the same admission whose ICU interval overlaps that 24-hour
admission-relative date. Date-only assignment intentionally uses the raw
ICU interval, not the padded `window_before_hours`/`window_after_hours`
interval, and date-only rows are not deduplicated by `overlap`.

`event_origin` is one of `"hospital"`, `"same_icu"`, `"other_icu"`, or
`"unassigned_icu"`. The last value means the source row had an ICU stay
ID, but it was not assigned to an ICU timeline by the selected rules.

eICU is supported with the same semantics: its ICU windows are numeric
minute offsets on the hospital-admission clock (`admittime = 0`) and its
`ts_tab` carries a hospital-admission-relative `time_since_hadm_min`, so
the same windowing and cross-stay assignment run, grouped on
`patienthealthsystemstayid` instead of `hadm_id`.

## Examples

``` r
if (FALSE) { # \dontrun{
make_icu_timeline("mimic_iv_demo", db_name = "rendehr", window_before_hours = 6)
} # }
```

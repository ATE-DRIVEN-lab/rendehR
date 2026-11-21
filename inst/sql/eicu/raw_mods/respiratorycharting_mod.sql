-- respiratorycharting_mod: clean respchartvalue to a numeric percentage
-- Handles three raw formats found in eicu respiratorycharting:
--   "40"   -> already a percent, use as-is
--   "40%"  -> strip the % suffix, use as-is
--   "0.4"  -> fraction (<=1), multiply by 100
--   "room air" etc -> non-numeric, set to NULL
-- search_path must be set to the raw schema before executing.

DROP TABLE IF EXISTS respiratorycharting_mod;

CREATE TABLE respiratorycharting_mod AS
WITH stripped AS (
    SELECT
        r.patientunitstayid,
        r.respchartoffset,
        r.respchartentryoffset,
        r.respchartvaluelabel,
        r.respchartvalue AS respchartvalue_raw,
        TRIM(REPLACE(r.respchartvalue, '%', '')) AS respchartvalue_stripped
    FROM respiratorycharting r
),

numeric_cast AS (
    SELECT
        *,
        CASE
            WHEN respchartvalue_stripped ~ '^-?[0-9]+(\.[0-9]+)?$'
            THEN CAST(respchartvalue_stripped AS NUMERIC)
            ELSE NULL
        END AS respchartvalue_num_raw
    FROM stripped
)

SELECT
    patientunitstayid,
    respchartoffset,
    respchartentryoffset,
    respchartvaluelabel,
    respchartvalue_raw,

    CASE
        WHEN respchartvalue_num_raw IS NULL  THEN NULL           -- non-numeric
        WHEN respchartvalue_num_raw <= 1     THEN respchartvalue_num_raw * 100  -- fraction -> %
        WHEN respchartvalue_num_raw <= 100   THEN respchartvalue_num_raw        -- already %
        ELSE NULL                                                -- out of range
    END AS respchartvalue

FROM numeric_cast;

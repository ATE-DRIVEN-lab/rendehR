-- unitdischargeoffset < 0 is a data-quality artifact (2 rows, down to -7,687 minutes) --
-- physically impossible (discharge before admission), so both raw and harmonized columns
-- are NULL in that case rather than passing a negative LOS through. unitdischargeoffset = 0
-- is a legitimate boundary case (immediate discharge/death/transfer, 1,211 rows) and is
-- intentionally left as icu_los = 0, not filtered.

DROP TABLE IF EXISTS {concepts_schema}.icu_los;

CREATE TABLE {concepts_schema}.icu_los AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    -- raw LOS in minutes (native eICU unit)
    CASE WHEN pt.unitdischargeoffset >= 0 THEN pt.unitdischargeoffset::integer END AS icu_los_raw,

    -- harmonized LOS in days, rounded to 2 decimal places
    CASE WHEN pt.unitdischargeoffset >= 0 THEN ROUND(pt.unitdischargeoffset::numeric / 1440, 2) END AS icu_los,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;
DROP TABLE IF EXISTS {concepts_schema}.icu_los;

CREATE TABLE {concepts_schema}.icu_los AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    -- raw LOS in days as stored in icustays (fractional, may have float artifacts)
    icu.los AS icu_los_raw,

    -- harmonized LOS in days, rounded to 2 decimal places
    ROUND(EXTRACT(EPOCH FROM (icu.outtime - icu.intime))::numeric / 86400, 2) AS icu_los,

    'icustays' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.icustays icu
    ON vw.stay_id = icu.stay_id;

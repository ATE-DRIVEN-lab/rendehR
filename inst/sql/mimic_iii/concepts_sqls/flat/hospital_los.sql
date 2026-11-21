-- dischtime < admittime is a data-quality artifact (98 of 58,976 rows, min -0.945 days) --
-- physically impossible (discharge before admission), so hospital_los is NULL in that case
-- rather than passing a negative LOS through. Matches ricu's declared min:0 bound for the
-- equivalent los_hosp concept.

DROP TABLE IF EXISTS {concepts_schema}.hospital_los;

CREATE TABLE {concepts_schema}.hospital_los AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    -- no single raw column; LOS derived from timestamps
    NULL::integer AS hospital_los_raw,

    -- harmonized LOS in days, rounded to 2 decimal places
    CASE WHEN adm.dischtime >= adm.admittime
        THEN ROUND(EXTRACT(EPOCH FROM (adm.dischtime - adm.admittime))::numeric / 86400, 2)
    END AS hospital_los,

    'admissions' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

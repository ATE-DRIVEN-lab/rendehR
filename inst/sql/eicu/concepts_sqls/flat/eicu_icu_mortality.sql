-- ICU mortality from patient.unitdischargestatus.
-- 1 = died during ICU stay, 0 = survived to ICU discharge, NULL = unknown.

DROP TABLE IF EXISTS {concepts_schema}.icu_mortality;

CREATE TABLE {concepts_schema}.icu_mortality AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.unitdischargestatus AS icu_mortality_raw,

    CASE
        WHEN pt.unitdischargestatus = 'Expired' THEN 1
        WHEN pt.unitdischargestatus = 'Alive'   THEN 0
        ELSE NULL
    END AS icu_mortality,

    'patient'      AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

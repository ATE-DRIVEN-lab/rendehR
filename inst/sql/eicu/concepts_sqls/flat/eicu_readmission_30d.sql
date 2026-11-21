-- Hospital readmission within 30 days cannot be determined in eICU.
-- eICU uses relative time offsets per hospital stay — no absolute timestamps
-- to reliably compare across separate hospital admissions.
-- All values are NULL.

DROP TABLE IF EXISTS {concepts_schema}.readmission_30d;

CREATE TABLE {concepts_schema}.readmission_30d AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    NULL::integer AS readmission_30d_raw,
    NULL::integer AS readmission_30d,

    'patient'      AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw;

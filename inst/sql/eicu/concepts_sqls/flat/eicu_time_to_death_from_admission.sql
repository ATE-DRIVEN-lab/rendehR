-- eICU has no absolute death timestamps and no post-discharge death tracking.
-- time_to_death_from_admission is not computable for eICU.

DROP TABLE IF EXISTS {concepts_schema}.time_to_death_from_admission;

CREATE TABLE {concepts_schema}.time_to_death_from_admission AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    NULL::numeric AS time_to_death_from_admission_raw,
    NULL::numeric AS time_to_death_from_admission,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw;

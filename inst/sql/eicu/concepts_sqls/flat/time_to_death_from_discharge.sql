-- eICU has no absolute death timestamps and no post-discharge death tracking.
-- time_to_death_from_discharge is not computable for eICU.

DROP TABLE IF EXISTS {concepts_schema}.time_to_death_from_discharge;

CREATE TABLE {concepts_schema}.time_to_death_from_discharge AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    NULL::numeric AS time_to_death_from_discharge_raw,
    NULL::numeric AS time_to_death_from_discharge,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw;

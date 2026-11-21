DROP TABLE IF EXISTS {concepts_schema}.sex;

CREATE TABLE {concepts_schema}.sex AS

SELECT

    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,


    -- raw value
    pt.gender AS sex_raw,

    -- cleaned value
    CASE
        WHEN pt.gender = 'Male' THEN 'male'
        WHEN pt.gender = 'Female' THEN 'female'
        ELSE NULL
    END AS sex,


'patient' AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
  ON vw.patientunitstayid = pt.patientunitstayid;

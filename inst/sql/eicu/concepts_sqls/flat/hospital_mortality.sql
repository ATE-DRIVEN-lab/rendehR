DROP TABLE IF EXISTS {concepts_schema}.hospital_mortality;

CREATE TABLE {concepts_schema}.hospital_mortality AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.hospitaldischargestatus AS hospital_mortality_raw,

    CASE
        WHEN pt.hospitaldischargestatus = 'Expired' THEN 1
        WHEN pt.hospitaldischargestatus IS NULL
          OR pt.hospitaldischargestatus = ''  THEN NULL
        ELSE 0
    END AS hospital_mortality,

'patient' AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

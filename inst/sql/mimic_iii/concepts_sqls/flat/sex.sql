
DROP TABLE IF EXISTS {concepts_schema}.sex;

CREATE TABLE {concepts_schema}.sex AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    pat.gender AS sex_raw,
    -- real MIMIC-III patients.gender values are 'M'/'F', never 'Male'/'Female' --
    -- matches MIMIC-IV's convention (miiv_sex.sql), ricu, MIT, and BlendedICU
    CASE
        WHEN pat.gender = 'M' THEN 'male'
        WHEN pat.gender = 'F' THEN 'female'
        ELSE NULL
    END AS sex,

  'patients' AS source_table,
  '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patients pat
  ON vw.subject_id = pat.subject_id;

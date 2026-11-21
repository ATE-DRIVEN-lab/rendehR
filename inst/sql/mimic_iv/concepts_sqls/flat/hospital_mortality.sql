DROP TABLE IF EXISTS {concepts_schema}.hospital_mortality;

CREATE TABLE {concepts_schema}.hospital_mortality AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    adm.hospital_expire_flag AS hospital_mortality_raw,

    CASE
        WHEN adm.hospital_expire_flag = 1 THEN 1
        WHEN adm.hospital_expire_flag = 0 THEN 0
    END AS hospital_mortality,

    'admissions' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;
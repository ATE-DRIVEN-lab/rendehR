-- Age at hospital admission for MIMIC-III.
-- Adapted from MIMIC-Code: concepts/demographics/icustay_detail.sql
-- Formula: EXTRACT(EPOCH FROM (admittime - dob)) / 31556908.8 (seconds per year)
-- MIMIC-III privacy rule: patients >= 89 have DOB shifted ~300 years forward,
-- causing computed age to be ~300. Capped at 90 with LEAST().
-- No native age column in MIMIC-III — age_raw is NULL.

DROP TABLE IF EXISTS {concepts_schema}.age;

CREATE TABLE {concepts_schema}.age AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    NULL::numeric AS age_raw,

    CASE
        WHEN adm.admittime IS NULL OR pat.dob IS NULL THEN NULL
        ELSE LEAST(
            ROUND(
                EXTRACT(EPOCH FROM (adm.admittime - pat.dob))::numeric / 31556908.8,
                1
            ),
            90
        )
    END AS age,

    'admissions+patients' AS source_table,
    '{raw_schema}'        AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm ON vw.hadm_id    = adm.hadm_id
LEFT JOIN {raw_schema}.patients   pat ON vw.subject_id = pat.subject_id;

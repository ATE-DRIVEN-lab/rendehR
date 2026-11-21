-- Age at hospital admission for MIMIC-IV.
-- Adapted from MIMIC-Code: concepts/demographics/age.sql
-- Formula: anchor_age + EXTRACT(EPOCH FROM admittime - MAKE_TIMESTAMP(anchor_year,1,1,0,0,0)) / 31556908.8
-- anchor_age = patient age in the anchor_year (shifted year for privacy).
-- No DOB available in MIMIC-IV — age is reconstructed from the anchor system.
-- Capped at 90 for consistency with MIMIC-III and eICU.
-- anchor_age stored as age_raw (closest native value).

DROP TABLE IF EXISTS {concepts_schema}.age;

CREATE TABLE {concepts_schema}.age AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    pat.anchor_age AS age_raw,

    CASE
        WHEN adm.admittime IS NULL THEN NULL
        ELSE LEAST(
            ROUND(
                (pat.anchor_age
                 + EXTRACT(EPOCH FROM adm.admittime
                           - MAKE_TIMESTAMP(pat.anchor_year::integer, 1, 1, 0, 0, 0))
                   / 31556908.8
                )::numeric,
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

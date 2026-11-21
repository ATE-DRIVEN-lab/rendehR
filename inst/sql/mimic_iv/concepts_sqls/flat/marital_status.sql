-- rendehR's own marital status categories.
-- 3-way scheme that distinguishes never-married from previously-married, rather than a
-- 2-way "married-partner" vs "without spouse or partner" grouping -- often
-- clinically/socially meaningful since support needs differ (e.g. widowed vs never-married).
-- Labels: married / never_married / previously_married.
DROP TABLE IF EXISTS {concepts_schema}.marital_status;

CREATE TABLE  {concepts_schema}.marital_status AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    adm.marital_status AS marital_status_raw,

     CASE
        WHEN adm.marital_status IS NULL THEN 'unknown'

        -- married
        WHEN adm.marital_status IN (
            'MARRIED', 'LIFE PARTNER'
        ) THEN 'married'

        -- never_married
        WHEN adm.marital_status IN (
            'SINGLE'
        ) THEN 'never_married'

        -- previously_married
        WHEN adm.marital_status IN (
            'SEPARATED', 'WIDOWED', 'DIVORCED'
        ) THEN 'previously_married'

       -- unknown
        WHEN adm.marital_status IN (
            'UNKNOWN (DEFAULT)'
        ) THEN 'unknown'

        -- other
        WHEN adm.marital_status IN (
            'Other'
        ) THEN 'other'


        ELSE 'other'
    END AS marital_status,

'admissions' AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
  ON vw.hadm_id = adm.hadm_id;

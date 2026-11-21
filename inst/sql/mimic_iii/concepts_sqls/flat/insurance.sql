-- rendehR's own insurance categories.
-- Medicare and Medicaid are both government-run programs, so they are merged into
-- 'government' alongside the raw 'Government' label rather than kept as three separate
-- categories.

DROP TABLE IF EXISTS {concepts_schema}.insurance;

CREATE TABLE  {concepts_schema}.insurance AS


SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    adm.insurance AS insurance_raw,

     CASE
        WHEN adm.insurance IS NULL THEN 'unknown'

       -- self_pay
        WHEN adm.insurance IN (
            'Self Pay'
        ) THEN 'self_pay'

       -- government (Medicare + Medicaid + Government all merged)
        WHEN adm.insurance IN (
            'Medicare', 'Medicaid', 'Government'
        ) THEN 'government'

        -- private
        WHEN adm.insurance IN (
            'Private Health Insurance', 'Private'
        ) THEN 'private'

        -- other
        WHEN adm.insurance IN (
            'Other'
        ) THEN 'other'


        ELSE 'other'
    END AS insurance,

'admissions' AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
  ON vw.hadm_id = adm.hadm_id;

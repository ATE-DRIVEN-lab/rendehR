-- rendehR's own language categories.
-- 3-category scheme (english / other / unknown), the same as for MIMIC-III
-- (mimic_iii/concepts_sqls/flat/language.sql), whose language values are hospital
-- interpreter-service codes that can't be reliably decoded beyond 'ENGL'.
-- MIMIC-IV's values are full names ('English', 'Spanish', 'Russian', 'Chinese', ...) and
-- could support more buckets; the shared scheme keeps the two MIMIC versions consistent.
-- Lowercase output, matching the convention used for race/sex.

DROP TABLE IF EXISTS {concepts_schema}.language;

CREATE TABLE  {concepts_schema}.language AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    adm.language AS language_raw,

      CASE
        WHEN adm.language IS NULL THEN 'unknown'
        WHEN adm.language = 'English' THEN 'english'
        ELSE 'other'
    END AS "language",

'admissions' AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
  ON vw.hadm_id = adm.hadm_id;

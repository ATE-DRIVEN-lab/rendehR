-- rendehR's own language categories.
-- MIMIC-III's admissions.language values are 4-5 letter hospital interpreter-service
-- codes (e.g. 'ENGL', 'SPAN', 'RUSS', 'PTUN', ~76 distinct codes total), not full
-- language names ('English', 'Spanish', ...). No authoritative decode table exists for
-- most of these codes (no official MIMIC documentation, and no ricu/MIT/BlendedICU
-- reference handles this field for MIMIC-III). Rather than risk mis-decoding ambiguous
-- codes (e.g. 'PTUN', 628 rows, meaning unclear), this uses a 3-category scheme limited
-- to what can be confidently verified: english / other / unknown. Lowercase output,
-- matching the convention used for race/sex.
-- 'ENGL' is the only code decoded with high confidence (29,086 of 58,976 rows,
-- the dominant value) -- everything else folds into 'other' rather than guessing.
-- MIMIC-IV uses the same scheme (see mimic_iv/concepts_sqls/flat/language.sql).

DROP TABLE IF EXISTS {concepts_schema}.language;

CREATE TABLE  {concepts_schema}.language AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    adm.language AS language_raw,

      CASE
        WHEN adm.language IS NULL THEN 'unknown'
        WHEN adm.language = 'ENGL' THEN 'english'
        ELSE 'other'
    END AS "language",

'admissions' AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
  ON vw.hadm_id = adm.hadm_id;

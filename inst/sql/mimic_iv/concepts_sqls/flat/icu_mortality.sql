-- ICU mortality inferred from deathtime falling within the ICU stay window.
-- 1 = died during ICU stay (deathtime BETWEEN intime AND outtime)
-- 0 = survived to ICU discharge (died after outtime or no death recorded)
-- NULL = death information unavailable.
-- Note: a patient can have hospital_mortality = 1 but icu_mortality = 0
-- if they survived the ICU and died on the floor.

DROP TABLE IF EXISTS {concepts_schema}.icu_mortality;

CREATE TABLE {concepts_schema}.icu_mortality AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    adm.hospital_expire_flag AS icu_mortality_raw,

    CASE
        WHEN icu.intime IS NULL OR icu.outtime IS NULL THEN NULL
        WHEN adm.hadm_id IS NULL THEN NULL
        WHEN adm.deathtime IS NULL THEN 0
        WHEN adm.deathtime BETWEEN icu.intime AND icu.outtime THEN 1
        ELSE 0
    END AS icu_mortality,

    'admissions+icustays' AS source_table,
    '{raw_schema}'        AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.icustays   icu ON vw.stay_id  = icu.stay_id
LEFT JOIN {raw_schema}.admissions adm ON vw.hadm_id  = adm.hadm_id;

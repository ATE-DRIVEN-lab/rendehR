-- time_to_death_from_discharge: days from hospital discharge to death
-- in-hospital deaths: 0 (patient died before leaving hospital)
-- post-discharge deaths: uses patients.dod (date only, day-level precision)
-- NULL = alive at last known follow-up
-- The post-discharge formula is timestamp-based, EXTRACT(DAY FROM (dod - dischtime)), the
-- same as MIMIC-III; truncating dischtime to a date first would diverge on 99.7% of rows
-- (avg +1 day systematic bias).
-- Negative gaps of up to 2 days clamp to 0 (benign midnight-truncation case); larger
-- negative gaps give NULL. Severe outliers exist (up to -3,833.7 days -- the same dod
-- privacy-jitter artifact handled in readmission_30d/time_to_death_from_admission), and
-- clamping a death years away to "died on day of discharge" would be confidently wrong, not
-- a safe default. The 2-day tolerance matches readmission_30d.

DROP TABLE IF EXISTS {concepts_schema}.time_to_death_from_discharge;

CREATE TABLE {concepts_schema}.time_to_death_from_discharge AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    -- raw: unrounded (0 for in-hospital, integer days for post-discharge)
    CASE
        WHEN adm.deathtime IS NOT NULL
            THEN 0::numeric
        WHEN p.dod IS NOT NULL AND CAST(p.dod AS TIMESTAMP) >= adm.dischtime - INTERVAL '2 days'
            THEN GREATEST(0, EXTRACT(DAY FROM (CAST(p.dod AS TIMESTAMP) - adm.dischtime)))::numeric
        ELSE NULL
    END AS time_to_death_from_discharge_raw,

    -- harmonized: same (already integer days or 0)
    CASE
        WHEN adm.deathtime IS NOT NULL
            THEN 0::numeric
        WHEN p.dod IS NOT NULL AND CAST(p.dod AS TIMESTAMP) >= adm.dischtime - INTERVAL '2 days'
            THEN GREATEST(0, EXTRACT(DAY FROM (CAST(p.dod AS TIMESTAMP) - adm.dischtime)))::numeric
        ELSE NULL
    END AS time_to_death_from_discharge,

    'admissions-patients' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id
LEFT JOIN {raw_schema}.patients p
    ON vw.subject_id = p.subject_id;

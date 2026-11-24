-- time_to_death_from_admission: days from hospital admission to death
-- in-hospital deaths: uses admissions.deathtime (exact datetime)
-- post-discharge deaths: uses patients.dod (date only, day-level precision)
-- NULL = alive at last known follow-up
-- The post-discharge branch uses the timestamp-based formula EXTRACT(DAY FROM
-- (dod - admittime)), the same as MIMIC-III. Truncating admittime to a date first
-- ((dod::date - admittime::date)) would diverge on 98.4% of rows (avg +1 day systematic
-- bias, since only 3.3% of admittime values fall exactly at midnight).
-- Guards for deathtime < admittime (26 rows -- a death timestamp before the start of the
-- very admission it's attached to is a pure data error) and dod < admittime on the
-- post-discharge branch (35 rows, gap up to -3,826 days -- the same dod privacy-jitter
-- artifact handled in readmission_30d) give NULL. No tolerance needed for either guard: the
-- dod branch only fires when deathtime IS NULL (post-discharge deaths), where dod should
-- always be well after admittime -- no legitimate midnight-truncation case applies here.

DROP TABLE IF EXISTS {concepts_schema}.time_to_death_from_admission;

CREATE TABLE {concepts_schema}.time_to_death_from_admission AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    -- raw: unrounded days (float for in-hospital, integer for post-discharge)
    CASE
        WHEN adm.deathtime IS NOT NULL AND adm.deathtime >= adm.admittime
            THEN EXTRACT(EPOCH FROM (adm.deathtime - adm.admittime))::numeric / 86400
        WHEN adm.deathtime IS NULL AND p.dod IS NOT NULL AND CAST(p.dod AS TIMESTAMP) >= adm.admittime
            THEN EXTRACT(DAY FROM (CAST(p.dod AS TIMESTAMP) - adm.admittime))
        ELSE NULL
    END AS time_to_death_from_admission_raw,

    -- harmonized: rounded to 2 decimal places
     CASE
        WHEN adm.deathtime IS NOT NULL AND adm.deathtime >= adm.admittime
            THEN ROUND(EXTRACT(EPOCH FROM (adm.deathtime - adm.admittime))::numeric / 86400, 2)
        WHEN adm.deathtime IS NULL AND p.dod IS NOT NULL AND CAST(p.dod AS TIMESTAMP) >= adm.admittime
            THEN ROUND(EXTRACT(DAY FROM (CAST(p.dod AS TIMESTAMP) - adm.admittime))::numeric, 2)
        ELSE NULL
    END AS time_to_death_from_admission,

    'admissions-patients' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id
LEFT JOIN {raw_schema}.patients p
    ON vw.subject_id = p.subject_id;

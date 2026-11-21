-- time_to_death_from_admission: days from hospital admission to death
-- in-hospital deaths: uses admissions.deathtime (exact datetime)
-- post-discharge deaths: uses patients.dod (date only, day-level precision)
-- NULL = alive at last known follow-up
-- Guards: deathtime < admittime (88 rows -- a death timestamp before the start of the very
-- admission it's attached to is a pure data error, not a benign artifact) and dod < admittime
-- on the post-discharge branch (10 rows) both give NULL.
-- No tolerance needed for either guard (unlike readmission_30d's dod-vs-dischtime check):
-- this dod branch only fires when deathtime IS NULL, i.e. specifically for post-discharge
-- deaths, where dod should always be well after admittime -- there's no legitimate
-- midnight-truncation case here.

DROP TABLE IF EXISTS {concepts_schema}.time_to_death_from_admission;

CREATE TABLE {concepts_schema}.time_to_death_from_admission AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    -- raw: unrounded days (float for in-hospital, integer for post-discharge)
    CASE
        WHEN adm.deathtime IS NOT NULL AND adm.deathtime >= adm.admittime
            THEN EXTRACT(EPOCH FROM (adm.deathtime - adm.admittime))::numeric / 86400
        WHEN adm.deathtime IS NULL AND p.dod IS NOT NULL AND p.dod >= adm.admittime
            THEN EXTRACT(DAY FROM (p.dod - adm.admittime))
        ELSE NULL
    END AS time_to_death_from_admission_raw,

    -- harmonized: rounded to 2 decimal places
    CASE
        WHEN adm.deathtime IS NOT NULL AND adm.deathtime >= adm.admittime
            THEN ROUND(EXTRACT(EPOCH FROM (adm.deathtime - adm.admittime))::numeric / 86400, 2)
        WHEN adm.deathtime IS NULL AND p.dod IS NOT NULL AND p.dod >= adm.admittime
            THEN ROUND(EXTRACT(DAY FROM (p.dod - adm.admittime))::numeric, 2)
        ELSE NULL
    END AS time_to_death_from_admission,

    'admissions-patients' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id
LEFT JOIN {raw_schema}.patients p
    ON vw.subject_id = p.subject_id;

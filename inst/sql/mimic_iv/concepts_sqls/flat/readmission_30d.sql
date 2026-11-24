-- 30-day hospital readmission after THIS discharge (forward-looking).
-- 1   = readmitted within 30 days of discharge.
-- 0   = survived 30 days without readmission.
-- NULL = died within 30 days of discharge without readmission (competing event).
--        In-hospital deaths (dischtime = deathtime) are also NULL — patient
--        never had a chance to be readmitted.
-- Raw column: next_admittime (timestamp of next admission, NULL if none).
-- The next admission is looked up in visit_windows, which holds every admission
-- of the cohort's patients, not only the imported ones; both times come from
-- there so their difference does not depend on the session time zone.
-- The dod-vs-dischtime check has a 2-day tolerance on its lower bound. dod can appear to
-- precede dischtime by up to 3,833 days in MIMIC-IV (a date-shift/privacy-jitter artifact on
-- dod, not a genuine "died before this discharge" case); without the tolerance, 17 MIMIC-IV
-- rows with a gap >30 days would count as a competing-death event for an unrelated discharge.
-- A hard `dod >= dischtime` cutoff is not used: dod is truncated to midnight, so same-day
-- deaths (the common case, ~5,800/~11,400 rows in MIMIC-III/IV) would flip from the correct
-- NULL to an incorrect 0. The 2-day tolerance keeps those while excluding the outliers.

DROP TABLE IF EXISTS {concepts_schema}.readmission_30d;

CREATE TABLE {concepts_schema}.readmission_30d AS

WITH adm_ranked AS (
    SELECT
        hadm_id,
        subject_id,
        dischtime,
        LEAD(admittime) OVER (
            PARTITION BY subject_id
            ORDER BY admittime
        ) AS next_admittime
    FROM (
        SELECT DISTINCT subject_id, hadm_id, admittime, dischtime
        FROM {raw_schema}.visit_windows
    ) adm_all
)

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    r.next_admittime AS readmission_30d_raw,

     CASE
        -- readmitted within 30 days of discharge
        WHEN r.next_admittime IS NOT NULL
             AND r.next_admittime > r.dischtime
             AND r.next_admittime - r.dischtime <= INTERVAL '30 days'
            THEN 1
        -- admission outside the imported scope: no discharge record
        WHEN adm.hadm_id IS NULL THEN NULL
        -- died within 30 days of discharge (competing event)
        WHEN pat.dod IS NOT NULL
             AND pat.dod >= adm.dischtime - INTERVAL '2 days'
             AND EXTRACT(DAY FROM (pat.dod - adm.dischtime)) <= 30
            THEN NULL
        ELSE 0
    END AS readmission_30d,

    'admissions+patients' AS source_table,
    '{raw_schema}'        AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN adm_ranked              r   ON vw.hadm_id    = r.hadm_id
LEFT JOIN {raw_schema}.admissions adm ON vw.hadm_id    = adm.hadm_id
LEFT JOIN {raw_schema}.patients   pat ON vw.subject_id = pat.subject_id;

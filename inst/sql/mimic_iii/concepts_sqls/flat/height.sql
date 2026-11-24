-- Adapted from MIMIC-Code (https://github.com/MIT-LCP/mimic-code)
-- Original: concepts/demographics/heightweight.sql + concepts/firstday/height_first_day.sql
-- Rather than averaging height readings across a time window, this pools every height
-- source (chartevents itemids + echo_data), converts all to cm, and takes the
-- chronologically FIRST valid (120-230cm) reading -- this represents "admission height"
-- directly rather than a stay-wide average.
-- "First" is scoped to the HOSPITAL ADMISSION (hadm_id), not the ICU stay (icustay_id) --
-- height is a hospital-visit-level fact, not an ICU-stay-level one. A patient with multiple
-- ICU stays within one hospitalization (3,260 of 57,786 MIMIC-III hadm_ids have >1
-- icustay_id, up to 7) shares the same height value across all of that hospitalization's
-- ICU stays, so an earlier reading tagged to a different ICU stay in the same
-- hospitalization is not missed.
-- Ties at the identical timestamp (duplicate pairs, e.g. 226707/226730, 4187/4188,
-- 3486/3485 -- all >0.999 correlation, same charttime) prefer the native-cm reading over
-- the inches-converted one.
-- Inch items: 920, 1394, 4187, 3486, 226707 (× 2.54 → cm).
-- CM items: 3485, 4188, 226730. They duplicate inch items but need no exclusion: the
-- first-valid selection above resolves duplicate pairs, and no AVG double-counts them.
-- Additional source: {concepts_schema}.echo_data (heights in echo notes, inches → cm),
-- merged into the same time-sorted candidate pool. Requires echo_data to be run first
-- (ehr/echo_data.sql).
-- Neonate detection: charttime <= dob + 1 year → excluded from the candidate pool.
-- Adult plausible range: 120–230 cm.

DROP TABLE IF EXISTS {concepts_schema}.height;

CREATE TABLE {concepts_schema}.height AS

WITH ht_candidates AS (
    SELECT
        c.hadm_id,
        c.charttime,
        CASE
            WHEN c.itemid IN (920, 1394, 4187, 3486, 226707)
                THEN c.valuenum * 2.54
            ELSE c.valuenum
        END AS height_cm,
        CASE WHEN c.itemid IN (226730, 4188, 3485) THEN 0 ELSE 1 END AS is_converted
    FROM {raw_schema}.chartevents c
    INNER JOIN {raw_schema}.patients pt ON c.subject_id = pt.subject_id
    WHERE c.itemid IN (920, 1394, 4187, 3486, 226707, 3485, 4188, 226730)
      AND c.valuenum IS NOT NULL
      AND c.valuenum > 0
      AND COALESCE(c.error, 0) = 0
      AND c.charttime > (pt.dob + INTERVAL '1 year')
      AND c.hadm_id IS NOT NULL

    UNION ALL

    SELECT
        adm.hadm_id,
        ec.charttime,
        ec.height_inches * 2.54 AS height_cm,
        0 AS is_converted
    FROM {raw_schema}.admissions adm
    INNER JOIN {concepts_schema}.echo_data ec ON adm.subject_id = ec.subject_id
    WHERE ec.height_inches IS NOT NULL
      AND ec.charttime IS NOT NULL
),
ht_valid AS (
    SELECT *
    FROM ht_candidates
    WHERE height_cm BETWEEN 120 AND 230
),
ht_ranked AS (
    SELECT
        hadm_id,
        charttime,
        height_cm,
        ROW_NUMBER() OVER (
            PARTITION BY hadm_id
            ORDER BY charttime ASC, is_converted ASC
        ) AS rn
    FROM ht_valid
)

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    hr.height_cm AS height_raw,
    ROUND(hr.height_cm::numeric, 2) AS height,

    'chartevents+echo_data' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN ht_ranked hr
    ON vw.hadm_id = hr.hadm_id
    AND hr.rn = 1;

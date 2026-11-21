-- Adapted from MIMIC-Code (https://github.com/MIT-LCP/mimic-code)
-- Original: concepts/measurement/height.sql + concepts/firstday/first_day_height.sql
-- Rather than averaging height readings across a first-ICU-day window, this pools both
-- height sources, converts to cm, and takes the chronologically FIRST valid (120-230cm)
-- reading -- represents "admission height" directly rather than a stay-wide average.
-- "First" is scoped to the HOSPITAL ADMISSION (hadm_id), not the ICU stay (stay_id) --
-- height is a hospital-visit-level fact, not an ICU-stay-level one. A patient with multiple
-- ICU stays within one hospitalization shares the same height value across all of that
-- hospitalization's ICU stays.
-- itemid 226707 = height in inches (× 2.54); itemid 226730 = height in cm.
-- 226707/226730 are a same-timestamp duplicate pair (43,575 paired rows, 99.8% close match
-- within 1cm, correlation 0.9998). Ties at the identical timestamp prefer the native-cm
-- reading, which resolves the duplicate without double-counting.
-- Neonate detection: approximate age at charttime = anchor_age + (year(charttime) - anchor_year).
--   If < 1 year → excluded from the candidate pool. Year-level precision only
--   (MIMIC-IV does not expose exact DOB).
-- Adult plausible range: 120–230 cm.

DROP TABLE IF EXISTS {concepts_schema}.height;

CREATE TABLE {concepts_schema}.height AS

WITH ht_candidates AS (
    SELECT
        c.hadm_id,
        c.charttime,
        CASE
            WHEN c.itemid = 226707 THEN c.valuenum * 2.54
            ELSE c.valuenum
        END AS height_cm,
        CASE WHEN c.itemid = 226730 THEN 0 ELSE 1 END AS is_converted
    FROM {raw_schema}.chartevents c
    INNER JOIN {raw_schema}.patients pt
        ON c.subject_id = pt.subject_id
    WHERE c.itemid IN (226707, 226730)
      AND c.valuenum IS NOT NULL
      AND c.valuenum > 0
      AND (pt.anchor_age + (EXTRACT(YEAR FROM c.charttime)::int - pt.anchor_year)) >= 1
      AND c.hadm_id IS NOT NULL
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
    vw.stay_id,

    hr.height_cm AS height_raw,
    ROUND(hr.height_cm::numeric, 2) AS height,

    'chartevents' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN ht_ranked hr
    ON vw.hadm_id = hr.hadm_id
    AND hr.rn = 1;

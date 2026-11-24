-- Adapted from MIMIC-Code (https://github.com/MIT-LCP/mimic-code)
-- Original: concepts/firstday/first_day_weight.sql
-- Rather than averaging weight readings across a time window, this pools every weight
-- source, filters to a plausible range, and takes the chronologically FIRST valid reading
-- -- represents "admission weight" directly rather than a blended average. See
-- mimic_iii/concepts_sqls/flat/weight_admit.sql for the data rationale (Admit Wt vs Daily
-- Weight are not unit-duplicates -- a non-trivial divergence, not a negligible duplicate
-- like height's inch/cm pairs).
-- "First" is scoped to the HOSPITAL ADMISSION (hadm_id), not the ICU stay (stay_id) --
-- weight_admit is a hospital-visit-level fact, not an ICU-stay-level one. A patient with
-- multiple ICU stays within one hospitalization shares the same weight_admit value across
-- all of that hospitalization's ICU stays.
-- Sources (both in kg):
--   itemid 226512 = Admit Wt
--   itemid 224639 = Daily Weight
-- Ties at the identical timestamp prefer the Admit-Wt-tagged reading.
-- Neonate detection: approximate age = anchor_age + (year(charttime) - anchor_year) < 1 →
--   excluded from the candidate pool.
-- Plausible range: 1-500 kg (matching ricu's own declared bound for this concept).

DROP TABLE IF EXISTS {concepts_schema}.weight_admit;

CREATE TABLE {concepts_schema}.weight_admit AS

WITH wt_candidates AS (
    SELECT
        c.hadm_id,
        c.charttime,
        c.valuenum AS weight_kg,
        CASE WHEN c.itemid = 226512 THEN 0 ELSE 1 END AS is_daily
    FROM {raw_schema}.chartevents c
    INNER JOIN {raw_schema}.patients pt ON c.subject_id = pt.subject_id
    WHERE c.itemid IN (
        226512,  -- Admit Wt (kg)
        224639   -- Daily Weight (kg)
    )
      AND c.valuenum IS NOT NULL
      AND c.valuenum > 0
      AND (pt.anchor_age + (EXTRACT(YEAR FROM c.charttime)::int - pt.anchor_year)) >= 1
      AND c.hadm_id IS NOT NULL
),
wt_valid AS (
    SELECT *
    FROM wt_candidates
    WHERE weight_kg BETWEEN 1 AND 500
),
wt_ranked AS (
    SELECT
        hadm_id,
        charttime,
        weight_kg,
        ROW_NUMBER() OVER (
            PARTITION BY hadm_id
            ORDER BY charttime ASC, is_daily ASC
        ) AS rn
    FROM wt_valid
)

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    wr.weight_kg AS weight_admit_raw,
    ROUND(wr.weight_kg::numeric, 2) AS weight_admit,

    'chartevents' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN wt_ranked wr
    ON vw.hadm_id = wr.hadm_id
    AND wr.rn = 1;

-- Adapted from MIMIC-Code (https://github.com/MIT-LCP/mimic-code)
-- Original: concepts/firstday/weight_first_day.sql
-- Rather than averaging weight readings across a time window, this pools every weight
-- source, filters to a plausible range, and takes the chronologically FIRST valid reading
-- -- this represents "admission weight" directly rather than a blended average (same
-- approach as height).
-- "First" is scoped to the HOSPITAL ADMISSION (hadm_id), not the ICU stay (icustay_id) --
-- weight_admit is a hospital-visit-level fact, not an ICU-stay-level one. A patient with
-- multiple ICU stays within one hospitalization (3,260 of 57,786 MIMIC-III hadm_ids have >1
-- icustay_id) shares the same weight_admit value across all of that hospitalization's ICU
-- stays.
-- Admit Wt and Daily Weight are NOT unit-duplicates of each other (unlike height's inch/cm
-- pairs) -- of 19,131 stays with both readings in the first ICU day, only 63.6% agree
-- within 2kg (avg diff 3.78kg), so blending them distorts "admission weight". As for
-- height: pool everything, no source-type priority, first valid by time wins. Ties at the
-- identical timestamp prefer the Admit-Wt-tagged reading.
-- Sources (all in kg, in one time-sorted pool):
--   1. chartevents Admit Wt     (itemids 762, 226512)
--   2. chartevents Daily Weight  (itemids 763, 224639)
--   3. echo_data in-hospital     (weight_lbs × 0.453592) — requires echo_data (ehr/echo_data.sql)
--      Only in-hospital echo used (hadm_id match). Pre-hospital outpatient echo excluded —
--      a weight up to 30 days before ICU admission is clinically unreliable.
-- Neonate detection: charttime <= dob + 1 year → excluded from the candidate pool.
-- Plausible range: 1-500 kg (matching ricu's own declared bound for this concept).

DROP TABLE IF EXISTS {concepts_schema}.weight_admit;

CREATE TABLE {concepts_schema}.weight_admit AS

WITH wt_candidates AS (

    -- Source 1 & 2: chartevents admit + daily weight
    SELECT
        c.hadm_id,
        c.charttime,
        c.valuenum AS weight_kg,
        CASE WHEN c.itemid IN (762, 226512) THEN 0 ELSE 1 END AS is_daily
    FROM {raw_schema}.chartevents c
    INNER JOIN {raw_schema}.patients pt ON c.subject_id = pt.subject_id
    WHERE c.itemid IN (
        762, 226512,  -- Admit Wt (kg)
        763, 224639   -- Daily Weight (kg)
    )
      AND c.valuenum IS NOT NULL
      AND c.valuenum > 0
      AND COALESCE(c.error, 0) = 0
      AND c.charttime > (pt.dob + INTERVAL '1 year')
      AND c.hadm_id IS NOT NULL

    UNION ALL

    -- Source 3: in-hospital echo weight (lbs → kg)
    SELECT
        ec.hadm_id,
        ec.charttime,
        ec.weight_lbs * 0.453592 AS weight_kg,
        0 AS is_daily
    FROM {concepts_schema}.echo_data ec
    WHERE ec.weight_lbs IS NOT NULL
      AND ec.weight_lbs > 0
      AND ec.hadm_id IS NOT NULL
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
    vw.icustay_id,

    wr.weight_kg AS weight_admit_raw,
    ROUND(wr.weight_kg::numeric, 2) AS weight_admit,

    'chartevents+echo_data' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN wt_ranked wr
    ON vw.hadm_id = wr.hadm_id
    AND wr.rn = 1;

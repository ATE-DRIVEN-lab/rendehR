-- SOFA renal input: urine output from MIMIC-III outputevents.
-- itemid list is mimic-code's urine-output set (CareVue + MetaVision);
-- 227488 (GU irrigant) is subtracted.
CREATE OR REPLACE TABLE sep_uo AS
SELECT
  ie.icustay_id,
  CAST(FLOOR(epoch(oe.charttime - ie.intime) / 3600.0) AS BIGINT) AS hr,
  SUM(CASE WHEN oe.itemid = 227488 THEN -1 * oe.value ELSE oe.value END) AS uo_hr
FROM outputevents oe
JOIN icustays ie ON oe.icustay_id = ie.icustay_id
WHERE oe.itemid IN (
    40055, 43175, 40069, 40094, 40715, 40473, 40085, 40057, 40056, 40405,
    40428, 40086, 40096, 40651,
    226559, 226560, 226561, 226584, 226563, 226564, 226565, 226567,
    226557, 226558, 227488, 227489
  )
  AND oe.value IS NOT NULL
GROUP BY ie.icustay_id, hr;

-- SOFA respiration inputs (PaO2, FiO2) from MIMIC-III labevents (blood gas).
-- itemids: 50821 PaO2, 50816 FiO2 (recorded as a percentage, 20-100).
CREATE OR REPLACE TABLE sep_bg AS
SELECT
  ie.icustay_id,
  CAST(FLOOR(epoch(le.charttime - ie.intime) / 3600.0) AS BIGINT) AS hr,
  MIN(CASE WHEN le.itemid = 50821 AND le.valuenum <= 800 THEN le.valuenum END) AS pao2_min,
  MAX(CASE WHEN le.itemid = 50816 AND le.valuenum >= 20 AND le.valuenum <= 100
           THEN le.valuenum END) AS fio2_max
FROM labevents le
JOIN icustays ie ON le.hadm_id = ie.hadm_id
WHERE le.itemid IN (50821, 50816) AND le.valuenum IS NOT NULL
GROUP BY ie.icustay_id, hr;

-- SOFA lab components (coagulation, liver, renal) from MIMIC-III labevents.
-- labevents is hospitalization-level (no icustay_id), so it is mapped to ICU
-- stays via hadm_id; hr = whole hours since that stay's ICU intime.
-- itemids: 50885 bilirubin, 50912 creatinine, 51265 platelet (mimic-code).
CREATE OR REPLACE TABLE sep_lab AS
SELECT
  ie.icustay_id,
  CAST(FLOOR(epoch(le.charttime - ie.intime) / 3600.0) AS BIGINT) AS hr,
  MAX(CASE WHEN le.itemid = 50885 AND le.valuenum <= 150   THEN le.valuenum END) AS bilirubin_max,
  MAX(CASE WHEN le.itemid = 50912 AND le.valuenum <= 150   THEN le.valuenum END) AS creatinine_max,
  MIN(CASE WHEN le.itemid = 51265 AND le.valuenum <= 10000 THEN le.valuenum END) AS platelet_min
FROM labevents le
JOIN icustays ie ON le.hadm_id = ie.hadm_id
WHERE le.itemid IN (50885, 50912, 51265)
  AND le.valuenum IS NOT NULL AND le.valuenum > 0
GROUP BY ie.icustay_id, hr;

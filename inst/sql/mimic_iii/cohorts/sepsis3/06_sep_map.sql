-- SOFA cardiovascular input: mean arterial pressure from MIMIC-III chartevents.
-- itemids cover CareVue + MetaVision MAP (mimic-code):
--   456 NBP Mean, 52 Arterial BP Mean, 6702 Arterial BP Mean #2,
--   443 Manual BP Mean, 220052/220181/225312 (MetaVision).
CREATE OR REPLACE TABLE sep_map AS
SELECT
  ie.icustay_id,
  CAST(FLOOR(epoch(ce.charttime - ie.intime) / 3600.0) AS BIGINT) AS hr,
  MIN(ce.valuenum) AS meanbp_min
FROM chartevents ce
JOIN icustays ie ON ce.icustay_id = ie.icustay_id
WHERE ce.itemid IN (456, 52, 6702, 443, 220052, 220181, 225312)
  AND ce.valuenum > 0 AND ce.valuenum < 300
GROUP BY ie.icustay_id, hr;

-- SOFA CNS input: Glasgow Coma Score from MIMIC-III chartevents.
-- MIMIC-III has no single GCS-total itemid, so it is summed from the three
-- components, merging CareVue and MetaVision itemids (mimic-code):
--   motor 454/223901, verbal 723/223900, eyes 184/220739.
-- A total needs all three components at the same charttime; intubated verbal
-- ('ET/Trach', valuenum NULL) rows are dropped (a known simplification).
CREATE OR REPLACE TABLE sep_gcs AS
WITH comp AS (
  SELECT
    ie.icustay_id,
    ce.charttime,
    ie.intime,
    MAX(CASE WHEN ce.itemid IN (454, 223901) THEN ce.valuenum END) AS motor,
    MAX(CASE WHEN ce.itemid IN (723, 223900) THEN ce.valuenum END) AS verbal,
    MAX(CASE WHEN ce.itemid IN (184, 220739) THEN ce.valuenum END) AS eyes
  FROM chartevents ce
  JOIN icustays ie ON ce.icustay_id = ie.icustay_id
  WHERE ce.itemid IN (454, 223901, 723, 223900, 184, 220739)
    AND ce.valuenum IS NOT NULL
  GROUP BY ie.icustay_id, ce.charttime, ie.intime
)
SELECT
  icustay_id,
  CAST(FLOOR(epoch(charttime - intime) / 3600.0) AS BIGINT) AS hr,
  MIN(motor + verbal + eyes) AS gcs_min
FROM comp
WHERE motor IS NOT NULL AND verbal IS NOT NULL AND eyes IS NOT NULL
GROUP BY icustay_id, hr;

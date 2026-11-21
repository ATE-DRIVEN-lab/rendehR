-- SOFA cardiovascular input: vasopressor administration (presence per hour).
-- MIMIC-III has two input systems; itemids from mimic-code SOFA:
--   CareVue inputevents_cv (rate): norepi 30047/30120, epi 30044/30119/30309,
--                                  dopamine 30043/30307, dobutamine 30042/30306
--   MetaVision inputevents_mv (rate): norepi 221906, epi 221289,
--                                     dopamine 221662, dobutamine 221653
-- Doses/units differ across systems, so this is a presence flag (any pressor
-- with rate > 0), matching the eICU chain's cardiovascular simplification.
CREATE OR REPLACE TABLE sep_vaso AS
SELECT
  ie.icustay_id,
  CAST(FLOOR(epoch(cv.charttime - ie.intime) / 3600.0) AS BIGINT) AS hr
FROM inputevents_cv cv
JOIN icustays ie ON cv.icustay_id = ie.icustay_id
WHERE cv.itemid IN (30047, 30120, 30044, 30119, 30309, 30043, 30307, 30042, 30306)
  AND TRY_CAST(cv.rate AS DOUBLE) > 0   -- inputevents_cv.rate is read as VARCHAR
GROUP BY ie.icustay_id, hr

UNION

SELECT
  ie.icustay_id,
  CAST(FLOOR(epoch(mv.starttime - ie.intime) / 3600.0) AS BIGINT) AS hr
FROM inputevents_mv mv
JOIN icustays ie ON mv.icustay_id = ie.icustay_id
WHERE mv.itemid IN (221906, 221289, 221662, 221653)
  AND mv.rate > 0
GROUP BY ie.icustay_id, hr;

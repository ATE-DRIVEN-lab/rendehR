-- Hourly SOFA score for MIMIC-III, adapted from the mimic-code SOFA query.
-- One row per ICU hour; each component is the worst value in a trailing 24h
-- window, imputing 0 when missing. sofa_time (= intime + hr+1 hours) is the
-- latest timestamp that hour's score is valid (used by the sepsis3 SI join).
--
-- Simplifications vs. mimic-code: respiration = PaO2/FiO2 only (no ventilation
-- flag); cardiovascular = any vasopressor -> 2, MAP<70 -> 1 (doses vary across
-- CareVue/MetaVision); renal urine = 24h rolling sum of charted urine.
CREATE OR REPLACE TABLE sofa AS
WITH co AS (
  SELECT
    ie.icustay_id,
    ie.subject_id,
    ie.hadm_id,
    g.hr,
    ie.intime + to_hours(g.hr + 1) AS sofa_time
  FROM icustays ie,
  LATERAL generate_series(
    0, GREATEST(CAST(CEIL(epoch(ie.outtime - ie.intime) / 3600.0) AS BIGINT), 0)
  ) AS g(hr)
  WHERE ie.outtime IS NOT NULL
),
pf AS (
  SELECT icustay_id, hr, pao2_min * 100.0 / NULLIF(fio2_max, 0) AS pao2fio2_min
  FROM sep_bg WHERE pao2_min IS NOT NULL AND fio2_max IS NOT NULL AND fio2_max > 0
),
scorecomp AS (
  SELECT
    co.icustay_id, co.subject_id, co.hadm_id, co.hr, co.sofa_time,
    pf.pao2fio2_min,
    lb.platelet_min,
    lb.bilirubin_max,
    lb.creatinine_max,
    mp.meanbp_min,
    gc.gcs_min,
    uo.uo_hr,
    vs.vaso_flag
  FROM co
  LEFT JOIN pf         ON co.icustay_id = pf.icustay_id  AND co.hr = pf.hr
  LEFT JOIN sep_lab lb ON co.icustay_id = lb.icustay_id  AND co.hr = lb.hr
  LEFT JOIN sep_map mp ON co.icustay_id = mp.icustay_id  AND co.hr = mp.hr
  LEFT JOIN sep_gcs gc ON co.icustay_id = gc.icustay_id  AND co.hr = gc.hr
  LEFT JOIN sep_uo  uo ON co.icustay_id = uo.icustay_id  AND co.hr = uo.hr
  LEFT JOIN (SELECT DISTINCT icustay_id, hr, 1 AS vaso_flag FROM sep_vaso) vs
         ON co.icustay_id = vs.icustay_id AND co.hr = vs.hr
),
scorecomp2 AS (
  SELECT
    scorecomp.*,
    SUM(uo_hr) OVER (
      PARTITION BY icustay_id ORDER BY hr ROWS BETWEEN 23 PRECEDING AND 0 FOLLOWING
    ) AS uo_24hr
  FROM scorecomp
),
scorecalc AS (
  SELECT
    scorecomp2.*,
    CASE
      WHEN pao2fio2_min < 100 THEN 4
      WHEN pao2fio2_min < 200 THEN 3
      WHEN pao2fio2_min < 300 THEN 2
      WHEN pao2fio2_min < 400 THEN 1
      WHEN pao2fio2_min IS NULL THEN NULL ELSE 0
    END AS respiration,
    CASE
      WHEN platelet_min < 20  THEN 4
      WHEN platelet_min < 50  THEN 3
      WHEN platelet_min < 100 THEN 2
      WHEN platelet_min < 150 THEN 1
      WHEN platelet_min IS NULL THEN NULL ELSE 0
    END AS coagulation,
    CASE
      WHEN bilirubin_max >= 12.0 THEN 4
      WHEN bilirubin_max >= 6.0  THEN 3
      WHEN bilirubin_max >= 2.0  THEN 2
      WHEN bilirubin_max >= 1.2  THEN 1
      WHEN bilirubin_max IS NULL THEN NULL ELSE 0
    END AS liver,
    CASE
      WHEN vaso_flag = 1 THEN 2
      WHEN meanbp_min < 70 THEN 1
      WHEN COALESCE(vaso_flag, meanbp_min) IS NULL THEN NULL ELSE 0
    END AS cardiovascular,
    CASE
      WHEN gcs_min >= 13 AND gcs_min <= 14 THEN 1
      WHEN gcs_min >= 10 AND gcs_min <= 12 THEN 2
      WHEN gcs_min >= 6  AND gcs_min <= 9  THEN 3
      WHEN gcs_min < 6 THEN 4
      WHEN gcs_min IS NULL THEN NULL ELSE 0
    END AS cns,
    CASE
      WHEN creatinine_max >= 5.0 THEN 4
      WHEN uo_24hr < 200 THEN 4
      WHEN creatinine_max >= 3.5 AND creatinine_max < 5.0 THEN 3
      WHEN uo_24hr < 500 THEN 3
      WHEN creatinine_max >= 2.0 AND creatinine_max < 3.5 THEN 2
      WHEN creatinine_max >= 1.2 AND creatinine_max < 2.0 THEN 1
      WHEN COALESCE(uo_24hr, creatinine_max) IS NULL THEN NULL ELSE 0
    END AS renal
  FROM scorecomp2
)
SELECT
  icustay_id, subject_id, hadm_id, hr, sofa_time,
  COALESCE(MAX(respiration)    OVER w, 0) AS respiration_24hours,
  COALESCE(MAX(coagulation)    OVER w, 0) AS coagulation_24hours,
  COALESCE(MAX(liver)          OVER w, 0) AS liver_24hours,
  COALESCE(MAX(cardiovascular) OVER w, 0) AS cardiovascular_24hours,
  COALESCE(MAX(cns)            OVER w, 0) AS cns_24hours,
  COALESCE(MAX(renal)          OVER w, 0) AS renal_24hours,
  COALESCE(MAX(respiration)    OVER w, 0)
    + COALESCE(MAX(coagulation)    OVER w, 0)
    + COALESCE(MAX(liver)          OVER w, 0)
    + COALESCE(MAX(cardiovascular) OVER w, 0)
    + COALESCE(MAX(cns)            OVER w, 0)
    + COALESCE(MAX(renal)          OVER w, 0) AS sofa_24hours
FROM scorecalc
WINDOW w AS (
  PARTITION BY icustay_id ORDER BY hr ROWS BETWEEN 23 PRECEDING AND 0 FOLLOWING
);

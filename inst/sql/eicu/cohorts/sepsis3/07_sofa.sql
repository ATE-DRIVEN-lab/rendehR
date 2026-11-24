-- Hourly SOFA score for eICU, adapted from the mimic-code SOFA query.
-- One row per ICU hour (minutes-from-unit-admission axis); each component is
-- the worst value in a trailing 24h window, imputing 0 when missing.
-- endtime_min = (hr+1)*60 is the latest time that hour's score is valid.
--
-- eICU simplifications: respiration = PaO2/FiO2 only (no vent flag);
-- cardiovascular = any vasopressor -> 2, MAP<70 -> 1 (no reliable doses);
-- renal urine uses a 24h rolling sum of charted urine volume.
CREATE OR REPLACE TABLE sofa AS
WITH co AS (
  SELECT
    p.patientunitstayid,
    g.hr,
    g.hr * 60       AS starttime_min,
    (g.hr + 1) * 60 AS endtime_min
  FROM patient p,
  LATERAL generate_series(
    0, GREATEST(CAST(CEIL(p.unitdischargeoffset / 60.0) AS BIGINT), 0)
  ) AS g(hr)
),
pf AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         MIN(pao2 / NULLIF(fio2, 0)) AS pao2fio2_min
  FROM sep_bg
  WHERE pao2 IS NOT NULL AND fio2 IS NOT NULL AND fio2 > 0
  GROUP BY 1, 2
),
plt AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         MIN(platelets) AS platelet_min
  FROM sep_lab WHERE platelets IS NOT NULL GROUP BY 1, 2
),
bili AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         MAX(bilirubin) AS bilirubin_max
  FROM sep_lab WHERE bilirubin IS NOT NULL GROUP BY 1, 2
),
cr AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         MAX(creatinine) AS creatinine_max
  FROM sep_lab WHERE creatinine IS NOT NULL GROUP BY 1, 2
),
mp AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         MIN(meanbp) AS meanbp_min
  FROM sep_map WHERE meanbp IS NOT NULL GROUP BY 1, 2
),
gc AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         MIN(gcs) AS gcs_min
  FROM sep_gcs WHERE gcs IS NOT NULL GROUP BY 1, 2
),
uo AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         SUM(urineoutput) AS uo_hr
  FROM sep_uo WHERE urineoutput IS NOT NULL GROUP BY 1, 2
),
vaso AS (
  SELECT patientunitstayid, CAST(FLOOR(chartoffset / 60.0) AS BIGINT) AS hr,
         1 AS vaso_flag
  FROM sep_vaso GROUP BY 1, 2
),
scorecomp AS (
  SELECT
    co.patientunitstayid, co.hr, co.starttime_min, co.endtime_min,
    pf.pao2fio2_min, plt.platelet_min, bili.bilirubin_max, cr.creatinine_max,
    mp.meanbp_min, gc.gcs_min, uo.uo_hr, vaso.vaso_flag
  FROM co
  LEFT JOIN pf   ON co.patientunitstayid = pf.patientunitstayid   AND co.hr = pf.hr
  LEFT JOIN plt  ON co.patientunitstayid = plt.patientunitstayid  AND co.hr = plt.hr
  LEFT JOIN bili ON co.patientunitstayid = bili.patientunitstayid AND co.hr = bili.hr
  LEFT JOIN cr   ON co.patientunitstayid = cr.patientunitstayid   AND co.hr = cr.hr
  LEFT JOIN mp   ON co.patientunitstayid = mp.patientunitstayid   AND co.hr = mp.hr
  LEFT JOIN gc   ON co.patientunitstayid = gc.patientunitstayid   AND co.hr = gc.hr
  LEFT JOIN uo   ON co.patientunitstayid = uo.patientunitstayid   AND co.hr = uo.hr
  LEFT JOIN vaso ON co.patientunitstayid = vaso.patientunitstayid AND co.hr = vaso.hr
),
scorecomp2 AS (
  SELECT
    scorecomp.*,
    SUM(uo_hr) OVER (
      PARTITION BY patientunitstayid ORDER BY hr
      ROWS BETWEEN 23 PRECEDING AND 0 FOLLOWING
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
      WHEN pao2fio2_min IS NULL THEN NULL
      ELSE 0
    END AS respiration,
    CASE
      WHEN platelet_min < 20  THEN 4
      WHEN platelet_min < 50  THEN 3
      WHEN platelet_min < 100 THEN 2
      WHEN platelet_min < 150 THEN 1
      WHEN platelet_min IS NULL THEN NULL
      ELSE 0
    END AS coagulation,
    CASE
      WHEN bilirubin_max >= 12.0 THEN 4
      WHEN bilirubin_max >= 6.0  THEN 3
      WHEN bilirubin_max >= 2.0  THEN 2
      WHEN bilirubin_max >= 1.2  THEN 1
      WHEN bilirubin_max IS NULL THEN NULL
      ELSE 0
    END AS liver,
    CASE
      WHEN vaso_flag = 1 THEN 2
      WHEN meanbp_min < 70 THEN 1
      WHEN COALESCE(vaso_flag, meanbp_min) IS NULL THEN NULL
      ELSE 0
    END AS cardiovascular,
    CASE
      WHEN gcs_min >= 13 AND gcs_min <= 14 THEN 1
      WHEN gcs_min >= 10 AND gcs_min <= 12 THEN 2
      WHEN gcs_min >= 6  AND gcs_min <= 9  THEN 3
      WHEN gcs_min < 6 THEN 4
      WHEN gcs_min IS NULL THEN NULL
      ELSE 0
    END AS cns,
    CASE
      WHEN creatinine_max >= 5.0 THEN 4
      WHEN uo_24hr < 200 THEN 4
      WHEN creatinine_max >= 3.5 AND creatinine_max < 5.0 THEN 3
      WHEN uo_24hr < 500 THEN 3
      WHEN creatinine_max >= 2.0 AND creatinine_max < 3.5 THEN 2
      WHEN creatinine_max >= 1.2 AND creatinine_max < 2.0 THEN 1
      WHEN COALESCE(uo_24hr, creatinine_max) IS NULL THEN NULL
      ELSE 0
    END AS renal
  FROM scorecomp2
)
SELECT
  patientunitstayid,
  hr,
  starttime_min,
  endtime_min,
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
  PARTITION BY patientunitstayid ORDER BY hr
  ROWS BETWEEN 23 PRECEDING AND 0 FOLLOWING
);

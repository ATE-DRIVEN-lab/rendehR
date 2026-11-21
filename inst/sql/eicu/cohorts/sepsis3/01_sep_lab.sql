-- SOFA lab components (coagulation, liver, renal) from the raw `lab` table.
-- One row per (stay, offset); keeps the latest-revised value per lab/offset.
-- Single statement (CREATE OR REPLACE) so it can run as one setup_sql step.
CREATE OR REPLACE TABLE sep_lab AS
WITH vw1 AS (
  SELECT
    patientunitstayid,
    labname,
    labresultoffset,
    labresult,
    ROW_NUMBER() OVER (
      PARTITION BY patientunitstayid, labname, labresultoffset
      ORDER BY labresultrevisedoffset DESC
    ) AS rn
  FROM lab
  WHERE (labname = 'total bilirubin'  AND labresult >= 0.2 AND labresult <= 70.175)
     OR (labname = 'creatinine'       AND labresult >= 0.1 AND labresult <= 28.28)
     OR (labname = 'platelets x 1000' AND labresult >  0   AND labresult <= 9999)
)
SELECT
  patientunitstayid,
  labresultoffset AS chartoffset,
  MAX(CASE WHEN labname = 'total bilirubin'  THEN labresult END) AS bilirubin,
  MAX(CASE WHEN labname = 'creatinine'       THEN labresult END) AS creatinine,
  MAX(CASE WHEN labname = 'platelets x 1000' THEN labresult END) AS platelets
FROM vw1
WHERE rn = 1
GROUP BY patientunitstayid, labresultoffset;

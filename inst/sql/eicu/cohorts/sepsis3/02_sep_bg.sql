-- SOFA respiration inputs (PaO2, FiO2) from the raw `lab` table.
-- FiO2 recorded as a percentage (>=20) is normalised to a fraction.
CREATE OR REPLACE TABLE sep_bg AS
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
  WHERE (labname = 'paO2' AND labresult >= 15  AND labresult <= 720)
     OR (labname = 'FiO2' AND labresult >= 0.2 AND labresult <= 1.0)
     OR (labname = 'FiO2' AND labresult >= 20  AND labresult <= 100)
)
SELECT
  patientunitstayid,
  labresultoffset AS chartoffset,
  MAX(CASE WHEN labname = 'paO2' THEN labresult END) AS pao2,
  MAX(CASE WHEN labname = 'FiO2' AND labresult >= 20 THEN labresult / 100.0
           WHEN labname = 'FiO2'                     THEN labresult END) AS fio2
FROM vw1
WHERE rn = 1
GROUP BY patientunitstayid, labresultoffset;

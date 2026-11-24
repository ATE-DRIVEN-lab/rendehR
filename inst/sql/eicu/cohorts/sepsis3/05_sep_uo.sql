-- SOFA renal input: urine output volume from `intakeoutput`.
-- Matches urine-bearing output cellpaths (urine / foley / catheter / void).
CREATE OR REPLACE TABLE sep_uo AS
SELECT
  patientunitstayid,
  intakeoutputoffset AS chartoffset,
  SUM(cellvaluenumeric) AS urineoutput
FROM intakeoutput
WHERE cellpath LIKE 'flowsheet|Flowsheet Cell Labels|I&O|Output (ml)|%'
  AND (
       lower(cellpath) LIKE '%urine%'
    OR lower(cellpath) LIKE '%foley%'
    OR lower(cellpath) LIKE '%cath%'
    OR lower(cellpath) LIKE '%void%'
  )
  AND cellvaluenumeric IS NOT NULL
GROUP BY patientunitstayid, intakeoutputoffset;

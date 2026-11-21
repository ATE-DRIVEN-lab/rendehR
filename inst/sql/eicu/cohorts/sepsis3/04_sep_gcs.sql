-- SOFA CNS input: Glasgow Coma Score from `nursecharting`.
CREATE OR REPLACE TABLE sep_gcs AS
WITH nc AS (
  SELECT
    patientunitstayid,
    nursingchartoffset AS chartoffset,
    MIN(
      CASE
        WHEN nursingchartcelltypevallabel = 'Glasgow coma score'
         AND nursingchartcelltypevalname  = 'GCS Total'
          THEN TRY_CAST(nursingchartvalue AS DOUBLE)
        WHEN nursingchartcelltypevallabel = 'Score (Glasgow Coma Scale)'
         AND nursingchartcelltypevalname  = 'Value'
          THEN TRY_CAST(nursingchartvalue AS DOUBLE)
      END
    ) AS gcs
  FROM nursecharting
  GROUP BY patientunitstayid, nursingchartoffset
)
SELECT patientunitstayid, chartoffset, gcs
FROM nc
WHERE gcs > 2 AND gcs < 16;

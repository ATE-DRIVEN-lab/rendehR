-- SOFA cardiovascular input: mean arterial pressure from `nursecharting`.
-- Invasive MAP is preferred over non-invasive when both are charted.
CREATE OR REPLACE TABLE sep_map AS
WITH nc AS (
  SELECT
    patientunitstayid,
    nursingchartoffset AS chartoffset,
    CASE
      WHEN nursingchartcelltypevallabel = 'Invasive BP'
       AND nursingchartcelltypevalname  = 'Invasive BP Mean'
        THEN TRY_CAST(nursingchartvalue AS DOUBLE)
      WHEN nursingchartcelltypevallabel = 'MAP (mmHg)'
       AND nursingchartcelltypevalname  = 'Value'
        THEN TRY_CAST(nursingchartvalue AS DOUBLE)
      WHEN nursingchartcelltypevallabel = 'Arterial Line MAP (mmHg)'
       AND nursingchartcelltypevalname  = 'Value'
        THEN TRY_CAST(nursingchartvalue AS DOUBLE)
    END AS ibp_mean,
    CASE
      WHEN nursingchartcelltypevallabel = 'Non-Invasive BP'
       AND nursingchartcelltypevalname  = 'Non-Invasive BP Mean'
        THEN TRY_CAST(nursingchartvalue AS DOUBLE)
    END AS nibp_mean
  FROM nursecharting
)
SELECT
  patientunitstayid,
  chartoffset,
  MIN(v) AS meanbp
FROM (
  SELECT patientunitstayid, chartoffset, COALESCE(ibp_mean, nibp_mean) AS v
  FROM nc
) t
WHERE v IS NOT NULL AND v >= 1 AND v <= 250
GROUP BY patientunitstayid, chartoffset;

-- Suspicion of infection (eICU).
--
-- eICU has essentially no body-fluid culture data (microlab covers <3% of
-- stays), so - as ricu does for eICU - suspicion is defined by ANTIBIOTIC
-- administration rather than an antibiotic+culture pairing. Each antibiotic
-- administration is an SI event and its offset is the SI time. Any culture
-- within [-72h, +24h] is attached only as an annotation (culture_offset,
-- positive_culture); it is NOT required.
--
-- To make suspicion stricter (e.g. require >=2 antibiotics within 24h, ricu's
-- abx_min_count), filter `antibiotic` upstream; the columns here are unchanged.
CREATE OR REPLACE TABLE suspicion_of_infection AS
WITH me AS (
  SELECT
    patientunitstayid,
    culturetakenoffset AS culture_offset,
    MAX(CASE
          WHEN organism IS NOT NULL
           AND lower(organism) NOT IN ('no growth', 'no growth to date', '')
          THEN 1 ELSE 0
        END) AS positiveculture
  FROM microlab
  GROUP BY patientunitstayid, culturetakenoffset
)
SELECT
  ab.patientunitstayid,
  -- patient- and hospital-stay-level IDs from the raw `patient` table (eICU's
  -- ID carrier, like MIMIC's icustays); note visit_windows does NOT exist yet
  -- at cohort-selection time.
  p.uniquepid,
  p.patienthealthsystemstayid,
  ab.ab_id,
  ab.antibiotic,
  ab.antibiotic_offset,
  1                    AS suspected_infection,
  ab.antibiotic_offset AS suspected_infection_offset,
  (SELECT me.culture_offset FROM me
    WHERE me.patientunitstayid = ab.patientunitstayid
      AND me.culture_offset >= ab.antibiotic_offset - 4320
      AND me.culture_offset <= ab.antibiotic_offset + 1440
    ORDER BY abs(me.culture_offset - ab.antibiotic_offset)
    LIMIT 1)          AS culture_offset,
  (SELECT MAX(me.positiveculture) FROM me
    WHERE me.patientunitstayid = ab.patientunitstayid
      AND me.culture_offset >= ab.antibiotic_offset - 4320
      AND me.culture_offset <= ab.antibiotic_offset + 1440) AS positive_culture
FROM antibiotic ab
JOIN patient p ON ab.patientunitstayid = p.patientunitstayid;

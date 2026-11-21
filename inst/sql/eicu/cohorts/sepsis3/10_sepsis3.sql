-- Sepsis-3: suspicion of infection + absolute SOFA >= 2 within the SI window
-- [SI_time - 48h, SI_time + 24h] (mimic-code rule, baseline SOFA 0).
-- One row per ICU stay at the earliest qualifying suspicion event.
-- 48h = 2880 min, 24h = 1440 min.
CREATE OR REPLACE TABLE sepsis3 AS
WITH sofa_hi AS (
  SELECT
    patientunitstayid,
    endtime_min AS sofa_offset,
    respiration_24hours    AS respiration,
    coagulation_24hours    AS coagulation,
    liver_24hours          AS liver,
    cardiovascular_24hours AS cardiovascular,
    cns_24hours            AS cns,
    renal_24hours          AS renal,
    sofa_24hours           AS sofa_score
  FROM sofa
  WHERE sofa_24hours >= 2
),
s1 AS (
  SELECT
    soi.patientunitstayid,
    soi.uniquepid,
    soi.patienthealthsystemstayid,
    soi.antibiotic_offset,
    soi.culture_offset,
    soi.suspected_infection_offset,
    sh.sofa_offset,
    sh.sofa_score,
    sh.respiration, sh.coagulation, sh.liver, sh.cardiovascular, sh.cns, sh.renal,
    (sh.sofa_score >= 2 AND soi.suspected_infection = 1) AS sepsis3,
    ROW_NUMBER() OVER (
      PARTITION BY soi.patientunitstayid
      ORDER BY soi.suspected_infection_offset, soi.antibiotic_offset, sh.sofa_offset
    ) AS rn_sus
  FROM suspicion_of_infection soi
  JOIN sofa_hi sh
    ON soi.patientunitstayid = sh.patientunitstayid
   AND sh.sofa_offset >= soi.suspected_infection_offset - 2880
   AND sh.sofa_offset <= soi.suspected_infection_offset + 1440
  WHERE soi.suspected_infection = 1
)
SELECT
  patientunitstayid,
  uniquepid,
  patienthealthsystemstayid,
  antibiotic_offset,
  culture_offset,
  suspected_infection_offset,
  sofa_offset,
  sofa_score,
  respiration, coagulation, liver, cardiovascular, cns, renal,
  sepsis3
FROM s1
WHERE rn_sus = 1;

-- Sepsis-3: suspicion of infection + absolute SOFA >= 2 within the SI window
-- [SI_time - 48h, SI_time + 24h] (mimic-code onset rule, baseline SOFA 0).
--
-- ALL suspicion events are considered (not just the earliest): a stay is
-- Sepsis-3 if ANY of its antibiotic+culture suspicion events (from
-- abx_micro_poe) has a SOFA hour >= 2 inside its window. The stay is then
-- emitted once, at its earliest qualifying (suspicion, SOFA) pair, so
-- `SELECT DISTINCT subject_id ... WHERE sepsis3` aggregates to the ID level:
-- an ID is selected if at least one of its stays is septic.
CREATE OR REPLACE TABLE sepsis3 AS
WITH sofa_hi AS (
  SELECT
    icustay_id, sofa_time,
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
-- every suspicion event for the stay (one row per antibiotic+culture pairing)
si_all AS (
  SELECT
    ie.subject_id, ie.hadm_id, am.icustay_id,
    am.suspected_infection_time,
    am.specimen, am.positiveculture
  FROM abx_micro_poe am
  JOIN icustays ie ON am.icustay_id = ie.icustay_id
  WHERE am.suspected_infection_time IS NOT NULL
),
-- each event matched to SOFA >= 2 hours in its [-48h, +24h] window
matched AS (
  SELECT
    si.subject_id, si.hadm_id, si.icustay_id,
    si.suspected_infection_time, si.specimen, si.positiveculture,
    sh.sofa_time, sh.sofa_score,
    sh.respiration, sh.coagulation, sh.liver, sh.cardiovascular, sh.cns, sh.renal,
    ROW_NUMBER() OVER (
      PARTITION BY si.icustay_id
      ORDER BY si.suspected_infection_time, sh.sofa_time
    ) AS rn
  FROM si_all si
  JOIN sofa_hi sh
    ON si.icustay_id = sh.icustay_id
   AND sh.sofa_time >= si.suspected_infection_time - INTERVAL 48 HOUR
   AND sh.sofa_time <= si.suspected_infection_time + INTERVAL 24 HOUR
)
SELECT
  subject_id,
  hadm_id,
  icustay_id,
  suspected_infection_time,
  specimen,
  positiveculture,
  sofa_time,
  sofa_score,
  respiration, coagulation, liver, cardiovascular, cns, renal,
  TRUE AS sepsis3
FROM matched
WHERE rn = 1;

-- Sepsis-3 (rendehR default): suspicion of infection on a sustained antibiotic
-- course, plus an acute SOFA rise of >= 2 points within the suspicion window
-- [SI_time - 48h, SI_time + 24h]; adults only. One row per ICU stay at the
-- earliest qualifying suspicion event. Offsets are minutes from unit admission
-- (48h = 2880, 24h = 1440, 72h = 4320).
--
-- - Suspected infection: an antibiotic administration (eICU has culture data for
--   <3% of stays, so a culture is not required - as ricu and YAIB do for eICU)
--   that belongs to an antibiotic course lasting >= 72 h, administrations or
--   orders less than 24 h apart joined into one course, or cut short by death
--   within 72 h of the course start (YAIB `abx_cont`).
-- - SOFA rise: the hourly 24-h SOFA minus the lowest 24-h SOFA of the stay so far
--   (YAIB/ricu `delta_cummin`), so organ dysfunction present from admission (an
--   injury, sedation, a vasopressor started on arrival) is the baseline, not
--   sepsis.
-- - Adults: age >= 18 ('> 89' counts as 90).
-- The previous rule (any antibiotic, absolute SOFA >= 2 from a baseline of 0) is
-- variants/sepsis3_absolute.sql, selected with definition = "absolute".
CREATE OR REPLACE TABLE sepsis3 AS
WITH ab AS (
  SELECT patientunitstayid, antibiotic_offset, antibiotic_stop_offset,
         MAX(antibiotic_stop_offset) OVER (
           PARTITION BY patientunitstayid ORDER BY antibiotic_offset, antibiotic_stop_offset
           ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_stop
  FROM antibiotic
),
ab_course AS (
  SELECT ab.*,
         SUM(CASE WHEN prev_stop IS NULL OR antibiotic_offset > prev_stop + 1440 THEN 1 ELSE 0 END)
           OVER (PARTITION BY patientunitstayid ORDER BY antibiotic_offset, antibiotic_stop_offset
                 ROWS UNBOUNDED PRECEDING) AS course_id
  FROM ab
),
course AS (
  SELECT c.patientunitstayid, c.course_id,
         MIN(c.antibiotic_offset) AS course_start, MAX(c.antibiotic_stop_offset) AS course_end
  FROM ab_course c
  GROUP BY c.patientunitstayid, c.course_id
),
death AS (
  SELECT patientunitstayid,
         CASE WHEN hospitaldischargestatus = 'Expired' THEN hospitaldischargeoffset END AS death_offset,
         CASE WHEN age = '> 89' THEN 90 ELSE TRY_CAST(age AS INTEGER) END AS age_years
  FROM patient
),
course_ok AS (
  SELECT c.*
  FROM course c
  JOIN death d ON d.patientunitstayid = c.patientunitstayid
  WHERE c.course_end - c.course_start >= 4320
     OR (d.death_offset IS NOT NULL AND d.death_offset <= c.course_start + 4320
         AND c.course_end >= d.death_offset - 1440)
),
soi_ok AS (
  SELECT soi.*
  FROM suspicion_of_infection soi
  JOIN death d ON d.patientunitstayid = soi.patientunitstayid
  WHERE soi.suspected_infection = 1 AND d.age_years >= 18
    AND EXISTS (SELECT 1 FROM course_ok co
                 WHERE co.patientunitstayid = soi.patientunitstayid
                   AND soi.antibiotic_offset BETWEEN co.course_start AND co.course_end)
),
sofa_rise AS (
  SELECT
    patientunitstayid,
    endtime_min AS sofa_offset,
    respiration_24hours    AS respiration,
    coagulation_24hours    AS coagulation,
    liver_24hours          AS liver,
    cardiovascular_24hours AS cardiovascular,
    cns_24hours            AS cns,
    renal_24hours          AS renal,
    sofa_24hours           AS sofa_score,
    sofa_24hours - MIN(sofa_24hours) OVER (
      PARTITION BY patientunitstayid ORDER BY hr ROWS UNBOUNDED PRECEDING) AS sofa_rise
  FROM sofa
),
s1 AS (
  SELECT
    soi.patientunitstayid,
    soi.uniquepid,
    soi.patienthealthsystemstayid,
    soi.antibiotic_offset,
    soi.culture_offset,
    soi.suspected_infection_offset,
    sr.sofa_offset,
    sr.sofa_score,
    sr.sofa_rise,
    sr.respiration, sr.coagulation, sr.liver, sr.cardiovascular, sr.cns, sr.renal,
    TRUE AS sepsis3,
    ROW_NUMBER() OVER (
      PARTITION BY soi.patientunitstayid
      ORDER BY soi.suspected_infection_offset, soi.antibiotic_offset, sr.sofa_offset
    ) AS rn_sus
  FROM soi_ok soi
  JOIN sofa_rise sr
    ON soi.patientunitstayid = sr.patientunitstayid
   AND sr.sofa_offset >= soi.suspected_infection_offset - 2880
   AND sr.sofa_offset <= soi.suspected_infection_offset + 1440
   AND sr.sofa_rise >= 2
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
  sofa_rise,
  respiration, coagulation, liver, cardiovascular, cns, renal,
  sepsis3
FROM s1
WHERE rn_sus = 1;

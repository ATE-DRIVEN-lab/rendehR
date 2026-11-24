-- Sepsis-3 (rendehR default): suspicion of infection on a sustained antibiotic
-- course, plus an acute SOFA rise of >= 2 points within the suspicion window
-- [SI_time - 48h, SI_time + 24h]; adults only. One row per ICU stay at the
-- earliest qualifying suspicion event.
--
-- - Suspected infection: the mimic-code antibiotic-culture pairing
--   (suspicion_of_infection), where the antibiotic belongs to a course lasting
--   >= 72 h - prescriptions of the admission less than 24 h apart are joined
--   into one course - or cut short by in-hospital death within 72 h of the
--   course start (YAIB `abx_cont`).
-- - SOFA rise: the hourly 24-h SOFA minus the lowest 24-h SOFA of the stay so far
--   (YAIB/ricu `delta_cummin`), so organ dysfunction present from admission (an
--   injury, sedation, a vasopressor started on arrival) is the baseline, not
--   sepsis.
-- - Adults: anchor_age >= 18.
-- The previous rule (absolute SOFA >= 2 from a baseline of 0, any antibiotic of
-- the pairing) is variants/sepsis3_absolute.sql, selected with
-- definition = "absolute".
CREATE OR REPLACE TABLE sepsis3 AS
WITH ab AS (
  SELECT hadm_id, starttime, GREATEST(starttime, COALESCE(stoptime, starttime)) AS stoptime,
         MAX(GREATEST(starttime, COALESCE(stoptime, starttime))) OVER (
           PARTITION BY hadm_id ORDER BY starttime, stoptime
           ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_stop
  FROM antibiotic
  WHERE hadm_id IS NOT NULL AND starttime IS NOT NULL
),
ab_course AS (
  SELECT ab.*,
         SUM(CASE WHEN prev_stop IS NULL OR starttime > prev_stop + INTERVAL '24' HOUR THEN 1 ELSE 0 END)
           OVER (PARTITION BY hadm_id ORDER BY starttime, stoptime ROWS UNBOUNDED PRECEDING) AS course_id
  FROM ab
),
course AS (
  SELECT hadm_id, course_id, MIN(starttime) AS course_start, MAX(stoptime) AS course_end
  FROM ab_course
  GROUP BY hadm_id, course_id
),
course_ok AS (
  SELECT c.*
  FROM course c
  JOIN admissions adm ON adm.hadm_id = c.hadm_id
  WHERE c.course_end - c.course_start >= INTERVAL '72' HOUR
     OR (adm.deathtime IS NOT NULL AND adm.deathtime <= c.course_start + INTERVAL '72' HOUR
         AND c.course_end >= adm.deathtime - INTERVAL '24' HOUR)
),
soi_ok AS (
  SELECT soi.*
  FROM suspicion_of_infection soi
  JOIN patients pt ON pt.subject_id = soi.subject_id
  WHERE soi.suspected_infection = 1 AND soi.stay_id IS NOT NULL AND pt.anchor_age >= 18
    AND EXISTS (SELECT 1 FROM course_ok co
                 WHERE co.hadm_id = soi.hadm_id
                   AND soi.antibiotic_time BETWEEN co.course_start AND co.course_end)
),
sofa_rise AS (
  SELECT
    stay_id,
    endtime,
    respiration_24hours AS respiration,
    coagulation_24hours AS coagulation,
    liver_24hours AS liver,
    cardiovascular_24hours AS cardiovascular,
    cns_24hours AS cns,
    renal_24hours AS renal,
    sofa_24hours AS sofa_score,
    sofa_24hours - MIN(sofa_24hours) OVER (
      PARTITION BY stay_id ORDER BY hr ROWS UNBOUNDED PRECEDING) AS sofa_rise
  FROM sofa
),
s1 AS (
  SELECT
    soi.subject_id,
    soi.stay_id,
    soi.antibiotic_time,
    soi.culture_time,
    soi.suspected_infection_time,
    sr.endtime,
    sr.sofa_score,
    sr.sofa_rise,
    sr.respiration, sr.coagulation, sr.liver, sr.cardiovascular, sr.cns, sr.renal,
    TRUE AS sepsis3,
    ROW_NUMBER() OVER (PARTITION BY soi.stay_id ORDER BY soi.suspected_infection_time NULLS FIRST,
                       soi.antibiotic_time NULLS FIRST, soi.culture_time NULLS FIRST, sr.endtime NULLS FIRST) AS rn_sus
  FROM soi_ok soi
  JOIN sofa_rise sr
    ON soi.stay_id = sr.stay_id
   AND sr.endtime >= soi.suspected_infection_time - INTERVAL '48' HOUR
   AND sr.endtime <= soi.suspected_infection_time + INTERVAL '24' HOUR
   AND sr.sofa_rise >= 2
)
SELECT
  subject_id,
  stay_id,
  antibiotic_time,
  culture_time,
  suspected_infection_time,
  endtime AS sofa_time,
  sofa_score,
  sofa_rise,
  respiration,
  coagulation,
  liver,
  cardiovascular,
  cns,
  renal,
  sepsis3
FROM s1
WHERE rn_sus = 1;

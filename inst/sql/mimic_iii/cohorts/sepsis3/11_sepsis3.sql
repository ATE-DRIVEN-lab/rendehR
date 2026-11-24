-- Sepsis-3 (rendehR default): suspicion of infection on a sustained antibiotic
-- course, plus an acute SOFA rise of >= 2 points within the suspicion window
-- [SI_time - 48h, SI_time + 24h]; adults only. One row per ICU stay at the
-- earliest qualifying suspicion event.
--
-- - Suspected infection: an antibiotic paired with a culture taken in the 72 h
--   before or the 24 h after it (the mimic-code rule; SI time = the culture time
--   when it came first, else the antibiotic time), where the antibiotic belongs
--   to a course lasting >= 72 h - prescriptions less than 24 h apart are joined
--   into one course - or cut short by in-hospital death within 72 h of the course
--   start (YAIB `abx_cont`). MIMIC-III prescriptions carry dates, not times.
-- - SOFA rise: the hourly 24-h SOFA minus the lowest 24-h SOFA of the stay so far
--   (YAIB/ricu `delta_cummin`), so organ dysfunction present from admission (an
--   injury, sedation, a vasopressor started on arrival) is the baseline, not
--   sepsis.
-- - Adults: age >= 18 at ICU admission.
-- The previous rule (any antibiotic, absolute SOFA >= 2 from a baseline of 0) is
-- variants/sepsis3_absolute.sql, selected with definition = "absolute".
CREATE OR REPLACE TABLE sepsis3 AS
WITH ab AS (
  SELECT icustay_id,
         CAST(antibiotic_time AS TIMESTAMP) AS starttime,
         CAST(GREATEST(antibiotic_time, COALESCE(antibiotic_endtime, antibiotic_time)) AS TIMESTAMP) AS stoptime
  FROM abx_micro_poe
  WHERE antibiotic_time IS NOT NULL
),
ab_prev AS (
  SELECT ab.*,
         MAX(stoptime) OVER (PARTITION BY icustay_id ORDER BY starttime, stoptime
                             ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_stop
  FROM ab
),
ab_course AS (
  SELECT ab_prev.*,
         SUM(CASE WHEN prev_stop IS NULL OR starttime > prev_stop + INTERVAL 24 HOUR THEN 1 ELSE 0 END)
           OVER (PARTITION BY icustay_id ORDER BY starttime, stoptime ROWS UNBOUNDED PRECEDING) AS course_id
  FROM ab_prev
),
course AS (
  SELECT icustay_id, course_id, MIN(starttime) AS course_start, MAX(stoptime) AS course_end
  FROM ab_course
  GROUP BY icustay_id, course_id
),
stay AS (
  SELECT ie.icustay_id, ie.subject_id, ie.hadm_id, adm.deathtime,
         DATE_DIFF('year', CAST(pt.dob AS TIMESTAMP), CAST(ie.intime AS TIMESTAMP)) AS age_years
  FROM icustays ie
  JOIN admissions adm ON adm.hadm_id = ie.hadm_id
  JOIN patients pt ON pt.subject_id = ie.subject_id
),
course_ok AS (
  SELECT c.*
  FROM course c
  JOIN stay st ON st.icustay_id = c.icustay_id
  WHERE c.course_end - c.course_start >= INTERVAL 72 HOUR
     OR (st.deathtime IS NOT NULL AND st.deathtime <= c.course_start + INTERVAL 72 HOUR
         AND c.course_end >= st.deathtime - INTERVAL 24 HOUR)
),
si_all AS (
  SELECT
    st.subject_id, st.hadm_id, am.icustay_id,
    am.suspected_infection_time,
    am.specimen, am.positiveculture
  FROM abx_micro_poe am
  JOIN stay st ON am.icustay_id = st.icustay_id
  WHERE am.suspected_infection_time IS NOT NULL
    AND (am.last72_charttime IS NOT NULL OR am.next24_charttime IS NOT NULL)
    AND st.age_years >= 18
    AND EXISTS (SELECT 1 FROM course_ok co
                 WHERE co.icustay_id = am.icustay_id
                   AND CAST(am.antibiotic_time AS TIMESTAMP) BETWEEN co.course_start AND co.course_end)
),
sofa_rise AS (
  SELECT
    icustay_id, sofa_time,
    respiration_24hours    AS respiration,
    coagulation_24hours    AS coagulation,
    liver_24hours          AS liver,
    cardiovascular_24hours AS cardiovascular,
    cns_24hours            AS cns,
    renal_24hours          AS renal,
    sofa_24hours           AS sofa_score,
    sofa_24hours - MIN(sofa_24hours) OVER (
      PARTITION BY icustay_id ORDER BY hr ROWS UNBOUNDED PRECEDING) AS sofa_rise
  FROM sofa
),
matched AS (
  SELECT
    si.subject_id, si.hadm_id, si.icustay_id,
    si.suspected_infection_time, si.specimen, si.positiveculture,
    sr.sofa_time, sr.sofa_score, sr.sofa_rise,
    sr.respiration, sr.coagulation, sr.liver, sr.cardiovascular, sr.cns, sr.renal,
    ROW_NUMBER() OVER (
      PARTITION BY si.icustay_id
      ORDER BY si.suspected_infection_time, sr.sofa_time
    ) AS rn
  FROM si_all si
  JOIN sofa_rise sr
    ON si.icustay_id = sr.icustay_id
   AND sr.sofa_time >= si.suspected_infection_time - INTERVAL 48 HOUR
   AND sr.sofa_time <= si.suspected_infection_time + INTERVAL 24 HOUR
   AND sr.sofa_rise >= 2
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
  sofa_rise,
  respiration, coagulation, liver, cardiovascular, cns, renal,
  TRUE AS sepsis3
FROM matched
WHERE rn = 1;

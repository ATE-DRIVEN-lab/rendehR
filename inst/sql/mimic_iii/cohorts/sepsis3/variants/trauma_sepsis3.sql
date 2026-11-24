-- Sepsis-3 for trauma admissions (bundle = "trauma_sepsis3"), after the
-- trauma-cohort sepsis-onset definition of ML4UWHealth/SepsisOnset_TraumaCohort
-- (MIT; https://github.com/ML4UWHealth/SepsisOnset_TraumaCohort). Hospital-
-- acquired sepsis, one decision per hospital admission:
--
-- - Suspected infection: a blood culture taken >= 72 h after hospital admission,
--   with a qualifying antibiotic course starting within 2 days of it. A course
--   joins prescriptions of the same drug less than 24 h apart; it qualifies when
--   it is intravenous (or oral vancomycin or linezolid), starts after hospital
--   day 1, is new (no course of the same drug ended in the 48 h before it), and
--   lasts >= 4 calendar days or until death or discharge. The 4-day minimum
--   leaves out single-dose surgical prophylaxis.
-- - Organ dysfunction: a rise of >= 2 points of a SOFA without its CNS (GCS) and
--   urine-output parts (respiration, coagulation, liver, cardiovascular and
--   creatinine), between any two hours within 3 days before and after the
--   culture, over all ICU stays of the admission.
-- - Onset: the earliest qualifying culture. Adults only (age >= 18 at admission).
-- MIMIC-III prescriptions carry dates, not times.
-- Every ICU stay of a septic admission is selected, with the onset as its
-- suspected_infection_time.
CREATE OR REPLACE TABLE sepsis3 AS
WITH adm AS (
  SELECT a.hadm_id, a.subject_id, a.admittime, a.dischtime, a.deathtime
  FROM admissions a
  JOIN patients p ON p.subject_id = a.subject_id
  WHERE DATE_DIFF('year', CAST(p.dob AS TIMESTAMP), CAST(a.admittime AS TIMESTAMP)) >= 18
),
bc AS (
  SELECT DISTINCT m.hadm_id, COALESCE(m.charttime, CAST(m.chartdate AS TIMESTAMP)) AS culture_time
  FROM microbiologyevents m
  JOIN adm a ON a.hadm_id = m.hadm_id
  WHERE UPPER(m.spec_type_desc) = 'BLOOD CULTURE'
    AND COALESCE(m.charttime, CAST(m.chartdate AS TIMESTAMP)) >= a.admittime + INTERVAL '72' HOUR
),
rx AS (
  SELECT pr.hadm_id, LOWER(pr.drug) AS drug, CAST(pr.startdate AS TIMESTAMP) AS starttime,
         CAST(GREATEST(pr.startdate, COALESCE(pr.enddate, pr.startdate)) AS TIMESTAMP) AS stoptime
  FROM prescriptions pr
  JOIN abx_poe_list ab ON ab.drug = pr.drug
  WHERE pr.hadm_id IS NOT NULL AND pr.startdate IS NOT NULL
    AND (UPPER(pr.route) LIKE 'IV%' OR UPPER(pr.route) = 'PB'
         OR ((LOWER(pr.drug) LIKE '%vanco%' OR LOWER(pr.drug) LIKE '%linezolid%')
             AND UPPER(pr.route) IN ('PO', 'NG', 'PO/NG', 'OG')))
),
rx_prev AS (
  SELECT rx.*, MAX(stoptime) OVER (PARTITION BY hadm_id, drug ORDER BY starttime, stoptime
                                   ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_stop
  FROM rx
),
rx_course AS (
  SELECT rx_prev.*,
         SUM(CASE WHEN prev_stop IS NULL OR starttime > prev_stop + INTERVAL '24' HOUR THEN 1 ELSE 0 END)
           OVER (PARTITION BY hadm_id, drug ORDER BY starttime, stoptime ROWS UNBOUNDED PRECEDING) AS course_id
  FROM rx_prev
),
course AS (
  SELECT hadm_id, drug, course_id, MIN(starttime) AS course_start, MAX(stoptime) AS course_end
  FROM rx_course GROUP BY hadm_id, drug, course_id
),
course_prev AS (
  SELECT course.*, LAG(course_end) OVER (PARTITION BY hadm_id, drug ORDER BY course_start) AS prev_course_end
  FROM course
),
course_ok AS (
  SELECT c.*
  FROM course_prev c
  JOIN adm a ON a.hadm_id = c.hadm_id
  WHERE CAST(c.course_start AS DATE) > CAST(a.admittime AS DATE)
    AND (c.prev_course_end IS NULL OR c.course_start > c.prev_course_end + INTERVAL '48' HOUR)
    AND (DATE_DIFF('day', CAST(c.course_start AS DATE), CAST(c.course_end AS DATE)) + 1 >= 4
         OR c.course_end >= COALESCE(a.deathtime, a.dischtime) - INTERVAL '24' HOUR)
),
si AS (
  SELECT DISTINCT bc.hadm_id, bc.culture_time
  FROM bc
  JOIN course_ok co ON co.hadm_id = bc.hadm_id
   AND co.course_start BETWEEN bc.culture_time - INTERVAL '48' HOUR AND bc.culture_time + INTERVAL '48' HOUR
),
tsofa AS (
  SELECT s.hadm_id, s.icustay_id, CAST(s.sofa_time AS TIMESTAMP) AS endtime,
         s.respiration_24hours + s.coagulation_24hours + s.liver_24hours + s.cardiovascular_24hours
           + COALESCE(MAX(CASE WHEN lb.creatinine_max >= 5.0 THEN 4 WHEN lb.creatinine_max >= 3.5 THEN 3
                               WHEN lb.creatinine_max >= 2.0 THEN 2 WHEN lb.creatinine_max >= 1.2 THEN 1
                               WHEN lb.creatinine_max IS NOT NULL THEN 0 END)
                      OVER (PARTITION BY s.icustay_id ORDER BY s.hr ROWS BETWEEN 23 PRECEDING AND 0 FOLLOWING), 0)
           AS sofa_score
  FROM sofa s
  LEFT JOIN sep_lab lb ON lb.icustay_id = s.icustay_id AND lb.hr = s.hr
),
win AS (
  SELECT si.hadm_id, si.culture_time, t.endtime, t.sofa_score,
         t.sofa_score - MIN(t.sofa_score) OVER (
           PARTITION BY si.hadm_id, si.culture_time ORDER BY t.endtime ROWS UNBOUNDED PRECEDING) AS sofa_rise
  FROM si
  JOIN tsofa t ON t.hadm_id = si.hadm_id
   AND t.endtime BETWEEN si.culture_time - INTERVAL '72' HOUR AND si.culture_time + INTERVAL '72' HOUR
),
onset AS (
  SELECT hadm_id, culture_time, endtime AS sofa_time, sofa_score, sofa_rise,
         ROW_NUMBER() OVER (PARTITION BY hadm_id ORDER BY culture_time, endtime) AS rn
  FROM win
  WHERE sofa_rise >= 2
)
SELECT
  ie.subject_id,
  ie.hadm_id,
  ie.icustay_id,
  o.culture_time AS suspected_infection_time,
  o.sofa_time,
  o.sofa_score,
  o.sofa_rise,
  TRUE AS sepsis3
FROM onset o
JOIN icustays ie ON ie.hadm_id = o.hadm_id
WHERE o.rn = 1;

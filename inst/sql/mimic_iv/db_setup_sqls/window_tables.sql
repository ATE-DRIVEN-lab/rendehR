-- Window spines for MIMIC-IV.
-- admission_windows: one row per hospital admission.
-- icu_windows: one row per ICU stay only.
-- visit_windows: admission-to-ICU crosswalk, retaining admissions without ICU.

CREATE TABLE admission_windows AS
SELECT
    a.subject_id,
    a.hadm_id,
    a.admittime,
    a.dischtime,
    ROUND((EXTRACT(EPOCH FROM (a.dischtime - a.admittime)) / 60.0)::numeric) AS hadm_los_min
FROM admissions a;

CREATE TABLE icu_windows AS
SELECT
    a.subject_id,
    a.hadm_id,
    i.stay_id,
    a.admittime,
    a.dischtime,
    i.intime  AS icu_intime,
    i.outtime AS icu_outtime,
    ROUND((EXTRACT(EPOCH FROM (i.outtime - i.intime)) / 60.0)::numeric) AS icu_los_min
FROM admissions a
INNER JOIN icustays i ON a.hadm_id = i.hadm_id;

CREATE TABLE visit_windows AS
SELECT
    a.subject_id,
    a.hadm_id,
    i.stay_id,
    a.admittime,
    a.dischtime,
    i.intime  AS icu_intime,
    i.outtime AS icu_outtime,
    ROUND((EXTRACT(EPOCH FROM (i.outtime - i.intime)) / 60.0)::numeric) AS icu_los_min
FROM admissions a
LEFT JOIN icustays i ON a.hadm_id = i.hadm_id;

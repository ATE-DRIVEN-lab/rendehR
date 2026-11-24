-- Window spines for eICU.
-- eICU is already unit-stay grain; visit_windows and icu_windows are the same
-- unit-stay spine. admission_windows aggregates unit stays to the hospital stay.
-- Offsets are relative to each unit stay's own admit, so both offset columns
-- come from one reference row (the earliest unit stay), not MIN/MAX across rows.

CREATE TABLE admission_windows AS
SELECT
    pat.uniquepid,
    pat.patienthealthsystemstayid,
    (array_agg(pat.hospitaladmitoffset ORDER BY pat.hospitaladmitoffset DESC NULLS LAST,
               pat.patientunitstayid))[1] AS hospitaladmitoffset,
    (array_agg(pat.hospitaldischargeoffset ORDER BY pat.hospitaladmitoffset DESC NULLS LAST,
               pat.patientunitstayid))[1] AS hospitaldischargeoffset,
    0::numeric AS admittime,
    MAX(pat.hospitaldischargeoffset - pat.hospitaladmitoffset) AS dischtime,
    MAX(pat.hospitaldischargeoffset - pat.hospitaladmitoffset) AS hadm_los_min
FROM patient pat
GROUP BY pat.uniquepid, pat.patienthealthsystemstayid;

CREATE TABLE icu_windows AS
SELECT
    pat.uniquepid,
    pat.patientunitstayid,
    pat.patienthealthsystemstayid,
    pat.hospitaladmitoffset,
    pat.hospitaldischargeoffset,
    pat.unitdischargeoffset,
    NULL AS hospital_start,
    NULL AS hospital_end,
    0::numeric AS admittime,
    (pat.hospitaldischargeoffset - pat.hospitaladmitoffset) AS dischtime,
    -pat.hospitaladmitoffset AS icu_intime,
    (pat.unitdischargeoffset - pat.hospitaladmitoffset) AS icu_outtime,
    pat.unitdischargeoffset AS icu_los_min
FROM patient pat;

CREATE TABLE visit_windows AS
SELECT * FROM icu_windows;

-- hospitaldischargeoffset < hospitaladmitoffset is a data-quality artifact (16 of 200,859
-- rows, min -0.8625 days) -- physically impossible (discharge before admission), so
-- hospital_los is NULL in that case rather than passing a negative LOS through. Matches ricu's
-- declared min:0 bound for the equivalent los_hosp concept. Grain check: this value is
-- constant across all multi-unit-stay health-system-stays, so reading it from any one
-- unit-stay row is safe.

DROP TABLE IF EXISTS {concepts_schema}.hospital_los;

CREATE TABLE {concepts_schema}.hospital_los AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    -- raw LOS in minutes (native eICU unit, derived from two offsets)
    NULL::integer AS hospital_los_raw,

    -- harmonized LOS in days, rounded to 2 decimal places
    CASE WHEN pt.hospitaldischargeoffset >= pt.hospitaladmitoffset
        THEN ROUND((pt.hospitaldischargeoffset - pt.hospitaladmitoffset)::numeric / 1440, 2)
    END AS hospital_los,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

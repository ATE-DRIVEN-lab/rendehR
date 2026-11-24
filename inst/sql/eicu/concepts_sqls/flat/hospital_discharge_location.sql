-- NOTE: eICU also captures ICU-level discharge via patient.unitdischargelocation.
-- A separate icu_discharge_location concept using that column should be created
-- once an equivalent ICU-level derivation from MIMIC transfers is implemented.
-- All 8 non-blank hospitaldischargelocation values are covered exactly. Blank string
-- ('', 2,033 rows) maps to unknown, not other: the CASE checks both IS NULL and = '', the
-- same handling as in age/sex/hospital_mortality/hospital_admission_location.
-- Harmonized values are lowercase snake_case, matching the other categorical concepts.

DROP TABLE IF EXISTS {concepts_schema}.hospital_discharge_location;

CREATE TABLE {concepts_schema}.hospital_discharge_location AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.hospitaldischargelocation AS hospital_discharge_location_raw,

    CASE
        WHEN pt.hospitaldischargelocation IS NULL
          OR TRIM(pt.hospitaldischargelocation) = ''
          OR UPPER(TRIM(pt.hospitaldischargelocation)) = 'NA'             THEN 'unknown'

        -- HOME
        WHEN UPPER(TRIM(pt.hospitaldischargelocation)) = 'HOME'           THEN 'home'

        -- DEATH
        WHEN UPPER(TRIM(pt.hospitaldischargelocation)) = 'DEATH'          THEN 'death'

        -- EXTERNAL HOSPITAL
        WHEN UPPER(TRIM(pt.hospitaldischargelocation)) = 'OTHER HOSPITAL' THEN 'acute_hospital'

        -- POST-ACUTE FACILITIES
        WHEN UPPER(TRIM(pt.hospitaldischargelocation)) = 'REHABILITATION' THEN 'rehabilitation'
        WHEN UPPER(TRIM(pt.hospitaldischargelocation)) = 'SKILLED NURSING FACILITY' THEN 'skilled_nursing_facility'
        WHEN UPPER(TRIM(pt.hospitaldischargelocation)) = 'NURSING HOME'   THEN 'nursing_facility'

        -- OTHER
        WHEN UPPER(TRIM(pt.hospitaldischargelocation)) IN (
            'OTHER',
            'OTHER EXTERNAL'
        )                                                                 THEN 'other'

        ELSE 'other'
    END AS hospital_discharge_location,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

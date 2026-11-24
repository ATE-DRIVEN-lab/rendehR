-- NOTE: eICU also has patient.unitadmitsource (ICU-level admit source, e.g. where the patient came from
-- immediately before the ICU unit). A separate icu_admission_location concept using that column
-- should be created for eICU once an equivalent derivation from MIMIC transfers is implemented.
--
-- admission_type/admission_location is a two-variable ontology -- this concept captures
-- WHERE/HOW the patient entered the hospital pathway (source/point of origin), independent
-- of admission urgency (admission_type).
-- External facility names are kept granular and aligned 1-to-1 with
-- hospital_discharge_location. Categories:
--   emergency_department, direct_admit, acute_hospital, internal_transfer,
--   perioperative, observation, other, unknown
-- Mapping verified against all 16 distinct hospitaladmitsource values + NULL/NA (no gaps).
-- Blank string ('', 49,464 rows) is treated as unknown, not other.

DROP TABLE IF EXISTS {concepts_schema}.hospital_admission_location;

CREATE TABLE {concepts_schema}.hospital_admission_location AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.hospitaladmitsource AS hospital_admission_location_raw,

    CASE
        WHEN pt.hospitaladmitsource IS NULL
          OR TRIM(pt.hospitaladmitsource) = ''
          OR UPPER(TRIM(pt.hospitaladmitsource)) = 'NA'         THEN 'unknown'

        -- EMERGENCY DEPARTMENT
        WHEN UPPER(TRIM(pt.hospitaladmitsource)) = 'EMERGENCY DEPARTMENT' THEN 'emergency_department'

        -- DIRECT ADMIT (Scheduled / Physician-directed admission bypassing ED)
        WHEN UPPER(TRIM(pt.hospitaladmitsource)) = 'DIRECT ADMIT' THEN 'direct_admit'

        -- EXTERNAL HOSPITAL / ICU TRANSFER (aligned 1-to-1 with MIMIC)
        WHEN UPPER(TRIM(pt.hospitaladmitsource)) IN (
            'OTHER HOSPITAL',
            'OTHER ICU'
        )                                                       THEN 'acute_hospital'

        -- INTERNAL HOSPITAL TRANSFER (Within same facility)
        WHEN UPPER(TRIM(pt.hospitaladmitsource)) IN (
            'FLOOR',
            'ACUTE CARE/FLOOR',
            'STEP-DOWN UNIT (SDU)',
            'ICU',
            'ICU TO SDU'
        )                                                       THEN 'internal_transfer'

        -- PERIOPERATIVE / SURGICAL
        WHEN UPPER(TRIM(pt.hospitaladmitsource)) IN (
            'OPERATING ROOM',
            'RECOVERY ROOM',
            'PACU'
        )                                                       THEN 'perioperative'

        -- OBSERVATION / SHORT-STAY EVALUATION UNIT
        WHEN UPPER(TRIM(pt.hospitaladmitsource)) IN (
            'OBSERVATION',
            'CHEST PAIN CENTER'
        )                                                       THEN 'observation'

        -- OTHER
        WHEN UPPER(TRIM(pt.hospitaladmitsource)) = 'OTHER'      THEN 'other'

        ELSE 'other'
    END AS hospital_admission_location,

    'patient'      AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

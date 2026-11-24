-- admission_type/admission_location is a two-variable ontology -- this concept captures
-- WHERE/HOW the patient entered the hospital pathway (source/point of origin), independent
-- of admission urgency (admission_type) and clinical service (admission_service).
-- This taxonomy is used instead of BlendedICU's cross-source (eICU/MIMIC/AUMC/HiRID)
-- category list, which would put ~61% of MIMIC-III admissions in 'other' and collapse
-- genuinely different pathways (physician referral, inter-hospital transfer,
-- nursing-facility transfer) that the raw column distinguishes.
-- External facility names are kept granular and aligned 1-to-1 with
-- hospital_discharge_location. Categories:
--   emergency_department, referral, acute_hospital, skilled_nursing_facility,
--   other_facility, internal_transfer, other, unknown
-- Mapping verified against all 9 distinct admission_location values + NULL/NA (no gaps).

DROP TABLE IF EXISTS {concepts_schema}.hospital_admission_location;

CREATE TABLE {concepts_schema}.hospital_admission_location AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    adm.admission_location AS hospital_admission_location_raw,

    CASE
        WHEN adm.admission_location IS NULL
          OR TRIM(adm.admission_location) = ''
          OR UPPER(TRIM(adm.admission_location)) IN ('NA', '** INFO NOT AVAILABLE **') THEN 'unknown'

        -- EMERGENCY DEPARTMENT
        WHEN UPPER(TRIM(adm.admission_location)) = 'EMERGENCY ROOM ADMIT'      THEN 'emergency_department'

        -- REFERRAL (Home / Outpatient Clinic / HMO)
        WHEN UPPER(TRIM(adm.admission_location)) IN (
            'PHYS REFERRAL/NORMAL DELI',
            'CLINIC REFERRAL/PREMATURE',
            'HMO REFERRAL/SICK'
        )                                                                      THEN 'referral'

        -- EXTERNAL FACILITIES (aligned 1-to-1 with discharge labels)
        WHEN UPPER(TRIM(adm.admission_location)) = 'TRANSFER FROM HOSP/EXTRAM' THEN 'acute_hospital'
        WHEN UPPER(TRIM(adm.admission_location)) = 'TRANSFER FROM SKILLED NUR' THEN 'skilled_nursing_facility'
        WHEN UPPER(TRIM(adm.admission_location)) = 'TRANSFER FROM OTHER HEALT' THEN 'other_facility'

        -- INTERNAL TRANSFER (Same hospital stay / facility)
        WHEN UPPER(TRIM(adm.admission_location)) = 'TRSF WITHIN THIS FACILITY' THEN 'internal_transfer'

        ELSE 'other'
    END AS hospital_admission_location,

    'admissions'   AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

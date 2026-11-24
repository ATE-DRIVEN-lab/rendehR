-- admission_type/admission_location is a two-variable ontology -- this concept captures
-- WHERE/HOW the patient entered the hospital pathway (source/point of origin), independent
-- of admission urgency (admission_type) and clinical service (admission_service).
-- This taxonomy is used instead of BlendedICU's cross-source (eICU/MIMIC/AUMC/HiRID)
-- category list, which would put ~45% of MIMIC-IV admissions in 'other' and collapse
-- genuinely different pathways (physician referral, inter-hospital transfer,
-- nursing-facility transfer, internal psych transfer) that the raw column distinguishes.
-- External facility names are kept granular and aligned 1-to-1 with
-- hospital_discharge_location. Categories:
--   emergency_department, referral, self_referral, acute_hospital,
--   skilled_nursing_facility, psychiatric_internal_transfer, perioperative, other, unknown
-- Mapping verified against all 11 non-null distinct admission_location values + NULL/NA (no gaps).

DROP TABLE IF EXISTS {concepts_schema}.hospital_admission_location;

CREATE TABLE {concepts_schema}.hospital_admission_location AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    adm.admission_location AS hospital_admission_location_raw,

    CASE
        WHEN adm.admission_location IS NULL
          OR TRIM(adm.admission_location) = ''
          OR UPPER(TRIM(adm.admission_location)) IN ('NA', 'INFORMATION NOT AVAILABLE') THEN 'unknown'

        -- EMERGENCY DEPARTMENT
        WHEN UPPER(TRIM(adm.admission_location)) = 'EMERGENCY ROOM'      THEN 'emergency_department'

        -- REFERRAL / SELF-REFERRAL (Home / Outpatient Clinic)
        WHEN UPPER(TRIM(adm.admission_location)) IN (
            'PHYSICIAN REFERRAL',
            'CLINIC REFERRAL'
        )                                                                THEN 'referral'
        WHEN UPPER(TRIM(adm.admission_location)) = 'WALK-IN/SELF REFERRAL' THEN 'self_referral'

        -- EXTERNAL FACILITIES (aligned 1-to-1 with discharge labels)
        WHEN UPPER(TRIM(adm.admission_location)) = 'TRANSFER FROM HOSPITAL' THEN 'acute_hospital'
        WHEN UPPER(TRIM(adm.admission_location)) = 'TRANSFER FROM SKILLED NURSING FACILITY' THEN 'skilled_nursing_facility'

        -- INTERNAL PSYCHIATRIC TRANSFER
        WHEN UPPER(TRIM(adm.admission_location)) = 'INTERNAL TRANSFER TO OR FROM PSYCH' THEN 'psychiatric_internal_transfer'

        -- PERIOPERATIVE / SURGICAL
        WHEN UPPER(TRIM(adm.admission_location)) IN (
            'PROCEDURE SITE',
            'PACU',
            'AMBULATORY SURGERY TRANSFER'
        )                                                                THEN 'perioperative'

        ELSE 'other'
    END AS hospital_admission_location,

    'admissions'   AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

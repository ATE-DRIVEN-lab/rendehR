-- admission_type/admission_location is a two-variable ontology -- this concept captures
-- WHERE/HOW the patient entered the hospital pathway (source/point of origin), independent
-- of admission urgency (admission_type) and clinical service (admission_service).
-- This compact taxonomy is used instead of BlendedICU's cross-source (eICU/MIMIC/AUMC/HiRID)
-- category list, which would put 61% of MIMIC-III admissions in 'other', collapsing
-- genuinely different pathways (physician referral, inter-hospital transfer,
-- nursing-facility transfer) that the raw column distinguishes. Categories:
--   emergency_department, referral, self_referral, direct_admit, external_transfer,
--   internal_transfer, perioperative, other, unknown
-- Mapping verified against all 9 distinct admission_location values (no gaps).

DROP TABLE IF EXISTS {concepts_schema}.hospital_admission_location;

CREATE TABLE {concepts_schema}.hospital_admission_location AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    adm.admission_location AS hospital_admission_location_raw,

    CASE
        WHEN adm.admission_location IS NULL THEN 'unknown'

        WHEN adm.admission_location = 'EMERGENCY ROOM ADMIT' THEN 'emergency_department'

        WHEN adm.admission_location IN (
            'PHYS REFERRAL/NORMAL DELI',
            'CLINIC REFERRAL/PREMATURE',
            'HMO REFERRAL/SICK'
        ) THEN 'referral'

        WHEN adm.admission_location IN (
            'TRANSFER FROM HOSP/EXTRAM',
            'TRANSFER FROM SKILLED NUR',
            'TRANSFER FROM OTHER HEALT'
        ) THEN 'external_transfer'

        WHEN adm.admission_location = 'TRSF WITHIN THIS FACILITY' THEN 'internal_transfer'

        WHEN adm.admission_location = '** INFO NOT AVAILABLE **' THEN 'unknown'

        ELSE 'other'
    END AS hospital_admission_location,

    'admissions'   AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

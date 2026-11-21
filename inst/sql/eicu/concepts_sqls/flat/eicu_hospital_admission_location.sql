-- NOTE: eICU also has patient.unitadmitsource (ICU-level admit source, e.g. where the patient came from
-- immediately before the ICU unit). A separate icu_admission_location concept using that column
-- should be created for eICU once an equivalent derivation from MIMIC transfers is implemented.
--
-- admission_type/admission_location is a two-variable ontology -- this concept captures
-- WHERE/HOW the patient entered the hospital pathway (source/point of origin), independent
-- of admission urgency (admission_type). Compact taxonomy shared with the MIMIC-III/IV files:
--   emergency_department, referral, self_referral, direct_admit, external_transfer,
--   internal_transfer, perioperative, other, unknown
-- Mapping verified against all 16 distinct hospitaladmitsource values (no gaps).
-- Blank string ('', 49,464 rows) is treated as unknown, not other: the CASE checks both
-- IS NULL and = ''.

DROP TABLE IF EXISTS {concepts_schema}.hospital_admission_location;

CREATE TABLE {concepts_schema}.hospital_admission_location AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.hospitaladmitsource AS hospital_admission_location_raw,

    CASE
        WHEN pt.hospitaladmitsource IS NULL
          OR pt.hospitaladmitsource = ''                THEN 'unknown'

        WHEN pt.hospitaladmitsource = 'Emergency Department' THEN 'emergency_department'

        WHEN pt.hospitaladmitsource = 'Direct Admit' THEN 'direct_admit'

        WHEN pt.hospitaladmitsource IN ('Other Hospital', 'Other ICU') THEN 'external_transfer'

        WHEN pt.hospitaladmitsource IN (
            'Floor',
            'Acute Care/Floor',
            'Step-Down Unit (SDU)',
            'ICU',
            'ICU to SDU'
        ) THEN 'internal_transfer'

        WHEN pt.hospitaladmitsource IN (
            'Operating Room',
            'Recovery Room',
            'PACU'
        ) THEN 'perioperative'

        ELSE 'other'
    END AS hospital_admission_location,

    'patient'      AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

-- All 17 MIMIC-III discharge_location values are covered exactly by this CASE, no gaps. The
-- 'other' bucket is small (2.7% of rows), so the category taxonomy is kept as is, unlike
-- hospital_admission_location. Harmonized values are lowercase snake_case, matching the
-- other categorical concepts.

DROP TABLE IF EXISTS {concepts_schema}.hospital_discharge_location;

CREATE TABLE {concepts_schema}.hospital_discharge_location AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    adm.discharge_location AS hospital_discharge_location_raw,

    CASE
        WHEN adm.discharge_location IS NULL
          OR TRIM(adm.discharge_location) = ''
          OR UPPER(TRIM(adm.discharge_location)) = 'NA'                THEN 'unknown'

        -- HOME
        WHEN UPPER(TRIM(adm.discharge_location)) = 'HOME'              THEN 'home'
        WHEN UPPER(TRIM(adm.discharge_location)) IN (
            'HOME HEALTH CARE',
            'HOME WITH HOME IV PROVIDR'
        )                                                              THEN 'home_health'

        -- DEATH
        WHEN UPPER(TRIM(adm.discharge_location)) = 'DEAD/EXPIRED'      THEN 'death'

        -- HOSPICE
        WHEN UPPER(TRIM(adm.discharge_location)) IN (
            'HOSPICE-HOME',
            'HOSPICE-MEDICAL FACILITY'
        )                                                              THEN 'hospice'

        -- EXTERNAL HOSPITALS
        WHEN UPPER(TRIM(adm.discharge_location)) IN (
            'SHORT TERM HOSPITAL',
            'DISC-TRAN CANCER/CHLDRN H',
            'DISC-TRAN TO FEDERAL HC'
        )                                                              THEN 'acute_hospital'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'LONG TERM CARE HOSPITAL' THEN 'long_term_acute_care'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'DISCH-TRAN TO PSYCH HOSP' THEN 'psychiatric_facility'

        -- POST-ACUTE FACILITIES
        WHEN UPPER(TRIM(adm.discharge_location)) = 'REHAB/DISTINCT PART HOSP' THEN 'rehabilitation'
        WHEN UPPER(TRIM(adm.discharge_location)) IN (
            'SNF',
            'SNF-MEDICAID ONLY CERTIF'
        )                                                              THEN 'skilled_nursing_facility'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'ICF'               THEN 'nursing_facility'

        -- AGAINST MEDICAL ADVICE
        WHEN UPPER(TRIM(adm.discharge_location)) = 'LEFT AGAINST MEDICAL ADVI' THEN 'against_medical_advice'

        -- GENERIC EXTERNAL FACILITY
        WHEN UPPER(TRIM(adm.discharge_location)) = 'OTHER FACILITY'    THEN 'other_facility'

        ELSE 'other'
    END AS hospital_discharge_location,

    'admissions' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

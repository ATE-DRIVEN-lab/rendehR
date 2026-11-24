-- All 14 MIMIC-IV discharge_location values (including the large 149,818-row NULL bucket,
-- 27.4% of rows) are covered exactly by this CASE, no gaps. The 'other' bucket is small
-- (1.5% of rows), so the category taxonomy is kept as is, unlike
-- hospital_admission_location. Harmonized values are lowercase snake_case, matching the
-- other categorical concepts.

DROP TABLE IF EXISTS {concepts_schema}.hospital_discharge_location;

CREATE TABLE {concepts_schema}.hospital_discharge_location AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    adm.discharge_location AS hospital_discharge_location_raw,

    CASE
        WHEN adm.discharge_location IS NULL
          OR TRIM(adm.discharge_location) = ''
          OR UPPER(TRIM(adm.discharge_location)) = 'NA'                THEN 'unknown'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'HOME'              THEN 'home'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'HOME HEALTH CARE'  THEN 'home_health'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'DIED'              THEN 'death'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'HOSPICE'           THEN 'hospice'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'ACUTE HOSPITAL'    THEN 'acute_hospital'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'CHRONIC/LONG TERM ACUTE CARE' THEN 'long_term_acute_care'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'PSYCH FACILITY'    THEN 'psychiatric_facility'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'REHAB'             THEN 'rehabilitation'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'SKILLED NURSING FACILITY' THEN 'skilled_nursing_facility'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'ASSISTED LIVING'   THEN 'nursing_facility'
        WHEN UPPER(TRIM(adm.discharge_location)) = 'AGAINST ADVICE'    THEN 'against_medical_advice'
        WHEN UPPER(TRIM(adm.discharge_location)) IN (
            'HEALTHCARE FACILITY',
            'OTHER FACILITY'
        )                                                              THEN 'other_facility'
        ELSE 'other'
    END AS hospital_discharge_location,

    'admissions' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

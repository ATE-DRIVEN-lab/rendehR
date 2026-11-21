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
        WHEN adm.discharge_location IS NULL THEN 'unknown'

        -- HOSPITAL
        WHEN adm.discharge_location IN (
            'ACUTE HOSPITAL',
            'Acute Care/Floor',
            'CHRONIC/LONG TERM ACUTE CARE',
            'SHORT TERM HOSPITAL',
            'LONG TERM CARE HOSPITAL',
            'Floor',
            'HEALTHCARE FACILITY',
            'Other Hospital',
            'Step-Down Unit (SDU)',
            'Telemetry',
            'Other Internal'
        ) THEN 'hospital'

        -- HOME
        WHEN adm.discharge_location IN (
            'HOME',
            'Home',
            'HOME HEALTH CARE',
            'HOME WITH HOME IV PROVIDR'
        ) THEN 'home'

        -- DEATH
        WHEN adm.discharge_location IN (
            'DIED',
            'death',
            'Death',
            'Overleden',
            'DEAD/EXPIRED'
        ) THEN 'death'

        -- HOSPICE (discharged alive; hospital_mortality is 0)
        WHEN adm.discharge_location IN (
            'HOSPICE',
            'HOSPICE-HOME',
            'HOSPICE-MEDICAL FACILITY'
        ) THEN 'hospice'

        -- OTHER ICU
        WHEN adm.discharge_location IN (
            'ICU',
            'Operating Room',
            'Other ICU',
            'Other ICU (CABG)'
        ) THEN 'other_icu'

        -- REHABILITATION
        WHEN adm.discharge_location IN (
            'REHAB',
            'Rehabilitation',
            'REHAB/DISTINCT PART HOSP'
        ) THEN 'rehabilitation'

        -- NURSING FACILITY
        WHEN adm.discharge_location IN (
            'ASSISTED LIVING',
            'SKILLED NURSING FACILITY',
            'SNF',
            'Skilled Nursing Facility',
            'Nursing Home'
        ) THEN 'nursing_facility'

        -- OTHER
        WHEN adm.discharge_location IN (
            'AGAINST ADVICE',
            'Other',
            'Other External',
            'OTHER FACILITY',
            'PSYCH FACILITY',
            'DISC-TRAN CANCER/CHLDRN H',
            'DISCH-TRAN TO PSYCH HOSP',
            'DISC-TRAN TO FEDERAL HC',
            'SNF-MEDICAID ONLY CERTIF',
            'LEFT AGAINST MEDICAL ADVI',
            'ICF'
        ) THEN 'other'

        -- UNKNOWN
        WHEN adm.discharge_location IN (
            'unknown','1','2','3','4','5','8','9','10','12','13','14','15','16','17','18','19',
            '20','21','22','23','25','26','27','29','31','32','33','35','36','37','38','40',
            '41','42','44','45','46','47','48','49','50','51'
        ) THEN 'unknown'

        ELSE 'other'
    END AS hospital_discharge_location,

'admissions'  AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
  ON vw.hadm_id = adm.hadm_id;

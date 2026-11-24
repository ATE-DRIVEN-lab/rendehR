-- Harmonized category values are lowercase snake_case, matching the convention used for
-- race/sex/insurance/marital_status/language/care_site.

DROP TABLE IF EXISTS {concepts_schema}.icu_last_careunit;

CREATE TABLE {concepts_schema}.icu_last_careunit AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    icu.last_careunit AS icu_last_careunit_raw,

    CASE
        WHEN icu.last_careunit IS NULL THEN 'unknown'

        -- MEDICAL
        WHEN icu.last_careunit IN (
            'Medical Intensive Care Unit (MICU)',
            'Medicine',
            'Medicine/Cardiology Intermediate'
        ) THEN 'medical'

        -- MEDICAL-SURGICAL
        WHEN icu.last_careunit IN (
            'Medical/Surgical Intensive Care Unit (MICU/SICU)',
            'Med/Surg'
        ) THEN 'medical_surgical'

        -- SURGICAL
        WHEN icu.last_careunit IN (
            'Surgical Intensive Care Unit (SICU)',
            'Surgery/Trauma',
            'Surgery/Vascular/Intermediate'
        ) THEN 'surgical'

        -- CARDIAC
        WHEN icu.last_careunit IN (
            'Cardiac Vascular Intensive Care Unit (CVICU)',
            'Coronary Care Unit (CCU)'
        ) THEN 'cardiac'

        -- NEUROLOGICAL
        WHEN icu.last_careunit IN (
            'Neuro Surgical Intensive Care Unit (Neuro SICU)',
            'Neuro Intermediate',
            'Neuro Stepdown',
            'Neurology'
        ) THEN 'neurological'

        -- TRAUMA
        WHEN icu.last_careunit IN (
            'Trauma SICU (TSICU)'
        ) THEN 'trauma'

        ELSE 'other'
    END AS icu_last_careunit,

    'icustays' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.icustays icu
    ON vw.stay_id = icu.stay_id;

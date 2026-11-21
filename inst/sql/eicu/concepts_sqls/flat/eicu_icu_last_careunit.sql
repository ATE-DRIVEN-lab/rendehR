-- eICU has a single unittype per unit stay — no separate last_careunit column.
-- icu_last_careunit is therefore identical to icu_first_careunit for eICU.
-- Harmonized category values are lowercase snake_case, matching the convention used for
-- race/sex/insurance/marital_status/language/care_site.

DROP TABLE IF EXISTS {concepts_schema}.icu_last_careunit;

CREATE TABLE {concepts_schema}.icu_last_careunit AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.unittype AS icu_last_careunit_raw,

    CASE
        WHEN pt.unittype IS NULL THEN 'unknown'
        WHEN pt.unittype IN ('Unknown', 'unknown') THEN 'unknown'

        -- CARDIAC
        WHEN pt.unittype IN (
            'Cardiac ICU',
            'CCU-CTICU',
            'CCU'
        ) THEN 'cardiac'

        -- MEDICAL
        WHEN pt.unittype IN (
            'MICU'
        ) THEN 'medical'

        -- MEDICAL-SURGICAL
        WHEN pt.unittype IN (
            'Med-Surg ICU',
            'MICU-SICU'
        ) THEN 'medical_surgical'

        -- SURGICAL
        WHEN pt.unittype IN (
            'SICU',
            'CSICU',
            'CTICU'
        ) THEN 'surgical'

        -- NEUROLOGICAL
        WHEN pt.unittype IN (
            'Neuro ICU'
        ) THEN 'neurological'

        -- TRAUMA
        WHEN pt.unittype IN (
            'Burn-Trauma ICU',
            'TSICU'
        ) THEN 'trauma'

        -- OTHER
        WHEN pt.unittype IN (
            'step down',
            'CCOT/Step down/Other',
            'Other'
        ) THEN 'other'

        ELSE 'other'
    END AS icu_last_careunit,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

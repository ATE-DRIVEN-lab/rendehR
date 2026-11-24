-- MIMIC-III admission urgency from admissions.admission_type.
-- Values: ELECTIVE, EMERGENCY, URGENT, NEWBORN.
-- admission_type and admission_location form a two-variable ontology (admission_type = what
-- kind of admission; admission_location = where/how the patient entered), so admission_type
-- legitimately contains 'newborn' (and, for MIMIC-IV, 'observation'); it is not required to
-- be a pure urgency variable. The mapping covers all 4 raw values exactly; ELSE 'unknown'
-- (not NULL) follows the unknown-not-NULL convention used elsewhere and does not fire on any
-- row of the current data.

DROP TABLE IF EXISTS {concepts_schema}.admission_type;

CREATE TABLE {concepts_schema}.admission_type AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    adm.admission_type AS admission_type_raw,

    CASE
        WHEN adm.admission_type = 'ELECTIVE'  THEN 'elective'
        WHEN adm.admission_type = 'EMERGENCY' THEN 'emergency'
        WHEN adm.admission_type = 'URGENT'    THEN 'urgent'
        WHEN adm.admission_type = 'NEWBORN'   THEN 'newborn'
        ELSE 'unknown'
    END AS admission_type,

    'admissions'   AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

-- MIMIC-IV admission urgency from admissions.admission_type.
-- Values in MIMIC-IV are more granular than MIMIC-III:
--   elective    : ELECTIVE, SURGICAL SAME DAY ADMISSION
--   emergency   : EW EMER. (emergency ward), DIRECT EMER. (direct, bypasses ED)
--   urgent      : URGENT
--   observation : EU OBSERVATION, DIRECT OBSERVATION, AMBULATORY OBSERVATION,
--                 OBSERVATION ADMIT (not a true inpatient admission — monitoring only)
-- No NEWBORN category in MIMIC-IV.
-- admission_type and admission_location form a two-variable ontology (admission_type = what
-- kind of admission; admission_location = where/how the patient entered), so admission_type
-- legitimately contains 'observation'; it is not required to be a pure urgency variable.
-- SURGICAL SAME DAY ADMISSION belongs in 'elective': its admission_location pattern is 99.8%
-- PHYSICIAN REFERRAL, matching true ELECTIVE's 99.9% pattern almost exactly -- a genuinely
-- scheduled admission by origin, even though MIT's own scoring code treats it as distinct
-- from 'ELECTIVE' for unrelated severity-scoring purposes. The mapping covers all 9 raw
-- values exactly; ELSE 'unknown' (not NULL) is for consistency and does not fire on any row
-- of the current data.

DROP TABLE IF EXISTS {concepts_schema}.admission_type;

CREATE TABLE {concepts_schema}.admission_type AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.stay_id,

    adm.admission_type AS admission_type_raw,

    CASE
        WHEN adm.admission_type IN ('ELECTIVE', 'SURGICAL SAME DAY ADMISSION')
            THEN 'elective'
        WHEN adm.admission_type IN ('EW EMER.', 'DIRECT EMER.')
            THEN 'emergency'
        WHEN adm.admission_type = 'URGENT'
            THEN 'urgent'
        WHEN adm.admission_type IN (
            'EU OBSERVATION',
            'DIRECT OBSERVATION',
            'AMBULATORY OBSERVATION',
            'OBSERVATION ADMIT'
        )
            THEN 'observation'
        ELSE 'unknown'
    END AS admission_type,

    'admissions'   AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.admissions adm
    ON vw.hadm_id = adm.hadm_id;

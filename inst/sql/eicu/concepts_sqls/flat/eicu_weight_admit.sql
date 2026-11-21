-- eICU patient.admissionweight is in kg at hospital admission.
-- Plausible range: 1-500 kg (matching ricu's own declared bound for this concept). The upper
-- cap matters: 37 rows exceed 300kg (up to 953kg, clearly implausible for a human).
-- No neonate distinction available in eICU (no DOB exposed).
-- weight_admit is a HOSPITAL-VISIT-level fact (patienthealthsystemstayid), not an
-- ICU-UNIT-stay-level one (patientunitstayid) -- same rationale and mechanism as
-- eicu_height.sql. All ICU unit stays within one health system stay share the first VALID
-- (1-500kg) reading, ordered by hospitaladmitoffset DESC rather than unitvisitnumber -- same
-- rationale as eicu_height.sql (99.5% agreement on which unit stay is "first" across 27,635
-- multi-unit health-system-stays; hospitaladmitoffset is a direct time measure,
-- unitvisitnumber an assigned label). hospitaladmitoffset is <= 0, so DESC puts the
-- least-negative (earliest) unit admission first.

DROP TABLE IF EXISTS {concepts_schema}.weight_admit;

CREATE TABLE {concepts_schema}.weight_admit AS

WITH wt_valid AS (
    SELECT
        pt.patienthealthsystemstayid,
        pt.hospitaladmitoffset,
        pt.admissionweight AS weight_kg
    FROM {raw_schema}.patient pt
    WHERE pt.admissionweight BETWEEN 1 AND 500
),
wt_ranked AS (
    SELECT
        patienthealthsystemstayid,
        weight_kg,
        ROW_NUMBER() OVER (
            PARTITION BY patienthealthsystemstayid
            ORDER BY hospitaladmitoffset DESC
        ) AS rn
    FROM wt_valid
)

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    wr.weight_kg AS weight_admit_raw,
    ROUND(wr.weight_kg::numeric, 2) AS weight_admit,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN wt_ranked wr
    ON vw.patienthealthsystemstayid = wr.patienthealthsystemstayid
    AND wr.rn = 1;

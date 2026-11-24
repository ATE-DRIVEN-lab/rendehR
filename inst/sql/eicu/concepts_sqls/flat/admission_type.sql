-- eICU admission type: inferred from elective surgical status.
-- Harmonized values: elective, unknown.
--
-- eICU-CRD does not contain a directly recorded hospital admission
-- urgency field comparable to admission_type in MIMIC-III/IV.
--
-- As a pragmatic approximation, this implementation uses
-- apachepredvar.electivesurgery:
--   1         -> elective (inferred)
--   0 or NULL -> unknown
--
-- The source variable describes elective surgical status at the
-- ICU-stay level, not hospital admission urgency. Consequently,
-- an elective classification does not necessarily indicate an
-- elective hospital admission. Classification may also differ
-- across ICU stays within the same hospitalization.
--
-- hospitaladmitsource is not used because it describes admission
-- location rather than urgency.
--
-- In the full eICU-CRD dataset, electivesurgery is populated for
-- 33,256 of 200,859 unit stays (16.56%), of which 29,705 (14.79%
-- of all stays) are classified as elective. Availability is
-- predominantly restricted to operative admissions.
--
-- This concept has limited cross-dataset comparability and should
-- be interpreted cautiously when combining eICU and MIMIC data.
--
-- Future revisions may distinguish hospital admission urgency
-- from ICU-level surgical urgency using the structured admissiondx
-- form and additional admission context.

DROP TABLE IF EXISTS {concepts_schema}.admission_type;

CREATE TABLE {concepts_schema}.admission_type AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    ap.electivesurgery AS admission_type_raw,

    CASE
        WHEN ap.electivesurgery = 1 THEN 'elective'
        ELSE 'unknown'
    END AS admission_type,

    'apachepredvar' AS source_table,
    '{raw_schema}'  AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.apachepredvar ap
    ON vw.patientunitstayid = ap.patientunitstayid;

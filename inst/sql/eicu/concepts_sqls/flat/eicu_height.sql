-- eICU patient.admissionheight is documented in cm.
-- Some US sites may have entered values in inches; these cannot be reliably detected.
-- A ~222-row cluster in the 60-79cm range is shaped exactly like inches-entered values;
-- the plausible adult range filter below excludes it.
-- No neonate distinction available in eICU (no DOB exposed).
-- height is a HOSPITAL-VISIT-level fact (patienthealthsystemstayid), not an ICU-UNIT-stay-
-- level one (patientunitstayid). eICU's raw patient table records admissionheight
-- independently per unit stay, and one health system stay can hold several unit stays
-- (27,635 of ~166K eICU health system stays have >1 unit stay, up to 17). All unit stays
-- within one health system stay therefore share the first VALID (120-230cm) reading.
-- "First" is ordered by hospitaladmitoffset (the offset in minutes from each unit's own
-- admission back to the single shared hospital-admission event -- a direct time
-- measurement), not by the externally-assigned unitvisitnumber label. This matches how
-- MIMIC's height/weight_admit are ordered (by actual timestamp, not a sequence label). The
-- two orderings agree on which unit stay is "first" in 27,495 of 27,635 multi-unit
-- health-system-stays (99.5%; dense-rank vs dense-rank, correcting for unitvisitnumber's
-- non-dense gaps); 140 stays (0.5%) disagree.
-- hospitaladmitoffset is <= 0 (unit admission is at or after hospital admission), so ORDER BY
-- ... DESC puts the least-negative (earliest) unit admission first.

DROP TABLE IF EXISTS {concepts_schema}.height;

CREATE TABLE {concepts_schema}.height AS

WITH ht_valid AS (
    SELECT
        pt.patienthealthsystemstayid,
        pt.hospitaladmitoffset,
        pt.admissionheight AS height_cm
    FROM {raw_schema}.patient pt
    WHERE pt.admissionheight BETWEEN 120 AND 230
),
ht_ranked AS (
    SELECT
        patienthealthsystemstayid,
        height_cm,
        ROW_NUMBER() OVER (
            PARTITION BY patienthealthsystemstayid
            ORDER BY hospitaladmitoffset DESC
        ) AS rn
    FROM ht_valid
)

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    hr.height_cm AS height_raw,
    ROUND(hr.height_cm::numeric, 2) AS height,

    'patient' AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN ht_ranked hr
    ON vw.patienthealthsystemstayid = hr.patienthealthsystemstayid
    AND hr.rn = 1;

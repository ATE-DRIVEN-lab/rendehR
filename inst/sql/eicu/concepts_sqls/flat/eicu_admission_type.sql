-- eICU admission urgency, from apachepredvar.electivesurgery.
-- Values: elective, unknown.
--
-- eICU has no direct admission-urgency field: patient.hospitaladmittype
-- (Elective/Emergency/Urgent) does not exist in the eICU-CRD schema (demo or
-- full release).
--
-- admission_type holds only directly observed values, not proxies, so it
-- reads apachepredvar.electivesurgery, a direct, purpose-built
-- elective-surgery indicator: 1 -> elective, anything else -> unknown,
-- including electivesurgery = 0 and no apachepredvar row at all.
-- hospitaladmitsource is not used here: it is an admission-LOCATION field,
-- not an urgency field, and an Emergency-Department-vs-not proxy built from
-- it disagrees with electivesurgery in 14.1% of the 33,256 stays where
-- electivesurgery is populated (hospitaladmitsource is the source column for
-- the separate hospital_admission_location concept).
-- This trades completeness for correctness -- electivesurgery is populated
-- for only ~16.6% of eICU stays (33,256 of 200,859), and is not missing at
-- random: it is populated disproportionately for surgical/APACHE-scored
-- patients, so admission_type coverage for eICU skews surgical and does not
-- represent a random sample.

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

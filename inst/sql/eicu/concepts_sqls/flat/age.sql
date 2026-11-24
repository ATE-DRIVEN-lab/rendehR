-- Age at hospital admission from patient.age.
-- patient.age is recorded at hospital admission (demographic field).
-- Stored as character due to eICU privacy rule: patients >= 90 are censored as "> 89".
-- Censored value mapped to 90. No further cap needed (eICU already enforces 90).
-- Some rows have age = '' (empty string, distinct from NULL and '> 89') -- mapped to NULL
-- rather than left to hit CAST(...AS NUMERIC), which errors on '' in both Postgres and DuckDB.

DROP TABLE IF EXISTS {concepts_schema}.age;

CREATE TABLE {concepts_schema}.age AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.age AS age_raw,

    CASE
        WHEN pt.age IS NULL   THEN NULL
        WHEN pt.age = '> 89'  THEN 90
        WHEN pt.age = ''      THEN NULL
        ELSE ROUND(CAST(pt.age AS NUMERIC), 1)
    END AS age,

    'patient'      AS source_table,
    '{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
    ON vw.patientunitstayid = pt.patientunitstayid;

-- This is based on RICU method

DROP TABLE IF EXISTS {concepts_schema}.admission_service;

CREATE TABLE {concepts_schema}.admission_service AS

WITH base AS (
    SELECT

        vw.uniquepid,
        vw.patienthealthsystemstayid,
        vw.patientunitstayid,

        adx.admitdxpath,
        string_to_array(adx.admitdxpath, '|') AS path
    FROM {raw_schema}.visit_windows vw
    LEFT JOIN {raw_schema}.admissiondx adx
      ON vw.patientunitstayid = adx.patientunitstayid
),

filtered AS (
    SELECT *
    FROM base
    WHERE path[2] = 'All Diagnosis'
),

parsed AS (
    SELECT
        uniquepid,
        patienthealthsystemstayid,
        patientunitstayid,

        path[3] AS admission_service_raw,

        CASE WHEN array_length(path,1) >= 3 THEN path[3] END AS service_group,
        CASE WHEN array_length(path,1) >= 5 THEN path[5] END AS specialty_group

    FROM filtered
)

SELECT
    uniquepid,
    patienthealthsystemstayid,
    patientunitstayid,

    admission_service_raw,

    CASE
        WHEN specialty_group IN ('Genitourinary','Transplant') THEN 'other'
        WHEN service_group = 'Operative' THEN 'surg'
        ELSE 'med'
    END AS admission_service,

    'admissiondx' AS source_table,
    '{raw_schema}' AS source_dataset


FROM parsed;
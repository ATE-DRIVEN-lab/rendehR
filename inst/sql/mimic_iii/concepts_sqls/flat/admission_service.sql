DROP TABLE IF EXISTS {concepts_schema}.admission_service;

CREATE TABLE {concepts_schema}.admission_service AS

WITH categorized AS (
    SELECT
        vw.subject_id,
        vw.hadm_id,
        vw.icustay_id,

        s.curr_service,
        s.transfertime,

        CASE
            WHEN s.curr_service IN ('MED','CMED','NMED','OMED') THEN 'med'
            WHEN s.curr_service IN (
                'SURG','CSURG','VSURG','NSURG',
                'ORTHO','TRAUM','TSURG',
                'PSURG','ENT','DENT'
            ) THEN 'surg'
            WHEN s.curr_service IN ('NB','NBB','GU','GYN','OBS','PSYCH','EYE')
                THEN 'other'
            ELSE NULL
        END AS admission_service

    FROM {raw_schema}.visit_windows vw

    LEFT JOIN {raw_schema}.services s
      ON vw.subject_id = s.subject_id
     AND vw.hadm_id   = s.hadm_id
),

ranked AS (
    SELECT
        *,
        ROW_NUMBER() OVER (
            PARTITION BY subject_id, hadm_id, icustay_id
            ORDER BY transfertime, curr_service
        ) AS rn
    FROM categorized
)

SELECT
   subject_id,
   hadm_id,
icustay_id,

    curr_service AS admission_service_raw,
    admission_service,

    'services'   AS source_table,
    '{raw_schema}'  AS source_dataset


FROM ranked
WHERE rn = 1;
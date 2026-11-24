DROP TABLE IF EXISTS {concepts_schema}.race;

CREATE TABLE {concepts_schema}.race AS

SELECT

    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,


    -- raw value
    pt.ethnicity AS race_raw,

    -- cleaned value
    CASE
        -- NA handling
        WHEN pt.ethnicity IS NULL THEN 'unknown'
        WHEN pt.ethnicity = ''    THEN 'unknown'

        -- white
        WHEN pt.ethnicity = 'Caucasian' THEN 'white'

        -- black
        WHEN pt.ethnicity = 'African American' THEN 'black'

        -- hispanic
        WHEN pt.ethnicity = 'Hispanic' THEN 'hispanic'

        -- asian
        WHEN pt.ethnicity = 'Asian' THEN 'asian'

        -- native
        WHEN pt.ethnicity = 'Native American' THEN 'native'

        -- other
        WHEN pt.ethnicity IN ('Other', 'Other/Unknown') THEN 'other'

        -- fallback
        ELSE 'other'
    END AS race,

'patient' AS source_table,
'{raw_schema}' AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient pt
  ON vw.patientunitstayid = pt.patientunitstayid;

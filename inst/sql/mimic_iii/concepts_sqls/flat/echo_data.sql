-- Adapted from MIMIC-Code (https://github.com/MIT-LCP/mimic-code)
-- Original: concepts/echo-data.sql (BigQuery) — converted to PostgreSQL.
-- Extracts structured fields from free-text echocardiogram notes in noteevents.
--
-- Fields extracted:
--   Height   (inches)  — convert × 2.54 for cm
--   Weight   (lbs)     — convert × 0.453592 for kg
--   BSA      (m²)
--   BPSys    (mmHg)
--   BPDias   (mmHg)
--   HR       (bpm)
--   Indication, Status, Test, Doppler, Contrast, TechnicalQuality (free text)
--
-- Note: charttime is always NULL in the raw noteevents for echo notes.
-- It is reconstructed here from chartdate + time parsed from the note text.
--
-- The MIMIC-III demo dataset does NOT ship noteevents (withheld for PII), so
-- when the table is absent we create an empty echo_data with the same schema
-- rather than failing the whole concept run. The full dataset path is unchanged.

DROP TABLE IF EXISTS {concepts_schema}.echo_data;

DO $do$
BEGIN
IF to_regclass('{raw_schema}.noteevents') IS NULL THEN

    -- noteevents missing (e.g. demo dataset): create empty table with schema
    CREATE TABLE {concepts_schema}.echo_data (
        row_id            integer,
        subject_id        integer,
        hadm_id           integer,
        chartdate         date,
        charttime         timestamp,
        indication        text,
        status            text,
        test              text,
        doppler           text,
        contrast          text,
        technical_quality text,
        height_inches     numeric,
        weight_lbs        numeric,
        bsa               numeric,
        bp                text,
        bp_sys            numeric,
        bp_dias           numeric,
        hr                numeric
    );

ELSE

    EXECUTE $sql$
    CREATE TABLE {concepts_schema}.echo_data AS

    SELECT
        ne.row_id,
        ne.subject_id,
        ne.hadm_id,
        ne.chartdate,

        -- Reconstruct charttime from date and time embedded in note text
        CASE
            WHEN SUBSTRING(ne.text FROM 'Date/Time: [^\n]+ at ([0-9]+:[0-9]{2})') IS NOT NULL
            THEN TO_TIMESTAMP(
                TO_CHAR(ne.chartdate, 'YYYY-MM-DD')
                || SUBSTRING(ne.text FROM 'Date/Time: [^\n]+ at ([0-9]+:[0-9]{2})')
                || ':00',
                'YYYY-MM-DDHH24:MI:SS'
            )
            ELSE NULL
        END AS charttime,

        -- Free-text fields
        SUBSTRING(ne.text FROM 'Indication: ([^\n]+)')        AS indication,
        SUBSTRING(ne.text FROM 'Status: ([^\n]+)')            AS status,
        SUBSTRING(ne.text FROM 'Test: ([^\n]+)')              AS test,
        SUBSTRING(ne.text FROM 'Doppler: ([^\n]+)')           AS doppler,
        SUBSTRING(ne.text FROM 'Contrast: ([^\n]+)')          AS contrast,
        SUBSTRING(ne.text FROM 'Technical Quality: ([^\n]+)') AS technical_quality,

        -- Numeric fields
        -- Height in inches
        NULLIF(SUBSTRING(ne.text FROM 'Height: \(in\) ([0-9]+)'), '')::numeric        AS height_inches,

        -- Weight in lbs
        NULLIF(SUBSTRING(ne.text FROM 'Weight \(lb\): ([0-9]+)'), '')::numeric         AS weight_lbs,

        -- BSA in m² (may include decimals)
        NULLIF(SUBSTRING(ne.text FROM 'BSA \(m2\): ([0-9.]+) m2'), '')::numeric        AS bsa,

        -- Blood pressure as raw string e.g. "120/80"
        SUBSTRING(ne.text FROM 'BP \(mm Hg\): ([^\n]+)')                               AS bp,

        -- Systolic BP
        NULLIF(SUBSTRING(ne.text FROM 'BP \(mm Hg\): ([0-9]+)/[0-9]+'), '')::numeric   AS bp_sys,

        -- Diastolic BP
        NULLIF(SUBSTRING(ne.text FROM 'BP \(mm Hg\): [0-9]+/([0-9]+)'), '')::numeric   AS bp_dias,

        -- Heart rate
        NULLIF(SUBSTRING(ne.text FROM 'HR \(bpm\): ([0-9]+)'), '')::numeric             AS hr

    FROM {raw_schema}.noteevents ne
    WHERE ne.category = 'Echo';
    $sql$;

END IF;
END
$do$;

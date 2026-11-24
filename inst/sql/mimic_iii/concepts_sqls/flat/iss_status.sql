-- Scoring status for the ISS family of concepts, as a flat concept.
--
-- Source: {raw_schema}.iss, built by run_iss() (R/iss.R). This is the scoring
-- provenance flag for the iss / niss / maxais concepts, not a clinical value.
-- Values:
--   'scored'                 - scored normally.
--   'no_mapped_condition'    - diagnoses present, none map to an injury. The
--                              scores are reported as 0, not NULL, so this is
--                              the only way to tell a real 0 from "no mapping".
--   'severity_unknown'       - codes map to injuries, but none has a known
--                              severity (AIS 9 only). The scores are NULL,
--                              not 0: severity is unknown, not absent.
--   'scored_with_exclusions' - scored on the codes whose ICD version could be
--                              resolved; others were excluded rather than
--                              guessed (eICU in practice).
--   'unresolved_diagnosis'   - codes present and well-formed, but NOT ONE had
--                              a resolvable ICD version, so nothing could be
--                              scored. The scores are NULL, not 0.
--   'missing_diagnosis'      - no usable diagnosis data for the admission.
--
-- MIMIC-III is coded entirely in ICD-9, so the ICD-10 scoring never
-- contributes here; every value comes from icdpicr.
--
-- GUARDED, deliberately: run_iss() is scid-conditional by default (see
-- run_one_source()), so {raw_schema}.iss does not exist for most cohort
-- profiles, and run_flat_sql_concepts() stops the whole concept stage on error
-- by default (skip_on_error = FALSE). This file therefore no-ops when the
-- source table is absent instead of failing the pipeline: the concept is
-- simply not built, and build_flat_table()'s auto-discovery finds nothing to
-- join. It appears automatically for any source where run_iss() did run.
--
-- Grain: derived once per hospital admission, so the value repeats across
-- every ICU stay within that admission. The DISTINCT subquery collapses the
-- source table's admission-by-ICU-stay rows back to one row per admission
-- before the join, so an admission with several ICU stays cannot fan out.

DO $$
BEGIN
    IF to_regclass('{raw_schema}.iss') IS NULL THEN
        RAISE NOTICE 'Skipping flat concept iss_status: {raw_schema}.iss does not exist (run_iss() did not run for this source).';
        RETURN;
    END IF;

    DROP TABLE IF EXISTS {concepts_schema}.iss_status;

    CREATE TABLE {concepts_schema}.iss_status AS
    SELECT
        vw.subject_id,
        vw.hadm_id,
        vw.icustay_id,

        i.iss_status,

        'iss'          AS source_table,
        '{raw_schema}' AS source_dataset

    FROM {raw_schema}.visit_windows vw
    LEFT JOIN (
        SELECT DISTINCT hadm_id, iss_status
        FROM {raw_schema}.iss
    ) i ON vw.hadm_id = i.hadm_id;
END $$;

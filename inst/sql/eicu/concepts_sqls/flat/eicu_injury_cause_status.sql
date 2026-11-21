-- Availability of injury mechanism and intent, as a flat concept.
--
-- Source: {raw_schema}.iss, built by run_iss() (R/iss.R). Companion flag for
-- the injury_mechanism and injury_intent concepts: it says why they are NULL,
-- so "nobody coded how the injury happened" is never confused with "we have no
-- data for this admission". Derived here in SQL from the already-published
-- {raw_schema}.iss - run_iss() itself is unchanged.
-- Values:
--   'reported'               - a mechanism and/or intent was derived.
--   'no_external_cause_code' - the admission's diagnoses were scored, but none
--                              of them was an external-cause code, so no
--                              mechanism or intent could be derived. This is a
--                              real finding about the coding, not missing data.
--   'unresolved_diagnosis'   - codes present and well-formed, but none had a
--                              resolvable ICD version, so nothing was scored.
--   'missing_diagnosis'      - no usable diagnosis data for the admission.
--
-- eICU codes carry no version flag, so .eicu_diag_versioned() resolves each
-- code's version (canonical dictionary, then the expert adjudication
-- crosswalk) and excludes the ones that stay ambiguous rather than guessing -
-- reported as 'scored_with_exclusions' in iss_status.
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
        RAISE NOTICE 'Skipping flat concept injury_cause_status: {raw_schema}.iss does not exist (run_iss() did not run for this source).';
        RETURN;
    END IF;

    DROP TABLE IF EXISTS {concepts_schema}.injury_cause_status;

    CREATE TABLE {concepts_schema}.injury_cause_status AS
    SELECT
        vw.uniquepid,
        vw.patienthealthsystemstayid,
        vw.patientunitstayid,

        CASE
            -- No row in the ISS table at all -> genuinely unknown, not a
            -- statement about how the injury was coded.
            WHEN i.patienthealthsystemstayid IS NULL
                THEN NULL
            WHEN i.mechmaj IS NOT NULL OR i.intent IS NOT NULL
                THEN 'reported'
            -- Nothing could be scored, so absence of a mechanism says nothing
            -- about external-cause coding; pass the scoring reason through.
            WHEN i.iss_status IN ('missing_diagnosis', 'unresolved_diagnosis')
                THEN i.iss_status
            -- Scored, but no external-cause code was present.
            ELSE 'no_external_cause_code'
        END AS injury_cause_status,

        'iss'          AS source_table,
        '{raw_schema}' AS source_dataset

    FROM {raw_schema}.visit_windows vw
    LEFT JOIN (
        SELECT DISTINCT patienthealthsystemstayid, mechmaj, intent, iss_status
        FROM {raw_schema}.iss
    ) i ON vw.patienthealthsystemstayid = i.patienthealthsystemstayid;
END $$;

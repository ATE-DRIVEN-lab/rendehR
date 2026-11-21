-- Maximum Abbreviated Injury Scale (AIS) severity across body regions for this hospital admission, as a flat concept.
--
-- Source: {raw_schema}.iss, built by run_iss() (R/iss.R). ICD-9 diagnoses are
-- scored with icdpicr and ICD-10 diagnoses with the isolated icdpicr2 2.1.0
-- adapter. Both scorers run, and the reported value is the higher of the two
-- when the admission has both scorings, or the only available one when it has
-- just one. The per-version columns (maxais_icd9 / maxais_i10) remain in
-- {raw_schema}.iss, so either scoring can be recovered or compared.
--
-- The maximum is deliberate rather than a per-admission choice of coding
-- system. eICU in particular codes the same stay in both systems, so the two
-- scorings are two representations of the same clinical content; taking the
-- maximum keeps an injury that is documented in only one of them.
--
-- Read this together with the iss_status concept: run_iss() reports 0 rather
-- than NULL when an admission has diagnoses but none of them map to an injury,
-- so a 0 here cannot be told apart from "nothing mapped" without iss_status.
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
        RAISE NOTICE 'Skipping flat concept maxais: {raw_schema}.iss does not exist (run_iss() did not run for this source).';
        RETURN;
    END IF;

    DROP TABLE IF EXISTS {concepts_schema}.maxais;

    CREATE TABLE {concepts_schema}.maxais AS
    SELECT
        vw.uniquepid,
        vw.patienthealthsystemstayid,
        vw.patientunitstayid,

        i.maxais,

        'iss'          AS source_table,
        '{raw_schema}' AS source_dataset

    FROM {raw_schema}.visit_windows vw
    LEFT JOIN (
        SELECT DISTINCT patienthealthsystemstayid, maxais
        FROM {raw_schema}.iss
    ) i ON vw.patienthealthsystemstayid = i.patienthealthsystemstayid;
END $$;

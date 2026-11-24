-- Intent of the injury (unintentional, assault, self-inflicted, ...), as a flat concept.
--
-- Source: {raw_schema}.iss, built by run_iss() (R/iss.R). icdpicr / icdpicr2
-- derive mechanism and intent from EXTERNAL-CAUSE codes (E-codes in ICD-9, the
-- V/W/X/Y range in ICD-10) - not from the injury diagnoses themselves. An
-- admission can therefore be fully scored for ISS and still have no mechanism
-- and no intent, because nobody coded how the injury happened.
--
-- Unlike the ISS scores, these are categorical and cannot be maximised across
-- coding systems, so run_iss() takes the ICD-9 value when present and the
-- ICD-10 value otherwise. This differs from iss/niss/maxais on the same row.
--
-- Coverage is genuinely uneven and worth checking before use. In the reference
-- cohorts: MIMIC-III ~49% of admissions, MIMIC-IV ~26%, and eICU ~1.3% - eICU
-- carries very few external-cause codes, so this concept is mostly
-- 'no_external_cause_code' there. It is published for every family anyway, so
-- the absence is visible in the data rather than hidden by a missing table.
--
-- HARMONISED HERE. icdpicr and icdpicr2 use different label vocabularies for
-- the same mechanisms - 19 ICD-9 labels vs 30 ICD-10 labels, only 8 of which
-- match exactly - and run_iss() coalesces them into one column, so the raw
-- value carries two spellings of several categories. This file canonicalises
-- them, following the same _raw/value convention as every other categorical
-- flat concept: the scorer's own label is kept verbatim in the *_raw column
-- and the canonical value is the concept itself, in the package's
-- lower_snake_case convention (as used by race, insurance, admission_service).
-- Punctuation is deliberately dropped: commas, slashes and hyphens in a
-- categorical value break CSV round-trips, are mangled by make.names(), and
-- are awkward as factor levels or grouping keys. Nothing is lost - the
-- scorer's exact label is in the *_raw column. run_iss() and
-- {raw_schema}.iss are unchanged.
--
-- The crosswalk below is derived from the scorers' COMPLETE mapping tables
-- (icdpicr::etab_s1, 1,116 ICD-9 E-codes; icdpicr2::i10_map_mech, 8,117 ICD-10
-- codes) - deliberately NOT from the labels observed in any one cohort, which
-- would miss categories that cohort happens not to contain. test-iss.R
-- pins both vocabularies, so a scorer upgrade that adds a label fails the test
-- instead of silently reintroducing duplicate categories here.
--
-- CAVEAT that harmonisation cannot fix: several categories exist in only one
-- coding system - 'Adverse effects' and 'Poisoning' are ICD-9 only, while
-- 'Hot Object/Substance', 'Bites and Stings, venomous' and the abuse/foreign
-- body categories are ICD-10 only. In a mixed-version cohort their frequency
-- reflects which coding system an admission used, not clinical incidence. This
-- matters: 'Adverse effects' is the single largest mechanism in MIMIC-IV and
-- comes exclusively from ICD-9-coded admissions.
--
-- Read with the injury_cause_status concept, which states WHY this is NULL
-- instead of leaving a missing value to be interpreted.
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
        RAISE NOTICE 'Skipping flat concept injury_intent: {raw_schema}.iss does not exist (run_iss() did not run for this source).';
        RETURN;
    END IF;

    DROP TABLE IF EXISTS {concepts_schema}.injury_intent;

    CREATE TABLE {concepts_schema}.injury_intent AS
    SELECT
        vw.uniquepid,
        vw.patienthealthsystemstayid,
        vw.patientunitstayid,

        i.intent AS injury_intent_raw,

        CASE
            -- The only defect in either intent vocabulary: a missing word
            -- break, which generic normalisation alone would not repair.
            WHEN i.intent = 'Legalintervention/War'
                THEN 'legal_intervention_war'
            ELSE btrim(lower(regexp_replace(i.intent, '[^A-Za-z0-9]+', '_', 'g')), '_')
        END AS injury_intent,

        'iss'          AS source_table,
        '{raw_schema}' AS source_dataset

    FROM {raw_schema}.visit_windows vw
    LEFT JOIN (
        SELECT DISTINCT patienthealthsystemstayid, intent
        FROM {raw_schema}.iss
    ) i ON vw.patienthealthsystemstayid = i.patienthealthsystemstayid;
END $$;

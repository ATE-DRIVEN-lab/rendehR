-- Major mechanism of injury (how the injury happened), as a flat concept.
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
        RAISE NOTICE 'Skipping flat concept injury_mechanism: {raw_schema}.iss does not exist (run_iss() did not run for this source).';
        RETURN;
    END IF;

    DROP TABLE IF EXISTS {concepts_schema}.injury_mechanism;

    CREATE TABLE {concepts_schema}.injury_mechanism AS
    SELECT
        vw.uniquepid,
        vw.patienthealthsystemstayid,
        vw.patientunitstayid,

        i.mechmaj AS injury_mechanism_raw,

        CASE
            -- Same mechanism, two spellings (ICD-9 form | ICD-10 form).
            WHEN i.mechmaj IN ('Cut/pierce', 'Cut/Pierce')
                THEN 'cut_pierce'
            WHEN i.mechmaj IN ('Drowning/submersion', 'Drowning/Submersion')
                THEN 'drowning_submersion'
            WHEN i.mechmaj IN ('Fire/burn', 'Fire/Flame')
                THEN 'fire_burn'
            WHEN i.mechmaj IN ('Natural/environmental',
                               'Natural/Environmental, Other')
                THEN 'natural_environmental'
            WHEN i.mechmaj IN ('Other specified, classifiable',
                               'Other Specified, Classifiable')
                THEN 'other_specified_classifiable'
            WHEN i.mechmaj IN ('Other specified, not elsewhere classifiable',
                               'Other Specified, NEC')
                THEN 'other_specified_nec'
            WHEN i.mechmaj IN ('Struck by, against', 'Struck by/against')
                THEN 'struck_by_against'
            WHEN i.mechmaj IN ('Transport, other', 'Other Transport')
                THEN 'transport_other'

            -- Granularity, not spelling: ICD-9 reports one traffic bucket,
            -- ICD-10 splits it by road-user role. Rolled up to the ICD-9
            -- granularity so traffic injuries are comparable across the two
            -- halves of a cohort; the ICD-10 detail is kept verbatim in
            -- injury_mechanism_raw.
            WHEN i.mechmaj IN ('Motor vehicle traffic', 'MVT-Occupant',
                               'MVT-Motorcyclist', 'MVT-Pedal Cyclist',
                               'MVT-Pedestrian', 'MVT-Other',
                               'MVT-Unspecified')
                THEN 'motor_vehicle_traffic'
            WHEN i.mechmaj = 'Motor Vehicle-Nontraffic'
                THEN 'motor_vehicle_nontraffic'

            -- Categories that exist in only one coding system keep their own
            -- identity - there is nothing to merge them with - but still take
            -- the concept's naming convention. Normalising generically rather
            -- than listing them means a label added by a future scorer version
            -- is named consistently instead of arriving in its own casing.
            ELSE btrim(lower(regexp_replace(i.mechmaj, '[^A-Za-z0-9]+', '_', 'g')), '_')
        END AS injury_mechanism,

        'iss'          AS source_table,
        '{raw_schema}' AS source_dataset

    FROM {raw_schema}.visit_windows vw
    LEFT JOIN (
        SELECT DISTINCT patienthealthsystemstayid, mechmaj
        FROM {raw_schema}.iss
    ) i ON vw.patienthealthsystemstayid = i.patienthealthsystemstayid;
END $$;

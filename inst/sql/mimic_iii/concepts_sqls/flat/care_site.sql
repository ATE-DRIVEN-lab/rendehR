-- MIMIC-III is a single-hospital database (Beth Israel Deaconess Medical Center).
-- No hospital table exists — name is hardcoded as a literal.
-- care_site_raw is NULL (no native hospital ID column).
-- care_region: for comparability with eICU's care_region field (US Census Bureau region,
-- from eICU's native hospital.region column). Beth Israel Deaconess Medical Center is a
-- publicly known hospital in Boston, Massachusetts -- unambiguously in the Northeast Census
-- region -- hardcoded as a literal, same justification as the hospital-name literal above
-- (public, verifiable fact about a named institution, not a derived/guessed value).

DROP TABLE IF EXISTS {concepts_schema}.care_site;

CREATE TABLE {concepts_schema}.care_site AS

SELECT
    vw.subject_id,
    vw.hadm_id,
    vw.icustay_id,

    NULL::integer                              AS care_site_raw,
    'Beth Israel Deaconess Medical Center'    AS care_site,
    'Northeast'                                AS care_region,

    'visit_windows'  AS source_table,
    '{raw_schema}'   AS source_dataset

FROM {raw_schema}.visit_windows vw;

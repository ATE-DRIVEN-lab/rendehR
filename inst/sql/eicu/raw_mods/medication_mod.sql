-- medication_mod: every raw medication column unchanged, plus parsed mod_*
-- columns. One row per raw row; nothing is dropped, cancelled orders
-- (drugordercancelled = 'Yes') included.
-- search_path must be set to the raw schema before executing, and the lookup
-- table rendehr_eicu_hicl_drugname_lookup must be loaded there
-- (run_raw_mods() and run_raw_mods_duckdb() do both).
--
-- mod_drugname: drugname, trimmed. When drugname is blank (36 % of orders in
-- eICU v2.0), the name of the order's drug code (drughiclseqno) from
-- rendehr_eicu_hicl_drugname_lookup, built from the named rows of medication
-- and admissiondrug:
--   - a code whose named rows mostly (more than half) match one rendehR
--     exposure concept gets that concept's most common name;
--   - any other code gets its most common name, whichever concept, if any,
--     that name matches.
-- Codes that carry unrelated drugs get no name. mod_drugname_source says which
-- rows were filled.
--
-- dosage is "<number> <unit>" ('40 mg', '1,000 mL'), "<number> <unit code>"
-- ('25 3'), a range ('5-10 mg') or text ('PYXIS'). Commas are removed first.
-- mod_drugdose, first matching rule:
--   NULL or blank                            -> NULL
--   no leading number ('PYXIS', 'ML')        -> NULL; note = the original dosage
--   range ('5-10 mg')                        -> the low end; note = the original dosage
--   zeros only ('0', '00')                   -> 0
--   leading zero, no dot ('05')              -> NULL; note '<dosage> (ambiguous leading zero)'
--   number ('40', '1.5', '.5')               -> the number
-- mod_drugdose_unit: the text after the number, as written ('mg', 'mEq',
-- 'ml/hr'), or the unit of a numeric unit code:
--   1 ml, 3 mg, 4 g, 5 units, 7 mEq, 8 mcg, 41 ml/hr, 5001 mg, 5002 tab.
-- The codes are not documented by eICU; each is the unit written in words on
-- 95-100 % of the same drug code's other orders (eICU v2.0). Any other code
-- gives a NULL unit, with the original dosage in the note.
--
-- Every ~ pattern is anchored ^...$: DuckDB's ~ matches the whole string,
-- Postgres's any substring, so only anchored patterns agree on both engines.

DROP TABLE IF EXISTS medication_mod;

CREATE TABLE medication_mod AS
WITH base AS (
    SELECT
        m.*,
        nullif(trim(m.drugname), '')                     AS name_txt,
        nullif(trim(replace(m.dosage::text, ',', '')), '') AS dose_txt
    FROM medication m
),

split AS (
    -- dose_num: the leading number (the low end of a range); dose_rest: what
    -- follows it. Both NULL when dosage does not start with a number.
    SELECT
        *,
        CASE
            WHEN dose_txt ~ '^[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:\s*-\s*(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+))?(?:\s*[A-Za-z%].*|\s+.*)?$'
            THEN regexp_replace(dose_txt, '^([+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)).*$', '\1')
        END AS dose_num,
        CASE
            WHEN dose_txt ~ '^[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:\s*-\s*(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+))?(?:\s*[A-Za-z%].*|\s+.*)?$'
            THEN nullif(trim(regexp_replace(dose_txt, '^[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:\s*-\s*(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+))?', '')), '')
        END AS dose_rest,
        dose_txt ~ '^[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)\s*-\s*(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:\s.*|[A-Za-z%].*)?$' AS dose_is_range
    FROM base
),

coded AS (
    SELECT
        *,
        CASE
            WHEN dose_rest IS NULL                  THEN NULL
            WHEN NOT dose_rest ~ '^[0-9]+$'         THEN dose_rest
            WHEN dose_rest = '1'                    THEN 'ml'
            WHEN dose_rest = '3'                    THEN 'mg'
            WHEN dose_rest = '4'                    THEN 'g'
            WHEN dose_rest = '5'                    THEN 'units'
            WHEN dose_rest = '7'                    THEN 'mEq'
            WHEN dose_rest = '8'                    THEN 'mcg'
            WHEN dose_rest = '41'                   THEN 'ml/hr'
            WHEN dose_rest = '5001'                 THEN 'mg'
            WHEN dose_rest = '5002'                 THEN 'tab'
        END AS dose_unit
    FROM split
)

SELECT
    c.medicationid,
    c.patientunitstayid,
    c.drugorderoffset,
    c.drugstartoffset,
    c.drugivadmixture,
    c.drugordercancelled,
    c.drugname,
    c.drughiclseqno,
    c.dosage,
    c.routeadmin,
    c.frequency,
    c.loadingdose,
    c.prn,
    c.drugstopoffset,
    c.gtc,

    c.drugstartoffset AS mod_drugstartoffset,
    c.drugstopoffset  AS mod_drugstopoffset,

    COALESCE(c.name_txt, l.drugname) AS mod_drugname,
    CASE
        WHEN c.name_txt IS NOT NULL  THEN 'drugname'
        WHEN l.drugname IS NOT NULL  THEN 'hicl_lookup'
    END AS mod_drugname_source,

    CASE
        WHEN c.dose_num IS NULL           THEN NULL
        WHEN c.dose_num ~ '^0+$'          THEN 0
        WHEN c.dose_num ~ '^0[0-9]+$'     THEN NULL
        ELSE c.dose_num::double precision
    END AS mod_drugdose,

    c.dose_unit AS mod_drugdose_unit,

    CASE
        WHEN c.dose_txt IS NULL                              THEN NULL
        WHEN c.dose_num IS NULL                              THEN c.dosage
        WHEN c.dose_num ~ '^0[0-9]+$' AND NOT c.dose_num ~ '^0+$'
        THEN c.dosage || ' (ambiguous leading zero)'
        WHEN c.dose_is_range                                 THEN c.dosage
        WHEN c.dose_rest IS NOT NULL AND c.dose_unit IS NULL THEN c.dosage
    END AS mod_drugdose_note,

    'medication' AS source_tab

FROM coded c
LEFT JOIN rendehr_eicu_hicl_drugname_lookup l
       ON l.drughiclseqno = c.drughiclseqno AND c.name_txt IS NULL;

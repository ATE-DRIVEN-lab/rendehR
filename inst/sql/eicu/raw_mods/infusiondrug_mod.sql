-- infusiondrug_mod: every raw infusiondrug column unchanged, plus parsed mod_*
-- columns. One row per raw row; nothing is dropped.
-- search_path must be set to the raw schema before executing.
--
-- drugname ends with the unit of drugrate in brackets, e.g.
-- 'Norepinephrine (mcg/min)'. drugamount is the amount of drug in the bag,
-- not the dose.
--
-- mod_drugrate (drugrate trimmed, commas removed), first matching rule:
--   NULL or blank                          -> NULL
--   zeros only ('0', '00', '000')          -> 0
--   leading zero, no dot ('05', '0900')    -> NULL; note '<drugrate> (ambiguous leading zero)'
--                                             ('05' is 0.5 for amiodarone but 5 for nitroglycerin)
--   number ('8', '2.13', '.5', '00.04')    -> the number
--   number + rate unit only ('40 mg/hr')   -> the number; note = the unit text
--                                             (mod_drugrate_unit stays the bracket unit)
--   anything else ('ERROR', 'OFF',
--     '150mg bolus given by anesthesia')   -> NULL; note = the original drugrate
--
-- Every ~ pattern is anchored ^...$: DuckDB's ~ matches the whole string,
-- Postgres's any substring, so only anchored patterns agree on both engines.

DROP TABLE IF EXISTS infusiondrug_mod;

CREATE TABLE infusiondrug_mod AS
WITH base AS (
    SELECT
        i.*,
        trim(replace(i.drugrate::text, ',', ''))   AS rate_txt,
        trim(replace(i.drugamount::text, ',', '')) AS amount_txt
    FROM infusiondrug i
),

split AS (
    -- A number followed only by a rate unit keeps the number in rate_num and
    -- the unit in rate_unit_txt; any other value is left whole in rate_num.
    SELECT
        *,
        CASE
            WHEN lower(rate_txt) ~ '^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)\s*(mcg|mg|ng|nanograms|units?|ml|cc)(/kg)?/(min|hr|h)$'
            THEN trim(regexp_replace(rate_txt, '\s*[A-Za-z].*$', ''))
            ELSE rate_txt
        END AS rate_num,
        CASE
            WHEN lower(rate_txt) ~ '^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)\s*(mcg|mg|ng|nanograms|units?|ml|cc)(/kg)?/(min|hr|h)$'
            THEN trim(regexp_replace(rate_txt, '^[^A-Za-z]*', ''))
        END AS rate_unit_txt
    FROM base
)

SELECT
    infusiondrugid,
    patientunitstayid,
    infusionoffset,
    drugname,
    drugrate,
    infusionrate,
    drugamount,
    volumeoffluid,
    patientweight,

    infusionoffset AS mod_drugstartoffset,
    infusionoffset AS mod_drugstopoffset,

    trim(regexp_replace(drugname, '\s*\([^()]*\)$', '')) AS mod_drugname,

    CASE
        WHEN rate_txt IS NULL OR rate_txt = ''            THEN NULL
        WHEN rate_num ~ '^0+$'                            THEN 0
        WHEN rate_num ~ '^0[0-9]+$'                       THEN NULL
        WHEN rate_num ~ '^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$'
        THEN rate_num::double precision
        ELSE NULL
    END AS mod_drugrate,

    CASE
        WHEN drugname ~ '^.*\([^()]*\)$'
        THEN NULLIF(trim(regexp_replace(drugname, '^.*\(([^()]*)\)$', '\1')), '')
    END AS mod_drugrate_unit,

    CASE
        WHEN rate_txt IS NULL OR rate_txt = ''            THEN NULL
        WHEN rate_num ~ '^0+$'                            THEN rate_unit_txt
        WHEN rate_num ~ '^0[0-9]+$'                       THEN drugrate || ' (ambiguous leading zero)'
        WHEN rate_num ~ '^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$'
        THEN rate_unit_txt
        ELSE drugrate
    END AS mod_drugrate_note,

    CASE
        WHEN amount_txt ~ '^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$'
        THEN amount_txt::double precision
    END AS mod_drugamount,

    'infusiondrug' AS source_tab

FROM split;

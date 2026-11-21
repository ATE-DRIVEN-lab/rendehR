-- lab_mod: clean labresult to a numeric percentage, for fio2's lab-table source.
-- eicu's lab table splits messy values across two columns: a clean number goes
-- to labresult (numeric), but a value with a '%' suffix (e.g. "40%") goes to
-- labresulttext instead and leaves labresult NULL. FiO2 additionally mixes
-- fraction-of-1 values (e.g. "0.4") with plain percentages (e.g. "40") in
-- labresult itself, the same ambiguity respiratorycharting_mod already
-- resolves for FiO2's other eicu raw table.
-- Not filtered to FiO2 rows here -- filtering happens downstream in the
-- concept config (rgx_itm on labname), matching respiratorycharting_mod's
-- pattern. No other concept reads this table, so transforming every row's
-- value (not just FiO2's) is safe in practice.
-- search_path must be set to the raw schema before executing.

DROP TABLE IF EXISTS lab_mod;

CREATE TABLE lab_mod AS
WITH stripped AS (
    SELECT
        l.patientunitstayid,
        l.labresultoffset,
        l.labname,
        l.labresult AS labresult_raw,
        l.labresulttext,
        TRIM(REPLACE(l.labresulttext, '%', '')) AS labresulttext_stripped
    FROM lab l
),

coalesced AS (
    SELECT
        *,
        COALESCE(
            labresult_raw,
            CASE
                WHEN POSITION('%' IN labresulttext) > 0
                     AND labresulttext_stripped ~ '^-?[0-9]+(\.[0-9]+)?$'
                THEN CAST(labresulttext_stripped AS NUMERIC)
                ELSE NULL
            END
        ) AS labresult_num
    FROM stripped
)

SELECT
    patientunitstayid,
    labresultoffset,
    labname,

    CASE
        WHEN labresult_num IS NULL                        THEN NULL           -- missing / non-numeric
        WHEN labresult_num > 0.2 AND labresult_num <= 1.0  THEN labresult_num * 100  -- fraction -> %
        WHEN labresult_num > 1   AND labresult_num < 20    THEN NULL           -- implausible
        WHEN labresult_num >= 20 AND labresult_num <= 100  THEN labresult_num  -- already %
        ELSE NULL                                                             -- out of range
    END AS labresult

FROM coalesced;

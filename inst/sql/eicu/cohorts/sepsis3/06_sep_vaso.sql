-- SOFA cardiovascular input: vasopressor administration events.
-- eICU doses are unreliable, so this is a presence flag (any vasopressor active
-- at an offset), from continuous infusions (`infusiondrug`) and the ordered
-- `treatment` table (vasopressor / inotropic-agent strings).
CREATE OR REPLACE TABLE sep_vaso AS
SELECT patientunitstayid, infusionoffset AS chartoffset
FROM infusiondrug
WHERE regexp_matches(
        lower(drugname),
        'dopamine|dobu|norepinephrine|levophed|epinephrine|phenylephrine|neosynephrine|neo-synephrine|vasopressin')
GROUP BY patientunitstayid, infusionoffset

UNION

SELECT patientunitstayid, treatmentoffset AS chartoffset
FROM treatment
WHERE treatmentstring LIKE '%vasopressors%'
   OR treatmentstring LIKE '%inotropic agent%'
GROUP BY patientunitstayid, treatmentoffset;

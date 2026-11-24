-- Antibiotic administrations (concept "abx"), using ricu's eICU regexes on the
-- raw `infusiondrug` and `medication` drug names. Offsets are minutes from
-- unit admission. ab_id orders administrations within a stay.
CREATE OR REPLACE TABLE antibiotic AS
WITH a AS (
  SELECT patientunitstayid, drugname AS antibiotic, infusionoffset AS antibiotic_offset,
         infusionoffset AS antibiotic_stop_offset
  FROM infusiondrug
  WHERE regexp_matches(
          drugname,
          'bactrim|cipro|flagyl|metronidazole|zithromax|zosyn|(((amika|cleo|ofloxa)|(azithro|clinda|tobra|vanco)my)c|(ampi|oxa|peni|pipera)cill|cefazol|levaqu|rifamp)in',
          'i')

  UNION ALL

  SELECT patientunitstayid, drugname AS antibiotic, drugstartoffset AS antibiotic_offset,
         GREATEST(drugstartoffset, COALESCE(drugstopoffset, drugstartoffset)) AS antibiotic_stop_offset
  FROM medication
  WHERE regexp_matches(
          drugname,
          'cipro|flagyl|maxipime|metronidazole|tazobactam|zosyn|cef(azolin|epime)|(((azithro|clinda|vanco)my|ofloxa|vanco)c|levaqu|piperacill|roceph)in',
          'i')
)
SELECT
  patientunitstayid,
  antibiotic,
  antibiotic_offset,
  antibiotic_stop_offset,
  ROW_NUMBER() OVER (
    PARTITION BY patientunitstayid
    ORDER BY antibiotic_offset, antibiotic
  ) AS ab_id
FROM a
WHERE antibiotic_offset IS NOT NULL;

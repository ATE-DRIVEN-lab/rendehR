-- admissiondrug_mod: every raw admissiondrug column unchanged, plus mod_*
-- columns named like those of medication_mod and infusiondrug_mod. One row per
-- raw row; nothing is dropped.
-- search_path must be set to the raw schema before executing.
--
-- admissiondrug is the home-medication list written into eICU notes. A drug is
-- listed again in each later note (61 % of rows repeat a stay's drug in eICU
-- v2.0), and drugoffset is when the note was written, so start = stop.
--
-- mod_drugname is drugname trimmed (the raw names are padded to 255
-- characters). mod_drugdose is drugdosage unchanged: 0 with a blank unit (72 %
-- of rows) means no dose was recorded. mod_drugdose_unit is drugunit trimmed,
-- NULL when blank.

DROP TABLE IF EXISTS admissiondrug_mod;

CREATE TABLE admissiondrug_mod AS
SELECT
    a.admissiondrugid,
    a.patientunitstayid,
    a.drugoffset,
    a.drugenteredoffset,
    a.drugnotetype,
    a.specialtytype,
    a.usertype,
    a.rxincluded,
    a.writtenineicu,
    a.drugname,
    a.drugdosage,
    a.drugunit,
    a.drugadmitfrequency,
    a.drughiclseqno,

    a.drugoffset AS mod_drugstartoffset,
    a.drugoffset AS mod_drugstopoffset,

    nullif(trim(a.drugname), '') AS mod_drugname,
    a.drugdosage                 AS mod_drugdose,
    nullif(trim(a.drugunit), '') AS mod_drugdose_unit,

    'admissiondrug' AS source_tab

FROM admissiondrug a;

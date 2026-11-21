-- Hospital site from patient.hospitalid joined to hospital.region.
-- care_site_raw / care_site: numeric hospitalid (eICU native identifier) -- site-level
-- identity, matching the concept's intent and BlendedICU's own precedent (BlendedICU maps
-- care_site directly to hospitalid, not region). No cleaning/transform needed, so raw and
-- harmonized are the same value.
-- care_region: coarse US Census Bureau region string from the hospital table (Midwest/
-- South/West/Northeast), kept as a SEPARATE field alongside care_site -- collapsing to
-- region alone would lose site-level distinguishability (208 distinct hospitals collapse
-- into just 4 regions), so care_site keeps full granularity and care_region provides the
-- coarser geographic view when needed.
--
-- hospital.hospitalname does not exist in the eICU-CRD schema (demo or full release) -
-- eICU de-identifies hospital identity by design, so a name column will never exist.
-- hospitalid is the closest available identifier -- it distinguishes sites even though
-- names are permanently unavailable.

DROP TABLE IF EXISTS {concepts_schema}.care_site;

CREATE TABLE {concepts_schema}.care_site AS

SELECT
    vw.uniquepid,
    vw.patienthealthsystemstayid,
    vw.patientunitstayid,

    pt.hospitalid AS care_site_raw,
    pt.hospitalid AS care_site,
    h.region       AS care_region,

    'patient+hospital' AS source_table,
    '{raw_schema}'     AS source_dataset

FROM {raw_schema}.visit_windows vw
LEFT JOIN {raw_schema}.patient   pt ON vw.patientunitstayid = pt.patientunitstayid
LEFT JOIN {raw_schema}.hospital  h  ON pt.hospitalid        = h.hospitalid;

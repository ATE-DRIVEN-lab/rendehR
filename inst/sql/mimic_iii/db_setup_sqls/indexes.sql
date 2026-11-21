-- ---------------------------------------------------------------------------
-- MIMIC-III raw table indexes  (rendehR pipeline edition)  -- POST-LOAD step
-- ---------------------------------------------------------------------------
-- Adapted from the official mimic-code buildmimic/postgres
-- postgres_add_indexes.sql for the rendehR pipeline.
--
-- This is a post-load script. It remains outside
-- db_setup_sqls/schema/ so run_db() executes it after loading and validation,
-- before atomic promotion. It can also be run idempotently against an already
-- complete raw schema, e.g.:
--   con <- connect_postgres(db_name, creds)
--   DBI::dbExecute(con, "SET search_path TO {src}_coh;")
--   run_sql_file(con, system.file("sql/mimic_iii/db_setup_sqls/indexes.sql", package = "rendehR"))
--
-- Differences from upstream and WHY:
--   * Bare, lower-case names; relies on search_path = {src}_coh.
--   * CREATE INDEX IF NOT EXISTS (idempotent; safe to re-run, no DROP needed).
--   * Only the indexes upstream had enabled (commented-out ones are omitted).
--   * chartevents: upstream indexed only itemid, and only on its 207 inherited
--     partitions. We use a single plain chartevents table, so indexes are
--     created directly on it -- and we add the join keys (subject_id, hadm_id,
--     icustay_id, charttime) the pipeline actually queries.
--   * Cohort tables are small (cohort-filtered), so building these is cheap.
-- ---------------------------------------------------------------------------

-- admissions
CREATE INDEX IF NOT EXISTS admissions_idx01 ON admissions (subject_id);
CREATE INDEX IF NOT EXISTS admissions_idx02 ON admissions (hadm_id);

-- callout
CREATE INDEX IF NOT EXISTS callout_idx01 ON callout (subject_id);
CREATE INDEX IF NOT EXISTS callout_idx02 ON callout (hadm_id);

-- chartevents (single plain table; join keys + itemid)
CREATE INDEX IF NOT EXISTS chartevents_idx01 ON chartevents (subject_id);
CREATE INDEX IF NOT EXISTS chartevents_idx02 ON chartevents (hadm_id);
CREATE INDEX IF NOT EXISTS chartevents_idx03 ON chartevents (icustay_id);
CREATE INDEX IF NOT EXISTS chartevents_idx04 ON chartevents (charttime);
CREATE INDEX IF NOT EXISTS chartevents_idx05 ON chartevents (itemid);

-- cptevents
CREATE INDEX IF NOT EXISTS cptevents_idx01 ON cptevents (subject_id);
CREATE INDEX IF NOT EXISTS cptevents_idx02 ON cptevents (cpt_cd);

-- datetimeevents
CREATE INDEX IF NOT EXISTS datetimeevents_idx01 ON datetimeevents (subject_id);
CREATE INDEX IF NOT EXISTS datetimeevents_idx02 ON datetimeevents (itemid);
CREATE INDEX IF NOT EXISTS datetimeevents_idx03 ON datetimeevents (icustay_id);
CREATE INDEX IF NOT EXISTS datetimeevents_idx04 ON datetimeevents (hadm_id);

-- diagnoses_icd
CREATE INDEX IF NOT EXISTS diagnoses_icd_idx01 ON diagnoses_icd (subject_id);
CREATE INDEX IF NOT EXISTS diagnoses_icd_idx02 ON diagnoses_icd (icd9_code);
CREATE INDEX IF NOT EXISTS diagnoses_icd_idx03 ON diagnoses_icd (hadm_id);

-- drgcodes
CREATE INDEX IF NOT EXISTS drgcodes_idx01 ON drgcodes (subject_id);
CREATE INDEX IF NOT EXISTS drgcodes_idx02 ON drgcodes (drg_code);
CREATE INDEX IF NOT EXISTS drgcodes_idx03 ON drgcodes (description);

-- d_icd_diagnoses
CREATE INDEX IF NOT EXISTS d_icd_diag_idx01 ON d_icd_diagnoses (icd9_code);
CREATE INDEX IF NOT EXISTS d_icd_diag_idx02 ON d_icd_diagnoses (long_title);

-- d_icd_procedures
CREATE INDEX IF NOT EXISTS d_icd_proc_idx01 ON d_icd_procedures (icd9_code);
CREATE INDEX IF NOT EXISTS d_icd_proc_idx02 ON d_icd_procedures (long_title);

-- d_items
CREATE INDEX IF NOT EXISTS d_items_idx01 ON d_items (itemid);
CREATE INDEX IF NOT EXISTS d_items_idx02 ON d_items (label);

-- d_labitems
CREATE INDEX IF NOT EXISTS d_labitems_idx01 ON d_labitems (itemid);
CREATE INDEX IF NOT EXISTS d_labitems_idx02 ON d_labitems (label);
CREATE INDEX IF NOT EXISTS d_labitems_idx03 ON d_labitems (loinc_code);

-- icustays
CREATE INDEX IF NOT EXISTS icustays_idx01 ON icustays (subject_id);
CREATE INDEX IF NOT EXISTS icustays_idx02 ON icustays (icustay_id);
CREATE INDEX IF NOT EXISTS icustays_idx06 ON icustays (hadm_id);

-- inputevents_cv
CREATE INDEX IF NOT EXISTS inputevents_cv_idx01 ON inputevents_cv (subject_id);
CREATE INDEX IF NOT EXISTS inputevents_cv_idx02 ON inputevents_cv (hadm_id);
CREATE INDEX IF NOT EXISTS inputevents_cv_idx03 ON inputevents_cv (icustay_id);
CREATE INDEX IF NOT EXISTS inputevents_cv_idx04 ON inputevents_cv (charttime);
CREATE INDEX IF NOT EXISTS inputevents_cv_idx05 ON inputevents_cv (itemid);

-- inputevents_mv
CREATE INDEX IF NOT EXISTS inputevents_mv_idx01 ON inputevents_mv (subject_id);
CREATE INDEX IF NOT EXISTS inputevents_mv_idx02 ON inputevents_mv (hadm_id);
CREATE INDEX IF NOT EXISTS inputevents_mv_idx03 ON inputevents_mv (icustay_id);
CREATE INDEX IF NOT EXISTS inputevents_mv_idx05 ON inputevents_mv (itemid);

-- labevents
CREATE INDEX IF NOT EXISTS labevents_idx01 ON labevents (subject_id);
CREATE INDEX IF NOT EXISTS labevents_idx02 ON labevents (hadm_id);
CREATE INDEX IF NOT EXISTS labevents_idx03 ON labevents (itemid);

-- microbiologyevents
CREATE INDEX IF NOT EXISTS microbiologyevents_idx01 ON microbiologyevents (subject_id);
CREATE INDEX IF NOT EXISTS microbiologyevents_idx02 ON microbiologyevents (hadm_id);

-- noteevents (absent in mimic_iii_demo; index creation is harmless if table is empty)
CREATE INDEX IF NOT EXISTS noteevents_idx01 ON noteevents (subject_id);
CREATE INDEX IF NOT EXISTS noteevents_idx02 ON noteevents (hadm_id);
CREATE INDEX IF NOT EXISTS noteevents_idx05 ON noteevents (category);

-- outputevents
CREATE INDEX IF NOT EXISTS outputevents_idx01 ON outputevents (subject_id);
CREATE INDEX IF NOT EXISTS outputevents_idx02 ON outputevents (itemid);
CREATE INDEX IF NOT EXISTS outputevents_idx03 ON outputevents (icustay_id);
CREATE INDEX IF NOT EXISTS outputevents_idx04 ON outputevents (hadm_id);

-- prescriptions
CREATE INDEX IF NOT EXISTS prescriptions_idx01 ON prescriptions (subject_id);
CREATE INDEX IF NOT EXISTS prescriptions_idx02 ON prescriptions (icustay_id);
CREATE INDEX IF NOT EXISTS prescriptions_idx03 ON prescriptions (drug_type);
CREATE INDEX IF NOT EXISTS prescriptions_idx04 ON prescriptions (drug);
CREATE INDEX IF NOT EXISTS prescriptions_idx05 ON prescriptions (hadm_id);

-- procedureevents_mv
CREATE INDEX IF NOT EXISTS procedureevents_mv_idx01 ON procedureevents_mv (subject_id);
CREATE INDEX IF NOT EXISTS procedureevents_mv_idx02 ON procedureevents_mv (hadm_id);
CREATE INDEX IF NOT EXISTS procedureevents_mv_idx03 ON procedureevents_mv (icustay_id);
CREATE INDEX IF NOT EXISTS procedureevents_mv_idx05 ON procedureevents_mv (itemid);

-- procedures_icd
CREATE INDEX IF NOT EXISTS procedures_icd_idx01 ON procedures_icd (subject_id);
CREATE INDEX IF NOT EXISTS procedures_icd_idx02 ON procedures_icd (icd9_code);
CREATE INDEX IF NOT EXISTS procedures_icd_idx03 ON procedures_icd (hadm_id);

-- services
CREATE INDEX IF NOT EXISTS services_idx01 ON services (subject_id);
CREATE INDEX IF NOT EXISTS services_idx02 ON services (hadm_id);

-- transfers
CREATE INDEX IF NOT EXISTS transfers_idx01 ON transfers (subject_id);
CREATE INDEX IF NOT EXISTS transfers_idx02 ON transfers (icustay_id);
CREATE INDEX IF NOT EXISTS transfers_idx03 ON transfers (hadm_id);

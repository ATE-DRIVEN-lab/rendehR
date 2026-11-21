-- ---------------------------------------------------------------------------
-- MIMIC-IV raw table indexes  (rendehR pipeline edition)  -- POST-LOAD step
-- ---------------------------------------------------------------------------
-- Adapted from the official mimic-code buildmimic/postgres index.sql.
--
-- Post-load script. It remains outside db_setup_sqls/schema/ so
-- run_db() executes it after loading and validation, before atomic promotion.
-- It can also be run idempotently against an already complete raw schema:
--   con <- connect_postgres(db_name, creds)
--   DBI::dbExecute(con, "SET search_path TO {src}_coh;")
--   run_sql_file(con, system.file("sql/mimic_iv/db_setup_sqls/indexes.sql", package = "rendehR"))
--
-- Differences from upstream and WHY:
--   * Bare, lower-case names; relies on search_path = {src}_coh (the upstream
--     mimiciv_hosp / mimiciv_icu SET search_path lines are removed).
--   * CREATE INDEX IF NOT EXISTS (idempotent; no DROP needed).
--   * Upstream relies on PRIMARY KEYS (from constraint.sql) for the join-key
--     indexes; since the rendehR DDL has no constraints, the join keys
--     (subject_id, hadm_id, stay_id, itemid) and code columns are indexed
--     explicitly here -- these are what the pipeline joins/filters on.
--   * Upstream secondary indexes are kept only when all their columns exist in
--     our canonical MIMIC-IV v3.1 column set; ones that
--     reference newer/removed columns are dropped.
-- ---------------------------------------------------------------------------

-- Join-key and code indexes (rendehR-added; cover what upstream PKs provided)
CREATE INDEX IF NOT EXISTS admissions_subject_id_idx ON admissions (subject_id);
CREATE INDEX IF NOT EXISTS admissions_hadm_id_idx ON admissions (hadm_id);
CREATE INDEX IF NOT EXISTS chartevents_subject_id_idx ON chartevents (subject_id);
CREATE INDEX IF NOT EXISTS chartevents_hadm_id_idx ON chartevents (hadm_id);
CREATE INDEX IF NOT EXISTS chartevents_stay_id_idx ON chartevents (stay_id);
CREATE INDEX IF NOT EXISTS chartevents_itemid_idx ON chartevents (itemid);
CREATE INDEX IF NOT EXISTS d_items_itemid_idx ON d_items (itemid);
CREATE INDEX IF NOT EXISTS d_labitems_itemid_idx ON d_labitems (itemid);
CREATE INDEX IF NOT EXISTS datetimeevents_subject_id_idx ON datetimeevents (subject_id);
CREATE INDEX IF NOT EXISTS datetimeevents_hadm_id_idx ON datetimeevents (hadm_id);
CREATE INDEX IF NOT EXISTS datetimeevents_stay_id_idx ON datetimeevents (stay_id);
CREATE INDEX IF NOT EXISTS datetimeevents_itemid_idx ON datetimeevents (itemid);
CREATE INDEX IF NOT EXISTS diagnoses_icd_subject_id_idx ON diagnoses_icd (subject_id);
CREATE INDEX IF NOT EXISTS diagnoses_icd_hadm_id_idx ON diagnoses_icd (hadm_id);
CREATE INDEX IF NOT EXISTS drgcodes_subject_id_idx ON drgcodes (subject_id);
CREATE INDEX IF NOT EXISTS drgcodes_hadm_id_idx ON drgcodes (hadm_id);
CREATE INDEX IF NOT EXISTS emar_subject_id_idx ON emar (subject_id);
CREATE INDEX IF NOT EXISTS emar_hadm_id_idx ON emar (hadm_id);
CREATE INDEX IF NOT EXISTS emar_detail_subject_id_idx ON emar_detail (subject_id);
CREATE INDEX IF NOT EXISTS hcpcsevents_subject_id_idx ON hcpcsevents (subject_id);
CREATE INDEX IF NOT EXISTS hcpcsevents_hadm_id_idx ON hcpcsevents (hadm_id);
CREATE INDEX IF NOT EXISTS icustays_subject_id_idx ON icustays (subject_id);
CREATE INDEX IF NOT EXISTS icustays_hadm_id_idx ON icustays (hadm_id);
CREATE INDEX IF NOT EXISTS icustays_stay_id_idx ON icustays (stay_id);
CREATE INDEX IF NOT EXISTS ingredientevents_subject_id_idx ON ingredientevents (subject_id);
CREATE INDEX IF NOT EXISTS ingredientevents_hadm_id_idx ON ingredientevents (hadm_id);
CREATE INDEX IF NOT EXISTS ingredientevents_stay_id_idx ON ingredientevents (stay_id);
CREATE INDEX IF NOT EXISTS ingredientevents_itemid_idx ON ingredientevents (itemid);
CREATE INDEX IF NOT EXISTS inputevents_subject_id_idx ON inputevents (subject_id);
CREATE INDEX IF NOT EXISTS inputevents_hadm_id_idx ON inputevents (hadm_id);
CREATE INDEX IF NOT EXISTS inputevents_stay_id_idx ON inputevents (stay_id);
CREATE INDEX IF NOT EXISTS inputevents_itemid_idx ON inputevents (itemid);
CREATE INDEX IF NOT EXISTS labevents_subject_id_idx ON labevents (subject_id);
CREATE INDEX IF NOT EXISTS labevents_hadm_id_idx ON labevents (hadm_id);
CREATE INDEX IF NOT EXISTS labevents_itemid_idx ON labevents (itemid);
CREATE INDEX IF NOT EXISTS microbiologyevents_subject_id_idx ON microbiologyevents (subject_id);
CREATE INDEX IF NOT EXISTS microbiologyevents_hadm_id_idx ON microbiologyevents (hadm_id);
CREATE INDEX IF NOT EXISTS omr_subject_id_idx ON omr (subject_id);
CREATE INDEX IF NOT EXISTS outputevents_subject_id_idx ON outputevents (subject_id);
CREATE INDEX IF NOT EXISTS outputevents_hadm_id_idx ON outputevents (hadm_id);
CREATE INDEX IF NOT EXISTS outputevents_stay_id_idx ON outputevents (stay_id);
CREATE INDEX IF NOT EXISTS outputevents_itemid_idx ON outputevents (itemid);
CREATE INDEX IF NOT EXISTS patients_subject_id_idx ON patients (subject_id);
CREATE INDEX IF NOT EXISTS pharmacy_subject_id_idx ON pharmacy (subject_id);
CREATE INDEX IF NOT EXISTS pharmacy_hadm_id_idx ON pharmacy (hadm_id);
CREATE INDEX IF NOT EXISTS poe_subject_id_idx ON poe (subject_id);
CREATE INDEX IF NOT EXISTS poe_hadm_id_idx ON poe (hadm_id);
CREATE INDEX IF NOT EXISTS poe_detail_subject_id_idx ON poe_detail (subject_id);
CREATE INDEX IF NOT EXISTS prescriptions_subject_id_idx ON prescriptions (subject_id);
CREATE INDEX IF NOT EXISTS prescriptions_hadm_id_idx ON prescriptions (hadm_id);
CREATE INDEX IF NOT EXISTS procedureevents_subject_id_idx ON procedureevents (subject_id);
CREATE INDEX IF NOT EXISTS procedureevents_hadm_id_idx ON procedureevents (hadm_id);
CREATE INDEX IF NOT EXISTS procedureevents_stay_id_idx ON procedureevents (stay_id);
CREATE INDEX IF NOT EXISTS procedureevents_itemid_idx ON procedureevents (itemid);
CREATE INDEX IF NOT EXISTS procedures_icd_subject_id_idx ON procedures_icd (subject_id);
CREATE INDEX IF NOT EXISTS procedures_icd_hadm_id_idx ON procedures_icd (hadm_id);
CREATE INDEX IF NOT EXISTS services_subject_id_idx ON services (subject_id);
CREATE INDEX IF NOT EXISTS services_hadm_id_idx ON services (hadm_id);
CREATE INDEX IF NOT EXISTS transfers_subject_id_idx ON transfers (subject_id);
CREATE INDEX IF NOT EXISTS transfers_hadm_id_idx ON transfers (hadm_id);
CREATE INDEX IF NOT EXISTS d_icd_diagnoses_icd_code_idx ON d_icd_diagnoses (icd_code);
CREATE INDEX IF NOT EXISTS d_icd_procedures_icd_code_idx ON d_icd_procedures (icd_code);
CREATE INDEX IF NOT EXISTS d_hcpcs_code_idx ON d_hcpcs (code);
CREATE INDEX IF NOT EXISTS diagnoses_icd_icd_code_idx ON diagnoses_icd (icd_code);
CREATE INDEX IF NOT EXISTS procedures_icd_icd_code_idx ON procedures_icd (icd_code);

-- Secondary indexes carried over from upstream index.sql
CREATE INDEX IF NOT EXISTS admissions_idx01 ON admissions (admittime, dischtime, deathtime);
CREATE INDEX IF NOT EXISTS d_icd_diag_idx02 ON d_icd_diagnoses (long_title);
CREATE INDEX IF NOT EXISTS d_icd_proc_idx02 ON d_icd_procedures (long_title);
CREATE INDEX IF NOT EXISTS drgcodes_idx01 ON drgcodes (drg_code, drg_type);
CREATE INDEX IF NOT EXISTS drgcodes_idx02 ON drgcodes (description, drg_severity);
CREATE INDEX IF NOT EXISTS d_labitems_idx01 ON d_labitems (label, fluid, category);
CREATE INDEX IF NOT EXISTS emar_detail_idx01 ON emar_detail (pharmacy_id);
CREATE INDEX IF NOT EXISTS emar_detail_idx02 ON emar_detail (product_code);
CREATE INDEX IF NOT EXISTS emar_detail_idx03 ON emar_detail (route, site, side);
CREATE INDEX IF NOT EXISTS emar_det_idx04 ON emar_detail (product_description);
CREATE INDEX IF NOT EXISTS emar_idx01 ON emar (poe_id);
CREATE INDEX IF NOT EXISTS emar_idx02 ON emar (pharmacy_id);
CREATE INDEX IF NOT EXISTS emar_idx03 ON emar (charttime, scheduletime, storetime);
CREATE INDEX IF NOT EXISTS emar_idx04 ON emar (medication);
CREATE INDEX IF NOT EXISTS hcpcsevents_idx04 ON hcpcsevents (short_description);
CREATE INDEX IF NOT EXISTS labevents_idx01 ON labevents (charttime, storetime);
CREATE INDEX IF NOT EXISTS labevents_idx02 ON labevents (specimen_id);
CREATE INDEX IF NOT EXISTS microbiologyevents_idx01 ON microbiologyevents (chartdate, charttime, storedate, storetime);
CREATE INDEX IF NOT EXISTS microbiologyevents_idx02 ON microbiologyevents (spec_itemid, test_itemid, org_itemid, ab_itemid);
CREATE INDEX IF NOT EXISTS microbiologyevents_idx03 ON microbiologyevents (micro_specimen_id);
CREATE INDEX IF NOT EXISTS patients_idx01 ON patients (anchor_age);
CREATE INDEX IF NOT EXISTS patients_idx02 ON patients (anchor_year);
CREATE INDEX IF NOT EXISTS pharmacy_idx01 ON pharmacy (poe_id);
CREATE INDEX IF NOT EXISTS pharmacy_idx02 ON pharmacy (starttime, stoptime);
CREATE INDEX IF NOT EXISTS pharmacy_idx03 ON pharmacy (medication);
CREATE INDEX IF NOT EXISTS pharmacy_idx04 ON pharmacy (route);
CREATE INDEX IF NOT EXISTS poe_idx01 ON poe (order_type);
CREATE INDEX IF NOT EXISTS prescriptions_idx01 ON prescriptions (starttime, stoptime);
CREATE INDEX IF NOT EXISTS transfers_idx01 ON transfers (hadm_id);
CREATE INDEX IF NOT EXISTS transfers_idx02 ON transfers (intime);
CREATE INDEX IF NOT EXISTS transfers_idx03 ON transfers (careunit);
CREATE INDEX IF NOT EXISTS chartevents_idx01 ON chartevents (charttime, storetime);
CREATE INDEX IF NOT EXISTS datetimeevents_idx01 ON datetimeevents (charttime, storetime);
CREATE INDEX IF NOT EXISTS datetimeevents_idx02 ON datetimeevents (value);
CREATE INDEX IF NOT EXISTS d_items_idx01 ON d_items (label, abbreviation);
CREATE INDEX IF NOT EXISTS d_items_idx02 ON d_items (category);
CREATE INDEX IF NOT EXISTS icustays_idx01 ON icustays (first_careunit, last_careunit);
CREATE INDEX IF NOT EXISTS icustays_idx02 ON icustays (intime, outtime);
CREATE INDEX IF NOT EXISTS inputevents_idx01 ON inputevents (starttime, endtime);
CREATE INDEX IF NOT EXISTS inputevents_idx02 ON inputevents (ordercategorydescription);
CREATE INDEX IF NOT EXISTS outputevents_idx01 ON outputevents (charttime, storetime);
CREATE INDEX IF NOT EXISTS procedureevents_idx01 ON procedureevents (starttime, endtime);
CREATE INDEX IF NOT EXISTS procedureevents_idx02 ON procedureevents (ordercategoryname);

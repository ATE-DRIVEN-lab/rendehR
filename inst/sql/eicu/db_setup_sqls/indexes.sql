-- ---------------------------------------------------------------------------
-- eICU-CRD raw table indexes  (rendehR pipeline edition)  -- POST-LOAD step
-- ---------------------------------------------------------------------------
-- Adapted from the official eicu-code postgres_add_indexes.sql.
--
-- Post-load script. It remains outside db_setup_sqls/schema/ so
-- run_db() executes it after loading and validation, before atomic promotion.
-- It can also be run idempotently against an already complete raw schema:
--   con <- connect_postgres(db_name, creds)
--   DBI::dbExecute(con, "SET search_path TO {src}_coh;")
--   run_sql_file(con, system.file("sql/eicu/db_setup_sqls/indexes.sql", package = "rendehR"))
--
-- Differences from upstream and WHY:
--   * Bare, lower-case names; relies on search_path = {src}_coh.
--   * CREATE INDEX IF NOT EXISTS (idempotent; no DROP needed).
--   * Upstream ALTER TABLE ... ADD CONSTRAINT PRIMARY KEY lines are removed
--     (the rendehR DDL is constraint-free); the patientunitstayid join key is
--     indexed explicitly on EVERY table that has it (upstream indexed it on
--     only some), since the pipeline joins all eICU tables on patientunitstayid
--     via visit_windows.
--   * Upstream secondary indexes are kept (columns verified against our config).
-- ---------------------------------------------------------------------------

-- patientunitstayid join-key index on every table that has it (rendehR-added)
CREATE INDEX IF NOT EXISTS admissiondrug_patientunitstayid_idx ON admissiondrug (patientunitstayid);
CREATE INDEX IF NOT EXISTS admissiondx_patientunitstayid_idx ON admissiondx (patientunitstayid);
CREATE INDEX IF NOT EXISTS allergy_patientunitstayid_idx ON allergy (patientunitstayid);
CREATE INDEX IF NOT EXISTS apacheapsvar_patientunitstayid_idx ON apacheapsvar (patientunitstayid);
CREATE INDEX IF NOT EXISTS apachepatientresult_patientunitstayid_idx ON apachepatientresult (patientunitstayid);
CREATE INDEX IF NOT EXISTS apachepredvar_patientunitstayid_idx ON apachepredvar (patientunitstayid);
CREATE INDEX IF NOT EXISTS careplancareprovider_patientunitstayid_idx ON careplancareprovider (patientunitstayid);
CREATE INDEX IF NOT EXISTS careplaneol_patientunitstayid_idx ON careplaneol (patientunitstayid);
CREATE INDEX IF NOT EXISTS careplangeneral_patientunitstayid_idx ON careplangeneral (patientunitstayid);
CREATE INDEX IF NOT EXISTS careplangoal_patientunitstayid_idx ON careplangoal (patientunitstayid);
CREATE INDEX IF NOT EXISTS careplaninfectiousdisease_patientunitstayid_idx ON careplaninfectiousdisease (patientunitstayid);
CREATE INDEX IF NOT EXISTS customlab_patientunitstayid_idx ON customlab (patientunitstayid);
CREATE INDEX IF NOT EXISTS diagnosis_patientunitstayid_idx ON diagnosis (patientunitstayid);
CREATE INDEX IF NOT EXISTS infusiondrug_patientunitstayid_idx ON infusiondrug (patientunitstayid);
CREATE INDEX IF NOT EXISTS intakeoutput_patientunitstayid_idx ON intakeoutput (patientunitstayid);
CREATE INDEX IF NOT EXISTS lab_patientunitstayid_idx ON lab (patientunitstayid);
CREATE INDEX IF NOT EXISTS medication_patientunitstayid_idx ON medication (patientunitstayid);
CREATE INDEX IF NOT EXISTS microlab_patientunitstayid_idx ON microlab (patientunitstayid);
CREATE INDEX IF NOT EXISTS note_patientunitstayid_idx ON note (patientunitstayid);
CREATE INDEX IF NOT EXISTS nurseassessment_patientunitstayid_idx ON nurseassessment (patientunitstayid);
CREATE INDEX IF NOT EXISTS nursecare_patientunitstayid_idx ON nursecare (patientunitstayid);
CREATE INDEX IF NOT EXISTS nursecharting_patientunitstayid_idx ON nursecharting (patientunitstayid);
CREATE INDEX IF NOT EXISTS pasthistory_patientunitstayid_idx ON pasthistory (patientunitstayid);
CREATE INDEX IF NOT EXISTS patient_patientunitstayid_idx ON patient (patientunitstayid);
CREATE INDEX IF NOT EXISTS physicalexam_patientunitstayid_idx ON physicalexam (patientunitstayid);
CREATE INDEX IF NOT EXISTS respiratorycare_patientunitstayid_idx ON respiratorycare (patientunitstayid);
CREATE INDEX IF NOT EXISTS respiratorycharting_patientunitstayid_idx ON respiratorycharting (patientunitstayid);
CREATE INDEX IF NOT EXISTS treatment_patientunitstayid_idx ON treatment (patientunitstayid);
CREATE INDEX IF NOT EXISTS vitalaperiodic_patientunitstayid_idx ON vitalaperiodic (patientunitstayid);
CREATE INDEX IF NOT EXISTS vitalperiodic_patientunitstayid_idx ON vitalperiodic (patientunitstayid);

-- Secondary indexes carried over from upstream
CREATE INDEX IF NOT EXISTS apachepatientresult_idx02 ON apachepatientresult (apacheversion);
CREATE INDEX IF NOT EXISTS apachepatientresult_idx03 ON apachepatientresult (apachescore);
CREATE INDEX IF NOT EXISTS apachepredvar_idx02 ON apachepredvar (sicuday);
CREATE INDEX IF NOT EXISTS patient_idx02 ON patient (patienthealthsystemstayid);
CREATE INDEX IF NOT EXISTS patient_idx03 ON patient (hospitaldischargeyear);
CREATE INDEX IF NOT EXISTS patient_idx05 ON patient (unittype);

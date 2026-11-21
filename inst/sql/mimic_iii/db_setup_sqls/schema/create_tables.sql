-- ---------------------------------------------------------------------------
-- MIMIC-III raw table DDL  (rendehR pipeline edition)
-- ---------------------------------------------------------------------------
-- Adapted from the official mimic-code buildmimic/postgres create-tables
-- script (postgres_create_tables.sql) for the rendehR pipeline.
--
-- Differences from the upstream script and WHY:
--   * Bare, lower-case table/column names. run_db() runs this with
--     `SET search_path TO {src}_coh`, so tables land in the raw cohort schema.
--   * CREATE TABLE IF NOT EXISTS, no DROP TABLE. A resumed / overwrite=FALSE
--     build must not wipe tables that were already loaded; only missing
--     tables are created here.
--   * No PRIMARY KEY / UNIQUE / NOT NULL / CONSTRAINT clauses. The purpose of
--     this file is to give every column an explicit, correct TYPE before the
--     parquet load (so column types are not inferred from the first batch).
--     Integrity constraints would only risk load failures and slow inserts;
--     they belong in a separate post-load step (see indexes.sql).
--   * chartevents is a single plain table. The upstream partitioning /
--     table inheritance / insert trigger is for CSV COPY bulk-loading and
--     conflicts with the pipeline's parquet append.
--   * Character columns keep the upstream VARCHAR(n)/CHAR(n)/TEXT declarations
--     where available, so this schema remains easy to compare with mimic-code.
--     Numeric and timestamp types are preserved exactly.
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS admissions (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  admittime timestamp(0),
  dischtime timestamp(0),
  deathtime timestamp(0),
  admission_type VARCHAR(50),
  admission_location VARCHAR(50),
  discharge_location VARCHAR(50),
  insurance VARCHAR(255),
  language VARCHAR(10),
  religion VARCHAR(50),
  marital_status VARCHAR(50),
  ethnicity VARCHAR(200),
  edregtime timestamp(0),
  edouttime timestamp(0),
  diagnosis VARCHAR(255),
  hospital_expire_flag smallint,
  has_chartevents_data smallint
);

CREATE TABLE IF NOT EXISTS callout (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  submit_wardid integer,
  submit_careunit VARCHAR(15),
  curr_wardid integer,
  curr_careunit VARCHAR(15),
  callout_wardid integer,
  callout_service VARCHAR(10),
  request_tele smallint,
  request_resp smallint,
  request_cdiff smallint,
  request_mrsa smallint,
  request_vre smallint,
  callout_status VARCHAR(20),
  callout_outcome VARCHAR(20),
  discharge_wardid integer,
  acknowledge_status VARCHAR(20),
  createtime timestamp(0),
  updatetime timestamp(0),
  acknowledgetime timestamp(0),
  outcometime timestamp(0),
  firstreservationtime timestamp(0),
  currentreservationtime timestamp(0)
);

CREATE TABLE IF NOT EXISTS caregivers (
  row_id integer,
  cgid integer,
  label VARCHAR(15),
  description VARCHAR(30)
);

CREATE TABLE IF NOT EXISTS chartevents (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  itemid integer,
  charttime timestamp(0),
  storetime timestamp(0),
  cgid integer,
  value VARCHAR(255),
  valuenum numeric,
  valueuom VARCHAR(50),
  warning integer,
  error integer,
  resultstatus VARCHAR(50),
  stopped VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS cptevents (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  costcenter VARCHAR(10),
  chartdate timestamp(0),
  cpt_cd VARCHAR(10),
  cpt_number integer,
  cpt_suffix VARCHAR(5),
  ticket_id_seq integer,
  sectionheader VARCHAR(50),
  subsectionheader VARCHAR(255),
  description VARCHAR(200)
);

CREATE TABLE IF NOT EXISTS datetimeevents (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  itemid integer,
  charttime timestamp(0),
  storetime timestamp(0),
  cgid integer,
  value timestamp(0),
  valueuom VARCHAR(50),
  warning smallint,
  error smallint,
  resultstatus VARCHAR(50),
  stopped VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS diagnoses_icd (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  seq_num integer,
  icd9_code VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS drgcodes (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  drg_type VARCHAR(20),
  drg_code VARCHAR(20),
  description VARCHAR(255),
  drg_severity smallint,
  drg_mortality smallint
);

CREATE TABLE IF NOT EXISTS d_cpt (
  row_id integer,
  category smallint,
  sectionrange VARCHAR(100),
  sectionheader VARCHAR(50),
  subsectionrange VARCHAR(100),
  subsectionheader VARCHAR(255),
  codesuffix VARCHAR(5),
  mincodeinsubsection integer,
  maxcodeinsubsection integer
);

CREATE TABLE IF NOT EXISTS d_icd_diagnoses (
  row_id integer,
  icd9_code VARCHAR(10),
  short_title VARCHAR(50),
  long_title VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS d_icd_procedures (
  row_id integer,
  icd9_code VARCHAR(10),
  short_title VARCHAR(50),
  long_title VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS d_items (
  row_id integer,
  itemid integer,
  label VARCHAR(200),
  abbreviation VARCHAR(100),
  dbsource VARCHAR(20),
  linksto VARCHAR(50),
  category VARCHAR(100),
  unitname VARCHAR(100),
  param_type VARCHAR(30),
  conceptid integer
);

CREATE TABLE IF NOT EXISTS d_labitems (
  row_id integer,
  itemid integer,
  label VARCHAR(100),
  fluid VARCHAR(100),
  category VARCHAR(100),
  loinc_code VARCHAR(100)
);

CREATE TABLE IF NOT EXISTS icustays (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  dbsource VARCHAR(20),
  first_careunit VARCHAR(20),
  last_careunit VARCHAR(20),
  first_wardid smallint,
  last_wardid smallint,
  intime timestamp(0),
  outtime timestamp(0),
  los numeric
);

CREATE TABLE IF NOT EXISTS inputevents_cv (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  charttime timestamp(0),
  itemid integer,
  amount numeric,
  amountuom VARCHAR(30),
  rate numeric,
  rateuom VARCHAR(30),
  storetime timestamp(0),
  cgid integer,
  orderid integer,
  linkorderid integer,
  stopped VARCHAR(30),
  newbottle integer,
  originalamount numeric,
  originalamountuom VARCHAR(30),
  originalroute VARCHAR(30),
  originalrate numeric,
  originalrateuom VARCHAR(30),
  originalsite VARCHAR(30)
);

CREATE TABLE IF NOT EXISTS inputevents_mv (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  starttime timestamp(0),
  endtime timestamp(0),
  itemid integer,
  amount numeric,
  amountuom VARCHAR(30),
  rate numeric,
  rateuom VARCHAR(30),
  storetime timestamp(0),
  cgid integer,
  orderid integer,
  linkorderid integer,
  ordercategoryname VARCHAR(100),
  secondaryordercategoryname VARCHAR(100),
  ordercomponenttypedescription VARCHAR(200),
  ordercategorydescription VARCHAR(50),
  patientweight numeric,
  totalamount numeric,
  totalamountuom VARCHAR(50),
  isopenbag smallint,
  continueinnextdept smallint,
  cancelreason smallint,
  statusdescription VARCHAR(30),
  comments_editedby VARCHAR(30),
  comments_canceledby VARCHAR(40),
  comments_date timestamp(0),
  originalamount numeric,
  originalrate numeric
);

CREATE TABLE IF NOT EXISTS labevents (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  itemid integer,
  charttime timestamp(0),
  value VARCHAR(200),
  valuenum numeric,
  valueuom VARCHAR(20),
  flag VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS microbiologyevents (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  chartdate timestamp(0),
  charttime timestamp(0),
  spec_itemid integer,
  spec_type_desc VARCHAR(100),
  org_itemid integer,
  org_name VARCHAR(100),
  isolate_num smallint,
  ab_itemid integer,
  ab_name VARCHAR(30),
  dilution_text VARCHAR(10),
  dilution_comparison VARCHAR(20),
  dilution_value numeric,
  interpretation VARCHAR(5)
);

CREATE TABLE IF NOT EXISTS noteevents (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  chartdate timestamp(0),
  charttime timestamp(0),
  storetime timestamp(0),
  category VARCHAR(50),
  description VARCHAR(255),
  cgid integer,
  iserror CHAR(1),
  text text
);

CREATE TABLE IF NOT EXISTS outputevents (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  charttime timestamp(0),
  itemid integer,
  value numeric,
  valueuom VARCHAR(30),
  storetime timestamp(0),
  cgid integer,
  stopped VARCHAR(30),
  newbottle CHAR(1),
  iserror integer
);

CREATE TABLE IF NOT EXISTS patients (
  row_id integer,
  subject_id integer,
  gender VARCHAR(5),
  dob timestamp(0),
  dod timestamp(0),
  dod_hosp timestamp(0),
  dod_ssn timestamp(0),
  expire_flag integer
);

CREATE TABLE IF NOT EXISTS prescriptions (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  startdate timestamp(0),
  enddate timestamp(0),
  drug_type VARCHAR(100),
  drug VARCHAR(100),
  drug_name_poe VARCHAR(100),
  drug_name_generic VARCHAR(100),
  formulary_drug_cd VARCHAR(120),
  gsn VARCHAR(200),
  ndc VARCHAR(120),
  prod_strength VARCHAR(120),
  dose_val_rx VARCHAR(120),
  dose_unit_rx VARCHAR(120),
  form_val_disp VARCHAR(120),
  form_unit_disp VARCHAR(120),
  route VARCHAR(120)
);

CREATE TABLE IF NOT EXISTS procedureevents_mv (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  starttime timestamp(0),
  endtime timestamp(0),
  itemid integer,
  value numeric,
  valueuom VARCHAR(30),
  location VARCHAR(30),
  locationcategory VARCHAR(30),
  storetime timestamp(0),
  cgid integer,
  orderid integer,
  linkorderid integer,
  ordercategoryname VARCHAR(100),
  secondaryordercategoryname VARCHAR(100),
  ordercategorydescription VARCHAR(50),
  isopenbag smallint,
  continueinnextdept smallint,
  cancelreason smallint,
  statusdescription VARCHAR(30),
  comments_editedby VARCHAR(30),
  comments_canceledby VARCHAR(30),
  comments_date timestamp(0)
);

CREATE TABLE IF NOT EXISTS procedures_icd (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  seq_num integer,
  icd9_code VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS services (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  transfertime timestamp(0),
  prev_service VARCHAR(20),
  curr_service VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS transfers (
  row_id integer,
  subject_id integer,
  hadm_id integer,
  icustay_id integer,
  dbsource VARCHAR(20),
  eventtype VARCHAR(20),
  prev_careunit VARCHAR(20),
  curr_careunit VARCHAR(20),
  prev_wardid smallint,
  curr_wardid smallint,
  intime timestamp(0),
  outtime timestamp(0),
  los numeric
);

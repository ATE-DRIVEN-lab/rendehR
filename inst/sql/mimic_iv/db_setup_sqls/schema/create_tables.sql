-- ---------------------------------------------------------------------------
-- MIMIC-IV v3.1 raw table DDL  (rendehR pipeline edition)
-- ---------------------------------------------------------------------------
-- Adapted from the official mimic-code buildmimic/postgres create.sql for the
-- rendehR pipeline.
--
-- Differences from upstream and WHY:
--   * Bare, lower-case names in a single schema. run_db() runs this with
--     `SET search_path TO {src}_coh`, so the upstream mimiciv_hosp / mimiciv_icu
--     schema split is collapsed into the one raw cohort schema.
--   * CREATE TABLE IF NOT EXISTS, no DROP TABLE. A resumed / overwrite=FALSE
--     build must not wipe tables that were already loaded.
--   * No NOT NULL / constraints. The job of this file is to give every column an
--     explicit TYPE before the parquet load, not to enforce integrity.
--   * Character columns keep the upstream VARCHAR(n)/CHAR(n)/TEXT declarations
--     where available; numeric/date/timestamp types are kept.
--   * The column set is the canonical rendehr-data-source.json MIMIC-IV v3.1
--     contract.
-- ---------------------------------------------------------------------------


CREATE TABLE IF NOT EXISTS admissions (
  subject_id integer,
  hadm_id integer,
  admittime timestamp,
  dischtime timestamp,
  deathtime timestamp,
  admission_type VARCHAR(40),
  admission_location VARCHAR(60),
  discharge_location VARCHAR(60),
  insurance VARCHAR(255),
  language VARCHAR(25),
  marital_status VARCHAR(30),
  race VARCHAR(80),
  edregtime timestamp,
  edouttime timestamp,
  hospital_expire_flag integer,
  admit_provider_id VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS caregiver (
  caregiver_id integer
);

CREATE TABLE IF NOT EXISTS chartevents (
  subject_id integer,
  hadm_id integer,
  stay_id integer,
  charttime timestamp,
  storetime timestamp,
  itemid integer,
  value VARCHAR(200),
  valuenum numeric,
  valueuom VARCHAR(20),
  warning integer,
  caregiver_id integer
);

CREATE TABLE IF NOT EXISTS d_hcpcs (
  code CHAR(5),
  category integer,
  long_description text,
  short_description VARCHAR(180)
);

CREATE TABLE IF NOT EXISTS d_icd_diagnoses (
  icd_code CHAR(7),
  icd_version integer,
  long_title VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS d_icd_procedures (
  icd_code CHAR(7),
  icd_version integer,
  long_title VARCHAR(222)
);

CREATE TABLE IF NOT EXISTS d_items (
  itemid integer,
  label VARCHAR(100),
  abbreviation VARCHAR(50),
  linksto VARCHAR(30),
  category VARCHAR(50),
  unitname VARCHAR(50),
  param_type VARCHAR(20),
  lownormalvalue numeric,
  highnormalvalue numeric
);

CREATE TABLE IF NOT EXISTS d_labitems (
  itemid integer,
  label VARCHAR(50),
  fluid VARCHAR(50),
  category VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS datetimeevents (
  subject_id integer,
  hadm_id integer,
  stay_id integer,
  charttime timestamp,
  storetime timestamp,
  itemid integer,
  value timestamp,
  valueuom VARCHAR(20),
  warning integer,
  caregiver_id integer
);

CREATE TABLE IF NOT EXISTS diagnoses_icd (
  subject_id integer,
  hadm_id integer,
  seq_num integer,
  icd_code CHAR(7),
  icd_version integer
);

CREATE TABLE IF NOT EXISTS drgcodes (
  subject_id integer,
  hadm_id integer,
  drg_type VARCHAR(4),
  drg_code VARCHAR(10),
  description VARCHAR(195),
  drg_severity integer,
  drg_mortality integer
);

CREATE TABLE IF NOT EXISTS emar (
  subject_id integer,
  hadm_id integer,
  emar_id VARCHAR(25),
  emar_seq integer,
  poe_id VARCHAR(25),
  pharmacy_id integer,
  charttime timestamp,
  medication text,
  event_txt VARCHAR(100),
  scheduletime timestamp,
  storetime timestamp,
  enter_provider_id VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS emar_detail (
  subject_id integer,
  emar_id VARCHAR(25),
  emar_seq integer,
  parent_field_ordinal VARCHAR(10),
  administration_type VARCHAR(50),
  pharmacy_id integer,
  barcode_type VARCHAR(4),
  reason_for_no_barcode text,
  complete_dose_not_given VARCHAR(5),
  dose_due VARCHAR(100),
  dose_due_unit VARCHAR(50),
  dose_given VARCHAR(255),
  dose_given_unit VARCHAR(50),
  will_remainder_of_dose_be_given VARCHAR(5),
  product_amount_given VARCHAR(30),
  product_unit VARCHAR(30),
  product_code VARCHAR(30),
  product_description VARCHAR(255),
  product_description_other VARCHAR(255),
  prior_infusion_rate VARCHAR(40),
  infusion_rate VARCHAR(40),
  infusion_rate_adjustment VARCHAR(50),
  infusion_rate_adjustment_amount VARCHAR(30),
  infusion_rate_unit VARCHAR(30),
  route VARCHAR(10),
  infusion_complete VARCHAR(1),
  completion_interval VARCHAR(50),
  new_iv_bag_hung VARCHAR(1),
  continued_infusion_in_other_location VARCHAR(1),
  restart_interval VARCHAR(2305),
  side VARCHAR(10),
  site VARCHAR(255),
  non_formulary_visual_verification VARCHAR(1)
);

CREATE TABLE IF NOT EXISTS hcpcsevents (
  subject_id integer,
  hadm_id integer,
  chartdate timestamp,
  hcpcs_cd CHAR(5),
  seq_num integer,
  short_description VARCHAR(180)
);

CREATE TABLE IF NOT EXISTS icustays (
  subject_id integer,
  hadm_id integer,
  stay_id integer,
  first_careunit VARCHAR(255),
  last_careunit VARCHAR(255),
  intime timestamp,
  outtime timestamp,
  los numeric
);

CREATE TABLE IF NOT EXISTS ingredientevents (
  subject_id integer,
  hadm_id integer,
  stay_id integer,
  caregiver_id integer,
  starttime timestamp,
  endtime timestamp,
  storetime timestamp,
  itemid integer,
  amount numeric,
  amountuom VARCHAR(20),
  rate numeric,
  rateuom VARCHAR(20),
  orderid integer,
  linkorderid integer,
  statusdescription VARCHAR(20),
  originalamount numeric,
  originalrate numeric
);

CREATE TABLE IF NOT EXISTS inputevents (
  subject_id integer,
  hadm_id integer,
  stay_id integer,
  caregiver_id integer,
  starttime timestamp,
  endtime timestamp,
  storetime timestamp,
  itemid integer,
  amount numeric,
  amountuom VARCHAR(20),
  rate numeric,
  rateuom VARCHAR(20),
  orderid integer,
  linkorderid integer,
  ordercategoryname VARCHAR(50),
  secondaryordercategoryname VARCHAR(50),
  ordercomponenttypedescription VARCHAR(100),
  ordercategorydescription VARCHAR(30),
  patientweight numeric,
  totalamount numeric,
  totalamountuom VARCHAR(50),
  isopenbag integer,
  continueinnextdept integer,
  statusdescription VARCHAR(20),
  originalamount numeric,
  originalrate numeric
);

CREATE TABLE IF NOT EXISTS labevents (
  labevent_id integer,
  subject_id integer,
  hadm_id integer,
  specimen_id integer,
  itemid integer,
  charttime timestamp(0),
  storetime timestamp(0),
  value VARCHAR(200),
  valuenum numeric,
  valueuom VARCHAR(20),
  ref_range_lower numeric,
  ref_range_upper numeric,
  flag VARCHAR(10),
  priority VARCHAR(7),
  comments text,
  order_provider_id VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS microbiologyevents (
  microevent_id integer,
  subject_id integer,
  hadm_id integer,
  micro_specimen_id integer,
  chartdate timestamp(0),
  charttime timestamp(0),
  spec_itemid integer,
  spec_type_desc VARCHAR(100),
  test_seq integer,
  storedate timestamp(0),
  storetime timestamp(0),
  test_itemid integer,
  test_name VARCHAR(100),
  org_itemid integer,
  org_name VARCHAR(100),
  isolate_num integer,
  quantity VARCHAR(50),
  ab_itemid integer,
  ab_name VARCHAR(30),
  dilution_text VARCHAR(10),
  dilution_comparison VARCHAR(20),
  dilution_value numeric,
  interpretation VARCHAR(5),
  comments text,
  order_provider_id VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS omr (
  subject_id integer,
  chartdate timestamp,
  seq_num integer,
  result_name VARCHAR(100),
  result_value text
);

CREATE TABLE IF NOT EXISTS outputevents (
  subject_id integer,
  hadm_id integer,
  stay_id integer,
  charttime timestamp(3),
  storetime timestamp(3),
  itemid integer,
  value numeric,
  valueuom VARCHAR(20),
  caregiver_id integer
);

CREATE TABLE IF NOT EXISTS patients (
  subject_id integer,
  gender CHAR(1),
  anchor_age integer,
  anchor_year integer,
  anchor_year_group VARCHAR(20),
  dod timestamp
);

CREATE TABLE IF NOT EXISTS pharmacy (
  subject_id integer,
  hadm_id integer,
  pharmacy_id integer,
  poe_id VARCHAR(25),
  starttime timestamp(3),
  stoptime timestamp(3),
  medication text,
  proc_type VARCHAR(50),
  status VARCHAR(50),
  entertime timestamp(3),
  verifiedtime timestamp(3),
  route VARCHAR(50),
  frequency VARCHAR(50),
  disp_sched VARCHAR(255),
  infusion_type VARCHAR(15),
  sliding_scale VARCHAR(1),
  lockout_interval VARCHAR(50),
  basal_rate numeric,
  one_hr_max VARCHAR(10),
  doses_per_24_hrs numeric,
  duration numeric,
  duration_interval VARCHAR(50),
  expiration_value integer,
  expiration_unit VARCHAR(50),
  expirationdate timestamp(3),
  dispensation VARCHAR(50),
  fill_quantity VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS poe (
  poe_id VARCHAR(25),
  poe_seq integer,
  subject_id integer,
  hadm_id integer,
  ordertime timestamp(0),
  order_type VARCHAR(25),
  order_subtype VARCHAR(50),
  transaction_type VARCHAR(15),
  discontinue_of_poe_id VARCHAR(25),
  discontinued_by_poe_id VARCHAR(25),
  order_status VARCHAR(15),
  order_provider_id VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS poe_detail (
  poe_id VARCHAR(25),
  poe_seq integer,
  subject_id integer,
  field_name VARCHAR(255),
  field_value text
);

CREATE TABLE IF NOT EXISTS prescriptions (
  subject_id integer,
  hadm_id integer,
  pharmacy_id integer,
  poe_id VARCHAR(25),
  poe_seq integer,
  starttime timestamp(3),
  stoptime timestamp(3),
  drug_type VARCHAR(20),
  drug VARCHAR(255),
  gsn VARCHAR(255),
  ndc VARCHAR(25),
  prod_strength VARCHAR(255),
  form_rx VARCHAR(25),
  dose_val_rx VARCHAR(100),
  dose_unit_rx VARCHAR(50),
  form_val_disp VARCHAR(50),
  form_unit_disp VARCHAR(50),
  doses_per_24_hrs numeric,
  route VARCHAR(50),
  order_provider_id VARCHAR(10),
  formulary_drug_cd VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS procedureevents (
  subject_id integer,
  hadm_id integer,
  stay_id integer,
  starttime timestamp,
  endtime timestamp,
  storetime timestamp,
  itemid integer,
  value numeric,
  valueuom VARCHAR(20),
  location VARCHAR(100),
  locationcategory VARCHAR(50),
  orderid integer,
  linkorderid integer,
  ordercategoryname VARCHAR(50),
  ordercategorydescription VARCHAR(30),
  patientweight numeric,
  isopenbag integer,
  continueinnextdept integer,
  statusdescription VARCHAR(20),
  originalamount numeric,
  originalrate numeric,
  caregiver_id integer
);

CREATE TABLE IF NOT EXISTS procedures_icd (
  subject_id integer,
  hadm_id integer,
  seq_num integer,
  chartdate timestamp,
  icd_code VARCHAR(7),
  icd_version integer
);

CREATE TABLE IF NOT EXISTS provider (
  provider_id VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS services (
  subject_id integer,
  hadm_id integer,
  transfertime timestamp,
  prev_service VARCHAR(10),
  curr_service VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS transfers (
  subject_id integer,
  hadm_id integer,
  transfer_id integer,
  eventtype VARCHAR(10),
  careunit VARCHAR(255),
  intime timestamp,
  outtime timestamp
);

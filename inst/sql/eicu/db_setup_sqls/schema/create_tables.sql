-- ---------------------------------------------------------------------------
-- eICU-CRD raw table DDL  (rendehR pipeline edition)
-- ---------------------------------------------------------------------------
-- Adapted from the official mimic-code eicu-code buildmimic/postgres
-- postgres_create_tables.sql for the rendehR pipeline.
--
-- Differences from upstream and WHY:
--   * Bare, lower-case names. run_db() runs this with
--     `SET search_path TO {src}_coh`, so tables land in the raw cohort schema.
--     (eICU upstream already uses bare table names; only casing is normalized.)
--   * CREATE TABLE IF NOT EXISTS, no DROP TABLE. A resumed / overwrite=FALSE
--     build must not wipe tables that were already loaded.
--   * No NOT NULL / constraints. The job of this file is to give every column an
--     explicit TYPE before the parquet load, not to enforce integrity.
--   * Character columns keep the upstream VARCHAR(n)/TEXT declarations where
--     available. NUMERIC(p,s) is relaxed to unconstrained NUMERIC to avoid
--     load-time precision/scale overflow.
--   * Floating types (double precision / float / real) are stored as NUMERIC so
--     the text values from the parquet load land exactly, instead of inheriting
--     binary-float representation noise (e.g. 36.7 -> 36.699999...). int /
--     bigint / boolean are kept.
--   * eICU has no timestamps (time is expressed as integer *offset columns).
--   * Column set verified against rendehr-data-source.json (eicu,
--     eicu_demo, eicu_scid all share the same 31 tables) -- no version skew.
-- ---------------------------------------------------------------------------


CREATE TABLE IF NOT EXISTS admissiondrug (
  admissiondrugid integer,
  patientunitstayid integer,
  drugoffset integer,
  drugenteredoffset integer,
  drugnotetype VARCHAR(255),
  specialtytype VARCHAR(255),
  usertype VARCHAR(255),
  rxincluded VARCHAR(5),
  writtenineicu VARCHAR(5),
  drugname VARCHAR(255),
  drugdosage numeric,
  drugunit VARCHAR(1000),
  drugadmitfrequency VARCHAR(1000),
  drughiclseqno integer
);

CREATE TABLE IF NOT EXISTS admissiondx (
  admissiondxid integer,
  patientunitstayid integer,
  admitdxenteredoffset integer,
  admitdxpath VARCHAR(500),
  admitdxname VARCHAR(255),
  admitdxtext VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS allergy (
  allergyid integer,
  patientunitstayid integer,
  allergyoffset integer,
  allergyenteredoffset integer,
  allergynotetype VARCHAR(255),
  specialtytype VARCHAR(255),
  usertype VARCHAR(255),
  rxincluded VARCHAR(5),
  writtenineicu VARCHAR(5),
  drugname VARCHAR(255),
  allergytype VARCHAR(255),
  allergyname VARCHAR(255),
  drughiclseqno integer
);

CREATE TABLE IF NOT EXISTS apacheapsvar (
  apacheapsvarid integer,
  patientunitstayid integer,
  intubated smallint,
  vent smallint,
  dialysis smallint,
  eyes smallint,
  motor smallint,
  verbal smallint,
  meds smallint,
  urine numeric,
  wbc numeric,
  temperature numeric,
  respiratoryrate numeric,
  sodium numeric,
  heartrate numeric,
  meanbp numeric,
  ph numeric,
  hematocrit numeric,
  creatinine numeric,
  albumin numeric,
  pao2 numeric,
  pco2 numeric,
  bun numeric,
  glucose numeric,
  bilirubin numeric,
  fio2 numeric
);

CREATE TABLE IF NOT EXISTS apachepatientresult (
  apachepatientresultsid integer,
  patientunitstayid integer,
  physicianspeciality VARCHAR(50),
  physicianinterventioncategory VARCHAR(50),
  acutephysiologyscore integer,
  apachescore integer,
  apacheversion VARCHAR(5),
  predictedicumortality VARCHAR(50),
  actualicumortality VARCHAR(50),
  predictediculos numeric,
  actualiculos numeric,
  predictedhospitalmortality VARCHAR(50),
  actualhospitalmortality VARCHAR(50),
  predictedhospitallos numeric,
  actualhospitallos numeric,
  preopmi integer,
  preopcardiaccath integer,
  ptcawithin24h integer,
  unabridgedunitlos numeric,
  unabridgedhosplos numeric,
  actualventdays numeric,
  predventdays numeric,
  unabridgedactualventdays numeric
);

CREATE TABLE IF NOT EXISTS apachepredvar (
  apachepredvarid integer,
  patientunitstayid integer,
  sicuday smallint,
  saps3day1 smallint,
  saps3today smallint,
  saps3yesterday smallint,
  gender smallint,
  teachtype smallint,
  region smallint,
  bedcount smallint,
  admitsource smallint,
  graftcount smallint,
  meds smallint,
  verbal smallint,
  motor smallint,
  eyes smallint,
  age smallint,
  admitdiagnosis VARCHAR(11),
  thrombolytics smallint,
  diedinhospital smallint,
  aids smallint,
  hepaticfailure smallint,
  lymphoma smallint,
  metastaticcancer smallint,
  leukemia smallint,
  immunosuppression smallint,
  cirrhosis smallint,
  electivesurgery smallint,
  activetx smallint,
  readmit smallint,
  ima smallint,
  midur smallint,
  ventday1 smallint,
  oobventday1 smallint,
  oobintubday1 smallint,
  diabetes smallint,
  managementsystem smallint,
  var03hspxlos numeric,
  pao2 numeric,
  fio2 numeric,
  ejectfx numeric,
  creatinine numeric,
  dischargelocation smallint,
  visitnumber smallint,
  amilocation smallint,
  day1meds smallint,
  day1verbal smallint,
  day1motor smallint,
  day1eyes smallint,
  day1pao2 numeric,
  day1fio2 numeric
);

CREATE TABLE IF NOT EXISTS careplancareprovider (
  cplcareprovderid integer,
  patientunitstayid integer,
  careprovidersaveoffset integer,
  providertype VARCHAR(255),
  specialty VARCHAR(255),
  interventioncategory VARCHAR(255),
  managingphysician VARCHAR(50),
  activeupondischarge VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS careplaneol (
  cpleolid integer,
  patientunitstayid integer,
  cpleolsaveoffset integer,
  cpleoldiscussionoffset integer,
  activeupondischarge VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS careplangeneral (
  cplgeneralid integer,
  patientunitstayid integer,
  activeupondischarge VARCHAR(10),
  cplitemoffset integer,
  cplgroup VARCHAR(255),
  cplitemvalue VARCHAR(1024)
);

CREATE TABLE IF NOT EXISTS careplangoal (
  cplgoalid integer,
  patientunitstayid integer,
  cplgoaloffset integer,
  cplgoalcategory VARCHAR(255),
  cplgoalvalue VARCHAR(1000),
  cplgoalstatus VARCHAR(255),
  activeupondischarge VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS careplaninfectiousdisease (
  cplinfectid integer,
  patientunitstayid integer,
  activeupondischarge VARCHAR(10),
  cplinfectdiseaseoffset integer,
  infectdiseasesite VARCHAR(64),
  infectdiseaseassessment VARCHAR(64),
  responsetotherapy VARCHAR(32),
  treatment VARCHAR(32)
);

CREATE TABLE IF NOT EXISTS customlab (
  customlabid integer,
  patientunitstayid integer,
  labotheroffset integer,
  labothertypeid integer,
  labothername VARCHAR(64),
  labotherresult VARCHAR(64),
  labothervaluetext VARCHAR(128)
);

CREATE TABLE IF NOT EXISTS diagnosis (
  diagnosisid integer,
  patientunitstayid integer,
  activeupondischarge VARCHAR(64),
  diagnosisoffset integer,
  diagnosisstring VARCHAR(200),
  icd9code VARCHAR(100),
  diagnosispriority VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS hospital (
  hospitalid integer,
  numbedscategory VARCHAR(32),
  teachingstatus boolean,
  region VARCHAR(64)
);

CREATE TABLE IF NOT EXISTS infusiondrug (
  infusiondrugid integer,
  patientunitstayid integer,
  infusionoffset integer,
  drugname VARCHAR(255),
  drugrate VARCHAR(255),
  infusionrate VARCHAR(255),
  drugamount VARCHAR(255),
  volumeoffluid VARCHAR(255),
  patientweight VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS intakeoutput (
  intakeoutputid integer,
  patientunitstayid integer,
  intakeoutputoffset integer,
  intaketotal numeric,
  outputtotal numeric,
  dialysistotal numeric,
  nettotal numeric,
  intakeoutputentryoffset integer,
  cellpath VARCHAR(500),
  celllabel VARCHAR(255),
  cellvaluenumeric numeric,
  cellvaluetext VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS lab (
  labid integer,
  patientunitstayid integer,
  labresultoffset integer,
  labtypeid numeric,
  labname VARCHAR(256),
  labresult numeric,
  labresulttext VARCHAR(255),
  labmeasurenamesystem VARCHAR(255),
  labmeasurenameinterface VARCHAR(255),
  labresultrevisedoffset integer
);

CREATE TABLE IF NOT EXISTS medication (
  medicationid integer,
  patientunitstayid integer,
  drugorderoffset integer,
  drugstartoffset integer,
  drugivadmixture VARCHAR(6),
  drugordercancelled VARCHAR(6),
  drugname VARCHAR(220),
  drughiclseqno integer,
  dosage VARCHAR(60),
  routeadmin VARCHAR(120),
  frequency VARCHAR(255),
  loadingdose VARCHAR(120),
  prn VARCHAR(6),
  drugstopoffset integer,
  gtc integer
);

CREATE TABLE IF NOT EXISTS microlab (
  microlabid integer,
  patientunitstayid integer,
  culturetakenoffset integer,
  culturesite VARCHAR(255),
  organism VARCHAR(255),
  antibiotic VARCHAR(255),
  sensitivitylevel VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS note (
  noteid integer,
  patientunitstayid integer,
  noteoffset integer,
  noteenteredoffset integer,
  notetype VARCHAR(50),
  notepath VARCHAR(255),
  notevalue VARCHAR(150),
  notetext VARCHAR(500)
);

CREATE TABLE IF NOT EXISTS nurseassessment (
  nurseassessid integer,
  patientunitstayid integer,
  nurseassessoffset integer,
  nurseassessentryoffset integer,
  cellattributepath VARCHAR(255),
  celllabel VARCHAR(255),
  cellattribute VARCHAR(255),
  cellattributevalue VARCHAR(4000)
);

CREATE TABLE IF NOT EXISTS nursecare (
  nursecareid integer,
  patientunitstayid integer,
  celllabel VARCHAR(255),
  nursecareoffset integer,
  nursecareentryoffset integer,
  cellattributepath VARCHAR(255),
  cellattribute VARCHAR(255),
  cellattributevalue VARCHAR(4000)
);

CREATE TABLE IF NOT EXISTS nursecharting (
  nursingchartid bigint,
  patientunitstayid integer,
  nursingchartoffset integer,
  nursingchartentryoffset integer,
  nursingchartcelltypecat VARCHAR(255),
  nursingchartcelltypevallabel VARCHAR(255),
  nursingchartcelltypevalname VARCHAR(255),
  nursingchartvalue VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS pasthistory (
  pasthistoryid integer,
  patientunitstayid integer,
  pasthistoryoffset integer,
  pasthistoryenteredoffset integer,
  pasthistorynotetype VARCHAR(40),
  pasthistorypath VARCHAR(255),
  pasthistoryvalue VARCHAR(100),
  pasthistoryvaluetext VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS patient (
  patientunitstayid integer,
  patienthealthsystemstayid integer,
  gender VARCHAR(25),
  age VARCHAR(10),
  ethnicity VARCHAR(50),
  hospitalid integer,
  wardid integer,
  apacheadmissiondx VARCHAR(1000),
  admissionheight numeric,
  hospitaladmittime24 VARCHAR(8),
  hospitaladmitoffset integer,
  hospitaladmitsource VARCHAR(30),
  hospitaldischargeyear smallint,
  hospitaldischargetime24 VARCHAR(8),
  hospitaldischargeoffset integer,
  hospitaldischargelocation VARCHAR(100),
  hospitaldischargestatus VARCHAR(10),
  unittype VARCHAR(50),
  unitadmittime24 VARCHAR(8),
  unitadmitsource VARCHAR(100),
  unitvisitnumber integer,
  unitstaytype VARCHAR(15),
  admissionweight numeric,
  dischargeweight numeric,
  unitdischargetime24 VARCHAR(8),
  unitdischargeoffset integer,
  unitdischargelocation VARCHAR(100),
  unitdischargestatus VARCHAR(10),
  uniquepid VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS physicalexam (
  physicalexamid integer,
  patientunitstayid integer,
  physicalexamoffset integer,
  physicalexampath VARCHAR(255),
  physicalexamvalue VARCHAR(100),
  physicalexamtext VARCHAR(500)
);

CREATE TABLE IF NOT EXISTS respiratorycare (
  respcareid integer,
  patientunitstayid integer,
  respcarestatusoffset integer,
  currenthistoryseqnum integer,
  airwaytype VARCHAR(30),
  airwaysize VARCHAR(10),
  airwayposition VARCHAR(32),
  cuffpressure numeric,
  ventstartoffset integer,
  ventendoffset integer,
  priorventstartoffset integer,
  priorventendoffset integer,
  apneaparams VARCHAR(80),
  lowexhmvlimit numeric,
  hiexhmvlimit numeric,
  lowexhtvlimit numeric,
  hipeakpreslimit numeric,
  lowpeakpreslimit numeric,
  hirespratelimit numeric,
  lowrespratelimit numeric,
  sighpreslimit numeric,
  lowironoxlimit numeric,
  highironoxlimit numeric,
  meanairwaypreslimit numeric,
  peeplimit numeric,
  cpaplimit numeric,
  setapneainterval VARCHAR(80),
  setapneatv VARCHAR(80),
  setapneaippeephigh VARCHAR(80),
  setapnearr VARCHAR(80),
  setapneapeakflow VARCHAR(80),
  setapneainsptime VARCHAR(80),
  setapneaie VARCHAR(80),
  setapneafio2 VARCHAR(80)
);

CREATE TABLE IF NOT EXISTS respiratorycharting (
  respchartid integer,
  patientunitstayid integer,
  respchartoffset integer,
  respchartentryoffset integer,
  respcharttypecat VARCHAR(255),
  respchartvaluelabel VARCHAR(255),
  respchartvalue VARCHAR(1000)
);

CREATE TABLE IF NOT EXISTS treatment (
  treatmentid integer,
  patientunitstayid integer,
  treatmentoffset integer,
  treatmentstring VARCHAR(200),
  activeupondischarge VARCHAR(10)
);

CREATE TABLE IF NOT EXISTS vitalaperiodic (
  vitalaperiodicid integer,
  patientunitstayid integer,
  observationoffset integer,
  noninvasivesystolic numeric,
  noninvasivediastolic numeric,
  noninvasivemean numeric,
  paop numeric,
  cardiacoutput numeric,
  cardiacinput numeric,
  svr numeric,
  svri numeric,
  pvr numeric,
  pvri numeric
);

CREATE TABLE IF NOT EXISTS vitalperiodic (
  vitalperiodicid bigint,
  patientunitstayid integer,
  observationoffset integer,
  temperature numeric,
  sao2 integer,
  heartrate integer,
  respiration integer,
  cvp integer,
  etco2 integer,
  systemicsystolic integer,
  systemicdiastolic integer,
  systemicmean integer,
  pasystolic integer,
  padiastolic integer,
  pamean integer,
  st1 numeric,
  st2 numeric,
  st3 numeric,
  icp integer
);

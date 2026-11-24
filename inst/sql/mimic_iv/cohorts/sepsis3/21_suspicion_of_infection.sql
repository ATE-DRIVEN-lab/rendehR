-- THIS SCRIPT IS AUTOMATICALLY GENERATED. DO NOT EDIT IT DIRECTLY.
--
-- Structured as a sequence of separate CREATE TABLE statements (materializing
-- each intermediate step) rather than one large nested-CTE statement. This is
-- not a style choice: DuckDB's query planner needs a large, unpredictable
-- amount of working memory when planning this many CTEs chained together,
-- each referencing others, combined with window functions - the same
-- pathology fixed in mimic_iii's cohorts/sepsis3/02_abx_micro_poe.sql
-- (reproduced there even on the ~100-patient mimic_iii_demo dataset,
-- independent of real data volume). This file's nested-CTE form was checked
-- at that time and passed, but the same query blows up on real full-scale
-- MIMIC-IV: the antibiotic-to-microbiology join here keys on subject_id only
-- (not hadm_id) with the 72h/24h window applied as a residual predicate
-- DuckDB can't push into the join, so a patient with many admissions/orders
-- over their whole record can materialize a candidate block far larger than
-- a typical patient's before the ROW_NUMBER() ranking narrows it back down.
-- Materializing each step sidesteps both problems. Internal tables are
-- prefixed "_" (excluded from flat auto-discovery elsewhere in the pipeline
-- by that same convention) and dropped at the end. No predicate or logic
-- changed from the original nested-CTE form.

DROP TABLE IF EXISTS _suspicion_of_infection_ab_tbl;
DROP TABLE IF EXISTS _suspicion_of_infection_me;
DROP TABLE IF EXISTS _suspicion_of_infection_me_then_ab;
DROP TABLE IF EXISTS _suspicion_of_infection_ab_then_me;

CREATE TABLE _suspicion_of_infection_ab_tbl AS
SELECT
  abx.subject_id,
  abx.hadm_id,
  abx.stay_id,
  abx.antibiotic,
  abx.starttime AS antibiotic_time,
  DATE_TRUNC('DAY', abx.starttime) AS antibiotic_date,
  abx.stoptime AS antibiotic_stoptime,
  ROW_NUMBER() OVER (PARTITION BY subject_id ORDER BY starttime NULLS FIRST, stoptime NULLS FIRST, antibiotic NULLS FIRST) AS ab_id
FROM antibiotic AS abx;

CREATE TABLE _suspicion_of_infection_me AS
SELECT
  micro_specimen_id,
  MAX(subject_id) AS subject_id,
  MAX(hadm_id) AS hadm_id,
  TRY_CAST(MAX(chartdate) AS DATE) AS chartdate,
  MAX(charttime) AS charttime,
  MAX(spec_type_desc) AS spec_type_desc,
  MAX(
    CASE
      WHEN NOT org_name IS NULL AND org_itemid <> 90856 AND org_name <> ''
      THEN 1
      ELSE 0
    END
  ) AS positiveculture
FROM microbiologyevents
GROUP BY
  micro_specimen_id;

CREATE TABLE _suspicion_of_infection_me_then_ab AS
SELECT
  ab_tbl.subject_id,
  ab_tbl.hadm_id,
  ab_tbl.stay_id,
  ab_tbl.ab_id,
  me72.micro_specimen_id,
  COALESCE(me72.charttime, CAST(me72.chartdate AS TIMESTAMP)) AS last72_charttime,
  me72.positiveculture AS last72_positiveculture,
  me72.spec_type_desc AS last72_specimen,
  ROW_NUMBER() OVER (PARTITION BY ab_tbl.subject_id, ab_tbl.ab_id ORDER BY me72.chartdate NULLS FIRST, me72.charttime) AS micro_seq
FROM _suspicion_of_infection_ab_tbl AS ab_tbl
LEFT JOIN _suspicion_of_infection_me AS me72
  ON ab_tbl.subject_id = me72.subject_id
  AND (
    (
      NOT me72.charttime IS NULL
      AND ab_tbl.antibiotic_time > me72.charttime
      AND ab_tbl.antibiotic_time <= me72.charttime + INTERVAL '72' HOUR
    )
    OR (
      me72.charttime IS NULL
      AND antibiotic_date >= me72.chartdate
      AND antibiotic_date <= me72.chartdate + INTERVAL 3 DAY
    )
  );

CREATE TABLE _suspicion_of_infection_ab_then_me AS
SELECT
  ab_tbl.subject_id,
  ab_tbl.hadm_id,
  ab_tbl.stay_id,
  ab_tbl.ab_id,
  me24.micro_specimen_id,
  COALESCE(me24.charttime, CAST(me24.chartdate AS TIMESTAMP)) AS next24_charttime,
  me24.positiveculture AS next24_positiveculture,
  me24.spec_type_desc AS next24_specimen,
  ROW_NUMBER() OVER (PARTITION BY ab_tbl.subject_id, ab_tbl.ab_id ORDER BY me24.chartdate NULLS FIRST, me24.charttime) AS micro_seq
FROM _suspicion_of_infection_ab_tbl AS ab_tbl
LEFT JOIN _suspicion_of_infection_me AS me24
  ON ab_tbl.subject_id = me24.subject_id
  AND (
    (
      NOT me24.charttime IS NULL
      AND ab_tbl.antibiotic_time >= me24.charttime - INTERVAL '24' HOUR
      AND ab_tbl.antibiotic_time < me24.charttime
    )
    OR (
      me24.charttime IS NULL
      AND ab_tbl.antibiotic_date >= me24.chartdate - INTERVAL 1 DAY
      AND ab_tbl.antibiotic_date <= me24.chartdate
    )
  );

CREATE OR REPLACE TABLE suspicion_of_infection AS
SELECT
  ab_tbl.subject_id,
  ab_tbl.stay_id,
  ab_tbl.hadm_id,
  ab_tbl.ab_id,
  ab_tbl.antibiotic,
  ab_tbl.antibiotic_time,
  CASE WHEN last72_specimen IS NULL AND next24_specimen IS NULL THEN 0 ELSE 1 END AS suspected_infection,
  CASE
    WHEN last72_specimen IS NULL AND next24_specimen IS NULL
    THEN NULL
    ELSE COALESCE(last72_charttime, antibiotic_time)
  END AS suspected_infection_time,
  COALESCE(last72_charttime, next24_charttime) AS culture_time,
  COALESCE(last72_specimen, next24_specimen) AS specimen,
  COALESCE(last72_positiveculture, next24_positiveculture) AS positive_culture
FROM _suspicion_of_infection_ab_tbl AS ab_tbl
LEFT JOIN _suspicion_of_infection_ab_then_me AS ab2me
  ON ab_tbl.subject_id = ab2me.subject_id
  AND ab_tbl.ab_id = ab2me.ab_id
  AND ab2me.micro_seq = 1
LEFT JOIN _suspicion_of_infection_me_then_ab AS me2ab
  ON ab_tbl.subject_id = me2ab.subject_id
  AND ab_tbl.ab_id = me2ab.ab_id
  AND me2ab.micro_seq = 1;

DROP TABLE _suspicion_of_infection_ab_tbl;
DROP TABLE _suspicion_of_infection_me;
DROP TABLE _suspicion_of_infection_me_then_ab;
DROP TABLE _suspicion_of_infection_ab_then_me;

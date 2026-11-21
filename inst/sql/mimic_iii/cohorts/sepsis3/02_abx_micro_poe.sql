-- defines suspicion of infection using prescriptions + microbiologyevents
--
-- Structured as a sequence of separate CREATE TABLE statements (materializing
-- each intermediate step) rather than one large nested-CTE statement. This
-- is not a style choice: DuckDB's query planner needs a large, unpredictable
-- amount of working memory (repeatedly observed requesting a fixed ~1.4 GiB
-- allocation, at different, seemingly arbitrary points within the statement
-- across otherwise-identical runs, regardless of how much actual data is
-- involved - reproduced even on the ~100-patient mimic_iii_demo dataset)
-- when planning this many CTEs chained together, each referencing others,
-- combined with window functions. Materializing each step as its own table
-- sidesteps that entirely - confirmed instant and negligible-memory on the
-- exact same data that OOM'd as nested CTEs, with an identical result.
-- Internal tables are prefixed "_" (excluded from flat auto-discovery
-- elsewhere in the pipeline by that same convention) and dropped at the end.

DROP TABLE IF EXISTS _abx_micro_poe_ab_tbl;
DROP TABLE IF EXISTS _abx_micro_poe_me;
DROP TABLE IF EXISTS _abx_micro_poe_me_then_ab;
DROP TABLE IF EXISTS _abx_micro_poe_ab_then_me;
DROP TABLE IF EXISTS _abx_micro_poe_ab_fnl;

-- get cultures for each icustay
-- note this duplicates prescriptions
-- each ICU stay in the same hospitalization will get a copy of all prescriptions for that hospitalization
CREATE TABLE _abx_micro_poe_ab_tbl AS
with abx as
(
  select pr.hadm_id
  , pr.drug as antibiotic_name
  , pr.startdate as antibiotic_time
  , pr.enddate as antibiotic_endtime
  from prescriptions pr
  -- inner join to subselect to only antibiotic prescriptions
  inner join abx_poe_list ab
      on pr.drug = ab.drug
)
select
      ie.subject_id, ie.hadm_id, ie.icustay_id
    , ie.intime, ie.outtime
    , abx.antibiotic_name
    , abx.antibiotic_time
    , abx.antibiotic_endtime
    -- identifies each (icustay, antibiotic event) pairing; lets the
    -- before/after culture match be narrowed to one best candidate per
    -- side (see _abx_micro_poe_me_then_ab/_abx_micro_poe_ab_then_me below)
    -- before the two sides are combined.
    , ROW_NUMBER() OVER (PARTITION BY ie.icustay_id ORDER BY abx.antibiotic_time NULLS FIRST, abx.antibiotic_name NULLS FIRST) as ab_id
from icustays ie
left join abx
    on ie.hadm_id = abx.hadm_id;

CREATE TABLE _abx_micro_poe_me AS
select hadm_id
  , chartdate, charttime
  , spec_type_desc
  , max(case when org_name is not null and org_name != '' then 1 else 0 end) as PositiveCulture
from microbiologyevents
group by hadm_id, chartdate, charttime, spec_type_desc;

-- best (earliest) matching culture in the 72 hours BEFORE the antibiotic, if
-- any - narrowed to one row per (icustay_id, ab_id) via micro_seq before
-- joining back, instead of keeping every candidate match.
CREATE TABLE _abx_micro_poe_me_then_ab AS
select
    t.icustay_id, t.ab_id
  , coalesce(me72.charttime,me72.chartdate) as last72_charttime
  , me72.positiveculture as last72_positiveculture
  , me72.spec_type_desc as last72_specimen
  , ROW_NUMBER() OVER (PARTITION BY t.icustay_id, t.ab_id ORDER BY me72.chartdate NULLS FIRST, me72.charttime) as micro_seq
from _abx_micro_poe_ab_tbl t
-- blood culture in last 72 hours
left join _abx_micro_poe_me me72
  on t.hadm_id = me72.hadm_id
  and t.antibiotic_time is not null
  and
  (
    -- if charttime is available, use it
    (
        t.antibiotic_time > me72.charttime
    and t.antibiotic_time <= me72.charttime + INTERVAL 72 HOUR
    )
    OR
    (
    -- if charttime is not available, use chartdate
        me72.charttime is null
    and t.antibiotic_time > me72.chartdate
    and t.antibiotic_time < me72.chartdate + INTERVAL 96 HOUR -- could equally do this with a date_trunc, but that's less portable
    )
  );

-- best (earliest) matching culture in the 24 hours AFTER the antibiotic, if
-- any - same narrowing as above.
CREATE TABLE _abx_micro_poe_ab_then_me AS
select
    t.icustay_id, t.ab_id
  , coalesce(me24.charttime,me24.chartdate) as next24_charttime
  , me24.positiveculture as next24_positiveculture
  , me24.spec_type_desc as next24_specimen
  , ROW_NUMBER() OVER (PARTITION BY t.icustay_id, t.ab_id ORDER BY me24.chartdate NULLS FIRST, me24.charttime) as micro_seq
from _abx_micro_poe_ab_tbl t
-- blood culture in subsequent 24 hours
left join _abx_micro_poe_me me24
  on t.hadm_id = me24.hadm_id
  and t.antibiotic_time is not null
  and
  (
    -- if charttime is available, use it
    (
        me24.charttime is not null
    and t.antibiotic_time > me24.charttime - INTERVAL 24 HOUR
    and t.antibiotic_time <= me24.charttime
    )
    OR
    (
    -- if charttime is not available, use chartdate
        me24.charttime is null
    and t.antibiotic_time > me24.chartdate
    and t.antibiotic_time <= me24.chartdate + INTERVAL 24 HOUR
    )
  );

CREATE TABLE _abx_micro_poe_ab_fnl AS
select
    t.icustay_id, t.intime, t.outtime
  , t.antibiotic_name
  , t.antibiotic_time
  , me2ab.last72_charttime
  , ab2me.next24_charttime

  , me2ab.last72_positiveculture
  , me2ab.last72_specimen
  , ab2me.next24_positiveculture
  , ab2me.next24_specimen
from _abx_micro_poe_ab_tbl t
left join _abx_micro_poe_me_then_ab me2ab
  on t.icustay_id = me2ab.icustay_id
 and t.ab_id = me2ab.ab_id
 and me2ab.micro_seq = 1
left join _abx_micro_poe_ab_then_me ab2me
  on t.icustay_id = ab2me.icustay_id
 and t.ab_id = ab2me.ab_id
 and ab2me.micro_seq = 1;

CREATE OR REPLACE TABLE abx_micro_poe AS
with ab_laststg as
(
select
  icustay_id
  , antibiotic_name
  , antibiotic_time
  , last72_charttime
  , next24_charttime

  -- time of suspected infection: either the culture time (if before antibiotic), or the antibiotic time
  , case
      when coalesce(last72_charttime,antibiotic_time) is null
        then 0
      else 1 end as suspected_infection

  , coalesce(last72_charttime,antibiotic_time) as suspected_infection_time

  -- the specimen that was cultured
  , case
      when last72_charttime is not null
        then last72_specimen
      when next24_charttime is not null
        then next24_specimen
    else null
  end as specimen

  -- whether the cultured specimen ended up being positive or not
  , case
      when last72_charttime is not null
        then last72_positiveculture
      when next24_charttime is not null
        then next24_positiveculture
    else null
  end as positiveculture
from _abx_micro_poe_ab_fnl
)
select
  icustay_id
  , antibiotic_name
  , antibiotic_time
  , last72_charttime
  , next24_charttime
  , suspected_infection_time
  -- -- the below two fields are used to extract data - modifying them facilitates sensitivity analyses
  -- , suspected_infection_time - INTERVAL 48 HOUR as si_starttime
  -- , suspected_infection_time + INTERVAL 24 HOUR as si_endtime
  , specimen, positiveculture
from ab_laststg
order by icustay_id, antibiotic_time;

DROP TABLE _abx_micro_poe_ab_tbl;
DROP TABLE _abx_micro_poe_me;
DROP TABLE _abx_micro_poe_me_then_ab;
DROP TABLE _abx_micro_poe_ab_then_me;
DROP TABLE _abx_micro_poe_ab_fnl;

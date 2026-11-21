-- Execution-order manifest for concepts_sqls/ehr/{eicu}. Unlike mimic_iii/
-- mimic_iv, the upstream eicu-code repo never shipped a manifest for these
-- files, so this one is hand-authored here (not copied from a vendor file).
--
-- Dependency graph (confirmed by grepping every file's FROM/JOIN targets
-- against every other file's output table - see RENDEHR_QA_PLAN.md):
-- pivoted-oasis.sql is the only file with cross-file dependencies (needs
-- pivoted_gcs, pivoted_uo, pivoted_vital); every other file is independent.
-- Without this manifest, execution fell back to alphabetical/radix sort,
-- which runs "oasis" before "vital" - a real, reproduced failure
-- (ERROR: relation "pivoted_vital" does not exist) against real eicu data.
\i basic_demographics.sql
\i diagnosis/apache-groups.sql
\i icustay_detail.sql
\i labsfirstday.sql
\i pivoted/pivoted-bg.sql
\i pivoted/pivoted-gcs.sql
\i pivoted/pivoted-infusion.sql
\i pivoted/pivoted-lab.sql
\i pivoted/pivoted-med.sql
\i pivoted/pivoted-o2.sql
\i pivoted/pivoted-score.sql
\i pivoted/pivoted-treatment-vasopressor.sql
\i pivoted/pivoted-uo.sql
\i pivoted/pivoted-vital-other.sql
\i pivoted/pivoted-vital.sql
\i pivoted/pivoted-weight.sql
\i pivoted/pivoted-oasis.sql

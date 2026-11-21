-- Adapted by rendehR::register_concept_sql()
-- src: mimic_iv_demo  concept: hr_test  schema: mimic_iv_demo_coh_concepts
SET search_path TO mimic_iv_demo_coh;

CREATE TABLE "mimic_iv_demo_coh_concepts"."hr_test" AS
SELECT subject_id FROM chartevents WHERE itemid = 1;


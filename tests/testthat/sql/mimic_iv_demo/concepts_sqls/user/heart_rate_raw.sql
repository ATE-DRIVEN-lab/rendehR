-- Adapted by rendehR::register_concept_sql()
-- src: mimic_iv_demo  concept: heart_rate_raw  schema: mimic_iv_demo_coh_concepts
SET search_path TO mimic_iv_demo_coh;

CREATE TABLE "mimic_iv_demo_coh_concepts"."heart_rate_raw" AS
SELECT subject_id, valuenum AS value FROM chartevents WHERE itemid = 220045;


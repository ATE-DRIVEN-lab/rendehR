-- Adapted by rendehR::register_concept_sql()
-- src: mimic_iv_demo  concept: test_concept  schema: mimic_iv_demo_coh_concepts
SET search_path TO mimic_iv_demo_coh;

CREATE TABLE "mimic_iv_demo_coh_concepts"."test_concept" AS
SELECT subject_id FROM admissions;


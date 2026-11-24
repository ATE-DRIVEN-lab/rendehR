-- Adapted by rendehR::register_concept_sql()
-- src: mimic_iv_demo  concept: will_fail  schema: mimic_iv_demo_coh_concepts
SET search_path TO mimic_iv_demo_coh;

CREATE TABLE "mimic_iv_demo_coh_concepts"."will_fail" AS
SELECT * FROM table_that_does_not_exist;


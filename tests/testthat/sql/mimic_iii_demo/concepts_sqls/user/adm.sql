-- Adapted by rendehR::register_concept_sql()
-- src: mimic_iii_demo  concept: adm  schema: mimic_iii_demo_coh_concepts
SET search_path TO mimic_iii_demo_coh;

CREATE TABLE "mimic_iii_demo_coh_concepts"."adm" AS
SELECT subject_id FROM admissions;


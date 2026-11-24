---
name: rendehr-add-concept
description: Help users add or adapt a clinical concept in an existing rendehR source through supplemental configuration or custom SQL, with provenance and validation. Use when extending variable definitions, not merely retrieving an existing concept or creating a cohort.
---

# Add a rendehR concept

Draft an extension using supported public interfaces without editing installed package files. Confirm whether the user wants a definition, code/files, or execution. Preserve the user's scientific choices and identify unresolved meaning, unit, timing, and aggregation decisions before materializing a concept.

## Workflow

1. Inspect existing concepts and source metadata so an available concept is not needlessly recreated.
2. Read [authoring](references/authoring.md) to choose supplemental CSV versus SQL, inspect the appropriate contract, and prepare an explicit draft.
3. Read [validation and integration](references/validation-and-integration.md) before execution, provenance writes, or claims that harmonized output is ready.
4. Return definition and source evidence, draft artifacts, selected public calls, validation status, and the necessary downstream work. Code-only requests stop at code.

## Boundaries

- User files belong in the project/external configuration locations, not the package library or original source repository.
- SQL preview without saving/execution requires dry_run=TRUE, save_to=FALSE, and no con. A con forces execution in register_concept_sql; its default save_to writes even during dry run.
- run_concepts dry runs can connect, initialize schemas, and write SQL. Do not invoke them as an unconditional read-only inspection.
- Name matches, citations, and similar distributions do not establish clinical correctness. Do not invent item IDs, conversions, code lists, or source equivalence.
- ehr_find and draft_config_row are MCP tools, not functions exported by rendehR.
- New source families belong to source-registration work. Cohort selection and ordinary retrieval belong to package-user work; this skill remains usable without other skills installed.

## Compatibility

Reconciled with rendehR 1.0.0 (November 2026): the functions and arguments it names exist. The MCP `draft_config_row` tool is covered by the package tests; the authoring steps themselves have not been run live. Confirm installed-release signatures, configuration headers, and supported layers before use. Report generated versus executed versus validated distinctly.

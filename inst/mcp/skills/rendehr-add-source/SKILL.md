---
name: rendehr-add-source
description: Help users register a new EHR dataset family in rendehR (an ICU database other than MIMIC-III, MIMIC-IV and eICU) through configuration and source-specific SQL, then validate it before running the pipeline. Use when adding a dataset family, not for a new cohort or a new variable in an existing source.
---

# Add a rendehR source family

A source family is a dataset with its own tables, identifiers and clock (for example AmsterdamUMCdb or HiRID). A new cohort in MIMIC or eICU is not a new family: that belongs to `rendehr-user`. A new variable in an existing family belongs to `rendehr-add-concept`.

Confirm whether the user wants a plan, files, or a registered source. Never edit the installed package; everything lives under the user's configuration root (`root`, or `EHR_EXTERNAL_CONFIG_ROOT`).

## Workflow

1. Establish the dataset's identifier hierarchy (patient, admission, optional ICU stay), the column used as time zero, whether times are timestamps or minute offsets, and the table and column that select a cohort. Ask; do not guess from another dataset.
2. Read [registry and scaffold](references/registry-and-scaffold.md): write the registry entry and SQL skeleton with `add_source_wizard()` or `ehr_add_source_assistant()`.
3. Read [SQL and validation](references/sql-and-validation.md): fill in the DDL and window SQL, add concept-config rows, validate, and run a bounded demo before claiming the source works.
4. Report what was written, what registered, what validated, what ran, and what remains (SQL still skeletal, concepts not mapped, stages skipped).

## Boundaries

- `add_source_wizard()` writes files under `root` (never overwrites without `overwrite = TRUE`) and registers the source for the session only when the entry is complete. `ehr_add_source_assistant(action = "script")` writes nothing; `action = "dry_run"` validates the entry the wizard would write, without writing.
- Sources under `EHR_EXTERNAL_CONFIG_ROOT/sources/*.json` register automatically when the package loads. Registering does not load data.
- The comorbidity and injury-severity stages are written for the built-in families: run a new family with `run_comorbidities = FALSE, run_iss = FALSE`.
- Concept rows for the new family follow `rendehr-add-concept`; do not copy MIMIC item ids or eICU labels to another dataset.
- Credentialed data stays local; do not paste patient rows into the conversation.

## Compatibility

Checked against rendehR 1.0.0 (November 2026): the wizard, the assistant's script and dry-run modes, registration and `validate_source_config()` were run on a scratch configuration root. A complete end-to-end build of a non-built-in dataset has not been run; say so when it matters.

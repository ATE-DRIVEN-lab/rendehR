---
name: rendehr-user
description: Help researchers operate rendehR using existing sources and concepts, including setup, custom cohort selection by IDs/ICDs/SQL/R callbacks, pipeline checks, retrieval, export, and merging. Use separate extension guidance when defining a new concept or registering a new source family.
---

# rendehR user

Use the installed package's public functions. Establish whether the user wants explanation, code, or execution, reusing explicit authorization and choices. Ask only about unresolved decisions that change the result. Produce reproducible code and report the actual execution state.

## Task routing

Read only the applicable guide; follow another guide when the task crosses a stage boundary.

| Task | Guide |
| --- | --- |
| New demo/project, configuration, prerequisites | [Setup and readiness](references/setup-and-readiness.md) |
| Discover sources, tables, concepts and provenance; retrieve, aggregate, export, or explore data | [Discovery and export](references/discovery-and-export.md) |
| Select/materialize a cohort from current datasets | [Custom cohorts](references/custom-cohorts.md) |
| Run stages, verify outputs, inspect continuation | [Run and verify](references/run-and-verify.md) |
| Combine already processed sources | [Merge sources](references/merge-sources.md) |

A new cohort in MIMIC/eICU is not a new source family. Defining a new variable belongs to `rendehr-add-concept`; adding a new dataset family belongs to `rendehr-add-source`. If those skills are unavailable, identify the extension task and consult installed public documentation without pretending this skill includes validated extension procedures. Ordinary workflows here do not depend on either extension skill.

## Shared rules

- Use installed-package loading, not developer worktree paths. Confirm version, source, backend, and configured project. Do not install dependencies or change package source as an implicit prerequisite.
- Manual setup has separate template/editing and continuation blocks. Use the setup guide's pause and result gate; its linked Block A/B assets are response templates, never auto-run hooks.
- For “no clinical scores,” control both enrichment stages and concept selection. Default run_concepts layers include score SQL; use the setup guide’s reviewed restrictions and disclose their output coverage.
- Preserve qualification key, extraction scope, output grain, time origin/window, units, aggregation, and missingness. Explicitly supplied clinical definitions remain the user's choices.
- Inspect actual availability before retrieval. A concept table can exist without harmonized data. Report unavailable/unknown evidence rather than inventing results.
- Selection functions can scan source data and execute supplied SQL/callbacks; dry-run options differ by API. Read the relevant guide before labeling an operation read-only.
- Do not clear active filters, replace files/tables, change source, or broaden cohort scope simply to make a command succeed. Explain the required change within the user's authorization.
- Keep secrets and full patient-level results local. Return artifacts, concise counts, checks performed, and remaining limitations. Preserve scripts and resolved options without credentials.

## MCP tools

When the rendehr MCP server is connected, `ehr_find` searches raw dictionary
labels in the loaded database and `draft_config_row` previews a candidate
dictionary row. Both are read-only; the concept workflow is in
`rendehr-add-concept`.

## Status

The guides were reconciled with the development branch in October 2026: every package function and argument they name exists, and the MIMIC-IV demo setup (Blocks A and B) was run end to end on a scratch database. The other guides' examples have not each been run live. Distinguish generated, syntax-checked, executed, and verified results. Development version strings alone do not identify an exact revision.

# Setup and readiness

Scope: prepare a demo setup plan and inspect existing configuration using public functions. Blocks A and B were run end to end in October 2026 on the MIMIC-IV demo (project setup, download, cohort build and database load). Treat other routes as untested for the target release.

## Decide which starting point applies

- New user/project: identify source, demo/full intent, project/data locations, backend needs, and whether code or execution is requested. Recommend a demo for a first example without silently replacing a requested full source.
- Existing project: inspect the supplied configuration before generating replacements. Do not assume the agent's current directory is the user's project.
- Prepared database: use discovery/export guidance; initialization is not required merely to retrieve data.

Do not ask for passwords in chat. Credentials belong in the user's local credential file or supported environment variables. Check that prerequisites are available; provide installation guidance if absent, without treating plan generation as permission to install dependencies.

## Important baseline behavior

- `ehr_setup_assistant()` returns an `ehr_setup_plan` with `choices`, `script`, and `next_steps`. With `interactive = FALSE, save_script = FALSE`, it generates code without creating the project or executing that code.
- Explicitly use `load_method = "library"` (the assistant's own default); `"load_all"` targets a developer checkout and `"none"` emits no library call.
- Specify cohort intent and optional scores. The assistant defaults to SCI and enabled comorbidity/injury scoring; those are not neutral scientific choices. The example below requests the whole available demo population and disables comorbidity/injury enrichment. This alone does not disable score-producing concept SQL; apply the no-score restrictions below before returning a final script.
- The assistant's `cohort` choices are `scid`, `sepsis3` (with `sepsis_bundle` variants and a hospital-level default scope), `custom` (ICD filter path), and `none`. Other named cohorts or direct ID routes in the package are not assistant choices; do not pass unsupported values.
- The assistant rejects mixed demo/full sources. Separate plans when appropriate; do not silently drop a source.
- `config_setup = "manual"` generates template/editing steps; `"skip"` assumes existing configuration; `"interactive"` adds interactive confirmation steps to the generated script (the alias `"template"` maps to manual). These choices describe future script behavior. Review generated instructions before execution.
- PostgreSQL is the complete pipeline route. `run_one_source(engine = "duckdb")` builds the cohort, windows, cohort_info and concepts in a local DuckDB file without contacting PostgreSQL, and skips the PostgreSQL-only stages (cohort annotations, comorbidities, ISS, external concepts, harmonized tables) with a message. The setup assistant's DuckDB route still generates only a cohort-loading workflow. Do not promise DuckDB harmonized tables.
- `ehr_init_project()` creates directories and missing configuration/credential templates. It is not a read-only check.
- `ehr_check_config()` returns `ok`, `problems`, `warnings`, and resolved paths. Warnings can coexist with `ok = TRUE`; missing credentials can be a warning. This is not an authentication, source-import, disk-capacity, or database-completeness test.
- `ehr_use_config()` changes the R session's active configuration. `ehr_connect()` stores credentials for later operations; returning successfully is not proof of a database connection.

## Generator starting point (requires adaptation)

User request: “Give me code for a MIMIC-IV demo project using PostgreSQL. Use the whole available demo population, admission-level output, no clinical scores, and do not run it.”

Resolve `project_dir`, `data_dir`, and `database_name` from the user's supplied locations/name before running the following plan-generation call. These are caller inputs, not fixed developer paths. This call itself does not create those locations or save a script.

```r
plan <- rendehR::ehr_setup_assistant(
  src_name = "mimic_iv_demo",
  source_size = "demo",
  engine = "postgres",
  db_name = database_name,
  base_dir = project_dir,
  data_dir = data_dir,
  config_setup = "manual",
  cohort = "none",
  pipeline = "stages",
  include_download = TRUE,
  run_comorbidities = FALSE,
  run_iss = FALSE,
  analysis_level = "admission",
  merge = "none",
  inspect_options = "none",
  load_method = "library",
  save_script = FALSE,
  interactive = FALSE
)
stopifnot(inherits(plan, "ehr_setup_plan"))
parse(text = plan$script)  # Syntax only; does not execute generated code.
cat(plan$script)
```

`include_download = TRUE` includes a future download block; it does not download while generating the plan. Do not `source()` or `eval()` the generated script to check syntax. If the user asks to save it, use `write_setup_script()` with an agreed new path and no overwrite. At this baseline, manual setup emits an editing message but no enforced pause, and warnings are not gated; the generated configuration check stops on a profile, source or path conflict. It also leaves concept execution unrestricted despite disabling enrichment scores. Apply the two-block presentation, configuration gate, and no-score concept restrictions below to the user-facing script; do not present the unmodified generator output as an unattended end-to-end run.

For a full-data request, resolve authorized source access and storage requirements first; do not reuse this whole-population demo choice silently.

## Reusable final-script template

For the exact request “MIMIC-IV demo, PostgreSQL, whole demo population, no clinical scores,” use [Block A output template](../assets/mimic-iv-demo-postgres-block-a.R) and [Block B output template](../assets/mimic-iv-demo-postgres-block-b.R). Read it as an asset for the response; do not source or run it to load the skill. It is a draft workflow, not an installation hook or an automatically executed tool.

1. Use the Block A asset and the explanation below to create the separate template/editing stage when the user wants the complete setup script. Block B assumes that stage is already complete and the target database exists.
2. Substitute only user-supplied project/data paths and database name. Keep clearly marked placeholders for code-only requests with missing values; obtain concrete values before execution. Keep settings consistent across both blocks and the edited configuration.
3. Preserve configuration and warning gates, credential reloading, connection cleanup, whole-population selection, and the explicit layer/concept restrictions. Preserve source/profile/path checks, reviewed package-definition checks, and the fresh-table precondition for this example. Do not auto-acknowledge warnings.
4. Present the adapted script as code or save a copy to a requested user location. Do not customize the shipped skill asset in place with personal paths or credentials.
5. For generation-only requests, stop after presenting the script and stating validation status. Execute a personalized copy only when execution is requested. Reading the skill never authorizes downloads or builds.

This template is specific to this setup scenario. Do not change only the source string and claim support for full sources, DuckDB, or a different cohort. Use the matching public API and guide, or report that the requested route is not yet evaluated. Keep the linked assets as the maintained Block A/B templates; the guidance below explains its gates without duplicating the full workflow.

## Enforce a no-clinical-scores request

Skipping run_comorbidities/run_iss does not exclude all scores. At this baseline run_concepts defaults to flat, ts, dur, ehr, user and external layers. The ehr layer includes SOFA, SAPS II, OASIS and other scores; flat also includes ISS-related outputs. GCS/RASS are scored assessments too. A no-score request therefore requires both stage control and concept control.

The reviewed MIMIC-IV demo Block B uses only layers c("flat", "ts", "dur") with an explicit allowlist. Flat selection matches SQL filename stems, which are the concept names (for example `age`), and atomic ts/dur selection matches concept_name. Harmonization receives the same flat names. Preserve this distinction; do not replace the allowlist with “all flat concepts.”

The starter output is deliberately limited: 22 non-score flat concepts, 13 selected vital/laboratory time series, and the package's MIMIC-IV medication-exposure duration definitions. It excludes ehr/user/external execution, ISS-related flat outputs, GCS/RASS concepts and enrichment scoring. It is not every available non-score concept. Raw imported data can still contain charted scores; no-score concept extraction does not mean removing those raw observations.

The asset checks the reviewed package definition locations, rejects supplemental CSV overrides for this fixed example, and requires a fresh source destination without existing concept/harmonized/derived tables or named raw scoring outputs. This prevents reuse of previously generated score tables in the normal table-based workflow. It is a fresh-install example, not a general resume solution or a complete audit of arbitrary views/custom SQL. Do not delete old objects to satisfy its guard. For an existing project, inspect actual objects and dependencies and construct a separately reviewed extraction plan.

Do not silently narrow a request for additional non-score variables to the starter list. Inspect and extend the supported selection, explaining any remaining gaps. For other sources/backends, verify their SQL and configuration instead of transplanting this MIMIC-IV allowlist. Package path equality and a development version string do not fingerprint SQL contents; recheck definitions against the intended release.

Before reporting a final script, syntax-check with parse(), never source()/eval(). Static checks establish syntax and selection intent only. A live test must confirm cohort counts, expected concept tables, harmonized outputs and absence of score computation. Record generation, parsing, execution and output verification separately.

## Live testing

`run_concepts()` writes generated ts/dur SQL under the project's
`sql/generated/<source>/{ts,dur}` (or the configured generated root), not into
the installed package, so live testing does not modify package files. Use a
disposable database and project, and verify `find.package("rendehR")` points at
the build you intend. Do not treat `dry_run = TRUE` as a syntax-only check: it
still generates SQL and opens a connection.

## Manual setup: two separately run blocks

When returning a final setup script with templates the user must edit, use this structure. These are changes to the generated user artifact, not to the package generator.

**Block A — create templates, then stop.** Include the selected paths, library loading, project directories, and template creation with overwrite disabled. Include no downloads or pipeline calls. End the block with an intentional stop:

```r
stop(
  "Templates are ready. Edit your local project and credential files, then run Block B separately.",
  call. = FALSE
)
```

Explain that this stop is expected, not a failed installation. In RStudio the user runs Block A as a selection, edits files locally, then selects Block B. If saving scripts, prefer separate setup and continuation files when requested. For a single file, retain the stop so sourcing the whole file cannot silently proceed. Running isolated lines below a stop bypasses it; do not claim it is a security boundary.

When no paths were supplied, clearly marked placeholders are acceptable for code-only output. Tell the user to replace them before running either block. Do not silently create literal `[path]` directories. A request to actually execute setup needs resolved inputs first.

**Block B — reload, check, then continue.** Repeat the required library and path declarations so the continuation can be run in a fresh R session. Read the edited configuration from disk on every attempt; do not reuse an earlier successful check after edits. The following local script helper enforces the returned result:

```r
require_valid_setup <- function(check, reviewed_warnings = character()) {
  if (!isTRUE(check$ok) || length(check$problems) > 0L) {
    if (length(check$problems)) message(paste(check$problems, collapse = "\n"))
    stop("Configuration check failed. Correct the reported problems before continuing.",
         call. = FALSE)
  }
  pending <- setdiff(check$warnings, reviewed_warnings)
  if (length(pending)) {
    message(paste(pending, collapse = "\n"))
    stop("Review configuration warnings before continuing; resolve them or explicitly acknowledge those acceptable for this task.",
         call. = FALSE)
  }
  invisible(check)
}

if (!file.exists(config_path)) stop("Project configuration file not found.")
check <- rendehR::ehr_check_config(config_path = config_path, quiet = TRUE)
# Leave empty initially. After reviewing locally, list only exact warning
# messages acceptable for this task; do not automatically copy check$warnings.
reviewed_warnings <- character()
require_valid_setup(check, reviewed_warnings)
rendehR::ehr_use_config(config_path)
# Connection/precondition checks, then explicitly requested download/build calls,
# follow this gate. Nothing expensive belongs above it.
```

Warnings are not all failures. For example, a missing credentials file may be acceptable when environment-based authentication is intentionally configured; that still requires a successful connection check for database work. Do not acknowledge a warning merely to advance the script. Show warning messages locally without printing credential values, and record why any unresolved warning was acceptable. New warning messages remain blocking even when previously reviewed warnings are acknowledged.

A thrown error from `ehr_check_config()` also stops Block B. Never catch such an error and continue with a previous check result. A clean configuration check is only the configuration gate; it does not prove that credentials, the database, or source data are ready.

For database work, perform the connection check described below before running expensive dependent operations. If the named database does not yet exist, report that state and resolve the intended creation step; do not present an intentional new database as a malformed project or create it as an unrequested connectivity fix. In a session that cached credentials before the user edited them, clear the package credential cache with `ehr_disconnect()` before reloading, or use a fresh R session. Do not expose credentials in the response.

## Inspect an existing configuration

Run this only against the user's supplied existing `config_path`. It checks configuration without creating a project. Errors during file loading should be reported as configuration inspection failures.

```r
if (!file.exists(config_path)) stop("Project configuration file not found.")
check <- rendehR::ehr_check_config(config_path = config_path, quiet = TRUE)
list(ok = check$ok, problems = check$problems, warnings = check$warnings)
```

Review paths locally with the user as necessary. Do not print the full configuration or credential values into conversation output. A valid configuration with warnings requires interpreting those warnings, not converting them into a blanket readiness PASS.

## Connection check, when requested

Use an already supplied DBI connection or open one using `load_credentials()` and `connect_postgres(db_name, creds)` with user-configured credentials. Avoid unattended prompts when credentials are missing. Run a small read-only query such as `SELECT 1`, and close only a connection this task opened. Do not create a database just because connecting failed.

A successful connection says nothing about source tables. Inspect source/schema availability and the specific next operation's required inputs separately. Creating a new project with an intentionally empty database is different from declaring an existing extraction database ready.

## Report readiness by evidence

| Check | Evidence needed | What it does not establish |
| --- | --- | --- |
| Package available | Installed package and version | Release compatibility by version string alone |
| Configuration acceptable | Returned problems and warnings reviewed | Credentials work or required tables exist |
| Database reachable | Actual connection and simple query | Source import or harmonization completed |
| Source/output present | Required tables and operation-specific metadata inspected | Clinical validity or complete upstream freshness |
| Plan generated | Choices reviewed and script parses | Downloads, builds, or analysis executed |

Use passed, failed, warning, and not checked where appropriate. Report the exact next step supported by evidence. Do not claim “ready for everything.”

## Common cases

- Missing package: explain that plan execution requires the package; do not claim a generated plan ran.
- Missing/malformed configuration: identify the problem; offer a new-project plan or supported configuration correction without overwriting existing files.
- Missing credentials: request local configuration, not secret values in chat.
- DuckDB-only request plus full harmonization: DuckDB delivers concepts but not harmonized tables or scores; explain the boundary and ask which outcome the user wants to prioritize.
- Unknown/mismatched source: report the rejected choice; do not substitute a dataset.
- User asks only to review setup: do not call initialization, download, schema creation, or run_one_source.

## Evidence

Reviewed `R/setup_assistant.R`, `R/env_config.R`, `R/access_core.R`, and public help for ehr_setup_assistant, ehr_check_config, ehr_use_config, ehr_init_project, ehr_connect, connect_postgres, and run_one_source. This guide does not replace validation of the installed release.

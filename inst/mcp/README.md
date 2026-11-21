# rendehR MCP server and skills

This directory is installed as `mcp/` inside the rendehR package. It contains
the local MCP runtime and three skill packages: `rendehr-user`,
`rendehr-add-concept` and `rendehr-add-source`. `VERSION` is the MCP version;
since rendehR 1.0.0 it equals the package version and moves with it.

The bundle is maintained in the rendehR repository. `tests/testthat/test-mcp.R`
covers the protocol, the offline tools and the stdio transport; the
database-backed tools run only when a test project is configured
(`EHR_TEST_MCP_CONFIG`).

## Start the server

The launcher installed with the package:

```r
system.file("mcp", "inst", "scripts", "rendehr_mcp.R", package = "rendehR")
```

Run it over stdio with `Rscript --vanilla`. A tested Claude Code registration:

```sh
claude mcp add rendehr -s user \
  -e R_LIBS_USER=/path/to/R/library \
  -e EHR_CONFIG=/abs/path/to/config/ehr_config.json \
  -- /usr/bin/Rscript --vanilla \
     "$(Rscript -e 'cat(system.file("mcp","inst","scripts","rendehr_mcp.R", package="rendehR"))')"
```

The equivalent JSON block (drop `type` in `claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "rendehr": {
      "type": "stdio",
      "command": "/usr/bin/Rscript",
      "args": ["--vanilla", "/path/to/rendehR/mcp/inst/scripts/rendehr_mcp.R"],
      "env": {"EHR_CONFIG": "/abs/path/to/config/ehr_config.json"}
    }
  }
}
```

`EHR_CONFIG` is needed only by the database tools (`ehr_find`,
`list_loaded_concepts`, `cohort_summary`). The credentials file is the same
nested `{"postgres": {...}}` file the pipeline uses; the `PGHOST`, `PGPORT`,
`PGUSER`, `PGPASSWORD` and `PGSSLMODE` variables override it. The server never
prompts, so in a non-interactive client a missing credential is an error message
rather than a password request.

`RENDEHR_SOURCE_PATH` optionally points at a rendehR checkout. It affects only
`search_concept_definitions` and `draft_config_row`; omit it to read the
installed package's catalogue. Startup requires jsonlite; the database tools
require the installed package and a prepared PostgreSQL database.

Restart the MCP client after an update and call `rendehr_status` to confirm the loaded versions;
it reports both `mcp_version` and the package `version`.

## Tools

| Tool | Reads | Notes |
|:--|:--|:--|
| `rendehr_status` | Package and MCP versions | No configuration needed |
| `check_setup` | Configuration, paths, database readiness | Requires `config_path` |
| `search_concept_definitions` | Packaged dictionary | `term`, optional `src`, `limit` 1–100 (default 20), `offset` |
| `list_topics` | Cached dictionary topics | `limit`, `offset` |
| `resolve_term` | Terms to topics | `term` (one string or 1–20 terms), `include_cached_topics` |
| `draft_config_row` | One candidate dictionary row | From a template; never writes |
| `ehr_find` | Raw dictionary item labels | Database tool; `src` required |
| `list_loaded_concepts` | Loaded concept metadata | `total` and the first `limit` rows |
| `cohort_summary` | Cohort counts and sex distribution | Aggregate only; errors when the source has no cohort tables |

All tools are read-only. Database sessions open read-only with a 30-second
timeout. `ehr_find` reads the raw dictionary tables declared in
`inst/config/mcp_dict_tables.json`; `limit` applies across tables in round-robin
order, `tables` accepts 1–6 table names, and `expand_synonyms` defaults to
`TRUE`. Terms of up to four characters (abbreviations such as `hr` and `map`)
match whole words only.

`draft_config_row` refuses to invent a row. The template must match exactly one
existing row (`template_match_value` disambiguates), the item class must be
`itm_itm`, `rgx_itm`, `rgx_exp_itm` or `col_itm`, the concept name must not
already exist, and it must match `^[a-z][a-z0-9_]*$`.

Search candidates are not validated clinical definitions, and a specimen type
is not a measurement unit. The server does not execute pipelines or save
concepts.

## Skills

- [rendehr-user](skills/rendehr-user/SKILL.md): package workflows, including the
  cohort explorer.
- [rendehr-add-concept](skills/rendehr-add-concept/SKILL.md): concept authoring
  with the MCP tools.
- [rendehr-add-source](skills/rendehr-add-source/SKILL.md): registering a new
  dataset family.

Install them as directories, keeping each `references/` and `assets/` folder
next to its `SKILL.md`:

```sh
cp -r "$(Rscript -e 'cat(system.file("mcp","skills", package="rendehR"))')"/rendehr-* \
      ~/.claude/skills/
```

Bundling does not activate a skill, and the skills' R templates do not run when
loaded. The skills cover public R workflows beyond the current MCP tools; do not
assume every workflow is exposed as a tool. The package tests check that every
`rendehR::`-qualified function the skills name is exported and that their R
templates parse; unqualified names, arguments and prose are not validated. Each
skill's status section says what has been run live.

## Licensing

The MCP runtime and skills are part of rendehR and are distributed under
GPL-3.

See `vignette("mcp")` for the setup and a session description.

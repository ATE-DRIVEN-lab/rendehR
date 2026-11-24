# Projects, configuration and raw data

Every rendehR function works inside a project: a folder with a
configuration file, credentials, and room for the raw and cohort files.
This article shows what a project contains, which settings matter, and
how several studies can share one copy of the raw data.

## The project folder

[`ehr_init_project()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_init_project.md)
creates the folder and its two configuration files. Nothing is
overwritten if they already exist.

``` r
library(rendehR)
ehr_init_project(src_name = "mimic_iv_demo", base_dir = "my-study")
```

``` r
list.files("my-study", recursive = TRUE, include.dirs = TRUE)
#>  [1] "config"                                  
#>  [2] "config/config_pass.json"                 
#>  [3] "config/ehr_config.json"                  
#>  [4] "data"                                    
#>  [5] "data/cohorts_data"                       
#>  [6] "data/cohorts_data/mimic_iv_demo_parquet" 
#>  [7] "data/source_data"                        
#>  [8] "data/source_data/mimic_iv_demo"          
#>  [9] "sql"                                     
#> [10] "sql/generated"                           
#> [11] "sql/generated/mimic_iv_demo"             
#> [12] "sql/generated/mimic_iv_demo/dur"         
#> [13] "sql/generated/mimic_iv_demo/ts"          
#> [14] "sql/mimic_iv_demo"                       
#> [15] "sql/mimic_iv_demo/concepts_sqls"         
#> [16] "sql/mimic_iv_demo/concepts_sqls/external"
#> [17] "sql/mimic_iv_demo/concepts_sqls/user"
```

| Path                                  | Holds                                                                                                               |
|:--------------------------------------|:--------------------------------------------------------------------------------------------------------------------|
| `config/ehr_config.json`              | Project settings: database, source, paths, cohort profile                                                           |
| `config/config_pass.json`             | PostgreSQL and PhysioNet credentials                                                                                |
| `data/source_data/<source>/`          | Raw files from PhysioNet (unless `raw_data_root` points elsewhere)                                                  |
| `data/cohorts_data/<source>_parquet/` | The cohort, imported to Parquet                                                                                     |
| `data/duckdb/`                        | DuckDB files, when you use the DuckDB engine                                                                        |
| `sql/<source>/concepts_sqls/user/`    | Your own SQL concepts (see [`vignette("concepts")`](https://ate-driven-lab.github.io/rendehR/articles/concepts.md)) |
| `sql/generated/<source>/`             | SQL that rendehR generates from the concept dictionary                                                              |

Keep `config/config_pass.json` and `data/` out of version control: one
holds a password, the other patient data.

## The configuration file

``` r
cat(readLines("my-study/config/ehr_config.json"), sep = "\n")
{
  "version": 1,
  "project": {
    "base_dir": "~/rendehr-docs/my-study"
  },
  "database": {
    "db_name": "rendehr",
    "credentials_path": "config/config_pass.json"
  },
  "source": {
    "src_name": "mimic_iv_demo"
  },
  "paths": {
    "raw_data_root": null,
    "cohort_parquet_root": "data/cohorts_data",
    "duckdb_root": "data/duckdb"
  },
  "concepts": {
    "ts_config_path": null,
    "dur_config_path": null,
    "user_concepts_path": null
  },
  "cohort": {
    "profile": "generic",
    "icd_filter_path": null,
    "icd_dict_path": null,
    "icd_proc_dict_path": null,
    "profiles": []
  }
}
```

The settings you are most likely to change:

- **`database.db_name`**: the PostgreSQL database for this project. It
  defaults to `"rendehr"`; give every study its own database. Functions
  that take `db_name` use this value when you leave it out.
- **`paths.raw_data_root`**: where the raw files are. `null` means
  `data/source_data` inside the project. Point it at a shared folder to
  reuse one download in several projects (below).
- **`cohort.profile`**: `"generic"` (the default) or `"scid"`, the
  spinal cord injury reference implementation. The profile decides two
  defaults. For an SCI cohort selected by diagnosis, `"scid"` adds the
  injury classification to the cohort table; any other cohort is left
  alone. And under `"scid"` the pipeline computes the comorbidity
  indices and injury severity scores unless told not to, while under
  `"generic"` they run only when asked.
- **`cohort.icd_filter_path`**: the ICD list used when a call names no
  cohort.

Relative paths are relative to `project.base_dir`, which
[`ehr_init_project()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_init_project.md)
writes as an absolute path.

## Credentials

`config_pass.json` holds the PostgreSQL login, and a PhysioNet login for
the credentialed datasets:

``` json
{
  "postgres": {"host": "localhost", "port": 5432, "user": "postgres", "password": "..."},
  "physionet": {"username": "...", "password": "..."}
}
```

Environment variables take precedence over the file: `PGHOST`, `PGPORT`,
`PGUSER`, `PGPASSWORD` and `PGSSLMODE` for PostgreSQL, `PHYSIONET_USER`
and `PHYSIONET_PASS` for PhysioNet. The demo datasets are open and need
no PhysioNet login.

## Using a project

[`ehr_use_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_use_config.md)
makes a project active for the R session, and
[`ehr_check_config()`](https://ate-driven-lab.github.io/rendehR/reference/ehr_check_config.md)
reports what the settings resolve to:

``` r
ehr_use_config("my-study/config/ehr_config.json")
check <- ehr_check_config(quiet = TRUE)
check$ok
#> [1] TRUE
check$paths$raw_dir
#> [1] "~/rendehr-docs/my-study/data/source_data/mimic_iv_demo"
```

Without an active project, rendehR looks for `config/ehr_config.json` in
the working directory, so [`setwd()`](https://rdrr.io/r/base/getwd.html)
into the project works too. You can also pass `base_dir` and `db_name`
to each call.

## Raw data

[`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
fetches a PhysioNet release into `raw_data_root`. It checks each file
against the release’s SHA-256 manifest, reuses files that already match,
and resumes interrupted downloads.

``` r
download_source("mimic_iv_demo")       # open
download_source("mimic_iv")            # needs a credentialed PhysioNet account
```

The supported releases and their size on disk as downloaded:

| Source         | Release |   Size |
|:---------------|:--------|-------:|
| MIMIC-IV demo  | 2.2     |  16 MB |
| MIMIC-III demo | 1.4     |  94 MB |
| eICU-CRD demo  | 2.0.1   |  53 MB |
| MIMIC-IV       | 3.1     |  10 GB |
| MIMIC-III      | 1.4     | 6.2 GB |
| eICU-CRD       | 2.0     | 5.2 GB |

The cohort import can cache the raw CSV as Parquet next to the project
(`data/raw_parquet_cache/`). That takes about as much space again, and
makes repeated imports of a full source much faster.

## Several studies, one download

A study is a project: its own cohort, database and settings. Point
`raw_data_root` at the folder that already holds the files and nothing
is downloaded twice. Here a second project reuses the demo downloaded in
[`vignette("rendehR")`](https://ate-driven-lab.github.io/rendehR/articles/rendehR.md):

``` r
ehr_init_project(
  src_name = "mimic_iv_demo", base_dir = "sci-study",
  raw_data_root = normalizePath("rendehr-demo/data/source_data")
)
ehr_check_config(config_path = "sci-study/config/ehr_config.json", quiet = TRUE)$paths$raw_dir
#> [1] "~/rendehr-docs/rendehr-demo/data/source_data/mimic_iv_demo"
```

Set a different `db_name` in each project’s configuration and the two
studies never touch each other’s tables.

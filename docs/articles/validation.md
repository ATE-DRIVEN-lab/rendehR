# Validation

rendehR is validated by a harness that is kept in the development
repository and run before every release. It does not use package code to
check package code: raw counts come from the PhysioNet files read as
untyped text, and concept selection, parsing, unit conversion, ICD
normalisation and cohort derivation are re-implemented from their
specifications. The last level compares rendehR with
[ricu](https://github.com/eth-mds/ricu) on the same admissions. This
page summarises the run for version 1.0.0.

## Two profiles

| Profile | Data                                                    | Cohorts                                                                    |
|---------|---------------------------------------------------------|----------------------------------------------------------------------------|
| Demo    | the open MIMIC-III, MIMIC-IV and eICU-CRD demo datasets | all patients, spinal cord injury (SCI), Sepsis-3, a random identifier list |
| Full    | the complete MIMIC-III, MIMIC-IV and eICU-CRD releases  | the SCI cohort                                                             |

The demo profile runs every level, including the package’s own test
suite and `R CMD check`; the full profile repeats the data levels at
scale.

## Levels

| Level | What is checked                                                                                                                                               |
|-------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| L0    | Environment, raw-file checksums, no credentials in outputs                                                                                                    |
| L1    | `R CMD check`, the test suite with every database test enabled, test safety, documentation, configuration linters, every vignette re-knitted on the demo data |
| L2    | Parsing, units, ICD normalisation and credentials on synthetic inputs                                                                                         |
| L3    | Each pipeline stage’s pre- and post-conditions on real data                                                                                                   |
| L4    | Row conservation, concept selection, unit conversion and event-level tracing back to the raw files                                                            |
| L5    | Demographics, mortality and value plausibility                                                                                                                |
| L6    | Identical rebuilds with other threads and chunk sizes; resumed imports                                                                                        |
| L7    | Bad inputs fail loudly; destructive operations are guarded                                                                                                    |
| L10   | The explorer’s numbers against independent SQL, its statistics, and every page and tab opened in a browser                                                    |
| L11   | Cohort, static variables, time series and exposures against ricu 0.6.3                                                                                        |

A check passes, warns or fails. A warning is a documented property of
the data (for example ICU stays recorded outside their admission in the
source); known differences from ricu are listed with their expected
shape in a register, and count as explained only when that shape is
measured.

## Full data: the SCI cohort

- **Cohort.** 46,513 admissions (MIMIC-III 4,320, MIMIC-IV 39,850,
  eICU-CRD 2,343). Admissions, patients and ICU stays match ricu exactly
  in every source.
- **Build.** The three sources take 73 minutes from raw files to the
  merged schema, with a peak memory of 71 GB.
- **Determinism.** A second build with other thread counts and import
  chunk sizes gives identical content in all 975 tables.
- **Static variables.** Age, sex, hospital mortality and length of stay
  agree with ricu for 100% of the shared admissions; height and
  admission weight, where rendehR pools more sources by design, for
  85-100%.
- **Time series.** 63-64 concepts per source reproduce ricu row for row
  (admission, minute, value); 9-12 of them per source differ by a
  documented definition, and the difference has the documented shape.

Distance between the value distributions of rendehR and ricu
(Kolmogorov-Smirnov D over all values of each concept, 0 = identical):

| Source    | Concepts | Mean D | Largest D                |
|-----------|----------|--------|--------------------------|
| MIMIC-III | 54       | 0.0010 | 0.038 (SpO2)             |
| MIMIC-IV  | 58       | 0.0013 | 0.023 (direct bilirubin) |
| eICU-CRD  | 54       | 0.0006 | 0.014 (magnesium)        |
| All       | 166      | 0.0010 | 0.038                    |

163 of the 166 concepts have D at most 0.01, and none exceeds 0.05.

## Check totals for 1.0.0

| Profile                       | Checks | Pass  | Warn | Fail | Error |
|-------------------------------|--------|-------|------|------|-------|
| Demo (4 cohorts, every level) | 1,129  | 1,097 | 18   | 1    | 0     |
| Full (SCI cohort)             | 314    | 301   | 11   | 0    | 0     |

The rows do not add up to the checks: the demo totals also hold 12
checks of earlier known failures that now pass and 1 registered known
failure, the full totals one check that now passes and one skipped check
(a destructive check that the demo profile covers). The one demo failure
is known and minor: concept values are stored with three decimals, which
a synthetic check of full precision flags.

Highlights of the demo run: `R CMD check` with 0 errors and 0 warnings;
the test suite with 4,235 passing tests offline and 4,756 with every
database test enabled, which leave every existing table of the test
database intact; all 11 vignettes re-knitted from scratch; and all 31
pages and tabs of the explorer opened in a browser without an error.

## How a run is recorded

The harness needs a PostgreSQL server, the raw PhysioNet files and ricu
0.6.3. Each run records every check with its level, status and evidence,
and the totals; comparing two runs lists the checks whose status
changed. A written plan describes every check and its pass criterion.

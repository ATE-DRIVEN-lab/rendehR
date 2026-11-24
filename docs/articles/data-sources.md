# Data sources

rendehR reads three intensive care databases from
[PhysioNet](https://physionet.org/) and their open demo versions. The
source registry (`rendehr-data-source.json`,
[`list_registered_sources()`](https://ate-driven-lab.github.io/rendehR/reference/list_registered_sources.md))
holds each one’s release, tables and identifiers;
[`register_source()`](https://ate-driven-lab.github.io/rendehR/reference/register_source.md)
adds a new one, see
[`vignette("adding-a-new-source")`](https://ate-driven-lab.github.io/rendehR/articles/adding-a-new-source.md).

| Source                                                              | Name in rendehR  | Release | Access       | Tables read |
|---------------------------------------------------------------------|------------------|---------|--------------|-------------|
| [MIMIC-III](https://physionet.org/content/mimiciii/1.4/)            | `mimic_iii`      | 1.4     | credentialed | 26          |
| [MIMIC-IV](https://physionet.org/content/mimiciv/3.1/)              | `mimic_iv`       | 3.1     | credentialed | 31          |
| [eICU-CRD](https://physionet.org/content/eicu-crd/2.0/)             | `eicu`           | 2.0     | credentialed | 31          |
| [MIMIC-III demo](https://physionet.org/content/mimiciii-demo/1.4/)  | `mimic_iii_demo` | 1.4     | open         | 26          |
| [MIMIC-IV demo](https://physionet.org/content/mimic-iv-demo/2.2/)   | `mimic_iv_demo`  | 2.2     | open         | 31          |
| [eICU-CRD demo](https://physionet.org/content/eicu-crd-demo/2.0.1/) | `eicu_demo`      | 2.0.1   | open         | 31          |

**Credentialed** sources need a PhysioNet account with credentialed
access, the required training and the project’s data use agreement
signed;
[`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
then fetches them with your PhysioNet credentials. The **demo** sources
(100 patients each for MIMIC, about 2,500 unit stays for eICU-CRD) are
open to everyone and are what the articles on this site use. The
registry also keeps definitions for HiRID, AmsterdamUMCdb and SICdb;
they are blocked until their support is validated.

## How the sources differ

|                        | MIMIC-III                           | MIMIC-IV                            | eICU-CRD                              |
|------------------------|-------------------------------------|-------------------------------------|---------------------------------------|
| Hospitals              | one (Beth Israel Deaconess, Boston) | one (Beth Israel Deaconess, Boston) | more than 200 US hospitals            |
| Years                  | 2001-2012                           | 2008-2022                           | 2014-2015                             |
| Patient                | `subject_id`                        | `subject_id`                        | (`uniquepid`)                         |
| Hospital admission     | `hadm_id`                           | `hadm_id`                           | `patienthealthsystemstayid`           |
| ICU stay               | `icustay_id`                        | `stay_id`                           | `patientunitstayid`                   |
| Time                   | dates shifted per patient           | dates shifted per patient           | minutes from unit admission, no dates |
| Diagnoses              | ICD-9                               | ICD-9 and ICD-10                    | problem list and ICD codes            |
| Deaths after discharge | yes                                 | up to one year                      | no (hospital only)                    |

These differences reach every analysis:

- **Clock.** rendehR expresses every time as minutes since hospital and
  ICU admission (`time_since_hadm_min`, `time_since_icu_min`), so the
  three sources share one clock. Calendar dates exist only in MIMIC,
  shifted per patient: intervals within a patient are real, absolute
  dates are not.
- **Level.** MIMIC records data per hospital admission with ICU stays
  inside it; eICU-CRD is organised by ICU unit stay.
  [`merge_sources()`](https://ate-driven-lab.github.io/rendehR/reference/merge_sources.md)
  merges at admission level (one row per hospital admission, first ICU
  stay) or at ICU level (one row per ICU stay); see
  [`vignette("harmonized-data")`](https://ate-driven-lab.github.io/rendehR/articles/harmonized-data.md).
- **Coding.** Cohort definitions list both ICD-9 and ICD-10 codes, so
  one definition selects comparable patients in every source; see
  [`vignette("cohorts")`](https://ate-driven-lab.github.io/rendehR/articles/cohorts.md).
- **Content.** Not every concept exists in every source (for example
  eICU-CRD has no post-discharge mortality). The [concept
  dictionary](https://ate-driven-lab.github.io/rendehR/articles/concept-dictionary.md)
  shows which sources define each concept, and the explorer’s
  *Availability* tab shows how often a cohort actually has it.

## Where the files go

[`download_source()`](https://ate-driven-lab.github.io/rendehR/reference/download_source.md)
stores each source as Parquet under the project’s raw data root, one
folder per source (`<raw_data_root>/<source>/`). The files are read,
never modified; the cohort import streams only the rows of the cohort
into the database. Keep credentialed data where your data use agreement
allows, and never publish individual-level rows.

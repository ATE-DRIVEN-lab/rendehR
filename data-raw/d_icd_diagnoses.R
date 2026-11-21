# Builds inst/extdata/d_icd_diagnoses.csv from the public-domain CMS ICD-9-CM
# and ICD-10-CM code description files (US government works, no copyright).
#
# Every code that appears in any listed release (ICD-9-CM v23-v32, ICD-10-CM
# FY2016-FY2025) is kept, with its title from the
# newest release that has it, so codes retired before FY2025 (still present in
# MIMIC and eICU data) resolve too.
#
# Usage (from the package root):
#   Rscript data-raw/d_icd_diagnoses.R [download_dir]

args <- commandArgs(trailingOnly = TRUE)
dl <- if (length(args)) args[[1]] else file.path(tempdir(), "cms_icd")
dir.create(dl, recursive = TRUE, showWarnings = FALSE)

cms <- "https://www.cms.gov"
icd10 <- c(
  `2016` = "/Medicare/Coding/ICD10/Downloads/2016-Code-Descriptions-in-Tabular-Order.zip",
  `2017` = "/Medicare/Coding/ICD10/Downloads/2017-ICD10-Code-Descriptions.zip",
  `2018` = "/Medicare/Coding/ICD10/Downloads/2018-ICD-10-Code-Descriptions.zip",
  `2019` = "/Medicare/Coding/ICD10/Downloads/2019-ICD-10-CM-Code-Descriptions.zip",
  `2020` = "/Medicare/Coding/ICD10/Downloads/2020-ICD-10-CM-Codes.zip",
  `2021` = "/files/zip/2021-code-descriptions-tabular-order.zip",
  `2022` = "/files/zip/2022-code-descriptions-tabular-order.zip",
  `2023` = "/files/zip/2023-code-descriptions-tabular-order.zip",
  `2024` = "/files/zip/2024-code-descriptions-tabular-order.zip",
  `2025` = "/files/zip/2025-code-descriptions-tabular-order.zip")
icd9 <- c(
  `28` = "/medicare/coding/icd9providerdiagnosticcodes/downloads/cmsv28_master_descriptions.zip",
  `29` = "/medicare/coding/icd9providerdiagnosticcodes/downloads/cmsv29_master_descriptions.zip",
  `30` = "/medicare/coding/icd9providerdiagnosticcodes/downloads/cmsv30_master_descriptions.zip",
  `32` = "/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip")
# Releases before v28 ship short titles only; they fill in codes retired by v28.
icd9_short <- c(
  `23` = "I9DX_DESC", `24` = "I9diagnosis", `25` = "I9diagnosesV25",
  `26` = "Diagnosis", `27` = "DESC_SHORT_DX")

# Downloads one archive and returns the path of the member matching `pattern`.
fetch <- function(path, pattern) {
  zip <- file.path(dl, basename(path))
  if (!file.exists(zip)) utils::download.file(paste0(cms, path), zip, mode = "wb", quiet = TRUE)
  files <- utils::unzip(zip, list = TRUE)$Name
  member <- grep(pattern, files, value = TRUE)
  stopifnot(length(member) == 1)
  utils::unzip(zip, member, exdir = file.path(dl, sub("\\.zip$", "", basename(zip))))
}

# ICD-10-CM order file: fixed width; code in columns 7-13, billable flag in 15,
# long title from column 78. Header (non-billable) codes are kept, as in MIMIC.
read10 <- function(year) {
  x <- readLines(fetch(icd10[[year]], "icd10cm_order_[0-9]{4}\\.txt$"), encoding = "latin1")
  data.frame(icd_code = trimws(substr(x, 7, 13)), icd_version = 10L,
             long_title = trimws(substring(x, 78)), release = as.integer(year))
}
# ICD-9-CM long diagnosis file: code, one space, title.
read9 <- function(v) {
  x <- iconv(readLines(fetch(icd9[[v]], "_DESC_LONG_DX"), encoding = "latin1"), "latin1", "UTF-8")
  data.frame(icd_code = trimws(substr(x, 1, 5)), icd_version = 9L,
             long_title = trimws(substring(x, 7)), release = as.integer(v))
}

# Old short-title files: code, whitespace, upper-case title; sentence case as in MIMIC.
read9_short <- function(v) {
  path <- sprintf("/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v%s_icd9.zip", v)
  x <- trimws(iconv(readLines(fetch(path, icd9_short[[v]]), encoding = "latin1"), "latin1", "UTF-8"))
  t <- tolower(sub("^\\S+\\s+", "", x))
  data.frame(icd_code = sub("\\s.*", "", x), icd_version = 9L,
             long_title = paste0(toupper(substr(t, 1, 1)), substring(t, 2)), release = as.integer(v))
}

all <- rbind(do.call(rbind, lapply(names(icd9_short), read9_short)),
             do.call(rbind, lapply(names(icd9), read9)),
             do.call(rbind, lapply(names(icd10), read10)))
all <- all[nzchar(all$icd_code), ]
all <- all[order(all$icd_version, all$icd_code, -all$release, method = "radix"), ]
out <- all[!duplicated(all[c("icd_code", "icd_version")]), c("icd_code", "icd_version", "long_title")]
stopifnot(!anyNA(out), all(nzchar(out$long_title)))

# Quote only titles that need it, as in the MIMIC dictionary this replaces.
q <- grepl('[,"]', out$long_title)
out$long_title[q] <- paste0('"', gsub('"', '""', out$long_title[q]), '"')
utils::write.csv(out, "inst/extdata/d_icd_diagnoses.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
message(sum(out$icd_version == 9), " ICD-9-CM and ", sum(out$icd_version == 10), " ICD-10-CM codes written")

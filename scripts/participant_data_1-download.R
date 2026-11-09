# participant-level data for PerioSigDB studies that deposited sequencing reads with per-sample annotations
# (probing depth, age, sex), for looking at taxon abundance by participant characteristics
# used by scripts/participant_data_2-dada2.R
#
# accessions: from Europe PMC text-mined accessions, NCBI PubMed -> BioProject links, and the data
# availability statements in the study PDFs (data/articles_included) and open-access full texts (Oct 2026)
# reads go to data/participant_reads/, tables to output/participant_data/

library(dplyr)
library(tidyr)
library(purrr)
library(httr2)
library(xml2)
library(readr)

out_dir <- 'output/participant_data'
reads_dir <- 'data/participant_reads'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(reads_dir, showWarnings = FALSE, recursive = TRUE)

# every PerioSigDB study with a public read accession
accessions <- tribble(
  ~Study,                  ~accession,     ~note,
  'Study 484',             'PRJNA324274',  'probing depth category per site, age, sex',
  'Study 1411',            'SRA062091',    'probing depth (mm) per site, age, sex, diabetes',
  'Study 1141',            'PRJEB6047',    'age, sex; subgingival, supragingival and tongue',
  'Study 191',             'PRJNA726254',  'age, sex, smoking, BMI; includes treated periodontitis',
  'Study 428',             'PRJNA580506',  'age, sex (all female, 18-25 y); includes feces',
  'Study 639',             'PRJNA508385',  'shotgun; only 25 healthy non-smokers (aged 20-48) deposited, periodontitis samples not public',
  'Study 156',             'SRP009299',    'health status and individual only',
  'Study 1106',            'PRJNA269205',  'no participant annotations',
  'Study 1106',            'SRP012422',    'no participant annotations',
  'Study 123',             'SRP118852',    'no participant annotations',
  'Study 1446',            'SRA051864',    'no participant annotations',
  'Study 766',             'PRJNA274944',  'no participant annotations',
  'Study 260',             'PRJNA291567',  'no participant annotations',
  'Study 734',             'PRJNA255922',  'no participant annotations; shotgun',
  'Study 452',             'PRJNA660085',  'no participant annotations; RNA-seq',
  'Study 2025-PubMed-49',  'PRJEB61123',   'no participant annotations',
  'Study 2025-PubMed-130', 'PRJNA870274',  'health status only',
  'Study 2025-180',        'PRJNA910319',  'no participant annotations',
  'Study 2025-224',        'PRJNA1200033', 'no participant annotations',
  'Study 2025-231',        'PRJNA765566',  'no participant annotations; shotgun',
  'Study 2025-253',        'SRP033553',    'no participant annotations; shotgun',
  'Study 2025-274',        'PRJNA828047',  'no participant annotations',
  'Study 2025-Embase-55',  'PRJNA889010',  'not released in ENA'
)

# ---- runs and sample attributes ----

ena_runs <- function(accession) {
  fields <- c('run_accession', 'sample_accession', 'study_accession', 'library_strategy', 'library_layout',
              'instrument_model', 'read_count', 'fastq_ftp', 'fastq_md5')
  r <- request('https://www.ebi.ac.uk/ena/portal/api/filereport') |>
    req_url_query(accession = accession, result = 'read_run', fields = paste(fields, collapse = ',')) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform() |>
    resp_body_string()
  if (!nzchar(r)) return(NULL)
  read_tsv(I(r), col_types = cols(.default = 'c'))
}

ena_sample_attributes <- function(samples) {
  map_dfr(split(samples, ceiling(seq_along(samples) / 100)), \(ids) {
    x <- request(paste0('https://www.ebi.ac.uk/ena/browser/api/xml/', paste(ids, collapse = ','))) |>
      req_retry(max_tries = 3) |>
      req_perform() |>
      resp_body_xml()
    map_dfr(xml_find_all(x, '//SAMPLE'), \(s) {
      a <- xml_find_all(s, './/SAMPLE_ATTRIBUTE')
      tibble(sample_accession = xml_attr(s, 'accession'),
             tag = c('title', xml_text(xml_find_first(a, 'TAG'))),
             value = c(xml_text(xml_find_first(s, 'TITLE')), xml_text(xml_find_first(a, 'VALUE'))))
    })
  })
}

runs <- accessions |>
  mutate(runs = map(accession, ena_runs)) |>
  unnest(runs)

attributes <- ena_sample_attributes(unique(runs$sample_accession)) |>
  distinct(sample_accession, tag, .keep_all = TRUE)

write_csv(count(runs, Study, accession, library_strategy, name = 'runs') |>
            right_join(accessions, by = c('Study', 'accession')), file.path(out_dir, 'study_accessions.csv'))
write_csv(select(runs, -fastq_ftp, -fastq_md5), file.path(out_dir, 'runs.csv'))
write_csv(attributes, file.path(out_dir, 'sample_attributes.csv'))

# ---- harmonised participant metadata for the annotated 16S studies ----

wide <- \(study) {
  attributes |>
    semi_join(filter(runs, Study == study), by = 'sample_accession') |>
    pivot_wider(id_cols = sample_accession, names_from = tag, values_from = value) |>
    left_join(distinct(filter(runs, Study == study), Study, sample_accession, run_accession), by = 'sample_accession')
}

# Study 484 (Perez-Chaparro 2018): one subgingival sample per tooth, sites chosen by probing depth
# (shallow <= 3, intermediate 4-6, deep >= 7 mm; Methods); C = healthy, P = periodontitis participant
m484 <- wide('Study 484') |>
  transmute(Study, run_accession, sample_accession, subject = host_subject_id,
            status = ifelse(grepl('^C', host_subject_id), 'healthy', 'periodontitis'),
            site = 'subgingival plaque', tooth, pd_category = probing_depth, pd_mm = NA_real_,
            age = as.numeric(host_age), sex = host_sex)

# Study 1411 (Zhou 2013): pooled subgingival plaque, with the probing depth of the sampled sites;
# all participants Chinese adults, with or without type 2 diabetes
m1411 <- wide('Study 1411') |>
  transmute(Study, run_accession, sample_accession, subject = sample_accession,
            status = ifelse(`periodontitis-status` == 'P+', 'periodontitis', 'healthy'),
            site = 'subgingival plaque', pd_mm = as.numeric(`periodontal-depth`),
            age = as.numeric(age), sex, diabetes = `diabetes-status` == 'D+')

# Study 1141 (Galimanas 2014): subgingival, supragingival and tongue samples from each participant
m1141 <- wide('Study 1141') |>
  transmute(Study, run_accession, sample_accession, subject = `host subject id`,
            status = `disease status`,
            site = recode(SampleRegion, Subg = 'subgingival plaque', Supra = 'supragingival plaque', Tongue = 'tongue'),
            age = as.numeric(age), sex)

# Study 191: healthy, untreated and treated (after non-surgical therapy) chronic periodontitis
m191 <- wide('Study 191') |>
  transmute(Study, run_accession, sample_accession, subject = host_subject_id,
            status = recode(host_disease, `healthy periodontium` = 'healthy', `chronic periodontitis` = 'periodontitis',
                            `chronic periodontitis-treated` = 'periodontitis, treated'),
            site = 'subgingival plaque', age = as.numeric(host_age), sex = host_sex,
            smoker = Smoker == 'YES', bmi = as.numeric(host_body_mass_index))

# Study 428: molar-incisor pattern periodontitis (grade C) and controls; affected and healthy subgingival
# sites, supragingival plaque and feces
m428 <- wide('Study 428') |>
  transmute(Study, run_accession, sample_accession, subject = `Patient ID`,
            status = ifelse(Group == 'Control', 'healthy', 'periodontitis'),
            site = recode(Site, `Subgingival healthy` = 'subgingival plaque', `Subgingival affected` = 'subgingival plaque',
                          Supragingival = 'supragingival plaque', Feces = 'feces'),
            site_affected = Site == 'Subgingival affected', age = as.numeric(age), sex = Gender)

participants <- bind_rows(m484, m1411, m1141, m191, m428)
write_csv(participants, file.path(out_dir, 'sample_metadata.csv'))

# ---- reads ----

to_download <- runs |>
  filter(run_accession %in% participants$run_accession) |>
  separate_longer_delim(c(fastq_ftp, fastq_md5), ';') |>
  mutate(dest = file.path(reads_dir, Study, basename(fastq_ftp))) |>
  filter(!file.exists(dest))

walk(unique(dirname(to_download$dest)), dir.create, showWarnings = FALSE, recursive = TRUE)
# the EBI server often drops transfers part way, so retry until the checksum matches
pwalk(list(to_download$fastq_ftp, to_download$dest, to_download$fastq_md5), \(url, dest, md5) {
  for (try in 1:20) {
    system2('curl', c('-sSL', '-o', shQuote(dest), paste0('https://', url)), stderr = FALSE)
    if (file.exists(dest) && unname(tools::md5sum(dest)) == md5) return(invisible())
    unlink(dest)
  }
  stop('failed to download ', url)
})

# ---- HOMD 16S reference ----

homd_dir <- 'data/homd_refseq'
homd_url <- 'https://www.homd.org/ftp/16S_rRNA_refseq/HOMD_16S_rRNA_RefSeq/V16.03/'
dir.create(homd_dir, showWarnings = FALSE)
for (f in c('HOMD_16S_rRNA_RefSeq_V16.03.fasta', 'HOMD_16S_rRNA_RefSeq_V16.03.qiime.taxonomy')) {
  if (!file.exists(file.path(homd_dir, f))) download.file(paste0(homd_url, f), file.path(homd_dir, f), mode = 'wb')
}

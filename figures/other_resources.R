# Periodontitis papers and signatures in other microbe-disease resources, for Figures 3A and S3.
#
# Downloads each resource and writes two files to figures/data/: other_resources.csv (counts per resource,
# Figure 3A) and resource_papers.csv (one row per paper in each resource, PerioSigDB and BugSigDB included,
# with its PMID and whether PerioSigDB has it; Figure S3). Run from figures/:
#   Rscript other_resources.R
#
# A row counts as periodontal when its disease name contains "periodont", the rule the report uses for
# BugSigDB. A signature is the taxa one paper reports changed in one direction for one comparison:
#   MicrobiomeAnalyst  taxon sets in the TSEA libraries whose name contains "periodont"
#   Disbiome           associations grouped by paper, disease, direction, method, sample, host and control
#   dbBact             "diffexp" annotations with periodontitis on one side of the comparison; papers are
#                      dbBact experiments
#   Amadis             associations grouped by paper, disease and direction (the download holds fewer rows
#                      than the website, but a website search for "Periodontal Diseases" finds the same 8)
#
# A paper is in PerioSigDB when its PMID is. PerioSigDB here is output/perio_bugs_corrected.rds without the
# studies the full-text review proposes excluding, as in the report.

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(httr)
})

FIG_DIR <- normalizePath(".")
ROOT <- dirname(FIG_DIR)
PERIO <- regex("periodont", ignore_case = TRUE)

download <- function(url, ext) {
  f <- tempfile(fileext = ext)
  download.file(url, f, mode = "wb", quiet = TRUE)
  f
}

# Each resource returns its periodontal papers (paper: the resource's own ID; pmid) and its signature count.

microbiomeanalyst <- function() {
  libs <- c("host_snps_new", "host_int", "host_ext", "environment", "microbiome_int")
  sets <- map_dfr(libs, \(l) read_csv(download(sprintf(
    "https://www.microbiomeanalyst.ca/resources/libs/tsea/tsea_%s.csv", l), ".csv"),
    col_types = cols(.default = "c"))) |>
    filter(str_detect(name, PERIO)) |>
    distinct(name, member, .keep_all = TRUE) |>
    mutate(pmid = str_extract(reference, "(?<=pubmed/)\\d+"))
  list(papers = distinct(sets, paper = pmid, pmid), signatures = nrow(sets))
}

#' PMID of a DOI from PubMed, or NA unless PubMed finds exactly one.
pmid_from_doi <- function(doi) {
  Sys.sleep(0.4)  # NCBI allows three requests a second without a key
  ids <- fromJSON(modify_url("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
                             query = list(db = "pubmed", retmode = "json", term = paste0(doi, "[doi]"))))
  ids <- ids$esearchresult$idlist
  if (length(ids) == 1) ids else NA_character_
}

disbiome <- function() {
  d <- fromJSON("https://disbiome.ugent.be:8080/experiment") |>
    filter(str_detect(disease_name, PERIO))
  # most papers link to PubMed; the rest link to PMC or to a PubMed search, so look those up by DOI
  pubs <- fromJSON("https://disbiome.ugent.be:8080/publication") |>
    filter(publication_id %in% d$publication_id) |>
    transmute(paper = as.character(publication_id), doi,
              pmid = str_match(pubmed_url, "pubmed(?:\\.ncbi\\.nlm\\.nih\\.gov)?/(\\d+)")[, 2]) |>
    mutate(pmid = ifelse(is.na(pmid) & !is.na(doi), map_chr(doi, pmid_from_doi), pmid)) |>
    select(paper, pmid)
  if (anyNA(pubs$pmid)) stop("Disbiome papers without a PMID: ", paste(pubs$paper[is.na(pubs$pmid)], collapse = ", "))
  sigs <- distinct(d, publication_id, disease_name, qualitative_outcome, method_name, sample_name, host_type,
                   control_name)
  list(papers = pubs, signatures = nrow(sigs))
}

# dbBact records titles, DOIs and SRA projects, not PMIDs. These come from PubMed searches on the titles and
# DOIs (November 2026). Experiment 104 has no PubMed record: its BioProject (PRJNA256234) links to no paper.
DBBACT_PMIDS <- c(`96` = "28320468", `102` = "28328799", `104` = NA, `106` = "28298910", `162` = "28744486",
                  `278` = "29339824", `378` = "28871594", `542` = "30885927", `704` = "33051450",
                  `725` = "30915280", `955` = "34941818")

dbbact <- function() {
  # the "periodontitis" term also returns annotations on its child terms (chronic, aggressive)
  # the API labels its JSON as text/html, so parse the text
  r <- GET("https://api.dbbact.org/ontology/get_annotations", query = list(term = "periodontitis"))
  ann <- fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)$annotations
  contrast <- keep(ann, \(a) a$annotationtype == "diffexp" &&
                     some(a$details, \(t) t[[1]] %in% c("high", "low") &&
                            str_detect(t[[2]], regex("periodontitis", ignore_case = TRUE))))
  exps <- as.character(unique(map_int(contrast, "expid")))
  missing <- setdiff(exps, names(DBBACT_PMIDS))
  if (length(missing)) stop("dbBact experiments without a PMID lookup: ", paste(missing, collapse = ", "))
  list(papers = tibble(paper = exps, pmid = unname(DBBACT_PMIDS[exps])), signatures = length(contrast))
}

amadis <- function() {
  d <- readxl::read_excel(download("http://gift2disease.net/GIFTED/download/GIFTED.xlsx", ".xlsx")) |>
    filter(str_detect(Disease, PERIO))
  sigs <- distinct(d, PubmedID, Disease, `Association between disease and microflora`)
  list(papers = distinct(d, paper = as.character(PubmedID), pmid = as.character(PubmedID)),
       signatures = nrow(sigs))
}

periosigdb <- readRDS(file.path(ROOT, "output", "perio_bugs_corrected.rds")) |>
  filter(!grepl("^proposed exclusion", `Review status`)) |>
  distinct(paper = as.character(PMID), pmid = as.character(PMID))

# BugSigDB as the report reads it (the bugsigdbr cache), with the report's periodontitis rule
bugsigdb <- suppressMessages(suppressWarnings(bugsigdbr::importBugSigDB())) |>
  filter(str_detect(Condition, PERIO)) |>
  distinct(paper = as.character(PMID), pmid = as.character(PMID))

resources <- list(Disbiome = disbiome(), dbBact = dbbact(), MicrobiomeAnalyst = microbiomeanalyst(),
                  Amadis = amadis())

counts <- tibble(resource = names(resources), signatures = map_int(resources, "signatures"),
                 papers = map_int(resources, \(r) nrow(r$papers))) |>
  select(resource, papers, signatures)

papers <- c(list(PerioSigDB = periosigdb, BugSigDB = bugsigdb), map(resources, "papers")) |>
  bind_rows(.id = "resource") |>
  mutate(in_periosigdb = !is.na(pmid) & pmid %in% periosigdb$pmid)

print(counts)
write_csv(counts, file.path(FIG_DIR, "data", "other_resources.csv"))
write_csv(papers, file.path(FIG_DIR, "data", "resource_papers.csv"), na = "")

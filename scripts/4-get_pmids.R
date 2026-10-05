library(readxl)
library(rentrez)
library(dplyr)


studies_file <- 'data/articles_included/Final selection Yes.xlsx'
overview_cleaned <- readRDS('output/overview_cleaned.rds')

# additional PMIDs added
missing_pmids <- readr::read_csv('data/missing_pmids.csv', col_types = 'cc') |> distinct()

# PMIDs for 2022-2025 studies (searched by first author and year, checked against title/abstract)
new_data_pmids <- tribble(
  ~Number, ~PMID,
  '2025-Embase-55', '36519166',
  '2025-PubMed-14', '36792073',
  '2025-PubMed-49', '39365037',
  '2025-PubMed-55', '40329006',
  '2025-PubMed-130', '36077282',
  '2025-144', '39617812',
  '2025-180', '36844402',
  '2025-187', '38193290',
  '2025-224', '39788958',
  '2025-231', '40396735',
  '2025-234', '36076344',
  '2025-246', '36806930',
  '2025-253', '34252627',
  '2025-274', '35998186',
  '2025-PubMed-357', '40202358',
  '2025-PubMed-504', '40344212'
)


# check concordance of study Number
studies <- read_excel(studies_file)
studies <- studies |> 
  mutate(Number = gsub('[.]$', '', Number)) |> 
  mutate(Number = as.character(as.numeric(Number))) |> 
  select(Number, `Article nr`, `Article link`)

table(overview_cleaned$Number %in% c(studies$Number, missing_pmids$Number, new_data_pmids$Number))
setdiff(overview_cleaned$Number, c(studies$Number, missing_pmids$Number, new_data_pmids$Number))

is.pubmed <- grepl("pubmed", studies$`Article link`)
is.pmc <- grepl("PMC[0-9]+", studies$`Article link`)

# add pubmed ids that have
studies <- studies |>
  mutate(
    PMID = ifelse(
      is.pubmed & !is.pmc,
      sub(".*(?:/pubmed/|pubmed\\.ncbi\\.nlm\\.nih\\.gov/|[?&]term=)([0-9]+).*", "\\1", `Article link`),
      NA
    ),
    PMCID = ifelse(
      is.pmc,
      sub(".*(PMC[0-9]+).*", "\\1", `Article link`),
      NA
    )
  )

no.pubmed <- is.na(studies$PMID) & is.na (studies$PMCID)
table(no.pubmed)
studies$`Article link`[no.pubmed]

# manual fixes
studies <- studies |> 
  mutate(
    PMID = case_match(
      `Article link`,
      "http://www.drjjournal.net/article.asp?issn=1735-3327;year=2018;volume=15;issue=3;spage=185;epage=190;aulast=Mahalakshmi" ~ '29922337',
      "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1600-0765.2011.01455.x" ~ '22220967',
      "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1600-0722.2011.00875.x" ~ '22112031',
      "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1600-0722.2011.00808.x" ~ '21410554',
      "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1600-0722.2010.00765.x" ~ '20831580',
      .default = PMID
    )
  )

# get PMIDs from PMCIDs
pmcid_to_pmid <- function(pmcid) {
  res <- entrez_search(db = "pubmed", term = pmcid)
  res$ids
}

studies <- studies |> 
  rowwise() |>
  mutate(
    PMID = if (!is.na(PMCID) & is.na(PMID)) pmcid_to_pmid(PMCID) else PMID
  )

studies <- select(studies, Number, PMID)

stopifnot(sum(is.na(studies$PMID)) == 0)

# add previously missing pmids
studies <- bind_rows(studies, missing_pmids, new_data_pmids)

# add study info

records <- entrez_summary(db = "pubmed", id = studies$PMID)
records <- unname(records)
authors <- sapply(records, function(x) paste0(x$authors$name, collapse = ', '))
get_doi <- function(articleids) {
  doi <- articleids |> 
    filter(idtype=='doi') |>
    pull(value)
  
  if (!length(doi)) return(NA)
  
  return(doi)
}
dois <- sapply(records, function(x) get_doi(x$articleids))
titles <- sapply(records, `[[`, 'title')
journal <- sapply(records, `[[`, 'fulljournalname')
pubdates <- sapply(records, `[[`, 'pubdate')
pubyears <- gsub('^(\\d{4}).+?$', '\\1', pubdates)


studies$`Authors list` <- authors
studies$Title <- titles
studies$DOI <- dois
studies$Journal <- journal
studies$Year <- pubyears

saveRDS(studies, 'output/study_pmids.rds')

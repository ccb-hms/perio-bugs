library(readxl)
library(rentrez)

studies_file <- 'data/articles_included/Final selection Yes.xlsx'
overview_cleaned <- readRDS('output/overview_cleaned.rds')


# check concordance of study Number
studies <- read_excel(studies_file)
studies <- studies |> 
  mutate(Number = gsub('[.]$', '', Number)) |> 
  mutate(Number = as.numeric(Number)) |> 
  select(Number, `Article nr`, `Article link`)

table(perio_bugs$Number %in% studies$Number)

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

saveRDS(studies, 'output/study_pmids.rds')

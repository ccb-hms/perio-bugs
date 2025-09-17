library(bugsigdbr)
library(readr)
library(rentrez)

overview_file <- 'output/overview_merged.rds'

# Get all sheet names
df <- readRDS(overview_file)

# read in bugsigdb database for reference
bugsigdb <- importBugSigDB()

# common function to extract unique types from bugsigdb column
extract_unique <- function(vals) {
  unique(unlist(strsplit(vals, split = ",(?! )", perl = TRUE)))
}

# extract and cleanup all fields from Feres data that need for bugsigdb

# Study design ----
unique_designs <- extract_unique(bugsigdb$`Study design`)
unique_designs

# general cleanup
designs <- df$`Study design`
unique(designs)

# values that might need fixing
unique(designs[!designs %in% unique_designs])

designs <- tolower(designs)
designs <- trimws(gsub("\\s+", " ", designs))

# replace with bugsigdb values
designs[grepl('case', designs) & 
          grepl('control', designs)] <- "case-control"

designs[grepl('cross', designs) & 
          grepl('sectional', designs)] <- "cross-sectional observational, not case-control"

# TODO: clarify "cohort" and "non-randomized studies of interventions*"

# Location of subjects ----
unique_locs <- extract_unique(bugsigdb$`Location of subjects`)
unique_locs

locs <- df$Country

# values that might need fixing
unique(locs[!locs %in% unique_locs])

# replace with bugsigdb values
locs <- gsub(' and ', ',', locs)
locs <- gsub(', ', ',', locs)
locs[locs == 'Brasil'] <- 'Brazil'
locs <- gsub('USA', 'United States of America', locs)
locs[locs == 'United States'] <- 'United States of America'
locs[locs %in% c('Netherland', 'The Netherlands')] <- 'Netherlands'
locs[locs == 'UK'] <- 'United Kingdom'
locs[locs == 'Korea'] <- 'South Korea'
locs[locs == 'Russia'] <- 'Russian Federation'


# Body site ----
# assume all the same
# TODO: ask Feres group explicitly
body_site <- rep("Subgingival dental plaque", length(locs))

# Group names ----

# assume all the same
group0_name <- rep('periodontal health', length(locs))
group1_name <- rep('periodontitis', length(locs))

# Group 1 (Periodontits) definition ----
# NOTE: Group 0 definition not present in BugSigDB

group1_def <- df$`Criteria used to define periodontitis`

# Group sample size ----

# Group 0 (periodontal health) and 1 (periodontitis)
group0_size <- df$`Sample size (periodontal health)`
group1_size <- df$`Sample size (periodontitis)`

# TODO: figure out multi-entry values 
#  -> agressive, generalised aggressive, chronic, etc
#  -> sum them? split into separate signatures? keep one?

# Antibiotic exclusion ----
# TODO: collect?

# Sequencing type, region, and platform ----
unique_seq_types <- extract_unique(bugsigdb$`Sequencing type`)
unique_seq_types

unique_16s_regions <- unique(bugsigdb$`16S variable region`)
unique_16s_regions

unique_seq_plats <- unique(unlist(strsplit(bugsigdb$`Sequencing platform`, split = ",(?! )", perl = TRUE)))
unique_seq_plats

seq_type <- df$`Diagnostic Method`

# Following prompt to ChatGPT 4.1
#
# create a table that I can save as CSV. 
# The first column is the original values exactly as pasted here:
# 
# [seq_type]
#
# For subsequent columns, determine values based on the text in the first column.
# Use NA when the value cannot be determined or is not applicable.
#
# For the second column (Sequencing type) use one of the following values:
#
# [unique_seq_types]
#
# For the third column (16S variable region), the possible values are 1 to 9.
# Multiple regions should be pasted together in increasing order. 
# For example, if the second column is "16S" and the text mentions V4-V5, 
# the third column should have a value of "45".
#
# For the fourth column (Sequencing platform) these are the possible values:
# 
# [unique_seq_plats]

# read in produced CSV
gpt_seqs <- 
  read_csv("output/gpt_seqs.csv") |> 
  mutate(`Diagnostic Method` = seq_type, .before = 1)

# differences seem to be related to escape characters
table(gpt_seqs$Original == seq_type)

# inspect
View(gpt_seqs |> 
       mutate(correct = `Diagnostic Method` == Original))

# create output for collaborator to review
gpt_seqs |> 
  dplyr::select(-Original) |> 
  write_csv('output/gpt_seqs_to_validate.csv')

# get possible PMIDs ----
refs <- df$REFERENCE
years <- df$Year

pmids <- list()

for (i in seq_along(refs)) {
  cat('Working on', i, 'of', length(refs), '...')
  ref <- refs[i]
  year <- years[i]
  # remove instances of et al
  authors <- gsub("(,?\\s*et\\.?\\s*al\\.?\\,?\\s*)$", "", ref, ignore.case = TRUE)
  
  # split authors
  first_author <- strsplit(authors, split = ',')[[1]][1]
  
  # remove initials: patterns that are one or more spaces followed by caps and dots at the end
  first_author <- gsub("\\s+[A-Z\\.\\s]+$", "", first_author)
  
  # remove unicode whitespace
  first_author <- trimws(first_author, whitespace = "[\\h\\v]")
  
  # create query string
  query <- paste0(
    first_author, "[First Author] AND ",
    year, "[dp] AND ",
    "periodontitis"
  )
  
  res <- entrez_search(db = "pubmed", term = query, retmax = 10)
  cat('Found', length(res$ids), 'ids.\n')
  
  if (length(res$ids)) 
    pmids[[i]] <- res$ids
}

# create table for chat GPT to pick best results of
gpt_pmids <- df[, c('REFERENCE', 'Year')] |>
  mutate(REF_NUM = seq_len(n())) |> 
  mutate(
    PMID = purrr::map(REF_NUM, ~ pmids[[.x]])
  ) |> 
  tidyr::unnest_longer(
    PMID, 
    values_to = "PMID", 
    keep_empty = TRUE
  )

# get records for PMIDs
na.pmid <- is.na(gpt_pmids$PMID)
records <- entrez_summary(db = "pubmed", id = gpt_pmids$PMID[!na.pmid])

# TODO: add records details, get ChatGPT to select most likely, ask colaborators
# to validate

# Other columns that still need ----
# Statistical test
# Significance threshold	
# MHT correction
# LDA Score above
# Matched on
# Confounders controlled for
# Pielou
# Shannon
# Chao1
# Simpson
# Inverse Simpson
# Richness
# Source
# Description
# Abundance in Group 1

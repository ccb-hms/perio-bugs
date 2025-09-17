library(readxl)
library(bugsigdbr)

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

library(bugsigdbr)
library(readr)
library(rentrez)
library(dplyr)
library(tibble)

overview_file <- 'output/overview_merged.rds'

# load in LLM prompted fixes
prompt_fixes_file <- 'output/prompt_fixes.rds'

if (file.exists(prompt_fixes_file)) {
  prompt_fixes <- readRDS(prompt_fixes_file)
} else {
  prompt_fixes <- list()
}


# Get all sheet names
df <- readRDS(overview_file)

# read in bugsigdb database for reference
bugsigdb <- importBugSigDB()

# common function to extract unique types from bugsigdb column
extract_unique <- function(vals) {
  unique(unlist(strsplit(vals, split = ",(?! )", perl = TRUE)))
}

# controlled vocabs from bugsigdb
source('scripts/controlled_vocab.R')

# extract and cleanup all fields from Feres data that need for bugsigdb

# Study design ----
reference_designs <- controlled_vocab$study_design
reference_designs

# general cleanup
designs <- df$`Study design`
unique(designs)

# values that might need fixing
unique(designs[!designs %in% reference_designs])

designs <- tolower(designs)
designs <- trimws(gsub("\\s+", " ", designs))

# replace with bugsigdb values
designs[grepl('case', designs) & 
          grepl('control', designs)] <- "case-control"

designs[grepl('cross', designs) & 
          grepl('sectional', designs)] <- "cross-sectional observational, not case-control"

designs[grepl('^cohort', designs)] <- "prospective cohort"
designs[grepl('^non-randomized studies of interventions', designs)] <- "prospective cohort"

# ensure everything fits
stopifnot(all(designs %in% reference_designs))

# Location of subjects ----

# controlled terms for location
reference_locs <- controlled_vocab$location

# actual location values 
locs <- df$Country

# values that might need fixing
unique_locs <- extract_unique(locs)
unique_locs[!unique_locs %in% reference_locs]

# replace with reference values
locs <- gsub(' and ', ',', locs)
locs <- gsub(', ', ',', locs)
locs[locs == 'Brasil'] <- 'Brazil'
locs <- gsub('USA', 'United States of America', locs)
locs[locs == 'United States'] <- 'United States of America'
locs[locs %in% c('Netherland', 'The Netherlands')] <- 'Netherlands'
locs[locs == 'UK'] <- 'United Kingdom'
locs[locs == 'Korea'] <- 'South Korea'
locs[locs == 'Russia'] <- 'Russian Federation'
locs[locs == 'Czech Republic'] <- 'Czechia'

# really?
locs[locs == 'Hong Kong'] <- 'China'


stopifnot(all(extract_unique(locs) %in% reference_locs))

# Body site ----
# assume all the same
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
group0_size_messy <- tibble(
  messy_num = df$`Sample size (periodontal health)`
)

group1_size_messy <- tibble(
  messy_num = df$`Sample size (periodontitis)`
)

# prompt if don't have
if (!check_prev_prompt(group0_size_messy, 'group0_size', prompt_fixes)) {
  
  prompt_fixes$group0_size <- run_sum_group_size_prompt(group0_size_messy)
  prompt_fixes$group1_size <- run_sum_group_size_prompt(group1_size_messy)
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# Antibiotic exclusion ----
# TODO: collect?

# Sequencing type, region, and platform ----
ref_seq_types <- controlled_vocab$sequencing_type
ref_16s_regions <- controlled_vocab$variable_region_16S
ref_seq_plats <- controlled_vocab$sequencing_platform

method <- df$`Diagnostic Method`

if (!check_prev_prompt(data.table(method), 'seq_res', prompt_fixes)) {
  
  prompt_fixes$seq_res <- run_diagnostic_method_prompt(
    method, ref_seq_types, ref_16s_regions, ref_seq_plats)
  
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# get possible PMIDs ----

# skip if already have
if (!file.exists('output/possible_pmids.csv')) {
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
  
  # create table to pick best results of
  possible_pmids <- df[, c('REFERENCE', 'Year')] |>
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
  na.pmid <- is.na(possible_pmids$PMID)
  records <- entrez_summary(db = "pubmed", id = possible_pmids$PMID[!na.pmid])
  
  # add records details
  record_df <- tibble(
    PMID = names(records),
    authors = sapply(records, function(rec) paste(rec$authors$name, collapse=', ')),
    journal = sapply(records, `[[`, 'fulljournalname'),
    title = sapply(records, `[[`, 'title')
  ) |> 
    distinct() |> 
    mutate(URL = paste0("https://pubmed.ncbi.nlm.nih.gov/", PMID))
  
  possible_pmids <- left_join(possible_pmids, record_df)
  write.csv(possible_pmids, 'output/possible_pmids.csv')
}


# chatGPT based cleanup of messy columns ----

# get free API key here: https://aistudio.google.com/
# Sys.setenv(GOOGLE_API_KEY = 'YOUR_API_KEY_HERE')

source('scripts/run_prompt.R')

# age
View(data.frame(original = df$`Age (years; mean +-SD)`, 
                age_mean, age_sd))


View(data.frame(original_mean = df$`Age mean (periodontal health)`, 
                original_sd = df$`Age SD (periodontal health)`,
                age_mean_health, age_sd_health))

View(data.frame(original_mean = df$`Age mean (periodontitis)`, 
                original_sd = df$`Age SD (periodontitis)`,
                age_mean_perio, age_sd_perio))

# number and percent male ----

# overall (both groups)
males_overall_messy <- tibble(
  messy_num = df$`Males (n,%)`
)

males_overall <- run_num_percent_prompt(males_overall_messy)

# healthy group
males_health_messy <- tibble(
  messy_num = df$`Males (n,%) (periodontal health)`,
  messy_percent = df$`Males % (periodontal health)`
)

males_health <- run_num_percent_prompt(males_health_messy)

# perio group
males_perio_messy <- tibble(
  messy_num = df$`Males (n,%) (periodontitis)`,
  messy_percent = df$`Males % (periodontitis)`
)

males_perio <- run_num_percent_prompt(males_perio_messy)


# number and percent smokers ----

# overall (both groups)
smokers_overall_messy <- tibble(
  messy_num = df$`Smokers (n,%)`
)

smokers_overall <- run_num_percent_prompt(smokers_overall_messy)

# healthy group
smokers_health_messy <- tibble(
  messy_num = df$`Smokers (n,%) (periodontal health)`,
  messy_percent = df$`Smokers (%) (periodontal health)`
)

smokers_health <- run_num_percent_prompt(smokers_health_messy)

# perio group
smokers_perio_messy <- tibble(
  messy_num = df$`Smokers (n,%) (periodontitis)`,
  messy_percent = df$`Smokers (%) (periodontitis)`
)

smokers_perio <- run_num_percent_prompt(smokers_perio_messy)


# bleeding on probing ----

# healthy group
bop_health_messy <- tibble(
  messy_percent = df$`Bleeding on probing (periodontal health)`,
  messy_sd = df$`Bleeding on probing (SD) (periodontal health)`
)

bop_health <- run_percent_sd_prompt(bop_health_messy, focus = 'bleeding on probing (BOP)')


# perio group
bop_perio_messy <- tibble(
  messy_percent = df$`Bleeding on probing (periodontitis)`,
  messy_sd = df$`Bleeding on probing (SD) (periodontitis)`
)

bop_perio <- run_percent_sd_prompt(bop_perio_messy, focus = 'bleeding on probing (BOP)')


# supporation  -----
# healthy group
supp_health_messy <- tibble(
  messy_percent = df$`Suppuration (periodontal health)`,
  messy_sd = df$`Suppuration (SD) (periodontal health)`
)

supp_health <- run_percent_sd_prompt(supp_health_messy, focus = 'suppuration (SUP)')


# perio group
supp_perio_messy <- tibble(
  messy_percent = df$`Suppuration (periodontitis)`,
  messy_sd = df$`Suppuration (SD) (periodontitis)`
)

supp_perio <- run_percent_sd_prompt(supp_perio_messy, focus = 'suppuration (SUP)')



# join cleaned up columns ----

cleaned_df <- tibble::tibble(
  # use bugsigdb column names
  "Study design" = designs,
  "Location of subjects" = locs,
  "Host species" = "Homo sapiens",
  "Body site" = body_site,
  "Condition" = "Periodontitis",
  "Group 0 name" = group0_name,
  "Group 1 name" = group1_name,
  "Group 1 definition" = group1_def,
  "Group 0 sample size" = group0_size,
  "Group 1 sample size" = group1_size_summed,
  "Sequencing type" = gpt_seqs$`Sequencing type`,
  "16S variable region" = gpt_seqs$`16S variable region`,
  "Sequencing platform" = gpt_seqs$`Sequencing platform`,
  
  # periodontits specific columns
  "Group 0 age mean" =  age_mean_health,
  "Group 0 age sd" =  age_sd_health,
  "Group 1 age mean" =  age_mean_perio,
  "Group 1 age sd" =  age_sd_perio,
  "Group 0 num males" =  males_num_health,
  "Group 1 num males" =  males_num_perio,
  "Group 0 percent males" =  males_percent_health,
  "Group 1 percent males" =  males_percent_perio,
  "Group 0 num smokers" =  smokers_num_health,
  "Group 1 num smokers" =  smokers_num_perio,
  "Group 0 percent smokers" =  smokers_percent_health,
  "Group 1 percent smokers" =  smokers_percent_perio,
  "Group 0 bleeding on probing percent" =  bop_percent_health,
  "Group 0 bleeding on probing SD" =  bop_sd_health,
  "Group 1 bleeding on probing percent" =  bop_percent_perio,
  "Group 1 bleeding on probing SD" =  bop_sd_perio,
  "Group 0 suppuration percent" =  supp_percent_health,
  "Group 0 suppuration SD" =  supp_sd_health,
  "Group 1 suppuration percent" =  supp_percent_perio,
  "Group 1 suppuration SD" =  supp_sd_perio,
  
  # only pocket depth for periodontitis group
  "Group 1 pocket depth mean (mm)" =  pd_mean_perio,
  "Group 1 pocket depth SD" =  pd_sd_perio,
  "Group 0 pocket depth mean (mm)" =  pd_mean_health,
  "Group 0 pocket depth SD" =  pd_sd_health,
  
  "Group 0 clinical attachment loss (mm)" =  cal_mean_health,
  "Group 0 clinical attachment loss SD" =  cal_sd_health,
  "Group 1 clinical attachment loss (mm)" =  cal_mean_perio,
  "Group 1 clinical attachment loss SD" =  cal_sd_perio,
  
  "Group 0 plaque" =  plaque_mean_health,
  "Group 0 plaque SD" =  plaque_sd_health,
  "Group 1 plaque" =  plaque_mean_perio,
  "Group 1 plaque SD" =  plaque_sd_perio,
)

# load in microbe data ----

diff_species <- readRDS('output/diff_species.rds')

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
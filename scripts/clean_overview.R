library(bugsigdbr)
library(readr)
library(rentrez)
library(dplyr)
library(tibble)
source('scripts/run_prompt.R')

# get free API key here: https://aistudio.google.com/
# Sys.setenv(GOOGLE_API_KEY = 'YOUR_API_KEY_HERE')

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
  
  prompt_fixes$group0_size <- run_generic_prompt(group0_size_messy, 'sum_group_size')
  prompt_fixes$group1_size <- run_generic_prompt(group1_size_messy, 'sum_group_size')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# Antibiotic exclusion ----
# TODO: collect?

# Sequencing type, region, and platform ----

dirty_seq <- tibble(
  method = df$`Diagnostic Method`
)

if (!check_prev_prompt(dirty_seq, 'seq_res', prompt_fixes)) {
  
  prompt_fixes$seq_res <- run_generic_prompt(
    dirty_seq,
    prompt_name = 'diagnostic_method'
  )
  
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


# TODO: convert age to prompt ----


# overall age
age_overall_messy <- tibble(
  messy_num = df$`Age (years; mean +-SD)`
)

if (!check_prev_prompt(age_overall_messy, 'age_overall', prompt_fixes)) {
  
  prompt_fixes$age_overall <- run_generic_prompt(age_overall_messy, 'age_overall')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# healthy group age
age_health_messy <- tibble(
  messy_num = df$`Age mean (periodontal health)`,
  messy_sd = df$`Age SD (periodontal health)`
)

if (!check_prev_prompt(age_health_messy, 'age_health', prompt_fixes)) {
  
  prompt_fixes$age_health <- run_generic_prompt(age_health_messy, 'age_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# perio group age

age_perio_messy <- tibble(
  messy_num = df$`Age mean (periodontitis)`,
  messy_sd = df$`Age SD (periodontitis)`
)

if (!check_prev_prompt(age_perio_messy, 'age_perio', prompt_fixes)) {
  
  prompt_fixes$age_perio <- run_generic_prompt(age_perio_messy, 'age_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}
# number and percent male ----

# overall (both groups)
males_overall_messy <- tibble(
  messy_num = df$`Males (n,%)`
)

if (!check_prev_prompt(males_overall_messy, 'males_overall', prompt_fixes)) {
  
  prompt_fixes$males_overall <- run_generic_prompt(males_overall_messy, 'males_overall')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# healthy group
males_health_messy <- tibble(
  messy_num = df$`Males (n,%) (periodontal health)`,
  messy_percent = df$`Males % (periodontal health)`
)

if (!check_prev_prompt(males_health_messy, 'males_health', prompt_fixes)) {
  
  prompt_fixes$males_health <- run_generic_prompt(males_health_messy, 'males_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
males_perio_messy <- tibble(
  messy_num = df$`Males (n,%) (periodontitis)`,
  messy_percent = df$`Males % (periodontitis)`
)

if (!check_prev_prompt(males_perio_messy, 'males_perio', prompt_fixes)) {
  
  prompt_fixes$males_perio <- run_generic_prompt(males_perio_messy, 'males_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# number and percent smokers ----

# overall (both groups)
smokers_overall_messy <- tibble(
  messy_num = df$`Smokers (n,%)`
)

if (!check_prev_prompt(smokers_overall_messy, 'smokers_overall', prompt_fixes)) {
  
  prompt_fixes$smokers_overall <- run_generic_prompt(smokers_overall_messy, 'smokers_overall')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# healthy group
smokers_health_messy <- tibble(
  messy_num = df$`Smokers (n,%) (periodontal health)`,
  messy_percent = df$`Smokers (%) (periodontal health)`
)

if (!check_prev_prompt(smokers_health_messy, 'smokers_health', prompt_fixes)) {
  
  prompt_fixes$smokers_health <- run_generic_prompt(smokers_health_messy, 'smokers_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
smokers_perio_messy <- tibble(
  messy_num = df$`Smokers (n,%) (periodontitis)`,
  messy_percent = df$`Smokers (%) (periodontitis)`
)

if (!check_prev_prompt(smokers_perio_messy, 'smokers_perio', prompt_fixes)) {
  
  prompt_fixes$smokers_perio <- run_generic_prompt(smokers_perio_messy, 'smokers_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# bleeding on probing ----

# healthy group
bop_health_messy <- tibble(
  messy_percent = df$`Bleeding on probing (periodontal health)`,
  messy_sd = df$`Bleeding on probing (SD) (periodontal health)`
)

if (!check_prev_prompt(bop_health_messy, 'bop_health', prompt_fixes)) {
  
  prompt_fixes$bop_health <- run_generic_prompt(bop_health_messy, 'bop_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# perio group
bop_perio_messy <- tibble(
  messy_percent = df$`Bleeding on probing (periodontitis)`,
  messy_sd = df$`Bleeding on probing (SD) (periodontitis)`
)

if (!check_prev_prompt(bop_perio_messy, 'bop_perio', prompt_fixes)) {
  
  prompt_fixes$bop_perio <- run_generic_prompt(bop_perio_messy, 'bop_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# supporation  -----
# healthy group
supp_health_messy <- tibble(
  messy_percent = df$`Suppuration (periodontal health)`,
  messy_sd = df$`Suppuration (SD) (periodontal health)`
)

if (!check_prev_prompt(supp_health_messy, 'supp_health', prompt_fixes)) {
  
  prompt_fixes$supp_health <- run_generic_prompt(supp_health_messy, 'supp_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
supp_perio_messy <- tibble(
  messy_percent = df$`Suppuration (periodontitis)`,
  messy_sd = df$`Suppuration (SD) (periodontitis)`
)

if (!check_prev_prompt(supp_perio_messy, 'supp_perio', prompt_fixes)) {
  
  prompt_fixes$supp_perio <- run_generic_prompt(supp_perio_messy, 'supp_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# pocket depth  -----

# healthy group
pd_health_messy <- tibble(
  messy_num = df$`PD (periodontal health)`,
  messy_num2 = df$`PD OG (periodontal health)`,
  messy_sd = df$`PD SD (periodontal health)`
)

if (!check_prev_prompt(pd_health_messy, 'pd_health', prompt_fixes)) {
  
  prompt_fixes$pd_health <- run_generic_prompt(pd_health_messy, 'pd_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
pd_perio_messy <- tibble(
  messy_num = df$`PD (periodontitis)`,
  messy_sd = df$`PD SD (periodontitis)`
)

if (!check_prev_prompt(pd_perio_messy, 'pd_perio', prompt_fixes)) {
  
  prompt_fixes$pd_perio <- run_generic_prompt(pd_perio_messy, 'pd_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# clinical attachment loss  -----

# healthy group
cal_health_messy <- tibble(
  messy_num = df$`Clinical attachment loss/level (mm; mean+-SD) (periodontal health)`,
  messy_sd = df$`CAL. SD (periodontal health)`
)

if (!check_prev_prompt(cal_health_messy, 'cal_health', prompt_fixes)) {
  
  prompt_fixes$cal_health <- run_generic_prompt(cal_health_messy, 'cal_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
cal_perio_messy <- tibble(
  messy_num = df$`Clinical attachment loss/level (mm; mean+-SD) (periodontitis)`,
  messy_sd = df$`CAL. SD (periodontitis)`
)

if (!check_prev_prompt(cal_perio_messy, 'cal_perio', prompt_fixes)) {
  
  prompt_fixes$cal_perio <- run_generic_prompt(cal_perio_messy, 'cal_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# plaque  -----

# healthy group
plaque_health_messy <- tibble(
  messy_num = df$`Plaque (mean +/SD) (periodontal health)`,
  messy_sd = df$`Plaque (SD) (periodontal health)`
)

if (!check_prev_prompt(plaque_health_messy, 'plaque_health', prompt_fixes)) {
  
  prompt_fixes$plaque_health <- run_generic_prompt(plaque_health_messy, 'plaque_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
plaque_perio_messy <- tibble(
  messy_num = df$`Plaque (mean +/SD) (periodontitis)`,
  messy_sd = df$`Plaque (SD) (periodontitis)`
)

if (!check_prev_prompt(plaque_perio_messy, 'plaque_perio', prompt_fixes)) {
  
  prompt_fixes$plaque_perio <- run_generic_prompt(plaque_perio_messy, 'plaque_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

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
  "Group 0 sample size" = prompt_fixes$group0_size$clean_num,
  "Group 1 sample size" = prompt_fixes$group1_size$clean_num,
  
  "Sequencing type" = prompt_fixes$seq_res$seq_type,
  "16S variable region" = prompt_fixes$seq_res$`16s_regions`,
  "Sequencing platform" = prompt_fixes$seq_res$seq_plat,
  
  "Group 0 age mean" =  prompt_fixes$age_health$clean_num,
  "Group 0 age sd" =  prompt_fixes$age_health$clean_sd,
  "Group 1 age mean" =  prompt_fixes$age_perio$clean_num,
  "Group 1 age sd" =  prompt_fixes$age_perio$clean_sd,
  
  "Group 0 num males" =  prompt_fixes$males_health$clean_num,
  "Group 1 num males" =  prompt_fixes$males_perio$clean_num,
  "Group 0 percent males" =  prompt_fixes$males_health$clean_percent,
  "Group 1 percent males" =  prompt_fixes$males_perio$clean_percent,
  
  "Group 0 num smokers" =  prompt_fixes$smokers_health$clean_num,
  "Group 1 num smokers" =  prompt_fixes$smokers_perio$clean_num,
  "Group 0 percent smokers" =  prompt_fixes$smokers_health$clean_percent,
  "Group 1 percent smokers" =  prompt_fixes$smokers_perio$clean_percent,
  
  "Group 0 bleeding on probing percent" =  prompt_fixes$bop_health$clean_percent,
  "Group 0 bleeding on probing SD" =  prompt_fixes$bop_health$clean_sd,
  "Group 1 bleeding on probing percent" =  prompt_fixes$bop_perio$clean_percent,
  "Group 1 bleeding on probing SD" =  prompt_fixes$bop_perio$clean_sd,
  
  "Group 0 suppuration percent" =  prompt_fixes$supp_health$clean_percent,
  "Group 0 suppuration SD" =  prompt_fixes$supp_health$clean_sd,
  "Group 1 suppuration percent" =  prompt_fixes$supp_perio$clean_percent,
  "Group 1 suppuration SD" =  prompt_fixes$supp_perio$clean_sd,
  
  "Group 1 pocket depth mean (mm)" =  prompt_fixes$pd_perio$clean_num,
  "Group 1 pocket depth SD" =  prompt_fixes$pd_perio$clean_sd,
  "Group 0 pocket depth mean (mm)" =  prompt_fixes$pd_health$clean_num,
  "Group 0 pocket depth SD" =  prompt_fixes$pd_health$clean_sd,

  "Group 0 clinical attachment loss (mm)" =  prompt_fixes$cal_health$clean_num,
  "Group 0 clinical attachment loss SD" =  prompt_fixes$cal_health$clean_sd,
  "Group 1 clinical attachment loss (mm)" =  prompt_fixes$cal_perio$clean_num,
  "Group 1 clinical attachment loss SD" =  prompt_fixes$cal_perio$clean_sd,

  "Group 0 plaque" =  prompt_fixes$plaque_health$clean_num,
  "Group 0 plaque SD" =  prompt_fixes$plaque_health$clean_sd,
  "Group 1 plaque" =  prompt_fixes$plaque_perio$clean_num,
  "Group 1 plaque SD" =  prompt_fixes$plaque_perio$clean_sd
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
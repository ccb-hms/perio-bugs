library(readr)
library(rentrez)
library(dplyr)
library(tibble)

# functions to run prompts
source('scripts/run_prompt.R')

# controlled vocabs from bugsigdb
source('scripts/controlled_vocab.R')

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

# manually cleaned values for 2022-2025 studies (used instead of LLM prompts)
source('scripts/manual_fixes_new_data.R')

# append manually cleaned values for rows not in previous prompt results
add_manual_fixes <- function(messy_data, check_name, prompt_fixes) {
  prev <- prompt_fixes[[check_name]]
  if (is.null(prev) || nrow(prev) >= nrow(messy_data)) return(prompt_fixes)
  
  new_idx <- seq(nrow(prev) + 1, nrow(messy_data))
  manual <- manual_fixes[[check_name]]
  manual <- manual[match(df$Number[new_idx], manual$Number), ]
  stopifnot(identical(manual$Number, df$Number[new_idx]))
  
  new_rows <- bind_cols(messy_data[new_idx, ], select(manual, -Number))
  
  # match column types of previous results
  for (col in names(new_rows)) {
    prev_col <- prev[[col]]
    new_rows[[col]] <- if (is.factor(prev_col)) {
      factor(new_rows[[col]], levels = union(levels(prev_col), new_rows[[col]]))
    } else {
      as(new_rows[[col]], class(prev_col))
    }
  }
  
  prompt_fixes[[check_name]] <- bind_rows(prev, new_rows)
  return(prompt_fixes)
}

# common function to extract unique types from bugsigdb column
extract_unique <- function(vals) {
  unique(unlist(strsplit(vals, split = ",(?! )", perl = TRUE)))
}

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

# e.g. 'Belgium; Chile; Peru; Spain (countries of origin of the subgingival samples)...'
locs <- gsub(' \\(.*$', '', locs)
locs <- gsub('; ?', ',', locs)

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
group0_def <- df$`Criteria used to define periodontal health`

# Group sample size ----

# Group 0 (periodontal health) and 1 (periodontitis)
group0_size_messy <- tibble(
  messy_num = df$`Sample size (periodontal health)`
)

group1_size_messy <- tibble(
  messy_num = df$`Sample size (periodontitis)`
)

# prompt if don't have
prompt_fixes <- add_manual_fixes(group0_size_messy, 'group0_size', prompt_fixes)
prompt_fixes <- add_manual_fixes(group1_size_messy, 'group1_size', prompt_fixes)
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

prompt_fixes <- add_manual_fixes(dirty_seq, 'seq_res', prompt_fixes)
if (!check_prev_prompt(dirty_seq, 'seq_res', prompt_fixes)) {
  
  prompt_fixes$seq_res <- run_generic_prompt(
    dirty_seq,
    prompt_name = 'diagnostic_method'
  )
  
  saveRDS(prompt_fixes, prompt_fixes_file)
}

if (!check_prev_prompt(dirty_seq, 'seq_res_cleaned', prompt_fixes)) {
  
  # cleanup some inconsistencies
  broad_platforms <- c("Illumina", "Roche454", "DNA-DNA Hybridization", "Human Intestinal Tract Chip")
  targeted_platforms <- c("RT-qPCR", "Sanger", "Non-quantitative PCR", "Mass spectrometry")
  
  seq_res <- prompt_fixes$seq_res
  seq_res$Number <- df$Number
  
  # inspect
  if (interactive()) {
    seq_res |> 
      filter(
        (seq_type %in% c("16S", "WMS") & seq_plat %in% targeted_platforms) |
          (seq_type == "PCR" & seq_plat %in% broad_platforms)
      ) |> 
      View()
  }
  
  # cleanup
  prompt_fixes$seq_res_cleaned <- seq_res |> 
    mutate(
      seq_type = case_when(
        # labelled 16S but method describes targeted species-specific PCR only
        Number %in% c(124, 1713, 127) ~ factor("PCR", levels = levels(seq_type)),
        
        # labelled PCR but method describes Pyrosequencing = broad 16S amplicon
        Number == 260 ~ factor("16S", levels = levels(seq_type)),
        
        # method describes both broad 16S sequencing and targeted qPCR
        Number %in% c(459, 428, 452, 509, 549) ~ factor("16S,PCR", levels = c(levels(seq_type), "16S,PCR")),
        .default = seq_type
      ),
      seq_plat = case_when(
        # labelled PCR but Pyrosequencing is the Roche454 platform
        Number == 260 ~ factor("Roche454", levels = levels(seq_plat)),
        .default = seq_plat
      )
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

# overall age
age_overall_messy <- tibble(
  messy_num = df$`Age (years; mean +-SD)`
)

prompt_fixes <- add_manual_fixes(age_overall_messy, 'age_overall', prompt_fixes)
if (!check_prev_prompt(age_overall_messy, 'age_overall', prompt_fixes)) {
  
  prompt_fixes$age_overall <- run_generic_prompt(age_overall_messy, 'age_overall')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# healthy group age
age_health_messy <- tibble(
  messy_num = df$`Age mean (periodontal health)`,
  messy_sd = df$`Age SD (periodontal health)`
)

prompt_fixes <- add_manual_fixes(age_health_messy, 'age_health', prompt_fixes)
if (!check_prev_prompt(age_health_messy, 'age_health', prompt_fixes)) {
  
  prompt_fixes$age_health <- run_generic_prompt(age_health_messy, 'age_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# perio group age

age_perio_messy <- tibble(
  messy_num = df$`Age mean (periodontitis)`,
  messy_sd = df$`Age SD (periodontitis)`
)

prompt_fixes <- add_manual_fixes(age_perio_messy, 'age_perio', prompt_fixes)
if (!check_prev_prompt(age_perio_messy, 'age_perio', prompt_fixes)) {
  
  prompt_fixes$age_perio <- run_generic_prompt(age_perio_messy, 'age_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}
# number and percent male ----

# overall (both groups)
males_overall_messy <- tibble(
  messy_num = df$`Males (n,%)`
)

prompt_fixes <- add_manual_fixes(males_overall_messy, 'males_overall', prompt_fixes)
if (!check_prev_prompt(males_overall_messy, 'males_overall', prompt_fixes)) {
  
  prompt_fixes$males_overall <- run_generic_prompt(males_overall_messy, 'males_overall')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# healthy group
males_health_messy <- tibble(
  messy_num = df$`Males (n,%) (periodontal health)`,
  messy_percent = df$`Males % (periodontal health)`
)

prompt_fixes <- add_manual_fixes(males_health_messy, 'males_health', prompt_fixes)
if (!check_prev_prompt(males_health_messy, 'males_health', prompt_fixes)) {
  
  prompt_fixes$males_health <- run_generic_prompt(males_health_messy, 'males_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
males_perio_messy <- tibble(
  messy_num = df$`Males (n,%) (periodontitis)`,
  messy_percent = df$`Males % (periodontitis)`
)

prompt_fixes <- add_manual_fixes(males_perio_messy, 'males_perio', prompt_fixes)
if (!check_prev_prompt(males_perio_messy, 'males_perio', prompt_fixes)) {
  
  prompt_fixes$males_perio <- run_generic_prompt(males_perio_messy, 'males_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# number and percent smokers ----

# overall (both groups)
smokers_overall_messy <- tibble(
  messy_num = df$`Smokers (n,%)`
)

prompt_fixes <- add_manual_fixes(smokers_overall_messy, 'smokers_overall', prompt_fixes)
if (!check_prev_prompt(smokers_overall_messy, 'smokers_overall', prompt_fixes)) {
  
  prompt_fixes$smokers_overall <- run_generic_prompt(smokers_overall_messy, 'smokers_overall')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# healthy group
smokers_health_messy <- tibble(
  messy_num = df$`Smokers (n,%) (periodontal health)`,
  messy_percent = df$`Smokers (%) (periodontal health)`
)

prompt_fixes <- add_manual_fixes(smokers_health_messy, 'smokers_health', prompt_fixes)
if (!check_prev_prompt(smokers_health_messy, 'smokers_health', prompt_fixes)) {
  
  prompt_fixes$smokers_health <- run_generic_prompt(smokers_health_messy, 'smokers_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
smokers_perio_messy <- tibble(
  messy_num = df$`Smokers (n,%) (periodontitis)`,
  messy_percent = df$`Smokers (%) (periodontitis)`
)

prompt_fixes <- add_manual_fixes(smokers_perio_messy, 'smokers_perio', prompt_fixes)
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

prompt_fixes <- add_manual_fixes(bop_health_messy, 'bop_health', prompt_fixes)
if (!check_prev_prompt(bop_health_messy, 'bop_health', prompt_fixes)) {
  
  prompt_fixes$bop_health <- run_generic_prompt(bop_health_messy, 'bop_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}


# perio group
bop_perio_messy <- tibble(
  messy_percent = df$`Bleeding on probing (periodontitis)`,
  messy_sd = df$`Bleeding on probing (SD) (periodontitis)`
)

prompt_fixes <- add_manual_fixes(bop_perio_messy, 'bop_perio', prompt_fixes)
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

prompt_fixes <- add_manual_fixes(supp_health_messy, 'supp_health', prompt_fixes)
if (!check_prev_prompt(supp_health_messy, 'supp_health', prompt_fixes)) {
  
  prompt_fixes$supp_health <- run_generic_prompt(supp_health_messy, 'supp_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
supp_perio_messy <- tibble(
  messy_percent = df$`Suppuration (periodontitis)`,
  messy_sd = df$`Suppuration (SD) (periodontitis)`
)

prompt_fixes <- add_manual_fixes(supp_perio_messy, 'supp_perio', prompt_fixes)
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

prompt_fixes <- add_manual_fixes(pd_health_messy, 'pd_health', prompt_fixes)
if (!check_prev_prompt(pd_health_messy, 'pd_health', prompt_fixes)) {
  
  prompt_fixes$pd_health <- run_generic_prompt(pd_health_messy, 'pd_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
pd_perio_messy <- tibble(
  messy_num = df$`PD (periodontitis)`,
  messy_sd = df$`PD SD (periodontitis)`
)

prompt_fixes <- add_manual_fixes(pd_perio_messy, 'pd_perio', prompt_fixes)
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

prompt_fixes <- add_manual_fixes(cal_health_messy, 'cal_health', prompt_fixes)
if (!check_prev_prompt(cal_health_messy, 'cal_health', prompt_fixes)) {
  
  prompt_fixes$cal_health <- run_generic_prompt(cal_health_messy, 'cal_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
cal_perio_messy <- tibble(
  messy_num = df$`Clinical attachment loss/level (mm; mean+-SD) (periodontitis)`,
  messy_sd = df$`CAL. SD (periodontitis)`
)

prompt_fixes <- add_manual_fixes(cal_perio_messy, 'cal_perio', prompt_fixes)
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

prompt_fixes <- add_manual_fixes(plaque_health_messy, 'plaque_health', prompt_fixes)
if (!check_prev_prompt(plaque_health_messy, 'plaque_health', prompt_fixes)) {
  
  prompt_fixes$plaque_health <- run_generic_prompt(plaque_health_messy, 'plaque_health')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# perio group
plaque_perio_messy <- tibble(
  messy_num = df$`Plaque (mean +/SD) (periodontitis)`,
  messy_sd = df$`Plaque (SD) (periodontitis)`
)

prompt_fixes <- add_manual_fixes(plaque_perio_messy, 'plaque_perio', prompt_fixes)
if (!check_prev_prompt(plaque_perio_messy, 'plaque_perio', prompt_fixes)) {
  
  prompt_fixes$plaque_perio <- run_generic_prompt(plaque_perio_messy, 'plaque_perio')
  saveRDS(prompt_fixes, prompt_fixes_file)
}

# join cleaned up columns ----

saveRDS(prompt_fixes, prompt_fixes_file)

# fix up factor results
prompt_fixes$seq_res_cleaned[] <- lapply(prompt_fixes$seq_res_cleaned, as.character)
prompt_fixes$seq_res_cleaned[prompt_fixes$seq_res_cleaned == 'NA'] <- NA

cleaned_df <- tibble::tibble(
  "PMID" = NA_integer_,
  # for joining with microbe data
  "Number" = df$Number,
  
  # use bugsigdb column names
  "Study design" = designs,
  "Location of subjects" = locs,
  "Host species" = "Homo sapiens",
  "Body site" = body_site,
  "Condition" = "Periodontitis",
  "Group 0 name" = group0_name,
  "Group 1 name" = group1_name,
  "Group 1 definition" = group1_def,
  "Group 0 definition" = group0_def,
  "Periodontitis classification" = NA_character_,
  "Systemic condition" = NA_character_,

  "Sequencing type" = prompt_fixes$seq_res_cleaned$seq_type,
  "16S variable region" = prompt_fixes$seq_res_cleaned$`16s_regions`,
  "Sequencing platform" = prompt_fixes$seq_res_cleaned$seq_plat,
  
  "Group 0 sample size" = prompt_fixes$group0_size$clean_num,
  "Group 1 sample size" = prompt_fixes$group1_size$clean_num,
  
  
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
  "Group 0 bleeding on probing sd" =  prompt_fixes$bop_health$clean_sd,
  "Group 1 bleeding on probing percent" =  prompt_fixes$bop_perio$clean_percent,
  "Group 1 bleeding on probing sd" =  prompt_fixes$bop_perio$clean_sd,
  
  "Group 0 suppuration percent" =  prompt_fixes$supp_health$clean_percent,
  "Group 0 suppuration sd" =  prompt_fixes$supp_health$clean_sd,
  "Group 1 suppuration percent" =  prompt_fixes$supp_perio$clean_percent,
  "Group 1 suppuration sd" =  prompt_fixes$supp_perio$clean_sd,
  
  "Group 1 pocket depth mean (mm)" =  prompt_fixes$pd_perio$clean_num,
  "Group 1 pocket depth sd" =  prompt_fixes$pd_perio$clean_sd,
  "Group 0 pocket depth mean (mm)" =  prompt_fixes$pd_health$clean_num,
  "Group 0 pocket depth sd" =  prompt_fixes$pd_health$clean_sd,
  
  "Group 0 clinical attachment loss (mm)" =  prompt_fixes$cal_health$clean_num,
  "Group 0 clinical attachment loss sd" =  prompt_fixes$cal_health$clean_sd,
  "Group 1 clinical attachment loss (mm)" =  prompt_fixes$cal_perio$clean_num,
  "Group 1 clinical attachment loss sd" =  prompt_fixes$cal_perio$clean_sd,
  
  "Group 0 plaque" =  prompt_fixes$plaque_health$clean_num,
  "Group 0 plaque sd" =  prompt_fixes$plaque_health$clean_sd,
  "Group 1 plaque" =  prompt_fixes$plaque_perio$clean_num,
  "Group 1 plaque sd" =  prompt_fixes$plaque_perio$clean_sd
)

# final clean up and save ----

# evaluate numeric columns (e.g. '139+100' --> 239)
non_numeric_cols <- c(
  'Number', 'Study design', 'Location of subjects', 'Host species', 'Body site',
  'Condition', 'Group 0 name', 'Group 1 name', 'Group 1 definition', 'Group 0 definition',
  'Periodontitis classification', 'Systemic condition',
  'Sequencing type', 'Sequencing platform'
)


for (col in colnames(cleaned_df)) {
  if (col %in% non_numeric_cols) next()
  cleaned_df[[col]] <- evaluate_col(cleaned_df[[col]])
}

# clean non-utf characters

# Clean by working with raw bytes directly
clean_string_safe <- function(x) {
  if (is.na(x)) return(x)
  
  raw_bytes <- charToRaw(x)
  
  # Keep only: printable chars (32-126), tab (9), newline (10), carriage return (13), 
  # and UTF-8 continuation bytes (128+)
  keep <- (raw_bytes >= as.raw(32) & raw_bytes <= as.raw(126)) |  # printable ASCII
    raw_bytes == as.raw(9) |   # tab
    raw_bytes == as.raw(10) |  # newline  
    raw_bytes == as.raw(13) |  # carriage return
    raw_bytes >= as.raw(128)   # UTF-8 multibyte
  
  rawToChar(raw_bytes[keep])
}

# Apply to all character columns
for (col_name in names(cleaned_df)) {
  if (is.character(cleaned_df[[col_name]])) {
    cat("Cleaning column:", col_name, "\n")
    cleaned_df[[col_name]] <- sapply(cleaned_df[[col_name]], 
                                     clean_string_safe, 
                                     USE.NAMES = FALSE)
  }
}

# without manual corrections and new columns
# TODO: replace with corrected version once corrections are confirmed by curators
saveRDS(
  select(cleaned_df, -`Group 0 definition`, -`Periodontitis classification`, -`Systemic condition`),
  'output/overview_cleaned.rds'
)

# manual corrections ----

# see scripts/manual_corrections.R
source('scripts/manual_corrections.R')

corrections <- bind_rows(corrections_raw, corrections_paper)

# sequencing type/platform
cleaned_df <- cleaned_df |>
  mutate(
    # HOMINGS is Illumina sequencing of 16S rRNA genes (labelled Human Intestinal Tract Chip)
    `Sequencing type` = ifelse(Number == '538', '16S', `Sequencing type`),
    `Sequencing platform` = ifelse(Number == '538', 'Illumina', `Sequencing platform`),
    # micro-IDent strips are multiplex PCR with reverse hybridization (as for 137)
    `Sequencing type` = ifelse(Number %in% c('589', '1915'), 'PCR', `Sequencing type`),
    `Sequencing platform` = ifelse(Number %in% c('589', '1915'), 'Non-quantitative PCR', `Sequencing platform`),
    # 16S rRNA clone libraries with Sanger sequencing (labelled 'Sequencing - pcr')
    `Sequencing type` = ifelse(Number == '2139', '16S', `Sequencing type`)
  )

# a value should only be corrected once
stopifnot(!anyDuplicated(corrections[, c('Number', 'column')]))
stopifnot(all(corrections$column %in% colnames(cleaned_df)))
stopifnot(all(corrections$Number %in% cleaned_df$Number))

# record values before correction (for review)
row_idx <- match(corrections$Number, cleaned_df$Number)
corrections$previous_value <- mapply(
  function(i, col) cleaned_df[[col]][i], row_idx, corrections$column)

# standard errors of the mean to SDs (sample sizes corrected first)
size_corrections <- filter(corrections, grepl('sample size', column))
for (i in seq_len(nrow(size_corrections))) {
  row <- cleaned_df$Number == size_corrections$Number[i]
  cleaned_df[[size_corrections$column[i]]][row] <- size_corrections$value[i]
}

sem_sds <- sem_sds |>
  mutate(
    column = paste('Group', group, sapply(measure_cols[measure], `[`, 2)),
    n_column = paste('Group', group, 'sample size')
  ) |>
  # values set in corrections are already SDs
  anti_join(corrections, by = c('Number', 'column'))

sem_sds$previous_value <- NA_real_
sem_sds$value <- NA_real_
for (i in seq_len(nrow(sem_sds))) {
  row <- cleaned_df$Number == sem_sds$Number[i]
  sem <- cleaned_df[[sem_sds$column[i]]][row]
  n <- cleaned_df[[sem_sds$n_column[i]]][row]
  sem_sds$previous_value[i] <- sem
  sem_sds$value[i] <- sem_to_sd(sem, n)
  cleaned_df[[sem_sds$column[i]]][row] <- sem_sds$value[i]
}

for (i in seq_len(nrow(corrections))) {
  row <- cleaned_df$Number == corrections$Number[i]
  cleaned_df[[corrections$column[i]]][row] <- corrections$value[i]
}

# number of males/smokers wrongly set to 0 when only a fraction was reported (e.g. '0.34')
# then fill in number or percent from the other and the group size
for (group in 0:1) {
  n <- cleaned_df[[paste('Group', group, 'sample size')]]
  for (var in c('males', 'smokers')) {
    num_col <- paste('Group', group, 'num', var)
    pct_col <- paste('Group', group, 'percent', var)
    num <- cleaned_df[[num_col]]
    pct <- cleaned_df[[pct_col]]

    num[which(num == 0 & pct > 0)] <- NA
    num <- ifelse(is.na(num), round(pct * n / 100), num)
    pct <- ifelse(is.na(pct), round(100 * num / n, 1), pct)

    cleaned_df[[num_col]] <- num
    cleaned_df[[pct_col]] <- pct
  }
}

# periodontitis classification and systemic condition
classification <- stack(perio_classification)
cleaned_df$`Periodontitis classification` <- coalesce(
  as.character(classification$ind[match(cleaned_df$Number, classification$values)]),
  'Periodontitis (type not specified)'
)

cleaned_df$`Systemic condition` <- case_when(
  cleaned_df$Number %in% names(systemic_disease) ~ systemic_disease[cleaned_df$Number],
  cleaned_df$Number %in% systemically_healthy ~ 'systemically healthy',
  .default = NA_character_
)

# plaque is reported either as % of sites or as an index (e.g. Silness-Löe 0-3)
max_plaque <- pmax(cleaned_df$`Group 0 plaque`, cleaned_df$`Group 1 plaque`, na.rm = TRUE)
cleaned_df <- cleaned_df |>
  mutate(
    `Plaque measure` = case_when(
      is.na(max_plaque) ~ NA_character_,
      max_plaque > 5 ~ '% sites',
      .default = 'index'
    ),
    .after = `Group 1 plaque sd`
  )

# for review by curators: corrections based on papers and SEM conversions
review <- bind_rows(
  filter(corrections, source == 'paper'),
  transmute(sem_sds, Number, column, value, previous_value, reason = paste('SEM to SD;', reason), source = 'paper')
) |>
  # only values that changed
  filter(xor(is.na(value), is.na(previous_value)) | abs(value - previous_value) > 1e-9) |>
  left_join(select(df, Number, REFERENCE, Year), by = 'Number') |>
  relocate(REFERENCE, Year, .after = Number) |>
  mutate(across(c(value, previous_value), ~ round(.x, 2))) |>
  arrange(Number)

write.csv(review, 'output/manual_corrections.csv', row.names = FALSE)

saveRDS(cleaned_df, 'output/overview_cleaned_corrected.rds')

library(readr)
library(rentrez)
library(dplyr)
library(tibble)
library(openxlsx)

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

# fix up factor results
prompt_fixes$seq_res[] <- lapply(prompt_fixes$seq_res, as.character)
prompt_fixes$seq_res[prompt_fixes$seq_res == 'NA'] <- NA

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
  
  "Sequencing type" = prompt_fixes$seq_res$seq_type,
  "16S variable region" = prompt_fixes$seq_res$`16s_regions`,
  "Sequencing platform" = prompt_fixes$seq_res$seq_plat,
  
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

# evaluate numeric columns (e.g. '139+100' --> 239)
non_numeric_cols <- c(
  'Study design', 'Location of subjects', 'Host species', 'Body site',
  'Condition', 'Group 0 name', 'Group 1 name', 'Group 1 definition', 
  'Sequencing type', 'Sequencing platform'
)


for (col in colnames(cleaned_df)) {
  if (col %in% non_numeric_cols) next()
  cleaned_df[[col]] <- evaluate_col(cleaned_df[[col]])
}

# clean non-utf characters ----

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

# load in microbe data and join ----

diff_species <- readRDS('output/diff_species.rds')

diff_species_collapsed <- diff_species  |> 
  group_by(Number, direction)  |> 
  summarise('NCBI Taxonomy IDs' = paste(taxid, collapse = ", "), .groups = "drop") |> 
  mutate(
    'Abundance in Group 1' = ifelse(direction == 'up', 'increased', 'decreased'),
    'Number' = as.numeric(Number)
    ) |> 
  dplyr::select(-direction)

# join, only keeping studies with microbe data
final_df <- cleaned_df |> 
  right_join(diff_species_collapsed, by='Number')

# save Excel with data validation build in ----

# Define validation configuration with actual column names
validation_config <- list(
  # Dropdowns from controlled vocab
  dropdowns = list(
    "Study design" = controlled_vocab$study_design,
    "Location of subjects" = controlled_vocab$location,
    "Host species" = "Homo sapiens",
    "Body site" = "Subgingival dental plaque",
    "Condition" = "Periodontitis",
    "Sequencing type" = controlled_vocab$sequencing_type,
    "Sequencing platform" = controlled_vocab$sequencing_platform,
    "Abundance in Group 1" = c("increased", "decreased")
  ),
  
  # Integer columns (>= 0) - get actual column names
  integers = grep("num|sample size|16S variable region|PMID|Number", names(final_df), value = TRUE),
  
  # Percentage columns (0-100) - get actual column names
  percentages = grep("percent", names(final_df), value = TRUE),
  
  # Positive decimals - get actual column names
  positive_decimals = grep("mean|sd|plaque|pocket depth|clinical attachment loss", 
                           names(final_df), value = TRUE)
)

# Get all columns with validation rules
validated_columns <- c(
  names(validation_config$dropdowns),
  validation_config$integers,
  validation_config$percentages,
  validation_config$positive_decimals
)

# Find columns NOT in any validation rule
unspecified_columns <- setdiff(names(final_df), validated_columns)

# Show them
cat("Columns without validation (free text):\n")
print(unspecified_columns)

# Create workbook
wb <- createWorkbook()
addWorksheet(wb, "DATABASE")
writeData(wb, "DATABASE", final_df)

max_rows <- nrow(final_df) + 2000
rows <- 2:max_rows

# Apply DROPDOWN validations
for (col_name in names(validation_config$dropdowns)) {
  col_idx <- which(names(final_df) == col_name)
  if (!length(col_idx)) {
    stop("Column doesn't exist: ", col_name)
  }
  
  values <- validation_config$dropdowns[[col_name]]
  values_string <- paste(values, collapse = ",")
  
  cat("Applying dropdown to:", col_name, "- Length:", nchar(values_string), "chars\n")
  
  dataValidation(wb, "DATABASE", 
                 col = col_idx, 
                 rows = rows,
                 type = "list",
                 value = paste0('"', values_string, '"'))
}

# Apply INTEGER validations
for (col_name in validation_config$integers) {
  col_idx <- which(names(final_df) == col_name)
  if (!length(col_idx)) {
    stop("Column doesn't exist: ", col_name)
  }
  
  dataValidation(wb, "DATABASE", col = col_idx, rows = rows,
                 type = "whole", operator = "greaterThanOrEqual", value = 0)
  cat("Applied integer validation to:", col_name, "\n")
}

# Apply PERCENTAGE validations
for (col_name in validation_config$percentages) {
  col_idx <- which(names(final_df) == col_name)
  if (!length(col_idx)) {
    stop("Column doesn't exist: ", col_name)
  }
  
  # Format as 2 decimal places
  addStyle(wb, "DATABASE", 
           style = createStyle(numFmt = "0.00"),
           rows = rows, cols = col_idx, gridExpand = TRUE)
  
  dataValidation(wb, "DATABASE", col = col_idx, rows = rows,
                 type = "decimal", operator = "between", value = c(0, 100))
  cat("Applied percentage validation to:", col_name, "\n")
}

# Apply POSITIVE DECIMAL validations
for (col_name in validation_config$positive_decimals) {
  col_idx <- which(names(final_df) == col_name)
  if (!length(col_idx)) {
    stop("Column doesn't exist: ", col_name)
  }
  
  # Format as 2 decimal places
  addStyle(wb, "DATABASE", 
           style = createStyle(numFmt = "0.00"),
           rows = rows, cols = col_idx, gridExpand = TRUE)
  
  dataValidation(wb, "DATABASE", col = col_idx, rows = rows,
                 type = "decimal", operator = "greaterThanOrEqual", value = 0)
  cat("Applied decimal validation to:", col_name, "\n")
}

saveWorkbook(wb, "output/perio_bugs.xlsx", overwrite = TRUE)
cat("\n✓ Workbook saved: output/perio_bugs.xlsx\n")

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
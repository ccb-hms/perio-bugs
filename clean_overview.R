library(readxl)
library(bugsigdbr)

overview_file <- 'Feres_PeriodontalMicrobiome/data/OVERVIEW_SHEET_RY.xlsx'

# Get all sheet names
sheets <- excel_sheets(overview_file)


# Read all sheets into a named list
all_data <- lapply(sheets, function(sheet) read_excel(overview_file, sheet = sheet))
names(all_data) <- sheets

# read in bugsigdb database for reference
bugsigdb <- importBugSigDB()

# get correspondence between sheets
# need study number to connect different sheets
study_num <- all_data$`General information`$Number
perio_sheet <- all_data$Periodontitis
perio_sheet <- perio_sheet[!is.na(perio_sheet$Number), ]

health_sheet <- all_data$`Periodontal health`
health_sheet <- health_sheet[!is.na(health_sheet$Number), ]

perio_num <- perio_sheet$Number
table(perio_num %in% study_num)

health_num <- health_sheet$Number
table(health_num %in% study_num)

# studies that are not in "General information" sheet
perio_num[!perio_num %in% study_num]
setequal(perio_num, health_num)

# have all studies that are in "General information" sheet
table(study_num %in% perio_num)

# get index to match between two sheets
idx_perio <- match(study_num, perio_num)
idx_health <- match(study_num, health_num)

all.equal(study_num, perio_num[idx_perio])
all.equal(study_num, health_num[idx_health])


# extract and cleanup all fields from Feres data that need for bugsigdb

# Study design ----
unique_designs <- unique(unlist(strsplit(bugsigdb$`Study design`, split = ",(?! )", perl = TRUE)))
unique_designs

# general cleanup
designs <- all_data$`General information`$`Study design`
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
unique_locs <- unique(unlist(strsplit(bugsigdb$`Location of subjects`, split = ",(?! )", perl = TRUE)))
unique_locs

locs <- all_data$`General information`$Country

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

# Body site ----
# assume all the same
body_site <- rep("Subgingival dental plaque", length(locs))

# Group names ----

# assume all the same
group0_name <- rep('periodontal health', length(locs))
group1_name <- rep('periodontitis', length(locs))

# Group 1 (Periodontits) definition ----
# NOTE: Group 0 definition not present in BugSigDB

group1_def <- perio_sheet$`Criteria used to define periodontitis`[idx_perio]

# Group sample size ----
group0_size <- health_sheet$N[idx_health]

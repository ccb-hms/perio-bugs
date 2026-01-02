library(dplyr)
library(openxlsx)

source('scripts/controlled_vocab.R')


# load in microbe, cleaned overview, and study pmids
overview_cleaned <- readRDS('output/overview_cleaned.rds')
diff_species <- readRDS('output/diff_species.rds')
study_pmids <- readRDS('output/study_pmids.rds')

# add PMIDs to overview
overview_cleaned <- overview_cleaned |> 
  select(-PMID) |> 
  left_join(study_pmids, by = 'Number') |> 
  relocate(PMID, .before = 1) |> 
  mutate(PMID = as.numeric(PMID))

overview_cleaned |> 
  select(Number, PMID) |> 
  distinct() |> 
  pull(PMID) |> 
  is.na() |> 
  table()


diff_species_collapsed <- diff_species  |> 
  group_by(Number, direction)  |> 
  summarise('NCBI Taxonomy IDs' = paste(taxid, collapse = ", "), .groups = "drop") |> 
  mutate(
    'Abundance in Group 1' = ifelse(direction == 'up', 'increased', 'decreased'),
    'Number' = as.numeric(Number)
  ) |> 
  dplyr::select(-direction)

# join, only keeping studies with microbe data
final_df <- overview_cleaned |> 
  right_join(diff_species_collapsed, by='Number')

# save
saveRDS(final_df, 'output/perio_bugs.rds')

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

# Other columns in BugSigDB that don't have ----
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
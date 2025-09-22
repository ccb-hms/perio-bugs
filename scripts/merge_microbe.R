library(readxl)
library(dplyr)

# Get all sheet names
microbe_file <- 'data/Cleaned Micro List RY_final.xlsx'
microbe_sheets <- excel_sheets(microbe_file)

# Read all sheets into a named list
microbe <- lapply(microbe_sheets, function(sheet) read_excel(microbe_file, sheet = sheet))
names(microbe) <- microbe_sheets

# used by process_database to split into data.frame per study
split_by_study <- function(db) {
  study_nums <- db$Number
  study_nums_unique <- na.omit(unique(study_nums))
  
  res <- list()
  
  for (study_num in study_nums_unique) {
    
    # first row for study
    start <- which(study_nums == study_num)
    stopifnot(length(start) == 1)
    
    # how far til next study
    jump <- which(!is.na(
      study_nums[(start+1):length(study_nums)]
      ))[1]
    
    # edge case: last study num
    if (is.na(jump)) jump <- length(study_nums)
    
    # subset to study and drop Number column
    db_study <- db[start:(start+jump-1), -1]
    
    # drop all NA rows
    db_study <- db_study[!apply(is.na(db_study), 1, all), ]
    
    # store in list
    res[[study_num]] <- db_study
  }
  
  return(res)
}

# for processing "Old DATABASE" and "New DATABASE"
process_database <- function(db) {
  
  # remove reference column and empty column
  db$...11 <- db$...2 <- NULL

  # indices for columns that are elevated in health/perio
  elevated_in_health_cols <- 2:9
  elevated_in_perio_cols <- 10:17
  
  # split into up/down regulated
  db_up <- db[, c(1, elevated_in_perio_cols)]
  db_dn <- db[, c(1, elevated_in_health_cols)]
  
  # rename using first row
  colnames(db_up) <- db_up[1, ]
  db_up <- db_up[-1, ]
  
  colnames(db_dn) <- db_dn[1, ]
  db_dn <- db_dn[-1, ]
  
  # split by study number
  db_up <- split_by_study(db_up)
  db_dn <- split_by_study(db_dn)
  
  return(list(
    db_up = db_up,
    db_dn = db_dn
  ))
}

new_database <- process_database(microbe$`New DATABASE`)
old_database <- process_database(microbe$`Old DATABASE`)

# check study overlap with overview data.frame
overview_file <- 'output/overview_merged.rds'
overview <- readRDS(overview_file)

# none of New DATABASE is present in overview studies
# all of Old Database is present in overview studies
table(names(new_database$db_up) %in% overview$Number)
table(names(old_database$db_up) %in% overview$Number)

# NOTE: sarahs_db2 has taxonomy ids (yay!)
sarahs_db <- microbe$`Sarah's Work`
sarahs_db2 <- microbe$`Sarah's Work (2`




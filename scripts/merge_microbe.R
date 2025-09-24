library(readxl)
library(dplyr)

# Get all sheet names
microbe_file <- 'data/Cleaned Micro List RY_final.xlsx'
microbe_sheets <- excel_sheets(microbe_file)

# Read all sheets into a named list
microbe <- lapply(microbe_sheets, function(sheet) read_excel(microbe_file, sheet = sheet))
names(microbe) <- microbe_sheets

# used to split into data.frame per study
split_by_study <- function(db) {
  
  db |>
    tidyr::fill(Number, .direction = "down") |> 
    dplyr::group_split(Number, .keep = FALSE) |> 
    setNames(unique(na.omit(db$Number))) |> 
    purrr::map(~ dplyr::filter(.x, !if_all(dplyr::everything(), is.na)))
}

# for processing "Old DATABASE", "New DATABASE", and "Sarah's Work"
process_database <- function(db) {
  
  # remove reference column and empty column
  db$...11 <- db$...2 <- NULL
  
  # indices for columns that are elevated in health/perio
  elevated_in_health_cols <- 2:9
  elevated_in_perio_cols <- 10:17
  
  # split into up/down regulated, keeping study number
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

# NOTE: sarahs_db2 has taxonomy ids (yay!)
# for processing "Sarah's Work (2)"
process_sarahs_db2 <- function(db) {
  db$...15 <- db$Reference <- NULL
  
  # indices for columns that are elevated in health/perio
  elevated_in_health_cols <- 2:13
  elevated_in_perio_cols <- 14:25
  
  # drop first row (header info e.g. "elevated in ...")
  db <- db[-1, ]
  
  # split into up/down regulated, keeping study number
  db_up <- db[, c(1, elevated_in_perio_cols)]
  db_dn <- db[, c(1, elevated_in_health_cols)]
  
  # clean up column names
  colnames(db_up) <- gsub('[.]{3}\\d+$', '', colnames(db_up))
  colnames(db_dn) <- gsub('[.]{3}\\d+$', '', colnames(db_dn))
  
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
sarahs_db <- process_database(microbe$`Sarah's Work`)
sarahs_db2 <- process_sarahs_db2(microbe$`Sarah's Work (2`)

# check study overlap with overview data.frame
overview_file <- 'output/overview_merged.rds'
overview <- readRDS(overview_file)

# none of "New DATABASE" is present in overview studies
table(names(new_database$db_up) %in% overview$Number)

# all of "Old Database" is present in overview studies
table(names(old_database$db_up) %in% overview$Number)

# one study in "Sarah's Work" is duplicated in overview studies (462 adjusted vs unadjusted?)
names(sarahs_db$db_up)[!names(sarahs_db$db_up) %in% overview$Number]
'462' %in% overview$Number

# all of "Sarah's Work (2)" is present in overview studies
table(names(sarahs_db2$db_up) %in% overview$Number)

# all of "Sarah's Work" is present in "Sarah's Work (2)"
table(names(sarahs_db$db_up) %in% names(sarahs_db2$db_up))

# have microbe data for all studies in overview
table(
  overview$Number %in%
    unique(c(names(old_database$db_up), names(sarahs_db2$db_up)))
)

# get most specific name from df of differentially abundant species
most_specific_name_base <- function(df) {
  specific_cols <- c("Species", "Genus", "Family", "Order", "Class", "Phylum", "Kingdom", "Domain")
  apply(df, 1, function(row) {
    g <- row["Genus"]
    s <- row["Species"]
    if(!is.na(g) && g != "" && !is.na(s) && s != "") {
      paste(g, s)
    } else {
      vals <- row[specific_cols]
      vals <- vals[vals != "" & !is.na(vals)]
      vals[1]
    }
  })
}

diff_species <- unlist(lapply(
  c(old_database$db_up, old_database$db_dn), 
  function(tbl) most_specific_name_base(tbl)
))

# save list for chatGPT
writeLines(
  unique(diff_species), 
  'output/diff_species.txt'
)

# read in results
gpt_names <- read.csv('output/gpt_names.csv')

# keep iterating until chatGPT gives full output
setdiff(gpt_names$Original_Name, diff_species)
setdiff(diff_species, gpt_names$Original_Name)
all.equal(unique(diff_species), gpt_names$Original_Name)

# have taxon ids for studies in sarahs_db2
# get taxon id for studies in old_database
gpt_official <- gpt_names$NCBI_Official_Taxon_Name
res <- lapply(gpt_official, taxizedb::name2taxid, out_type = 'summary')
names(res) <- gpt_names$Original_Name

# either multiple results or none
ambig <- sapply(res, function(df) nrow(df) > 1)
absnt <- sapply(res, function(df) nrow(df) == 0)
table(ambig)
table(absnt)

res[ambig]
gpt_official[absnt]

# try less specific name
needs_fixing <- gpt_official[absnt]
less_specific <- gsub(' oral taxon.+?$', '', needs_fixing)
less_specific <- gsub(' clone.+?$', '', less_specific)
less_specific <- gsub(' subsp.+?$', '', less_specific)
less_specific <- gsub(' sp.+?$', '', less_specific)
less_specific <- gsub('( bacterium) .+?$', '\\1', less_specific)
less_specific <- gsub('^[^ ]+ (.+bacterium)$', '\\1', less_specific)
less_specific <- gsub('Saccharibacteria', 'Saccharimonadota', less_specific)

res2 <- lapply(less_specific, taxizedb::name2taxid, out_type = 'summary')
ambig2 <- sapply(res2, function(df) nrow(df) > 1)
absnt2 <- sapply(res2, function(df) nrow(df) == 0)
table(ambig2)
table(absnt2)

# a few remaining
needs_fixing[absnt2]

taxizedb::name2taxid("saccharimonadota bacterium", out_type = 'summary')


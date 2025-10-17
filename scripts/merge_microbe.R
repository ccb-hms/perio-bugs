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

original_names <- unlist(lapply(
  c(old_database$db_up, old_database$db_dn), 
  function(tbl) most_specific_name_base(tbl)
))

original_names <- unique(original_names)

# save list for chatGPT
# writeLines(
#   original_names, 
#   'output/original_names.txt'
# )

# chatGPT guesses for official taxon names in old_database
gpt_res <- read.csv('output/gpt_names.csv')
gpt_names <- gpt_res$NCBI_Official_Taxon_Name
gpt_original <- gpt_res$Original_Name

# keep iterating until chatGPT gives full output
setdiff(gpt_original, original_names)
setdiff(original_names, gpt_original)
all.equal(unique(original_names), gpt_original)

# first atempt with original names ----
# convert names to taxids

# this gets about 10 additional on first pass (chatGPT gets at least 9 anyway)
# original_names <- gsub('_ot', 'oral taxon', original_names)
# original_names <- gsub('sp[.]?', '.', original_names)

res <- lapply(original_names, taxizedb::name2taxid, out_type = 'summary')

# either multiple results or none
orig_ambig <- sapply(res, function(df) nrow(df) > 1)
orig_absnt <- sapply(res, function(df) nrow(df) == 0)
table(orig_ambig)
table(orig_absnt)

original_names[orig_ambig]
gpt_names[orig_ambig]

# second attempt with GPT names ----
use.gpt <- orig_absnt | orig_ambig
gpt_names <- gpt_names[use.gpt]

# fix up some identified errors 

# pick one (could be either)
gpt_names <- gsub('intermedia\\/nigrescens', 'intermedia', gpt_names)

# use correct identifier as only have phylum
gpt_names <- gsub('^Firmicutes$', 'Bacillaeota', gpt_names)

# misspellings
gpt_names <- gsub('Arsenicococcus', 'Arsenicicoccus', gpt_names)
gpt_names <- gsub('Fusibacterium', 'Fusobacterium', gpt_names)

# official name is updated
gpt_names <- gsub('GNO2 \\[G-1\\]', 'Gracilibacteria', gpt_names)
gpt_names <- gsub('Prevotella oralis', 'Hoylesella oralis', gpt_names)
gpt_names[gpt_names == 'Fusobacterium nucleatum subsp. polymorphum'] <- 'Fusobacterium polymorphum'
gpt_names[gpt_names == 'Fusobacterium nucleatum subsp. vincentii'] <- 'Fusobacterium vincentii'

# alternative name works better with taxizedb
gpt_names <- gsub('Candidatus Saccharibacteria bacterium', 'TM7 phylum sp.', gpt_names)

# second attempt 

# convert names to taxids
res <- lapply(gpt_names, taxizedb::name2taxid, out_type = 'summary')

# either multiple results or none
gpt_ambig <- sapply(res, function(df) nrow(df) > 1)
gpt_absnt <- sapply(res, function(df) nrow(df) == 0)
table(gpt_ambig)
table(gpt_absnt)

gpt_names[gpt_absnt]

# third attempt with simplified names ----
use.simple <- gpt_absnt | gpt_ambig

needs_fixing <- gpt_names[use.simple]

# remove " oral taxon...", " clone...", and " subsp..." at end
simple_names <- gsub(' oral taxon.+?$', '', needs_fixing)
simple_names <- gsub(' clone.+?$', '', simple_names)
simple_names <- gsub(' subsp.+?$', '', simple_names)

# remove anything after unclassified species ("sp.")
simple_names <- gsub('(sp[.]).+?$', '\\1', simple_names)

# set multiple unclassified species ("spp.") to unclassified species ("sp.")
simple_names <- gsub(' spp[.]$', ' sp.', simple_names)

# remove anything after " bacterium"
simple_names <- gsub('( bacterium) .+?$', '\\1', simple_names)

# set remaining TM7 to unclassified
simple_names[simple_names == 'TM7 phylum sp.'] <- 'unclassified Candidatus Saccharimonadota'

res <- lapply(simple_names, taxizedb::name2taxid, out_type = 'summary')
simple_ambig <- sapply(res, function(df) nrow(df) > 1)
simple_absnt <- sapply(res, function(df) nrow(df) == 0)
table(simple_ambig)
table(simple_absnt)

# for checking the few remaining
original_names[use.gpt][use.simple][simple_absnt]

# final result ----
cleaned_names <- original_names
cleaned_names[use.gpt] <- gpt_names
cleaned_names[use.gpt][use.simple] <- simple_names

taxid_tbl <- data.frame(
  original_name = original_names,
  gpt_name = NA,
  simple_name = NA,
  taxid = taxizedb::name2taxid(cleaned_names)
)

taxid_tbl$gpt_name[use.gpt] <- gpt_names
taxid_tbl$simple_name[use.gpt][use.simple] <- simple_names

View(filter(taxid_tbl, original_name != cleaned_names))

# save results for Feres lab to validate
# don't need to validate entries where original and clean are the same
# taxid_tbl |> 
#   filter(original_name != cleaned_names) |> 
#   write.csv('output/taxon_ids_to_validate.csv')

# add taxon ids for differential species tables using lookup table (taxid_tbl)
add_taxid <- function(tbl, taxid_tbl) {
  taxon_names <- most_specific_name_base(tbl)
  idx <- match(taxon_names, taxid_tbl$original_name)
  tbl$`Taxon ID` <- taxid_tbl$taxid[idx]
  return(tbl)
}

old_database$db_up <- lapply(old_database$db_up, add_taxid, taxid_tbl = taxid_tbl)
old_database$db_dn <- lapply(old_database$db_dn, add_taxid, taxid_tbl = taxid_tbl)

# cleanup sarahs_db2 taxon ids ----

# Human Oral Microbiome
homd <- data.table::fread('data/HOMD_taxon_table2025-08-26_1756207550.txt')
homd <- homd |> 
  select(HMT_ID:Species, Synonyms, NCBI_taxon_id)

# convert list of lists into a single data.frame
rbind_taxdbs <- function(db) {
  study_nums <- names(db$db_up)
  for (study_num in study_nums) {
    # add study number to each table
    db$db_up[[study_num]]$Number <- study_num
    db$db_dn[[study_num]]$Number <- study_num
    
    # add direction to each table
    db$db_up[[study_num]]$direction <- "up"
    db$db_dn[[study_num]]$direction <- "dn"
  }
  
  do.call(rbind, c(db$db_up, db$db_dn))
}

sarahs_db2_df <- rbind_taxdbs(sarahs_db2)

# remove working columns
# NOTE: "... - condensed" removes blank rows
# NOTE: "... w/ numerical values" are rows where there is a 0-9 in corresponding taxon name
# NOTE: "Taxon ID" values not aligned with "Family", "Genus", "Species" (possibly aligned with "... - condensed")
sarahs_db2_df <- 
  select(sarahs_db2_df, Family, Genus, Species, direction, Number)

# work with original annotations of Family, Genus, Species
sarahs_db2_df <- sarahs_db2_df |> 
  mutate(most_specific_name = most_specific_name_base(sarahs_db2_df)) |>
  # remove duplicate Genus identifier
  mutate(most_specific_name = gsub('^([A-Z][a-z]+) \\1 ', '\\1 ', most_specific_name)) |> 
  filter(!is.na(most_specific_name))

# add exact HOMD Genus species matches
homd_names <- data.frame(
  most_specific_name = most_specific_name_base(homd),
  taxid = homd$NCBI_taxon_id
  ) |> 
  distinct(most_specific_name, .keep_all = TRUE)


sarahs_db2_df <- left_join(
  sarahs_db2_df, homd_names, keep = TRUE, suffix = c('_sarahs', '_homd'))

table(is.na(sarahs_db2_df$taxid))

needs_fixing <- sarahs_db2_df |> 
  filter(is.na(taxid)) |> 
  pull(most_specific_name_sarahs) |> 
  unique()

# try to get taxids
res <- lapply(needs_fixing, taxizedb::name2taxid, out_type = 'summary')

# either multiple results or none
orig_ambig <- sapply(res, function(df) nrow(df) > 1)
orig_absnt <- sapply(res, function(df) nrow(df) == 0)
table(orig_ambig)
table(orig_absnt)

needs_fixing[orig_ambig]
needs_fixing[orig_absnt]

res <- do.call(rbind, res)
res$id <- as.numeric(res$id)

# fill in hits
sarahs_db2_df <- sarahs_db2_df |> 
  left_join(res, join_by(most_specific_name_sarahs == name)) |> 
  mutate(taxid = coalesce(taxid, id)) |> 
  select(-id)

# second attempt with chatGPT names ----
# PROMPT:
# For the following list of bacterial taxon names, get the closest matching official
# NCBI taxon names (Genus species) for the included table.
# 
# Format the result as a csv with each line having the row number, the original name
# I am providing as well as the corresponding official NCBI taxon name.
# provide the first 300 rows and then prompt me for the next batch.
# 
# cat(needs_fixing[orig_absnt], sep='\n')

gpt_res <- read.csv('output/gpt_names_sarah2.csv', stringsAsFactors = FALSE) |> 
  separate_rows(ncbi_name, sep = ";") |> 
  rename(ncbi_name_split = ncbi_name)

gpt_original <- gpt_res$original_name
gpt_names <- gpt_res$ncbi_name_split

all.equal(unique(gpt_original), needs_fixing[orig_absnt])

# convert names to taxids
res <- lapply(gpt_names, taxizedb::name2taxid, out_type = 'summary')

# either multiple results or none
gpt_ambig <- sapply(res, function(df) nrow(df) > 1)
gpt_absnt <- sapply(res, function(df) nrow(df) == 0)
table(gpt_ambig)
table(gpt_absnt)

gpt_names[gpt_absnt]

# third attempt asking gemini in google (uses search) ----
# fixed up remaining (LOTS) manually in the csv

gem_res <- read.csv('output/gemini_names_sarah2.csv', stringsAsFactors = FALSE) |> 
  select(-notes, -taxon_id)

gem_names <- gem_res$ncbi_name

res <- lapply(gem_names, taxizedb::name2taxid, out_type = 'summary')

# either multiple results or none
gem_ambig <- sapply(res, function(df) nrow(df) > 1)
gem_absnt <- sapply(res, function(df) nrow(df) == 0)
table(gem_ambig)
table(gem_absnt)

gem_names[gem_ambig]
gem_names[gem_absnt]

# bind and remove eukaryotic Kingella
res <- do.call(rbind, res) |> 
  distinct() |> 
  filter(id != '52002') |>
  right_join(gem_res, join_by(name == ncbi_name))

View(res)


# merge all differentially up and down tables
diff_species <- list(
  db_up = c(old_database$db_up, sarahs_db2$db_up),
  db_dn = c(old_database$db_dn, sarahs_db2$db_dn)
)

saveRDS(diff_species, 'output/diff_species.rds')

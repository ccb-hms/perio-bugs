library(readxl)
library(dplyr)

# setup -----
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

# NOTE: sarahs_db2 has some taxonomy ids
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

# Old DATABASE: setup ----
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

# Old DATABASE: first attempt with original names ----
# convert names to taxids

# Human Oral Microbiome
homd <- data.table::fread('data/HOMD_taxon_table2025-08-26_1756207550.txt')
homd <- homd |> 
  select(HMT_ID:Species, Synonyms, NCBI_taxon_id)

# check in HOMD
homd_names <- most_specific_name_base(homd)
in.homd <- original_names %in% homd_names
table(in.homd)

# this gets about 10 additional on first pass (chatGPT gets at least 9 anyway)
cleaned_names <- gsub('_ot', 'oral taxon', original_names)
cleaned_names <- gsub(' sp(\\.)? ', ' sp. ', cleaned_names)

res <- lapply(cleaned_names, taxizedb::name2taxid, out_type = 'summary')

# either multiple results or none
cleaned_ambig <- sapply(res, function(df) nrow(df) > 1)
cleaned_absnt <- sapply(res, function(df) nrow(df) == 0)
table(cleaned_ambig)
table(cleaned_absnt)

original_names[cleaned_ambig]
gpt_names[cleaned_ambig]

# any additional in HOMD? --> No
table(in.homd & cleaned_absnt)

# Old DATABASE: second attempt with GPT names ----
use.gpt <- (cleaned_absnt | cleaned_ambig) & !in.homd
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

# Old DATABASE: third attempt with simplified names ----
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
cleaned_names[use.gpt][use.simple][simple_absnt]

# Old DATABASE: final result ----
final_names <- cleaned_names
final_names[use.gpt] <- gpt_names
final_names[use.gpt][use.simple] <- simple_names

taxid_tbl <- data.frame(
  original_name = original_names,
  cleaned_name = cleaned_names,
  gpt_name = NA,
  simple_name = NA,
  taxid = taxizedb::name2taxid(final_names)
)

taxid_tbl$gpt_name[use.gpt] <- gpt_names
taxid_tbl$simple_name[use.gpt][use.simple] <- simple_names

# check if HOMD taxids are the same
homd_taxids <- homd$NCBI_taxon_id
names(homd_taxids) <- homd_names

homd.same <- homd_taxids[original_names[in.homd]] == taxid_tbl$taxid[in.homd]
table(homd.same)

# taxid from taxizedb is correct
homd_taxids[original_names[in.homd]][!homd.same]
taxid_tbl[in.homd, ][!homd.same, ]

View(filter(taxid_tbl, final_names != original_name))

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

# merge all tables into one
old_database_df <- rbind_taxdbs(old_database)

# Sarah's Work (2): setup ----

clean_names_simple <- function(x) {
  # remove duplicate Genus identifier
  x <- gsub('^([A-Z][a-z]+) \\1 ', '\\1 ', x)
  return(x)
}

clean_names <- function(x) {
  # Remove all [ ... ] and any spaces immediately after them
  x <- gsub("\\[[^]]*\\]\\s*", "", x)
  
  # Remove space, open parenthesis, numbers, close parenthesis at end of string
  x <- gsub(" \\([0-9]+\\)$", "", x)
  
  # Remove "V1-2" or "V1 2" (optionally surrounded by whitespace)
  x <- gsub("\\s*V1[ -]2\\s*", " ", x)
  
  # replace OT and HOT with oral taxon
  x <- gsub("\\bOT ([0-9]+)\\b", "oral taxon \\1", x)
  x <- gsub("\\bHOT ([0-9]+)\\b", "oral taxon \\1", x)
  
  # remove duplicate Genus identifier
  x <- gsub('^([A-Z][a-z]+) \\1 ', '\\1 ', x, ignore.case = TRUE)
  
  # Collapse any runs of multiple spaces to single spaces, then trim
  x <- gsub(" +", " ", x)
  x <- trimws(x)
  
  return(x)
}

# merge all tables into one
sarahs_db2_df <- rbind_taxdbs(sarahs_db2)

# extract annotated taxon id <--> Genus species for checking later
annotated_names <- tibble(
  Genus = sarahs_db2_df$`Genus w/ numerical values`,
  Species = sarahs_db2_df$`Species w/ numerical values - condensed`,
  'Taxon ID' = sarahs_db2_df$`Taxon ID`) |> 
  filter(!is.na(`Taxon ID`))

annotated_names <- annotated_names |> 
  mutate(most_specific_name = most_specific_name_base(annotated_names)) |> 
  mutate(most_specific_name = clean_names(most_specific_name)) |> 
  distinct()

# 588 unique cleaned annotated Genus species
length(unique(annotated_names$most_specific_name))

# remove working columns
# NOTE: "... - condensed" removes blank rows
# NOTE: "... w/ numerical values" are rows where there is a 0-9 in corresponding taxon name
# NOTE: "Taxon ID" values not aligned with "Family", "Genus", "Species" (possibly aligned with "... - condensed")
sarahs_db2_df <- 
  select(sarahs_db2_df, Family, Genus, Species, direction, Number) |> 
  mutate(most_specific_name = most_specific_name_base(sarahs_db2_df)) |>
  filter(!is.na(most_specific_name)) |>
  mutate(most_specific_name = clean_names(most_specific_name)) |> 
  distinct()

# 838 unique cleaned Genus species
length(unique(sarahs_db2_df$most_specific_name))

# Sarah's Work (2): first attempt with HOMD database ----
# add exact HOMD Genus species matches
homd_names <- data.frame(
  most_specific_name = most_specific_name_base(homd),
  taxid = homd$NCBI_taxon_id
) |> 
  distinct(most_specific_name, .keep_all = TRUE)

sarahs_db2_df <- left_join(
  sarahs_db2_df, homd_names, keep = TRUE, suffix = c('_sarahs', '_homd'))

table(is.na(sarahs_db2_df$taxid))

# check if HOMD is useful
homd_success_names <- sarahs_db2_df |> 
  filter(!is.na(taxid)) |> 
  pull(most_specific_name_sarahs) |> 
  unique()

homd_success_taxids <- sarahs_db2_df |> 
  filter(!is.na(taxid)) |> 
  pull(taxid) |> 
  unique() |> 
  as.character()

res <- lapply(homd_success_names, taxizedb::name2taxid, out_type = 'summary')
homd_absnt <- sapply(res, function(df) nrow(df) == 0)
homd_success_calc_taxids <- do.call(rbind, res)$id

# 7 cases where HOMD taxids not found by taxizedb
# 1 different taxid
table(homd_absnt)
all.equal(homd_success_calc_taxids, homd_success_taxids[!homd_absnt])

# Sarah's Work (2): second attempt with cleaned names ----

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
# needs_fixing[orig_absnt]

res <- do.call(rbind, res)
res$id <- as.numeric(res$id)

# fill in hits
sarahs_db2_df <- sarahs_db2_df |> 
  left_join(res, join_by(most_specific_name_sarahs == name)) |> 
  mutate(taxid = coalesce(taxid, id)) |> 
  select(-id)

# update what needs fixing
needs_fixing <- needs_fixing[orig_absnt]

# Sarah's Work (2): third attempt with chatGPT names ----
#
# PROMPT:
# For the following list of bacterial taxon names, get the closest matching official
# NCBI taxon names (Genus species) for the included table.
# 
# Format the result as a csv with each line having the row number, the original name
# I am providing as well as the corresponding official NCBI taxon name.
# provide the first 300 rows and then prompt me for the next batch.
# 
# cat(needs_fixing, sep='\n')

gpt_res <- read.csv('output/gpt_names_sarah2.csv', stringsAsFactors = FALSE) |> 
  tidyr::separate_rows(ncbi_name, sep = ";") |> 
  rename(ncbi_name_split = ncbi_name) |> 
  mutate(original_name = clean_names(original_name)) |> 
  select(-row) |> 
  filter(original_name %in% needs_fixing)

gpt_original <- gpt_res$original_name
gpt_names <- gpt_res$ncbi_name_split

all.equal(needs_fixing, unique(gpt_original))

# convert names to taxids
res <- lapply(gpt_names, taxizedb::name2taxid, out_type = 'summary')

# either multiple results or none
gpt_ambig <- sapply(res, function(df) nrow(df) > 1)
gpt_absnt <- sapply(res, function(df) nrow(df) == 0)
table(gpt_ambig)
table(gpt_absnt)

gpt_names[gpt_absnt]

# join back to chatGPT results
res <- do.call(rbind, res) |> 
  distinct() |> 
  right_join(gpt_res, join_by(name == ncbi_name_split)) |> 
  mutate(id = as.numeric(id)) |> 
  select(-name)

# fill in hits
# many-to-many expected due to split above
sarahs_db2_df <- sarahs_db2_df |> 
  left_join(res, join_by(most_specific_name_sarahs == original_name), relationship = 'many-to-many') |> 
  mutate(taxid = coalesce(taxid, id)) |> 
  select(-id)

# how many left?
needs_fixing <- sarahs_db2_df |> 
  filter(is.na(taxid)) |> 
  pull(most_specific_name_sarahs) |> 
  unique()

length(needs_fixing)

# Sarah's Work (2): fourth attempt asking Google AI mode ----
# fixed up remaining manually in the csv

gem_res <- read.csv('output/gemini_names_sarah2.csv', stringsAsFactors = FALSE) |> 
  select(-notes, -taxon_id) |> 
  mutate(original_name = clean_names(original_name)) |> 
  filter(original_name %in% needs_fixing) |> 
  distinct()
  

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
  right_join(gem_res, join_by(name == ncbi_name)) |> 
  mutate(id = as.numeric(id)) |> 
  select(-name)

# fill in hits
sarahs_db2_df <- sarahs_db2_df |> 
  left_join(res, join_by(most_specific_name_sarahs == original_name)) |> 
  mutate('Taxon ID' = coalesce(taxid, id)) |> 
  select(-id, taxid)

# Sarah's Work (2): check concordance with annotated taxids ----
taxonomic_tree_distance <- function(taxid1, taxid2) {
  # Retrieve lineages
  cl1 <- taxizedb::classification(taxid1, db = "ncbi")[[1]]
  cl2 <- taxizedb::classification(taxid2, db = "ncbi")[[1]]
  
  # annotated taxid not found
  if (length(cl1) == 1 && is.na(cl1)) return(NA)
  
  # Remove nodes with 'no rank'
  cl1 <- cl1[cl1$rank != "no rank", , drop = FALSE]
  cl2 <- cl2[cl2$rank != "no rank", , drop = FALSE]
  
  # Find where the lineages diverge
  min_length <- min(nrow(cl1), nrow(cl2))
  split_index <- 0
  for (i in seq_len(min_length)) {
    if (cl1$name[i] != cl2$name[i]) {
      split_index <- i - 1
      break
    }
    if (i == min_length) split_index <- min_length
  }
  
  steps1 <- nrow(cl1) - split_index
  steps2 <- nrow(cl2) - split_index
  distance <- steps1 + steps2
  mrca <- if (split_index > 0) cl1$name[split_index] else NA
  
  return(distance)
}

annotated_names <- annotated_names |> 
  select(most_specific_name, `Taxon ID`) |> 
  left_join(
    sarahs_db2_df |> select(most_specific_name_sarahs, `Taxon ID`), 
    join_by(most_specific_name == most_specific_name_sarahs),
  ) |> 
  distinct() |> 
  rename(
    taxid_annot = `Taxon ID.x`,
    taxid_calc = `Taxon ID.y`
  )

table(annotated_names$taxid_annot == annotated_names$taxid_calc)

annotated_names  <- annotated_names |> 
  filter(taxid_annot != taxid_calc) |> 
  # e.g. '199 / 203' becomes NA
  filter(!is.na(taxid_annot)) |> 
  rowwise() |> 
  mutate(tree_distance = taxonomic_tree_distance(taxid_annot, taxid_calc))

table(annotated_names$tree_distance)

# Old DATABASE and Sarah's Work (2): merge all and save ---

# have taxids for all
sum(is.na(sarahs_db2_df$`Taxon ID`))
sum(is.na(old_database_df$`Taxon ID`))

diff_species <- rbind(
  old_database_df |> select('Number', 'Taxon ID', 'direction'),
  sarahs_db2_df |> select('Number', 'Taxon ID', 'direction')
) |> distinct()

saveRDS(diff_species, 'output/diff_species.rds')

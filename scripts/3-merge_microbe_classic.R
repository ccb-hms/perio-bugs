library(readxl)
library(dplyr)
library(rols)
library(stringr)

# setup -----
# Get all sheet names
microbe_file <- 'data/Cleaned Micro List RY_final.xlsx'
microbe_sheets <- excel_sheets(microbe_file)

# Read all sheets into a named list
microbe <- lapply(microbe_sheets, function(sheet) read_excel(microbe_file, sheet = sheet))
names(microbe) <- microbe_sheets

# helper functions
source('scripts/microbe_helpers.R')

new_database <- process_database(microbe$`New DATABASE`)
old_database <- process_database(microbe$`Old DATABASE`)
sarahs_db <- process_database(microbe$`Sarah's Work`)
sarahs_db2 <- process_sarahs_db2(microbe$`Sarah's Work (2`)

# check study overlap with overview data.frame
overview_file <- 'output/overview_merged.rds'
overview <- readRDS(overview_file)

# all of "New DATABASE" is present in overview studies
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
    unique(c(
      names(new_database$db_up), 
      names(old_database$db_up), 
      names(sarahs_db2$db_up)
      ))
)

# Old DATABASE and New DATABASE: setup ----

old_database <- process_database(microbe$`Old DATABASE`)
new_database <- process_database(microbe$`New DATABASE`)
old_database <- rbind_taxdbs(old_database) 
new_database <- rbind_taxdbs(new_database) 

old_and_new_database <- dplyr::bind_rows(old_database, new_database) |> 
  tidyr::separate_rows(Species, sep = "\\/")

# convert entries marked 'Unclassified' to NA
old_and_new_database[old_and_new_database == 'Unclassified'] <- NA

old_and_new_database <- old_and_new_database |>
  mutate(
    most_specific = most_specific_name_base(old_and_new_database),
    next_most_specific = most_specific_name_base(old_and_new_database, exclude_species = TRUE))


old_and_new_database_queries <- old_and_new_database |> 
  select(most_specific, next_most_specific, Number, direction) |> 
  mutate(
    most_specific = clean_names(most_specific),
    next_most_specific = clean_names(next_most_specific)
  ) |> 
  mutate(most_specific = expand_oral_taxon(most_specific)) |> 
  tidyr::separate_rows(most_specific, sep = ';')

old_and_new_database_results <- run_ncbitaxon_queries(old_and_new_database_queries)

# add back Number and direction
cols <- c('Number', 'direction')
old_and_new_database_results[,cols] <- old_and_new_database_queries[,cols]

# identify queries with wrong oral taxon mapping
old_and_new_database_results <- old_and_new_database_results |> 
  mutate(
    taxname_ot = str_extract(taxname, "oral taxon (\\d+)", group = 1),
    taxname_ot_wrong = !is.na(taxname_ot) &
      !str_detect(tolower(query), fixed(taxname_ot))
  )

# inspect them
old_and_new_database_results |> 
  filter(taxname_ot_wrong) |> 
  View()

# extract them and run the queries without oral taxon
rerun_df <- old_and_new_database_results |> 
  filter(taxname_ot_wrong) |> 
  mutate(most_specific = str_trim(str_remove(query, "oral taxon \\d+"))) |> 
  select(most_specific, next_most_specific) |> 
  run_ncbitaxon_queries()

rerun_df |> View()

# replace results
wrong_idx <- which(old_and_new_database_results$taxname_ot_wrong)
old_and_new_database_results[wrong_idx, names(rerun_df)] <- rerun_df


# manually check results where Domain isn't Bacteria
domain <- get_rank_vals(old_and_new_database_results)
old_and_new_database_results[domain != 'Bacteria', ]

# fix results where Domain is not Bacteria
# also other manually identified errors go here
old_and_new_database_results <- old_and_new_database_results |> 
  mutate(
    taxname_fixed = case_match(
      query,
      # Domain not Bacteria
      
      'OP11 clone X112' ~ 'uncultured bacterium X112',
      'Micromonas micros' ~ 'Parvimonas micra',
      'Eubacterium yuri subsp' ~ 'Eubacterium yurii subsp. yurii',
      'Treponema E25 8' ~ 'Treponema',
      'Lachnospiracee sp.' ~ 'unclassified Lachnospiraceae',
      'Tessnema sp.' ~ 'Synergistaceae',
      'Streptococcus sanguis' ~ 'Streptococcus sanguinis',
      'Haemophilus P3D1 620' ~ 'Haemophilus',
      'G1 958' ~ 'unclassified Bacteria',
      'G1 869' ~ 'unclassified Bacteria',
      
      # manually identified errors
      
      'Treponema E D 05 72' ~ 'Treponema',
      'Bifidobacterium dentum' ~ 'Bifidobacterium dentium',
      'Lachnospiraceae JM048' ~ 'Lachnospiraceae',
      'Saccharibacteria (TM7) -like sp.' ~ 'unclassified Candidatus Saccharimonadota',
      'TM7 401H12' ~ 'unclassified Candidatus Saccharimonadota',
      'TM7 clone I025' ~ 'unclassified Candidatus Saccharimonadota',
      'Prevotella oralis' ~ 'Hoylesella oralis',
    )
  )

# extract them and run the queries
rerun_df <- old_and_new_database_results |> 
  filter(!is.na(taxname_fixed)) |> 
  mutate(most_specific = taxname_fixed) |> 
  select(most_specific, next_most_specific) |> 
  run_ncbitaxon_queries()

# replace results
fixed_idx <- which(!is.na(old_and_new_database_results$taxname_fixed))
old_and_new_database_results[fixed_idx, names(rerun_df)] <- rerun_df

# add exact taxizedb results
old_and_new_database_results <- add_exact_taxids(old_and_new_database_results)

# add distances between taxids and genus
old_and_new_database_results <- add_tree_dist_genus(old_and_new_database_results)

table(old_and_new_database_results$tree_dist_genus, useNA = 'always')

# inspect large genus tree dist
old_and_new_database_results |> 
  filter(is.na(taxid_exact)) |> 
  filter(tree_dist_genus >= 3) |> 
  View()

# inspect NA genus tree dists
old_and_new_database_results |> 
  filter(is.na(taxid_exact)) |> 
  filter(is.na(tree_dist_genus)) |> 
  View()

# prefer exact taxid and add distances to genus
old_and_new_database_results <- old_and_new_database_results |> 
  mutate(taxid = coalesce(taxid_exact, taxid)) |>
  add_tree_dist_genus()

table(old_and_new_database_results$tree_dist_genus, useNA = 'always')

# double check that have results for all studies
stopifnot(setequal(
  old_and_new_database_results$Number,
  c(old_database$Number, new_database$Number)
))


# Sarah's Work (2) ----

sarahs_db2 <- process_sarahs_db2(microbe$`Sarah's Work (2`)

# extract annotated taxon id <--> Genus species for checking later
annotated_names <- rbind_taxdbs(sarahs_db2) |> 
  select(`Genus w/ numerical values`, 
         `Species w/ numerical values - condensed`, 
         `Taxon ID`)  |> 
  purrr::set_names(c("genus_annot", "species_annot", "taxid_annot")) |> 
  filter(!is.na(taxid_annot)) |> 
  distinct()


sarahs_db2 <- rbind_taxdbs(sarahs_db2) |>
  select(Family:Species, Number, direction) |> 
  # remove rows without taxon info
  filter(!if_all(Family:Species, is.na)) |>
  # keep for joining with annotated taxids later
  mutate(
    species_annot = Species,
    genus_annot = Genus,
  ) |> 
  # expand rows with " and " in Species column
  tidyr::separate_rows(Species, sep = " and ") |> 
  # remove Genus copied to Species column
  mutate(Species = stringr::str_remove(
    Species, 
    paste0("^", stringr::str_escape(Genus), " "))
  ) |> 
  distinct()

# get "Genus species" and "Genus"
sarahs_db2 <- sarahs_db2 |>
  mutate(
    most_specific = most_specific_name_base(sarahs_db2),
    next_most_specific = most_specific_name_base(sarahs_db2, exclude_species = TRUE))

# clean up names for OLS queries
sarahs_db2_queries <- sarahs_db2 |> 
  select(most_specific, next_most_specific, genus_annot, species_annot, Number, direction) |> 
  mutate(
    most_specific = clean_names_sarah(most_specific),
    next_most_specific = clean_names_sarah(next_most_specific)
  ) |> 
  # expand rows with multiple oral taxon listed together
  mutate(most_specific = expand_oral_taxon(most_specific)) |> 
  mutate(most_specific = expand_genus_suffix_oral_taxon(most_specific)) |> 
  mutate(most_specific = expand_double_species_oral_taxon(most_specific)) |> 
  tidyr::separate_rows(most_specific, sep = ';')

# query OLS
sarahs_db2_distinct_queries <- sarahs_db2_queries |> 
  select(most_specific, next_most_specific) |> 
  distinct()

sarahs_db2_results <- run_ncbitaxon_queries(sarahs_db2_distinct_queries)

sarahs_db2_results_backup <- sarahs_db2_results
# sarahs_db2_results <- sarahs_db2_results_backup

# identify queries with wrong oral taxon mapping
sarahs_db2_results <- sarahs_db2_results |> 
  mutate(
    taxname_ot = str_extract(taxname, "oral taxon \\d+"),
    taxname_ot_wrong = !is.na(taxname_ot) &
      !str_detect(tolower(query), fixed(taxname_ot))
  )

# inspect them
sarahs_db2_results |> 
  filter(taxname_ot_wrong) |> 
  View()

# extract them and run the queries without oral taxon identifier
rerun_df <- sarahs_db2_results |> 
  filter(taxname_ot_wrong) |> 
  mutate(most_specific = str_trim(str_remove(query, "oral taxon \\d+"))) |> 
  select(most_specific, next_most_specific) |> 
  run_ncbitaxon_queries()

# inspect results
rerun_df |> View()

# replace results
wrong_idx <- which(sarahs_db2_results$taxname_ot_wrong)
sarahs_db2_results[wrong_idx, names(rerun_df)] <- rerun_df


# manually check results where Domain isn't Bacteria
domain <- get_rank_vals(sarahs_db2_results)
sarahs_db2_results[domain != 'Bacteria', ]


# fix results where Domain is not Bacteria
# as well as other manually identified errors
sarahs_db2_results <- sarahs_db2_results |> 
  mutate(
    taxname_fixed = case_match(
      query,
      # not domain Bacteria
      
      'Human oral sp.' ~ 'human oral bacterium C20',
      'Leptothrix' ~ 'Leptothrix sp. (in: b-proteobacteria)',
      'sp. ot131' ~ 'Acidaminococcaceae',
      'sp. ot274' ~ 'Bacteroidetes oral taxon 274',
      'Streptococcus sanguis' ~ 'Streptococcus sanguinis',
      '- SR1 AF125207' ~ 'Candidatus Absconditibacteriota',
      'SR1 AF125207' ~ 'Candidatus Absconditibacteriota',
      
      # other identified errors
      
      'Unclassified FX006 -' ~ 'Comamonadaceae',
      'Uncultured human sp.' ~ 'uncultured human oral bacterium A27',
      'TM7 sp. oral taxon 238' ~ 'unclassified Candidatus Saccharimonadota',
      'TM7 401H12' ~ 'unclassified Candidatus Saccharimonadota',
      'Lactobacillus colehominis' ~ 'Limosilactobacillus coleohominis',
      'Chloroflexi sp. oral taxon 347' ~ 'unclassified Chloroflexota',
      'Firmicutes sp.' ~ 'Firmicutes oral clone F058',
    )
  )

# extract them and run the queries
rerun_df <- sarahs_db2_results |> 
  filter(!is.na(taxname_fixed)) |> 
  mutate(most_specific = taxname_fixed) |> 
  select(most_specific, next_most_specific) |> 
  run_ncbitaxon_queries()

# inspect results
rerun_df

# replace results
fixed_idx <- which(!is.na(sarahs_db2_results$taxname_fixed))
sarahs_db2_results[fixed_idx, names(rerun_df)] <- rerun_df

# add exact taxizedb results
sarahs_db2_results <- add_exact_taxids(sarahs_db2_results)

# add distances between taxids and genus
sarahs_db2_results <- add_tree_dist_genus(sarahs_db2_results)

table(sarahs_db2_results$tree_dist_genus, useNA = 'always')

# inspect large tree dists
sarahs_db2_results |> 
  filter(is.na(taxid_exact)) |> 
  filter(tree_dist_genus >= 8) |> 
  View()

# re-run without oral taxon when large genus tree dist
rerun_df <- sarahs_db2_results |> 
  filter(is.na(taxid_exact)) |> 
  filter(tree_dist_genus >= 8) |> 
  mutate(most_specific = str_trim(str_remove(query, "oral taxon .+?$"))) |> 
  mutate(most_specific = str_trim(str_remove(most_specific, "strain .+?$"))) |> 
  select(most_specific, next_most_specific) |> 
  run_ncbitaxon_queries()

rerun_df

fixed_idx <- which(
  is.na(sarahs_db2_results$taxid_exact) & 
    sarahs_db2_results$tree_dist_genus >= 8)

sarahs_db2_results[fixed_idx, names(rerun_df)] <- rerun_df

# add exact taxizedb results
sarahs_db2_results <- add_exact_taxids(sarahs_db2_results)

# prefer exact taxid and add distances to genus
sarahs_db2_results <- sarahs_db2_results |> 
  mutate(taxid = coalesce(taxid_exact, taxid)) |>
  select(query, next_most_specific, taxname, taxid) |> 
  add_tree_dist_genus()

table(sarahs_db2_results$tree_dist_genus, useNA = 'always')

# inspect NA genus tree dists
sarahs_db2_results |> 
  filter(is.na(tree_dist_genus)) |> 
  View()

# join distinct query results back to non-distinct
nrow(sarahs_db2_results) == nrow(sarahs_db2_distinct_queries)

# restore most_specific as it was mutated a bunch
sarahs_db2_results$most_specific <- 
  sarahs_db2_distinct_queries$most_specific

sarahs_db2_results <- sarahs_db2_results |>
  right_join(sarahs_db2_queries)

# double check that have all studies
stopifnot(setequal(
  sarahs_db2_results$Number,
  sarahs_db2$Number
))

# check concordance with collaborator annotated ----

# join results back to annotated names
annotated_names <- annotated_names |> 
  left_join(
    sarahs_db2_results |> 
      select(taxid, genus_annot, species_annot) |> 
      distinct(),
    relationship = 'many-to-many'
  )

# calculate tree distance
annotated_names <- annotated_names |> 
  rowwise() |> 
  mutate(
    annot_tree_dist = ifelse(
      !is.na(taxid_annot),
      taxonomic_tree_distance(taxid, taxid_annot),
      NA
    ))

# tabulate tree distance values
annotated_names |> 
  distinct() |> 
  filter(!is.na(annot_tree_dist)) |> 
  pull(annot_tree_dist) |>
  table()

# length = 584
#
#   0   1   2   3   4   5   6   7  12  14 
# 328  33 129  35  17  23  12   3   2   2

# 14 more results (probably line splits)
# 0: 99 more
# 1: 23 less
# 2: 68 less
# 3: same
# 4: 6 less
# 5: 5 more
# 6: 8 more
# 7: same


# Old DATABASE and Sarah's Work (2): merge all and save ---

# have taxids for all
sum(is.na(sarahs_db2_results$taxid))
sum(is.na(old_and_new_database_results$taxid))

diff_species <- rbind(
  old_and_new_database_results |> select('Number', 'taxid', 'direction'),
  sarahs_db2_results |> select('Number', 'taxid', 'direction')
) |> distinct()

stopifnot(setequal(
  diff_species$Number,
  c(old_database$Number, new_database$Number, sarahs_db2$Number)
))

saveRDS(diff_species, 'output/diff_species.rds')

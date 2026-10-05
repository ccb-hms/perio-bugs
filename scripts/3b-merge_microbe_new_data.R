library(dplyr)
library(tidyr)
library(stringr)

# extracts differentially abundant taxa for 2022-2025 studies
# NOTE: unlike other studies, taxa for these are not in 'Cleaned Micro List RY_final.xlsx'
# instead taxa are parsed from the free text "Microbiological assessment" sheet

source('scripts/microbe_helpers.R')

overview <- readRDS('output/overview_merged.rds')

# setup -----

taxa_cols <- c(
  'Elevated in health - Phylum', 'Elevated in health - Genera', 'Elevated in health - Species',
  'Elevated in periodontitis - Phylum', 'Elevated in periodontitis - Genera', 'Elevated in periodontitis - Species'
)

new_studies <- overview |>
  filter(grepl('^2025-', Number)) |>
  select(Number, all_of(taxa_cols))

# one row per study, direction, and cell of free text
new_taxa_text <- new_studies |>
  pivot_longer(-Number, names_to = 'column', values_to = 'text') |>
  mutate(
    # up: elevated in periodontitis (Group 1)
    direction = ifelse(grepl('periodontitis', column), 'up', 'dn')
  ) |>
  select(-column)

# manually curated studies ----
# taxa are reported as sentences rather than lists

manual_studies <- c('2025-246', '2025-PubMed-357', '2025-PubMed-504')

manual_taxa <- tribble(
  ~Number, ~direction, ~query,
  # "All the species that were assessed presented higher levels in periodontitis"
  '2025-246', 'up', 'Aggregatibacter actinomycetemcomitans',
  '2025-246', 'up', 'Eubacterium nodatum',
  '2025-246', 'up', 'Fusobacterium nucleatum',
  '2025-246', 'up', 'Porphyromonas gingivalis',
  '2025-246', 'up', 'Treponema denticola',
  '2025-246', 'up', 'Tannerella forsythia',
  '2025-246', 'up', 'Desulfobulbus oralis',
  '2025-246', 'up', 'Eubacterium brachy',
  '2025-246', 'up', 'Eubacterium saphenum',
  '2025-246', 'up', 'Filifactor alocis',

  # only explicitly named taxa (full list in supplementary data)
  '2025-PubMed-357', 'dn', 'Actinomyces oris',
  '2025-PubMed-357', 'dn', 'Rothia dentocariosa',
  '2025-PubMed-357', 'dn', 'Rothia mucilaginosa',
  '2025-PubMed-357', 'dn', 'Streptococcus sanguinis',
  '2025-PubMed-357', 'dn', 'Haemophilus parainfluenzae',
  '2025-PubMed-357', 'up', 'Porphyromonas gingivalis',
  '2025-PubMed-357', 'up', 'Tannerella forsythia',

  # bacteria enriched vs either periodontitis group (Stage III Grade B or C)
  # consistent with other studies where taxa from all periodontitis subgroups are included
  # excludes viruses, archaea, and fungi
  '2025-PubMed-504', 'dn', 'Actinobacteria',
  '2025-PubMed-504', 'dn', 'Corynebacterium',
  '2025-PubMed-504', 'dn', 'Actinomyces',
  '2025-PubMed-504', 'dn', 'Cardiobacterium',
  '2025-PubMed-504', 'dn', 'Selenomonas',
  '2025-PubMed-504', 'dn', 'Corynebacterium matruchotii',
  '2025-PubMed-504', 'dn', 'Actinomyces naeslundii',
  '2025-PubMed-504', 'dn', 'Cardiobacterium hominis',
  '2025-PubMed-504', 'dn', 'Selenomonas noxia',  # vs Grade B only
  '2025-PubMed-504', 'up', 'Bacteroidetes',
  '2025-PubMed-504', 'up', 'Spirochaetes',
  '2025-PubMed-504', 'up', 'Treponema',
  '2025-PubMed-504', 'up', 'Porphyromonas',
  '2025-PubMed-504', 'up', 'Prevotella',
  '2025-PubMed-504', 'up', 'Porphyromonas gingivalis',
  '2025-PubMed-504', 'up', 'Prevotella intermedia',
  '2025-PubMed-504', 'up', 'Treponema denticola',
  '2025-PubMed-504', 'up', 'Tannerella forsythia',
  '2025-PubMed-504', 'up', 'Neisseria sicca',
  '2025-PubMed-504', 'up', 'Treponema vincentii',
  '2025-PubMed-504', 'up', 'Treponema medium',
  '2025-PubMed-504', 'up', 'Porphyromonas endodontalis',
  '2025-PubMed-504', 'up', 'Prevotella nigrescens',  # Grade B only
  '2025-PubMed-504', 'up', 'Treponema sp. OMZ 804',  # Grade B only
  '2025-PubMed-504', 'up', 'Capnocytophaga granulosa',  # Grade C only
  '2025-PubMed-504', 'up', 'Capnocytophaga sp. CM59'  # Grade C only
)

# parse free text lists ----

not_reported <- c('NR', 'ND', 'NR/ND', 'NA')

parsed_taxa <- new_taxa_text |>
  filter(!Number %in% manual_studies, !is.na(text)) |>
  mutate(
    text = text |>
      # drop details on following lines (e.g. '[DETAILS: ...')
      str_remove('\n[\\s\\S]*$') |>
      # drop notes (e.g. '[ANCOM-BC adjusted for ...]')
      str_remove_all('\\s*\\[ANCOM[^]]*\\]') |>
      str_replace_all('sp\\.\\. ', 'sp., ') |>
      # missing separator (e.g. 'Bacillota (Firmicutes) Bacteroidota')
      str_replace_all('\\) (?=[A-Z])', '), ') |>
      str_trim()
  ) |>
  filter(!text %in% not_reported) |>
  # one row per taxon
  separate_rows(text, sep = '[,;]| and ') |>
  mutate(text = str_trim(str_remove(text, '\\.$'))) |>
  filter(text != '')

# cleanup names for queries ----

clean_names_new <- function(x) {
  x |>
    # synonyms in brackets (e.g. 'Bacillota (Firmicutes)')
    str_remove('\\s*\\((?![^)]*\\d)[^)]*\\)') |>
    # HOMD human microbial taxon (e.g. 'sp. HMT 322', '(HMT 439)')
    str_replace('\\(?HMT[- ]?(\\d+)\\)?', 'oral taxon \\1') |>
    str_replace('orral taxon', 'oral taxon') |>
    # redundant for named species (e.g. 'Anaeroglobus geminatus oral taxon 439')
    str_replace('^([A-Z][a-z]+ (?!bacterium)[a-z]+) oral taxon \\d+$', '\\1') |>
    # unclassified species (e.g. 'Streptococcus NA', 'Treponema spp.', 'Acidovorax sp.')
    str_remove(' (NA|spp?\\.?)$') |>
    # HOMD unnamed groups (e.g. 'Lachnospiraceae [G-8] bacterium oral taxon 500')
    str_remove_all('\\s*\\[(XI|G-\\d+)\\]') |>
    # HOMD names for [Eubacterium] species (e.g. 'Peptostreptococcaceae [XI][G-5] [Eubacterium] saphenum')
    str_replace('^Peptostreptococcaceae \\[Eubacterium\\] (\\w+)$', 'Eubacterium \\1') |>
    # expand e.g. 'Veillonella parvula/dispar'
    str_replace('^(\\w+) (\\w+)/(\\w+)$', '\\1 \\2;\\1 \\3') |>
    str_squish()
}

parsed_taxa <- parsed_taxa |>
  mutate(query = clean_names_new(text)) |>
  separate_rows(query, sep = ';') |>
  mutate(
    query = case_match(
      query,
      # typos
      'Anaeroglobus germinatus' ~ 'Anaeroglobus geminatus',
      'Leptrotrichia sp. oral taxon 417' ~ 'Leptotrichia sp. oral taxon 417',

      # HOMD names for [Eubacterium] species
      'Eubacterium brachy group brachy' ~ 'Eubacterium brachy',
      c('Eubacterium saphenum group saphenum', 'Peptostreptococcaceae_saphenum') ~ 'Eubacterium saphenum',
      # i.e. 'Peptostreptococcaceae [G-6] bacterium (nodatum)'
      'Peptostreptococcaceae bacterium' ~ 'Eubacterium nodatum',

      # no oral taxon number
      'Selenomonas sp. oral taxon' ~ 'Selenomonas',

      # NCBI names for HOMD taxa
      'Bacteroidaceae bacterium oral taxon 272' ~ 'Bacteroidetes bacterium oral taxon 272',
      'Bacteroidales bacterium oral taxon 274' ~ 'Bacteroidetes bacterium oral taxon 274',
      'Stomatobaculum sp. oral taxon 373' ~ 'Lachnospiraceae bacterium oral taxon 373',
      'Fretibacterium sp. oral taxon 360' ~ 'Synergistetes bacterium oral taxon 360',
      'Fretibacterium sp. oral taxon 361' ~ 'Synergistetes bacterium oral taxon 361',
      'Peptoniphilaceae bacterium oral taxon 113' ~ 'Peptostreptococcaceae bacterium oral taxon 113',

      # SILVA names
      'Rikenellaceae RC9' ~ 'Rikenellaceae',
      'Actinomycetaceae F0332' ~ 'Actinomycetaceae',
      .default = query
    )
  )

# combine with manually curated ----

new_queries <- bind_rows(
  select(parsed_taxa, Number, direction, query),
  manual_taxa
) |>
  distinct()

# get NCBI taxonomy ids ----

distinct_queries <- distinct(new_queries, query)

# prefer exact match in local NCBI taxonomy, otherwise query OLS
distinct_queries$taxid_exact <- sapply(distinct_queries$query, get_exact_taxid, USE.NAMES = FALSE)

ols_queries <- distinct_queries |>
  filter(is.na(taxid_exact)) |>
  pull(query)

# NA if no results
query_ncbitaxon_safe <- function(taxon_name) {
  res <- tryCatch(query_ncbitaxon(taxon_name), error = function(e) NULL)
  if (is.null(res) || !nrow(res))
    res <- data.frame(query = taxon_name, taxid = NA_character_, taxname = NA_character_)
  res
}

ols_results <- purrr::map_dfr(ols_queries, query_ncbitaxon_safe)

# names with multiple NCBI matches (e.g. genus homonyms) or old phylum names
taxid_fixes <- c(
  'Rothia' = '32207',
  'Kingella' = '32257',
  'Bacteroidetes' = '976',
  'Chloroflexi' = '200795',
  'Spirochaetes' = '203691'
)

distinct_queries <- distinct_queries |>
  left_join(select(ols_results, query, taxid_ols = taxid, taxname_ols = taxname), by = 'query') |>
  mutate(
    taxid_fixed = unname(taxid_fixes[query]),
    taxid = coalesce(taxid_fixed, as.character(taxid_exact), taxid_ols)
  )

# inspect OLS results
distinct_queries |>
  filter(is.na(taxid_exact), is.na(taxid_fixed)) |>
  select(query, taxname_ols, taxid_ols) |>
  print(n = Inf)

stopifnot(!anyNA(distinct_queries$taxid))

# check that all are bacteria
distinct_queries$domain <- get_rank_vals(distinct_queries, 'domain')
stopifnot(all(distinct_queries$domain == 'Bacteria'))

# keep most specific taxa per signature ----
# e.g. drop genus Porphyromonas if Porphyromonas gingivalis is also reported
# consistent with curated taxa for other studies

new_species <- new_queries |>
  left_join(select(distinct_queries, query, taxid), by = 'query') |>
  distinct(Number, direction, taxid)

lineages <- taxizedb::classification(unique(new_species$taxid), db = 'ncbi')

# taxids that are ancestors of taxid
get_ancestors <- function(taxid) {
  lineage <- lineages[[taxid]]
  setdiff(lineage$id, taxid)
}

diff_species_new <- new_species |>
  group_by(Number, direction) |>
  filter(!taxid %in% unlist(lapply(taxid, get_ancestors))) |>
  ungroup()

# number of taxa reported and kept per signature
new_species |>
  count(Number, direction, name = 'n_reported') |>
  left_join(count(diff_species_new, Number, direction, name = 'n_kept'), by = c('Number', 'direction')) |>
  print(n = Inf)

stopifnot(setequal(diff_species_new$Number, new_studies$Number))

saveRDS(diff_species_new, 'output/diff_species_new_data.rds')

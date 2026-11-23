# NCBI Taxonomy IDs for the signatures of the studies added from the full-text review
# (scripts/llm_annotation/added_studies.R)
#
# usage: Rscript scripts/llm_annotation/resolve_added_taxa.R
# reads output/llm_annotation/added/Study <ID>_taxa.csv, one per study, written with the extraction: for the
# studies the curators extracted, their taxa list checked against the paper; for the others, the signature
# taken from the paper. Writes output/llm_annotation/added/taxa.csv, one row per taxon and direction.
#
# A name is looked up as a scientific name, then among all names (synonyms, e.g. "Bacteroidetes"), and must
# match exactly one taxon. Names that don't are set by hand in `overrides`. Taxa the extraction found in the
# paper but the curators did not list ("MISSING FROM CURATED LIST") are kept out of the signature, as are taxa
# set to NA in `overrides`; added_studies.R logs them for curators.

library(dplyr)
library(purrr)
library(readr)
library(rentrez)

dir <- 'output/llm_annotation/added'

# taxon names NCBI does not resolve to one taxon, set by hand (NA: no NCBI taxon; left out of the signature)
overrides <- c(
  'TM7' = '95818',                         # Candidatus Saccharibacteria
  'Candidatus Saccharibacteria' = '95818',
  'TM7_genera incertae sedis' = '95818',
  'GN02' = '363464',                       # Candidatus Altimarinota (candidate division GN02)
  # renamed species and HOMD oral taxa under their NCBI names
  'Mycoplasma faucium' = '56142',          # Metamycoplasma faucium
  'Fusobacterium nucleatum subsp. polymorphum' = '76857',  # Fusobacterium polymorphum
  'Actinomyces meyeri' = '52773',          # Schaalia meyeri
  'Bosea' = '85413',                       # Allobosea
  'Stomatobaculum sp. oral taxon 373' = '712987',  # Lachnospiraceae bacterium oral taxon 373
  'Lachnospiraceae sp. oral taxon 500' = '712991', # Lachnospiraceae bacterium oral taxon 500
  'Fretibacterium sp. oral taxon 359' = '713033',  # Synergistetes bacterium oral taxon 359
  'Fretibacterium sp. oral taxon 360' = '713034',  # Synergistetes bacterium oral taxon 360
  'Peptoniphilaceae sp. oral taxon 113' = '1321783',  # Peptostreptococcaceae bacterium oral taxon 113
  'Peptostreptococcaceae sp. oral taxon 950' = '543314' # HOMD: Anaerovoracaceae [G-3] bacterium HMT-950
)

taxa <- list.files(dir, pattern = '_taxa[.]csv$', full.names = TRUE) |>
  map_dfr(\(f) read_csv(f, col_types = cols(.default = 'c')) |>
            mutate(Study = sub('_taxa[.]csv$', '', basename(f)))) |>
  mutate(taxon_name = trimws(taxon_name),
         curated_missing = grepl('^MISSING FROM CURATED LIST', coalesce(note, '')))

# bacteria and archaea only: several genus names are shared with plants or animals (Rothia, Kingella, Yersinia)
search <- function(name, field) {
  term <- sprintf('"%s"[%s] AND (Bacteria[Subtree] OR Archaea[Subtree])', name, field)
  for (attempt in 1:5) {
    Sys.sleep(0.4 * attempt)  # NCBI allows three requests a second without a key; back off when refused
    ids <- tryCatch(entrez_search('taxonomy', term)$ids, error = \(e) if (grepl('429', conditionMessage(e))) NULL else stop(e))
    if (!is.null(ids)) return(ids)
  }
  stop('NCBI kept refusing requests (HTTP 429)')
}

#' One NCBI ID for a name: an exact scientific name, then any name (synonyms such as "Bacteroidetes"); where
#' several taxa share the name, the one at the reported rank.
lookup <- function(name, rank) {
  if (name %in% names(overrides)) return(unname(overrides[name]))
  for (field in c('Scientific Name', 'All Names')) {
    ids <- search(name, field)
    if (length(ids) == 1) return(ids)
    if (length(ids) > 1) {
      Sys.sleep(0.4)
      ranks <- map_chr(entrez_summary('taxonomy', ids, always_return_list = TRUE), 'rank')
      at_rank <- ids[ranks == tolower(rank)]
      return(if (length(at_rank) == 1) at_rank else NA_character_)
    }
  }
  NA_character_
}

# names without an NCBI taxon (e.g. HOMD oral taxa NCBI lacks) fall back to their genus, as the curated
# taxa do (scripts/3-merge_microbe_classic.R reruns such queries without the oral taxon)
names_found <- distinct(taxa, taxon_name, rank) |>
  mutate(taxid = map2_chr(taxon_name, rank, lookup),
         genus = ifelse(is.na(taxid) & grepl(' ', taxon_name), sub(' .*', '', taxon_name), NA),
         taxid = coalesce(taxid, map_chr(genus, \(g) if (is.na(g)) NA_character_ else lookup(g, 'genus'))),
         fallback = ifelse(!is.na(genus) & !is.na(taxid), paste(genus, '(no NCBI taxon for the full name)'), NA)) |>
  select(-genus) |>
  distinct(taxon_name, .keep_all = TRUE)

found <- filter(names_found, !is.na(taxid))
summaries <- entrez_summary('taxonomy', unique(found$taxid), always_return_list = TRUE)
ncbi <- tibble(taxid = names(summaries),
               ncbi_name = map_chr(summaries, 'scientificname'),
               ncbi_rank = map_chr(summaries, 'rank'))

resolved <- taxa |>
  left_join(select(names_found, -rank), by = 'taxon_name') |>
  left_join(ncbi, by = 'taxid') |>
  mutate(include = !curated_missing & !is.na(taxid)) |>
  select(Study, direction, taxon_reported, taxon_name, taxid, ncbi_name, ncbi_rank, include, everything(),
         -curated_missing)

unresolved <- filter(resolved, is.na(taxid), !grepl('^MISSING FROM CURATED LIST', coalesce(note, '')))
if (nrow(unresolved)) {
  message('taxa without an NCBI ID (add to overrides):')
  print(distinct(unresolved, Study, taxon_name))
}

write_csv(resolved, file.path(dir, 'taxa.csv'), na = '')
print(count(resolved, Study, direction, include))

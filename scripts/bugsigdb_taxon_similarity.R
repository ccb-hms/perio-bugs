# Lin similarity of each BugSigDB taxon to its closest taxon in each PerioSigDB consensus signature
# used by reports/metadata_options/report.qmd (Option 14, Version C2 taxa by condition)
#
# run after scripts/bugsigdb_semantic_similarity.R (uses its consensus signatures)

library(dplyr)
library(purrr)
library(ontologySimilarity)

semsim <- readRDS('output/bugsigdb_semantic_similarity.rds')

options(timeout = 1200)
onto <- BugSigDBStats::getNcbiTaxonomyObo()
ic <- descendants_IC(onto)

terminal_ids <- function(x) paste0('NCBITaxon:', unique(sub('^.*[|]', '', x)))

bsdb <- bugsigdbr::importBugSigDB()
bsdb <- filter(bsdb, !grepl('periodont', Condition, ignore.case = TRUE))
taxa <- unique(unlist(map(bsdb$`NCBI Taxonomy IDs`, terminal_ids)))
taxa <- intersect(taxa, onto$id)

consensus <- unlist(semsim$consensus)
sim <- get_term_sim_mat(onto, ic, method = 'lin', row_terms = taxa, col_terms = consensus)

closest <- function(ids) {
  s <- sim[, ids, drop = FALSE]
  tibble(taxon = taxa, match = ids[max.col(s, ties.method = 'first')], sim = apply(s, 1, max))
}

up <- closest(semsim$consensus$increased)
down <- closest(semsim$consensus$decreased)

taxon_similarity <- tibble(
  taxon = taxa,
  name = onto$name[taxa],
  match_up = onto$name[up$match], sim_up = up$sim,
  match_down = onto$name[down$match], sim_down = down$sim
)

# lineage (root to taxon) for ordering taxa taxonomically in plots
# (positional indexing: name lookups in the full taxonomy are slow)
all_ids <- union(taxa, consensus)
pos <- match(all_ids, onto$id)
anc <- onto$ancestors[pos]
anc_ids <- unique(unlist(anc))
anc_pos <- match(anc_ids, onto$id)
depth <- setNames(lengths(onto$ancestors[anc_pos]), anc_ids)
anc_name <- setNames(onto$name[anc_pos], anc_ids)
lineage <- setNames(map_chr(anc, \(a) paste(anc_name[a[order(depth[a])]], collapse = '|')), all_ids)

saveRDS(
  list(closest = taxon_similarity, sim = sim, lineage = lineage,
       names = setNames(onto$name[pos], all_ids)),
  'output/bugsigdb_taxon_similarity.rds'
)

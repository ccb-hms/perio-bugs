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

closest <- function(consensus) {
  sim <- get_term_sim_mat(onto, ic, method = 'lin', row_terms = taxa, col_terms = consensus)
  tibble(taxon = taxa, match = consensus[max.col(sim, ties.method = 'first')], sim = apply(sim, 1, max))
}

up <- closest(semsim$consensus$increased)
down <- closest(semsim$consensus$decreased)

taxon_similarity <- tibble(
  taxon = taxa,
  name = onto$name[taxa],
  match_up = onto$name[up$match], sim_up = up$sim,
  match_down = onto$name[down$match], sim_down = down$sim
)

saveRDS(taxon_similarity, 'output/bugsigdb_taxon_similarity.rds')

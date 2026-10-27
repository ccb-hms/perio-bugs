# pairwise semantic similarity between all PerioSigDB and human BugSigDB signatures with at least
# 5 taxa, for testing how well each condition's signatures replicate across studies (as in
# Figure 2 of the BugSigDB paper, Geistlinger et al. 2024)
# used by reports/metadata_options/report.qmd ("How consistent are periodontitis signatures?")
#
# similarity: best-match average of Lin similarity on the NCBI taxonomy, at the reported rank

library(dplyr)
library(purrr)
library(ontologySimilarity)

# TODO: use output/perio_bugs.rds once manual corrections are confirmed by curators
dat <- readRDS('output/perio_bugs_corrected.rds')

options(timeout = 1200)
onto <- BugSigDBStats::getNcbiTaxonomyObo()
ic <- descendants_IC(onto)

terminal_ids <- function(x) intersect(paste0('NCBITaxon:', unique(sub('^.*[|]', '', x))), onto$id)

bsdb <- bugsigdbr::importBugSigDB()
bsdb <- filter(bsdb, `Host species` == 'Homo sapiens', !grepl('periodont', Condition, ignore.case = TRUE))

ids <- c(
  setNames(map(dat$`NCBI Taxonomy IDs`, terminal_ids), paste0('perio:', seq_len(nrow(dat)))),
  setNames(map(bsdb$`NCBI Taxonomy IDs`, terminal_ids), bsdb$`BSDB ID`)
)
ids <- ids[lengths(ids) >= 5]

sim <- get_sim_grid(ontology = onto, information_content = ic, term_sim_method = 'lin', term_sets = ids)

saveRDS(sim, 'output/bugsigdb_replicability_sim.rds')

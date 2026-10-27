# Lin similarity between all taxa in PerioSigDB and human oral BugSigDB signatures, for comparing
# sample size-weighted metasignatures of oral conditions (as in Figure 4 of the BugSigDB paper,
# Geistlinger et al. 2024)
# used by reports/metadata_options/report.qmd ("Where does periodontitis sit among oral conditions?")

library(dplyr)
library(purrr)
library(ontologySimilarity)

# TODO: use output/perio_bugs.rds once manual corrections are confirmed by curators
dat <- readRDS('output/perio_bugs_corrected.rds')

options(timeout = 1200)
onto <- BugSigDBStats::getNcbiTaxonomyObo()
ic <- descendants_IC(onto)

terminal_ids <- function(x) paste0('NCBITaxon:', unique(sub('^.*[|]', '', x)))

oral_sites <- 'plaque|saliva|mouth|oral|tongue|gingiv|throat|oroph|tonsil|buccal|cheek'
bsdb <- bugsigdbr::importBugSigDB()
bsdb <- filter(bsdb, `Host species` == 'Homo sapiens', grepl(oral_sites, `Body site`, ignore.case = TRUE))

taxa <- unique(unlist(map(c(dat$`NCBI Taxonomy IDs`, bsdb$`NCBI Taxonomy IDs`), terminal_ids)))
taxa <- intersect(taxa, onto$id)

sim <- get_term_sim_mat(onto, ic, method = 'lin', row_terms = taxa, col_terms = taxa)

saveRDS(sim, 'output/oral_taxon_similarity.rds')

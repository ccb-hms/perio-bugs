# semantic similarity between PerioSigDB consensus signatures and BugSigDB signatures
# used by reports/metadata_options/report.qmd (Option 14, Version C)
#
# PerioSigDB consensus signatures: taxa (at their reported rank) reported significantly more
# often in one direction than the other (exact binomial test, FDR < 0.05)
#
# similarity: best-match average of Lin similarity on the NCBI taxonomy (as in
# BugSigDBStats::calcSemanticSimilarity), so a BugSigDB genus partly matches a PerioSigDB species
# of that genus
#
# takes ~20 minutes; the NCBI taxonomy ontology (~130 MB) is downloaded once and cached

library(dplyr)
library(purrr)
library(tidyr)
library(ontologySimilarity)

# taxa (or genera) count as periodontitis-associated at FDR < 0.05 (Benjamini-Hochberg). PERIO_P05=1 uses
# unadjusted p < 0.05 instead and writes a _p05 copy of the output, to check the results do not depend on the cut-off
fdr_only <- Sys.getenv('PERIO_P05') != '1'
suffix <- if (fdr_only) '' else '_p05'

# TODO: use output/perio_bugs.rds once manual corrections are confirmed by curators
dat <- readRDS('output/perio_bugs_corrected.rds')
# studies the full-text review proposes excluding (not a health vs periodontitis comparison)
dat <- dat[!grepl('^proposed exclusion', dat$`Review status`), ]

options(timeout = 1200)
onto <- BugSigDBStats::getNcbiTaxonomyObo()
ic <- descendants_IC(onto)

# consensus signatures ----

terminal_ids <- function(x) paste0('NCBITaxon:', unique(sub('^.*[|]', '', x)))

taxon_tests <- tibble(direction = dat$`Abundance in Group 1`, ids = map(dat$`NCBI Taxonomy IDs`, terminal_ids)) |>
  mutate(sig = row_number()) |>
  unnest(ids) |>
  count(ids, direction) |>
  pivot_wider(names_from = direction, values_from = n, values_fill = 0) |>
  mutate(
    n = increased + decreased,
    p = map2_dbl(increased, n, \(k, n) binom.test(k, n, p = 0.5)$p.value),
    fdr = p.adjust(p, method = 'BH')
  )
cut_p <- if (fdr_only) taxon_tests$fdr else taxon_tests$p

consensus <- list(
  increased = with(taxon_tests, ids[cut_p < 0.05 & increased > decreased]),
  decreased = with(taxon_tests, ids[cut_p < 0.05 & decreased > increased])
) |>
  lapply(intersect, onto$id)

# BugSigDB signatures (all except periodontitis) ----

bsdb <- bugsigdbr::importBugSigDB()
bsdb <- filter(bsdb, !grepl('periodont', Condition, ignore.case = TRUE))

bsdb_ids <- map(bsdb$`NCBI Taxonomy IDs`, \(x) intersect(terminal_ids(x), onto$id))
has_taxa <- lengths(bsdb_ids) > 0

sim <- get_sim_grid(
  ontology = onto, information_content = ic, term_sim_method = 'lin',
  term_sets = consensus, term_sets2 = bsdb_ids[has_taxa]
)

semantic_similarity <- tibble(
  `BSDB ID` = bsdb$`BSDB ID`[has_taxa],
  sim_increased = sim['increased', ],
  sim_decreased = sim['decreased', ]
)

saveRDS(
  list(similarity = semantic_similarity, consensus = consensus, taxon_tests = taxon_tests),
  paste0('output/bugsigdb_semantic_similarity', suffix, '.rds')
)

# prevalence of each genus in healthy oral samples from the Human Microbiome Project (16S V3-V5),
# for contrasting with how often genera are reported decreased (as in Figure 4 of the BugSigDB paper)
# also Spearman correlation between genera in healthy subgingival plaque, for contrasting with how often
# genera are reported together (as in Figure 4 of the BugSigDB paper)
# used by reports/metadata_options/report.qmd ("Are the decreased genera simply common oral taxa?",
# "Which periodontitis genera are reported together?")
#
# prevalence: share of samples with at least one read of the genus, and (as most oral genera are in
# nearly every sample) with at least 0.1% relative abundance; first visit per participant

library(dplyr)
library(SummarizedExperiment)

hmp <- HMP16SData::V35()

sites <- c('Subgingival Plaque', 'Supragingival Plaque', 'Saliva')
hmp <- hmp[, hmp$HMP_BODY_SUBSITE %in% sites & hmp$VISITNO == 1]

genus <- rowData(hmp)$GENUS
counts <- assay(hmp)
keep <- !is.na(genus) & genus != ''
genus_counts <- rowsum(counts[keep, ], genus[keep])

prevalence <- lapply(sites, \(site) {
  x <- genus_counts[, hmp$HMP_BODY_SUBSITE == site, drop = FALSE]
  rel <- sweep(x, 2, colSums(counts[, colnames(x)]), '/')
  tibble(site = site, taxon = rownames(x), prevalence = rowMeans(x > 0),
         prevalence_01 = rowMeans(rel >= 0.001), mean_abundance = rowMeans(rel), samples = ncol(x))
}) |>
  bind_rows()

saveRDS(prevalence, 'output/hmp_oral_prevalence.rds')

# genera with at least 0.1% relative abundance in at least 10% of subgingival plaque samples
x <- genus_counts[, hmp$HMP_BODY_SUBSITE == 'Subgingival Plaque']
rel <- sweep(x, 2, colSums(counts[, colnames(x)]), '/')
rel <- rel[rowMeans(rel >= 0.001) >= 0.1, ]
saveRDS(cor(t(rel), method = 'spearman'), 'output/hmp_subgingival_correlation.rds')

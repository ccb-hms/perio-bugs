# enrichment of the PerioSigDB periodontitis consensus signatures in colorectal cancer (CRC) vs control
# stool metagenomes from curatedMetagenomicData, among all BugSigDB signatures (as in Figure 3 of the
# BugSigDB paper, Geistlinger et al. 2024; vignettes/Figure3.Rmd in waldronlab/BugSigDBPaper)
# used by reports/metadata_options/report.qmd
#
# per dataset: species counts (relative abundance x reads) summed to genus, limma-trend on log-CPM,
# then ORA (hypergeometric on genera with BH-adjusted p < 0.05) and PADOG (1000 permutations) via
# EnrichmentBrowser; signatures at genus level with at least 5 genera in the dataset
# positive controls: the CRC meta-analysis signatures of Wirbel 2019 and Thomas 2019 (Nat Med)
# BugSigDB periodontitis signatures are excluded so PerioSigDB is the only periodontitis source
#
# rank: ties count against the signature (as EnrichmentBrowser's comp.ranks), so a dataset with no
# differentially abundant genera gives every signature the bottom ORA rank
#
# takes ~3 minutes once curatedMetagenomicData downloads are cached (ExperimentHub)

library(dplyr)
library(purrr)
library(tidyr)
library(SummarizedExperiment)
library(EnrichmentBrowser)

set.seed(1)

# PerioSigDB consensus signatures at genus level ----

consensus <- readRDS('output/bugsigdb_semantic_similarity.rds')$consensus
perio <- readRDS('output/perio_bugs_corrected.rds')

# terminal NCBI ID -> genus, from the paired MetaPhlAn lineage strings
id_genus <- tibble(ids = unlist(perio$`NCBI Taxonomy IDs`), names = unlist(perio$`MetaPhlAn taxon names`)) |>
  distinct() |>
  mutate(
    id = paste0('NCBITaxon:', sub('^.*[|]', '', ids)),
    genus = ifelse(grepl('g__', names), sub('^.*g__([^|]+).*$', '\\1', names), NA)
  ) |>
  distinct(id, genus)

perio_sigs <- map(consensus, \(ids) sort(unique(na.omit(id_genus$genus[id_genus$id %in% ids]))))
names(perio_sigs) <- paste0('PerioSigDB_consensus_', names(perio_sigs))

# BugSigDB signatures (all except periodontitis) ----

bsdb <- bugsigdbr::importBugSigDB()
bsdb <- filter(bsdb, !grepl('periodont', Condition, ignore.case = TRUE))

bsdb_sigs <- bugsigdbr::getSignatures(bsdb, tax.id.type = 'taxname', tax.level = 'genus', exact.tax.level = FALSE)
bsdb_sigs <- bsdb_sigs[lengths(bsdb_sigs) >= 5]

controls <- c(Wirbel2019 = 'bsdb:30936547/1/1', Thomas2019 = 'bsdb:30936548/1/1')
control_names <- map_chr(controls, \(id) grep(paste0('^', id, '_'), names(bsdb_sigs), value = TRUE))

sigs <- c(perio_sigs, bsdb_sigs)
focus <- c(names(perio_sigs), control_names)
focus_label <- setNames(c(names(perio_sigs), names(controls)), focus)

# CRC vs control stool datasets ----

# ThomasAM_2019_c is excluded as in the paper (participants overlap with YachidaS_2019)
crc_ids <- curatedMetagenomicData::sampleMetadata |>
  filter(body_site == 'stool', study_condition == 'CRC') |>
  distinct(study_name) |>
  pull() |>
  setdiff('ThomasAM_2019_c') |>
  sort()

load_genus <- function(id) {
  res <- curatedMetagenomicData::curatedMetagenomicData(paste0(id, '.relative_abundance'), dryrun = FALSE, counts = TRUE)
  se <- res[[length(res)]]
  se <- se[, se$study_condition %in% c('CRC', 'control') & se$body_site == 'stool']
  se <- se[, !duplicated(se$subject_id)]

  genus <- rowData(se)$genus
  keep <- !is.na(genus)
  counts <- rowsum(as.matrix(assay(se)[keep, ]), genus[keep])
  counts <- counts[rowSums(counts) > 0, ]

  out <- SummarizedExperiment(list(counts = counts), colData = colData(se))
  out$GROUP <- as.integer(out$study_condition == 'CRC')
  assay(out, 'cpm') <- edgeR::cpm(counts, log = TRUE, prior.count = 3)
  out
}

limma_trend <- function(se) {
  design <- model.matrix(~ factor(se$GROUP))
  fit <- limma::eBayes(limma::lmFit(assay(se, 'cpm'), design), trend = TRUE)
  tt <- limma::topTable(fit, coef = 2, number = Inf, sort.by = 'none', adjust.method = 'BH')
  rowData(se)[[configEBrowser('FC.COL')]] <- tt$logFC
  rowData(se)$limma.STAT <- tt$t
  rowData(se)[[configEBrowser('PVAL.COL')]] <- tt$P.Value
  rowData(se)[[configEBrowser('ADJP.COL')]] <- tt$adj.P.Val
  se
}

dsets <- map(set_names(crc_ids), \(id) limma_trend(load_genus(id)))

datasets <- imap(dsets, \(se, id) tibble(
  dataset = id,
  n_crc = sum(se$GROUP == 1),
  n_control = sum(se$GROUP == 0),
  n_genera = nrow(se),
  n_genera_da = sum(rowData(se)$ADJ.PVAL < 0.05)
)) |>
  list_rbind()

# enrichment ----

run_method <- function(se, method) {
  res <- if (method == 'ora') {
    sbea('ora', se, sigs, perm = 0)
  } else {
    sbea('padog', se, sigs, assay = 'cpm', perm = 1000)
  }
  gsRanking(res, signif.only = FALSE) |>
    as.data.frame() |>
    as_tibble() |>
    transmute(signature = GENE.SET, n_genera = lengths(map(sigs[GENE.SET], intersect, rownames(se))), p = PVAL)
}

results <- expand_grid(dataset = crc_ids, method = c('ora', 'padog')) |>
  mutate(res = map2(dataset, method, \(d, m) {
    message(d, ' ', m)
    run_method(dsets[[d]], m)
  })) |>
  unnest(res) |>
  group_by(dataset, method) |>
  mutate(
    n_signatures = n(),
    rank = rank(p, ties.method = 'max'),
    percentile = 100 * rank / n_signatures,
    fdr = p.adjust(p, method = 'BH')
  ) |>
  ungroup()

ranks <- results |>
  filter(signature %in% focus) |>
  mutate(
    label = unname(focus_label[signature]),
    role = ifelse(grepl('^PerioSigDB', signature), 'periodontitis', 'positive control')
  ) |>
  select(dataset, method, label, role, signature, n_genera, rank, n_signatures, percentile, p, fdr) |>
  arrange(label, method, dataset)

# periodontitis genera in each dataset ----

genera <- imap(dsets, \(se, id) {
  rd <- rowData(se)
  tibble(dataset = id, genus = rownames(se), logFC = rd$FC, t = rd$limma.STAT, p = rd$PVAL, fdr = rd$ADJ.PVAL)
}) |>
  list_rbind() |>
  inner_join(
    tibble(signature = names(perio_sigs), genus = perio_sigs) |> unnest(genus),
    by = 'genus', relationship = 'many-to-many'
  ) |>
  relocate(signature, .after = dataset) |>
  arrange(signature, genus, dataset)

session <- c(
  R = R.version.string,
  map_chr(
    set_names(c('bugsigdbr', 'curatedMetagenomicData', 'EnrichmentBrowser', 'PADOG', 'limma', 'edgeR')),
    \(p) as.character(packageVersion(p))
  ),
  bugsigdb = paste(formals(bugsigdbr::importBugSigDB)$version),
  date = as.character(Sys.Date())
)

saveRDS(
  list(
    ranks = ranks,
    genera = genera,
    datasets = datasets,
    signatures = perio_sigs,
    controls = controls,
    all_results = results,
    session = session
  ),
  'output/crc_bugset_enrichment.rds'
)

# amplicon sequence variants (ASVs) and taxon abundance per sample for the PerioSigDB studies with
# participant annotations (see scripts/participant_data_1-download.R)
#
# DADA2 per study, with settings matched to how each study deposited its reads; taxonomy from the HOMD
# 16S RefSeq (V16.03): genus by the naive Bayesian classifier, species by exact match, recorded as HOMD
# taxon IDs (HMT) and mapped to NCBI taxon IDs with the HOMD taxon table
#
# writes to output/participant_data/:
# - asv_counts.rds: Study, run_accession, asv, count
# - asv_taxonomy.csv: asv, sequence, phylum ... genus, hmt (one or more, '/'-separated), species,
#   ncbi_taxon_id (species, when the exact match is a single HMT)
# - taxon_abundance.csv: counts and relative abundance per sample at genus and species rank

library(dplyr)
library(tidyr)
library(purrr)
library(dada2)

reads_dir <- 'data/participant_reads'
out_dir <- 'output/participant_data'
work_dir <- 'data/participant_reads/filtered'
dir.create(work_dir, showWarnings = FALSE)
threads <- parallel::detectCores() - 1

participants <- read.csv(file.path(out_dir, 'sample_metadata.csv'))

# ---- per-study read layouts ----

# 1141: Illumina 2x151, V3; R1 has 4 random bases before 341F (17), R2 starts with 518R (17)
# 484: Illumina 2x250, V4; 515F (19) / 806R (20)
# 191: one file per run holding all reverse reads then all forward reads (pairing lost on upload);
#      V3-V4, a 14-base linker before 341F (17) and before 806R (20); the forward reads drop below Q20
#      after ~200 bases, too short to overlap the reverse reads across the ~430-base amplicon, so only
#      the reverse reads are used (single-end, from 806R into V4)
# 1411: Roche 454, V1-V3, primers removed, reads run from the reverse primer
# 428: reads already merged, V3-V4, starting with 341F (17) and ending with the reverse complement of
#      805R (21), whose degenerate bases would otherwise split each sequence into several ASVs
studies <- list(
  `Study 1141` = list(layout = 'paired', trim_left = c(21, 17), trunc_len = c(145, 140), max_ee = c(2, 2)),
  `Study 484`  = list(layout = 'paired', trim_left = c(19, 20), trunc_len = c(230, 180), max_ee = c(2, 2)),
  `Study 191`  = list(layout = 'split_reverse', trim_left = 34, trunc_len = 250, max_ee = 2, reverse = TRUE),
  `Study 1411` = list(layout = '454', trim_left = 0, trunc_len = 400, max_ee = 2, reverse = TRUE),
  `Study 428`  = list(layout = 'merged', trim_left = 17, trim_right = 21, trunc_len = 0, max_ee = 3)
)

# Study 191: write the forward (second half) and reverse (first half) reads to separate files
split_halves <- function(file, out_r1, out_r2) {
  fq <- ShortRead::readFastq(file)
  half <- length(fq) / 2
  stopifnot(half == round(half))
  ShortRead::writeFastq(fq[(half + 1):length(fq)], out_r1, compress = TRUE)
  ShortRead::writeFastq(fq[seq_len(half)], out_r2, compress = TRUE)
}

run_study <- function(study, cfg) {
  runs <- participants$run_accession[participants$Study == study]
  dir <- file.path(reads_dir, study)
  filt <- file.path(work_dir, study)
  dir.create(filt, showWarnings = FALSE)

  if (cfg$layout == 'paired') {
    r1 <- file.path(dir, paste0(runs, '_1.fastq.gz'))
    r2 <- file.path(dir, paste0(runs, '_2.fastq.gz'))
    # runs whose files could not be downloaded (see participant_data_1-download.R) are dropped
    keep <- file.exists(r1) & file.exists(r2)
    runs <- runs[keep]
    f1 <- file.path(filt, paste0(runs, '_F.fastq.gz'))
    f2 <- file.path(filt, paste0(runs, '_R.fastq.gz'))
    track <- filterAndTrim(r1[keep], f1, r2[keep], f2, trimLeft = cfg$trim_left, truncLen = cfg$trunc_len,
                           maxEE = cfg$max_ee, maxN = 0, truncQ = 2, rm.phix = TRUE, compress = TRUE,
                           multithread = threads)
    ok <- file.exists(f1)
    runs <- runs[ok]
    err1 <- learnErrors(f1[ok], multithread = threads)
    err2 <- learnErrors(f2[ok], multithread = threads)
    dd1 <- dada(f1[ok], err = err1, multithread = threads)
    dd2 <- dada(f2[ok], err = err2, multithread = threads)
    merged <- mergePairs(dd1, f1[ok], dd2, f2[ok])
    seqtab <- makeSequenceTable(merged)
  } else {
    raw <- file.path(dir, paste0(runs, '.fastq.gz'))
    if (cfg$layout == 'split_reverse') {
      r1 <- file.path(filt, paste0(runs, '_raw_1.fastq.gz'))
      r2 <- file.path(filt, paste0(runs, '_raw_2.fastq.gz'))
      todo <- !file.exists(r2) & file.exists(raw)
      pwalk(list(raw[todo], r1[todo], r2[todo]), split_halves)
      raw <- r2
    }
    keep <- file.exists(raw)
    runs <- runs[keep]
    f1 <- file.path(filt, paste0(runs, '_F.fastq.gz'))
    track <- filterAndTrim(raw[keep], f1, trimLeft = cfg$trim_left, trimRight = cfg$trim_right %||% 0,
                           truncLen = cfg$trunc_len, maxEE = cfg$max_ee, maxN = 0, truncQ = 2, compress = TRUE,
                           multithread = threads)
    ok <- file.exists(f1)
    runs <- runs[ok]
    err <- learnErrors(f1[ok], multithread = threads)
    dd <- if (cfg$layout == '454') {
      dada(f1[ok], err = err, HOMOPOLYMER_GAP_PENALTY = -1, BAND_SIZE = 32, multithread = threads)
    } else {
      dada(f1[ok], err = err, multithread = threads)
    }
    seqtab <- makeSequenceTable(dd)
  }
  rownames(seqtab) <- runs
  seqtab <- removeBimeraDenovo(seqtab, method = 'consensus', multithread = threads)
  if (isTRUE(cfg$reverse)) colnames(seqtab) <- as.character(Biostrings::reverseComplement(Biostrings::DNAStringSet(colnames(seqtab))))

  message(study, ': ', nrow(seqtab), ' samples, ', ncol(seqtab), ' ASVs, ',
          round(100 * sum(seqtab) / sum(track[, 'reads.in'])), '% of input reads kept')
  list(seqtab = seqtab, track = track)
}

# cached per study; delete data/participant_reads/filtered/<Study>.rds to rerun one
# optional arguments: the studies to process (default: all)
selected <- commandArgs(trailingOnly = TRUE)
if (length(selected)) studies <- studies[selected]
results <- imap(studies, \(cfg, study) {
  cache <- file.path(work_dir, paste0(study, '.rds'))
  if (!file.exists(cache)) saveRDS(run_study(study, cfg), cache)
  readRDS(cache)
})

counts <- imap_dfr(results, \(r, study) {
  as_tibble(r$seqtab, rownames = 'run_accession') |>
    pivot_longer(-run_accession, names_to = 'sequence', values_to = 'count') |>
    filter(count > 0) |>
    mutate(Study = study, .before = 1)
})

# ---- taxonomy ----

homd_dir <- 'data/homd_refseq'
ref_fasta <- Biostrings::readDNAStringSet(file.path(homd_dir, 'HOMD_16S_rRNA_RefSeq_V16.03.fasta'))
ref_tax <- read.delim(file.path(homd_dir, 'HOMD_16S_rRNA_RefSeq_V16.03.qiime.taxonomy'), header = FALSE,
                      col.names = c('id', 'taxonomy'))
ref_tax <- ref_tax[match(names(ref_fasta), ref_tax$id), ]
ranks <- strsplit(sub(';s__.*', '', gsub('[kpcofg]__', '', ref_tax$taxonomy)), ';')
hmt <- sub('_.*', '', ref_tax$id)

# training set for the genus classifier: 'Kingdom;Phylum;...;Genus;'
train_fasta <- file.path(work_dir, 'homd_train.fasta')
genus_ref <- ref_fasta
names(genus_ref) <- map_chr(ranks, \(x) paste0(paste(x, collapse = ';'), ';'))
Biostrings::writeXStringSet(genus_ref, train_fasta)

# exact-match species reference: 'id Genus HMT' so that matches come back as HOMD taxon IDs
species_fasta <- file.path(work_dir, 'homd_species.fasta')
species_ref <- ref_fasta
names(species_ref) <- paste(ref_tax$id, map_chr(ranks, \(x) x[6]), hmt)
Biostrings::writeXStringSet(species_ref, species_fasta)

seqs <- unique(counts$sequence)
tax <- assignTaxonomy(seqs, train_fasta, minBoot = 80, multithread = threads)
colnames(tax) <- c('kingdom', 'phylum', 'class', 'order', 'family', 'genus')
sp <- assignSpecies(seqs, species_fasta, allowMultiple = TRUE)

# species names as in NCBI: unnamed HOMD genera ('Peptostreptococcaceae_[G-5]') are dropped from
# bracketed species ('[Eubacterium]_saphenum' -> '[Eubacterium] saphenum')
homd_taxa <- read.delim('data/HOMD_taxon_table2025-08-26_1756207550.txt', skip = 1, check.names = FALSE) |>
  transmute(hmt = sprintf('HMT-%03d', HMT_ID),
            species_name = ifelse(grepl('_\\[G-', Genus), sub('_', ' ', Species), paste(Genus, Species)),
            ncbi_taxon_id = NCBI_taxon_id)

# HOMD gives subspecies or family IDs for some species; NCBI species IDs instead
species_ids <- c('Fusobacterium nucleatum' = 851, 'Streptococcus oralis' = 1303, '[Eubacterium] saphenum' = 51123)
homd_taxa <- mutate(homd_taxa, ncbi_taxon_id = coalesce(unname(species_ids[species_name]), ncbi_taxon_id))

# an exact match to several HMTs is a species when they are all the same species (e.g. subspecies of
# F. nucleatum); genus from the classifier must agree with the match, which assignSpecies already requires
match_species <- function(hmt) {
  map_chr(strsplit(hmt, '/'), \(h) {
    s <- unique(homd_taxa$species_name[match(h, homd_taxa$hmt)])
    if (length(s) == 1) s else NA_character_
  })
}

taxonomy <- tibble(sequence = seqs, asv = paste0('ASV', seq_along(seqs))) |>
  bind_cols(as_tibble(tax)) |>
  mutate(hmt = sp[, 'Species'], species = match_species(hmt)) |>
  left_join(distinct(homd_taxa, species = species_name, ncbi_taxon_id), by = 'species')

counts <- counts |>
  left_join(select(taxonomy, sequence, asv), by = 'sequence') |>
  select(Study, run_accession, asv, count)

saveRDS(counts, file.path(out_dir, 'asv_counts.rds'))
write.csv(taxonomy, file.path(out_dir, 'asv_taxonomy.csv'), row.names = FALSE)

# ---- abundance per sample at genus and species rank ----

totals <- summarise(counts, total = sum(count), .by = c(Study, run_accession))
with_tax <- left_join(counts, taxonomy, by = 'asv')

abundance <- bind_rows(
  with_tax |>
    filter(!is.na(genus)) |>
    summarise(count = sum(count), .by = c(Study, run_accession, genus)) |>
    transmute(Study, run_accession, rank = 'genus', taxon = genus, ncbi_taxon_id = NA, count),
  with_tax |>
    filter(!is.na(species)) |>
    summarise(count = sum(count), .by = c(Study, run_accession, species, ncbi_taxon_id)) |>
    transmute(Study, run_accession, rank = 'species', taxon = species, ncbi_taxon_id, count)
) |>
  left_join(totals, by = c('Study', 'run_accession')) |>
  mutate(relative_abundance = count / total) |>
  # samples without the taxon count as zero
  complete(nesting(Study, run_accession, total), nesting(rank, taxon, ncbi_taxon_id),
           fill = list(count = 0, relative_abundance = 0)) |>
  semi_join(distinct(counts, Study, run_accession), by = c('Study', 'run_accession')) |>
  select(Study, run_accession, rank, taxon, ncbi_taxon_id, count, total, relative_abundance)

write.csv(abundance, file.path(out_dir, 'taxon_abundance.csv'), row.names = FALSE)

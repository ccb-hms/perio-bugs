library(tidyverse)
library(bugSigSimple)
library(ggplot2)
library(ComplexHeatmap)
library(circlize)
library(dendextend)
library(plotly)

# helper functions ----

get_most_specific <- function(taxpart) {
  taxpart_split <- strsplit(taxpart, split='|', fixed=TRUE)
  most_specific <- sapply(taxpart_split, tail, 1)
  sapply(strsplit(most_specific, '__'), tail, 1)
}

make_freq_dat <- function(dat_exp, direction, min_freq = 3) {
  dat_exp |>
    filter(`Abundance in Group 1` == direction) |> 
    count(last_taxname, last_taxid, name = "freq") |>
    filter(freq >= min_freq) |> 
    arrange(desc(freq)) |> 
    mutate(across(c(last_taxid, last_taxname), ~ factor(., levels = .)))
}

make_freq_dat_combined <- function(dat_exp, pval_dat, min_freq = 3) {
  dat_up_unfiltered <- dat_exp |>
    filter(`Abundance in Group 1` == "increased") |> 
    count(last_taxname, last_taxid, name = "freq") |> 
    arrange(desc(freq))

  dat_dn_unfiltered <- dat_exp |>
    filter(`Abundance in Group 1` == "decreased") |> 
    count(last_taxname, last_taxid, name = "freq") |> 
    arrange(desc(freq))

  # Get taxa with freq >= min_freq in EITHER direction
  taxa_to_show <- unique(c(
    (dat_up_unfiltered |> filter(freq >= min_freq))$last_taxname,
    (dat_dn_unfiltered |> filter(freq >= min_freq))$last_taxname
  ))

  # Use provided p-values
  taxon_table_for_pval <- pval_dat

  # Get dominance and difference info
  taxa_stats <- bind_rows(
    dat_up_unfiltered |> mutate(direction = "increased"),
    dat_dn_unfiltered |> mutate(direction = "decreased")
  ) |>
    pivot_wider(
      id_cols = last_taxname,
      names_from = direction,
      values_from = freq,
      values_fill = 0
    ) |>
    mutate(
      difference = increased - decreased
    ) |>
    select(last_taxname, difference) |>
    left_join(taxon_table_for_pval, by = "last_taxname")

  # Combine all data
  result <- bind_rows(
    dat_up_unfiltered |> mutate(direction = "increased"),
    dat_dn_unfiltered |> mutate(direction = "decreased")
  ) |>
    filter(last_taxname %in% taxa_to_show) |>
    left_join(taxa_stats, by = "last_taxname") |>
    mutate(
      freq_display = freq,
      alpha_val = ifelse(pval > 0.05, 0.3, 1)
    ) |>
    # Create taxon factor ordered by number of signatures (descending)
    mutate(
      last_taxname = factor(last_taxname, 
        levels = names(sort(sapply(split(freq, last_taxname), sum), decreasing = TRUE)))
    )
  
  result
}

make_freq_plot <- function(dat_combined, title) {
  ggplot(dat_combined, aes(
    y = forcats::fct_rev(last_taxname), 
    x = freq_display,
    fill = direction,
    alpha = alpha_val
  )) +
    geom_col(position = "stack", color = "#333") +
    scale_x_continuous(labels = abs, position = "top") +
    scale_fill_manual(
      values = c("increased" = "#ffcccc", "decreased" = "#b3d9ff"),
      labels = c("increased" = "Increased", "decreased" = "Decreased")
    ) +
    scale_alpha_identity() +
    labs(
      y = NULL, 
      x = NULL,
      title = title,
      subtitle = "Excludes taxa in fewer than 3 signatures\nTransparent: p-value > 0.05",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "top",
      plot.title = element_text(hjust = 0)
    )
}

make_taxon_table <- function(dat) {
  bugSigSimple::createTaxonTable(
    mutate(dat, comparison1 = paste(`Group 0 name`, `Group 1 name`, sep = " vs ")),
    n = Inf
  ) |> 
    select(
      `Taxon Name`,
      `Signatures` = total_signatures,
      `Increased` = increased_signatures,
      `Decreased` = decreased_signatures,
      `P-value` = `Binomial Test pval`
    ) |> 
    mutate(`Δ` = Increased - Decreased, .after = Decreased)
}

make_dt <- function(dat) {
  DT::datatable(
    dat,
    filter = 'top',
    class = 'row-border hover order-column',
    options = list(
      dom = 'lrtip',
      columnDefs = list(list(targets = c(6), searchable = FALSE))
    )
  )
}

# load data ----

# replace by bigsigdbr::importBugSigDB()
dat <- readRDS('output/perio_bugs.rds')
dim(dat)

# expand one row per bug and extract terminal taxon ----
dat_exp <- dat |> 
  unnest(c(`NCBI Taxonomy IDs`, `MetaPhlAn taxon names`)) |>
  mutate(
    last_taxname = get_most_specific(`MetaPhlAn taxon names`),
    last_taxid = get_most_specific(`NCBI Taxonomy IDs`)
  )

# number of unique bugs per direction
dat_exp |>
  summarise(
    n_unique = n_distinct(last_taxname),
    .by = `Abundance in Group 1`)

# number of unique bugs total
n_distinct(dat_exp$last_taxname)

# frequency plots ----
taxon_table <- make_taxon_table(dat)
pval_dat_all <- taxon_table |>
  select(last_taxname = `Taxon Name`, pval = `P-value`)

dat_freq_combined <- make_freq_dat_combined(dat_exp, pval_dat_all)
dat_freq_combined |> 
  filter(difference > 0) |>
  mutate(direction = factor(direction, levels = c("decreased", "increased"))) |>
  make_freq_plot("Taxon Frequency: Up-Regulated")

dat_freq_combined |> 
  filter(difference < 0) |>
  mutate(freq_display = abs(freq_display)) |>
  mutate(direction = factor(direction, levels = c("increased", "decreased"))) |>
  make_freq_plot("Taxon Frequency: Down-Regulated")


# identify broad coverage signatures ----
broad_platforms <- c("DNA-DNA Hybridization", "Human Intestinal Tract Chip", 
                     "Illumina", "Roche454")

targeted_platforms <- c("RT-qPCR", "Non-quantitative PCR", "Sanger", "Mass spectrometry")

dat_broad <- dat |> 
  mutate(seq_keep = case_when(
    # use seq_type first if available
    `Sequencing type` %in% c("16S", "WMS") ~ TRUE,
    `Sequencing type` %in% c("PCR", "16S,PCR") ~ FALSE,
    # fall back to seq_plat if seq_type is NA
    is.na(`Sequencing type`) & `Sequencing platform` %in% broad_platforms ~ TRUE,
    is.na(`Sequencing type`) & `Sequencing platform` %in% targeted_platforms ~ FALSE,
    .default = NA
  )) |>
  filter(seq_keep)

# checks
count(dat_broad, seq_keep)
table(dat_broad$`Sequencing platform`, dat_broad$seq_keep)
table(dat_broad$`Sequencing type`, dat_broad$seq_keep)

# sig length histogram ----
# Create dat_condition for signature length extraction
dat_condition <- dat |> 
  mutate(comparison1 = paste(`Group 0 name`, `Group 1 name`, sep = " vs "))

dat_condition_broad <- dat_broad |> 
  mutate(comparison1 = paste(`Group 0 name`, `Group 1 name`, sep = " vs "))

# Get signatures and extract lengths
allsigs <- bugsigdbr::getSignatures(dat_condition, tax.id.type = "taxname")
siglengths <- sapply(allsigs, length)

allsigs_broad <- bugsigdbr::getSignatures(dat_condition_broad, tax.id.type = "taxname")
siglengths_broad <- sapply(allsigs_broad, length)

# Create bar chart with overlaid bars
sig_counts_all <- data.frame(siglength = siglengths) |>
  count(siglength, name = "count") |>
  mutate(data_type = "All Data")

sig_counts_broad <- data.frame(siglength = siglengths_broad) |>
  count(siglength, name = "count") |>
  mutate(data_type = "Broad Data Only")

sig_data <- bind_rows(sig_counts_all, sig_counts_broad)

ggplot(sig_data, aes(x = siglength, y = count, fill = data_type)) +
  geom_col(position = "identity", alpha = c(0.5, 0.8)[match(sig_data$data_type, c("All Data", "Broad Data Only"))]) +
  scale_fill_manual(values = c("All Data" = "#999999", "Broad Data Only" = "#2ecc71")) +
  scale_x_continuous(limits = c(0.5, 50.5), breaks = c(1, seq(5, 50, by = 5))) +
  labs(
    x = "Signature Length (number of taxa)",
    y = "Number of Signatures",
    title = "Distribution of Signature Lengths: All Data vs Broad Data Only",
    fill = "Data Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


# taxon tables - all signatures ----
taxon_table |> 
  filter(Increased > Decreased)  |>
  arrange(`P-value`) |>
  make_dt()

taxon_table |> 
  filter(Decreased >= Increased) |>
  arrange(`P-value`) |>
  make_dt()

# taxon tables - broad coverage signatures only ----
taxon_table_broad <- make_taxon_table(dat_broad)
pval_dat_broad <- taxon_table_broad |>
  select(last_taxname = `Taxon Name`, pval = `P-value`)

taxon_table_broad |> 
  filter(Increased > Decreased) |>
  arrange(`P-value`) |>
  make_dt()

taxon_table_broad |>
  filter(Decreased >= Increased) |>
  arrange(`P-value`) |>
  make_dt()
# ============================================================================
# PART 1: Basic Jaccard Similarity Analysis
# ============================================================================

# cluster analysis
allsigs <- bugsigdbr::getSignatures(dat_condition, tax.id.type = "taxname")
allsigs <- allsigs[sapply(allsigs, length) > 1] #require length > 1

mydists <- BugSigDBStats::calcPairwiseOverlaps(allsigs)

# Create a matrix of Jaccard similarities (0 for no overlap, 1 for 100% overlap)
jmat <- BugSigDBStats::calcJaccardSimilarity(allsigs)

# Remove annotation name, use column title on heatmap
ha <- HeatmapAnnotation(
  `Signature Length` = anno_barplot(siglengths),
  show_annotation_name = FALSE  # Hide the annotation label
)
hm <- Heatmap(
  jmat,
  top_annotation = ha, 
  column_title = "Signature Length",  # Add as column title
  column_title_side = "top",
  left_annotation = NULL,
  row_names_max_width = unit(20, "cm"),
  column_names_max_height = unit(20, "cm"),
  row_labels = gsub('^.+?_(UP|DOWN)$', "\\1", rownames(jmat)),
  column_labels = gsub('^.+?_(UP|DOWN)$', "\\1", colnames(jmat)),
  row_names_gp = gpar(fontsize = 6),  
  column_names_gp = gpar(fontsize = 6),
  show_column_dend = FALSE,
  heatmap_legend_param = list(
    title = 'Jaccard Similarity',
    direction = "horizontal"  # Make legend horizontal
  )
)

# Draw with legend at bottom
draw(hm, heatmap_legend_side = "bottom")

# ============================================================================
# PART 2: Create Taxa Heatmap with Original Clustering
# ============================================================================

# Helper function to get author labels (without year)
get_author_label <- function(dat, sname) {
  id <- unlist(strsplit(sname, "_"))[1]
  id <- gsub("^bsdb:", "", id)
  id <- unlist(strsplit(id, "/"))
  
  sdat <- dat[dat$Study == paste0("Study ", id[1]) & 
                dat$Experiment == paste0("Experiment ", id[2]), ]
  
  if(nrow(sdat) == 0) return("Unknown")
  
  author <- sdat$`Authors list`[1]
  aspl <- unlist(strsplit(author, " "))[1]
  
  # Add "et al." if multiple authors
  if(grepl(",", author)) {
    return(paste0(aspl, " et al."))
  } else {
    return(aspl)
  }
}


create_taxa_heatmap <- function(dat_condition, 
                                clust_up, 
                                jmat, 
                                hc, 
                                tax_level = "species",
                                width = 12,
                                height = 10) {
  
  # Get signatures
  sigs_taxa <- bugsigdbr::getSignatures(
    dat_condition,
    tax.id.type = "taxname",
    tax.level = tax_level,
    exact.tax.level = FALSE
  )
  
  # Filter to only signatures in clust_up
  sigs_taxa <- sigs_taxa[names(sigs_taxa) %in% clust_up]
  
  # Remove empty signatures
  sigs_taxa <- sigs_taxa[sapply(sigs_taxa, length) > 0]
  
  # Create binary presence/absence matrix
  all_taxa <- unique(unlist(sigs_taxa))
  
  taxa_matrix <- matrix(0, 
                        nrow = length(sigs_taxa),
                        ncol = length(all_taxa),
                        dimnames = list(names(sigs_taxa), all_taxa))
  
  for(i in seq_along(sigs_taxa)) {
    sig_name <- names(sigs_taxa)[i]
    taxa_in_sig <- sigs_taxa[[i]]
    taxa_matrix[sig_name, taxa_in_sig] <- 1
  }
  
  # Calculate taxa frequency
  taxa_freq <- colSums(taxa_matrix) / nrow(taxa_matrix)
  taxa_order <- order(taxa_freq, decreasing = TRUE)
  taxa_freq_ordered <- taxa_freq[taxa_order]
  
  # Re-cluster using original distance matrix
  sigs_to_use <- rownames(taxa_matrix)
  jmat_subset <- jmat[sigs_to_use, sigs_to_use]
  hc_subset <- hclust(dist(jmat_subset), method = hc$method)
  dend_subset <- as.dendrogram(hc_subset)
  
  # Reorder taxa_matrix to match clustering
  taxa_matrix_ordered <- taxa_matrix[hc_subset$labels, taxa_order]
  
  # Apply author labels
  sig_labels_ordered <- vapply(rownames(taxa_matrix_ordered), 
                               get_author_label, 
                               character(1), 
                               dat = dat_condition)
  
  labels(dend_subset) <- sig_labels_ordered
  rownames(taxa_matrix_ordered) <- sig_labels_ordered
  
  # Create color matrix
  color_matrix <- taxa_matrix_ordered
  for(j in 1:ncol(color_matrix)) {
    for(i in 1:nrow(color_matrix)) {
      if(color_matrix[i, j] == 0) {
        color_matrix[i, j] <- NA
      } else {
        color_matrix[i, j] <- taxa_freq_ordered[j]
      }
    }
  }
  
  # Color function
  col_fun <- colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
  
  # Create heatmap
  hm_taxa <- Heatmap(
    color_matrix,
    name = "Frequency",
    col = col_fun,
    na_col = "white",
    cluster_rows = dend_subset,
    cluster_columns = FALSE,
    show_row_dend = TRUE,
    row_dend_width = unit(1, "cm"),
    row_names_side = "left",
    column_names_side = "bottom",
    row_names_gp = gpar(fontsize = 8),
    column_names_gp = gpar(fontsize = 8),
    column_names_rot = 90,
    column_title = NULL,
    row_title = NULL,
    top_annotation = NULL,
    heatmap_legend_param = list(
      title = "Frequency",
      direction = "vertical",
      title_gp = gpar(fontsize = 8),
      labels_gp = gpar(fontsize = 7),
      at = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", "0.25", "0.5", "0.75", "1"),
      legend_height = unit(2, "cm"),
      legend_width = unit(0.3, "cm")
    ),
    width = unit(width, "cm"),
    height = unit(height, "cm")
  )
  
  return(hm_taxa)
}

# identify the clusters
hc <- hclust(dist(jmat))
orig_labs <- hc$labels

hc$labels <- gsub('^.+?_(UP|DOWN)$', "\\1", hc$labels)
plot(hc)

# restore labels and get clusters
hc$labels <- orig_labs

#set the number of clusters here with k
clusts <- sort(cutree(hc, k = 2)) 
clusts <- lapply(unique(clusts), function(i) names(clusts)[clusts == i])

# proceed with cluster of UP signatures with high similarity
clust_up <- clusts[[2]]

# Usage examples:
# For species
hm_species <- create_taxa_heatmap(dat_condition, clust_up, jmat, hc, 
                                  tax_level = "species")
draw(hm_species, heatmap_legend_side = "right")

# For genus
hm_genus <- create_taxa_heatmap(dat_condition, clust_up, jmat, hc, 
                                tax_level = "genus")
draw(hm_genus, heatmap_legend_side = "right")

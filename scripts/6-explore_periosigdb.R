library(tidyverse)
library(bugSigSimple)
library(ggplot2)
library(ComplexHeatmap)
library(circlize)
library(dendextend)

# replace by bigsigdbr::importBugSigDB()
dat <- readRDS('output/perio_bugs.rds')
dim(dat)

dat_condition <- dat |> 
  mutate(comparison1 = paste(`Group 0 name`, `Group 1 name`, sep = " vs "))

# table of studies
taxon_table <- bugSigSimple::createTaxonTable(dat_condition, n=20) |> 
  select(
    `Taxon Name`,
    `Signatures` = total_signatures,
    `Increased` = increased_signatures,
    `Decreased` = decreased_signatures,
    `P-value` = `Binomial Test pval`
  )

taxon_table |> 
  dplyr::filter(Increased > Decreased) |>
  kableExtra::kbl()

# ============================================================================
# PART 1: Basic Jaccard Similarity Analysis
# ============================================================================

# cluster analysis
allsigs <- bugsigdbr::getSignatures(dat_condition, tax.id.type = "taxname")
allsigs <- allsigs[sapply(allsigs, length) > 1] #require length > 1

# what is the distribution of signature lengths?
siglengths <- sapply(allsigs, length)
siglengths.df <- data.frame(siglengths = siglengths)
ggplot(siglengths.df, aes(x=siglengths)) +
  geom_bar(width=.9, colour='black', fill= '#ddd', linewidth=0.5) +
  theme_bw() +
  xlab('Number of Microbes in Signature') +
  ylab('Number of Signatures')

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

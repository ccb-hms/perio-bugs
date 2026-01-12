library(tidyverse)
library(bugSigSimple)
library(ggplot2)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)



# replace by bigsigdbr::importBugSigDB()
dat <- readRDS('output/perio_bugs.rds')
dim(dat)


dat_condition <- dat |> 
  mutate(comparison1 = paste(`Group 0 name`, `Group 1 name`, sep = " vs "))

# table of studies
kableExtra::kbl(bugSigSimple::createTaxonTable(dat_condition))


# cluster analysis
allsigs <- bugsigdbr::getSignatures(dat_condition, tax.id.type = "taxname")
allsigs <- allsigs[sapply(allsigs, length) > 1] #require length > 1
length(allsigs)

mydists <- BugSigDBStats::calcPairwiseOverlaps(allsigs)
dim(mydists)


# what is the distribution of signature lengths?
siglengths <- sapply(allsigs, length)
siglengths.df <- data.frame(siglengths = siglengths)
ggplot(siglengths.df, aes(x=siglengths)) +
  geom_bar()

table(siglengths)

# Create a matrix of Jaccard similarities (0 for no overlap, 1 for 100% overlap)
jmat <- BugSigDBStats::calcJaccardSimilarity(allsigs)

ha <- HeatmapAnnotation(`Signature Length` = anno_barplot(siglengths))
hr <- rowAnnotation(`Signature Length` = anno_barplot(siglengths))
hm <- Heatmap(
  jmat,
  top_annotation = ha, 
  left_annotation = NULL,
  row_names_max_width = unit(20, "cm"),
  column_names_max_height = unit(20, "cm"),
  row_labels = gsub('^.+?_(UP|DOWN)$', "\\1", rownames(jmat)),  #get rid of study labels
  column_labels = gsub('^.+?_(UP|DOWN)$', "\\1", colnames(jmat)),
  row_names_gp = gpar(fontsize = 4),  
  column_names_gp = gpar(fontsize = 4),
  show_column_dend = FALSE,
  heatmap_legend_param = list(title = 'Jaccard Similarity')
)
hm

# interactive version
hm <- draw(hm)
htShiny(hm)

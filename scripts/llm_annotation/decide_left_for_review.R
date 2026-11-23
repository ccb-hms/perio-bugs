# Decisions by rule on taxa-review items left for review, following the curators' practice
#
# usage: Rscript scripts/llm_annotation/decide_left_for_review.R (after resolve_taxa_review.R; then rerun it)
# writes output/llm_annotation/taxa_review/decisions/rules.csv (see resolve_taxa_review.R for the format)
#
# The curators' extraction template (data/OVERVIEW_SHEET_*.xlsx) collects phyla, genera and species. They
# recorded a family, order or class only for an OTU that could not be named below that rank (e.g. study 1141
# "Comamonadaceae" twice, for two OTUs). So:
# - a family, order or class result reported for an OTU named only to that rank ("OTU0016 family of
#   Propionibacteriaceae") is added, as the curators did elsewhere
# - a result from a test at the family, order or class level (LEfSe cladogram node, class-level bar chart) is
#   not added
# - a prevalence result is not added where the curators recorded the same paper's abundance analysis

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

review_dir <- 'output/llm_annotation/taxa_review'
items <- read_csv(file.path(review_dir, 'corrections.csv'), col_types = cols(.default = 'c')) |>
  filter(status == 'left for review' | decided_by %in% 'rule')

rank_rule <- items |>
  filter(str_detect(reason, '^a family, order or class result')) |>
  mutate(otu = str_detect(evidence, regex('OTU ?[0-9]+ (family|order|class) of', ignore_case = TRUE)),
         decision = ifelse(otu, 'apply', 'decline'),
         rationale = ifelse(otu,
                            'an OTU named only to this rank; the curators recorded such OTUs at family level',
                            'a result of a test at the family, order or class level; the curators recorded phyla, genera and species only'))

prevalence_rule <- items |>
  filter(str_detect(reason, '^a prevalence result')) |>
  mutate(decision = 'decline',
         rationale = 'a prevalence result; the curators recorded this paper\'s abundance analysis, one analysis per study')

decisions <- bind_rows(rank_rule, prevalence_rule) |>
  transmute(item_id, decision, to_taxid = NA, to_direction = NA, rationale, decided_by = 'rule')

dir.create(file.path(review_dir, 'decisions'), showWarnings = FALSE)
write_csv(decisions, file.path(review_dir, 'decisions', 'rules.csv'), na = '')
print(count(decisions, decision, rationale))

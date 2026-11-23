# Combine the taxa review (scripts/llm_annotation/taxa_review_instructions.md) into tables for curators
#
# usage: Rscript scripts/llm_annotation/summarise_taxa_review.R
# reads output/llm_annotation/taxa_review/Study <ID>_review.csv (curated studies) and
# output/llm_annotation/added/taxa_review/Study <ID>_review.csv (studies added from the full-text review)
# writes, in output/llm_annotation/taxa_review/:
# - taxa_review_all.csv: every reviewed row, with the study and which review it came from
# - taxa_review_by_study.csv: one row per study, counting each action, with the review's notes on rows that
#   need a decision

library(dplyr)
library(purrr)
library(readr)
library(tidyr)

files <- c(list.files('output/llm_annotation/taxa_review', pattern = '_review[.]csv$', full.names = TRUE),
           list.files('output/llm_annotation/added/taxa_review', pattern = '_review[.]csv$', full.names = TRUE))

rows <- map_dfr(files, \(f) {
  read_csv(f, col_types = cols(.default = 'c'), show_col_types = FALSE) |>
    mutate(Study = sub('_review[.]csv$', '', basename(f)),
           review = ifelse(grepl('/added/', f), 'added study', 'curated study'), .before = 1)
}) |>
  mutate(action = tolower(trimws(action)),
         recorded_taxon = coalesce(recorded_taxon, taxon_reported))

write_csv(rows, 'output/llm_annotation/taxa_review/taxa_review_all.csv', na = '')

by_study <- rows |>
  count(Study, review, action) |>
  pivot_wider(names_from = action, values_from = n, values_fill = 0) |>
  mutate(rows_recorded = rowSums(across(-c(Study, review, any_of('add')))),
         needs_decision = rowSums(across(-c(Study, review, rows_recorded, any_of('keep')))) > 0) |>
  relocate(rows_recorded, .after = review) |>
  arrange(desc(needs_decision), Study)

write_csv(by_study, 'output/llm_annotation/taxa_review/taxa_review_by_study.csv', na = '')
print(count(rows, review, action))
print(summarise(by_study, studies = n(), with_changes = sum(needs_decision), .by = review))

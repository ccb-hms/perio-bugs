# Spreadsheet of the taxa-review changes for the curators to approve
#
# usage: Rscript scripts/llm_annotation/export_for_curators.R (after resolve_taxa_review.R)
# writes output/llm_annotation/taxa_review/corrections_for_curators.xlsx:
# - "Awaiting approval": open items decided by rule or by a second pass, and changes found later; the
#   corrected build already uses them. curator_decision: approve / reject (with curator_note)
# - "Reviewer changes": changes the first reviewer settled from the paper (also applied)
# - "ID mapping": catch-all and merged NCBI IDs replaced in every signature
# Curators' verdicts go back into corrections.csv (curator_decision, curator_note) by item_id.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(openxlsx)
})

review_dir <- 'output/llm_annotation/taxa_review'
x <- read_csv(file.path(review_dir, 'corrections.csv'), col_types = cols(.default = 'c'))

what <- function(action, recorded_taxon, to_name, direction, to_direction) {
  case_when(
    action == 'remove' ~ paste('remove', recorded_taxon),
    action %in% c('add', 'include') ~ paste('add', coalesce(to_name, recorded_taxon)),
    action == 'fix_direction' ~ paste('move', recorded_taxon, 'to', to_direction),
    action %in% c('fix_ncbi', 'placeholder', 'merged ID') ~ paste('map', recorded_taxon, 'to', to_name),
    action == 'check' ~ paste('keep', recorded_taxon, 'as recorded'),
    .default = paste(action, recorded_taxon))
}

sheet <- function(d) {
  d |>
    mutate(change = what(action, recorded_taxon, to_name, direction, to_direction),
           applied = grepl('^applied', status)) |>
    transmute(item_id, Study, direction, change, applied, decided_by,
              why = coalesce(rationale, reason), paper_evidence = evidence, first_review_note = note,
              from_taxid, to_taxid, curator_decision, curator_note) |>
    arrange(Study, direction, change)
}

awaiting <- x |> filter(grepl('awaiting curator approval', status)) |> sheet()
reviewer <- x |> filter(status %in% c('applied', 'not applied'), !action %in% c('placeholder', 'merged ID')) |>
  mutate(decided_by = 'first review') |> sheet()
mapping <- x |> filter(action %in% c('placeholder', 'merged ID')) |> mutate(decided_by = 'ID audit') |> sheet()

wb <- createWorkbook()
for (nm in c('Awaiting approval', 'Reviewer changes', 'ID mapping')) {
  d <- list(`Awaiting approval` = awaiting, `Reviewer changes` = reviewer, `ID mapping` = mapping)[[nm]]
  addWorksheet(wb, nm)
  writeDataTable(wb, nm, d, withFilter = TRUE)
  freezePane(wb, nm, firstRow = TRUE)
  setColWidths(wb, nm, cols = seq_along(d), widths = c(12, 14, 10, 45, 8, 14, 60, 60, 40, 10, 10, 16, 30)[seq_along(d)])
  dataValidation(wb, nm, col = which(names(d) == 'curator_decision'), rows = 2:(nrow(d) + 1),
                 type = 'list', value = '"approve,reject"')
}
saveWorkbook(wb, file.path(review_dir, 'corrections_for_curators.xlsx'), overwrite = TRUE)
cat(sprintf('awaiting approval: %d (%d applied) | reviewer changes: %d | ID mapping: %d\n',
            nrow(awaiting), sum(awaiting$applied), nrow(reviewer), nrow(mapping)))

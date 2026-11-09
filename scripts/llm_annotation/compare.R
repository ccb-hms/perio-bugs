# compare LLM-extracted annotation (output/llm_annotation/<run>/*.json, see extraction_instructions.md)
# with the manual curation (output/perio_bugs_corrected.rds), without changing the manual curation
#
# usage: Rscript scripts/llm_annotation/compare.R <run> [<source run> ...]
# with source runs, the extractions are taken from them (a study in a later run replaces the same study
# in an earlier one) and the results are written to <run>; e.g. final full curated_comparison uses the
# re-extraction for the curators' comparison wherever the blind extraction chose a different one
# writes output/llm_annotation/<run>/comparison.csv, one row per study x field:
# - agree: both have a value and they match (numbers within 2% or 0.051 absolute; text after normalising)
# - differs: both have a value and they don't match (for curator review)
# - fills gap: the manual curation has no value, the paper reports one
# - manual only: the manual curation has a value, the extractor found none
# - new field: field not in the manual curation (statistical analysis, diversity, signature source)
# - both missing

library(dplyr)
library(purrr)
library(tidyr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
run_dir <- file.path('output/llm_annotation', args[1])
source_dirs <- file.path('output/llm_annotation', if (length(args) > 1) args[-1] else args[1])
dir.create(run_dir, showWarnings = FALSE)

# the extraction file of each study, from the last source run that has it
json_files <- map_dfr(source_dirs, \(d) tibble(file = list.files(d, pattern = '[.]json$', full.names = TRUE))) |>
  mutate(study_file = basename(file)) |>
  slice_tail(n = 1, by = study_file) |>
  pull(file)

manual <- readRDS('output/perio_bugs_corrected.rds') |>
  distinct(Study, .keep_all = TRUE)

# extracted field -> manual column
field_map <- c(
  study_design = 'Study design', location_of_subjects = 'Location of subjects', body_site = 'Body site',
  periodontitis_classification = 'Periodontitis classification', systemic_condition = 'Systemic condition',
  sequencing_type = 'Sequencing type', variable_region_16s = '16S variable region',
  sequencing_platform = 'Sequencing platform', plaque_measure = 'Plaque measure'
)
group_fields <- c(
  name = 'name', definition = 'definition', sample_size = 'sample size', age_mean = 'age mean', age_sd = 'age sd',
  num_males = 'num males', percent_males = 'percent males', num_smokers = 'num smokers',
  percent_smokers = 'percent smokers', bleeding_on_probing_percent = 'bleeding on probing percent',
  bleeding_on_probing_sd = 'bleeding on probing sd', suppuration_percent = 'suppuration percent',
  suppuration_sd = 'suppuration sd', pocket_depth_mean = 'pocket depth mean (mm)', pocket_depth_sd = 'pocket depth sd',
  clinical_attachment_loss_mean = 'clinical attachment loss (mm)', clinical_attachment_loss_sd = 'clinical attachment loss sd',
  plaque = 'plaque', plaque_sd = 'plaque sd'
)
for (g in 0:1) {
  field_map <- c(field_map, setNames(paste('Group', g, group_fields), paste0('group_', g, '_', names(group_fields))))
}

as_text <- \(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_character_ else paste(unlist(x), collapse = '; ')

extracted <- map_dfr(json_files, \(f) {
  j <- fromJSON(f, simplifyVector = FALSE)
  imap_dfr(j$fields, \(x, field) tibble(
    Study = j$study, field = field, extracted = as_text(x$value), quote = as_text(x$quote),
    location = as_text(x$location), confidence = as_text(x$confidence), note = as_text(x$note)
  ))
})

normalise <- \(x) gsub('[^a-z0-9.]', '', tolower(x))

# 16S regions as digit strings, so '1-3', 'V1-V3' and '123' match
expand_region <- function(x) {
  map_chr(x, \(r) {
    if (is.na(r)) return(NA_character_)
    r <- gsub('[Vv ]', '', r)
    if (grepl('^[0-9]-[0-9]$', r)) r <- paste(seq(substr(r, 1, 1), substr(r, 3, 3)), collapse = '')
    gsub('[^0-9]', '', r)
  })
}
same_value <- function(a, b) {
  na <- suppressWarnings(as.numeric(a))
  nb <- suppressWarnings(as.numeric(b))
  ifelse(!is.na(na) & !is.na(nb),
         abs(na - nb) <= pmax(0.051, 0.02 * abs(nb)),
         normalise(a) == normalise(b))
}

manual_long <- manual |>
  select(Study, all_of(unname(field_map))) |>
  mutate(across(-Study, as.character)) |>
  pivot_longer(-Study, names_to = 'manual_column', values_to = 'manual') |>
  mutate(field = names(field_map)[match(manual_column, field_map)])

# checkerboard, culture and other non-sequencing methods: the manual curation leaves the sequencing type
# empty and some extractions used 'PCR'; compare them as empty on both sides
hybridization <- extracted |>
  filter(field == 'sequencing_platform', grepl('hybridi|culture|maldi|mass spec', extracted, ignore.case = TRUE)) |>
  pull(Study)
extracted <- extracted |>
  mutate(extracted = case_when(
    field == 'sequencing_type' & Study %in% hybridization & extracted %in% c('PCR', 'DNA-DNA Hybridization') ~ NA,
    field == 'variable_region_16s' ~ expand_region(extracted),
    .default = extracted
  ))
manual_long <- mutate(manual_long, manual = ifelse(field == 'variable_region_16s', expand_region(manual), manual))

# free-text extractions mapped onto the categories the manual curation uses
classification_category <- function(x) {
  x <- tolower(x)
  case_when(
    is.na(x) ~ NA,
    grepl('chronic|adult', x) & grepl('aggressive|juvenile', x) ~ 'chronic and aggressive periodontitis',
    grepl('chronic|adult', x) ~ 'chronic periodontitis',
    grepl('aggressive|juvenile|molar-incisor', x) ~ 'aggressive periodontitis',
    grepl('stage|grade', x) ~ 'periodontitis (stage/grade)',
    .default = 'periodontitis (type not specified)'
  )
}
plaque_category <- \(x) case_when(is.na(x) ~ NA, grepl('%|percent|sites|surfaces', x, ignore.case = TRUE) ~ '% sites',
                                  .default = 'index')
site_category <- function(x) {
  x <- tolower(x)
  map_chr(x, \(s) if (is.na(s)) NA_character_ else
    paste(c('subgingival', 'supragingival', 'saliva', 'crevicular', 'tongue', 'mucosa')[
      c(grepl('subgingival', s), grepl('supragingival', s), grepl('saliva', s), grepl('crevicular|gcf', s),
        grepl('tongue', s), grepl('mucosa', s))], collapse = '+'))
}
categorise <- function(field, x) {
  case_when(field == 'periodontitis_classification' ~ classification_category(x),
            field == 'plaque_measure' ~ plaque_category(x),
            field == 'body_site' ~ site_category(x),
            .default = x)
}

# free-text fields (group names, case definitions) are paraphrased by the curators, so differences are
# wording to review rather than disagreements
wording_fields <- c('group_0_name', 'group_1_name', 'group_0_definition', 'group_1_definition')

# extractions keep SEMs as printed (with a note) while the manual curation converts them to SDs (SEM x sqrt(n))
sizes <- extracted |>
  filter(field %in% c('group_0_sample_size', 'group_1_sample_size')) |>
  transmute(Study, group = substr(field, 1, 7), n = suppressWarnings(as.numeric(extracted)))
sem_converted <- function(field, extracted, manual, Study) {
  group <- substr(field, 1, 7)
  n <- sizes$n[match(paste(Study, group), paste(sizes$Study, sizes$group))]
  e <- suppressWarnings(as.numeric(extracted))
  m <- suppressWarnings(as.numeric(manual))
  grepl('_sd$', field) & !is.na(e) & !is.na(m) & !is.na(n) & abs(e * sqrt(n) - m) <= pmax(0.051, 0.05 * m)
}

comparison <- extracted |>
  left_join(manual_long, by = c('Study', 'field')) |>
  mutate(status = case_when(
    is.na(manual_column) ~ 'new field',
    !is.na(manual) & !is.na(extracted) & same_value(extracted, manual) ~ 'agree',
    !is.na(manual) & !is.na(extracted) &
      normalise(categorise(field, extracted)) == normalise(categorise(field, manual)) ~ 'agree',
    sem_converted(field, extracted, manual, Study) ~ 'agree (SEM converted to SD)',
    !is.na(manual) & !is.na(extracted) & field %in% wording_fields ~ 'wording',
    !is.na(manual) & !is.na(extracted) ~ 'differs',
    is.na(manual) & !is.na(extracted) ~ 'fills gap',
    !is.na(manual) ~ 'manual only',
    .default = 'both missing'
  )) |>
  relocate(Study, field, manual_column, status, manual, extracted)

# a different Group 1 sample size usually means the extractor used a different comparison (e.g. aggressive
# instead of chronic periodontitis), so the other group-level differences for that study follow from it
comparison_choice <- comparison |>
  filter(field %in% c('group_0_sample_size', 'group_1_sample_size')) |>
  summarise(comparison_differs = any(status == 'differs'), .by = Study)
comparison <- left_join(comparison, comparison_choice, by = 'Study')

studies <- map_dfr(json_files, \(f) {
  j <- fromJSON(f, simplifyVector = FALSE)
  tibble(Study = j$study, comparison_found = isTRUE(j$comparison_found), comparison_note = as_text(j$comparison_note))
}) |>
  left_join(comparison_choice, by = 'Study')

write.csv(comparison, file.path(run_dir, 'comparison.csv'), row.names = FALSE)
write.csv(studies, file.path(run_dir, 'studies.csv'), row.names = FALSE)

# ---- files for curator review ----

review_cols <- c('Study', 'field', 'manual', 'extracted', 'confidence', 'quote', 'location', 'note')

# studies where the extractor used a different comparison than the curated one
comparison |>
  filter(comparison_differs, field %in% c('group_0_sample_size', 'group_1_sample_size')) |>
  select(Study, field, manual, extracted) |>
  pivot_wider(names_from = field, values_from = c(manual, extracted)) |>
  left_join(select(studies, Study, comparison_note), by = 'Study') |>
  write.csv(file.path(run_dir, 'review_comparison_choice.csv'), row.names = FALSE)

# disagreements where both used the same comparison: candidate corrections either way
comparison |>
  filter(!comparison_differs, status == 'differs') |>
  select(all_of(review_cols)) |>
  write.csv(file.path(run_dir, 'review_differences.csv'), row.names = FALSE)

# values the paper reports that the manual curation lacks (same comparison only)
comparison |>
  filter(!comparison_differs, status == 'fills gap') |>
  select(all_of(review_cols)) |>
  write.csv(file.path(run_dir, 'review_fills_gap.csv'), row.names = FALSE)

# the new fields, one row per study, for possible import
comparison |>
  filter(status == 'new field') |>
  select(Study, field, extracted) |>
  pivot_wider(names_from = field, values_from = extracted) |>
  write.csv(file.path(run_dir, 'new_fields.csv'), row.names = FALSE)
print(count(comparison, status))
print(count(studies, comparison_found, comparison_differs))

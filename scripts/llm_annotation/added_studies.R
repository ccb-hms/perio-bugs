# Studies added to the corrected PerioSigDB from the full-text review (pending curator confirmation)
# used by scripts/5-join_overview_and_microbe.R --corrected and scripts/llm_annotation/apply_review.R
#
# These are eligible studies (scripts/llm_annotation/extraction_instructions.md; the review protocol's
# PECOS criteria) that other microbe-disease resources hold but the curated data lacked
# (output/papers_not_in_periosigdb.csv):
# - extracted by the curators, whose taxa were never entered in the taxa list: their overview rows are the
#   curators'; their signatures are the curators' taxa lists from the overview sheet, checked against the paper
# - selected by the curators but never extracted, or found eligible on checking: their overview rows and
#   signatures come from the full-text extraction
#
# Inputs, written with the extraction:
# - output/llm_annotation/added/Study <ID>.json, the extraction (format as for the other review runs)
# - output/llm_annotation/added/taxa.csv, the signatures with NCBI IDs (scripts/llm_annotation/resolve_added_taxa.R)
#
# Values are taken as the review takes them (scripts/llm_annotation/apply_review.R): vocabulary terms only,
# SEMs converted to SDs, and only values extracted with high or medium confidence; medians are left out.

library(dplyr)
library(purrr)
library(jsonlite)

added_dir <- 'output/llm_annotation/added'

added_studies <- tribble(
  ~Study,                ~source,     ~reason,
  'Study 403',           'curators',  "extracted by the curators, but the taxa were never entered in the taxa list",
  'Study 1032',          'curators',  "extracted by the curators, but the taxa were never entered in the taxa list",
  'Study 1531',          'curators',  "extracted by the curators, but the taxa were never entered in the taxa list",
  'Study 6',             'extraction', "selected by the curators but never extracted",
  'Study 74',            'extraction', "selected by the curators but never extracted; 3 people per group, non-diabetic pair",
  'Study 105',           'extraction', "selected by the curators but never extracted",
  'Study 109',           'extraction', "selected by the curators but never extracted",
  'Study 167',           'extraction', "selected by the curators but never extracted",
  'Study added-30710838', 'extraction', "selected by the curators (as study 144, a number shared with Haffajee 2006) but never extracted"
)

# ---- overview rows from the extraction ----

# extracted field -> overview column, for the group fields
group_columns <- c(
  name = 'name', definition = 'definition', sample_size = 'sample size', age_mean = 'age mean', age_sd = 'age sd',
  num_males = 'num males', percent_males = 'percent males', num_smokers = 'num smokers',
  percent_smokers = 'percent smokers', bleeding_on_probing_percent = 'bleeding on probing percent',
  bleeding_on_probing_sd = 'bleeding on probing sd', suppuration_percent = 'suppuration percent',
  suppuration_sd = 'suppuration sd', pocket_depth_mean = 'pocket depth mean (mm)', pocket_depth_sd = 'pocket depth sd',
  clinical_attachment_loss_mean = 'clinical attachment loss (mm)', clinical_attachment_loss_sd = 'clinical attachment loss sd',
  plaque = 'plaque', plaque_sd = 'plaque sd'
)

# "V3-V4" -> 34, "V1-V3" -> 123, "V4" -> 4
variable_region <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_real_)
  n <- as.integer(regmatches(x, gregexpr('[0-9]', x))[[1]])
  if (!length(n)) return(NA_real_)
  if (grepl('-|–|to', x) && length(n) == 2) n <- seq(n[1], n[2])
  as.numeric(paste(n, collapse = ''))
}

read_extraction <- function(study) {
  fromJSON(file.path(added_dir, paste0(study, '.json')), simplifyVector = FALSE)
}

#' One overview row (the columns of output/overview_cleaned_corrected.rds) from a study's extraction.
overview_from_extraction <- function(study) {
  f <- read_extraction(study)$fields
  usable <- \(x) !is.null(x$value) && !identical(x$confidence, 'low') && !grepl('median', coalesce(x$note, ''), ignore.case = TRUE)
  value <- \(name) if (usable(f[[name]])) unlist(f[[name]]$value)[1] else NA
  text <- \(name) { v <- f[[name]]$value; if (is.null(v)) NA_character_ else paste(unlist(v), collapse = '; ') }
  num <- \(name) suppressWarnings(as.numeric(value(name)))

  row <- tibble(
    Number = sub('^Study ', '', study),
    `Study design` = intersect(text('study_design'), controlled_vocab$study_design)[1],
    `Location of subjects` = text('location_of_subjects'),
    `Host species` = 'Homo sapiens',
    `Body site` = 'Subgingival dental plaque',
    Condition = 'Periodontitis',
    `Periodontitis classification` = coalesce(classification_value(text('periodontitis_classification')),
                                              'Periodontitis (type not specified)'),
    `Systemic condition` = systemic_value(text('systemic_condition')),
    `Sequencing type` = intersect(text('sequencing_type'), controlled_vocab$sequencing_type)[1],
    `16S variable region` = variable_region(text('variable_region_16s')),
    `Sequencing platform` = intersect(text('sequencing_platform'), c(controlled_vocab$sequencing_platform, 'HOMIM'))[1],
    `Plaque measure` = case_when(grepl('%', text('plaque_measure')) ~ '% sites',
                                 grepl('index', text('plaque_measure'), ignore.case = TRUE) ~ 'index')
  )
  for (g in 0:1) {
    for (field in names(group_columns)) {
      name <- paste0('group_', g, '_', field)
      column <- paste('Group', g, group_columns[[field]])
      row[[column]] <- if (field %in% c('name', 'definition')) text(name) else num(name)
    }
    # SEMs reported by the paper are converted to SDs (SD = SEM * sqrt(n)), as in apply_review.R
    n <- row[[paste('Group', g, 'sample size')]]
    for (field in grep('_sd$', names(group_columns), value = TRUE)) {
      x <- f[[paste0('group_', g, '_', field)]]
      if (grepl('SEM|\\bSE\\b|standard error', coalesce(x$note, ''))) {
        column <- paste('Group', g, group_columns[[field]])
        row[[column]] <- round(row[[column]] * sqrt(n), 2)
      }
    }
  }
  row
}

added_overview <- function() {
  added_studies |>
    filter(source == 'extraction') |>
    pull(Study) |>
    map_dfr(overview_from_extraction)
}

# ---- signatures ----

added_taxa_table <- function() {
  read.csv(file.path(added_dir, 'taxa.csv'), colClasses = 'character') |>
    filter(Study %in% added_studies$Study)
}

#' Taxa of the added studies, as output/diff_species.rds (Number, taxid, direction).
added_taxa <- function() {
  added_taxa_table() |>
    filter(include == 'TRUE') |>
    transmute(Number = sub('^Study ', '', Study), taxid,
              direction = ifelse(direction == 'increased', 'up', 'dn')) |>
    distinct()
}

#' Review log rows: one per added study, and the taxa left for curators.
added_log <- function() {
  taxa <- added_taxa_table()
  bind_rows(
    transmute(added_studies, Study, item = 'study', action = 'added',
              reason = paste0(reason, '; ', ifelse(source == 'curators',
                                                   "curators' groups and taxa list, checked against the paper",
                                                   'groups, clinical data and taxa from the full-text extraction'))),
    taxa |>
      filter(include != 'TRUE') |>
      transmute(Study, item = paste('taxon', direction), proposed = taxon_name, action = 'flagged',
                reason = ifelse(grepl('^MISSING FROM CURATED LIST', note),
                                "the paper reports this taxon for the comparison, but the curators' list does not include it",
                                'no NCBI taxon for this name; left out of the signature'),
                location, note),
    taxa |>
      filter(include == 'TRUE', paper_direction_agrees == 'no' | found_in_paper == 'no') |>
      transmute(Study, item = paste('taxon', direction), curated = taxon_reported, action = 'flagged',
                reason = "curated taxon not found in the paper for this comparison, or in the other direction; kept",
                location, note)
  )
}

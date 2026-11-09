# corrections from the full-text review, applied to the corrected PerioSigDB (pending curator confirmation)
# used by scripts/5-join_overview_and_microbe.R --corrected: apply_review(perio_bugs)
#
# evidence: LLM extractions compared with the curated values (scripts/llm_annotation/compare.R, run
# 'final' = blind extractions, re-extracted for the curators' comparison where they had chosen another),
# and a check of flagged signatures against their papers (output/llm_annotation/flagged_signature_review.md)
#
# changes values, adds BugSigDB fields (statistical test, threshold, MHT correction, diversity, source,
# description), the curators' diagnostic method, and a 'Review status' column (proposed exclusions are kept,
# not dropped); writes output/llm_annotation/review_log.csv with every change ('changed'), proposed
# exclusion and item left for curators ('flagged'), each with the paper evidence
#
# conventions follow scripts/manual_corrections.R (SEMs as SDs, pooled subgroups, curators' groups kept),
# plus:
# - study design: case-control when the groups were recruited by periodontal status
# - systemic condition: describes the participants in the curated comparison, not the study's focus
# - sequencing type: BugSigDB terms (16S, 18S, ITS / ITS2, WMS, PCR); empty for checkerboard, culture and
#   MALDI-TOF; 16S microarrays are 16S
# - smokers: current smokers

library(dplyr)
library(tidyr)
library(purrr)

# ---- decisions from reviewing individual disagreements against the papers ----

decision <- function(Study, field, proposed, reason) {
  tibble(Study = paste('Study', Study), field, proposed = as.character(proposed), reason)
}

review_decisions <- bind_rows(
  # numbers
  decision('2053', c('group_0_age_mean', 'group_0_age_sd', 'group_1_age_mean', 'group_1_age_sd',
                     'group_0_num_males', 'group_0_percent_males', 'group_1_num_males', 'group_1_percent_males'),
           c(22.8, 8.5, 40.8, 11.9, 13, 40.6, 11, 34.4), 'curated values swapped between groups (Table 1)'),
  decision('1684', c('group_1_pocket_depth_mean', 'group_1_pocket_depth_sd', 'group_1_clinical_attachment_loss_mean',
                     'group_1_clinical_attachment_loss_sd', 'group_0_clinical_attachment_loss_mean',
                     'group_0_clinical_attachment_loss_sd'),
           c(2.31, 1, 0.63, 1.03, 0.08, 0.35),
           'curated values are the full-mouth % of sites with PD >= 4 / CAL >= 3 mm, read as mm; sampled-site means in mm'),
  decision('485', c('group_0_sample_size', 'group_1_sample_size', 'group_0_percent_males', 'group_1_percent_males'),
           c(7, 10, 42.9, 50), 'curated sizes are samples (4 per subject), not subjects; % males computed from samples'),
  decision('2025-PubMed-14', c('group_0_num_males', 'group_0_percent_males'), c(9, 39.1), 'Table 1'),
  decision('132', c('group_1_suppuration_percent', 'group_1_suppuration_sd'), c(5, 11),
           'reported as a proportion (0.05, 0.11); suppuration is % of sites'),
  decision('1897', c('group_1_num_smokers', 'group_1_percent_smokers'), c(17, 38.6),
           'current smokers (curated value included 4 former smokers)'),
  decision('1673', c('group_1_num_smokers', 'group_1_percent_smokers'), c(0, 0),
           'current smokers were excluded; the curated 29% are former smokers'),
  decision('2017', 'group_1_sample_size', 169, '169 chronic periodontitis subjects; 109 appears nowhere in the paper'),
  decision('2310', c('group_1_sample_size', 'periodontitis_classification'), c(29, 'Chronic periodontitis'),
           'the curated definition is the paper\'s "advanced adult periodontitis" group (n = 29); the paper has no aggressive periodontitis group'),
  # study details
  decision(c('2367', '1746', '1988'), 'location_of_subjects', c('Cameroon', 'India', 'United States of America'),
           'country of recruitment stated in the paper'),
  decision(c('123', '260', '2025-234'), 'body_site', 'Gingival crevicular fluid', 'samples were gingival crevicular fluid'),
  decision(c('145', '1907', '304', '428', '509', '549'), 'sequencing_platform',
           c('RT-qPCR', 'RT-qPCR', 'RT-qPCR', 'Illumina', 'Roche454', 'Illumina'), 'platform stated in the paper'),
  decision(c('140', '1867', '2067'), 'sequencing_type', '16S',
           'BugSigDB terms: clone libraries, DGGE band sequencing and 16S microarrays are 16S'),
  decision('1867', 'sequencing_platform', 'HOMIM', 'HOMIM 16S microarray (new platform term)'),
  decision('28', 'variable_region_16s', 567, 'V5-V7'),
  decision('549', 'group_1_name', 'treated periodontitis',
           'Group 1 is treated, well-maintained patients (Fig 2F); also resolve the k__Bacteria placeholder (G-1 952/869)')
)

# ---- proposed exclusions (see output/llm_annotation/flagged_signature_review.md) ----

review_exclusions <- tribble(
  ~Study,                  ~reason,
  'Study 450',             'taxa from healthy vs anti-CCP+ (rheumatoid arthritis risk) groups, not periodontitis',
  'Study 2025-PubMed-130', 'taxa are correlations with ACPA levels; health vs periodontitis never compared',
  'Study 1590',            'taxa are species shared by health and disease clusters in a network analysis',
  'Study 452',             'no significant health vs periodontitis differences; phylum shifts described from 4 vs 6 samples',
  'Study 1982',            'sampling-reproducibility study; health vs periodontitis not tested',
  'Study 155',             'no periodontally healthy group; only spirochetes (morphotype) tested at site level',
  'Study 134',             'duplicate of Study 1690 (PMID 21375703), with a subset of its taxa',
  'Study 1690',            'pilot data shown without per-taxon tests (if kept: add Prevotella sp. OT306, map Selenomonas sp. to Mitsuokella sp. OT131, drop C. sputigena)',
  'Study 144',             'curated groups (189 vs 635 chronic periodontitis) and definitions do not appear in the paper; likely from another study'
)

# ---- rules applied to the extracted values ----

# extracted systemic condition as a short value describing the compared participants
systemic_value <- function(x) {
  case_when(
    is.na(x) ~ NA,
    grepl('not restricted|not selected', x, ignore.case = TRUE) ~ 'not restricted',
    grepl('^non-?diabetic', x, ignore.case = TRUE) ~ 'non-diabetic',
    grepl('^HIV-negative', x, ignore.case = TRUE) ~ 'HIV-negative',
    grepl('systemically healthy|generally healthy|no serious systemic|no immunological|without COPD', x,
          ignore.case = TRUE) ~ 'systemically healthy',
    .default = NA
  )
}

classification_value <- function(x) {
  x <- tolower(x)
  case_when(
    grepl('chronic|adult', x) & grepl('aggressive|juvenile', x) ~ 'Chronic and aggressive periodontitis',
    grepl('chronic|adult', x) ~ 'Chronic periodontitis',
    grepl('aggressive|juvenile|molar-incisor', x) ~ 'Aggressive periodontitis',
    grepl('stage|grade', x) ~ 'Periodontitis (stage/grade)',
    .default = NA
  )
}

text_fields <- c('group_0_name', 'group_1_name', 'group_0_definition', 'group_1_definition', 'body_site',
                 'location_of_subjects', 'sequencing_platform', 'sequencing_type', 'variable_region_16s',
                 'plaque_measure', 'periodontitis_classification', 'systemic_condition', 'study_design')

as_char <- \(x) ifelse(is.na(x), NA_character_, as.character(x))
normalise <- \(x) gsub('[^a-z0-9.]', '', tolower(x))
differs <- function(a, b) {
  na <- suppressWarnings(as.numeric(a))
  nb <- suppressWarnings(as.numeric(b))
  ifelse(!is.na(na) & !is.na(nb), abs(na - nb) > pmax(0.051, 0.02 * abs(nb)), normalise(a) != normalise(b))
}

apply_review <- function(perio_bugs, run_dir = 'output/llm_annotation/final',
                         log_file = 'output/llm_annotation/review_log.csv') {
  extractions <- read.csv(file.path(run_dir, 'comparison.csv')) |>
    select(Study, field, manual_column, extracted, confidence, quote, location, note)
  studies <- read.csv(file.path(run_dir, 'studies.csv'))
  aligned <- studies$Study[studies$comparison_found & !studies$comparison_differs]
  vocab <- controlled_vocab
  confident <- \(x) x %in% c('high', 'medium')

  # current (pre-review) value of each mapped field, one row per study
  columns <- distinct(filter(extractions, !is.na(manual_column)), field, manual_column)
  current <- perio_bugs |>
    distinct(Study, .keep_all = TRUE) |>
    select(Study, all_of(columns$manual_column)) |>
    mutate(across(-Study, as_char)) |>
    pivot_longer(-Study, names_to = 'manual_column', values_to = 'curated')
  evidence <- left_join(extractions, current, by = c('Study', 'manual_column'))

  rules <- bind_rows(
    # numbers the paper reports that the curated data lacks
    evidence |>
      filter(Study %in% aligned, is.na(curated), !field %in% text_fields, confident(confidence),
             !is.na(suppressWarnings(as.numeric(extracted)))) |>
      mutate(proposed = extracted, reason = 'reported in the paper, missing in the curated data'),
    # study design by recruitment
    evidence |>
      filter(Study %in% aligned, field == 'study_design', confident(confidence), extracted %in% vocab$study_design) |>
      mutate(proposed = extracted, reason = 'study design: case-control when groups were recruited by periodontal status'),
    # systemic condition of the compared participants
    evidence |>
      filter(Study %in% aligned, field == 'systemic_condition') |>
      mutate(proposed = systemic_value(extracted),
             reason = 'systemic condition describes the participants in the curated comparison'),
    # classification where the curated value is 'type not specified' and the paper names the type
    evidence |>
      filter(Study %in% aligned, field == 'periodontitis_classification', confidence == 'high',
             coalesce(curated, 'Periodontitis (type not specified)') == 'Periodontitis (type not specified)') |>
      mutate(proposed = classification_value(extracted), reason = 'the paper names the type of periodontitis'),
    # sequencing type and platform missing in the curated data, in BugSigDB terms
    evidence |>
      filter(Study %in% aligned, field %in% c('sequencing_type', 'sequencing_platform'), is.na(curated),
             confident(confidence), extracted %in% c(vocab$sequencing_type, vocab$sequencing_platform)) |>
      mutate(proposed = extracted, reason = 'reported in the paper, missing in the curated data')
  ) |>
    filter(!is.na(proposed), !is.na(manual_column))

  # SEMs reported by the paper are converted to SDs (SD = SEM * sqrt(n)), as in manual_corrections.R
  sizes <- evidence |>
    filter(field %in% c('group_0_sample_size', 'group_1_sample_size')) |>
    transmute(Study, group = substr(field, 1, 7), n = as.numeric(coalesce(curated, extracted)))
  rules <- rules |>
    mutate(group = substr(field, 1, 7)) |>
    left_join(sizes, by = c('Study', 'group')) |>
    mutate(sem = grepl('_sd$', field) & grepl('SEM|\\bSE\\b|standard error', note),
           proposed = ifelse(sem, as.character(round(as.numeric(proposed) * sqrt(n), 2)), proposed),
           reason = ifelse(sem, paste(reason, '(SEM converted to SD)'), reason)) |>
    filter(!is.na(proposed)) |>
    select(-group, -n, -sem)

  decided <- review_decisions |>
    left_join(select(evidence, -manual_column), by = c('Study', 'field')) |>
    left_join(columns, by = 'field') |>
    mutate(manual_column = coalesce(manual_column, c(group_1_name = 'Group 1 name')[field]))

  # decisions take precedence over rules; unchanged values are dropped
  changes <- bind_rows(decided, anti_join(rules, decided, by = c('Study', 'field'))) |>
    filter(!Study %in% review_exclusions$Study, is.na(curated) | differs(proposed, curated))

  for (i in seq_len(nrow(changes))) {
    col <- changes$manual_column[i]
    rows <- perio_bugs$Study == changes$Study[i]
    value <- changes$proposed[i]
    perio_bugs[[col]][rows] <- if (is.numeric(perio_bugs[[col]])) as.numeric(value) else value
  }

  # new BugSigDB fields from the extractions (signature-level ones by direction)
  new_fields <- read.csv(file.path(run_dir, 'new_fields.csv'))
  perio_bugs <- perio_bugs |>
    left_join(new_fields, by = 'Study') |>
    mutate(
      `Statistical test` = statistical_test,
      `Significance threshold` = suppressWarnings(as.numeric(significance_threshold)),
      `MHT correction` = as.logical(toupper(mht_correction)),
      `LDA Score above` = suppressWarnings(as.numeric(lda_score_above)),
      `Data transformation` = data_transformation, `Matched on` = matched_on,
      `Confounders controlled for` = confounders_controlled_for,
      Pielou = pielou, Shannon = shannon, Chao1 = chao1, Simpson = simpson, `Inverse Simpson` = inverse_simpson,
      Richness = richness,
      Source = ifelse(`Abundance in Group 1` == 'increased', source_increased, source_decreased),
      Description = ifelse(`Abundance in Group 1` == 'increased', description_increased, description_decreased)
    ) |>
    select(-any_of(names(new_fields)[-1]))

  # the curators' diagnostic method (raw overview sheets, dropped when PerioSigDB was first built)
  overview <- readRDS('output/overview_merged.rds')
  perio_bugs$`Diagnostic method` <- trimws(gsub('[[:space:]]+', ' ',
    overview$`Diagnostic Method`[match(sub('Study ', '', perio_bugs$Study), overview$Number)]))

  perio_bugs$`Review status` <- ifelse(
    perio_bugs$Study %in% review_exclusions$Study,
    paste('proposed exclusion:', review_exclusions$reason[match(perio_bugs$Study, review_exclusions$Study)]),
    'included'
  )

  # items left for curators
  flags <- bind_rows(
    evidence |>
      filter(Study %in% aligned, !is.na(curated), !is.na(extracted), differs(extracted, curated),
             !field %in% c('group_0_name', 'group_1_name', 'group_0_definition', 'group_1_definition')) |>
      anti_join(changes, by = c('Study', 'field')) |>
      mutate(proposed = extracted,
             reason = 'paper value differs; not changed (low confidence, pooled subgroups, sampled vs full-mouth, SEM, or convention)'),
    evidence |>
      filter(Study %in% aligned, !is.na(curated), is.na(extracted), !field %in% text_fields) |>
      mutate(proposed = NA, reason = 'curated value not found in the retrieved full text (may be in a supplement)'),
    studies |>
      filter(!comparison_found | comparison_differs, !Study %in% review_exclusions$Study) |>
      transmute(Study, manual_column = 'comparison', note = comparison_note,
                reason = 'curated comparison not found as such in the paper (subgroups pooled but tested separately, or group sizes absent)')
  )

  bind_rows(
    mutate(changes, action = 'changed'),
    transmute(review_exclusions, Study, manual_column = 'study', reason, action = 'proposed exclusion'),
    mutate(flags, action = 'flagged')
  ) |>
    transmute(Study, item = manual_column, curated, proposed, action, reason, confidence, quote, location, note) |>
    arrange(Study, action, item) |>
    write.csv(log_file, row.names = FALSE)

  perio_bugs
}

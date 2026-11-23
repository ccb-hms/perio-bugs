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
  decision('added-30710838', c('group_0_percent_smokers', 'group_1_percent_smokers'), c(57.1, 27.3),
           'Table 1 gives current smokers as a percentage (controls 57.1, patients 27.3); counts cannot be derived'),
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
  # systemic comorbidity in the compared groups themselves (the curation meant to exclude these comparisons)
  'Study 126',             'systemic comorbidity: all participants are dyspepsia patients (both compared groups)',
  'Study 766',             'systemic comorbidity: the periodontitis group includes patients with coronary artery disease, diabetes and hypertension (Table S1); the healthy group excluded diabetes',
  'Study 2025-187',        'systemic comorbidity: both groups include participants with hypertension, high cholesterol or diabetes (Appendix Table 2); only cancer and inflammatory bowel disease were excluded'
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

# ---- smoking scope and confounder review ----

# variable names in BugSigDB's terms
confounder_terms <- c(smoking = 'smoking status', gender = 'sex')
# judgement calls on the reviewer's output
confounder_drop <- list(
  # the curated contrast is within the HIV-negative participants: restriction, not control
  'Study 1874' = 'HIV infection'
)

# curated smoker percentages that count former smokers too (the paper gives no current-smoker figure);
# the convention is current smokers, so these are left for curators
ever_smokers <- c('Study 1137', 'Study 153', 'Study 1687')

# per-study answers, the smoker counts to set to 0 (smokers excluded by design but the curated value says
# otherwise or is missing) and the items left for curators
read_smoking_review <- function(dir = 'output/llm_annotation/smoking', perio_bugs = NULL) {
  files <- list.files(dir, pattern = '\\.json$', full.names = TRUE)
  terms <- function(x, study) {
    x <- unlist(x)
    x <- setdiff(coalesce(confounder_terms[x], x), confounder_drop[[study]])
    if (length(x)) paste(x, collapse = '; ') else NA_character_
  }
  summary <- map_dfr(files, function(f) {
    j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    s <- j$smoking
    method <- \(v) coalesce(j$handling[[v]]$method, NA_character_)
    num <- \(x) if (is.null(x)) NA_real_ else as.numeric(x)
    tibble(
      Study = j$study, scope = s$scope, smokers_in_signature = s$smokers_in_signature,
      smoking_population = s$population,
      paper_group_0_percent = num(s$paper_group_0_percent), paper_group_1_percent = num(s$paper_group_1_percent),
      smoking_confidence = s$confidence, smoking_quote = s$quote, smoking_location = s$location,
      smoking_note = s$note,
      smoking_method = method('smoking'), age_method = method('age'), sex_method = method('sex'),
      matched_on = terms(j$matched_on, j$study), confounders_controlled_for = terms(j$confounders_controlled_for, j$study),
      fields_quote = coalesce(j$fields_quote, NA_character_), fields_location = coalesce(j$fields_location, NA_character_),
      summary = j$summary
    )
  })
  if (is.null(perio_bugs)) return(summary)

  curated <- distinct(perio_bugs, Study, .keep_all = TRUE) |>
    filter(Study %in% summary$Study) |>
    select(Study, matches('^Group [01] (num|percent) smokers$')) |>
    mutate(across(-Study, as_char)) |>
    pivot_longer(-Study, names_to = 'manual_column', values_to = 'curated')
  excluded_by_design <- summary |>
    filter(scope == 'smokers excluded by design', smoking_confidence == 'high')
  smokers <- curated |>
    filter(Study %in% excluded_by_design$Study, is.na(curated) | as.numeric(curated) != 0) |>
    left_join(select(excluded_by_design, Study, quote = smoking_quote, location = smoking_location, note = summary),
              by = 'Study') |>
    mutate(field = manual_column, proposed = '0', confidence = 'high',
           reason = 'smokers were excluded from the study, so the compared groups have no smokers')

  percent <- curated |>
    filter(grepl('percent', manual_column)) |>
    mutate(group = sub('^Group ([01]).*', '\\1', manual_column))
  paper <- summary |>
    select(Study, `0` = paper_group_0_percent, `1` = paper_group_1_percent) |>
    pivot_longer(-Study, names_to = 'group', values_to = 'paper')
  flags <- bind_rows(
    # smoking figures that do not describe the signature's participants
    summary |>
      filter(!scope %in% c('signature participants', 'smokers excluded by design') |
               smokers_in_signature %in% 'unclear') |>
      transmute(Study, manual_column = 'smoking scope', proposed = paste0(scope, '; smokers in signature: ', smokers_in_signature),
                quote = smoking_quote, location = smoking_location, confidence = smoking_confidence,
                note = paste(summary, smoking_note),
                reason = 'smoking figures may not describe the participants whose samples produced the signature'),
    # curated percentages of current or former smokers
    summary |>
      filter(Study %in% ever_smokers) |>
      transmute(Study, manual_column = 'Group 1 percent smokers',
                curated = percent$curated[match(paste(Study, manual_column), paste(percent$Study, percent$manual_column))],
                quote = smoking_quote, location = smoking_location,
                confidence = smoking_confidence, note = smoking_note,
                reason = 'the paper counts current and former smokers together; the convention is current smokers only'),
    # paper percentages that differ from the curated ones
    percent |>
      inner_join(paper, by = c('Study', 'group')) |>
      filter(!is.na(paper), is.na(curated) | abs(paper - as.numeric(curated)) > 1) |>
      anti_join(smokers, by = c('Study', 'manual_column')) |>
      left_join(select(summary, Study, quote = smoking_quote, location = smoking_location,
                       confidence = smoking_confidence, note = smoking_note), by = 'Study') |>
      transmute(Study, manual_column, curated, proposed = as.character(paper), quote, location, confidence, note,
                reason = 'smoking and confounder review: the paper\'s percentage of current smokers differs; not changed')
  )
  list(summary = summary, smokers = smokers, flags = flags)
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
  # studies sequenced for 16S (curated or extracted), and studies with plaque values (curated or extracted)
  sequenced_16s <- evidence |>
    filter(field == 'sequencing_type', coalesce(curated, extracted) == '16S') |>
    pull(Study)
  with_plaque <- evidence |>
    filter(field %in% c('group_0_plaque', 'group_1_plaque'),
           !is.na(curated) | (Study %in% aligned & confident(confidence) & !is.na(suppressWarnings(as.numeric(extracted))))) |>
    pull(Study) |>
    unique()

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
      mutate(proposed = extracted, reason = 'reported in the paper, missing in the curated data'),
    # 16S variable region missing in the curated data, for 16S sequencing studies (not probe hybridization)
    evidence |>
      filter(Study %in% aligned, field == 'variable_region_16s', is.na(curated), confident(confidence),
             grepl('^[0-9]+$', extracted), Study %in% sequenced_16s) |>
      mutate(proposed = extracted, reason = 'reported in the paper, missing in the curated data'),
    # plaque measure missing in the curated data, where the study reports plaque values
    evidence |>
      filter(Study %in% aligned, field == 'plaque_measure', is.na(curated), confident(confidence),
             Study %in% with_plaque) |>
      mutate(proposed = case_when(grepl('%', extracted) ~ '% sites',
                                  grepl('index', extracted, ignore.case = TRUE) ~ 'index'),
             reason = 'reported in the paper, missing in the curated data')
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

  # smoking scope and confounder review of the studies reporting smokers (scripts/llm_annotation/
  # smoking_confounder_instructions.md): its matched / adjusted variables replace the first review's
  smoking <- read_smoking_review(perio_bugs = perio_bugs)
  write.csv(smoking$summary, 'output/llm_annotation/smoking_review.csv', row.names = FALSE)
  first_review <- distinct(perio_bugs, Study, .keep_all = TRUE) |>
    select(Study, `Matched on`, `Confounders controlled for`) |>
    pivot_longer(-Study, names_to = 'manual_column', values_to = 'curated')
  confounder_changes <- smoking$summary |>
    transmute(Study, `Matched on` = matched_on, `Confounders controlled for` = confounders_controlled_for) |>
    pivot_longer(-Study, names_to = 'manual_column', values_to = 'proposed') |>
    left_join(first_review, by = c('Study', 'manual_column')) |>
    filter(!Study %in% review_exclusions$Study, coalesce(proposed, '') != coalesce(curated, '')) |>
    left_join(select(smoking$summary, Study, quote = fields_quote, location = fields_location, note = summary),
              by = 'Study') |>
    mutate(reason = 'smoking and confounder review: variables matched on or adjusted for in the analysis that produced the signature',
           confidence = 'high')
  smoker_changes <- smoking$smokers |>
    filter(!Study %in% review_exclusions$Study)
  for (ch in list(confounder_changes, smoker_changes)) {
    for (i in seq_len(nrow(ch))) {
      col <- ch$manual_column[i]
      rows <- perio_bugs$Study == ch$Study[i]
      perio_bugs[[col]][rows] <- if (is.numeric(perio_bugs[[col]])) as.numeric(ch$proposed[i]) else ch$proposed[i]
    }
  }
  changes <- bind_rows(changes, mutate(confounder_changes, field = manual_column), smoker_changes)

  # the curators' diagnostic method (raw overview sheets, dropped when PerioSigDB was first built)
  overview <- readRDS('output/overview_merged.rds')
  perio_bugs$`Diagnostic method` <- trimws(gsub('[[:space:]]+', ' ',
    overview$`Diagnostic Method`[match(sub('Study ', '', perio_bugs$Study), overview$Number)]))

  # studies added from the review (added_studies.R): the extraction's diagnostic method where the curators
  # have none
  source('scripts/llm_annotation/added_studies.R', local = TRUE)
  extracted_method <- filter(extractions, field == 'diagnostic_method')
  perio_bugs$`Diagnostic method` <- coalesce(
    perio_bugs$`Diagnostic method`,
    ifelse(perio_bugs$Study %in% added_studies$Study,
           extracted_method$extracted[match(perio_bugs$Study, extracted_method$Study)], NA)
  )

  perio_bugs$`Review status` <- case_when(
    perio_bugs$Study %in% review_exclusions$Study ~
      paste('proposed exclusion:', review_exclusions$reason[match(perio_bugs$Study, review_exclusions$Study)]),
    perio_bugs$Study %in% added_studies$Study ~
      paste('added:', added_studies$reason[match(perio_bugs$Study, added_studies$Study)]),
    .default = 'included'
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
                reason = 'curated comparison not found as such in the paper (subgroups pooled but tested separately, or group sizes absent)'),
    filter(smoking$flags, !Study %in% review_exclusions$Study)
  )

  bind_rows(
    mutate(changes, action = 'changed'),
    transmute(review_exclusions, Study, manual_column = 'study', reason, action = 'proposed exclusion'),
    mutate(flags, action = 'flagged')
  ) |>
    transmute(Study, item = manual_column, curated, proposed, action, reason, confidence, quote, location, note) |>
    bind_rows(added_log()) |>
    arrange(Study, action, item) |>
    write.csv(log_file, row.names = FALSE)

  perio_bugs
}

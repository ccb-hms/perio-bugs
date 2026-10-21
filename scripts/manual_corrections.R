# manual corrections and additions to cleaned overview values
# used by 2-clean_overview.R (corrections, new columns) and 5-join_overview_and_microbe.R (duplicates)
#
# found by reviewing the LLM-cleaned values in output/prompt_fixes.rds against the raw
# overview sheets and, where the raw value was ambiguous, against the source paper
# (data/articles_included/ or PubMed Central)
#
# each correction has a 'source':
# - 'raw': value re-parsed from the raw overview sheet (LLM parsing error)
# - 'paper': value differs from or adds to the raw overview sheet, based on the paper
#   (to confirm with curators; written to output/manual_corrections.csv)
#
# conventions (in addition to those in run_prompt.R and manual_fixes_new_data.R):
# - subgroup means/SDs are pooled (rather than set to 'NA') when subgroup sizes are known
# - standard errors of the mean are converted to SDs (SD = SEM * sqrt(n))
# - bleeding on probing is % of sites; bleeding/gingival indices are set to NA
# - plaque is kept on its reported scale and its scale recorded in 'Plaque measure'
# - curators' selection of groups/subgroups from each paper is kept

library(tibble)
library(dplyr)

# helpers ----

# pooled mean and SD of subgroups (Cochrane Handbook, Table 6.5.a)
pool_mean <- function(n, m) sum(n * m) / sum(n)

pool_sd <- function(n, m, s) {
  pm <- pool_mean(n, m)
  sqrt((sum((n - 1) * s^2) + sum(n * (m - pm)^2)) / (sum(n) - 1))
}

sem_to_sd <- function(sem, n) sem * sqrt(n)

# cleaned_df column names of each measure: mean/num then sd/percent
measure_cols <- list(
  size = c('sample size', NA),
  age = c('age mean', 'age sd'),
  males = c('num males', 'percent males'),
  smokers = c('num smokers', 'percent smokers'),
  bop = c('bleeding on probing percent', 'bleeding on probing sd'),
  supp = c('suppuration percent', 'suppuration sd'),
  pd = c('pocket depth mean (mm)', 'pocket depth sd'),
  cal = c('clinical attachment loss (mm)', 'clinical attachment loss sd'),
  plaque = c('plaque', 'plaque sd')
)

# rows of corrections for one measure of one group (0: health, 1: periodontitis)
# value2 is the SD (or percent for males/smokers); NULL leaves it unchanged
fix <- function(Number, group, measure, value, value2 = NULL, reason, source = 'raw') {
  cols <- paste('Group', group, measure_cols[[measure]])
  vals <- list(value, value2)
  keep <- !vapply(vals, is.null, logical(1)) & !is.na(measure_cols[[measure]])
  tibble(
    Number = Number,
    column = cols[keep],
    value = as.numeric(unlist(vals[keep])),
    reason = reason,
    source = source
  )
}

# pooled mean and SD of subgroups for one measure of one group
fix_pooled <- function(Number, group, measure, n, m, s = NULL, reason = 'pooled subgroups', source = 'raw') {
  sd <- if (is.null(s)) NA else pool_sd(n, m, s)
  fix(Number, group, measure, pool_mean(n, m), sd, reason = reason, source = source)
}

# duplicated studies ----

# same paper entered twice with identical signatures: drop (names) in favour of (values)
duplicate_studies <- c(
  '138' = '1874',  # PMID 19639349
  '143' = '2010',  # PMID 17980633
  '589' = '129'    # PMID 32315811
)

# periodontitis classification ----

# from 'Diagnosis' and 'Criteria used to define periodontitis' of 'Periodontitis' sheet
perio_classification <- list(
  'Chronic periodontitis' = c(
    '136', '1817', '1874', '2001', '2010', '2017', '2061', '132', '138', '143', '296', '304',
    '462', '484', '782', '1025', '1137', '1141', '1968', '1446', '1588', '2043', '2044', '147',
    '1818', '918', '1093', '131', '1496', '887', '1915', '2066', '2108', '126', '766', '716',
    '1581', '2067', '127', '1907', '1982', '639', '1106', '144', '1174', '260', '452', '2025-253'
  ),
  'Aggressive periodontitis' = c(
    '1667', '1798', '1910', '1539', '1897', '1986', '2310', '1085', '2202'
  ),
  'Chronic and aggressive periodontitis' = c(
    '1687', '139', '811', '1158', '970', '137', '1752', '711', '129', '589', '2225', '28'
  ),
  # Diagnosis 'PR': refractory periodontitis vs health (good responders not used)
  'Refractory periodontitis' = '1867',
  # 2018 classification (Papapanou et al. 2018)
  'Periodontitis (stage/grade)' = c(
    '116', '191', '428', '485', '2025-PubMed-14', '2025-PubMed-49', '2025-144', '2025-180',
    '2025-187', '2025-224', '2025-234', '2025-246', '2025-274', '2025-PubMed-357',
    '2025-PubMed-504'
  )
)
# all others: 'Periodontitis (type not specified)'

# systemic condition ----

# 'Syst. Condition' of 'General information' sheet
# NOTE: for most studies in OVERVIEW_SHEET_RY.xlsx this column lists the groups
# (e.g. 'H , CP') rather than systemic condition, so these are left as NA
systemically_healthy <- c(
  '1025', '1085', '1106', '1137', '1141', '1158', '116', '1174', '123', '125', '1253', '131',
  '132', '134', '136', '137', '139', '140', '143', '144', '1446', '147', '148', '1496', '153',
  '1539', '156', '191', '28', '296', '304', '428', '448', '462', '484', '485', '509', '534',
  '549', '639', '672', '716', '782', '811', '887', '970', '2025-144', '2025-224', '2025-234',
  '2025-246', '2025-253', '2025-274', '2025-PubMed-357', '2025-PubMed-49', '2025-PubMed-504',
  '2025-PubMed-55'
)

systemic_disease <- c(
  '126' = 'dyspepsia',
  '129' = 'rheumatoid arthritis',
  '589' = 'rheumatoid arthritis',
  '138' = 'HIV',
  '1411' = 'diabetes mellitus',
  '450' = 'rheumatoid arthritis',
  '515' = 'chronic obstructive pulmonary disease',
  '734' = 'type 2 diabetes mellitus',
  '766' = 'cardiovascular disease, diabetes mellitus (periodontitis group)',
  '2025-180' = 'chronic obstructive pulmonary disease',
  '2025-187' = 'hypertension, hypercholesterolaemia, diabetes mellitus',
  '2025-231' = 'hypertension',
  '2025-Embase-55' = 'type 2 diabetes mellitus',
  '2025-PubMed-130' = 'rheumatoid arthritis'
)

# standard errors of the mean reported as SDs ----

# SD columns of these measures are converted with sem_to_sd() using the group size
# NOTE: values set in 'corrections' below are already SDs and are not converted
sem <- function(Number, measures, reason, groups = 0:1) {
  tidyr::expand_grid(Number = Number, group = groups, measure = measures) |>
    mutate(reason = reason)
}

sem_sds <- bind_rows(
  sem('1874', c('pd', 'cal'), 'Table 1: PD and CAL mean (± SEM)'),
  sem('1910', c('pd', 'cal', 'bop'), 'Table 2: mean ± SEM (age labelled SD)'),
  sem('2061', c('pd', 'cal'), 'Table 2: PD and CAL mean ± SE'),
  sem('2066', c('age', 'bop', 'supp', 'pd', 'cal', 'plaque'), 'Table 1: mean ± SEM'),
  sem('2104', c('age', 'bop', 'pd', 'cal', 'plaque'), 'Table 1: mean ± SEM'),
  sem('2108', c('age', 'bop', 'supp', 'pd', 'cal', 'plaque'), 'Table 1: mean ± SEM'),
  sem('2260', c('age', 'bop', 'supp', 'pd', 'cal', 'plaque'), 'Table 2: mean (± SEM); age in text unlabelled'),
  sem('734', c('age', 'bop', 'pd'), 'Table 1: mean ± s.e.m')
)

# value corrections: LLM parsing errors ----

corrections_raw <- bind_rows(
  # 1897: current (17) and former (4) smokers of the same group (n = 44), not subgroups
  fix('1897', 1, 'smokers', 17 + 4, 100 * 21 / 44, reason = 'current and former smokers summed as subgroups'),

  # 2017: '35.5' is percent males
  fix('2017', 0, 'males', NA, 35.5, reason = 'percent parsed as number (35) and percent (50)'),

  # 549: '47,62%' and '41,18%' are percents with decimal commas (10/21 and 7/17)
  fix('549', 0, 'males', NA, 47.62, reason = 'decimal comma parsed as number and percent'),
  fix('549', 1, 'males', NA, 41.18, reason = 'decimal comma parsed as number and percent'),
  fix('549', 1, 'pd', 2.51, 1.14, reason = 'decimal commas, full mouth'),

  # 589 (same as 129): 'AP: 2 / CP: 20' parsed as 100%
  fix('589', 1, 'males', 22, NA, reason = 'percent computed as 100%'),

  # 129: AP (n = 7) and CP (n = 55), raw values of duplicate entry 589
  fix('129', 1, 'males', 2 + 20, NA, reason = "'AP: 2 / CP: 20' (from 589)"),
  fix('129', 1, 'smokers', 2 + 17, NA, reason = "'AP: 2 / CP: 17' (from 589)"),
  fix_pooled('129', 1, 'age', c(7, 55), c(30.3, 54.1), c(8.8, 11.1)),

  # 2202: '24.6 (4.6, 14-32)' and '7.1 (1.9, 4-13)' are mean (SD, range)
  fix('2202', 1, 'age', 24.6, 4.6, reason = 'mean (SD, range)'),
  fix('2202', 1, 'pd', 7.1, 1.9, reason = 'mean (SD, range)'),

  # 462: periodontitis BOP '19.3 ± 2.8 ... AND 19.5 (2.5) - GI' (prompt example not followed)
  fix('462', 1, 'bop', 19.3, 2.8, reason = 'BOP (% sites) set to NA'),

  # 639: '100' deep / '8 ± 3' shallow sampled sites only
  fix('639', 1, 'bop', NA, NA, reason = 'shallow sampled-site BOP used'),

  # 1415: '165.0 ± 6.5 (disease free) periodontal sites' is a number of sites, not plaque
  fix('1415', 0, 'plaque', NA, NA, reason = 'value is number of sites, not plaque'),
  fix('1415', 1, 'plaque', NA, NA, reason = 'value is number of sites, not plaque'),

  # 153: curators excluded the 'recession' subgroup (see Other comments); Group 0 is health
  # and gingivitis (as entered), size from Table 1 (health 13, gingivitis 18)
  fix('153', 1, 'size', 12, reason = "'12 active / 5 recession' summed although recession excluded"),
  fix('153', 1, 'age', 41.5, 8.2, reason = 'active subgroup'),
  fix('153', 1, 'males', 5, 41.7, reason = 'active subgroup'),
  fix('153', 1, 'smokers', 10, 83, reason = 'active subgroup'),
  fix('153', 1, 'bop', 6.5, 5.3, reason = 'active subgroup'),
  fix('153', 1, 'cal', 1.6, 0.2, reason = 'active subgroup'),
  fix('153', 1, 'plaque', 47.3, 29.7, reason = 'active subgroup'),

  # subgroups pooled (subgroup sizes from 'Sample size (periodontitis)')
  fix_pooled('139', 1, 'age', c(15, 25, 30), c(15.2, 25.2, 42.0), c(2.8, 3.2, 6.2)),
  fix_pooled('139', 1, 'bop', c(15, 25, 30), c(35.5, 68.7, 63.6), c(4.8, 15.8, 20.2)),
  fix_pooled('139', 1, 'cal', c(15, 25, 30), c(3.0, 4.7, 4.3), c(0.4, 1.2, 1.0)),
  fix_pooled('139', 1, 'plaque', c(15, 25, 30), c(41.7, 49.2, 84.7), c(8.1, 12.9, 10.7)),

  fix_pooled('811', 1, 'age', c(30, 30), c(26.3, 42.0), c(3.5, 5.7)),
  # second bleeding measure is BOP (first is gingival bleeding)
  fix_pooled('811', 1, 'bop', c(30, 30), c(45.5, 42.1), c(15.9, 29.9)),
  fix_pooled('811', 1, 'supp', c(30, 30), c(4.32, 3.09), c(3.49, 3.7)),
  fix_pooled('811', 1, 'pd', c(30, 30), c(4.1, 4.0), c(0.9, 1.1)),
  fix_pooled('811', 1, 'cal', c(30, 30), c(3.8, 3.7), c(1.1, 1.0)),
  fix_pooled('811', 1, 'plaque', c(30, 30), c(78.1, 86.4), c(16.7, 18.9)),

  fix_pooled('1158', 1, 'age', c(35, 24), c(44.6, 33.1), c(11.4, 3.9)),
  fix_pooled('1158', 1, 'bop', c(35, 24), c(51.5, 72.3), c(18.0, 18.6)),
  fix_pooled('1158', 1, 'supp', c(35, 24), c(0.4, 1.1), c(0.9, 1.9)),
  fix_pooled('1158', 1, 'pd', c(35, 24), c(2.8, 4.6), c(0.5, 0.7)),
  fix_pooled('1158', 1, 'cal', c(35, 24), c(3.3, 5.2), c(1.2, 1.0)),
  fix_pooled('1158', 1, 'plaque', c(35, 24), c(55.7, 72.3), c(25.6, 18.6)),

  fix_pooled('137', 1, 'age', c(78, 63), c(40.6, 48.6), c(9.8, 9.6)),
  fix_pooled('137', 1, 'bop', c(78, 63), c(78.1, 69.5), c(22.7, 24.9)),
  fix_pooled('137', 1, 'cal', c(78, 63), c(6.5, 5.9), c(1.5, 1.4)),

  fix_pooled('1752', 1, 'age', c(62, 82), c(34.2, 54.4), c(6.2, 12.1)),
  fix_pooled('1752', 1, 'pd', c(62, 82), c(7.9, 5.8), c(1.3, 1.4)),

  fix_pooled('711', 1, 'pd', c(30, 30), c(7.81, 9.08), c(0.62, 0.70)),
  fix_pooled('711', 1, 'plaque', c(30, 30), c(2.25, 1.06), c(0.48, 0.35)),

  # GCP SD printed as 60 (typo in paper) so SD not pooled
  fix_pooled('2225', 1, 'age', c(18, 17, 14), c(21.8, 31, 43.9)),
  fix_pooled('148', 1, 'age', c(51, 29), c(43.7, 43.0), c(11.5, 8.8)),
  fix_pooled('155', 1, 'age', c(38, 25), c(38.3, 49.7), c(11.0, 11.0)),
  fix_pooled('28', 1, 'age', c(75, 95), c(34.05, 45.76), c(6.39, 9.21)),

  # Group 0 combines health (n = 139) and gingivitis (n = 103), as its sample size does
  fix_pooled('298', 0, 'age', c(139, 103), c(28.3, 29.2), c(11.1, 12)),
  fix_pooled('298', 0, 'pd', c(139, 103), c(1.8, 2.0), c(0.3, 0.2)),
  fix_pooled('298', 0, 'cal', c(139, 103), c(1.8, 2.0), c(0.5, 0.4)),
  fix_pooled('298', 0, 'plaque', c(139, 103), c(10.9, 35.7), c(12.2, 16.6)),

  fix_pooled('2025-PubMed-49', 1, 'age', c(12, 18), c(43.5, 48.8), c(3.8, 2.6)),
  fix_pooled('2025-PubMed-49', 1, 'pd', c(12, 18), c(2.7, 3.7), c(0.0, 0.1)),
  fix_pooled('2025-PubMed-49', 1, 'cal', c(12, 18), c(2.8, 4.2), c(0.0, 0.2)),
  fix_pooled('2025-PubMed-49', 1, 'bop', c(12, 18), c(67.2, 74.0), c(4.1, 3.9)),
  fix_pooled('2025-PubMed-49', 1, 'plaque', c(12, 18), c(1.2, 1.1), c(0.1, 0.1)),

  # all-sites values (full mouth) used
  fix_pooled('2025-PubMed-504', 1, 'age', c(10, 10), c(38.90, 35.20), c(10.64, 8.26)),
  fix_pooled('2025-PubMed-504', 1, 'bop', c(10, 10), c(63.00, 73.00), c(10.93, 9.91)),
  fix_pooled('2025-PubMed-504', 1, 'pd', c(10, 10), c(3.55, 3.89), c(0.37, 0.35)),
  fix_pooled('2025-PubMed-504', 1, 'cal', c(10, 10), c(3.84, 4.40), c(0.48, 0.43)),

  fix_pooled('2025-144', 1, 'age', c(6, 10, 12, 5), c(35.85, 39.58, 40.87, 49.00), c(2.54, 4.74, 6.20, 8.8)),
  fix_pooled('2025-144', 1, 'pd', c(6, 10, 12, 5), c(2.56, 2.71, 4.20, 4.47), c(0.71, 0.85, 1.51, 1.36)),
  fix_pooled('2025-144', 1, 'cal', c(6, 10, 12, 5), c(1.28, 1.65, 3.17, 4.04), c(0.60, 0.83, 1.27, 1.32)),

  fix_pooled('2025-PubMed-14', 1, 'age', c(16, 24, 32, 9), c(43.88, 49.92, 46.94, 56.33), c(8.62, 8.99, 8.58, 5.80)),
  fix_pooled('2025-PubMed-14', 1, 'pd', c(16, 24, 32, 9), c(2.74, 3.09, 3.94, 3.99), c(0.48, 0.78, 0.73, 1.20)),
  fix_pooled('2025-PubMed-14', 1, 'cal', c(16, 24, 32, 9), c(2.85, 3.39, 4.59, 5.56), c(0.45, 0.57, 1.40, 1.66))
)

# value corrections: based on the paper (to confirm with curators) ----

corrections_paper <- bind_rows(
  # 1687: CP (n = 219) and AP (n = 90), Table 1 (mean ± SEM)
  # 'Sample size (periodontitis)' was entered as % females (60, 67), AP smokers as '4,1' (9.1)
  fix('1687', 1, 'size', 219 + 90, reason = "Table 1: CP n = 219, AP n = 90 (entered '60.67' is % females)"),
  fix('1687', 0, 'age', 30.6, sem_to_sd(1.5, 51), reason = 'Table 1: SEM'),
  fix_pooled('1687', 1, 'age', c(219, 90), c(45.4, 31.4), sem_to_sd(c(0.7, 0.6), c(219, 90)), reason = 'Table 1: pooled CP and AP, SEM'),
  fix('1687', 1, 'males', NA, pool_mean(c(219, 90), c(40, 33)), reason = 'Table 1: pooled CP and AP'),
  fix('1687', 1, 'smokers', NA, pool_mean(c(219, 90), c(44.7, 9.1)), reason = "Table 1: AP smokers 9.1% (entered '4,1')"),
  fix('1687', 0, 'pd', 1.8, sem_to_sd(0.04, 51), reason = 'Table 1: SEM (LLM also used wrong value 2.4)'),
  fix_pooled('1687', 1, 'pd', c(219, 90), c(2.9, 3.9), sem_to_sd(c(0.06, 0.09), c(219, 90)), reason = 'Table 1: pooled CP and AP, SEM'),
  fix('1687', 0, 'cal', 1.7, sem_to_sd(0.06, 51), reason = 'Table 1: SEM'),
  fix_pooled('1687', 1, 'cal', c(219, 90), c(3.5, 4.3), sem_to_sd(c(0.08, 0.12), c(219, 90)), reason = 'Table 1: pooled CP and AP, SEM'),
  fix('1687', 0, 'bop', 4.0, sem_to_sd(0.5, 51), reason = 'Table 1: SEM'),
  fix_pooled('1687', 1, 'bop', c(219, 90), c(40.9, 64.5), sem_to_sd(c(1.6, 3.1), c(219, 90)), reason = 'Table 1: pooled CP and AP, SEM'),
  fix('1687', 0, 'plaque', 13.1, sem_to_sd(2.7, 51), reason = 'Table 1: supragingival biofilm not entered, SEM'),
  fix_pooled('1687', 1, 'plaque', c(219, 90), c(63.8, 70.3), sem_to_sd(c(1.8, 2.5), c(219, 90)), reason = 'Table 1: supragingival biofilm not entered, SEM'),

  # 153: healthy size not entered (Group 0 is health 13 + gingivitis 18 as entered)
  fix('153', 0, 'size', 13 + 18, reason = 'Table 1: health 13, gingivitis 18 (size not entered)'),

  # 1874: SEM of healthy PD printed as 0.5 (likely typo for 0.05)
  fix('1874', 0, 'pd', 2.7, NA, reason = 'Table 1: healthy PD SEM printed as 0.5, unreliable'),

  # smoking was an exclusion criterion for both groups (periodontitis not entered)
  fix('1874', 1, 'smokers', 0, 0, reason = 'smokers excluded (both groups)', source = 'paper'),
  fix('1910', 1, 'smokers', 0, 0, reason = 'smokers excluded (both groups)'),
  fix('2010', 1, 'smokers', 0, 0, reason = 'smokers excluded (both groups)'),
  fix('2066', 1, 'smokers', 0, 0, reason = 'all subjects non-smokers'),
  fix('2108', 1, 'smokers', 0, 0, reason = 'all subjects non-smokers'),
  fix('1915', 1, 'smokers', 0, 0, reason = 'all participants non-smokers'),

  # 1910: supragingival biofilm (SEM) not entered
  fix('1910', 0, 'plaque', 33.8, sem_to_sd(5, 11), reason = 'Table 2: supragingival biofilm not entered, SEM'),
  fix('1910', 1, 'plaque', 71, sem_to_sd(4.4, 18), reason = 'Table 2: supragingival biofilm not entered, SEM'),

  # 2010: '% males' entered in the 'n' column
  fix('2010', 0, 'males', NA, 41, reason = 'Table 1: 41 is % males'),
  fix('2010', 1, 'males', NA, 29, reason = 'Table 1: 29 is % males'),

  # 2061: periodontitis smokers entered as 43.1 (healthy % males); HIV-negative groups
  fix('2061', 1, 'smokers', NA, 28.6, reason = 'Table 2: periodontitis smokers 28.6%'),

  # 1915: healthy PD printed as 0.20 mm (implausible); BOP (fraction) and OHI-S not entered
  fix('1915', 0, 'pd', NA, NA, reason = 'healthy PD 0.20 ± 0.01 mm in paper is implausible (misprint)'),
  fix('1915', 0, 'bop', 0, 0, reason = 'Table 1: BOP not entered (0.00 ± 0.00 fraction of sites)'),
  fix('1915', 1, 'bop', 78, 11, reason = 'Table 1: BOP not entered (0.78 ± 0.11 fraction of sites)'),
  fix('1915', 0, 'plaque', 0.26, 0.03, reason = 'Table 1: OHI-S not entered'),
  fix('1915', 1, 'plaque', 2.84, 0.58, reason = 'Table 1: OHI-S not entered'),

  # 137: CP males entered as 24 (38.5%); healthy-group values not entered; plaque was AgP only
  fix('137', 1, 'males', 30 + 22, 100 * 52 / 141, reason = 'Table 1: CP males 22 (34.9%)'),
  fix_pooled('137', 1, 'pd', c(78, 63), c(5.7, 5.2), c(1.4, 1.1), reason = 'Table 1: pooled AgP and CP (entered PD SD averaged)'),
  fix_pooled('137', 1, 'plaque', c(78, 63), c(52.3, 60.5), c(28.5, 25.6), reason = 'Table 1: pooled AgP and CP (entered AgP only)'),
  fix('137', 0, 'bop', 45.7, 23.7, reason = 'Table 1: not entered'),
  fix('137', 0, 'cal', 3, 0.8, reason = 'Table 1: not entered'),
  fix('137', 0, 'plaque', 47.5, 21.5, reason = 'Table 1: not entered'),

  # 2025-246: Table 1 prints Spain stage III-IV smokers as '19 (43)' with n = 30 (see Other comments)
  fix('2025-246', 1, 'smokers', 2 + 5 + 7 + 13, NA, reason = 'Table 1: 13 (43%) with 17 non-smokers, so 27/120'),

  # 1818 (CP), 1897 (GAP): sampled-site PD not entered
  fix('1818', 1, 'pd', 7.1, 1.4, reason = 'Table 2: PD of samples not entered'),
  fix('1897', 1, 'pd', 7.5, 2.9, reason = 'Table 2: PD of samples not entered'),

  # 1411: average of the four deepest pockets, computed from per-subject Table S1
  fix('1411', 0, 'pd', 1.50, 0.50, reason = 'computed from Table S1'),
  fix('1411', 1, 'pd', 6.00, 1.10, reason = 'computed from Table S1'),
  fix('1411', 0, 'cal', 0, 0, reason = 'computed from Table S1'),
  fix('1411', 1, 'cal', 1.92, 1.02, reason = 'computed from Table S1'),

  # 116: periodontitis age is median [IQR] 19.0 [0.0]
  fix('116', 1, 'age', 19, NA, reason = "Table 1: median [IQR] 19.0 [0.0] entered as '19 ± 0'"),

  # 132: '0.13' with '26' read as 13 males (26% of 50); full text not available
  fix('132', 1, 'males', 13, 26, reason = 'inferred: 13/50 = 26% (full text not available)'),

  # 428: 14 volunteers (7 per group); 28 is the number of samples per group
  fix('428', 0, 'size', 7, reason = 'Table 1: n = 7 (28 is number of samples)'),
  fix('428', 1, 'size', 7, reason = 'Table 1: n = 7 (28 is number of samples)'),

  # 534: healthy PD of 11.1 mm is implausible (full text not available)
  fix('534', 0, 'pd', NA, NA, reason = 'healthy PD 11.1 mm implausible (full text not available)'),

  # 2323: SDs of % sites printed on a proportion scale (Table 2)
  fix('2323', 0, 'bop', 0, 0, reason = 'Table 2'),
  fix('2323', 1, 'bop', 89, 0.3 * 100, reason = 'Table 2: SD on proportion scale'),
  fix('2323', 0, 'plaque', 11, 0.3 * 100, reason = 'Table 2: SD on proportion scale'),
  fix('2323', 1, 'plaque', 95, 0.2 * 100, reason = 'Table 2: SD on proportion scale'),

  # 2025-144, 2025-234: healthy BOP '0.05' next to periodontitis in % (full text not available)
  fix('2025-144', 0, 'bop', NA, NA, reason = 'healthy BOP 0.05 ambiguous: fraction or % (full text not available)'),
  fix('2025-234', 0, 'bop', NA, NA, reason = 'healthy BOP 0.05 ambiguous: fraction or % (full text not available)')
) |>
  mutate(source = 'paper')

sem_sds <- bind_rows(
  sem_sds,
  sem('2025-Embase-55', c('pd', 'cal'), 'Table 1: mean ± S.E.M. (age not converted: implausible as SEM)', groups = 1)
)

corrections_raw <- bind_rows(
  corrections_raw,

  # 1667: periodontitis CAL '4.41±1.27' set to NA
  fix('1667', 1, 'cal', 4.41, 1.27, reason = 'CAL set to NA'),

  # 970: CP (n = 30) and AgP (n = 26); healthy full-mouth PD only in 'PD OG'
  fix_pooled('970', 1, 'age', c(30, 26), c(45.7, 33.9), c(10.1, 7.7)),
  fix_pooled('970', 1, 'pd', c(30, 26), c(4.4, 5.9), c(1.3, 2.2)),
  fix_pooled('970', 1, 'cal', c(30, 26), c(5.1, 6.5), c(1.5, 2.1)),
  fix_pooled('970', 1, 'plaque', c(30, 26), c(54, 39), c(19, 17)),
  fix('970', 0, 'pd', 1.5, 0.3, reason = "full-mouth PD in 'PD OG' set to NA"),

  # 1085: GAgP (n = 10) and LAgP (n = 9); overall age given (SD 0.84 implausible as pooled SD)
  fix('1085', 1, 'age', 23.74, NA, reason = 'overall AgP age set to NA'),
  fix_pooled('1085', 1, 'pd', c(10, 9), c(3.7, 3.0), c(0.6, 0.8)),
  fix_pooled('1085', 1, 'cal', c(10, 9), c(4.0, 2.9), c(2.2, 2.3)),
  fix_pooled('1085', 1, 'plaque', c(10, 9), c(4.1, 4.0), c(1.3, 2.0)),

  # 131: CAL of non-smokers (n = 15) and smokers (n = 15)
  fix_pooled('131', 1, 'cal', c(15, 15), c(7.01, 6.23), c(1.17, 1.05)),

  # 1907: BOP and plaque reported as fractions of sites (Table 2)
  fix('1907', 0, 'bop', 12, NA, reason = 'fraction of sites (0.12)'),
  fix('1907', 0, 'plaque', 24, NA, reason = 'fraction of sites (0.24)'),
  fix('1907', 1, 'plaque', 61, NA, reason = 'fraction of sites (0.61)'),

  # 452: BOP and plaque are numbers (%) of subjects with BOP/plaque, not % sites
  fix('452', 0, 'bop', NA, NA, reason = 'number of subjects, not % sites'),
  fix('452', 0, 'plaque', NA, NA, reason = 'number of subjects, not % sites'),
  fix('452', 1, 'plaque', NA, NA, reason = 'number of subjects, not % sites'),

  # 450: healthy sites were defined by no BOP (sites within the same subjects)
  fix('450', 0, 'bop', NA, NA, reason = 'site selection criterion, not a measurement'),

  # 2025-PubMed-130: RA and control subgroups (n = 6 + 6 healthy, 12 + 12 periodontitis)
  fix_pooled('2025-PubMed-130', 0, 'age', c(6, 6), c(54.50, 53.33), c(11.66, 11.04)),
  fix_pooled('2025-PubMed-130', 1, 'age', c(12, 12), c(59.00, 58.50), c(7.80, 7.86)),
  fix('2025-PubMed-130', 0, 'bop', NA, NA, reason = "'Absent (0%)' is a selection criterion")
)

corrections_paper <- bind_rows(
  corrections_paper,
  bind_rows(
    # 1746: PD 38.5/CAL 39.5 (healthy 0.1) cannot be mm (local PDF is a different paper)
    fix('1746', 0, 'pd', NA, NA, reason = 'PD 0.1 (health) and 38.5 (periodontitis) are not mm; not verifiable'),
    fix('1746', 1, 'pd', NA, NA, reason = 'PD 0.1 (health) and 38.5 (periodontitis) are not mm; not verifiable'),
    fix('1746', 0, 'cal', NA, NA, reason = 'CAL 0.1 (health) and 39.5 (periodontitis) are not mm; not verifiable'),
    fix('1746', 1, 'cal', NA, NA, reason = 'CAL 0.1 (health) and 39.5 (periodontitis) are not mm; not verifiable'),

    # 131: healthy PD and CAL printed as 0.00 ± 0.000 (implausible); plaque (Turesky index)
    fix('131', 0, 'pd', NA, NA, reason = 'Table: healthy PD 0.00 ± 0.000 implausible'),
    fix('131', 0, 'cal', NA, NA, reason = 'Table: healthy CAL 0.00 ± 0.000 implausible'),
    fix('131', 0, 'plaque', 1.69, 0.725, reason = 'Table: healthy plaque index not entered'),
    fix_pooled('131', 1, 'plaque', c(15, 15), c(2.61, 2.91), c(0.549, 0.594), reason = 'Table: pooled non-smokers and smokers (entered non-smokers only)'),
    # 123: plaque '1.69' is not reported in the paper (same value as healthy plaque of 131)
    fix('123', 0, 'plaque', NA, NA, reason = 'plaque not reported in Table S1 (1.69 matches 131)'),

    # 1667: Table 2 has GAgP n = 30 and health n = 30 (16S clone libraries: 10 per group)
    fix('1667', 1, 'size', 30, reason = 'Table 2: GAgP n = 30 (entered 10; 14 males > 10)'),

    # 2139: '73' is % males; smoking history excluded; 16S clone library sequencing
    fix('2139', 1, 'males', NA, 73, reason = 'Text: periodontitis group 73% male'),
    fix('2139', 1, 'smokers', 0, 0, reason = 'history of smoking excluded'),

    # 304: 38 of 75 is 50.7%
    fix('304', 1, 'males', 38, 100 * 38 / 75, reason = 'Table 1: 38 males of 75 (entered 56%)'),

    # 970: AgP males 7 (26.92% of 26), not 11
    fix('970', 1, 'males', 13 + 7, 100 * 20 / 56, reason = 'Table 1: AgP males 7 (26.92%), entered 11'),

    # 1496: healthy males 8 of 21 (printed 48% is a typo for 38%)
    fix('1496', 0, 'males', 8, 100 * 8 / 21, reason = 'Table 1: 8/21 males (printed 48% sums to 110%)'),

    # 1986: healthy age entered as smokers; smokers per group not reported
    fix('1986', 0, 'smokers', NA, NA, reason = 'Table 1: 37.65 ± 10.88 is healthy age; smokers by group not reported'),
    fix('1986', 0, 'age', 37.65, 10.88, reason = 'Table 1: healthy age'),
    fix('1986', 0, 'plaque', 15.77, 12.56, reason = 'Table 1: approximal plaque index (% sites) not entered'),
    fix('1986', 1, 'plaque', 41.35, 30.58, reason = 'Table 1: approximal plaque index (% sites) not entered'),

    # 2095: calculus index entered as plaque; Silness-Löe plaque index
    fix('2095', 0, 'plaque', 0.18, 0.16, reason = 'Table 1: calculus index was entered as plaque'),
    fix('2095', 1, 'plaque', 0.66, 0.66, reason = 'Table 1: calculus index was entered as plaque'),

    # bleeding indices, not % of sites with BOP
    fix('549', 0, 'bop', NA, NA, reason = 'bleeding index (BI), not % sites (abstract)'),
    fix('2025-Embase-55', 0, 'bop', NA, NA, reason = 'Table 1: bleeding index (BI), not % sites'),
    fix('2025-Embase-55', 1, 'bop', NA, NA, reason = 'Table 1: bleeding index (BI), not % sites'),

    # 448: under 60 (n = 11) and over 60 (n = 7) years (subgroup sizes from abstract)
    fix_pooled('448', 1, 'age', c(11, 7), c(48.2, 68.1), c(7.8, 8.0), reason = 'pooled subgroups (sizes from abstract; order inferred)'),

    # 2025-PubMed-14: BOP read as fraction of sites (full text not available)
    fix('2025-PubMed-14', 0, 'bop', 4, 3, reason = '0.04 (0.03) read as fraction of sites (not verifiable)'),
    fix_pooled('2025-PubMed-14', 1, 'bop', c(16, 24, 32, 9), c(31, 39, 60, 53), c(26, 31, 41, 50),
               reason = 'pooled stages, read as fractions of sites (not verifiable)')
  ) |>
    mutate(source = 'paper')
)

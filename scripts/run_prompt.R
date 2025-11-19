# for checking if the previous prompt results are saved and valid
check_prev_prompt <- function(messy_data, check_name, prompt_fixes) {
  prompt_res <- prompt_fixes[[check_name]]
  if (is.null(prompt_res)) return(FALSE)
  if (nrow(messy_data) != nrow(prompt_res)) return(FALSE)
  return(TRUE)
}

# helper utility for other run_..._prompt functions
run_prompt <- function(chat, prompt, type_clean_df) {
  
  instruct_json <- "
  You're an expert data wrangler who also loves JSON. I am going to give you a JSON 
  with messy values and your job is to return the requested clean JSON. Just return the
  JSON and no other commentary.
  "
  
  chat$set_system_prompt(instruct_json)
  
  
  clean_results_df <- chat$chat_structured(
    prompt, 
    type = type_clean_df
  )
  
  return(clean_results_df)
}

# need to define enums for some clean cols
source('scripts/controlled_vocab.R')

# all column types for prompts
get_type_clean_cols <- function(clean_cols) {
  clean_cols <- c('row', clean_cols)
  
  type_clean_cols <- ellmer::type_object(
    row = ellmer::type_string(
      description = "The original row number in the messy data."
    ),
    clean_num = ellmer::type_string(
      description = "The number of individuals in the group.",
      required = FALSE
    ),
    clean_percent = ellmer::type_string(
      description = "The percentage of individuals in the group.",
      required = FALSE
    ),
    clean_sd = ellmer::type_string(
      description = "The standard deviation in the group.",
      required = FALSE
    ),
    seq_type = ellmer::type_enum(
      values = c(controlled_vocab$sequencing_type, 'NA'),
      description = "The sequencing type."
    ),
    `16s_regions` = ellmer::type_string(
      description = "The 16S variable regions when seq_type is 16S."
    ),
    seq_plat = ellmer::type_enum(
      values = c(controlled_vocab$sequencing_platform, 'NA'),
      description = "The sequencing platform."
    )
  )
  
  defined_cols <- names(type_clean_cols@properties)
  
  stopifnot(all(clean_cols %in% defined_cols))
  
  # remove cols that not requested
  remove_cols <- setdiff(defined_cols, clean_cols)
  for (col in remove_cols) 
    type_clean_cols@properties[[col]] <- NULL
  
  return(type_clean_cols)
}


get_prompt_specifics <- function(prompt_name) {
  
  # age overall ----
  age_overall = list(
    dirty_cols ='messy_num',
    clean_cols = c('clean_num', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c('average 30 (range 19-40)', '30', NA),
      c('18 and 70', NA, NA),
      c('26.2±4.1 (20–29)24.5±5.1 (20–28)', NA, NA),
      c('≥18 years', NA, NA),
      c('26 ± 3     25 ± 4', NA, NA),
      c('43.8 years', '43.8', NA),
      c('	over 18 years', NA, NA),
      c('38.33 ± 13.19', '38.33', '13.19'),
      c('36.2±21.3 (range: 18–70)', '36.2', '21.3'),
      c('mean (±SD): 47.2 ± 8.3*, range: 33-63*', '47.2', '8.3'),
      c('37,4 ± 12,9', '37.4', '12.9'),
      c('mean*:54.53(±11.93), range*: 32-70; individual values: 63, 41, 54, 70, 70, 46, 49, 62, 55, 32, 65, 53, 41, 42, 44, 70, 46, 67, 66', '54.53', '11.93'),
      c('not determined for all', NA, NA),
      c('ND', NA, NA),
      c('25-63 (range)', NA, NA)
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - if values for multiple groups are reported and no overall mean or sd (e.g. '26 ± 3     25 ± 4'),
      set both 'clean_num' and 'clean_sd' to 'NA'
    - if a range is given and no mean or standard deviation (e.g. '≥18 years' or '25-63 (range)'),
      set both 'clean_num' and 'clean_sd' to 'NA'
    "
  )
  
  # age health ----
  age_health = list(
    dirty_cols =c('messy_num', 'messy_sd'),
    clean_cols = c('clean_num', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c(NA, NA, NA, NA),
      c('45.8 ± 10.5', NA, '45.8', '10.5'),
      c('24.5±5.1 (20–28)', NA, '24.5', '5.1'),
      c('30.6 ± 1.5', NA, '30.6', '1.5'),
      c('42.8 years', NA, '42.8', NA),
      c('27 1.5', NA, '27', '1.5'),
      c('34,3 ± 12', NA, '34.3', '12'),
      c('25.01', '5.96', '25.01', '5.96'),
      c('42.8', '11.0', '42.8', '11.0'),
      c('45.1 ± 5.9 (probably mean and SD)', NA, '45.1', '5.9'),
      c('29.9 ± 9.7 (27.0) (Mean ± SD (Median))', NA, '29.9', '9.7'),
      c('45.9 ± 9.9 (age range: 35–59)', NA, '45.9', '9.9'),
      c('age range 13–20 yr', NA, NA, NA),
      c('23.33±2.52* (mean±SD); 23, 21, 26 (individual values)', NA, '23.33', '2.52'),
      c('33.4 (range: 18-60)', NA, '33.4', NA),
      c('≥20', NA, NA, NA),
      c('46 (mean)', NA, '46', NA)
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - if a range is given and no mean or standard deviation (e.g. '≥20' or 'age range 13–20 yr'),
      set both 'clean_num' and 'clean_sd' to 'NA'
    "
  )
  
  # age perio ----
  age_perio = list(
    dirty_cols =c('messy_num', 'messy_sd'),
    clean_cols = c('clean_num', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c('not reported', NA, NA, NA),
      c('37.5± 11.1', NA, '37.5', '11.1'),
      c('26.2±4.1 (20–29)', NA, '26.2', '4.1'),
      c('≥18 years', NA, NA, NA),
      c('44.7 years', NA, '44.7', NA),
      c('52.44', '11.37', '52.44', '11.37'),
      c('46.5 ± 9.0 (47.0) (Mean ± SD (Median))', NA, '46.5', '9.0'),
      c('15.2 ± 2.8 [localised aggressive periodontitis], 25.2 ± 3.2 [generalised aggressive periodontitis], 42.0 ± 6.2 [chronic periodontitis]', NA, NA, NA),
      c('GAgP: 26.3 ± 3.5, GChP: 42.0 ± 5.7', NA, NA, NA),
      c('49.1', '8.6', '49.1', '8.6'),
      c('range: 20-70', NA, NA, NA),
      c('17.8 years (range 15–23)', NA, '17.8', NA)
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - if a range is given and no mean or standard deviation (e.g. '≥18 years' or 'range: 20-70'),
      set both 'clean_num' and 'clean_sd' to 'NA'
    - if multiple subgroups are given (e.g. 'GAgP: 26.3 ± 3.5, GChP: 42.0 ± 5.7'),
      set both 'clean_num' and 'clean_sd' to NA
    "
  )
  
  # sum group size -----
  sum_group_size = list(
    dirty_cols = 'messy_num',
    clean_cols = 'clean_num',
    eval_res = TRUE,
    data_example = list(
      c("CP: 30, AgP: 26", "30+26"),
      c("ModP: 12, SevP: 13", "12+13"),
      c("87", "87"),
      c("15 [localised aggressive periodontitis], 25 [generalised aggressive periodontitis], 30 [chronic periodontitis]", "15+25+30"),
      c("60.67", "60+67"),
      c("ND", "NA"),
      c("NA", "NA")
    ),
    prompt_notes = "
    Note that:
    - sum subgroups if specified (e.g. 'CP: 30, AgP: 26' should be '30+26')
    - do not attempt to evaluate sums (e.g. leave above as '30+26', NOT '56')
    - set 'ND' or 'NA' or similar to 'NA'
    "
  )
  
  # diagnostic method -----
  diagnostic_method = list(
    dirty_cols ='method',
    clean_cols = c('seq_type', '16s_regions', 'seq_plat'),
    eval_res = FALSE,
    data_example = list(
      c('Anaerobic incubation on selective plates and morphologic and biochemical properties', NA, NA, NA),
      c('qPCR', 'PCR', NA, 'RT-qPCR'),
      c('Checkerboard DNA–DNA hybridization', NA, NA, 'DNA-DNA Hybridization'),
      c('PCR', 'PCR', NA, 'Non-quantitative PCR'),
      c('Targetted PCR amplification using species-specific primers.', 'PCR', NA, 'Non-quantitative PCR'),
      c('Culture', NA, NA, NA),
      c('16S rDNA high-throughput sequencing', '16S', NA, NA),
      c('16S rRNA gene sequencing (V4 region)', '16S', '4', NA),
      c('454 FLX Titanium pyrosequencing (V1-V3)', '16S', '123', 'Roche454'),
      c('MALDI-TOF-MS', NA, NA, 'Mass spectrometry'),
      c('HOMIM', NA, NA, NA),
      c('Metatranscriptomic Illumina sequencing', 'WMS', NA, 'Illumina'),
      c('Illumina sequencing (V1-V2 and V5-V6 regions) of the 16S rRNA gene', '16S', '1256', 'Illumina')
    ),
    prompt_notes = "
      Note that:
      - 'row' should be preserved exactly, going from 1 to {nrow(messy_data)}
      - For '16s_regions' the possible values are 1 to 9 and should be pasted together 
        in increasing order (e.g. 'V4-V5' should be '45') 
      "
  )
  
  
  # males overall ----
  males_overall = list(
    dirty_cols ='messy_num',
    clean_cols = c('clean_num', 'clean_percent'),
    eval_res = TRUE,
    data_example = list(
      c("0, 0%", "0", '0'),
      c('89(35.5%)', '89', '35.5'),
      c('27 ( 45%)', '27', '45'),
      c(NA, NA, NA),
      c('0.45', NA, '45'),
      c('0', '0', '0'),
      c('53, 33%', '53', '33'),
      c('n=133', '133', NA)
    ),
    prompt_notes = "
    Note that:
    - if 'clean_num' or 'clean_percent' will be set to '0', set both to '0'
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - fractions (e.g. 0.371) should be treated as percentages to go in 'clean_percent' (e.g. '37.1')
    - 'clean_num' should always be an integer
    "
  )
  
  # males health ----
  males_health = list(
    dirty_cols =c('messy_num', 'messy_percent'),
    clean_cols = c('clean_num', 'clean_percent'),
    eval_res = TRUE,
    data_example = list(
      c('46 (31,5%)', NA, '46', '31.5'),
      c('n=13', NA, '13', NA),
      c('0.37', NA, NA, '37'),
      c('0.45', NA, NA, '45'),
      c('0% (exclusion criteria)', NA, '0', '0'),
      c('ND', NA, NA, NA),
      c('17', '32.1%*', '17', '32.1'),
      c('7', '50', '7', '50'),
      c('41 (45.05%)', NA, '41', '45.05'),
      c('26', '0.52', '26', '52'),
      c('exclusion criteria', NA, '0', '0'),
      c('45.9% of all males were in Controls group', NA, NA, NA),
      c('health: 41*, 29.5% / gingivitis: 32*, 31.1%', NA, '41+32', '(41+32)/((41/.295)+(32/.311))*100'),
      c('47,62%', NA, '47', '62')
    ),
    prompt_notes = "
    Note that:
    - if 'clean_num' or 'clean_percent' will be set to '0', set both to '0'
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - fractions (e.g. 0.371) should be treated as percentages to go in 'clean_percent' (e.g. '37.1')
    - 'clean_num' should always be an integer
    - if values for multiple subgroups are provided (e.g. 'health: 41*, 29.5% / gingivitis: 32*, 31.1%'), 
      'clean_num' should sum of individuals in subgroups (e.g. '41+32') and 'clean_percent' should be
      sum of individuals in subgroups divided by sum of calculated total size of subgroups 
      (e.g. '(41+32)/((41/.295)+(32/.311))*100')
    "
  )
  
  # males perio ----
  males_perio = list(
    dirty_cols =c('messy_num', 'messy_percent'),
    clean_cols = c('clean_num', 'clean_percent'),
    eval_res = TRUE,
    data_example = list(
      c('0', '0', '0', '0'),
      c('0.34200000000000003', NA, '0', '34.2'),
      c('6', '34', '6', '34'),
      c('59.2', NA, NA, '59.2'),
      c('0.52', NA, NA, '52'),
      c('37 (48.68%)', NA, '37', '48.68'),
      c('ND', NA, NA, NA),
      c('n=14', NA, '14', NA),
      c('4 (66.67%)*', NA, '4', '66.67'),
      c('41,18%', NA, '41', '18'),
      c('343', '54', '343', '54'),
      c('0.13', '26', NA, '26'),
      c('4 (21.05%), (GAgP: 2 (22%)*, LAgP: 2 (23%)*)', NA, '4', '21.05'),
      c('54.1% of all males were in Cases group', NA, NA, NA),
      c('58 (34.1%) [AgP and CP]*, 23 (30.7%) [AgP], 35 (36.8%) [CP]', NA, '58', '34.1'),
      c('24 (38.7%)36 (43.9%', NA, '24+36', '(24+36)/((24/.387)+(36/.439))*100'),
      c('GAgP: 14 (46.67%), GChP: 13 (43.33%)', NA, '14+13', '(14+13)/((14.4667/)+(13/.4333))*100'),
      c('6 (40.0%)* [localised aggressive periodontitis], 10 (40.0%)* [generalised aggressive periodontitis], 8 (26.7%)* [chronic periodontitis]', NA, '6+10+8', '(6+10+8)/((6/.40)+(10/.40)+(8/.267))*100')
    ),
    prompt_notes = "
    Note that:
    - if 'clean_num' or 'clean_percent' will be set to '0', set both to '0'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - fractions (e.g. 0.371) should be treated as percentages to go in 'clean_percent' (e.g. '37.1')
    - 'clean_num' should always be an integer
    - if values for multiple subgroups are provided (e.g. 'GAgP: 14 (46.67%), GChP: 13 (43.33%)'), 
      'clean_num' should sum of individuals in subgroups (e.g. '14+13') and 'clean_percent' should be
      sum of individuals in subgroups divided by sum of calculated total size of subgroups 
      (e.g. '(14+13)/((14.4667/)+(13/.4333))*100')
    - if values for multiple subgroups are provided in addition to overall values (e.g. 
      '58 (34.1%) [AgP and CP]*, 23 (30.7%) [AgP], 35 (36.8%) [CP]'), 'clean_num' should be the overall
      group size (e.g. '58') and 'clean_percent' should be the overall group percent (e.g. '34.1')
    "
  )
  
  # smokers overall ----
  smokers_overall = list(
    dirty_cols ='messy_num',
    clean_cols = c('clean_num', 'clean_percent'),
    eval_res = TRUE,
    data_example = list(
      c('ND', NA, NA),
      c('2 (4.9%)*', '2', '4.9'),
      c('59 (27.0%)*', '59', '27.0'),
      c('37.7 %', NA, '37.7'),
      c('50% H 40% P', NA, NA),
      c('50, 32.47%', '50', '32.47'),
      c('n=13', '13', NA),
      c('Excluded', '0', '0'),
      c('0', '0', '0'),
      c('0.34100000000000003', NA, '34.1'),
      c('0 , 38 %, 80%', NA, NA),
      c('3.5000000000000003E-2', NA, '3.5'),
      c('10', '10', NA),
      c('30', '30', NA),
      c('12,5  25', NA, NA)
    ),
    prompt_notes = "
    Note that:
    - if 'clean_num' or 'clean_percent' will be set to '0', set both to '0'
    - if 'messy_num' indicates an exclusion criteria ('Excluded' or similar language),
      set both 'clean_num' and 'clean_percent' to '0'
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - fractions (e.g. 0.371) should be treated as percentages to go in 'clean_percent' (e.g. '37.1')
    - 'clean_num' should always be an integer
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    "
  )
  
  # smokers health ----
  smokers_health = list(
    dirty_cols =c('messy_num', 'messy_percent'),
    clean_cols = c('clean_num', 'clean_percent'),
    eval_res = TRUE,
    data_example = list(
     c('7', '29.2', '7', '29.2'),
     c('18 (12.3%)', NA, '18', '12.3'),
     c('ND', NA, NA, NA),
     c('0.11600000000000001', NA, NA, '11.6'),
     c('exclusion criteria', NA, '0', '0'),
     c('0', NA, '0', '0'),
     c('Excluded', NA, '0', '0'),
     c('4 (18.2%)', NA, '4', '18.2'),
     c('n=4', NA, '4', NA),
     c('9.1999999999999998E-2', NA, NA, '9.20'),
     c('18', '22.2', '18', '22.2'),
     c('37.65±10.88', NA, NA, '37.65'),
     c('0.5', NA, NA, '50'),
     c('not reported', NA, NA, NA),
     c('4 (31%) heatlh / 3 (17%) gingivitis', NA, '4+3', '(4+3)/((4/.31)+(3/.17))*100'),
     c('32.4% of all cigarette smokers were in Controls group, 22.2% of all water pipe smokers were in Controls group, 50.0% of all qat chewers were in Controls group', NA, NA, NA)
    ),
    prompt_notes = "
    Note that:
    - if 'clean_num' or 'clean_percent' will be set to '0', set both to '0'
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    - fractions (e.g. 0.371) should be treated as percentages to go in 'clean_percent' (e.g. '37.1')
    - 'clean_num' should always be an integer
    - if values for multiple subgroups are provided (e.g. '4 (31%) heatlh / 3 (17%) gingivitis'), 
      'clean_num' should sum of individuals in subgroups (e.g. '4+3') and 'clean_percent' should be
      sum of individuals in subgroups divided by sum of calculated total size of subgroups 
      (e.g. '(4+3)/((4/.31)+(3/.17))*100')
    "
  )
  
  # smokers perio ----
  smokers_perio = list(
    dirty_cols =c('messy_num', 'messy_percent'),
    clean_cols = c('clean_num', 'clean_percent'),
    eval_res = TRUE,
    data_example = list(
      c('34', '50.00', '34', '50.00'),
      c('26 (24.8%)', NA, '26', '24.8'),
      c('44.7, 4,1 % *', NA, NA, NA),
      c('0', NA, '0', '0'),
      c('178', '28.00', '178', '28.00'),
      c('PCnoS group: 0, PCS group: 6 (100%)', NA, '6+0', NA),
      c('43.1', NA, NA, '43.1'),
      c('n=29', NA, '29', NA),
      c('12 (25)', NA, '12', '25'),
      c('0.2', NA, NA, '20'),
      c('excluded', NA, '0', '0'),
      c('0', '0.00', '0', '0'),
      c('ChP: 10 (27.5%)*, AgP: 1 (2.9%)*', NA, '10+1', '(10+1)/((10/.275)+(1/.029))*100'),
      c('active: 10 (83%)* / recession: 1 (20%)', NA, '10+1', '(10+1)/((10/.83)+(1/.20))*100'),
      c('Current 17 (38.6) Former 4 (9.1)', NA, '17+4', '(17+4)/((17/.386)+(4/.091))*100'),
      c('6 (46.15%)*', NA, '6', '46.15'),
      c('2', '7.7', '2', '7.7'),
      c('Ap 27, 34.6% / Cp 14, 22.2%', NA, '27+14', '(27+14)/((27/.346)+(14/.222))*100'),
      c('n=29', NA, '29', NA),
      c('0.40500000000000003', NA, NA, '40.5'),
      c('ND', NA, NA, NA),
      c('38-80%', NA, NA, NA),
      c('49 (28.8%) [AgP and CP]*, 23 (30.7%) [AgP], 26 (27.4%) [CP]', NA, '49', '28.8'),
      c('67.6% of all cigarette smokers were in Cases group, 77.8% of all cigarette smokers were in Cases group, 50.0% of all cigarette smokers were in Cases group', NA, NA, NA)
      ),
    prompt_notes = "
    Note that:
    - if 'clean_num' or 'clean_percent' will be set to '0', set both to '0'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - fractions (e.g. 0.371) should be treated as percentages to go in 'clean_percent' (e.g. '37.1')
    - 'clean_num' should always be an integer
    - if values for multiple subgroups are provided (e.g. 'Ap 27, 34.6% / Cp 14, 22.2%'), 
      'clean_num' should sum of individuals in subgroups (e.g. '27+14') and 'clean_percent' should be
      sum of individuals in subgroups divided by sum of calculated total size of subgroups 
      (e.g. '(27+14)/((27/.346)+(14/.222))*100')
    - if values for multiple subgroups are provided in addition to overall values (e.g. 
      '49 (28.8%) [AgP and CP]*, 23 (30.7%) [AgP], 26 (27.4%) [CP]'), 'clean_num' should be the overall
      group size (e.g. '49') and 'clean_percent' should be the overall group percent (e.g. '28.8')
    "
  )
  
  # bop health ----
  bop_health = list(
    dirty_cols =c('messy_percent', 'messy_sd'),
    clean_cols = c('clean_percent', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c(NA, NA, NA, NA),
      c('10.31±9.25', NA, '10.31', '9.25'),
      c('4.0 ±  0.5 %', NA, '4.0', '0.5'),
      c('8 ± 9', NA, '8', '9'),
      c('ND', NA, NA, NA),
      c('1,9 ± 4,1 %', NA, '1.9', '4.1'),
      c('0.0', NA, '0', NA),
      c('6', '5.00', '6', '5.00'),
      c('0.56±0.18 (GI)', NA, NA, NA),
      c('<30% [FMBS}', NA, NA, NA),
      c('range: 11.90-48.20* (% of total, full mouth BOP)', NA, NA, NA),
      c('not reported', NA, NA, NA),
      c('2.1 ± 1.1 and 3.2 ± 1.8 (% sites with gingival bleeding (0/1) and BOP (0/1), measured at six sites per tooth (MB, B, DB, DL, L and ML) in all teeth, excl 3rd molars)', NA, '3.2', '1.8'),
      c('4.5 ± 2.7 (%)', NA, '4.5', '2.7'),
      c('11.40 ± 1.22 (gingival plaque index)', NA, NA, NA),
      c('5.8 ± 0.5 (6 sites per tooth of all teeth, excl 3rd molars) - BOP(% of sites)', NA, '5.8', '0.5'),
      c('1±1% BOP, full-mouth, 6 ± 3% gingival bleeding (GB) full-mouth', NA, '1', '1')
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    - if multiple measures are reported (e.g. '1±1% BOP, full-mouth, 6 ± 3% gingival bleeding (GB) full-mouth' 
      report both bleeding on probing (BOP) and gingival bleeding (GB)) extract only the bleeding on probing
      (e.g. 'clean_percent': '1' and 'clean_sd': '1')
    - if a single measure is reported and there is an indication that it is not related to bleeding on probing (BOP)
      (e.g. '0.56±0.18 (GI)'), set both 'clean_percent' and 'clean_sd' to 'NA'
    - if a range is reported (e.g. <30% [FMBS}' or 'range: 11.90-48.20* (% of total, full mouth BOP)')
      set both 'clean_percent' and 'clean_sd' to NA
    "
  )
  
  # bop perio ----
  bop_perio = list(
    dirty_cols =c('messy_percent', 'messy_sd'),
    clean_cols = c('clean_percent', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c('not reported', NA, NA, NA),
      c('72.30±23.05', NA, '72.30', '23.05'),
      c('40.9 ±  1.6 ; 64.5 ± 3.1 %', NA, NA, NA),
      c('72 ± 28', NA, '72', '28'),
      c('ND', NA, NA, NA),
      c('0.53249999999999997', NA, '53.25', NA),
      c('0.64', NA, '64', NA),
      c('0.97 ± 0.02 [sampled tooth]', '0.02', '97', '2'),
      c('47,2 ± 29', NA, '47.2', '29'),
      c('100.0%', NA, '100', NA),
      c('0.97 ± 0.02 [sampled tooth]', '0.02', '0.97', '0.02'),
      c('35.78 ± 0.53 (%) (gingival plaque index)', NA, NA, NA),
      c('19.3 ± 2.8 (6 sites per tooth of all teeth, excl 3rd molars) - BOP (% of sites) AND 19.5 (2.5) - GI (gingival bleeding (GI)) (6 sites per tooth of all teeth, excl 3rd molars)', NA, '19.3', '2.8'),
      c('68±10% BOP full-mouth, 31 ± 6% full-mouth, gingival bleeding (GB) (%sites) full-mouth', NA, '68', '10'),
      c('27.7±15.3 and 78.9±14.1 (% sites with gingival bleeding (0/1) and BOP (0/1), measured at six sites per tooth (measured at six sites per tooth (MB, B, DB, DL, L and ML) in all teeth, excl 3rd molars)', NA, '78.9', '14.1'),
      c('35.5 ± 4.8 [localised aggressive periodontitis], 68.7 ± 15.8 [generalised aggressive periodontitis], 63.6 ± 20.2 [chronic periodontitis]', NA, NA, NA),
      c('≥30% [FMBS]', NA, NA, NA),
      c('ND', NA, NA, NA),
      c('65.1 % (20.3)', NA, '65.1', '20.3'),
      c('ChP: 51.5 ± 18.0, AgP: 72.3 ± 18.6 (%BOP, full mouth)', NA, NA, NA)
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - 'ND' or 'NA' should be set to 'NA'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    - fractions (e.g. '0.64') should be treated as percents (e.g. 'clean_percent': '64')
    - fractions (e.g. '0.97 ± 0.02 [sampled tooth]') should be treated as percents 
      (e.g. 'clean_percent': '97' and 'clean_sd': '2')
    - if multiple measures are reported (e.g. '68±10% BOP full-mouth, 31 ± 6% full-mouth, gingival bleeding (GB) (%sites) full-mouth' 
      report both bleeding on probing (BOP) and gingival bleeding (GB)) extract only the bleeding on probing
      (e.g. 'clean_percent': '68' and 'clean_sd': '10')
    - if a single measure is reported and there is an indication that it is not related to bleeding on probing (BOP)
      (e.g. '35.78 ± 0.53 (%) (gingival plaque index)'), set both 'clean_percent' and 'clean_sd' to 'NA'
    - if a range is reported (e.g. ≥30% [FMBS]' or 'range: 69.87-100.00*  (% of total, full mouth BOP)')
      set both 'clean_percent' and 'clean_sd' to NA
    - if multiple subgroups are given (e.g. 'ChP: 51.5 ± 18.0, AgP: 72.3 ± 18.6 (%BOP, full mouth)'),
      set both 'clean_percent' and 'clean_sd' to NA
    "
  )
  
  # supp health ----
  supp_health = list(
    dirty_cols =c('messy_percent', 'messy_sd'),
    clean_cols = c('clean_percent', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c(NA, NA, NA, NA),
      c('0', NA, '0', NA),
      c('ND', NA, NA, NA),
      c('0.0', NA, '0.0', NA),
      c('not reported', NA, NA, NA),
      c('0  (6 sites per tooth of all teeth, excl 3rd molars)', NA, '0', NA),
      c('0.0±0.0 (% sites with suppuration (0/1), measured at six sites per tooth  (MB, B, DB, DL, L and ML) in all teeth, excl 3rd molars)', NA, '0.0', '0.0'),
      c('0 (suppuration (SUP), %sites, measured at 6 sites per tooth)', NA, '0', NA),
      c('0 ± 0', NA, '0', '0'),
      c('0 (%)', NA, '0', NA)
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - values like 'ND' or 'NA' or 'not reported' should be set to 'NA'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    "
  )
  
  # supp perio ----
  supp_perio = list(
    dirty_cols =c('messy_percent', 'messy_sd'),
    clean_cols = c('clean_percent', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c('not reported', NA, NA, NA),
      c(NA, NA, NA, NA),
      c('ND', NA, NA, NA),
      c('3.57±3.78', NA, '3.57', '3.78'),
      c('3.09 ± 3.7', NA, '3.09', '3.7'),
      c('10 ± 19', NA, '10', '19'),
      c('0.05', '0.11', '0.05', '0.11'),
      c('1', '2.5', '1', '2.5'),
      c('10', '19.00', '10', '19.00'),
      c('1.1±0.8 (% sites with suppuration (0/1), measured at six sites per tooth (measured at six sites per tooth (MB, B, DB, DL, L and ML) in all teeth, excl 3rd molars)', NA, '1.1', '0.8'),
      c('GAgP: 4.32 ± 3.49, GChP: 3.09 ± 3.7 (% sites with suppuration (0/1), measured at six sites per tooth (measured at six sites per tooth (MB, B, DB, DL, L and ML) in all teeth, excl 3rd molars)', NA, NA, NA),
      c('ChP: 0.4 ± 0.9, AgP: 1.1 ± 1.9 (%SUP, full mouth)', NA, NA, NA)
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - values like 'ND' or 'NA' or 'not reported' should be set to 'NA'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    - if multiple subgroups are given (e.g. 'ChP: 0.4 ± 0.9, AgP: 1.1 ± 1.9 (%SUP, full mouth)'),
      set both 'clean_percent' and 'clean_sd' to NA
    "
  )
  
  
  # pocket depth health ----
  pd_health = list(
    dirty_cols =c('messy_num', 'messy_num2', 'messy_sd'),
    clean_cols = c('clean_num', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c(NA, NA, NA, NA, NA),
      c('0.1', '0.1± 0.4', '0.4', '0.1', '0.4'),
      c('2.2799999999999998', '2.28±0.59 (full mouth)', NA, '2.28', '0.59'),
      c('2.4', '2.4 mm, ranging from 1.0 to 3.0 mm (full mouth)', NA, '2.4', NA),
      c('2', '2.0 ± 0.07 (full mouth)', '7.0000000000000007E-2', '2.0', '0.07'),
      c('2.1', '2.1', '0.21', '2.1', '0.21'),
      c('2.7', '2.7', '0.5', '2.7', '0.5'),
      c('1.7', '1.7 ± 0.6 (2.0) (Mean ± SD (Median); measurements made at 6 sites per tooth (MV, V, DV, ML, L and DL), excluding the 3rd molars)', '0.6', '1.7', '0.6'),
      c(NA, 'ND recommendations (1997)', NA, NA, NA),
      c(NA, 'ND', NA, NA, NA),
      c(NA, 'ND (range: 2-3 mm)', NA, NA, NA),
      c('1.75', '1.75 (0.75) ( sampled sites)', NA, '1.75', '0.75')
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - values like 'ND' or 'NA' or 'not reported' should be set to 'NA'
    - if only a range is given (e.g. 'ND (range: 2-3 mm)'), set both 'clean_num'
      and 'clean_sd' to 'NA'
    "
  )
  
  
  
  # pocket depth perio ----
  pd_perio = list(
    dirty_cols =c('messy_num', 'messy_sd'),
    clean_cols = c('clean_num', 'clean_sd'),
    eval_res = TRUE,
    data_example = list(
      c(NA, NA, NA, NA),
      c('38.5 ± 17.4', NA, '38.5', '17.4'),
      c('4.3600000000000003', '0.880', '4.36', '0.880'),
      c('2.9 ±  0.06 ;  3.9   ± 0.09 mm', NA, NA, NA),
      c('4.5', '1.20', '4.5', '1.20'),
      c('3.5 mm', NA, '3.5', NA),
      c('ND (6 sites per tooth of all teeth, excl 3rd molars)', NA, NA, NA),
      c('ChP: 2.8 ± 0.5, AgP: 4.6 ± 0.7 (full mouth)', NA, NA, NA),
      c('≥5 (PD (mm) in > 30% of sites)', NA, NA, NA),
      c('5.3 (6.7)', NA, '5.3', '6.7'),
      c('range: 5-8 (8,6,6,5,5,6)  (average depth in millimeters of the 4 deepest periodontal pockets)', NA, NA, NA),
      c('5.73±0.17 mm', NA, '5.73', '0.17'),
      c('44473', NA, NA, NA)
    ),
    prompt_notes = "
    Note that:
    - row should be preserved exactly, going from 1 to {nrow(messy_data)}
    - values like 'ND' or 'NA' or 'not reported' should be set to 'NA'
    - if only a range is given (e.g. '≥5 (PD (mm) in > 30% of sites)'), set both 'clean_num'
      and 'clean_sd' to 'NA'
    - if values for multiple subgroups are provided (e.g. 'ChP: 2.8 ± 0.5, AgP: 4.6 ± 0.7 (full mouth)'), 
      'clean_num' and 'clean_sd' should both be 'NA'
    - if any cases are provided exactly as examples of messy data, they should be cleaned exactly as shown
    "
  )
  
  
  
  # put all together ----
  prompt_specifics <- list(
    age_overall = age_overall,
    age_health = age_health,
    age_perio = age_perio,
    sum_group_size = sum_group_size,
    diagnostic_method = diagnostic_method,
    males_overall = males_overall,
    males_health = males_health,
    males_perio = males_perio,
    smokers_overall = smokers_overall,
    smokers_health = smokers_health,
    smokers_perio = smokers_perio,
    bop_health = bop_health,
    bop_perio = bop_perio,
    supp_health = supp_health,
    supp_perio = supp_perio,
    pd_health = pd_health,
    pd_perio = pd_perio
  )
  
  return(prompt_specifics[[prompt_name]])
}



run_generic_prompt <- function(messy_data, prompt_name, model = c("gemini-2.0-flash", "gpt-oss:20b")) {
  model <- match.arg(model)
  
  messy_data <- rownames_to_column(messy_data, 'row')
  
  # create the chat object
  model_functions <- list(
    "gemini-2.0-flash" = ellmer::chat_google_gemini,
    "gpt-oss:20b"      = ellmer::chat_ollama
  )
  
  # Call the relevant function with the desired model argument
  chat <- model_functions[[model]](model = model)
  
  # get prompt specific stuff and destructure
  spec <- get_prompt_specifics(prompt_name)
  
  eval_res <- spec$eval_res
  clean_cols <- spec$clean_cols
  dirty_cols <- spec$dirty_cols
  data_example <- spec$data_example
  prompt_notes <- spec$prompt_notes
  
  # setup messy and clean data examples
  data_example <- do.call(rbind, data_example) |> 
    as_tibble() |> 
    setNames(c(dirty_cols, clean_cols)) |> 
    rownames_to_column('row')
  
  messy_data_example <- data_example |> 
    select(row, all_of(dirty_cols))
  
  clean_data_example <- data_example |> 
    select(row, all_of(clean_cols))
  
  # desired output structure
  type_clean_cols <- get_type_clean_cols(clean_cols)
  type_clean_df <- ellmer::type_array(type_clean_cols)
  
  # the actual prompt
  prompt_template <- paste0(
    "
    Determine {paste(shQuote(clean_cols), collapse=' and ')} from {paste(shQuote(dirty_cols), collapse=' and ')} in this JSON:
    
    {jsonlite::toJSON(messy_data, na='string')}
    ",
    prompt_notes, 
    "
    Below are a few examples of how values should be fixed. 
    Here are some of the original messy values:
    
    {jsonlite::toJSON(messy_data_example, na='string')}
    
    and the corresponding cleaned values:
    
    {jsonlite::toJSON(clean_data_example, na='string')}
    "
  )
  
  prompt <- glue::glue(prompt_template)
  
  res <- run_prompt(chat, prompt, type_clean_df)
  
  stopifnot(all.equal(res$row, messy_data$row))
  
  # check that can evaluate strings
  if (eval_res) try(apply(res, 2, evaluate_col))
  
  res_final <- dplyr::bind_cols(
    dplyr::select(messy_data, -row),
    dplyr::select(res, -row)
  )
  
  return(res_final)
}

evaluate_col <- function(col) {
  sapply(col, function(x) eval(parse(text = x)), simplify = TRUE)
}



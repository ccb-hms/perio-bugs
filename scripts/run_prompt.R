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
  
  # put all together ----
  prompt_specifics <- list(
    sum_group_size = sum_group_size,
    diagnostic_method = diagnostic_method,
    males_overall = males_overall,
    males_health = males_health,
    males_perio = males_perio
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

# used for:
# - Males (n, %) 
# - Smokers (n, %) 
run_num_percent_prompt <- function(messy_data, model = c("gemini-2.0-flash", "gpt-oss:20b")) {
  
  model <- match.arg(model)
  
  messy_data <- rownames_to_column(messy_data, 'row')
  
  # create the chat object
  model_functions <- list(
    "gemini-2.0-flash" = ellmer::chat_google_gemini,
    "gpt-oss:20b"      = ellmer::chat_ollama
  )
  
  # Call the relevant function with the desired model argument
  chat <- model_functions[[model]](model = model)
  
  # few shot prompts ----
  messy_cols <- c('messy_num', 'messy_percent')
  clean_cols <- c('clean_num', 'clean_percent')
  
  # column order is as above
  # one example per line with messy then clean for ease of creating examples
  data_example <- list(
    c('46 (31,5%)', NA, '46', '31.5'),
    c('n=13', NA, '13', NA),
    c('37.65±10.88', NA, NA, '37.65'),
    c('0.37', NA, NA, '37'),
    c('0.15', NA, NA, '15'),
    c('0.6', NA, NA, '60'),
    c('12,5  25', NA, '12+5', '25'),
    c('ND', NA, NA, NA),
    c('17', '32.1%*', '17', '32.1'),
    c('0 , 38 %, 80%', NA, NA, NA),
    c('7', '50', '7', '50'),
    c('1 (10.00%)', NA, '1', '10'),
    c('N=10', NA, '10', NA),
    c('n 72', NA, 72, NA),
    c('8', '53.3', '8', '53.3'),
    c('0% (exclusion criteria)', NA, '0', '0'),
    c('exclusion criteria', NA, '0', '0'),
    c('Excluded', NA, '0', '0'),
    c('0', NA, '0', '0'),
    c('45.9% of all males were in Controls group', NA, NA, NA),
    c('32.4% of all cigarette smokers were in Controls group, 22.2% of all water pipe smokers were in Controls group, 50.0% of all qat chewers were in Controls group', NA, NA, NA),
    c('8', '0.53300000000000003', '8', '53.3'),
    c('26', '0.52', '26', '52'),
    c('4 (21.05%), (GAgP: 2 (22%)*, LAgP: 2 (23%)*)', NA, '4', '21.05'),
    c('NS-Perio: 8 (28.57%) / S-Perio: 16 (57.14%)', NA, '8+16', '(8+16) / ((8/0.2857)+(16/0.5714)) * 100'),
    c('moderate: 19 (37%) / severe: 14 (48%)', NA, '19+14', '(19+14) / ((19/0.37)+(14/0.48)) * 100'),
    c('24 (38.7%)36 (43.9%', NA, '24+36', '(24+36) / ((24/0.387)+(36/0.439)) * 100'),
    c('GAgP: 14 (46.67%), GChP: 13 (43.33%)', NA, '14+13', '(14+13) / ((14/0.4667)+(13/0.4333)) * 100'),
    c('Ap 30, 38.5% / Cp 24, 38.5%', NA, '30+24', '(30+24) / ((30/0.385)+(24/0.385)) * 100')
  )
  
  data_example <- do.call(rbind, data_example) |> 
    as.tibble() |> 
    setNames(c(clean_cols, messy_cols)) |> 
    rownames_to_column('row')
  
  messy_data_example <- data_example |> 
    select(row, all_of(messy_cols))
  
  clean_data_example <- data_example |> 
    select(row, all_of(clean_cols))
  
  # desired output structure ----
  type_clean_cols <- ellmer::type_object(
    row = ellmer::type_string(
      description = "The original row number in the messy data."
    ),
    clean_num = ellmer::type_string(
      description = "The number of individuals in the group",
      required = FALSE
    ),
    clean_percent = ellmer::type_string(
      description = "The percentage of individuals in the group.",
      required = FALSE
    )
  )
  
  type_clean_df <- ellmer::type_array(type_clean_cols)
  
  # the actual prompt ----
  
  prompt <- glue::glue("
  Extract the number and percentage of individuals from this JSON:
  
  {jsonlite::toJSON(messy_data, na='string')}
  
  Note that:
  - 'messy_percent' often has missing values that can be obtained from 'messy_num'
  - if 'clean_num' or 'clean_percent' will be set to '0', set both to '0'
  - if exclusion criteria ('Excluded', 'exclusion criteria' or similar text) is given,
    set both 'clean_num' and 'clean_percent' to '0'
  - row should be preserved exactly, going from 1 to {nrow(messy_data)}
  - 'ND' or 'NA' should be set to 'NA'
  - never include an '=' or '±' sign in 'clean_num' or 'clean_percent'
  - fractions (e.g. 0.37) should be treated as percentages to go in 'clean_percent' (e.g. 37)
  - 'clean_num' should always be an integer whole number
  
  How to rationalize certain types of examples:
  
  1) Subgroups num and percent is given (and no overall num and percent):
  messy_num: 'Ap 30, 38.5% / Cp 24, 42.5%'
  messy_percent: 'NA'
  
  add the subgroup nums to get the total num:
  clean_num: '30+24'
  
  and divide the total num by the calculated group sizes as follows:
  clean_percent: '(30+24) / ((30/0.385) + (24/.425)) * 100'
  
  2) An overall num and percent is given in addition to subgroup nums and percents:
  messy_num: '4 (21.05%), (GAgP: 2 (22%)*, LAgP: 2 (23%)*)'
  messy_percent: 'NA'
  
  So just return the overall num and percent:
  clean_num: '4'
  clean_percent: '21.05'
  
  3)  An overall num and percent is given in addition to subgroup nums and percents:
  messy_num: '58 (34.1%) [AgP and CP]*, 23 (30.7%) [AgP], 35 (36.8%) [CP]'
  messy_percent: 'NA'
  
  So just return the overall num and percent:
  clean_num: '58'
  clean_percent: '34.1'
  
  4) Just the subgroup nums are present (no overall or subgroup percent):
  messy_num: 'N=15 CP N=7 H'
  messy_percent: 'NA'
  
  So just return the calculation for the overall num:
  clean_num: '15+7'
  clean_percent: 'NA'
  
  5) Fractions should be treated as a percent:
  messy_num: '0.37'
  clean_num: 'NA'
  clean_percent: '37'
  
  6) Only group size given:
  messy_num: ' n=13'
  clean_num: '13'
  clean_percent: 'NA'

  Any formulas should follow the exact same patterns as above.
  
  Below are a few examples of how values should be fixed. 
  Here are some of the original messy values:
  
  {jsonlite::toJSON(messy_data_example, na='string')}
  
  and the corresponding cleaned values:
  
  {jsonlite::toJSON(clean_data_example, na='string')}
  
  ")
  
  res <- run_prompt(chat, prompt, type_clean_df)
  
  # check that can evaluate strings
  try(apply(res, 2, evaluate_col))
  
  stopifnot(all.equal(res$row, messy_data$row))
  
  
  res_final <- dplyr::bind_cols(
    dplyr::select(messy_data, -row),
    dplyr::select(res, -row)
  )
  
  return(res_final)
}

evaluate_col <- function(col) {
  sapply(col, function(x) eval(parse(text = x)), simplify = TRUE)
}

# used for:
# - Bleeding on probing
# - Suppuration
run_percent_sd_prompt <- function(messy_data, focus, model = c("gemini-2.0-flash", "gpt-oss:20b")) {
  
  model <- match.arg(model)
  
  messy_data <- rownames_to_column(messy_data, 'row')
  
  # create the chat object
  model_functions <- list(
    "gemini-2.0-flash" = ellmer::chat_google_gemini,
    "gpt-oss:20b"      = ellmer::chat_ollama
  )
  
  # Call the relevant function with the desired model argument
  chat <- model_functions[[model]](model = model)
  
  # few shot prompts ----
  cols <- c('messy_percent', 'messy_sd', 'clean_percent', 'clean_sd')
  
  # column order is as above
  # one example per line with messy then clean for ease of creating examples
  data_example <- list(
    c(NA, NA, NA, NA),
    c('6', '5.0', '6', '5.0'),
    c('10.31±9.25', NA, '10.31', '9.25'),
    c('4.0 ±  0.5 %', NA, '4.0', '0.5'),
    c('8 ± 9', NA, '8', '9'),
    c('ND', NA, NA, NA),
    c('1,9 ± 4,1 %', NA, '1.9', '4.1'),
    c('0.0', NA, '0', '0'),
    c('0.3±0.5', NA, '0.3', '0.5'),
    c('5.8 ± 0.5 (6 sites per tooth of all teeth, excl 3rd molars) - BOP(% of sites)', NA, '5.8', '0.5'),
    c('1±1% BOP, full-mouth, 6 ± 3% gingival bleeding (GB) full-mouth', NA, '1', '1'),
    c('11.40 ± 1.22 (gingival plaque index)', NA, NA, NA),
    c('2.1 ± 1.1 and 3.2 ± 1.8 (% sites with gingival bleeding (0/1) and BOP (0/1), measured at six sites per tooth (MB, B, DB, DL, L and ML) in all teeth, excl 3rd molars)', NA, '3.2', '1.8'),
    c('range: 11.90-48.20* (% of total, full mouth BOP)', NA, NA, NA),
    c('<30% [FMBS', NA, NA, NA),
    c('35.5 ± 4.8 [localised aggressive periodontitis], 68.7 ± 15.8 [generalised aggressive periodontitis], 63.6 ± 20.2 [chronic periodontitis]', NA, NA, NA),
    c('ChP: 0.4 ± 0.9, AgP: 1.1 ± 1.9 (%SUP, full mouth)', NA, NA, NA)
  )
  
  data_example <- do.call(rbind, data_example) |> 
    as.tibble() |> 
    setNames(cols) |> 
    rownames_to_column('row')
  
  messy_data_example <- data_example |> 
    select(row, messy_percent, messy_sd)
  
  clean_data_example <- data_example |> 
    select(row, clean_percent, clean_sd)
  
  # desired output structure ----
  type_clean_cols <- ellmer::type_object(
    row = ellmer::type_string(
      description = "The original row number in the messy data."
    ),
    clean_percent = ellmer::type_string(
      description = "The percentage in the group.",
      required = FALSE
    ),
    clean_sd = ellmer::type_string(
      description = "The sd in the group.",
      required = FALSE
    )
  )
  
  type_clean_df <- ellmer::type_array(type_clean_cols)
  
  # the actual prompt ----
  
  prompt <- glue::glue("
  Extract the {focus} percent and sd from this JSON:
  
  {jsonlite::toJSON(messy_data, na='string')}
  
  Note that:
  - if multiple measures are recorded, only extract {focus}
  - if measure recorded is something other than {focus}, set cleaned values to NA
  - '{cols[2]}' often has missing values that can be obtained from '{cols[1]}'
  - if you see a fraction (e.g. 0.37) treat that as a percentage (37)
  - if you see a range (e.g. 11.90-48.20 or <30%), set cleaned values to NA
  - if there is 0 for percent, both percent and sd are 0
  - if measures for multiple subgroups and not overall, set cleaned values to NA
  - row should be preserved exactly, going from 1 to {nrow(messy_data)}
  
  Below are a few examples of how values should be fixed. 
  Here are some of the original messy values:
  
  {jsonlite::toJSON(messy_data_example, na='string')}
  
  and the corresponding cleaned values:
  
  {jsonlite::toJSON(clean_data_example, na='string')}
  
  ")
  
  res <- run_prompt(chat, prompt, type_clean_df)
  
  # check that can evaluate strings
  try(apply(res, 2, evaluate_col))
  
  stopifnot(all.equal(res$row, messy_data$row))
  
  res_final <- dplyr::bind_cols(
    dplyr::select(messy_data, -row),
    dplyr::select(res, -row)
  )
  
  return(res_final)
}



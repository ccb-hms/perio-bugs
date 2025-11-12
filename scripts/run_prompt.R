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

run_diagnostic_method_prompt <- function(method, 
                                         unique_seq_types,
                                         unique_16s_regions,
                                         unique_seq_plats,
                                         model = c("gemini-2.0-flash", "gpt-oss:20b")) {
  
  model <- match.arg(model)
  
  messy_data <- tibble(method) |> 
    rownames_to_column('row')
  
  # create the chat object
  model_functions <- list(
    "gemini-2.0-flash" = ellmer::chat_google_gemini,
    "gpt-oss:20b"      = ellmer::chat_ollama
  )
  
  # Call the relevant function with the desired model argument
  chat <- model_functions[[model]](model = model)
  
  # few shot prompts ----
  cols <- c('method', 'seq_type', '16s_regions', 'seq_plat')
  
  # column order is as above
  # one example per line with messy then clean for ease of creating examples
  data_example <- list(
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
    c('Metatranscriptomic Illumina sequencing', 'WMS', NA, 'Illumina'),
    c('Illumina sequencing (V1-V2 and V5-V6 regions) of the 16S rRNA gene', '16S', '1256', 'Illumina')
  )
  
  data_example <- do.call(rbind, data_example) |> 
    as.tibble() |> 
    setNames(cols) |> 
    rownames_to_column('row')
  
  messy_data_example <- data_example |> 
    select(row, method)
  
  clean_data_example <- data_example |> 
    select(row, seq_type, `16s_regions`, seq_plat)
  
  # so that we can get NA from enum
  unique_seq_types[is.na(unique_seq_types)] <- 'NA'
  unique_seq_plats[is.na(unique_seq_plats)] <- 'NA'
  
  # desired output structure ----
  type_clean_cols <- ellmer::type_object(
    row = ellmer::type_string(
      description = "The original row number in the messy data."
    ),
    seq_type = ellmer::type_enum(
      values = unique_seq_types,
      description = "The sequencing type."
    ),
    `16s_regions` = ellmer::type_string(
      description = "The 16S variable regions when seq_type is 16S."
    ),
    seq_plat = ellmer::type_enum(
      values = unique_seq_plats,
      description = "The sequencing platform."
    )
  )
  
  type_clean_df <- ellmer::type_array(type_clean_cols)
  
  # the actual prompt ----
  
  prompt <- glue::glue("
  Extract 'seq_type', '16s_regions', and 'seq_plat' from column 'method' in this JSON:
  
  {jsonlite::toJSON(messy_data, na='string')}
  
  Note that:
  - set output to 'NA' when the value cannot be determined or is not applicable
  - For '16s_regions' the possible values are 1 to 9 and should be pasted together 
    in increasing order (e.g. 'V4-V5' should be '45') 

  Below are a few examples of how values should be fixed. 
  Here are some of the original messy values:
  
  {jsonlite::toJSON(messy_data_example, na='string')}
  
  and the corresponding cleaned values:
  
  {jsonlite::toJSON(clean_data_example, na='string')}
  
  ")
  
  res <- run_prompt(chat, prompt, type_clean_df)
  
  stopifnot(all.equal(res$row, messy_data$row))
  
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
  cols <- c('messy_num', 'messy_percent', 'clean_num', 'clean_percent')
  
  # column order is as above
  # one example per line with messy then clean for ease of creating examples
  data_example <- list(
    c('46 (31,5%)', NA, '46', '31.5'),
    c('n=13', NA, '13', NA),
    c('0.37', NA, NA, '37'),
    c('17', '32.1%*', '17', '32.1'),
    c('7', '50', '7', '50'),
    c('1 (10.00%)', NA, '1', '10'),
    c('N=10', NA, '10', NA),
    c('n 72', NA, 72, NA),
    c('8', '53.3', '8', '53.3'),
    c('0% (exclusion criteria)', NA, '0', '0'),
    c('8', '0.53300000000000003', '8', '53.3'),
    c('26', '0.52', '26', '52'),
    c('GAgP: 14 (46.67%), GChP: 13 (43.33%)', NA, '14+13', '(14+13) / ((14/0.4667)+(13/0.4333)) * 100'),
    c('Ap 30, 38.5% / Cp 24, 38.5%', NA, '30+24', '(30+24) / ((30/0.385)+(24/0.385)) * 100')
  )
  
  data_example <- do.call(rbind, data_example) |> 
    as.tibble() |> 
    setNames(cols) |> 
    rownames_to_column('row')
  
  messy_data_example <- data_example |> 
    select(row, messy_num, messy_percent)
  
  clean_data_example <- data_example |> 
    select(row, clean_num, clean_percent)
  
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
  - '{cols[2]}' often has missing values that can be obtained from '{cols[1]}'
  - if you see a fraction (e.g. 0.37) treat that as a percentage (37)
  - if there are 0% or 0 individuals, both percent and num are 0
  - row should be preserved exactly, going from 1 to {nrow(messy_data)}
  
  Below are a few examples of how values should be fixed. 
  Here are some of the original messy values:
  
  {jsonlite::toJSON(messy_data_example, na='string')}
  
  and the corresponding cleaned values:
  
  {jsonlite::toJSON(clean_data_example, na='string')}
  
  ")
  
  res <- run_prompt(chat, prompt, type_clean_df)
  
  stopifnot(all.equal(res$row, messy_data$row))
  
  # evalute strings
  res <- as.data.frame(
    apply(res, 2, function(col) sapply(col, function(x) eval(parse(text = x))))
  )
  
  res_final <- dplyr::bind_cols(
    dplyr::select(messy_data, -row),
    dplyr::select(res, -row)
  )
  
  
  return(res_final)
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
  
  stopifnot(all.equal(res$row, messy_data$row))
  
  # evalute strings
  res <- as.data.frame(
    apply(res, 2, function(col) sapply(col, function(x) eval(parse(text = x))))
  )
  
  res_final <- dplyr::bind_cols(
    dplyr::select(messy_data, -row),
    dplyr::select(res, -row)
  )
  
  return(res_final)
}



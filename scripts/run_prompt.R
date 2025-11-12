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

run_num_percent_prompt <- function(chat, messy_data, model = c("gemini-2.0-flash", "gpt-oss:20b")) {
  
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
  - 'messy_percent' often has missing values that can be obtained from 'messy_num'.
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



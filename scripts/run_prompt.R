# get free API key here: https://aistudio.google.com/
Sys.setenv(GOOGLE_API_KEY = 'YOUR_API_KEY')

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


# define the messy data 
messy_data <- tibble(
  messy_num = df$`Males (n,%) (periodontal health)`,
  messy_percent = df$`Males % (periodontal health)`
) |> rownames_to_column('row')

# messy_data <- messy_data[1:20, ]

# define the desired output structure
type_clean_cols <- type_object(
  clean_num = type_string(
    description = "The number of individuals in the group",
    required = FALSE
  ),
  clean_percent = type_string(
    description = "The percentage of individuals in the group.",
    required = FALSE
  ),
  row = type_string(
    description = "The original row number in the messy data."
  )
)

type_clean_df <- type_array(type_clean_cols)

# few shot prompts ----
messy_data_example <- tibble(
  messy_num = c(
    '46 (31,5%)','n=13', '0.37', '17', '7', '1 (10.00%)', 'N=10', 'n 72', 
    '8','0% (exclusion criteria)','8','26'),
  messy_percent = c(
    NA, NA, NA, '32.1%*', '50',NA, NA, NA, '53.3',NA,'0.53300000000000003','0.52')
) |> rownames_to_column('row')

clean_data_example <- tibble(
  clean_num = c(
    '46', '13', NA, '17', '7', '1', '10', '72', '8','0','8','26'),
  clean_percent = c(
    '31.5', NA, '37', '32.1', '50', '10', NA, NA, '53.3','0','53.3','52')
) |> rownames_to_column('row')


# run the prompt ----
# create the chat object
chat <- chat_google_gemini(model = "gemini-2.0-flash")
# chat <- chat_ollama(model = "gpt-oss:20b")

# define the desired output structure
type_clean_cols <- type_object(
  clean_num = type_string(
    description = "The number of individuals in the group",
    required = FALSE
  ),
  clean_percent = type_string(
    description = "The percentage of individuals in the group.",
    required = FALSE
  ),
  row = type_string(
    description = "The original row number in the messy data."
  )
)

type_clean_df <- type_array(type_clean_cols)

# string for prompt:
prompt <- glue("
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

res_final <- bind_cols(
  select(messy_data, -row),
  select(res, -row)
  )

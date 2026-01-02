# Functions to interact with BugSigDB API

library(httr)
library(jsonlite)

#' Login to BugSigDB API
#'
#' @param username Your BugSigDB username (or username@botname for bot password)
#' @param password Your BugSigDB password (or bot password)
#' @param base_url Base URL for the API (default: "https://bugsigdb.org/w/api.php")
#' @return A list containing cookies for authenticated requests
#' @export
bugsigdb_login <- function(username, password, 
                           base_url = "https://bugsigdb.org/w/api.php") {
  
  # Create a shared handle to maintain cookies across requests
  h <- handle(base_url)
  
  # Get login token (using the shared handle)
  token_response <- POST(
    url = base_url,
    body = list(
      action = "query",
      meta = "tokens",
      type = "login",
      format = "json"
    ),
    encode = "form",
    handle = h
  )
  
  token_content <- content(token_response, as = "parsed")
  login_token <- token_content$query$tokens$logintoken
  
  cat("Login token obtained:", login_token, "\n")
  
  # Perform login (using the same shared handle to maintain cookies)
  login_response <- POST(
    url = base_url,
    body = list(
      action = "login",
      lgname = username,
      lgpassword = password,
      lgtoken = login_token,
      format = "json"
    ),
    encode = "form",
    handle = h
  )
  
  login_content <- content(login_response, as = "parsed")
  
  # Check result
  if (login_content$login$result != "Success") {
    stop("Login failed: ", login_content$login$result, 
         "\nReason: ", login_content$login$reason)
  }
  
  message("Successfully logged in as ", username)
  
  # Extract and return cookies
  all_cookies <- cookies(login_response)
  
  # Print cookie info for debugging
  cat("\nCookies obtained:\n")
  print(all_cookies[, c("name", "domain")])
  
  return(all_cookies)
}


#' Create a new study in BugSigDB
#'
#' @param cookies Cookie data from bugsigdb_login()
#' @param pmid PubMed ID
#' @param doi DOI of the study
#' @param uri URL of the study
#' @param study_design Character vector of study designs (e.g., c("case-control", "cohort"))
#' @param state "Complete" or "Incomplete" (default: "Incomplete")
#' @param edit_summary Summary for edit history
#' @param base_url Base URL for the API
#' @return A list with study information including the study target
#' @export
bugsigdb_create_study <- function(cookies, 
                                  pmid = NULL,
                                  doi = NULL,
                                  uri = NULL,
                                  study_design = NULL,
                                  state = "Incomplete",
                                  edit_summary = "Adding study via API",
                                  base_url = "https://bugsigdb.org/w/api.php") {
  
  # Build query parameters
  query_params <- list(
    editRevId = "0",
    wpSummary = edit_summary,
    form = "Study"
  )
  
  if (!is.null(study_design)) {
    for (design in study_design) {
      query_params[[length(query_params) + 1]] <- design
      names(query_params)[length(query_params)] <- "Study[study design][]"
    }
    query_params[["Study[study design][is_list]"]] <- "1"
  }
  
  query_params[["Study[Mode]"]] <- "Auto"
  
  if (!is.null(pmid)) query_params[["Study[PMID]"]] <- pmid
  if (!is.null(doi)) query_params[["Study[DOI]"]] <- doi
  if (!is.null(uri)) query_params[["Study[URI]"]] <- uri
  if (!is.null(state)) query_params[["Study[State]"]] <- state
  
  # Encode query parameters
  query_string <- paste(
    mapply(function(name, value) {
      paste0(URLencode(name, reserved = TRUE), "=", 
             URLencode(as.character(value), reserved = TRUE))
    }, names(query_params), query_params),
    collapse = "&"
  )
  
  # Make request
  response <- POST(
    base_url,
    body = list(
      action = "pfautoedit",
      format = "json",
      query = query_string
    ),
    encode = "form",
    set_cookies(setNames(cookies$value, cookies$name))
  )
  
  result <- content(response, as = "parsed")
  
  if (result$status != 200) {
    stop("Failed to create study: ", result$responseText)
  }
  
  message("Successfully created ", result$target)
  return(result)
}


#' Create a new experiment in BugSigDB
#'
#' @param cookies Cookie data from bugsigdb_login()
#' @param study_target Study identifier (e.g., "Study 719") from create_study()
#' @param location_of_subjects Character vector of countries
#' @param host_species Host species (e.g., "Homo sapiens")
#' @param body_site Character vector of body sites
#' @param condition Character vector of conditions
#' @param group0_name Name for control/reference group
#' @param group1_name Name for experimental/case group
#' @param group1_definition Definition of group 1
#' @param group0_sample_size Sample size for group 0
#' @param group1_sample_size Sample size for group 1
#' @param antibiotics_exclusion Antibiotic exclusion criteria
#' @param sequencing_type Sequencing type (e.g., "16S", "WMS")
#' @param sequencing_platform Character vector of platforms
#' @param statistical_test Character vector of statistical tests
#' @param significance_threshold Significance threshold (e.g., 0.05)
#' @param mht_correction "Yes" or "No" for multiple hypothesis testing correction
#' @param lda_score_above LDA score threshold
#' @param matched_on Character vector of matching variables
#' @param confounders Character vector of confounders controlled for
#' @param pielou "decreased", "increased", or "unchanged"
#' @param shannon "decreased", "increased", or "unchanged"
#' @param chao1 "decreased", "increased", or "unchanged"
#' @param simpson "decreased", "increased", or "unchanged"
#' @param inverse_simpson "decreased", "increased", or "unchanged"
#' @param richness "decreased", "increased", or "unchanged"
#' @param state "Complete" or "Incomplete" (default: "Incomplete")
#' @param edit_summary Summary for edit history
#' @param base_url Base URL for the API
#' @return A list with experiment information including the experiment target
#' @export
bugsigdb_create_experiment <- function(cookies,
                                       study_target,
                                       location_of_subjects = NULL,
                                       host_species = NULL,
                                       body_site = NULL,
                                       condition = NULL,
                                       group0_name = NULL,
                                       group1_name = NULL,
                                       group1_definition = NULL,
                                       group0_sample_size = NULL,
                                       group1_sample_size = NULL,
                                       antibiotics_exclusion = NULL,
                                       sequencing_type = NULL,
                                       sequencing_platform = NULL,
                                       statistical_test = NULL,
                                       significance_threshold = NULL,
                                       mht_correction = NULL,
                                       lda_score_above = NULL,
                                       matched_on = NULL,
                                       confounders = NULL,
                                       pielou = NULL,
                                       shannon = NULL,
                                       chao1 = NULL,
                                       simpson = NULL,
                                       inverse_simpson = NULL,
                                       richness = NULL,
                                       state = "Incomplete",
                                       edit_summary = "Adding experiment via API",
                                       base_url = "https://bugsigdb.org/w/api.php") {
  
  # Build query parameters
  query_params <- list(
    editRevId = "0",
    wpSummary = edit_summary,
    form = "Experiment",
    "Experiment[Base page]" = study_target
  )
  
  # Helper function to add list parameters
  add_list_param <- function(params, param_name, values) {
    if (!is.null(values)) {
      for (val in values) {
        params[[length(params) + 1]] <- val
        names(params)[length(params)] <- paste0(param_name, "[]")
      }
      params[[paste0(param_name, "[is_list]")]] <- "1"
    }
    return(params)
  }
  
  # Add list parameters
  query_params <- add_list_param(query_params, "Experiment[Location of subjects]", 
                                 location_of_subjects)
  query_params <- add_list_param(query_params, "Experiment[Body site]", body_site)
  query_params <- add_list_param(query_params, "Experiment[Condition]", condition)
  query_params <- add_list_param(query_params, "Experiment[Sequencing platform]", 
                                 sequencing_platform)
  query_params <- add_list_param(query_params, "Experiment[Statistical test]", 
                                 statistical_test)
  query_params <- add_list_param(query_params, "Experiment[Matched on]", matched_on)
  query_params <- add_list_param(query_params, "Experiment[Confounders controlled for]", 
                                 confounders)
  
  # Add single value parameters
  if (!is.null(host_species)) 
    query_params[["Experiment[Host species]"]] <- host_species
  if (!is.null(group0_name)) 
    query_params[["Experiment[Group 0 name]"]] <- group0_name
  if (!is.null(group1_name)) 
    query_params[["Experiment[Group 1 name]"]] <- group1_name
  if (!is.null(group1_definition)) 
    query_params[["Experiment[Group 1 definition]"]] <- group1_definition
  if (!is.null(group0_sample_size)) 
    query_params[["Experiment[Group 0 sample size]"]] <- as.character(group0_sample_size)
  if (!is.null(group1_sample_size)) 
    query_params[["Experiment[Group 1 sample size]"]] <- as.character(group1_sample_size)
  if (!is.null(antibiotics_exclusion)) 
    query_params[["Experiment[Antibiotics exclusion]"]] <- antibiotics_exclusion
  if (!is.null(sequencing_type)) 
    query_params[["Experiment[Sequencing type]"]] <- sequencing_type
  if (!is.null(significance_threshold)) 
    query_params[["Experiment[Significance threshold]"]] <- as.character(significance_threshold)
  if (!is.null(mht_correction)) 
    query_params[["Experiment[MHT correction]"]] <- mht_correction
  if (!is.null(lda_score_above)) 
    query_params[["Experiment[LDA Score above]"]] <- as.character(lda_score_above)
  if (!is.null(pielou)) 
    query_params[["Experiment[Pielou]"]] <- pielou
  if (!is.null(shannon)) 
    query_params[["Experiment[Shannon]"]] <- shannon
  if (!is.null(chao1)) 
    query_params[["Experiment[Chao1]"]] <- chao1
  if (!is.null(simpson)) 
    query_params[["Experiment[Simpson]"]] <- simpson
  if (!is.null(inverse_simpson)) 
    query_params[["Experiment[Inverse Simpson]"]] <- inverse_simpson
  if (!is.null(richness)) 
    query_params[["Experiment[Richness]"]] <- richness
  if (!is.null(state)) 
    query_params[["Experiment[State]"]] <- state
  
  # Encode query parameters
  query_string <- paste(
    mapply(function(name, value) {
      paste0(URLencode(name, reserved = TRUE), "=", 
             URLencode(as.character(value), reserved = TRUE))
    }, names(query_params), query_params),
    collapse = "&"
  )
  
  # Make request
  response <- POST(
    base_url,
    body = list(
      action = "pfautoedit",
      format = "json",
      query = query_string
    ),
    encode = "form",
    set_cookies(setNames(cookies$value, cookies$name))
  )
  
  result <- content(response, as = "parsed")
  
  if (result$status != 200) {
    stop("Failed to create experiment: ", result$responseText)
  }
  
  message("Successfully created ", result$target)
  return(result)
}


#' Create a new signature in BugSigDB
#'
#' @param cookies Cookie data from bugsigdb_login()
#' @param experiment_target Experiment identifier (e.g., "Study 719/Experiment 1")
#' @param ncbi_ids Numeric vector of NCBI taxonomy IDs
#' @param abundance "increased" or "decreased" (required)
#' @param source Source of the signature (e.g., "Figure 2", "Table 3")
#' @param description Description of the signature
#' @param state "Complete" or "Incomplete" (default: "Incomplete")
#' @param edit_summary Summary for edit history
#' @param base_url Base URL for the API
#' @return A list with signature information including the signature target
#' @export
bugsigdb_create_signature <- function(cookies,
                                      experiment_target,
                                      ncbi_ids,
                                      abundance = "increased",
                                      source = NULL,
                                      description = NULL,
                                      state = "Incomplete",
                                      edit_summary = "Adding signature via API",
                                      base_url = "https://bugsigdb.org/w/api.php") {
  
  # Build query parameters
  query_params <- list(
    editRevId = "0",
    wpSummary = edit_summary,
    form = "Signature",
    "Signature[Base page]" = experiment_target,
    "Signature[Abundance in Group 1]" = abundance
  )
  
  # Add NCBI IDs
  for (ncbi_id in ncbi_ids) {
    query_params[[length(query_params) + 1]] <- as.character(ncbi_id)
    names(query_params)[length(query_params)] <- "Signature[NCBI][]"
  }
  query_params[["Signature[NCBI][is_list]"]] <- "1"
  
  # Add optional parameters
  if (!is.null(source)) query_params[["Signature[Source]"]] <- source
  if (!is.null(description)) query_params[["Signature[Description]"]] <- description
  if (!is.null(state)) query_params[["Signature[State]"]] <- state
  
  # Encode query parameters
  query_string <- paste(
    mapply(function(name, value) {
      paste0(URLencode(name, reserved = TRUE), "=", 
             URLencode(as.character(value), reserved = TRUE))
    }, names(query_params), query_params),
    collapse = "&"
  )
  
  # Make request
  response <- POST(
    base_url,
    body = list(
      action = "pfautoedit",
      format = "json",
      query = query_string
    ),
    encode = "form",
    set_cookies(setNames(cookies$value, cookies$name))
  )
  
  result <- content(response, as = "parsed")
  
  if (result$status != 200) {
    stop("Failed to create signature: ", result$responseText)
  }
  
  message("Successfully created ", result$target)
  return(result)
}

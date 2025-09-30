library(httr)

# Helper to encode values for x-www-form-urlencoded URL params
urlenc <- function(x) {
  # Use reserved=TRUE to ensure spaces become %20 not +
  URLencode(as.character(x), reserved=TRUE)
}

# Helper to double-encode the 'query' string
make_double_encoded_query <- function(params) {
  # params: named list (possibly with repeated names for fields ending in [])
  pieces <- c()
  for (name in names(params)) {
    vals <- params[[name]]
    if (is.null(vals)) next
    if (!is.vector(vals)) vals <- list(vals)
    for (val in vals) {
      if (!is.null(val) && !is.na(val)) {
        # Handle boolean
        if (is.logical(val)) val <- ifelse(val, "1", "0")
        # For params like foo[], leave the [] in the name
        pieces <- c(pieces, paste0(urlenc(name), "=", urlenc(val)))
      }
    }
  }
  # single-encoded
  q_one <- paste(pieces, collapse = "&")
  # now double encode for pfautoedit
  q_two <- urlenc(q_one)
  return(q_two)
}

# ---- BUILDERS (do not send request!) ----

make_study_body <- function(
    study_design = "case-control", # char vector, one or more
    pmid = "", doi = "", uri = "", state = "Complete",
    editRevId = 0, wpSummary = "Adding study via API", form = "Study",
    action="pfautoedit", format="json"
) {
  params <- list(
    # List items: assign multiple entries with same name Study[study design][]
    'Study[study design][]' = study_design,
    'Study[study design][is_list]' = "1",
    'Study[Mode]' = "Auto",
    'Study[PMID]' = pmid,
    'Study[DOI]' = doi,
    'Study[URI]' = uri,
    'Study[State]' = state,
    'editRevId' = editRevId,
    'wpSummary' = wpSummary,
    'form' = form
  )
  body <- list(
    action = action,
    format = format,
    query = make_double_encoded_query(params)
  )
  return(body)
}

make_experiment_body <- function(
    base_page, # required, eg "Study_123"
    location_of_subjects = NULL,  # char vector
    host_species = NULL,
    body_site = NULL,             # char vector
    condition = NULL,             # char vector
    group0_name = NULL, group1_name = NULL, group1_def = NULL,
    group0_n = NULL, group1_n = NULL,
    antibiotics_exclusion = NULL,
    sequencing_type = NULL,
    sequencing_platform = NULL,   # char vector
    statistical_test = NULL,      # char vector
    significance_threshold = NULL,
    mht_correction = NULL, lda_score_above = NULL,
    matched_on = NULL,
    confounders = NULL,
    pielou = NULL, shannon = NULL, chao1 = NULL, simpson = NULL, inverse_simpson = NULL, richness = NULL,
    state = "Complete", editRevId = 0, wpSummary = "Adding experiment via API", form = "Experiment",
    action="pfautoedit", format="json"
) {
  params <- list(
    # Form-page attachment
    'Experiment[Base page]' = base_page,
    'Experiment[Location of subjects][]' = location_of_subjects,
    'Experiment[Location of subjects][is_list]' = if(!is.null(location_of_subjects)) "1",
    'Experiment[Host species]' = host_species,
    'Experiment[Body site][]' = body_site,
    'Experiment[Body site][is_list]' = if(!is.null(body_site)) "1",
    'Experiment[Condition][]' = condition,
    'Experiment[Condition][is_list]' = if(!is.null(condition)) "1",
    'Experiment[Group 0 name]' = group0_name,
    'Experiment[Group 1 name]' = group1_name,
    'Experiment[Group 1 definition]' = group1_def,
    'Experiment[Group 0 sample size]' = group0_n,
    'Experiment[Group 1 sample size]' = group1_n,
    'Experiment[Antibiotics exclusion]' = antibiotics_exclusion,
    'Experiment[Sequencing type]' = sequencing_type,
    'Experiment[Sequencing platform][]' = sequencing_platform,
    'Experiment[Sequencing platform][is_list]' = if(!is.null(sequencing_platform)) "1",
    'Experiment[Statistical test][]' = statistical_test,
    'Experiment[Statistical test][is_list]' = if(!is.null(statistical_test)) "1",
    'Experiment[Significance threshold]' = significance_threshold,
    'Experiment[MHT correction]' = mht_correction,
    'Experiment[LDA Score above]' = lda_score_above,
    'Experiment[Matched on][]' = matched_on,
    'Experiment[Matched on][is_list]' = if(!is.null(matched_on)) "1",
    'Experiment[Confounders controlled for][]' = confounders,
    'Experiment[Confounders controlled for][is_list]' = if(!is.null(confounders)) "1",
    'Experiment[Pielou]' = pielou,
    'Experiment[Shannon]' = shannon,
    'Experiment[Chao1]' = chao1,
    'Experiment[Simpson]' = simpson,
    'Experiment[Inverse Simpson]' = inverse_simpson,
    'Experiment[Richness]' = richness,
    'Experiment[State]' = state,
    'editRevId' = editRevId,
    'wpSummary' = wpSummary,
    'form' = form
  )
  body <- list(
    action = action,
    format = format,
    query = make_double_encoded_query(params)
  )
  return(body)
}

make_signature_body <- function(
    base_page, # e.g. "Study_473/Experiment_4"
    source = NULL, description = NULL, abundance_in_group1 = "increased",
    ncbi = NULL,  # vector (numbers/character)
    state = "Complete", editRevId = 0, wpSummary = "Adding signature via API", form = "Signature",
    action="pfautoedit", format="json"
) {
  params <- list(
    'Signature[Base page]' = base_page,
    'Signature[Source]' = source,
    'Signature[Description]' = description,
    'Signature[Abundance in Group 1]' = abundance_in_group1,
    'Signature[NCBI][]' = ncbi,
    'Signature[NCBI][is_list]' = if(!is.null(ncbi)) "1",
    'Signature[State]' = state,
    'editRevId' = editRevId,
    'wpSummary' = wpSummary,
    'form' = form
  )
  body <- list(
    action = action,
    format = format,
    query = make_double_encoded_query(params)
  )
  return(body)
}

# ---- SENDER ----

send_bugsigdb_request <- function(
    api_url = "https://bugsigdb.org/w/api.php",
    body,
    cookies = NULL # named vector, e.g. c(mediawikiUserName=..., ...)
) {
  req <- httr::POST(
    api_url,
    body = body,
    encode = "form",
    httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
    if (!is.null(cookies)) httr::set_cookies(.cookies = cookies)
  )
  httr::content(req, as="parsed")
}

# --- EXAMPLE USAGE ---

## 1. Build a study
study_body <- make_study_body(
  study_design = c("case-control", "meta-analysis"),
  pmid = "12345", doi = "10.1038/abc.12345", uri = "https://example.com"
)
# You would supply your cookies (from a browser login), for example:
# cookies <- c(mediawikiUserName="...", mediawikiUserID="...", mediawikiToken="...", mediawiki_session="...")

# res <- send_bugsigdb_request(body = study_body, cookies = cookies)
# study_target <- res$target

## 2. Build an experiment under this study
# experiment_body <- make_experiment_body(
#   base_page = study_target,
#   location_of_subjects = c("Canada", "Norway"),
#   body_site = c("Accessory saphenous vein", "1st arch mandibular endoderm"),
#   condition = "(+)-dexrazoxane",
#   group0_name = "Control", group1_name = "Treatment",
#   group0_n = 100, group1_n = 100,
#   antibiotics_exclusion = "Antibiotic exclusion here",
#   sequencing_type = "WMS",
#   sequencing_platform = c("Ion Torrent", "DNA-DNA Hybridization"),
#   statistical_test = "Dunn's test",
#   significance_threshold = "0.01",
#   mht_correction = "Yes",
#   lda_score_above = "3",
#   state = "Complete"
# )
# exp_res <- send_bugsigdb_request(body = experiment_body, cookies = cookies)
# experiment_target <- exp_res$target

## 3. Build a signature for this experiment
# sig_body <- make_signature_body(
#   base_page = experiment_target,
#   source = "Some dataset",
#   description = "Meta-analysis signature",
#   abundance_in_group1 = "increased",
#   ncbi = c("1581061", "12345")
# )
# sig_res <- send_bugsigdb_request(body = sig_body, cookies = cookies)
# signature_target <- sig_res$target
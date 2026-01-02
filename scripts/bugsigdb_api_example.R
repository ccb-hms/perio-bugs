library(httr)
library(jsonlite)
source('scripts/bugsigdb_api.R')

# login
# Get credentials from environment
username <- Sys.getenv("BUGSIGDB_USERNAME")
password <- Sys.getenv("BUGSIGDB_PASSWORD")
cookies <- bugsigdb_login(username, password)

# study creation
# can only run once (only admin can delete)

test_study <- bugsigdb_create_study(
  cookies = cookies,
  pmid = "12345678",
  doi = "10.1234/test",
  dry_run = FALSE  # Just print, don't execute
)

test_study <- list(target = '12345678')

# create experiment
test_experiment <- bugsigdb_create_experiment(
  cookies = cookies,
  study_target = test_study$target,  # Links to your study
  
  # Required/important fields
  host_species = "Homo sapiens",
  body_site = c("feces"),
  condition = c("healthy"),
  
  # Group definitions
  group0_name = "control",
  group1_name = "case",
  group1_definition = "Test case group",
  group0_sample_size = 50,
  group1_sample_size = 50,
  
  # Sequencing info
  sequencing_type = "16S",
  
  # Statistical info
  significance_threshold = 0.05,
  
  state = "Complete",
  dry_run = FALSE
)

test_experiment <- list(target = "12345678/Experiment 1")

test_signature <- bugsigdb_create_signature(
  cookies = cookies,
  experiment_target = test_experiment$target,  # Links to your experiment
  
  # NCBI Taxonomy IDs (required)
  ncbi_ids = c(1580, 1579),  # Example: Lactobacillus, Lactobacillus acidophilus
  
  # Abundance change (required)
  abundance = "increased",  # or "decreased"
  
  # Optional metadata
  source = "Figure 2A",
  description = "Test signature - increased in case group",
  
  state = "Complete",
  dry_run = FALSE
)

# Check what was created
test_signature$target
# Should return something like "12345678/Experiment 1/Signature 1"
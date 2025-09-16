# comparing bugsigdb and Feres data
#
# one common study PMID: 23613868 Zhou M et al 2013
#
# study 1411 in Feres database:
#  - present in sheets "Sarah's Work" and "Sarah's Work (2)"
#  - not present on "New DATABASE" or "Old DATABASE"
#  - in "Sarah's Work"
#     - 10 "species elevated in health"
#     - 1 "species elevated in periodontitis"
#  - in "Sarah's Work (2)"
#     - 10 "species elevated in health"
#     - 0 "species elevated in periodontitis"
#
# BugSigDB has 8 studies on periodontitis
# Feres data has ~119 studies on periodontitis

library(bugsigdbr)

# get study where condition is periodontitis
df <- importBugSigDB()
study_df <- subset(df, PMID == '23613868' & Condition == 'Periodontitis')

# based on sample sizes recorded in sheet "Microbiological assessment" for study 1411
# comparing periodontitis (n=6) vs healthy (n=5) in non-diabetic
# corresponding signatures are first two rows
study_df <- subset(study_df, `Group 0 sample size` == 5 & `Group 1 sample size` == 6)
study_df$`Abundance in Group 1`
study_df$`Group 1 name`

perio <- getSignatures(study_df, "taxname")
perio

# ---- UP in periodontitis:

sarahs_work_perio_up <- c(
  'Selenomonas sputigena'          # both
)

# in bugsigdb but not Feres data:
"Prevotellaceae"                   # table 1: unique perio. UP in diabetes-negative ✓
"Streptococcus gordonii"           # table 1: common health UP irrespective of diabetes ❌

# in neither bugsigdb or Feres but either unique perio. UP or common perio. UP:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3632544/table/pone-0061516-t001/
"Treponema denticola"              # common perio. UP
"Prevotella intermedia"            # common perio. UP
"family of Prevotellaceae"         # common perio. UP

# ---- DOWN in periodontitis:

sarahs_work_perio_dn <- c(
  'Corynebacterium matruchotii',   # both
  'Prevotella loescheii',          # both
  'Aggregatibacter',               # both
  'Neisseria flavescens',          # both
  'Cardiobacterium hominis',       # both
  'Cardiobacterium valvarum',      # both
  'Leptotrichia',                  # both
  'Leptotrichia',                  # family (Leptotrichiaceae) in bugsigdb (✓), genus in Feres data (❌)
  'Capnocytophaga sputigena',      # both
  'Porphyromonas sp.'              # both
)

# in bugsigdb but not Feres data:
"Hoylesella loescheii"             # table 1: ❌
"Propionibacteriaceae"             # table 1: unique health UP in diabetes-negative ✓
"Selenomonas noxia"                # table 1: unique health UP in diabetes-negative ✓

# in neither bugsigdb or Feres but in common perio. UP:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3632544/table/pone-0061516-t001/
"Streptococcus infantis"
"Streptococcus gordonii"
"Actinomyces naeslundii"

# Corrections to the signatures' taxa from the taxa review (pending curator confirmation)
#
# usage: Rscript scripts/llm_annotation/resolve_taxa_review.R
# reads the taxa review (scripts/llm_annotation/taxa_review_instructions.md; output/llm_annotation/
# taxa_review/ and output/llm_annotation/added/taxa_review/) and writes
# output/llm_annotation/taxa_review/corrections.csv, one row per proposed change, with its status:
# - applied: scripts/5-join_overview_and_microbe.R --corrected makes the change (apply_taxa_corrections())
# - left for review: the reviewer could not settle it from the paper (action "check"), or made it conditional
#   on a curator's decision (e.g. "add only if curators accept the four-group test")
# - not applied: no usable NCBI ID, or the change was already made
# - applied / not applied, awaiting curator approval: an item left for review that has since been decided
#   (see "decisions" below); the curators confirm or reject it in curator_decision
#
# Changes, as the reviewer proposed them against the paper:
# - remove: the taxon is not reported as significant for the curated comparison
# - fix_direction: the paper reports the taxon in the other direction
# - fix_ncbi: the ID is not the organism the paper names; replaced by the reviewer's ID
# - add / include: the paper reports the taxon for the comparison, but it was not recorded
# and two made for every signature, from the taxon ID audit (scripts/audit_taxon_ids.R):
# - merged ID: an ID NCBI has merged into another is replaced by the current one
# - placeholder: a catch-all node such as "Streptococcus sp." (any unidentified member of the genus) is
#   replaced by its nearest ranked ancestor, usually the genus, so the same taxon is mapped the same way in
#   every study
# Every new ID is checked against NCBI (it must be current) and the local NCBI copy that builds the lineages.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(rentrez)
})

review_dir <- 'output/llm_annotation/taxa_review'
added_dir <- 'output/llm_annotation/added/taxa_review'

read_dir <- function(dir, pattern) {
  map_dfr(list.files(dir, pattern = pattern, full.names = TRUE),
          \(f) read_csv(f, col_types = cols(.default = 'c'), show_col_types = FALSE) |>
            mutate(Study = sub('_(review|recorded_taxa)[.]csv$', '', basename(f))))
}

# the recorded taxa each review row refers to (row = position in the study's input)
inputs <- bind_rows(
  read_dir(file.path(review_dir, 'input'), '_recorded_taxa[.]csv$') |>
    transmute(Study, row, input_direction = direction, from_taxid = ncbi_taxid),
  read_dir(file.path(added_dir, 'input'), '_recorded_taxa[.]csv$') |>
    transmute(Study, row, input_direction = direction, from_taxid = ncbi_taxid)
)

review <- bind_rows(read_dir(review_dir, '_review[.]csv$'), read_dir(added_dir, '_review[.]csv$')) |>
  mutate(recorded_taxon = coalesce(recorded_taxon, taxon_reported), action = tolower(trimws(action))) |>
  left_join(inputs, by = c('Study', 'row')) |>
  mutate(direction = coalesce(input_direction, direction),
         suggested = stringr::str_extract(suggested_ncbi_taxid, '[0-9]+'),
         conditional = grepl(paste0('only if|if curators|if the curator|conditional|optional|may drop|may prefer|',
                                    'may want|decide|if accepted|if .{0,30}accept'), note, ignore.case = TRUE))

other <- c(increased = 'decreased', decreased = 'increased')

proposed <- review |>
  filter(action != 'keep') |>
  transmute(
    Study, direction, action, recorded_taxon, paper_name,
    from_taxid = ifelse(action %in% c('remove', 'fix_direction', 'fix_ncbi', 'check', 'include'), from_taxid, NA),
    to_taxid = case_when(action == 'remove' ~ NA,
                         action == 'fix_direction' ~ coalesce(suggested, from_taxid),
                         action == 'include' ~ coalesce(suggested, from_taxid),
                         .default = suggested),
    to_direction = case_when(action == 'fix_direction' ~ unname(other[direction]),
                             action == 'remove' ~ NA, .default = direction),
    status = case_when(
      action == 'check' ~ 'left for review',
      conditional ~ 'left for review',
      action != 'remove' & is.na(to_taxid) ~ 'not applied',
      action %in% c('remove', 'fix_direction', 'fix_ncbi') & is.na(from_taxid) ~ 'not applied',
      .default = 'applied'),
    reason = case_when(
      action == 'check' ~ 'the reviewer could not settle it from the paper',
      conditional ~ "the reviewer made it conditional on a curator's decision",
      action != 'remove' & is.na(to_taxid) ~ 'no NCBI ID for this taxon',
      action %in% c('remove', 'fix_direction', 'fix_ncbi') & is.na(from_taxid) ~ 'recorded row not found',
      .default = c(remove = 'not reported as significant for the curated comparison',
                   fix_direction = 'the paper reports the other direction',
                   fix_ncbi = 'the ID is not the organism the paper names',
                   add = 'the paper reports it for the comparison, but it was not recorded',
                   include = 'the paper reports it for the comparison, but it was left out')[action]),
    evidence, note, reviewer_ncbi_ok = ncbi_ok)

# ---- placeholder and merged IDs in every signature ----

# the signatures' taxa before any correction (written by scripts/5-join_overview_and_microbe.R --corrected)
recorded <- readRDS('output/diff_species_corrected_prereview.rds') |>
  transmute(Study = paste('Study', Number), direction = ifelse(direction == 'up', 'increased', 'decreased'), taxid) |>
  distinct()

ncbi_status <- function(ids) {
  ids <- unique(na.omit(ids))
  one <- \(chunk) {
    Sys.sleep(1)
    s <- entrez_summary('taxonomy', chunk, always_return_list = TRUE)
    tibble(taxid = names(s), status = map_chr(s, \(x) x$status %||% 'not found'),
           name = map_chr(s, \(x) x$scientificname %||% NA_character_),
           rank = map_chr(s, \(x) x$rank %||% NA_character_),
           merged_into = map_chr(s, \(x) as.character(x$akataxid %||% '')))
  }
  # IDs NCBI does not know make the whole request fail; they are then looked up one at a time
  map_dfr(split(ids, ceiling(seq_along(ids) / 150)), \(chunk) {
    res <- tryCatch(one(chunk), error = \(e) NULL)
    if (!is.null(res) && nrow(res) == length(chunk)) return(res)
    map_dfr(chunk, \(id) tryCatch(one(id), error = \(e) tibble(taxid = id, status = 'not found', name = NA,
                                                                rank = NA, merged_into = '')))
  })
}

status <- ncbi_status(c(recorded$taxid, proposed$to_taxid))
placeholder <- \(name) grepl('^[A-Z][a-z]+ sp\\.$|^[A-Z][a-z]+ (bacterium|archaeon)$|^uncultured|^unidentified', name)
lineage <- \(id) { l <- taxizedb::classification(id, db = 'ncbi')[[1]]; if (is.data.frame(l)) l else NULL }
ranked_parent <- function(id) {
  l <- lineage(id)
  if (is.null(l)) return(NA_character_)
  l <- l[l$id != id & l$rank %in% c('genus', 'family', 'order', 'class', 'phylum'), ]
  if (nrow(l)) l$id[nrow(l)] else NA_character_
}

global <- status |>
  filter(status == 'merged' | (status == 'active' & placeholder(name))) |>
  mutate(to_taxid = ifelse(status == 'merged', merged_into, map_chr(taxid, ranked_parent)),
         action = ifelse(status == 'merged', 'merged ID', 'placeholder'))

global_rows <- recorded |>
  inner_join(select(global, taxid, to_taxid, action, name), by = 'taxid') |>
  transmute(Study, direction, action, recorded_taxon = name, from_taxid = taxid, to_taxid, to_direction = direction,
            status = ifelse(is.na(to_taxid), 'not applied', 'applied'),
            reason = ifelse(action == 'merged ID', 'NCBI merged this ID into the current one',
                            'catch-all node replaced by its nearest ranked ancestor'))

# placeholder targets of the reviewer's own new IDs are replaced the same way
global_rows <- global_rows |>
  mutate(reason = ifelse(is.na(to_taxid), 'no ranked ancestor in NCBI', reason)) |>
  # the reviewer's change to the same recorded taxon takes precedence
  anti_join(filter(proposed, status == 'applied', action %in% c('remove', 'fix_direction', 'fix_ncbi')),
            by = c('Study', 'direction', 'from_taxid'))

corrections <- bind_rows(proposed, global_rows) |>
  left_join(select(global, taxid, global_to = to_taxid), by = c('to_taxid' = 'taxid')) |>
  mutate(to_taxid = coalesce(global_to, to_taxid)) |>
  select(-global_to)

# every new ID must be current in NCBI and in the local copy that builds the lineages
new_ids <- setdiff(unique(na.omit(corrections$to_taxid)), status$taxid)
status <- bind_rows(status, ncbi_status(new_ids))
local_ok <- map_lgl(unique(na.omit(corrections$to_taxid)), \(id) !is.null(lineage(id)))
names(local_ok) <- unique(na.omit(corrections$to_taxid))
corrections <- corrections |>
  left_join(select(status, taxid, to_status = status, to_name = name, to_rank = rank), by = c('to_taxid' = 'taxid')) |>
  mutate(
    reason = case_when(status == 'applied' & !is.na(to_taxid) & to_status != 'active' ~ paste('new ID is', to_status, 'in NCBI'),
                       status == 'applied' & !is.na(to_taxid) & !local_ok[to_taxid] ~ 'new ID not in the local NCBI copy',
                       .default = reason),
    status = ifelse(status == 'applied' & !is.na(to_taxid) & (to_status != 'active' | !local_ok[to_taxid]),
                    'not applied', status)) |>
  left_join(select(status, taxid, from_name = name), by = c('from_taxid' = 'taxid')) |>
  relocate(from_name, .after = from_taxid) |>
  relocate(to_name, to_rank, .after = to_taxid) |>
  select(-to_status)

# ---- the curators' practice (data/OVERVIEW_SHEET_*.xlsx, "Microbiological assessment") ----
# Added taxa beyond what the curators chose to record are left for review:
# - family, order and class results: the extraction template collects phyla, genera and species only; the
#   curators recorded higher ranks only for unnamed genera or species, which the reviewer marks 'parent'
# - prevalence results where the curators recorded the same paper's abundance analysis (one analysis per study)
# (a taxon whose evidence also cites the paper's abundance results is kept)
abundance_chosen <- tribble(
  ~Study,       ~prevalence_source,      ~abundance_source,
  'Study 1446', 'Figure S3|Figure S7',   'Figure S1|Figure S5|Figure S9',  # curators used LEfSe (Figure S1)
  'Study 1032', 'Supp[.]? Table 1',      'Supp[.]? Table 2',               # curators used abundance
  'Study 811',  'Table 4',               'Table 3'                         # curators used levels
)
prevalence_add <- \(study, evidence) map2_lgl(study, evidence, \(s, e) {
  i <- which(abundance_chosen$Study == s)
  e <- coalesce(e, '')
  length(i) == 1 && grepl(abundance_chosen$prevalence_source[i], e) && !grepl(abundance_chosen$abundance_source[i], e)
})
corrections <- corrections |>
  mutate(
    rank_result = action == 'add' & to_rank %in% c('family', 'order', 'class') & !reviewer_ncbi_ok %in% 'parent',
    prevalence = action %in% c('add', 'include') & prevalence_add(Study, evidence),
    reason = case_when(status == 'applied' & rank_result ~
                         'a family, order or class result; the curators recorded phyla, genera and species only',
                       status == 'applied' & prevalence ~
                         "a prevalence result; for this paper the curators recorded the abundance analysis",
                       .default = reason),
    status = ifelse(status == 'applied' & (rank_result | prevalence), 'left for review', status)) |>
  select(-rank_result, -prevalence)

# ---- decisions on the items left for review (pending curator approval) ----
# Each item has a stable ID. Decisions are in output/llm_annotation/taxa_review/decisions/*.csv (item_id,
# decision, to_taxid, to_direction, rationale, decided_by), written by decide_left_for_review.R (rules from
# the curators' practice) and by the per-study adjudication (scripts/llm_annotation/adjudication_instructions.md).
# decision is one of:
# - apply: make the proposed change (add, include, remove, fix_ncbi)
# - decline: change nothing
# - keep: (for a check) leave the recorded taxon as it is
# - remove, fix_direction, fix_ncbi, add: make this change instead (fix_ncbi and add need to_taxid)
# Items with a decision get the status "applied, awaiting curator approval" (the corrected build makes the
# change) or "not applied, awaiting curator approval". Curators record their verdict in curator_decision.
corrections <- corrections |>
  mutate(item_id = substr(map_chr(paste(Study, action, direction, recorded_taxon, paper_name, from_taxid, to_taxid,
                                        sep = '|'), digest::digest, algo = 'md5', serialize = FALSE), 1, 10))
stopifnot(!anyDuplicated(corrections$item_id))

decisions <- map_dfr(list.files(file.path(review_dir, 'decisions'), pattern = '[.]csv$', full.names = TRUE),
                     \(f) read_csv(f, col_types = cols(.default = 'c'), show_col_types = FALSE)) |>
  bind_rows(tibble(item_id = character(), decision = character(), to_taxid = character(),
                   to_direction = character(), rationale = character(), decided_by = character())) |>
  mutate(decision = tolower(trimws(decision))) |>
  select(item_id, decision, decision_taxid = to_taxid, decision_direction = to_direction, rationale, decided_by)
stopifnot(!anyDuplicated(decisions$item_id),
          all(decisions$decision %in% c('apply', 'decline', 'keep', 'remove', 'fix_direction', 'fix_ncbi', 'add')))
# a decision can also overturn an applied reviewer change (e.g. an add found to come from another comparison)
decidable <- corrections$status %in% c('left for review', 'applied')
unknown <- setdiff(decisions$item_id, corrections$item_id[decidable])
if (length(unknown)) warning(length(unknown), ' decisions refer to no open or applied item: ', toString(unknown))

decided <- corrections |>
  filter(decidable) |>
  inner_join(decisions, by = 'item_id') |>
  mutate(
    proposed_action = action,
    action = case_when(decision %in% c('apply', 'decline', 'keep') ~ action, .default = decision),
    to_taxid = case_when(action == 'remove' ~ NA_character_,
                         action == 'fix_direction' ~ coalesce(decision_taxid, from_taxid),
                         .default = coalesce(decision_taxid, to_taxid)),
    to_direction = case_when(action == 'remove' ~ NA_character_,
                             action == 'fix_direction' ~ coalesce(decision_direction, unname(other[direction])),
                             .default = coalesce(decision_direction, to_direction, direction)),
    makes_change = !decision %in% c('decline', 'keep') & action != 'check',
    status = if_else(makes_change, 'applied, awaiting curator approval', 'not applied, awaiting curator approval'))

# changes found later that no review item covers (adjudication extras, comparison audits):
# output/llm_annotation/taxa_review/extra_changes.csv (Study, direction, action = add / remove /
# fix_direction, taxid, rationale, decided_by)
extra_file <- file.path(review_dir, 'extra_changes.csv')
if (file.exists(extra_file)) {
  extra <- read_csv(extra_file, col_types = cols(.default = 'c'), show_col_types = FALSE) |>
    transmute(Study, direction, action, recorded_taxon = name,
              from_taxid = ifelse(action == 'add', NA_character_, taxid),
              to_taxid = ifelse(action == 'remove', NA_character_, taxid),
              to_direction = case_when(action == 'remove' ~ NA_character_,
                                       action == 'fix_direction' ~ unname(other[direction]), .default = direction),
              status = 'applied, awaiting curator approval', reason = 'found after the review', decision = action, makes_change = TRUE,
              rationale, decided_by, proposed_action = action) |>
    mutate(item_id = substr(map_chr(paste('extra', Study, action, direction, coalesce(from_taxid, to_taxid), sep = '|'), digest::digest,
                                    algo = 'md5', serialize = FALSE), 1, 10))
  decided <- bind_rows(decided, extra)
}

# new IDs from the decisions are checked like the reviewer's
new_ids <- setdiff(unique(na.omit(decided$to_taxid)), status$taxid)
status <- bind_rows(status, ncbi_status(new_ids))
ok <- \(id) !is.na(id) & id %in% status$taxid[status$status == 'active'] & map_lgl(id, \(i) is.na(i) || !is.null(lineage(i)))
decided <- decided |>
  select(-to_name, -to_rank) |>
  left_join(select(status, taxid, to_name = name, to_rank = rank), by = c('to_taxid' = 'taxid')) |>
  mutate(bad_id = makes_change & action != 'remove' & !ok(to_taxid),
         status = if_else(bad_id, 'not applied', status),
         rationale = if_else(bad_id, paste(rationale, '(not applied: the new ID is not current in NCBI)'), rationale)) |>
  select(-makes_change, -bad_id, -decision_taxid, -decision_direction)

# a decided change to a recorded taxon takes precedence over its placeholder or merged-ID row
replaced_by_decision <- filter(decided, grepl('^applied', status), action %in% c('remove', 'fix_direction', 'fix_ncbi'))
corrections <- corrections |>
  filter(!item_id %in% decided$item_id) |>
  anti_join(filter(corrections, action %in% c('placeholder', 'merged ID')) |>
              semi_join(replaced_by_decision, by = c('Study', 'direction', 'from_taxid')), by = 'item_id') |>
  bind_rows(decided) |>
  mutate(curator_decision = '', curator_note = '') |>
  relocate(item_id) |>
  relocate(proposed_action, decision, rationale, decided_by, .after = status) |>
  arrange(Study, status, action)

write_csv(corrections, file.path(review_dir, 'corrections.csv'), na = '')
print(count(corrections, status, action))

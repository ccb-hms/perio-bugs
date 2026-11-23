# Audit of the NCBI Taxonomy IDs that PerioSigDB's taxa were mapped to
#
# usage: Rscript scripts/audit_taxon_ids.R
# checks every taxon of every signature in output/perio_bugs_corrected.rds and writes, in
# output/taxon_id_audit/:
# - taxid_audit.csv: one row per NCBI ID, with what NCBI says about it now and the problems found
# - signature_taxa_audit.csv: one row per taxon of each signature, with the ID's problems and, where the
#   taxa review (scripts/llm_annotation/taxa_review_instructions.md) checked the row against the paper,
#   whether the ID is the organism the paper names, and the ID it suggests
# - inconsistent_names.csv: taxa the papers name the same way but that were mapped to different IDs
#
# Problems flagged for an ID:
# - merged / deleted: NCBI no longer has the ID as given (merged IDs point to the ID that replaced them)
# - not bacteria or archaea: the lineage is a plant, animal or other eukaryote (fungi are reported apart)
# - placeholder: a catch-all node such as "Streptococcus sp." or "Clostridiales bacterium", which stands for
#   any unidentified member of the group; the genus or the specific oral taxon is the better ID
# - not oral: a species whose ID is not in the Human Oral Microbiome Database (HOMD) and whose genus has no
#   HOMD taxon either; such IDs are often a wrong match for an oral taxon (e.g. Salmonella, Elusimicrobium)

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(rentrez)
})

out_dir <- 'output/taxon_id_audit'
dir.create(out_dir, showWarnings = FALSE)

# ---- the signatures' taxa ----

pb <- readRDS('output/perio_bugs_corrected.rds')
sig_taxa <- pb |>
  transmute(Study, direction = `Abundance in Group 1`, review_status = sub(':.*', '', `Review status`),
            taxid = map(`NCBI Taxonomy IDs`, \(x) sub('^.*[|]', '', x))) |>
  unnest(taxid) |>
  group_by(Study, direction) |>
  mutate(row_in_signature = row_number()) |>
  ungroup()

ids <- unique(sig_taxa$taxid)

# ---- what NCBI says about each ID now ----

summaries <- map_dfr(split(ids, ceiling(seq_along(ids) / 150)), \(chunk) {
  Sys.sleep(0.5)
  s <- entrez_summary('taxonomy', chunk, always_return_list = TRUE)
  tibble(taxid = names(s), status = map_chr(s, 'status'), ncbi_name = map_chr(s, 'scientificname'),
         ncbi_rank = map_chr(s, 'rank'), merged_into = map_chr(s, \(x) as.character(x$akataxid)))
})

# lineage of current IDs (merged IDs through the ID they were merged into), from the local NCBI copy
current <- coalesce(na_if(summaries$merged_into, ''), summaries$taxid)
lineages <- taxizedb::classification(unique(current), db = 'ncbi')
lineage_of <- \(id) { l <- lineages[[id]]; if (is.null(l) || !is.data.frame(l)) NULL else l }
rank_name <- \(id, rank) { l <- lineage_of(id); if (is.null(l)) NA_character_ else l$name[l$rank == rank][1] }

# ---- HOMD ----

homd <- read_tsv(list.files('data', pattern = '^HOMD_taxon_table[0-9]', full.names = TRUE)[1], skip = 1,
                 col_types = cols(.default = 'c'), show_col_types = FALSE)
homd_ids <- unique(trimws(unlist(strsplit(na.omit(homd$NCBI_taxon_id), '[,; ]+'))))
clean_genus <- \(x) gsub('[][]', '', sub(' .*', '', x))
homd_genera <- unique(c(clean_genus(homd$Genus),
                        clean_genus(unlist(strsplit(na.omit(homd$Synonyms), '[,;] *')))))
homd_names <- tolower(unique(c(paste(clean_genus(homd$Genus), homd$Species), unlist(strsplit(na.omit(homd$Synonyms), '[,;] *')))))
# HOMD's own naming for its unnamed taxa, which NCBI files as e.g. "Synergistetes bacterium oral taxon 360"
oral_named <- \(name) grepl('oral taxon|oral clone|human oral|HOT-|Oral Taxon|HMT', name)

# ---- problems per ID ----

placeholder <- \(name) grepl('^[A-Z][a-z]+ sp\\.$|^[A-Z][a-z]+ (bacterium|archaeon)$|^uncultured|^unidentified|environmental sample', name)

audit <- summaries |>
  mutate(current_id = coalesce(na_if(merged_into, ''), taxid),
         superkingdom = map_chr(current_id, \(id) coalesce(rank_name(id, 'superkingdom'), rank_name(id, 'domain'))),
         kingdom = map_chr(current_id, \(id) rank_name(id, 'kingdom')),
         genus = map_chr(current_id, \(id) rank_name(id, 'genus')),
         in_homd = taxid %in% homd_ids | current_id %in% homd_ids | tolower(gsub('[][]', '', ncbi_name)) %in% homd_names |
           oral_named(ncbi_name),
         genus_in_homd = clean_genus(coalesce(genus, ncbi_name)) %in% homd_genera | clean_genus(ncbi_name) %in% homd_genera,
         problems = pmap_chr(list(status, ncbi_name, ncbi_rank, superkingdom, kingdom, in_homd, genus_in_homd, merged_into),
           \(status, name, rank, sk, kg, homd, ghomd, into) {
             p <- c(
               if (status == 'merged') paste('merged into', into),
               if (!status %in% c('active', 'merged')) status,
               if (!is.na(sk) && !sk %in% c('Bacteria', 'Archaea') && !identical(kg, 'Fungi')) paste('not bacteria or archaea:', sk),
               if (identical(kg, 'Fungi')) 'fungus',
               if (status == 'active' && placeholder(name)) 'placeholder',
               if (status == 'active' && rank %in% c('species', 'subspecies', 'strain') && !homd && !ghomd) 'not an oral taxon (HOMD)'
             )
             if (length(p)) paste(p, collapse = '; ') else NA_character_
           })) |>
  select(taxid, status, merged_into, ncbi_name, ncbi_rank, superkingdom, genus, in_homd, genus_in_homd, problems)

write_csv(audit, file.path(out_dir, 'taxid_audit.csv'), na = '')

# ---- per signature taxon, with the paper-based taxa review ----

# the review's input rows are the signature's taxa in order (output/llm_annotation/taxa_review/input)
read_review <- function(dir) {
  files <- list.files(dir, pattern = '_review[.]csv$', full.names = TRUE)
  map_dfr(files, \(f) read_csv(f, col_types = cols(.default = 'c'), show_col_types = FALSE) |>
            mutate(Study = sub('_review[.]csv$', '', basename(f))))
}
review <- read_review('output/llm_annotation/taxa_review') |>
  filter(!is.na(row), row != '') |>
  transmute(Study, review_row = row, paper_name, review_ncbi_ok = ncbi_ok, review_action = action,
            suggested_ncbi_taxid, suggested_ncbi_name, review_note = note)
inputs <- list.files('output/llm_annotation/taxa_review/input', full.names = TRUE) |>
  map_dfr(\(f) read_csv(f, col_types = cols(.default = 'c'), show_col_types = FALSE) |>
            mutate(Study = sub('_recorded_taxa[.]csv$', '', basename(f)))) |>
  transmute(Study, review_row = row, direction, taxid = ncbi_taxid)

rows <- sig_taxa |>
  left_join(inputs |> distinct(Study, direction, taxid, .keep_all = TRUE), by = c('Study', 'direction', 'taxid')) |>
  left_join(review, by = c('Study', 'review_row')) |>
  left_join(select(audit, taxid, ncbi_name, ncbi_rank, problems), by = 'taxid') |>
  mutate(paper_says_wrong = review_ncbi_ok == 'no') |>
  select(Study, review_status, direction, taxid, ncbi_name, ncbi_rank, problems, paper_name, review_ncbi_ok,
         suggested_ncbi_taxid, suggested_ncbi_name, review_action, review_note)

write_csv(rows, file.path(out_dir, 'signature_taxa_audit.csv'), na = '')

# ---- the same paper name mapped to different IDs ----

norm <- \(x) tolower(gsub('[^A-Za-z0-9 ]', '', gsub('\\s+', ' ', trimws(x))))
inconsistent <- rows |>
  filter(!is.na(paper_name), paper_name != '') |>
  mutate(name_key = norm(paper_name)) |>
  group_by(name_key) |>
  filter(n_distinct(taxid) > 1) |>
  summarise(paper_names = paste(unique(paper_name), collapse = ' | '),
            ids = paste(unique(paste0(taxid, ' (', ncbi_name, ')')), collapse = '; '),
            studies = paste(unique(Study), collapse = ', '), .groups = 'drop')
write_csv(inconsistent, file.path(out_dir, 'inconsistent_names.csv'), na = '')

# ---- summary ----

cat('IDs:', nrow(audit), '| with problems:', sum(!is.na(audit$problems)), '\n')
audit |> filter(!is.na(problems)) |> separate_rows(problems, sep = '; ') |>
  mutate(problems = sub(' (into|:).*', '', problems)) |> count(problems) |> print()
cat('signature taxa:', nrow(rows), '| ID with a problem:', sum(!is.na(rows$problems)),
    '| reviewed against the paper:', sum(!is.na(rows$review_ncbi_ok)),
    '| paper names another organism (ncbi_ok = no):', sum(rows$review_ncbi_ok %in% 'no'),
    '| mapped to a parent:', sum(rows$review_ncbi_ok %in% 'parent'), '\n')
cat('paper names mapped to more than one ID:', nrow(inconsistent), '\n')

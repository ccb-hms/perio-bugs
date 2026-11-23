# Apply the taxa review's corrections (output/llm_annotation/taxa_review/corrections.csv, written by
# scripts/llm_annotation/resolve_taxa_review.R) to the signatures' taxa, pending curator confirmation
# used by scripts/5-join_overview_and_microbe.R --corrected
#
# Only rows with status "applied" or "applied, awaiting curator approval" are used. Removed, moved and remapped taxa are taken out by study, direction
# and ID; their replacements and the added taxa are put in. Rows left for review change nothing.

apply_taxa_corrections <- function(diff_species, file = 'output/llm_annotation/taxa_review/corrections.csv') {
  dir_code <- c(increased = 'up', decreased = 'dn')
  changes <- read.csv(file, colClasses = 'character') |>
    dplyr::filter(grepl('^applied', status)) |>
    dplyr::mutate(Number = sub('^Study ', '', Study), from_dir = dir_code[direction], to_dir = dir_code[to_direction])

  replaced <- dplyr::filter(changes, action %in% c('remove', 'fix_direction', 'fix_ncbi', 'merged ID', 'placeholder'))
  key <- \(number, direction, taxid) paste(number, direction, taxid)

  # the review saw each taxon at its MetaPhlAn rank (create_metaphlan_hierarchy(): the nearest ancestor at a
  # standard rank, e.g. a subspecies as its species), so a correction matches the recorded ID or that one
  ranks <- c('domain', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species')
  as_ranked <- vapply(unique(diff_species$taxid), \(id) {
    l <- taxizedb::classification(id)[[1]]
    if (!is.data.frame(l)) return(id)
    l <- l[l$rank %in% ranks, ]
    if (nrow(l)) l$id[nrow(l)] else id
  }, character(1))
  recorded <- diff_species |> dplyr::mutate(ranked = unname(as_ranked[taxid]))
  replaced_keys <- key(replaced$Number, replaced$from_dir, replaced$from_taxid)
  found <- replaced_keys %in% c(key(recorded$Number, recorded$direction, recorded$taxid),
                                key(recorded$Number, recorded$direction, recorded$ranked))
  if (!all(found)) {
    warning(sum(!found), ' corrections refer to taxa no longer recorded (already made in the extraction); skipped')
    changes <- dplyr::anti_join(changes, replaced[!found, ], by = names(changes))
  }

  out <- recorded |>
    dplyr::filter(!key(Number, direction, taxid) %in% replaced_keys[found],
                  !key(Number, direction, ranked) %in% replaced_keys[found]) |>
    dplyr::select(-ranked) |>
    dplyr::bind_rows(changes |>
                       dplyr::filter(action != 'remove', !is.na(to_taxid), to_taxid != '') |>
                       dplyr::transmute(Number, taxid = to_taxid, direction = to_dir)) |>
    dplyr::distinct()

  message(sprintf('taxa corrections: %d applied; %d taxa before, %d after',
                  nrow(changes), nrow(diff_species), nrow(out)))
  out
}

library(readxl)
library(dplyr)

# one-time correction of study labels in output/diff_species.rds
#
# split_by_study() in 3-merge_microbe_classic.R (versions up to v0.2.2) named
# groups in order of appearance, but dplyr::group_split() returns them sorted by
# Number. For sheets not sorted by Number ("New DATABASE" and "Sarah's Work (2)")
# taxa were labelled with the wrong study (e.g. taxa for study 2010 labelled 811).
#
# split_by_study() is now fixed. This script relabels the existing results so that
# the interactive taxon matching in 3-merge_microbe_classic.R doesn't need to be
# re-run. Not needed if 3-merge_microbe_classic.R is re-run with the fixed function.

diff_species_file <- 'output/diff_species.rds'
diff_species <- readRDS(diff_species_file)

# check that labels are in the incorrect state
# e.g. study 2010 only reports Dialister pneumosintes (taxid 39950)
dialister_studies <- diff_species$Number[diff_species$taxid == '39950']
if ('2010' %in% dialister_studies || !'811' %in% dialister_studies) {
  stop('Study labels in ', diff_species_file, ' appear to be already correct.')
}

# label assigned by split_by_study -> correct study number ----

microbe_file <- 'data/Cleaned Micro List RY_final.xlsx'

get_label_map <- function(numbers) {
  numbers <- na.omit(numbers)
  tibble(
    # order of appearance (used to name groups)
    label = as.character(unique(numbers)),
    # order from group_split (sorted)
    Number_correct = as.character(sort(unique(numbers)))
  )
}

# Number columns as used by process_database and process_sarahs_db2
old_numbers <- read_excel(microbe_file, sheet = 'Old DATABASE')$...1[-1]
new_numbers <- read_excel(microbe_file, sheet = 'New DATABASE')$...1[-1]
sarah2_numbers <- read_excel(microbe_file, sheet = "Sarah's Work (2)")$Number[-1]

label_map <- bind_rows(
  get_label_map(new_numbers),
  get_label_map(sarah2_numbers)
)

# Old DATABASE is sorted so labels are correct
stopifnot(identical(get_label_map(old_numbers)$label, get_label_map(old_numbers)$Number_correct))

# labels are unique across sheets so mapping is unambiguous
stopifnot(!anyDuplicated(label_map$label))
stopifnot(!any(label_map$label %in% as.character(old_numbers)))

# relabel ----

diff_species_fixed <- diff_species |>
  left_join(label_map, by = c('Number' = 'label')) |>
  mutate(Number = coalesce(Number_correct, Number)) |>
  select(-Number_correct)

stopifnot(nrow(diff_species_fixed) == nrow(diff_species))

# study 2010 (Ferraro et al. 2007) only reports Dialister pneumosintes
stopifnot(identical(diff_species_fixed$taxid[diff_species_fixed$Number == '2010'], '39950'))

saveRDS(diff_species_fixed, diff_species_file)

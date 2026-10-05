# helper functions for matching curated taxon names to NCBI Taxonomy IDs
# used by 3-merge_microbe_classic.R and 3b-merge_microbe_new_data.R

library(dplyr)
library(rols)
library(stringr)

# used to split into data.frame per study
split_by_study <- function(db) {

  groups <- db |>
    tidyr::fill(Number, .direction = "down") |>
    dplyr::group_split(Number)

  # name each group by its own study number
  # NOTE: group_split sorts groups by Number, so can't use order of appearance
  study_nums <- sapply(groups, function(group) as.character(group$Number[1]))

  groups |>
    purrr::map(~ dplyr::select(.x, -Number)) |>
    purrr::map(~ dplyr::filter(.x, !if_all(dplyr::everything(), is.na))) |>
    setNames(study_nums)
}

# for processing "Old DATABASE", "New DATABASE", and "Sarah's Work"
process_database <- function(db) {
  
  # remove reference column and empty column
  db$...11 <- db$...2 <- NULL
  
  # indices for columns that are elevated in health/perio
  elevated_in_health_cols <- 2:9
  elevated_in_perio_cols <- 10:17
  
  # split into up/down regulated, keeping study number
  db_up <- db[, c(1, elevated_in_perio_cols)]
  db_dn <- db[, c(1, elevated_in_health_cols)]
  
  # rename using first row
  colnames(db_up) <- db_up[1, ]
  db_up <- db_up[-1, ]
  
  colnames(db_dn) <- db_dn[1, ]
  db_dn <- db_dn[-1, ]
  
  # split by study number
  db_up <- split_by_study(db_up)
  db_dn <- split_by_study(db_dn)
  
  return(list(
    db_up = db_up,
    db_dn = db_dn
  ))
}

# NOTE: sarahs_db2 has some taxonomy ids
# for processing "Sarah's Work (2)"
process_sarahs_db2 <- function(db) {
  db$...15 <- db$Reference <- NULL
  
  # indices for columns that are elevated in health/perio
  elevated_in_health_cols <- 2:13
  elevated_in_perio_cols <- 14:25
  
  # drop first row (header info e.g. "elevated in ...")
  db <- db[-1, ]
  
  # split into up/down regulated, keeping study number
  db_up <- db[, c(1, elevated_in_perio_cols)]
  db_dn <- db[, c(1, elevated_in_health_cols)]
  
  # clean up column names
  colnames(db_up) <- gsub('[.]{3}\\d+$', '', colnames(db_up))
  colnames(db_dn) <- gsub('[.]{3}\\d+$', '', colnames(db_dn))
  
  # split by study number
  db_up <- split_by_study(db_up)
  db_dn <- split_by_study(db_dn)
  
  return(list(
    db_up = db_up,
    db_dn = db_dn
  ))
  
}

# get most specific name from df of differentially abundant species
most_specific_name_base <- function(df, exclude_species = FALSE) {
  specific_cols <- c("Genus", "Family", "Order", "Class", "Phylum", "Kingdom", "Domain")
  
  apply(df, 1, function(row) {
    g <- row["Genus"]
    s <- ifelse(exclude_species, NA, row["Species"])
    
    if(!is.na(g) && g != "" && !is.na(s) && s != "") {
      paste(g, s)
    } else {
      vals <- row[specific_cols]
      vals <- vals[vals != "" & !is.na(vals)]
      vals[1]
    }
  })
}

# determine tree distance between two taxon ids
taxonomic_tree_distance <- function(taxid1, taxid2) {
  # Retrieve lineages
  cl1 <- taxizedb::classification(taxid1, db = "ncbi")[[1]]
  cl2 <- taxizedb::classification(taxid2, db = "ncbi")[[1]]
  
  # taxid not found
  if (length(cl1) == 1 && is.na(cl1)) return(NA)
  if (length(cl2) == 1 && is.na(cl2)) return(NA)
  
  # Remove nodes with 'no rank'
  cl1 <- cl1[cl1$rank != "no rank", , drop = FALSE]
  cl2 <- cl2[cl2$rank != "no rank", , drop = FALSE]
  
  # Find where the lineages diverge
  min_length <- min(nrow(cl1), nrow(cl2))
  split_index <- 0
  for (i in seq_len(min_length)) {
    if (cl1$name[i] != cl2$name[i]) {
      split_index <- i - 1
      break
    }
    if (i == min_length) split_index <- min_length
  }
  
  steps1 <- nrow(cl1) - split_index
  steps2 <- nrow(cl2) - split_index
  distance <- steps1 + steps2
  mrca <- if (split_index > 0) cl1$name[split_index] else NA
  
  return(distance)
}

# cleanup taxon names
clean_names <- function(taxon_names) {
  cleaned_names <- gsub('_ot', 'oral taxon', taxon_names)
  cleaned_names <- gsub(' OT ', ' oral taxon ', cleaned_names)
  cleaned_names <- gsub(' sp(\\.)? ', ' sp. ', cleaned_names)
  cleaned_names <- gsub("\\s*\\[.*?\\]\\s*", " ", cleaned_names)
  
  
  
  # specific cases
  cleaned_names <- gsub("Peptostreptococcaceae XI.+?$", "Peptostreptococcaceae", cleaned_names)
  cleaned_names[grepl('Fretibacterium fastidiosum', cleaned_names)] <- 'Fretibacterium fastidiosum'
  cleaned_names <- gsub('^Synergistetes OTU .+?$', 'Synergistetes', cleaned_names)
  
  cleaned_names <- gsub("\\s{2,}", " ", cleaned_names)
  cleaned_names <- trimws(cleaned_names)
  return(cleaned_names)
}

# query NCBI Taxon using OLS API
query_ncbitaxon <- function(taxon_name) {
  
  # get top result for
  res <- rols::OlsSearch(q = taxon_name, ontology = "ncbitaxon", rows = 1)
  res <- rols::olsSearch(res)
  res <- as(res, 'data.frame') |> 
    select(label, obo_id) |> 
    mutate(query = taxon_name,
           taxid = gsub('^NCBITaxon:', '', obo_id),
           taxname = label) |> 
    select(-obo_id, -label, query, taxid, taxname)
  
  return(res)
}

# run data.frame of queries
run_ncbitaxon_queries <- function(queries) {
  
  rols_results <- list()
  
  for (i in seq_len(nrow(queries))) {
    next_most_specific <- queries$next_most_specific[i]
    most_specific <- queries$most_specific[i]
    
    cat('Working on:', most_specific, '\n')
    res <- query_ncbitaxon(most_specific)
    res$next_most_specific <- next_most_specific
    
    rols_results[[i]] <- res
  }
  do.call(rbind, rols_results)
}


# get shortest taxonomic tree distance for ambiguous results
get_shortest_tdist <- function(taxid, genus) {
  if (grepl('TM7', genus)) genus <- 'Candidatus Saccharimonadota'
  
  # taxids
  taxids <- taxizedb::name2taxid(genus, out_type = 'summary')
  if (!nrow(taxids)) return(NA)
  
  tdists <- sapply(
    taxids$id,
    function(taxid2) taxonomic_tree_distance(taxid, taxid2)
  )
  
  return(min(tdists))
}

# convert list of lists into a single data.frame
rbind_taxdbs <- function(db) {
  study_nums <- names(db$db_up)
  for (study_num in study_nums) {
    # add study number to each table
    db$db_up[[study_num]]$Number <- study_num
    db$db_dn[[study_num]]$Number <- study_num
    
    # add direction to each table
    db$db_up[[study_num]]$direction <- "up"
    db$db_dn[[study_num]]$direction <- "dn"
  }
  
  do.call(rbind, c(db$db_up, db$db_dn))
}

# use taxizedb to get rank values
get_rank_vals <- function(results, rank_name = 'domain') {
  hierarchies <- taxizedb::classification(results$taxid, db = "ncbi")
  
  rank_vals <- lapply(hierarchies, function(df) 
    df |> filter(rank == rank_name) |> pull(name) |> first()) |> 
    unlist(use.names = FALSE)
  
  rank_vals[is.na(rank_vals)] <- ''
  
  return(rank_vals)
}

add_tree_dist_genus <- function(results, taxid_name = 'taxid') {
  
  tree_dists <- c()
  for (i in seq_len(nrow(results))) {
    taxid <- results[[taxid_name]][i]
    next_most_specific <- results$next_most_specific[i]
    tree_dist <- get_shortest_tdist(taxid, next_most_specific)
    tree_dists <- c(tree_dists, tree_dist)
  }
  
  results$tree_dist_genus <- tree_dists
  return(results)
}

expand_oral_taxon <- function(x) {
  sapply(x, function(each) {
    if (!grepl("oral taxon ", each)) return(each)
    # Get prefix
    m <- regexpr(".*oral taxon", each)
    prefix <- regmatches(each, m)
    # Split on spaces and slashes
    nums <- unlist(strsplit(sub(".*oral taxon ", "", each), "[ /]+"))
    paste(paste(prefix, nums), collapse = ";")
  }, USE.NAMES = FALSE)
}

expand_genus_suffix_oral_taxon <- function(x) {
  sapply(x, function(each) {
    # Look for pattern: "oral taxon N <suffix>. oral taxon M"
    match <- regexpr("oral taxon \\w+ (subsp\\.|sp\\.) oral taxon \\w+", each)
    if (match != -1) {
      genus <- strsplit(each, " ")[[1]][1]
      # Up to (but not including) the suffix (.e.g "subsp.")
      part1 <- sub(" (subsp\\.|sp\\.).*", "", each)
      # The 'suffix. oral taxon M' ('subsp. oral taxon M', etc.)
      part2 <- regmatches(each, regexpr("(subsp\\.|sp\\.) oral taxon \\w+", each))
      paste0(part1, ";", genus, " ", part2)
    } else {
      each
    }
  }, USE.NAMES = FALSE)
}

expand_double_species_oral_taxon <- function(x) {
  sapply(x, function(each) {
    # Detect pattern: two 'oral taxon' phrases separated by a word in between
    pattern <- "^([A-Za-z]+) ([a-z]+) oral taxon ([\\w]+) ([a-z]+) oral taxon ([\\w]+)"
    if (grepl(pattern, each, perl=TRUE)) {
      m <- regmatches(each, regexec(pattern, each, perl=TRUE))[[1]]
      genus <- m[2]
      species1 <- m[3]
      ot1 <- m[4]
      species2 <- m[5]
      ot2 <- m[6]
      paste0(
        genus, " ", species1, " oral taxon ", ot1, ";",
        genus, " ", species2, " oral taxon ", ot2
      )
    } else {
      each
    }
  }, USE.NAMES = FALSE)
}

clean_names_sarah <- function(x) {
  # Remove all [ ... ]
  x <- gsub("\\[[^]]*\\]", "", x)
  
  # Remove space, open parenthesis, anything, close parenthesis at end of string
  x <- gsub(" \\(.+\\)$", "", x)
  
  # Remove "V1-2" or "V1 2" (optionally surrounded by whitespace)
  x <- gsub("\\s*V1[ -]2\\s*", " ", x)
  
  # replace OT, HOT, and -HMT with oral taxon
  x <- gsub("\\bOT ([0-9]+)\\b", "oral taxon \\1", x)
  x <- gsub("\\bHOT ([0-9]+)\\b", "oral taxon \\1", x)
  x <- gsub("\\bHOT([0-9]+)", "oral taxon \\1", x)
  x <- gsub("HOT\\.", "oral taxon ", x)
  x <- gsub("HMT-([0-9]+)", "oral taxon \\1", x)
  
  # This regex handles "_ot" with one or more numbers, possibly separated by underscores
  x <- gsub("_ot", " oral taxon ", x)
  x <- gsub("_", " ", x)
  
  # matches one or more repeats of the same "oral taxon nnn"
  x <- gsub("(oral taxon \\d+)( \\1)+", "\\1", x)
  
  # remove G1-9 group identifier
  x <- gsub("\\bG[1-9]\\b", "", x)
  
  # remove clone identifier eg. "/AC39" from "724/AC39"
  x <- gsub("/[A-Za-z][A-Za-z0-9]*", "", x)
  
  # ensure sp has period
  x <- gsub("\\bsp(?!\\.)\\b", "sp.", x, perl = TRUE)
  
  # Remove "Cluster" with roman numerals
  x <- gsub(" Cluster [I]+", "", x)
  # Remove "Cluster" plus a number (e.g., Cluster1)
  x <- gsub(" Cluster[0-9]+", "", x)
  # Remove "Cluster" slash number (e.g., Cluster/096)
  x <- gsub(" Cluster/[0-9]+", "", x)
  
  # replace ssp. or ss. or spp. with subsp.
  x <- gsub(" ssp\\.", " subsp.", x)
  x <- gsub(" spp\\.", " subsp.", x)
  x <- gsub(" ss\\.", " subsp.", x)
  
  # Fix rare Elusimicrobium "oral taxon 673 4/5" strain cases
  x <- gsub("oral taxon 673 [45]$", "oral taxon 673", x)
  
  # remove sp. NUM
  x <- gsub("sp\\. \\d+", "", x)
  
  # some specific edge cases
  x <- gsub("Clostridiates", "Clostridiales", x)
  x <- gsub("TMZ", "TM7", x)
  x <- gsub("\\bG 2\\b", "", x)
  x <- gsub("\\bF2\\b", "", x)
  x <- gsub("\\bXI\\b", "", x)
  
  # Collapse any runs of multiple spaces to single spaces, then trim
  x <- gsub(" +", " ", x)
  x <- trimws(x)
  
  return(x)
}

get_exact_taxid <- function(taxname) {
  tbl <- taxizedb::name2taxid(taxname, out_type = 'summary')
  if (nrow(tbl) != 1) return (NA)
  tbl$id
}

# add taxizedb taxids for query and genus
add_exact_taxids <- function(results) {
  # exact taxids for query
  results$taxid_exact <- sapply(results$query, get_exact_taxid, USE.NAMES = FALSE)
  
  # exact taxids for genus
  genus <- results$next_most_specific
  genus[grepl('TM7', genus)] <- 'Candidatus Saccharimonadota'
  
  results$taxid_genus_exact <- sapply(genus, get_exact_taxid, USE.NAMES = FALSE)
  return(results)
}

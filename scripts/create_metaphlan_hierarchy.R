create_metaphlan_hierarchy <- function(taxon_ids_vector) {
  
  # Mapping of taxonomic ranks to MetaPhlAn prefixes
  rank_prefix_map <- c(
    "domain" = "k__",
    "kingdom" = "k__",
    "phylum" = "p__",
    "class" = "c__",
    "order" = "o__",
    "family" = "f__",
    "genus" = "g__",
    "species" = "s__"
  )
  
  result_ncbi <- list()
  result_metaphlan <- list()
  
  for (i in seq_along(taxon_ids_vector)) {
    # Split comma-separated IDs and trim whitespace
    ids <- trimws(strsplit(taxon_ids_vector[i], ",")[[1]])
    
    ncbi_hierarchies <- character()
    metaphlan_hierarchies <- character()
    
    for (taxon_id in ids) {
      tryCatch({
        # Get classification from taxizedb
        classif <- taxizedb::classification(taxon_id)[[1]]
        
        if (!is.null(classif) && !inherits(classif, "character") && nrow(classif) > 0) {
          # Filter for ranks we care about for MetaPhlAn
          relevant_rows <- classif[classif$rank %in% names(rank_prefix_map), ]
          
          if (nrow(relevant_rows) > 0) {
            # Create NCBI hierarchy (pipe-separated IDs)
            ncbi_hier <- paste(relevant_rows$id, collapse = "|")
            
            # Create MetaPhlAn hierarchy (pipe-separated prefixed names)
            metaphlan_parts <- paste0(
              rank_prefix_map[relevant_rows$rank],
              relevant_rows$name
            )
            metaphlan_hier <- paste(metaphlan_parts, collapse = "|")
            
            ncbi_hierarchies <- c(ncbi_hierarchies, ncbi_hier)
            metaphlan_hierarchies <- c(metaphlan_hierarchies, metaphlan_hier)
          }
        }
      }, error = function(e) {
        message(paste("Error processing taxon ID", taxon_id, ":", e$message))
      })
    }
    
    # Store as list elements (can be empty if no valid hierarchies)
    result_ncbi[[i]] <- ncbi_hierarchies
    result_metaphlan[[i]] <- metaphlan_hierarchies
  }
  
  # Create data frame with list columns
  result_df <- data.frame(
    `MetaPhlAn taxon names` = I(result_metaphlan),
    `NCBI Taxonomy IDs` = I(result_ncbi),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  return(result_df)
}
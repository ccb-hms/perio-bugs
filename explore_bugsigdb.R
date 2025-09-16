library(bugsigdbr)
library(rentrez)

# get study where condition is periodontitis
df <- importBugSigDB()
perio_df <- subset(df, Condition == 'Periodontitis')

# get list of all columns and number of unique values
cols <- colnames(df)
for (col in cols) {
  cat('Column: ', col, ', Num unique:', length(unique(df[[col]])), '\n', sep='')
}

perio_df$`MetaPhlAn taxon names`[[1]]
perio_df$`NCBI Taxonomy IDs`[[1]]

# remove list columns as can't save
perio_df$`MetaPhlAn taxon names` <- perio_df$`NCBI Taxonomy IDs` <- NULL
write.csv(perio_df, 'bugsigdb_periodontits.csv')

pmid <- "23613868"
record <- entrez_summary(db = "pubmed", id = pmid)
record$title
record$authors
record$elocationid

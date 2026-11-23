# Study 1411 (Zhou 2013, PMID 23613868): the paper's Table 1 against BugSigDB, the curators' data and PerioSigDB
# after the taxa review, with what the review changed. The first four columns are output/PMID23613868.xlsx.
#
# usage: Rscript scripts/llm_annotation/study_1411_table.R
# writes output/llm_annotation/taxa_review/study_1411_comparison.png

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

# status of each source against Table 1: ok, missing, wrong (wrong rank or name)
rows <- tribble(
  ~group, ~otu, ~paper, ~bugsigdb, ~bsdb_status, ~feres, ~feres_status, ~now, ~change,
  "health", "OTU0003", "Corynebacterium matruchotii", "Corynebacterium matruchotii", "ok", "Corynebacterium matruchotii", "ok",
  "Corynebacterium matruchotii", "None",
  "health", "OTU0016", "family of Propionibacteriaceae", "Propionibacteriaceae", "ok", "", "missing",
  "Propionibacteriaceae", "Added (OTU named only to family). Awaiting curator approval",
  "health", "OTU0020", "Prevotella loescheii", "Hoylesella loescheii", "ok", "Prevotella loescheii", "ok",
  "Hoylesella loescheii", "None (Hoylesella loescheii is the current NCBI name)",
  "health", "OTU0025", "genus of Aggregatibacter", "Aggregatibacter", "ok", "Aggregatibacter", "ok",
  "Aggregatibacter", "None",
  "health", "OTU0026", "Selenomonas noxia", "Selenomonas noxia", "ok", "", "missing",
  "Selenomonas noxia", "Added (NCBI calls it Centipeda noxia, 135083)",
  "health", "OTU0088", "Cardiobacterium hominis", "Cardiobacterium hominis", "ok", "Cardiobacterium hominis", "ok",
  "Cardiobacterium hominis", "None",
  "health", "OTU0094", "Neisseria flavescens", "Neisseria flavescens", "ok", "Neisseria flavescens", "ok",
  "Neisseria flavescens", "None",
  "health", "OTU0096", "Cardiobacterium valvarum", "Cardiobacterium valvarum", "ok", "Cardiobacterium valvarum", "ok",
  "Cardiobacterium valvarum", "None",
  "health", "OTU0102", "family of Leptotrichiaceae", "Leptotrichiaceae", "ok", "Leptotrichia", "wrong",
  "Leptotrichiaceae", "Added the family (OTU named only to family). Awaiting curator approval",
  "health", "OTU0127", "genus of Leptotrichia", "Leptotrichia", "ok", "Leptotrichia", "ok",
  "Leptotrichia", "Mapped 'Leptotrichia sp.' (104608) to the genus (32067)",
  "health", "OTU0161", "Capnocytophaga sputigena", "Capnocytophaga sputigena", "ok", "Capnocytophaga sputigena", "ok",
  "Capnocytophaga sputigena", "None",
  "health", "OTU2004", "Porphyromonas sp.", "Porphyromonas sp.", "ok", "Porphyromonas sp.", "ok",
  "Porphyromonas", "Mapped 'Porphyromonas sp.' to the genus (836). Kept Table 1 over Table S2's P. catoniae; awaiting curator approval",
  "health", "OTU0052", "Streptococcus infantis", "", "missing", "", "missing",
  "Streptococcus infantis", "Added (Table 1; Table S2 gives S. mitis)",
  "health", "OTU2002", "Streptococcus gordonii", "", "missing", "", "missing",
  "Streptococcus gordonii", "Added",
  "health", "OTU2005", "Actinomyces naeslundii", "", "missing", "", "missing",
  "Actinomyces naeslundii", "Added",
  "perio", "OTU2001", "Selenomonas sputigena", "Selenomonas sputigena", "ok", "Selenomonas sputigena", "ok",
  "Selenomonas sputigena", "Added. In 'Sarah's Work' only, so PerioSigDB had no increased signature for this study",
  "perio", "OTU0507", "family of Prevotellaceae", "Prevotellaceae", "ok", "", "missing",
  "Prevotellaceae", "Added (one entry for OTU0507 and OTU0343). Awaiting curator approval",
  "perio", "OTU0011", "Treponema denticola", "", "missing", "", "missing",
  "Treponema denticola", "Added",
  "perio", "OTU2006", "Prevotella intermedia", "", "missing", "", "missing",
  "Prevotella intermedia", "Added",
  "perio", "OTU0343", "family of Prevotellaceae", "", "missing", "", "missing",
  "Prevotellaceae", "Added (same entry as OTU0507). Awaiting curator approval"
)

COLS <- c(otu = "OTU", paper = "PMID23613868 Table 1", bugsigdb = "BugSigDB", feres = "Feres data",
          now = "PerioSigDB now", change = "What the review changed")
WIDTHS <- c(otu = 0.75, paper = 2.1, bugsigdb = 1.9, feres = 1.9, now = 1.9, change = 4.2)
WRAP <- c(otu = 10, paper = 30, bugsigdb = 28, feres = 28, now = 28, change = 58)
FILL <- c(ok = "#e7f3e9", missing = "#f9d5d3", wrong = "#fbe7c6", none = "white", changed = "#e3ecf8",
          header = "grey20", group = "grey90")

# one block per group: a group title row, the header row, then the OTUs
blocks <- rows |>
  mutate(now_status = "none",
         change_status = if_else(change == "None" | str_starts(change, "None "), "none", "changed"),
         paper_status = "none", otu_status = "none") |>
  mutate(id = row_number())
group_titles <- c(health = "Health-associated OTUs (decreased in periodontitis)",
                  perio = "Periodontitis-associated OTUs (increased in periodontitis)")

lines <- list(); y <- 0
for (g in names(group_titles)) {
  y <- y + 1
  lines[[length(lines) + 1]] <- tibble(y = y, col = "otu", text = group_titles[[g]], fill = "group", kind = "group")
  y <- y + 1
  lines[[length(lines) + 1]] <- tibble(y = y, col = names(COLS), text = unname(COLS), fill = "header", kind = "header")
  for (i in which(blocks$group == g)) {
    r <- blocks[i, ]
    y <- y + 1
    lines[[length(lines) + 1]] <- tibble(
      y = y, col = names(COLS),
      text = c(r$otu, r$paper, if (r$bugsigdb == "") "(missing)" else r$bugsigdb,
               if (r$feres == "") "(missing)" else r$feres, r$now, r$change),
      fill = c("none", "none", r$bsdb_status, r$feres_status, "none", r$change_status), kind = "cell")
  }
}
cells <- bind_rows(lines)

x_right <- cumsum(WIDTHS); x_left <- x_right - WIDTHS
cells <- cells |>
  mutate(x0 = unname(x_left[col]), x1 = unname(x_right[col]),
         x1 = if_else(kind == "group", max(x_right), x1),
         label = if_else(kind == "group", text, purrr::map2_chr(text, unname(WRAP[col]), str_wrap)),
         lines = str_count(label, "\n") + 1)
# row heights follow the longest wrapped cell
heights <- cells |> summarise(h = max(lines) * 0.34 + 0.22, .by = y) |> arrange(y) |>
  mutate(top = -lag(cumsum(h), default = 0), bottom = top - h)
cells <- left_join(cells, heights, by = "y")

p <- ggplot(cells) +
  geom_rect(aes(xmin = x0, xmax = x1, ymin = bottom, ymax = top, fill = fill), colour = "white", linewidth = 0.6) +
  geom_text(aes(x = x0 + 0.06, y = (top + bottom) / 2, label = label,
                colour = kind == "header", fontface = ifelse(kind == "cell", "plain", "bold")),
            hjust = 0, vjust = 0.5, size = 2.6, lineheight = 0.95) +
  scale_fill_manual(values = FILL, guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"), guide = "none") +
  coord_cartesian(expand = FALSE) +
  labs(title = "Study 1411 (Zhou et al. 2013, PMID 23613868): Table 1 against BugSigDB, the Feres data and PerioSigDB",
       caption = paste("Green: matches Table 1. Red: missing. Amber: wrong rank (genus recorded for a family-level OTU).",
                       "Blue: the review changed PerioSigDB.",
                       "\"Awaiting curator approval\": applied in the corrected build, listed in corrections_for_curators.xlsx.")) +
  theme_void(base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 9, margin = margin(b = 6)),
        plot.caption = element_text(hjust = 0, size = 7, colour = "grey30", margin = margin(t = 6)),
        plot.margin = margin(8, 8, 8, 8), plot.background = element_rect(fill = "white", colour = NA))

out <- "output/llm_annotation/taxa_review/study_1411_comparison.png"
ggsave(out, p, width = sum(WIDTHS) * 0.98, height = -min(heights$bottom) * 0.33 + 0.8, dpi = 200, bg = "white")
message("wrote ", out)

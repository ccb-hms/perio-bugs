# Shared style, colours and helpers for the PerioSigDB paper figures (R version).
#
# Every figure script sources this file. The data come from output/figure_data/, which
# reports/metadata_options/report.qmd writes on every render. Colours follow the report.

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(ggbeeswarm)
  library(ggtext)
  library(grid)
})

# --- paths ------------------------------------------------------------------------------------------

# the repository root: the first directory upwards from here that holds output/figure_data
fig_root <- local({
  d <- normalizePath(".")
  while (!dir.exists(file.path(d, "output", "figure_data")) && d != dirname(d)) d <- dirname(d)
  d
})
# PERIO_P05=1: the check with p < 0.05 taxa sets (output/figure_data_p05, drawn to figures/output_p05)
suffix <- if (Sys.getenv("PERIO_P05") == "1") "_p05" else ""
DATA <- file.path(fig_root, "output", paste0("figure_data", suffix))
OUT <- file.path(fig_root, "figures", paste0("output", suffix))
FIG_DIR <- file.path(fig_root, "figures")

load_fig <- function(name) read_csv(file.path(DATA, paste0(name, ".csv")), show_col_types = FALSE)

# --- sizes (Nature: 89 mm single column, 183 mm double column) ---------------------------------------

SINGLE <- 89
DOUBLE <- 183

# --- colours (as in the report) ------------------------------------------------------------------------

INC <- "#eb6834"       # increased in periodontitis / periodontitis group
DEC <- "#2a78d6"       # decreased in periodontitis / health group
INC_DARK <- "#c2410c"
DEC_DARK <- "#1c5cab"
NEUTRAL <- "#f0efec"
FRAME <- "grey75"      # panel borders
GRID <- "grey90"       # gridlines
TEXT_GREY <- "grey35"
pal_group <- c(Health = DEC, Periodontitis = INC)
pal_direction <- c(increased = INC, decreased = DEC)

METHODS <- c("16S rRNA sequencing", "Shotgun metagenomics", "DNA-DNA hybridization", "PCR", "Other / not reported")
pal_method <- setNames(c("#2a78d6", "#eb6834", "#008300", "#d55181", "grey65"), METHODS)
METHOD_SHORT <- setNames(c("16S rRNA", "Shotgun", "DNA–DNA hybridization", "PCR", "Other / not reported"), METHODS)

RANKS <- c("Species", "Genus", "Family", "Higher")
pal_rank <- setNames(c("#184f95", "#3987e5", "#86b6ef", "#cde2fb"), RANKS)

# body site groups of BugSigDB study units (the report's names and their short labels)
SITE_NAMES <- c(oral = "mouth", gastrointestinal = "gut", "upper airway" = "airway", "lower airway" = "lung",
                urogenital = "urogenital", skin = "skin", eye = "eye", other = "other", mixed = "mixed sites")
pal_site <- c(mouth = "#1b9e77", gut = "#a6761d", airway = "#7570b3", lung = "#e7298a", urogenital = "#66a61e",
              skin = "#e6ab02", eye = "#1f78b4", other = "grey55", "mixed sites" = "grey30",
              "mixed (no site ≥ 2/3)" = "grey60")

# share reported increased: blue (all decreased) to orange (all increased)
scale_fill_direction <- function(...) {
  scale_fill_gradient2(low = DEC_DARK, mid = NEUTRAL, high = INC_DARK, midpoint = 0.5, limits = c(0, 1),
                       na.value = "white", ...)
}

ABBREVIATIONS <- c("Alzheimer's disease" = "AD", "Autism spectrum disorder" = "ASD", "Colorectal cancer" = "CRC",
                   "Dry eye syndrome" = "Dry eye", "Gastric cancer" = "GC", "Head and neck cancer" = "HNC",
                   "HIV infection" = "HIV", "Parkinson's disease" = "PD", "Rheumatoid arthritis" = "RA",
                   "Schizophrenia" = "SCZ", "Smoking behavior" = "Smoking", "Type I diabetes mellitus" = "T1D",
                   "Type II diabetes mellitus" = "T2D", "Chronic obstructive pulmonary disease" = "COPD",
                   "Human papilloma virus infection" = "HPV",
                   "Ulcerative colitis" = "UC", "Unipolar depression" = "MDD")
abbrev <- function(x) coalesce(unname(ABBREVIATIONS[x]), x)

SIGNIFICANCE_FILL <- c("FDR < 0.05" = "grey10", "p < 0.05" = "grey60", "Not significant" = "white")

# --- theme ----------------------------------------------------------------------------------------------

# the report's theme_bw look at print size: 7 pt text, light grey frame and gridlines
theme_fig <- function(base_size = 7) {
  theme_bw(base_size = base_size, base_family = "Arial") +
    theme(
      panel.border = element_rect(colour = FRAME, fill = NA, linewidth = 0.4),
      panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "grey40", linewidth = 0.3),
      axis.text = element_text(colour = "grey15", size = base_size - 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0, size = base_size),
      plot.title = element_text(face = "bold", size = base_size, hjust = 0),
      plot.title.position = "panel",
      plot.tag = element_text(face = "bold", size = 9),
      legend.position = "top",
      legend.title = element_text(size = base_size - 0.5),
      legend.text = element_text(size = base_size - 1),
      legend.key.size = unit(7, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(3, "pt"),
      plot.margin = margin(4, 4, 4, 4)
    )
}
theme_set(theme_fig())

# --- saving ---------------------------------------------------------------------------------------------

#' Vector PDF (fonts embedded) and a 600 dpi PNG, width and height in mm.
save_fig <- function(p, name, height, width = DOUBLE) {
  dir.create(OUT, showWarnings = FALSE)
  ggsave(file.path(OUT, paste0(name, ".pdf")), p, width = width, height = height, units = "mm", device = cairo_pdf)
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = width, height = height, units = "mm", dpi = 600,
         device = ragg::agg_png, bg = "white")
  message(sprintf("wrote %s.pdf and .png (%.0f x %.0f mm)", file.path(OUT, name), width, height))
}

# --- taxon names ------------------------------------------------------------------------------------------

# words of a taxon name set in roman type: placeholders and descriptors, not Latin names
ROMAN <- c("sp.", "oral", "taxon", "bacterium", "genomosp.", "uncultured", "group", "clade", "HMT")

#' Markdown label (for ggtext's element_markdown / geom_richtext): Latin names in italics, placeholders roman.
taxon_md <- function(x) {
  vapply(as.character(x), function(name) {
    words <- strsplit(name, " ", fixed = TRUE)[[1]]
    roman <- words %in% ROMAN | grepl("[0-9]", words) | startsWith(words, "(")
    out <- character(0)
    run <- character(0)
    for (i in seq_along(words)) {
      if (roman[i]) {
        if (length(run)) out <- c(out, paste0("*", paste(run, collapse = " "), "*"))
        run <- character(0)
        out <- c(out, words[i])
      } else {
        run <- c(run, words[i])
      }
    }
    if (length(run)) out <- c(out, paste0("*", paste(run, collapse = " "), "*"))
    paste(out, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

#' Porphyromonas gingivalis -> P. gingivalis (genus-only names unchanged)
short_taxon <- function(x) {
  parts <- strsplit(as.character(x), " ", fixed = TRUE)
  vapply(parts, function(p) {
    if (length(p) >= 2 && grepl("^[A-Z]", p[1]) && grepl("^[a-z]", p[2])) {
      paste0(substr(p[1], 1, 1), ". ", paste(p[-1], collapse = " "))
    } else paste(p, collapse = " ")
  }, character(1))
}

# --- whole-signature similarity panels (Figures 4A, 5B and S4) -------------------------------------------

COMPARISONS <- tibble(perio = c("increased", "decreased", "increased", "decreased"),
                      direction = c("increased", "decreased", "decreased", "increased"))
# x axis: a study unit's rank is the share of other conditions' study units (same direction) that are less
# similar to the consensus, so 50% is the median of other conditions
SIMILARITY_LABEL <- "Similarity percentile among other conditions' studies (%)"

#' y positions for the chosen rows, top to bottom, with a gap between conditions.
#' rows: Condition, kind ("all", "site", "site_all"), site (report site group name or "").
similarity_layout <- function(rows, gap = 0.6) {
  rows |>
    mutate(site = coalesce(site, ""),
           group = cumsum(Condition != lag(Condition, default = "")) - 1,
           pos = -(row_number() - 1 + group * gap),
           # short labels: common abbreviations for conditions (spelled out in the captions)
           label = case_when(kind == "all" ~ abbrev(Condition),
                             kind == "site_all" ~ paste0(SITE_NAMES[site], " studies (vs all)"),
                             .default = paste0(SITE_NAMES[site], " studies (vs ", SITE_NAMES[site], ")")))
}

#' One plot per comparison (periodontitis consensus, condition direction), side by side: study-unit ranks as
#' points coloured by body site, and the mean with its 95% CI filled by significance. Opposite-direction
#' comparisons get a grey background. Each panel is titled with its two directions; the periodontitis and
#' condition labels ("Periodontitis:", "Condition:") name the two title lines once, left of the first panel, above a thin rule that runs
#' across all panels. Units without a test (fewer than 5 in the set) appear as points only.
#' Returns a patchwork of four plots (legend collected).
similarity_panels <- function(rows, stats, units, condition = "Condition", perio_name = "Periodontitis",
                              point_size = 0.5, label_bold = NULL,
                              title_size = 6.5, seed = 1) {
  keys <- c("Condition", "kind", "site")
  rows <- mutate(rows, site = coalesce(site, ""))
  st <- stats |> mutate(site = coalesce(site, "")) |> inner_join(select(rows, all_of(keys), pos), by = keys) |>
    mutate(significance = factor(case_when(fdr < 0.05 ~ "FDR < 0.05", p < 0.05 ~ "p < 0.05",
                                           .default = "Not significant"), levels = names(SIGNIFICANCE_FILL)))
  un <- units |> mutate(site = coalesce(site, "")) |> inner_join(select(rows, all_of(keys), pos), by = keys) |>
    mutate(body_site = coalesce(unname(SITE_NAMES[site_group]), "other"))
  sites_present <- intersect(names(pal_site), unique(un$body_site))
  starts <- rows$pos[rows$Condition != lag(rows$Condition, default = rows$Condition[1])]
  ylim <- c(min(rows$pos) - 0.6, max(rows$pos) + 0.6)
  if (is.null(label_bold)) label_bold <- rows$kind == "all"
  line_h <- title_size * 1.15   # pt between the two title lines
  base_y <- 4                   # pt from the panel top to the bottom of the title

  plots <- pmap(COMPARISONS |> mutate(i = row_number()), function(perio, direction, i) {
    s <- filter(st, .data$perio == !!perio, .data$direction == !!direction)
    u <- filter(un, .data$perio == !!perio, .data$direction == !!direction)
    title <- paste0(str_to_sentence(perio), "\n", str_to_sentence(direction))
    names_grob <- textGrob(paste0(perio_name, ":\n", condition, ":"), x = unit(0, "npc") - unit(5, "pt"),
                           y = unit(1, "npc") + unit(base_y, "pt"), hjust = 1, vjust = 0,
                           gp = gpar(fontsize = title_size, fontface = "bold", col = TEXT_GREY,
                                     fontfamily = "Arial", lineheight = 1.15))
    rule_left <- if (i == 1) unit(0, "npc") - unit(5, "pt") - grobWidth(names_grob) else unit(0, "npc") - unit(6, "pt")
    p <- ggplot() +
      { if (perio != direction) annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey95") } +
      geom_hline(yintercept = rows$pos, colour = GRID, linewidth = 0.3) +
      geom_hline(yintercept = starts + 0.8, colour = "grey80", linewidth = 0.3) +
      geom_vline(xintercept = 0.5, colour = "grey45", linetype = "dashed", linewidth = 0.3) +
      geom_quasirandom(data = u, aes(x = rank, y = pos, colour = body_site), orientation = "y", width = 0.28,
                       size = point_size, alpha = 0.65, stroke = 0) +
      geom_segment(data = s, aes(x = pmax(mean_rank - ci, 0), xend = pmin(mean_rank + ci, 1), y = pos, yend = pos),
                   colour = "grey10", linewidth = 0.5) +
      geom_point(data = s, aes(x = mean_rank, y = pos, fill = significance), shape = 21, size = 1.8,
                 colour = "grey10", stroke = 0.4) +
      scale_colour_manual(values = pal_site[sites_present], breaks = sites_present, name = "Body site") +
      scale_fill_manual(values = SIGNIFICANCE_FILL, drop = FALSE, name = "Mean and 95% CI") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "50", "100")) +
      scale_y_continuous(breaks = rows$pos, labels = ifelse(label_bold, paste0("**", rows$label, "**"), rows$label)) +
      coord_cartesian(xlim = c(-0.02, 1.02), ylim = ylim, expand = FALSE, clip = "off") +
      labs(x = NULL, y = NULL) +
      # two-line title above the panel, and the rule between its lines
      annotation_custom(textGrob(title, x = unit(0.5, "npc"), y = unit(1, "npc") + unit(base_y, "pt"), vjust = 0,
                                 gp = gpar(fontsize = title_size, fontface = "bold", fontfamily = "Arial",
                                           lineheight = 1.15))) +
      annotation_custom(segmentsGrob(x0 = rule_left,
                                     x1 = unit(1, "npc") + unit(6, "pt"),
                                     y0 = unit(1, "npc") + unit(base_y + line_h * 0.93, "pt"),
                                     y1 = unit(1, "npc") + unit(base_y + line_h * 0.93, "pt"),
                                     gp = gpar(col = "grey65", lwd = 0.5))) +
      theme(plot.margin = margin(base_y + 2 * line_h + 4, 6, 2, 6),  # room between neighbouring "100" and "0"
            panel.grid.major = element_blank(),
            axis.ticks.y = element_blank(),
                  axis.text.y = if (i == 1) element_markdown(hjust = 1, colour = "grey10") else element_blank(),
            legend.position = "none")
    if (i == 1) p <- p + annotation_custom(names_grob)
    p
  })
  wrap_plots(plots, nrow = 1)
}

#' Legend of the similarity panels as a patchwork element: mean significance fills and body site colours.
similarity_legend <- function(sites, nrow = 1) {
  # every significance level and every site in the data, so each legend key gets its symbol
  sig <- tibble(significance = factor(names(SIGNIFICANCE_FILL), levels = names(SIGNIFICANCE_FILL)))
  site <- tibble(site = factor(sites, levels = sites))
  p <- ggplot() +
    geom_point(data = sig, aes(0, 0, fill = significance), shape = 21, size = 1.8, colour = "grey10", stroke = 0.4) +
    geom_point(data = site, aes(0, 0, colour = site), size = 1) +
    scale_fill_manual(values = SIGNIFICANCE_FILL, drop = FALSE, name = "Mean and 95% CI") +
    scale_colour_manual(values = pal_site[sites], name = "Study units, by body site") +
    guides(fill = guide_legend(nrow = nrow, order = 1), colour = guide_legend(nrow = nrow, order = 2)) +
    theme(legend.position = "top", legend.title = element_text(margin = margin(r = 4)))
  wrap_elements(full = cowplot::get_plot_component(p, "guide-box-top"))
}

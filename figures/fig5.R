# Figure 5. Colorectal cancer, from published signatures to stool metagenomes.
#
# A the species behind the genera increased in both periodontitis and colorectal cancer; B rank of the
# periodontitis genera among BugSigDB signatures for association with colorectal cancer in ten stool metagenome
# datasets; C the periodontitis-increased genera in each dataset (log fold change, cancer vs
# control). The genus directions across conditions are Figure 4A.

source("common.R")

# enrichment method shown in B: "padog" (gene set test that down-weights genera shared by many signatures) or
# "ora" (over-representation analysis)
METHOD <- "ora"
METHOD_TITLE <- c(padog = "PADOG", ora = "Over-representation analysis")
TAG <- element_text(face = "bold", size = 9, hjust = 0)
pt <- function(x) x / .pt  # text size in points, for geom_text
# C's fold-change scale (plain diverging), for the contrast of its FDR dots
fold_change_colour <- scales::div_gradient_pal(DEC_DARK, NEUTRAL, INC_DARK, "Lab")

# --- A: species behind the shared genera ------------------------------------------------------------------

TONES <- c(top1 = INC_DARK, top2 = INC, top3 = "#F2B48C", other = "#C8C8C8")
TONE_LABELS <- c(top1 = "Main periodontitis species", top2 = "2nd", top3 = "3rd", other = "Other species")
# source names as the report labels them (and the panel titles)
SOURCES <- c("Periodontitis", "Colorectal Cancer")

#' White on dark fills, near-black on light ones.
text_colour <- function(fill) {
  rgb <- col2rgb(fill) / 255
  ifelse(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ] < 0.55, "white", "grey10")
}

#' Species mix in periodontitis and colorectal cancer, and their overlap: a patchwork of three plots.
species_panels <- function(text_size = 5.6) {
  mix <- load_fig("fig5_species_mix")
  overlap <- load_fig("fig5_species_overlap") |> arrange(overlap)  # highest overlap at the top
  y_of <- setNames(seq_along(overlap$genus), overlap$genus)
  ylim <- c(0.4, length(y_of) + 0.6)
  missing <- setdiff(SOURCES, mix$source)
  if (length(missing)) stop("no rows for ", paste(missing, collapse = ", "), "; sources in the data: ",
                            paste(sort(unique(mix$source)), collapse = ", "))
  mix <- mix |>
    filter(source %in% SOURCES) |>
    mutate(y = y_of[genus], tone = factor(tone, levels = names(TONES)),
           text_col = text_colour(TONES[as.character(tone)]))

  bar_panel <- function(src, first) {
    d <- filter(mix, source == src)
    totals <- distinct(d, genus, y, total)
    ggplot(d) +
      geom_rect(aes(xmin = start, xmax = end, ymin = y - 0.36, ymax = y + 0.36, fill = tone),
                colour = "white", linewidth = 0.25) +
      geom_text(data = filter(d, share >= 0.16), aes(x = (start + end) / 2, y = y, label = epithet,
                                                     colour = text_col),
                fontface = "italic", size = text_size / .pt, family = "Arial") +
      geom_text(data = totals, aes(x = 1.02, y = y, label = total), hjust = 0, colour = TEXT_GREY,
                size = text_size / .pt, family = "Arial") +
      # heading of the count column, above the panel's top edge
      annotate("text", x = 1.1, y = max(ylim), label = "Reports", hjust = 1, vjust = -0.4, colour = TEXT_GREY,
               size = (text_size - 0.3) / .pt, family = "Arial") +
      scale_fill_manual(values = TONES, guide = "none") +
      scale_colour_identity() +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      scale_y_continuous(breaks = y_of, labels = taxon_md(names(y_of))) +
      coord_cartesian(xlim = c(0, 1.1), ylim = ylim, expand = FALSE, clip = "off") +
      labs(title = src, x = "Share of species-level reports", y = NULL) +
      theme(panel.grid.major = element_blank(), axis.ticks.y = element_blank(),
            axis.text.y = if (first) element_markdown(colour = "grey10") else element_blank())
  }

  # lollipops start at the left edge so every row reads as a line
  x0 <- -0.05
  ov <- overlap |> mutate(y = y_of[genus])
  p_overlap <- ggplot(ov) +
    geom_segment(aes(x = x0, xend = overlap, y = y, yend = y), colour = "grey60", linewidth = 0.35) +
    geom_point(aes(x = overlap, y = y), colour = "grey15", size = 1.1) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
    coord_cartesian(xlim = c(x0, 1.08), ylim = ylim, expand = FALSE) +
    labs(title = "Overlap", x = "Shared species mix", y = NULL) +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

  wrap_plots(bar_panel(SOURCES[1], TRUE), bar_panel(SOURCES[2], FALSE), p_overlap, nrow = 1,
             widths = c(4, 4, 1.4))
}

#' A plot reduced to its legend, as a patchwork element. Unlike extracting the guide box beforehand, the legend
#' is laid out on the output device, so the spacing follows the real font.
legend_only <- function(p) {
  wrap_elements(full = p +
    coord_cartesian(xlim = c(10, 11), ylim = c(10, 11)) +  # keeps the data out of view
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(),
          axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
          legend.position = "top", plot.margin = margin(0, 0, 0, 0)))
}

#' Legend of the species panels as a patchwork element (one row). extra: ggplot components to add (a tag).
species_legend <- function(extra = NULL) {
  keys <- TONE_LABELS
  values <- unname(TONES)
  d <- tibble(x = seq_along(keys), key = factor(keys, levels = keys))
  p <- ggplot(d, aes(x, x, fill = key)) +
    geom_tile() +
    scale_fill_manual(values = setNames(values, keys), name = NULL) +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.key.spacing.x = unit(8, "pt"))
  legend_only(p + extra)
}


panel_species <- function() {
  p <- species_panels(text_size = 5.3)
  # tag on the legend row, so it heads the panel
  legend <- species_legend(extra = list(labs(tag = "A"), theme(plot.tag = TAG)))
  wrap_plots(legend, p, ncol = 1, heights = c(0.07, 1))
}

# --- B: enrichment ranks; C: genus fold changes -----------------------------------------------------------


SIGNATURES <- tribble(
  ~label,                        ~name,                       ~colour,   ~shape, ~offset,
  "PerioSigDB_genera_increased", "Periodontitis increased",   INC,       21,     0.27,
  "PerioSigDB_genera_decreased", "Periodontitis decreased",   DEC,       21,     0.09,
  "Wirbel2019",                  "Wirbel 2019 meta-analysis", "grey25",  22,     -0.09,
  "Thomas2019",                  "Thomas 2019 meta-analysis", "grey25",  24,     -0.27
)
OPEN <- "FDR ≥ 0.05"

datasets <- function() {
  load_fig("fig5_crc_datasets") |>
    mutate(n = n_crc + n_control) |>
    arrange(n) |>  # largest at top
    mutate(y = row_number(), label = paste0(str_replace_all(dataset, "_", " "), " (", n_crc, "/", n_control, ")"))
}

panel_ranks <- function(ds) {
  ranks <- load_fig("fig5_crc_ranks") |>
    filter(method == METHOD) |>
    inner_join(SIGNATURES, by = "label") |>
    mutate(y = ds$y[match(dataset, ds$dataset)] + offset,
           name = factor(name, levels = c(SIGNATURES$name, OPEN)),
           fill = ifelse(fdr < 0.05, colour, "white"))
  keys <- c(SIGNATURES$name, OPEN)
  breaks <- c(100, 50, 10, 1, 0.1)
  ggplot(ranks, aes(log10(percentile), y)) +
    # top 5%: shaded to the right (the axis runs from 100% on the left to 0.1% on the right)
    annotate("rect", xmin = log10(5), xmax = -Inf, ymin = -Inf, ymax = Inf, fill = alpha(INC, 0.14)) +
    annotate("text", x = log10(5), y = nrow(ds) + 0.45, label = " top 5%", hjust = 0, vjust = 1, size = pt(5.5),
             colour = INC_DARK, family = "Arial") +
    geom_hline(yintercept = seq_len(nrow(ds) - 1) + 0.5, colour = GRID, linewidth = 0.3) +
    geom_vline(xintercept = log10(50), colour = "grey45", linetype = "dashed", linewidth = 0.3) +
    # show.legend: also draw the key of the open symbol, which has no data of its own
    geom_point(aes(shape = name, colour = name, fill = I(fill)), size = 1.5, stroke = 0.45, show.legend = TRUE) +
    scale_shape_manual(values = setNames(c(SIGNATURES$shape, 21), keys), limits = keys, name = NULL) +
    scale_colour_manual(values = setNames(c(SIGNATURES$colour, "grey25"), keys), limits = keys, name = NULL) +
    # rows: the two periodontitis signatures, the two meta-analyses, then the open symbol (FDR)
    guides(shape = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(fill = c(SIGNATURES$colour, "white"))),
           colour = guide_legend(ncol = 2, byrow = TRUE)) +
    scale_x_reverse(breaks = log10(breaks), labels = paste0(breaks, "%")) +
    scale_y_continuous(breaks = ds$y, labels = ds$label) +
    coord_cartesian(xlim = c(log10(130), -1), ylim = c(0.5, nrow(ds) + 0.5), expand = FALSE) +
    labs(title = paste0(METHOD_TITLE[[METHOD]], ", CRC vs control stool"),
         x = "Rank among BugSigDB signatures (top %, log scale)", y = NULL, tag = "C") +
    theme(panel.grid.major = element_blank(), axis.ticks.y = element_blank(), plot.tag = TAG,
          legend.position = "bottom", legend.justification = "left", legend.location = "plot",
          legend.text = element_text(size = 6), legend.key.size = unit(8, "pt"),
          plot.title.position = "plot")
}

panel_fold_change <- function(ds, genus_order) {
  lim <- 4
  g <- load_fig("fig5_crc_genera") |> filter(signature == "PerioSigDB_genera_increased")
  genera <- intersect(genus_order, g$genus)
  cells <- tidyr::expand_grid(genus = genera, dataset = ds$dataset) |>
    left_join(g, by = c("genus", "dataset")) |>
    mutate(x = match(genus, genera), y = ds$y[match(dataset, ds$dataset)],
           value = pmin(pmax(logFC, -lim), lim),
           dot = text_colour(ifelse(is.na(value), "white", fold_change_colour((value + lim) / (2 * lim)))))
  ggplot(cells, aes(x, y)) +
    geom_tile(data = filter(cells, is.na(value)), fill = "white", colour = "grey88", linewidth = 0.2) +
    geom_tile(data = filter(cells, !is.na(value)), aes(fill = value), colour = "white", linewidth = 0.3) +
    geom_point(data = filter(cells, fdr < 0.05), aes(colour = I(dot)), size = 0.7) +
    scale_fill_gradient2(low = DEC_DARK, mid = NEUTRAL, high = INC_DARK, midpoint = 0, limits = c(-lim, lim),
                         breaks = c(-4, -2, 0, 2, 4), labels = c("≤ −4", "−2", "0", "2", "≥ 4"),
                         name = "log2 fold change, CRC vs control\n(dot: FDR < 0.05)") +
    guides(fill = guide_colourbar(theme = theme(legend.title.position = "right",
                                                legend.title = element_text(angle = 90, hjust = 0.5, size = 6),
                                                legend.key.height = unit(34, "mm"),
                                                legend.key.width = unit(2.2, "mm")))) +
    scale_x_continuous(breaks = seq_along(genera), labels = taxon_md(genera)) +
    scale_y_continuous(breaks = ds$y, labels = NULL) +
    coord_cartesian(expand = FALSE) +
    labs(title = "Periodontitis-increased genera in each dataset", x = NULL, y = NULL, tag = "D") +
    theme(panel.grid.major = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_markdown(angle = 60, hjust = 1, vjust = 1, size = 5.6, colour = "grey10"),
          plot.tag = TAG, legend.position = "right", legend.text = element_text(size = 5.5),
          legend.margin = margin(0, 0, 0, 4))
}

#' The enrichment ranks (B) and the fold changes (C) side by side.
panels_bc <- function() {
  ds <- datasets()
  genus_order <- load_fig("fig5_perio_genera") |> filter(direction == "increased") |> pull(taxon)
  # keep B's axis title and legend close to its axis: C's slanted genus names take no space of their own
  # titles start a little right of the panel letters
  pad <- theme(plot.title = element_text(margin = margin(l = 10, b = 3)))
  wrap_plots(panel_ranks(ds) + labs(tag = "B") + pad,
             free(panel_fold_change(ds, genus_order) + labs(tag = "C") + pad, type = "space", side = "b"), nrow = 1,
             widths = c(1, 1.05))
}

heights <- c(62, 86)
p <- wrap_plots(panel_species(), wrap_elements(full = panels_bc()), ncol = 1, heights = heights)
save_fig(p, "fig5", height = sum(heights))

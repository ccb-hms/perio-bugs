# Figure 2. The PerioSigDB consensus taxa, how they co-vary in healthy plaque, and who the studies recruited.
#
# A taxa that more PerioSigDB signatures report increased (left) or decreased (right) in periodontitis than
# the other way; B correlation between pairs of periodontitis-associated genera in healthy subgingival plaque
# from the Human Microbiome Project; C age, sex and smoking of the health and periodontitis groups in each study.

source("common.R")

taxa <- load_fig("fig2_consensus_taxa")
pt_size <- function(pt) pt / .pt

# --- A: consensus taxa ---------------------------------------------------------------------------------------

#' Shorter taxon names: genus initial for the one long species name, and no "bacterium" or "sp." before
#' "oral taxon".
shorten_taxon <- function(x) {
  x <- str_replace(x, "^Aggregatibacter ", "A. ")
  str_replace(x, " (bacterium|sp\\.) oral taxon", " oral taxon")
}

#' Each panel's x range fits its own bars, with a fixed share of the width for the p values (left) and the
#' participant circles (right), so the two panels have different scales.
consensus_panel <- function(direction, title, labeller = identity) {
  d <- taxa |>
    filter(.data$direction == !!direction) |>
    mutate(y = rev(seq_len(n())), strength = if_else(fdr < 0.05, "FDR < 0.05", "p < 0.05 only"))
  lo <- -max(d$decreased); hi <- max(d$increased); span <- hi - lo
  x_p <- lo - 0.3 * span; x_circle <- hi + 0.1 * span; xlim <- c(lo - 0.33 * span, hi + 0.17 * span)
  bars <- d |>
    select(y, strength, increased, decreased) |>
    pivot_longer(c(increased, decreased), names_to = "reported", values_to = "n") |>
    filter(n > 0) |>
    mutate(n = if_else(reported == "decreased", -n, n),
           reported = factor(paste("Reported", reported), levels = c("Reported increased", "Reported decreased")))
  ggplot(d, aes(y = y)) +
    geom_col(data = bars, aes(x = n, y = y, fill = reported, alpha = strength), width = 0.7, orientation = "y") +
    geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.3) +
    geom_text(aes(x = x_p, label = if_else(p < 0.001, "<0.001", sprintf("%.3f", p))), hjust = 0,
              size = pt_size(5.3), colour = TEXT_GREY) +
    geom_point(aes(x = x_circle, size = participants), shape = 21, fill = "grey65", colour = "grey20",
               stroke = 0.25) +
    scale_fill_manual(values = c("Reported increased" = INC, "Reported decreased" = DEC), name = "Signatures:",
                      drop = FALSE) +
    scale_alpha_manual(values = c("FDR < 0.05" = 1, "p < 0.05 only" = 0.4), name = NULL, drop = FALSE) +
    scale_size_area(max_size = 2.6, limits = c(0, max(taxa$participants)), breaks = c(500, 2000, 5000),
                    labels = scales::label_comma(), name = "Participants in the studies:") +
    scale_x_continuous(breaks = scales::breaks_extended(6), labels = abs) +
    scale_y_continuous(breaks = d$y, labels = taxon_md(labeller(d$label))) +
    coord_cartesian(xlim = xlim, ylim = c(0.4, nrow(d) + 0.6), expand = FALSE, clip = "off") +
    labs(x = "Signatures reporting the taxon", y = NULL, title = title) +
    guides(fill = guide_legend(order = 1), alpha = guide_legend(order = 2, override.aes = list(fill = "grey40")),
           size = guide_legend(order = 3)) +
    theme(axis.text.y = element_markdown(size = 6),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank())  # no gridlines in A
}

p_up <- consensus_panel("Increased in periodontitis", "Increased in periodontitis", labeller = shorten_taxon)
p_down <- consensus_panel("Decreased in periodontitis", "Decreased in periodontitis")

# --- B: pair correlations in healthy plaque (HMP) -------------------------------------------------------------

PAIR_TYPES <- c("Both increased", "Both decreased", "One increased,\none decreased")
pal_pair <- setNames(c(INC, DEC, "grey75"), PAIR_TYPES)

pairs <- load_fig("fig2_cooc_pairs")
tests <- load_fig("fig2_cooc_tests")
hmp <- pairs$column[pairs$column_order == 1][1]
n_healthy <- str_extract(hmp, "\\((\\d+)", group = 1)
row_y <- setNames(rev(seq_along(PAIR_TYPES)) - 1, PAIR_TYPES)   # first pair type on top

hp <- pairs |> filter(column == hmp) |> mutate(y = row_y[pair])
ht <- tests |> filter(column == hmp) |> mutate(y = row_y[pair])
n_pairs <- count(hp, pair)
y_labels <- paste0(PAIR_TYPES, "\n(", n_pairs$n[match(PAIR_TYPES, n_pairs$pair)], " pairs)")

p_hmp <- ggplot(hp, aes(x = r, y = y)) +
  geom_vline(xintercept = 0, colour = "grey45", linetype = "dashed", linewidth = 0.3) +
  geom_quasirandom(aes(fill = pair, group = y), orientation = "y", width = 0.3, shape = 21, size = 1,
                   colour = "grey20", stroke = 0.15, alpha = 0.85) +
  geom_segment(data = ht, aes(x = median, xend = median, y = y - 0.36, yend = y + 0.36), colour = "grey10",
               linewidth = 0.7) +
  geom_text(data = ht, aes(x = median, y = y + 0.4, label = sprintf("%.2f", median)), vjust = 0,
            size = pt_size(5.5)) +
  geom_text(data = ht, aes(x = -0.62, y = y + 0.4, label = sprintf("p = %s", signif(p, 1))), hjust = 0, vjust = 0,
            size = pt_size(5.3), fontface = "italic", colour = TEXT_GREY) +
  scale_fill_manual(values = pal_pair, guide = "none") +
  scale_x_continuous(breaks = c(-0.5, 0, 0.5)) +
  scale_y_continuous(breaks = row_y, labels = y_labels) +
  coord_cartesian(xlim = c(-0.65, 0.85), ylim = c(-0.6, 2.85), expand = FALSE) +
  labs(x = "Pair correlation (Spearman, CLR)", y = NULL,
       title = paste0("Healthy plaque (HMP, ", n_healthy, " participants)")) +
  theme(axis.ticks.y = element_blank(), panel.grid.major.x = element_blank(),  # row lines only
        axis.text.y = element_text(size = 6, lineheight = 0.95))

# --- C: the study groups -----------------------------------------------------------------------------------------

source("study_groups.R")  # study_group_row()
groups <- study_group_row()

# --- layout ---------------------------------------------------------------------------------------------------

# the increased panel runs the full height on the left; the right column holds the decreased panel, then B, then
# the three plots of C side by side
n_up <- sum(taxa$direction == "Increased in periodontitis")
n_down <- sum(taxa$direction == "Decreased in periodontitis")
# wrapped as one element, so the y titles sit by their own axes, not out by the decreased panel's taxon names
lower_right <- wrap_elements(full = (p_hmp + labs(tag = "B")) /
  wrap_elements(full = (groups[[1]] + labs(tag = "C")) + groups[[2]] + groups[[3]] + plot_layout(nrow = 1)) +
  plot_layout(heights = c(1, 1.45)))
panels <- (p_up + labs(tag = "A")) + p_down + lower_right +
  plot_layout(design = c(area(1, 1, 2, 1), area(1, 2), area(2, 2)), widths = c(0.9, 1.22),
              heights = c(n_down + 1.5, n_up - n_down)) &
  theme(legend.position = "none")

# one legend row above the panels: bar colours and shading, then circle sizes
# (suppressWarnings: cowplot measures the text on a device without Arial; the saved figure uses Cairo)
legend_row <- wrap_elements(full = suppressWarnings(cowplot::get_plot_component(
  p_up + theme(legend.position = "top", legend.justification = "left", legend.title.position = "top",
               legend.box.just = "bottom", legend.spacing.x = unit(14, "pt")),
  "guide-box-top")))
fig <- legend_row / panels + plot_layout(heights = c(0.045, 1))

save_fig(fig, "fig2", height = 165)

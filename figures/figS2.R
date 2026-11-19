# Figure S2. How specific the periodontitis-associated genera are to periodontitis.
#
# For the periodontitis-increased (A) and -decreased (B) genera of Figure 4A: support in PerioSigDB against the
# number of other BugSigDB conditions reporting the genus in the same direction.

source("common.R")
library(ggrepel)

SUPPORTED <- 10   # shaded corner: at least 10 PerioSigDB studies ...
FEW_OTHERS <- 15  # ... and at most 15 other conditions
X_MIN <- 5
NOTE_SHARE <- 0.32  # share of the (log) x range the corner note spans, from the right edge

#' Every genus labelled. Corner points and shading take the direction's colours. The report exports only the
#' periodontitis-associated genera, each in its consensus direction.
panel_specificity <- function(direction) {
  up <- direction == "Increased in periodontitis"
  way <- if (up) "increased" else "decreased"
  dark <- if (up) INC_DARK else DEC_DARK
  d <- load_fig("figS2_specificity") |>
    filter(.data$direction == !!direction) |>
    mutate(corner = perio_studies >= SUPPORTED & other_conditions <= FEW_OTHERS, label = taxon)
  x_max <- max(d$perio_studies) * 1.6
  y_max <- max(d$other_conditions) + 4
  note <- tibble(x = x_max / 1.04, y = -5, label = paste0("Commonly ", way, " in periodontitis,\ninfrequently in other conditions"))
  # unlabelled stand-ins that keep the genus labels off the corner note
  note_left <- x_max / exp(NOTE_SHARE * log(x_max / X_MIN))
  blockers <- expand_grid(perio_studies = exp(seq(log(note_left), log(x_max), length.out = 8)),
                          other_conditions = seq(-5.5, 3.5, length.out = 4)) |>
    mutate(label = "")
  ggplot(d, aes(perio_studies, other_conditions)) +
    annotate("rect", xmin = SUPPORTED, xmax = Inf, ymin = -Inf, ymax = FEW_OTHERS + 0.5,
             fill = if (up) "#fbe3d6" else "#dde8f6") +
    geom_text(data = note, aes(x, y, label = label), hjust = 1, vjust = 0, size = 5.5 / .pt, colour = dark,
              lineheight = 0.95) +
    geom_point(aes(fill = corner), shape = 21, size = 1.6, colour = "grey15", stroke = 0.35) +
    # max.time high so that max.iter, not the clock, ends the search: the same layout on every run
    geom_text_repel(data = bind_rows(d, blockers), aes(label = label), size = 5.5 / .pt, fontface = "italic",
                    family = "Arial", colour = "grey20", seed = 1, max.overlaps = Inf, box.padding = 0.12,
                    point.padding = 0.12, min.segment.length = unit(5, "pt"), segment.colour = "grey60",
                    segment.size = 0.2, max.time = 30, max.iter = 1e5) +
    scale_fill_manual(values = c(`TRUE` = if (up) INC else DEC, `FALSE` = "grey75"), guide = "none") +
    scale_x_log10(breaks = c(5, 10, 20, 50, 100), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 120, 20), expand = c(0, 0)) +
    coord_cartesian(xlim = c(X_MIN, x_max), ylim = c(-7, y_max)) +
    labs(x = paste("PerioSigDB studies reporting the genus", way, "(log scale)"),
         y = paste("Other BugSigDB conditions reporting the genus", way),
         title = paste("Genera", way, "in periodontitis"))
}

p <- (panel_specificity("Increased in periodontitis") + labs(tag = "A")) +
  (panel_specificity("Decreased in periodontitis") + labs(tag = "B")) +
  plot_layout(nrow = 1) & theme(plot.tag = element_text(face = "bold", size = 9))
save_fig(p, "figS2", height = 95)

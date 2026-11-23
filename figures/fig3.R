# Figure 3. PerioSigDB against existing resources.
#
# A periodontitis papers and signatures in each resource; B profiling method and C taxonomic rank of
# PerioSigDB and BugSigDB signatures; D how consistent each condition's signatures are across studies.
#
# The rows for other resources come from figures/data/other_resources.csv, which other_resources.R writes.

source("common.R")

HIGHLIGHT <- "#332288"
OTHER <- "grey60"
COUNT_COLOURS <- c(Papers = "#332288", Signatures = "#88CCEE")  # as in Figure 1A
ROW_SHADE <- c(perio = "#dff3ea", plaque = "#e6eef8")            # as in the report
PCT <- scales::label_percent()

coverage <- function() {
  real <- load_fig("fig3_coverage")
  other <- read_csv(file.path(FIG_DIR, "data", "other_resources.csv"), show_col_types = FALSE)
  bind_rows(real, other) |> mutate(y = rev(seq_along(resource)) - 1)
}

value_text <- function(...) geom_text(..., hjust = 0, size = 5.5 / .pt, colour = TEXT_GREY)

# --- A: coverage --------------------------------------------------------------------------------------------

# papers and signatures as paired bars in one plot
panel_coverage_single <- function() {
  d <- coverage()
  h <- 0.38
  top <- max(d$signatures)
  r <- d |>
    pivot_longer(c(papers, signatures), names_to = "series", values_to = "value") |>
    mutate(series = str_to_sentence(series), col = COUNT_COLOURS[series],
           yc = y + ifelse(series == "Papers", h / 2, -h / 2),
           xmin = 0, xmax = value, ymin = yc - h / 2, ymax = yc + h / 2)
  ggplot() +
    geom_rect(data = r, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = series)) +
    value_text(data = r, aes(x = value + top * 0.015, y = yc, label = value)) +
    scale_fill_manual(values = COUNT_COLOURS, name = NULL) +
    scale_x_continuous(limits = c(0, top * 1.12), expand = c(0, 0)) +
    scale_y_continuous(breaks = d$y, labels = d$resource, expand = expansion(add = 0.15)) +
    labs(x = "Count", y = NULL, title = "Periodontitis studies in each resource") +
    theme(panel.grid.major.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "inside", legend.position.inside = c(0.985, 0.04), legend.justification = c(1, 0),
          legend.key.spacing.y = unit(2, "pt"), legend.byrow = TRUE)
}

# --- B, C: profiling method and taxonomic rank ----------------------------------------------------------------

#' Black or white text, whichever reads better on the fill.
text_colour <- function(fill) {
  rgb <- col2rgb(fill) / 255
  ifelse(colSums(rgb * c(0.299, 0.587, 0.114)) > 0.55, "grey10", "white")
}

#' 100% stacked bars of PerioSigDB (top) and BugSigDB, shares labelled where wide enough.
stacked <- function(d, column, palette, title, xlabel, labels = names(palette), ncol = 3) {
  levels <- names(palette)
  s <- d |>
    rename(level = all_of(column)) |>
    mutate(level = factor(level, levels = levels), y = ifelse(db == "PerioSigDB", 1, 0)) |>
    arrange(db, level) |>
    group_by(db) |>
    mutate(xmax = cumsum(prop), xmin = xmax - prop) |>
    ungroup()
  ggplot(s) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = y - 0.31, ymax = y + 0.31, fill = level),
              colour = "white", linewidth = 0.25) +
    geom_text(data = filter(s, prop >= 0.07),
              aes(x = (xmin + xmax) / 2, y = y, label = PCT(round(prop, 2)), colour = I(text_colour(palette[level]))),
              size = 5.6 / .pt) +
    scale_fill_manual(values = palette, labels = labels, name = NULL) +
    scale_x_continuous(breaks = seq(0, 1, 0.25), labels = PCT, expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 1)) +
    guides(fill = guide_legend(ncol = ncol)) +
    labs(x = xlabel, y = NULL, title = title) +
    theme(panel.grid.major.y = element_blank(), axis.ticks.y = element_blank(),
          legend.key.spacing.x = unit(6, "pt"), legend.key.spacing.y = unit(1, "pt"))
}

method_plot <- function() {
  stacked(load_fig("fig3_methods"), "method", pal_method, "Profiling method of signatures", "Share of signatures",
          labels = METHOD_SHORT)
}
rank_plot <- function() {
  stacked(load_fig("fig3_ranks"), "rank", pal_rank, "Taxonomic rank of reported taxa", "Share of taxa", ncol = 4)
}

# legend inside the panel, above the two bars
legend_inside <- function(p, y_top) {
  p + scale_y_continuous(breaks = c(1, 0), labels = c("PerioSigDB", "BugSigDB"), limits = c(-0.45, y_top),
                         expand = c(0, 0)) +
    theme(legend.position = "inside", legend.position.inside = c(0.01, 0.985), legend.justification = c(0, 1))
}

# --- D: consistency across studies --------------------------------------------------------------------------------

CONSISTENCY_KEYS <- c("Observed, FDR < 0.05", "Observed, not significant")

#' Mean similarity between signatures from different studies, against random sets of other conditions'
#' signatures matched on number, direction and body site (grey: central 95% and mean). Returns the two plots
#' (increased, decreased) and their legend.
panel_consistency <- function() {
  d <- load_fig("fig3_replicability") |>
    # shorter check-row labels: "Periodontitis: sequencing (32)", "Dental plaque, pooled (15)"
    mutate(label = str_replace(label, "^(Periodontitis: \\w+) only", "\\1") |>
             str_replace("^Dental plaque, BugSigDB pooled", "Dental plaque, pooled"),
           obs = factor(ifelse(filled, CONSISTENCY_KEYS[1], CONSISTENCY_KEYS[2]), levels = CONSISTENCY_KEYS),
           # bold the periodontitis and plaque rows
           label_md = ifelse(kind == "condition", label, paste0("**", label, "**")))
  one <- function(direction, title) {
    g <- d |> filter(.data$direction == !!direction) |> arrange(key) |> mutate(y = row_number() - 1)  # largest excess at top
    ggplot(g, aes(y = y)) +
      geom_rect(data = filter(g, kind != "condition"), aes(xmin = -Inf, xmax = Inf, ymin = y - 0.5, ymax = y + 0.5,
                                                           fill = I(ROW_SHADE[kind])), show.legend = FALSE) +
      geom_hline(yintercept = g$y, colour = GRID, linewidth = 0.3) +
      geom_segment(aes(x = lo, xend = hi, yend = y, colour = "Random sets: 95% range and mean"), linewidth = 1.5) +
      geom_segment(aes(x = random_mean, xend = random_mean, y = y - 0.28, yend = y + 0.28), colour = "grey40",
                   linewidth = 0.35) +
      geom_point(aes(x = observed, fill = obs), shape = 21, size = 1.7, colour = "grey10", stroke = 0.45) +
      scale_fill_manual(values = setNames(c("grey10", "white"), CONSISTENCY_KEYS), drop = FALSE, name = NULL) +
      scale_colour_manual(values = "grey82", name = NULL) +
      scale_x_continuous(limits = c(0, 0.66), breaks = c(0, 0.2, 0.4, 0.6), expand = c(0, 0)) +
      scale_y_continuous(breaks = g$y, labels = g$label_md, expand = c(0, 0)) +
      coord_cartesian(ylim = c(-0.6, nrow(g) - 0.4)) +
      guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)) +
      labs(x = "Mean similarity between studies", y = NULL, title = title) +
      # no theme gridlines: the rows have their own lines (panel.grid.major is set by name in the theme)
      theme(panel.grid.major = element_blank(), axis.ticks.y = element_blank(),
            axis.text.y = element_markdown(size = 5.8, colour = "grey10"),
            legend.position = "bottom", legend.key.spacing.x = unit(8, "pt"))
  }
  plots <- list(one("Increased in the condition", "Increased signatures"),
                one("Decreased in the condition", "Decreased signatures"))
  legend <- wrap_elements(full = cowplot::get_plot_component(plots[[1]], "guide-box-bottom"))
  list(plots = map(plots, \(p) p + theme(legend.position = "none")), legend = legend)
}

consistency_block <- function() {
  cons <- panel_consistency()
  panels <- wrap_plots(cons$plots[[1]] + labs(tag = "D"), cons$plots[[2]], nrow = 1)
  wrap_plots(panels, cons$legend, ncol = 1, heights = c(1, 0.04))
}

# --- assemble ------------------------------------------------------------------------------------------------------

# method and rank plots: no x-axis title (the panel titles say what the shares are of) and no gridlines
# (panel.grid.major is set by name in the theme)
plain_b <- list(labs(x = NULL), theme(panel.grid.major = element_blank()))

#' fig3: A (papers and signatures as paired bars) on the left; B (method) above C (rank) on the right, so that
#' their bars run the full width of their column; D below.
build_main <- function() {
  pad <- theme(plot.title = element_text(margin = margin(l = 3, b = 3)))
  right <- (legend_inside(method_plot(), y_top = 2.75) + labs(tag = "B") + pad + plain_b) /
    (legend_inside(rank_plot(), y_top = 2.15) + labs(tag = "C") + pad + plain_b) +
    plot_layout(heights = c(0.76, 0.62))
  top <- wrap_plots(panel_coverage_single() + labs(tag = "A") + pad, plot_spacer(), right, nrow = 1,
                    widths = c(1, 0.05, 1.15))
  p <- wrap_plots(top, free(consistency_block()), ncol = 1, heights = c(0.62, 0.8))
  save_fig(p, "fig3", height = 140)
}


build_main()

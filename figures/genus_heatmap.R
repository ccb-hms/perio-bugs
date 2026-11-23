# Direction heatmaps of the periodontitis-increased and -decreased genera across BugSigDB conditions (Figure 4A).
#
# Sourced by fig4.R. panel_genus_heatmap() returns one plot drawn in millimetre coordinates (y = 0 at the top,
# negative downwards), GENUS_HEATMAP_HEIGHT mm tall and the full figure width, so both blocks share the row height
# and the decreased block, which has fewer rows, can hold the legend underneath.

SIGNIFICANCE_ALPHA <- c("FDR < 0.05" = 1, "p < 0.05 (not FDR)" = 0.55, "Not significant" = 0.25)
pt <- function(x) x / .pt  # text size in points, for geom_text
# share reported increased: grey strictly between 1/3 and 2/3 (neither direction reaches the report's 2/3 cut-off),
# then blue or orange as one direction dominates
MIXED_GREY <- "#e3e1dd"  # a shade darker than "not reported" white, lighter than the 1-2 studies grey
FEW_GREY <- "grey74"
# the grey band is open at the cut-offs: a share of exactly 1/3 or 2/3 has a clear direction (as the report counts
# it), so the colour steps to light blue or orange there
EPS <- 1e-6
SHARE_COLOURS <- c(DEC_DARK, DEC, "#a9c8ee", MIXED_GREY, MIXED_GREY, "#f5b796", INC, INC_DARK)
SHARE_VALUES <- c(0, 0.15, 1 / 3, 1 / 3 + EPS, 2 / 3 - EPS, 2 / 3, 0.85, 1)
scale_fill_share <- function(...) {
  scale_fill_gradientn(colours = SHARE_COLOURS, values = SHARE_VALUES, limits = c(0, 1), na.value = "white", ...)
}

HEAT_WIDTH <- DOUBLE
ROW_H <- 2.9
# the rows of the longer (increased) block, plus the header above and 15 mm below for the column labels
GENUS_HEATMAP_HEIGHT <- 12 + max(table(load_fig("fig5_perio_genera")$direction)) * ROW_H + 15
BAR_W <- 15
BAR_GAP <- 1
LEFT <- 24          # room for the increased genus names
LABELS_W <- 21      # room for the decreased genus names
TITLE_Y <- -0.2
ORAL_TOP <- -5
ORAL_BOTTOM <- -10.5
HEAT_TOP <- -12
BAR_TICKS <- list(increased = list(ticks = c(0, 25, 50, 75), lim = c(-30, 80)),
                  decreased = list(ticks = c(-25, 0, 25), lim = c(-36, 26)))

heatmap_columns <- function(direction) {
  load_fig("fig5_direction_columns") |>
    filter(genus_group == direction) |>
    arrange(col_order) |>
    # All BugSigDB stands apart from the conditions, and conditions going the opposite way sit in a second block
    mutate(group = ifelse(Condition == "All BugSigDB", "all", block),
           # short labels, as in B and C: "CRC (9/9)", "COVID-19 gut (3/4)" for a body site subset
           col = ifelse(group == "all", col,
                        paste(abbrev(str_remove(Condition, " \\(\\w+\\)$")),
                              coalesce(str_match(Condition, "\\((\\w+)\\)$")[, 2], ""),
                              str_extract(col, "\\(\\d+/\\d+\\)$")) |> str_squish()),
           x = row_number() - 1 + 0.5 * (group != "all") + 0.5 * (group == "opposite"))
}

#' Geometry of one block (bars, heatmap, oral bars), starting at x0 mm; returns the layers' data.
block_data <- function(direction, x0, unit) {
  genera <- load_fig("fig5_perio_genera") |> filter(direction == !!direction)
  cols <- heatmap_columns(direction)
  n <- nrow(genera)
  heat_left <- x0 + BAR_W + BAR_GAP
  heat_bottom <- HEAT_TOP - n * ROW_H
  row_y <- setNames(HEAT_TOP - (seq_len(n) - 0.5) * ROW_H, genera$taxon)
  cols <- mutate(cols, xc = heat_left + (x + 0.5) * unit)
  ticks <- BAR_TICKS[[direction]]
  bx <- function(v) x0 + (v - ticks$lim[1]) / diff(ticks$lim) * BAR_W

  cells <- load_fig("fig5_direction_cells") |>
    filter(genus_group == direction) |>
    left_join(select(cols, Condition, xc), by = "Condition") |>
    mutate(yc = row_y[taxon],
           kind = case_when(is.na(n) ~ "none", few ~ "few", .default = "share"),
           # a significant share inside the grey band (large columns such as All BugSigDB) takes the light colour of
           # its direction, so every significant cell shows a direction
           share_plot = if_else(significant & share_plot > 1 / 3 & share_plot < 2 / 3,
                                if_else(share_plot > 0.5, 2 / 3, 1 / 3), share_plot),
           label = ifelse(kind == "share", paste0(same, "/", n), NA))
  groups <- cols |>
    summarise(xmin = heat_left + min(x) * unit, xmax = heat_left + (max(x) + 1) * unit, .by = group)

  list(
    genera = mutate(genera, y = row_y[taxon], alpha = SIGNIFICANCE_ALPHA[significance], label = taxon_md(taxon),
                    x_label = x0 - 0.8),
    bars = bind_rows(
      transmute(genera, xmin = bx(0), xmax = bx(increased), fill = INC),
      transmute(genera, xmin = bx(-decreased), xmax = bx(0), fill = DEC)
    ) |> mutate(y = rep(row_y, 2), alpha = rep(SIGNIFICANCE_ALPHA[genera$significance], 2)),
    bar_frame = tibble(xmin = x0, xmax = x0 + BAR_W, ymin = heat_bottom, ymax = HEAT_TOP, zero = bx(0)),
    bar_ticks = tibble(x = bx(ticks$ticks), label = as.character(abs(ticks$ticks)), y = heat_bottom),
    cells = cells,
    cols = mutate(cols, y = heat_bottom - 1),
    oral = mutate(cols, xmin = xc - 0.35 * unit, xmax = xc + 0.35 * unit, ymax = ORAL_BOTTOM + oral * (ORAL_TOP - ORAL_BOTTOM)),
    groups = groups, heat_left = heat_left, heat_bottom = heat_bottom, title_x = x0 + BAR_W / 2, unit = unit,
    width = BAR_W + BAR_GAP + (max(cols$x) + 1) * unit
  )
}

block_layers <- function(b, direction, oral_label) {
  cell_w <- b$unit
  list(
    annotate("text", x = b$title_x, y = TITLE_Y, label = paste("Periodontitis", direction), fontface = "bold",
             size = pt(6.5), vjust = 1, family = "Arial"),
    # share of each column's studies from oral sites
    geom_rect(data = b$oral, aes(xmin = xmin, xmax = xmax, ymin = ORAL_BOTTOM, ymax = ymax), fill = "grey60"),
    geom_rect(data = b$groups, aes(xmin = xmin, xmax = xmax, ymin = ORAL_BOTTOM, ymax = ORAL_TOP), fill = NA,
              colour = FRAME, linewidth = 0.3),
    annotate("segment", x = b$heat_left - 0.6, xend = b$heat_left, y = c(ORAL_BOTTOM, ORAL_TOP),
             yend = c(ORAL_BOTTOM, ORAL_TOP), colour = "grey40", linewidth = 0.25),
    annotate("text", x = b$heat_left - 0.9, y = c(ORAL_BOTTOM, ORAL_TOP), label = c("0%", "100%"), hjust = 1,
             size = pt(5.3), colour = "grey15", family = "Arial"),
    if (oral_label) annotate("text", x = b$heat_left - 8, y = (ORAL_TOP + ORAL_BOTTOM) / 2,
                             label = "Oral\nstudies", hjust = 1, size = pt(6), lineheight = 0.9, family = "Arial"),
    # PerioSigDB signatures reporting each genus decreased (left) and increased (right)
    geom_rect(data = b$bars, aes(xmin = xmin, xmax = xmax, ymin = y - 0.35 * ROW_H, ymax = y + 0.35 * ROW_H,
                                 alpha = alpha), fill = b$bars$fill),
    geom_rect(data = b$bar_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA,
              colour = FRAME, linewidth = 0.4),
    geom_segment(data = b$bar_frame, aes(x = zero, xend = zero, y = ymin, yend = ymax), colour = "grey20",
                 linewidth = 0.25),
    geom_segment(data = b$bar_ticks, aes(x = x, xend = x, y = y, yend = y - 0.6), colour = "grey40",
                 linewidth = 0.25),
    geom_text(data = b$bar_ticks, aes(x = x, y = y - 0.9, label = label), vjust = 1, size = pt(5.5),
              colour = "grey15", family = "Arial"),
    geom_richtext(data = b$genera, aes(x = x_label, y = y, label = label), hjust = 1, size = pt(6),
                  fill = NA, label.colour = NA, label.padding = unit(0, "pt"), colour = "grey10", family = "Arial"),
    # cells: not reported (white), 1-2 studies (grey, no count), share increased
    geom_tile(data = filter(b$cells, kind == "none"), aes(x = xc, y = yc), width = cell_w,
              height = ROW_H, fill = "white", colour = "grey88", linewidth = 0.2),
    geom_tile(data = filter(b$cells, kind == "few"), aes(x = xc, y = yc), width = cell_w, height = ROW_H,
              fill = FEW_GREY, colour = "white", linewidth = 0.35),
    geom_tile(data = filter(b$cells, kind == "share"), aes(x = xc, y = yc, fill = share_plot), width = cell_w,
              height = ROW_H, colour = "white", linewidth = 0.35),
    geom_tile(data = filter(b$cells, significant, kind == "share"), aes(x = xc, y = yc), width = 0.84 * cell_w,
              height = 0.78 * ROW_H, fill = NA, colour = "grey10", linewidth = 0.4),
    geom_text(data = filter(b$cells, kind == "share"),
              aes(x = xc, y = yc, label = label,
                  # long counts shrink with the cell width (more columns, narrower cells)
                  size = case_when(nchar(label) <= 4 ~ pt(4.4),
                                   nchar(label) == 5 ~ pt(4.2) * min(1, cell_w / 5.4),
                                   .default = pt(4) * min(1, cell_w / 6.2))),
              colour = "grey10", family = "Arial"),
    # All BugSigDB, same-direction and opposite-direction conditions boxed apart
    geom_rect(data = b$groups, aes(xmin = xmin, xmax = xmax, ymin = b$heat_bottom, ymax = HEAT_TOP), fill = NA,
              colour = FRAME, linewidth = 0.3),
    geom_text(data = b$cols, aes(x = xc, y = y, label = col), angle = 40, hjust = 1, vjust = 0.5,
              size = pt(5.6), colour = "grey10", family = "Arial")
  )
}

#' Colour key and cell legend under the decreased block (whose rows end higher than the increased ones).
legend_layers <- function(x0, top) {
  key_w <- 30
  key <- tibble(x = seq(0, 1, length.out = 101)) |> mutate(xc = x0 + x * key_w)
  items <- tibble(label = c("1–2 studies", "Not reported", "FDR < 0.05"),
                  fill = c(FEW_GREY, "white", "white"), colour = c(NA, "grey70", "grey10"),
                  lw = c(0, 0.2, 0.4)) |>
    mutate(x = x0 + key_w + 9, y = top - 1.2 - (row_number() - 1) * 3.3)
  kw <- 3.6
  kh <- 2.4
  list(
    annotate("text", x = x0, y = top, label = "Studies reporting the genus", hjust = 0, vjust = 1, size = pt(6),
             family = "Arial"),
    geom_tile(data = key, aes(x = xc, y = top - 4.6, fill = x), width = key_w / 100 + 0.02, height = 2),
    annotate("segment", x = x0 + c(0, 0.5, 1) * key_w, y = top - 5.6, yend = top - 6.2, colour = "grey40",
             linewidth = 0.25),
    annotate("text", x = x0 + c(0, 0.5, 1) * key_w, y = top - 6.6,
             label = c("All\ndecreased", "Mixed", "All\nincreased"), vjust = 1, size = pt(5.3), lineheight = 0.9,
             colour = "grey15", family = "Arial"),
    geom_tile(data = items, aes(x = x, y = y), width = kw, height = kh, fill = items$fill, colour = items$colour,
              linewidth = items$lw),
    geom_text(data = items, aes(x = x + kw / 2 + 1, y = y, label = label), hjust = 0, size = pt(5.6),
              family = "Arial")
  )
}

panel_genus_heatmap <- function(tag = "A") {
  units <- c(increased = max(heatmap_columns("increased")$x) + 1, decreased = max(heatmap_columns("decreased")$x) + 1)
  unit <- (HEAT_WIDTH - LEFT - 2 * (BAR_W + BAR_GAP) - LABELS_W - 1.5) / sum(units)
  inc <- block_data("increased", LEFT, unit)
  dec <- block_data("decreased", LEFT + inc$width + LABELS_W, unit)
  ggplot() +
    annotate("text", x = 0, y = 0, label = tag, fontface = "bold", size = pt(9), hjust = 0, vjust = 1,
             family = "Arial") +
    block_layers(inc, "increased", oral_label = TRUE) +
    block_layers(dec, "decreased", oral_label = FALSE) +
    legend_layers(dec$heat_left - 19, dec$heat_bottom - 13) +
    scale_fill_share(guide = "none") +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_size_identity() +
    coord_fixed(xlim = c(0, HEAT_WIDTH), ylim = c(-GENUS_HEATMAP_HEIGHT, 0), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# Figure S1. Pair correlations of the periodontitis-associated genera in healthy plaque (HMP) and in the
# healthy and periodontitis participants of the five PerioSigDB studies with per-sample data.

source("common.R")

PAIR_TYPES <- c("Both increased", "Both decreased", "One increased,\none decreased")
pal_pair <- setNames(c(INC, DEC, "grey75"), PAIR_TYPES)
GROUPS <- c("Health", "Periodontitis")
row_y <- setNames(rev(seq_along(PAIR_TYPES)) - 1, PAIR_TYPES)   # first pair type on top
pt_size <- function(pt) pt / .pt

pairs <- load_fig("fig2_cooc_pairs")
tests <- load_fig("fig2_cooc_tests")
columns <- pairs |> distinct(column, column_order) |> arrange(column_order) |> pull(column)

# column titles: "Study 191\n13 healthy,\n15 periodontitis"
column_title <- function(col) {
  parts <- str_split_fixed(col, "\n", 2)
  paste0(parts[, 1], "\n", str_replace_all(str_remove_all(parts[, 2], "[()]"), ", ", ",\n"))
}
facets <- function(d) {
  mutate(d, column = factor(column_title(column), levels = column_title(columns)),
         group = factor(group, levels = GROUPS))
}

st <- tests |>
  mutate(y = row_y[pair], sig = p < 0.05, ink = if_else(sig, "grey10", "grey65"),
         # "0.00", not "-0.00"; a median near the right edge gets its label on the left of the bar
         median_label = sprintf("%.2f", round(median, 2) + 0), right = median < 0.6) |>
  facets()
# pairs drawn faint where the pair type's median is not significant
pt <- pairs |>
  left_join(select(tests, column, group, pair, p), by = c("column", "group", "pair")) |>
  mutate(y = row_y[pair], alpha = if_else(p < 0.05, 0.85, 0.2)) |>
  facets()

# a light line through each row and a zero line, except in the empty panel (HMP has no periodontitis participants)
rows <- distinct(st, column, group) |> cross_join(tibble(y = row_y))
all_panels <- expand_grid(column = levels(st$column), group = GROUPS) |> facets()
empty <- anti_join(all_panels, distinct(st, column, group), by = c("column", "group"))

fig <- ggplot(pt, aes(x = r, y = y)) +
  geom_hline(data = rows, aes(yintercept = y), colour = GRID, linewidth = 0.3) +
  # zero line in the panels with data only
  geom_vline(data = distinct(st, column, group), aes(xintercept = 0), colour = "grey45", linetype = "dashed",
             linewidth = 0.3) +
  geom_quasirandom(aes(fill = pair, alpha = alpha, group = y), orientation = "y", width = 0.3, shape = 21,
                   size = 0.6, colour = "grey20", stroke = 0.1) +
  geom_segment(data = st, aes(x = median, xend = median, y = y - 0.34, yend = y + 0.34, colour = ink),
               linewidth = 0.6) +
  geom_text(data = st, aes(x = if_else(right, median + 0.04, median - 0.04), y = y + 0.3, label = median_label,
                          colour = ink, hjust = if_else(right, 0, 1)), vjust = 0, size = pt_size(5.2)) +
  geom_text(data = st, aes(x = -1, y = y + 0.38, label = sprintf("p = %s", signif(p, 1)), colour = ink),
            hjust = 0, vjust = 0, size = pt_size(5), fontface = "italic") +
  geom_text(data = empty, aes(x = 0, y = 1.1, label = "No periodontitis\nparticipants"), size = pt_size(6),
            colour = "grey55", lineheight = 0.95) +
  facet_grid(group ~ column, switch = "y") +
  scale_fill_manual(values = pal_pair, guide = "none") +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_x_continuous(breaks = c(-0.5, 0, 0.5)) +
  scale_y_continuous(breaks = row_y, labels = PAIR_TYPES) +
  coord_cartesian(xlim = c(-1.04, 1.04), ylim = c(-0.6, 2.85), expand = FALSE) +
  labs(x = "Correlation of the pair across participants (Spearman, CLR abundance)", y = NULL) +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 5.8, lineheight = 0.95),
        strip.placement = "outside",
        strip.text.x = element_text(size = 6, hjust = 0.5, lineheight = 1.05, margin = margin(0, 0, 3, 0)),
        strip.text.y.left = element_text(size = 7, angle = 90, hjust = 0.5, margin = margin(0, 4, 0, 0)),
        panel.spacing.x = unit(4, "pt"),
        panel.spacing.y = unit(8, "pt"))

save_fig(fig, "figS1", height = 109)

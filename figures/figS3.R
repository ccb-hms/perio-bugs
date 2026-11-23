# Figure S3. Periodontal papers shared between PerioSigDB and other resources (UpSet plot).
#
# The papers come from figures/data/resource_papers.csv, which other_resources.R writes. Papers are matched by
# PMID; a paper without one (a dbBact experiment) counts as a paper of its own. Drawn with ggplot2 and patchwork (ComplexUpset does not work with ggplot2 4).

source("common.R")

RESOURCES <- c("PerioSigDB", "Disbiome", "dbBact", "BugSigDB", "MicrobiomeAnalyst", "Amadis")
COLOUR <- "#332288"  # papers, as in Figure 3A
value_text <- function(...) geom_text(..., size = 5.5 / .pt, colour = TEXT_GREY)

papers <- read_csv(file.path(FIG_DIR, "data", "resource_papers.csv"), col_types = cols(.default = "c")) |>
  filter(resource %in% RESOURCES) |>
  mutate(id = coalesce(pmid, paste(resource, paper)))

# each paper's combination of resources; combinations ordered by size, then by how many resources they span
combos <- papers |>
  group_by(id) |>
  summarise(combo = paste(RESOURCES[RESOURCES %in% resource], collapse = "&"), .groups = "drop") |>
  count(combo) |>
  mutate(k = str_count(combo, "&")) |>
  arrange(desc(n), k, combo) |>
  mutate(x = row_number())

# matrix: one point per combination and resource, filled where the combination includes the resource
y_of <- setNames(rev(seq_along(RESOURCES)), RESOURCES)
matrix <- combos |>
  select(x, combo) |>
  cross_join(tibble(resource = RESOURCES)) |>
  mutate(member = map2_lgl(combo, resource, \(c, r) r %in% str_split_1(c, "&")), y = y_of[resource])
segments <- matrix |> filter(member) |> group_by(x) |> summarise(lo = min(y), hi = max(y))
sizes <- count(distinct(papers, resource, id), resource) |> mutate(y = y_of[resource])

x_scale <- scale_x_continuous(limits = c(0.4, nrow(combos) + 0.6), expand = c(0, 0))
y_scale <- scale_y_continuous(limits = c(0.4, length(RESOURCES) + 0.6), expand = c(0, 0))
stripes <- geom_rect(data = tibble(y = y_of[c(FALSE, TRUE)]), inherit.aes = FALSE,
                     aes(xmin = -Inf, xmax = Inf, ymin = y - 0.5, ymax = y + 0.5), fill = "grey96")

bars <- ggplot(combos, aes(x, n)) +
  geom_col(fill = COLOUR, width = 0.6) +
  value_text(aes(label = n), vjust = -0.5) +
  x_scale +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Papers in exactly\nthese resources") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank())

dots <- ggplot(matrix, aes(x, y)) +
  stripes +
  geom_segment(data = segments, aes(x = x, xend = x, y = lo, yend = hi), colour = COLOUR, linewidth = 0.5) +
  geom_point(aes(colour = member), size = 1.7) +
  scale_colour_manual(values = c(`TRUE` = COLOUR, `FALSE` = "grey85"), guide = "none") +
  x_scale +
  scale_y_continuous(breaks = y_of, labels = names(y_of), limits = c(0.4, length(RESOURCES) + 0.6),
                     expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  # panel.grid.major is set by name in the theme, so blank it by name
  theme(panel.grid.major = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 6.5, colour = "grey10"))

totals <- ggplot(sizes, aes(n, y)) +
  stripes +
  geom_col(fill = COLOUR, width = 0.6, orientation = "y") +
  value_text(aes(label = n), hjust = 1.2) +
  scale_x_reverse(breaks = c(0, 50, 100), expand = c(0, 0)) +
  expand_limits(x = max(sizes$n) * 1.4) +  # room for the largest count's label
  y_scale +
  labs(x = "Papers in resource", y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.y = element_blank())

p <- wrap_plots(plot_spacer(), bars, totals, dots, ncol = 2, widths = c(0.24, 1), heights = c(1, 0.62))
save_fig(p, "figS3", height = 80, width = 120)

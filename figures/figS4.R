# Figure S4. Similarity of BugSigDB signatures to the PerioSigDB consensus signatures, for every condition with
# unadjusted p < 0.05 in any comparison, by body site group. The report leaves out colorectal cancer's mouth studies.

source("common.R")

rows <- load_fig("fig4_sim_rows") |>
  arrange(desc(pos)) |>
  select(Condition, kind, site) |>
  similarity_layout()
units <- load_fig("fig4_sim_units")

sites <- intersect(c("mouth", "gut", "airway", "lung", "eye", "skin", "other"),
                   coalesce(unname(SITE_NAMES[units$site_group]), "other"))

# one x-axis title shared by the four panels
panels <- (similarity_panels(rows, load_fig("fig4_sim_stats"), units, point_size = 0.6) &
             labs(x = SIMILARITY_LABEL) &
             # similarity_panels leaves the theme's major grid at x = 0 and 100
             theme(panel.grid.major = element_blank())) +
  plot_layout(axis_titles = "collect_x")

p <- similarity_legend(sites) / panels + plot_layout(heights = c(0.025, 1))
save_fig(p, "figS4", height = 234)

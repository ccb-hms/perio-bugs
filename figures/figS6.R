# Figure S6. Direction of the periodontitis-associated genera in each BugSigDB condition (direction summary).

source("common.R")

d <- load_fig("figS6_direction_summary")
arrows <- load_fig("figS6_direction_arrows")

# label placement: offset from the point in pt, and horizontal justification (default: 6 pt to the right)
OFFSETS <- tribble(
  ~label,            ~dx, ~dy, ~hjust,
  "COVID-19 gut",    -8,  4,   1,
  "GC",              8,   0,   0,
  "COVID-19 airway", 0,   -13, 0.5,
  "COVID-19",        0,   10,  0.5,
  "PD",              0,   10,  0.5,
  "CRC",             0,   10,  0.5,
  "RA",              0,   10,  0.5,
  "HNC",             0,   -11, 0.5,
  "Smoking",         0,   11,  0.5
)
PT <- 1.12 / 234   # data units per pt: the axis spans 1.12 units over about 234 pt (83 mm) of panel
SITES <- c(mouth = "mouth", gut = "gut", airway = "airway", "mixed (no site ≥ 2/3)" = "mixed sites")
sites <- intersect(names(SITES), d$main_site)

d <- d |>
  left_join(OFFSETS, by = "label") |>
  mutate(dx = coalesce(dx, 6), dy = coalesce(dy, 0), hjust = coalesce(hjust, 0),
         site = as.logical(site),
         main_site = factor(main_site, levels = sites))
pct <- c(0, 0.25, 0.5, 0.75, 1)

p <- ggplot(d, aes(x, y)) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1, fill = "grey92") +
  geom_hline(yintercept = 0.5, colour = "grey55", linetype = "dashed", linewidth = 0.3) +
  geom_vline(xintercept = 0.5, colour = "grey55", linetype = "dashed", linewidth = 0.3) +
  geom_segment(data = arrows, aes(x = x0, y = y0, xend = x1, yend = y1), colour = "grey35", linewidth = 0.3,
               arrow = arrow(length = unit(3, "pt"), type = "closed")) +
  geom_point(aes(size = genera, fill = main_site, shape = shape), colour = "grey15", stroke = 0.3, alpha = 0.9) +
  # conditions upright and dark; their body-site subsets smaller, italic and grey
  geom_text(aes(x = x + dx * PT, y = y + dy * PT, label = label, hjust = hjust,
                size = NULL, fontface = ifelse(site, "italic", "plain"), colour = site),
            size = ifelse(d$site, 5.5, 6.2) / .pt, family = "Arial", show.legend = FALSE) +
  # inside the shaded square, clear of the HNC point at its top edge
  annotate("text", x = 0.99, y = 0.515, label = "Same direction\nas periodontitis", hjust = 1, vjust = 0,
           size = 5.5 / .pt, colour = "grey45", lineheight = 0.95, family = "Arial") +
  scale_shape_identity() +
  scale_colour_manual(values = c("TRUE" = "grey35", "FALSE" = "grey10"), guide = "none") +
  scale_fill_manual(values = pal_site[sites], labels = SITES[sites], name = "Main body site",
                    guide = guide_legend(override.aes = list(shape = 21, size = 2.5), order = 1)) +
  scale_size_area(max_size = 5.5, limits = c(0, max(d$genera)), breaks = c(5, 10, 20), name = "Genera counted",
                  guide = guide_legend(override.aes = list(shape = 21, fill = "white"), order = 2)) +
  scale_x_continuous(breaks = pct, labels = scales::percent) +
  scale_y_continuous(breaks = pct, labels = scales::percent) +
  coord_equal(xlim = c(-0.06, 1.06), ylim = c(-0.06, 1.06), expand = FALSE, clip = "off") +
  labs(x = "Periodontitis-increased genera\nreported increased in the condition",
       y = "Periodontitis-decreased genera\nreported decreased in the condition") +
  theme(panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
        legend.position = "right", legend.direction = "vertical", legend.justification = "top",
        legend.spacing.y = unit(10, "pt"),
        legend.key.spacing.y = unit(2, "pt"))

save_fig(p, "figS6", height = 95, width = 124)

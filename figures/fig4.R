# Figure 4. Which conditions report periodontitis-associated taxa like periodontitis does.
#
# A direction of the periodontitis-increased and -decreased genera in each BugSigDB condition that reports most of
# them in one direction (genus_heatmap.R); B similarity of the BugSigDB signatures of the chosen conditions to the
# PerioSigDB consensus signatures, in all four pairings of direction; C the same conditions as one point each: in
# which direction the condition reports each periodontitis consensus. Rows of B and C are compared with all other
# conditions in BugSigDB, or for a body site row possibly with their units from that site. The report chooses the
# rows (FDR < 0.05 in any column; see sim_main_rows() in report.qmd) and exports them as fig4_rows.

source("common.R")
source("genus_heatmap.R")  # panel_genus_heatmap(), GENUS_HEATMAP_HEIGHT

# label placement in C, in data units from the point, and horizontal justification
LABEL_OFFSETS <- tribble(
  ~short,            ~dx,    ~dy,    ~hjust,
  "CRC",             0,      -0.04,  0.5,
  "HNC",             0,      -0.035, 0.5,
  "RA mouth",        -0.02,  0,      1,
  "COVID-19 airway", 0,      -0.035, 0.5,
  "Smoking",         0.022,  0,      0,
  "COVID-19 gut",    -0.022, 0.012,  1,
  "UC gut",          -0.02,  -0.012, 1
)
UNIT_BREAKS <- c(10, 20, 40)
UNIT_MAX <- 100      # study units drawn at MAX_SIZE; same scale in C and in its legend
MAX_SIZE <- 5
BODY_SITES <- c("mouth", "gut", "airway")
# panel tags sit in a row just above the panel; a bottom margin lifts them above B's two-line header
TAG <- element_text(face = "bold", size = 9, margin = margin(b = 19))

stats <- load_fig("fig4_sim_stats") |> mutate(site = coalesce(site, ""))
# rows of B and points of C, in order (Condition, kind, site, short label)
rows <- load_fig("fig4_rows") |>
  mutate(site = coalesce(site, "")) |>
  similarity_layout() |>
  # B's row labels: the condition in bold (once, on its first row); below, for one body site group, the site and
  # what it is compared with
  mutate(first = Condition != lag(Condition, default = ""),
         site_text = paste0("(", SITE_NAMES[site], " vs ", if_else(kind == "site", SITE_NAMES[site], "all"), ")"),
         label = case_when(kind == "all" ~ paste0("**", abbrev(Condition), "**"),
                           first ~ paste0("**", abbrev(Condition), "**<br><span style='font-size:5.5pt'>",
                                          site_text, "</span>"),
                           .default = paste0("<span style='font-size:5.5pt'>", site_text, "</span>")))

# --- C: one point per row --------------------------------------------------------------------------------------

#' Only similarity above the median (50%) counts. x: the periodontitis-increased consensus, resembled by the
#' condition's increased minus its decreased signatures; y: the periodontitis-decreased consensus, resembled by
#' its decreased minus its increased signatures.
overview_points <- function(rows) {
  ov <- load_fig("fig4_overview")
  pmap(rows, function(Condition, kind, site, short, ...) {
    s <- filter(stats, .data$Condition == !!Condition, .data$kind == !!kind, .data$site == !!site)
    get <- function(direction, perio, col = "mean_rank") {
      v <- s[[col]][s$direction == direction & s$perio == perio]
      if (length(v)) v[1] else NA_real_
    }
    excess <- function(direction, perio) {
      v <- get(direction, perio)
      if (is.na(v)) 0 else max(v - 0.5, 0)
    }
    tibble(short = short,
           px = excess("increased", "increased") - excess("decreased", "increased"),
           py = excess("decreased", "decreased") - excess("increased", "decreased"),
           units = sum(get("increased", "increased", "studies"), get("decreased", "decreased", "studies"),
                       na.rm = TRUE),
           body_site = if (kind == "all") ov$main_site[ov$Condition == Condition & ov$kind == "all"][1]
                       else unname(SITE_NAMES[site]))
  }) |>
    list_rbind() |>
    left_join(LABEL_OFFSETS, by = "short") |>
    # a point without its own placement gets its label centred below it
    mutate(dx = coalesce(dx, 0), dy = coalesce(dy, -0.035), hjust = coalesce(hjust, 0.5))
}

pts <- overview_points(rows)
UNIT_MAX <- max(UNIT_MAX, pts$units)   # never drop a point off the size scale
x0 <- -0.2; x1 <- 0.36; y0 <- -0.34; y1 <- 0.42
pad <- 0.012
# the two zero lines run as arrows to the panel edges, each end labelled with what moving that way means: the top
# and bottom labels centred beyond their arrowheads, the left and right ones flat at the ends, raised clear of the
# line and of the largest point, which sits on it. Each arrow stops short of the panel edge (gap in data units).
gap_x <- 0.012; gap_y <- 0.045
axis_ends <- tibble(x = c(x1 - pad, x0 + pad, 0, 0), y = c(0.032, 0.032, y1 - pad, y0 + pad),
                    hjust = c(1, 0, 0.5, 0.5), vjust = c(0, 0, 1, 0),
                    label = c("Up in condition", "Down in condition", "Down in condition", "Up in condition"))
axis_arrows <- tibble(x = c(x0 + gap_x, 0), xend = c(x1 - gap_x, 0), y = c(0, y0 + gap_y), yend = c(0, y1 - gap_y))

p_b <- ggplot(pts, aes(px, py)) +
  annotate("rect", xmin = 0, xmax = x1, ymin = 0, ymax = y1, fill = "grey92") +
  geom_segment(data = axis_arrows, aes(x = x, xend = xend, y = y, yend = yend), colour = "grey55", linewidth = 0.3,
               arrow = arrow(length = unit(3, "pt"), ends = "both", type = "closed")) +
  geom_text(data = axis_ends, aes(x, y, label = label, hjust = hjust, vjust = vjust), size = 5.6 / .pt,
            colour = "grey40", family = "Arial") +
  geom_point(aes(size = units, fill = body_site), shape = 21, colour = "grey15", stroke = 0.3, alpha = 0.9) +
  geom_text(aes(x = px + dx, y = py + dy, label = short, hjust = hjust), size = 6 / .pt, colour = "grey10",
            family = "Arial") +
  scale_size_area(max_size = MAX_SIZE, limits = c(0, UNIT_MAX), guide = "none") +
  scale_fill_manual(values = pal_site, guide = "none") +
  scale_x_continuous(breaks = c(-0.2, 0, 0.2)) +
  scale_y_continuous(breaks = c(-0.2, 0, 0.2, 0.4)) +
  coord_cartesian(xlim = c(x0, x1), ylim = c(y0, y1), expand = FALSE) +
  # no ticks or numbers: the axis titles name the taxa and the arrow labels say what each direction means
  labs(x = "Perio Increased", y = "Perio Decreased",
       tag = "C") +
  # theme_fig sets panel.grid.major itself, so blanking panel.grid alone leaves it
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        plot.tag = TAG, plot.margin = margin(0, 6, 2, 10))

# --- B: similarity panels --------------------------------------------------------------------------------------

p_a <- similarity_panels(rows, stats, load_fig("fig4_sim_units"), condition = "Cond", perio_name = "Perio",
                         label_bold = rep(FALSE, nrow(rows))) &
  labs(x = SIMILARITY_LABEL)
# the tag row of the first panel makes room for the header, so the panels need no top margin of their own
# small gaps between the column blocks, wide enough to keep "100" and "0" apart; blank the theme's major grid, which similarity_panels leaves at x = 0 and 100
p_a <- p_a & theme(plot.margin = margin(0, 3.5, 2, 3.5), panel.grid.major = element_blank())
p_a[[1]] <- p_a[[1]] + labs(tag = "B") + theme(plot.tag = TAG)
p_a <- p_a + plot_layout(axis_titles = "collect_x")

# --- legends (below B and C): B's mean and body site, C's point sizes ----------------------------------------------------------

legend_row <- local({
  d <- tibble(x = 1:3, site = factor(BODY_SITES, levels = BODY_SITES),
              significance = factor(names(SIGNIFICANCE_FILL), levels = names(SIGNIFICANCE_FILL)),
              units = UNIT_BREAKS)
  p <- ggplot(d, aes(x, x)) +
    geom_point(aes(fill = significance), shape = 21, size = 1.8, colour = "grey10", stroke = 0.4) +
    geom_point(aes(colour = site), size = 1.6) +
    geom_point(aes(size = units), shape = 21, colour = "grey15", fill = "white", stroke = 0.3) +
    scale_fill_manual(values = SIGNIFICANCE_FILL, name = "B: mean and 95% CI") +
    scale_colour_manual(values = pal_site[BODY_SITES], name = "Body site") +
    scale_size_area(max_size = MAX_SIZE, limits = c(0, UNIT_MAX), breaks = UNIT_BREAKS, name = "C: study units") +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2), size = guide_legend(order = 3)) +
    theme(legend.title = element_text(margin = margin(r = 4)), legend.spacing.x = unit(14, "pt"))
  wrap_elements(full = cowplot::get_plot_component(p, "guide-box-top"))
})

similarity <- (wrap_plots(p_a, p_b, widths = c(1.45, 1))) / legend_row + plot_layout(heights = c(1, 0.07))
# A is drawn in mm at a fixed height; B and C get 80 mm, with their legend row below
p <- wrap_plots(wrap_elements(full = panel_genus_heatmap("A")), wrap_elements(full = similarity), ncol = 1,
                heights = unit(c(GENUS_HEATMAP_HEIGHT, 80), "mm")) +
  plot_annotation(theme = theme(plot.margin = margin(2, 0, 0, 0, "mm")))  # room for A's titles at the top
save_fig(p, "fig4", height = GENUS_HEATMAP_HEIGHT + 80 + 2)

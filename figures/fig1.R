# Figure 1. What PerioSigDB contains.
#
# A papers and signatures by publication period; B papers per country; C signatures listing each taxon;
# D signature size; E profiling method of increased and decreased signatures; F the most reported taxa by
# profiling method. (The study groups are in Figure 2C.)

source("common.R")
suppressPackageStartupMessages(library(sf))

pal_period <- c(Papers = "#1baf7a", Signatures = "#4a3aa7")   # the report's colours for panel A
COUNTRY_BINS <- c("1", "2–4", "5–9", "10–19", "20+")
pal_country <- setNames(c("#deebf7", "#9ecae1", "#4292c6", "#2171b5", "#08306b"), COUNTRY_BINS)
# C and D use the report's salmon and slate (its overview figure): the method palette of E and F already
# uses blue (16S) and orange (shotgun), so the usual increased/decreased colours would read as methods
pal_dir <- c(increased = "#f1807e", decreased = "#6182a6")
pt_size <- function(pt) pt / .pt
LABEL_PT <- 5.5

# panel tag at the top-left corner of the whole plot area, margins included, so tags in a row line up
tag <- function(letter) {
  list(labs(tag = letter),
       theme(plot.tag.location = "plot", plot.tag.position = c(0, 1),
             plot.tag = element_text(face = "bold", size = 9, hjust = 0, vjust = 1)))
}

#' white text on dark fills, near-black on light ones
text_colour <- function(fill) {
  rgb <- col2rgb(fill) / 255
  ifelse(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ] < 0.55, "white", "grey10")
}

# --- A: papers and signatures by publication period -----------------------------------------------------------

period <- load_fig("fig1_period") |>
  mutate(type = str_to_sentence(type), period = factor(period, levels = rev(sort(unique(period)))))

p_a <- ggplot(period, aes(x = n, y = period, fill = type)) +
  geom_col(position = position_dodge(0.8, reverse = TRUE), width = 0.76) +
  geom_text(aes(label = n), position = position_dodge(0.8, reverse = TRUE), hjust = -0.3, size = pt_size(LABEL_PT),
            colour = TEXT_GREY) +
  scale_fill_manual(values = pal_period, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Count", y = NULL) +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "inside", legend.position.inside = c(0.98, 0.98), legend.justification = c(1, 1),
        legend.background = element_blank())

# --- B: papers per country ------------------------------------------------------------------------------------------

world <- st_read(file.path(DATA, "fig1_world.geojson"), quiet = TRUE) |>
  left_join(load_fig("fig1_country"), by = c("name_long" = "country")) |>
  mutate(papers_bin = factor(papers_bin, levels = COUNTRY_BINS))

p_b <- ggplot(world) +
  geom_sf(aes(fill = papers_bin), colour = "white", linewidth = 0.08) +
  scale_fill_manual(values = pal_country, breaks = COUNTRY_BINS, na.value = "grey88", name = "Papers") +
  coord_sf(expand = FALSE) +   # longitude and latitude, as in the report
  theme_void(base_size = 7, base_family = "Arial") +
  theme(legend.position = "inside", legend.position.inside = c(0.01, 0.03), legend.justification = c(0, 0),
        legend.key.size = unit(7, "pt"), legend.title = element_text(size = 6.5),
        legend.text = element_text(size = 6), plot.margin = margin(4, 4, 4, 4))

# --- C, D: mirror histograms (increased above the axis, decreased below) ------------------------------------------

mirror_bins <- function(value, direction, width) {
  tibble(value, direction) |>
    mutate(bin = ceiling(value / width)) |>
    count(direction, bin) |>
    mutate(x = (bin - 0.5) * width, y = if_else(direction == "increased", n, -n))
}

# a symmetric log axis, linear around zero, so that bins of 1 and of 100 both show
mirror_hist <- function(bins, width, ticks) {
  list(geom_col(data = bins, aes(x = x, y = y, fill = direction), width = width, colour = "white", linewidth = 0.2),
       geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.3),
       scale_fill_manual(values = pal_dir, breaks = names(pal_dir), labels = c("Increased", "Decreased"),
                         name = "In periodontitis:"),
       scale_y_continuous(transform = scales::pseudo_log_trans(sigma = 1, base = 10),
                          breaks = c(-rev(ticks), ticks), labels = abs))
}

freq <- load_fig("fig1_taxon_freq")
taxon_bins <- mirror_bins(freq$signatures, freq$direction, 4)
bin_x <- function(v) (ceiling(v / 4) - 0.5) * 4

# the 5 taxa most often reported increased, then the 5 most often decreased (shown by method in F)
up <- freq |> filter(direction == "increased") |> slice_max(signatures, n = 5, with_ties = FALSE)
down <- freq |> filter(direction == "decreased", !label %in% up$label) |>
  slice_max(signatures, n = 5, with_ties = FALSE)

# labels as in the report's overview figure, inside the panel. Increased: vertical labels above each taxon's bar,
# all starting at one height clear of every bar they span. Decreased: several top taxa share a bar, so their
# labels go in rows to the right (log-spaced, most reported on top), with a thin line back to each bar
top_lab <- bind_rows(up, down) |>
  mutate(x = bin_x(signatures), label = taxon_md(short_taxon(label))) |>
  left_join(select(taxon_bins, direction, x, y), by = c("direction", "x"))
up_start <- 3 * max(taxon_bins$y[taxon_bins$direction == "increased" & taxon_bins$x >= min(up$signatures) - 4])
up_lab <- filter(top_lab, direction == "increased") |> mutate(label_y = up_start)
down_lab <- filter(top_lab, direction == "decreased") |>
  arrange(desc(signatures)) |>
  mutate(label_y = -3 * 3^(row_number() - 1))
X_LABEL <- 30

p_c <- ggplot() +
  mirror_hist(taxon_bins, 4, c(1, 10, 100)) +
  geom_segment(data = up_lab, aes(x = x, xend = x, y = y, yend = label_y), colour = "grey50", linewidth = 0.2) +
  geom_richtext(data = up_lab, aes(x = x, y = label_y, label = label), angle = 90, hjust = 0, vjust = 0.5,
                size = pt_size(LABEL_PT), fill = NA, label.colour = NA, label.padding = unit(c(0, 0, 0, 1), "pt")) +
  geom_segment(data = down_lab, aes(x = x, xend = X_LABEL, y = y, yend = label_y), colour = "grey50",
               linewidth = 0.2) +
  geom_richtext(data = down_lab, aes(x = X_LABEL, y = label_y, label = label), hjust = 0, vjust = 0.5,
                size = pt_size(LABEL_PT), fill = NA, label.colour = NA, label.padding = unit(c(0, 0, 0, 1), "pt")) +
  coord_cartesian(xlim = c(-2, 80), ylim = c(-500, 3000), expand = FALSE) +
  labs(x = "Signatures listing the taxon", y = "Taxa") +
  theme(legend.position = "none")

sigs <- load_fig("fig1_signatures")

p_d <- ggplot() +
  mirror_hist(mirror_bins(sigs$size, sigs$direction, 5), 5, c(1, 10, 50)) +
  coord_cartesian(xlim = c(-3, 163), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Taxa per signature", y = "Signatures") +
  # one legend for C and D, under D
  theme(legend.position = "bottom", legend.title.position = "left", legend.justification = "center",
        legend.box.spacing = unit(4, "pt"))

# --- E: profiling method of increased and decreased signatures --------------------------------------------------------

n_dir <- count(sigs, direction)
dir_label <- function(d) paste0(str_to_sentence(d), "\n(n = ", n_dir$n[match(d, n_dir$direction)], ")")
method_share <- sigs |>
  count(direction, method) |>
  mutate(share = n / sum(n), .by = direction) |>
  mutate(direction = factor(dir_label(direction), levels = dir_label(c("increased", "decreased"))),
         method = factor(method, levels = METHODS), ink = text_colour(pal_method[as.character(method)]))

p_e <- ggplot(method_share, aes(x = direction, y = share, fill = method, group = method)) +
  geom_col(width = 0.65, colour = "white", linewidth = 0.3, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = if_else(share >= 0.07, scales::percent(share, accuracy = 1), ""), colour = ink),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = pt_size(LABEL_PT)) +
  scale_fill_manual(values = pal_method, guide = "none") +
  scale_colour_identity() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = scales::label_percent(), expand = c(0, 0)) +
  labs(x = NULL, y = "Share of signatures") +
  theme(axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x = element_text(lineheight = 1))

# --- F: the most reported taxa by profiling method (decreased to the left) -------------------------------------------

top <- load_fig("fig1_top_taxa_method")
order <- top |> distinct(taxon, rank) |> arrange(rank)
top <- top |> mutate(y = nrow(order) + 1 - rank, method = factor(method, levels = METHODS))

p_f <- ggplot(top, aes(x = n, y = y, fill = method)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.2, orientation = "y",
           position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.3) +
  # the first five taxa are the most often reported increased, the rest the most often decreased
  geom_hline(yintercept = nrow(order) - 4.5, colour = "grey45", linewidth = 0.3, linetype = "dashed") +
  scale_fill_manual(values = pal_method, breaks = METHODS, labels = METHOD_SHORT[METHODS],
                    name = "Profiling method (E, F)") +
  scale_x_continuous(breaks = seq(-20, 80, 20), labels = abs) +
  scale_y_continuous(breaks = nrow(order) + 1 - order$rank, labels = taxon_md(order$taxon)) +
  coord_cartesian(xlim = c(-26, 92), ylim = c(0.4, nrow(order) + 0.6), expand = FALSE) +
  labs(x = "Signatures", y = NULL) +
  theme(axis.text.y = element_markdown(size = 6),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "inside", legend.position.inside = c(0.99, 0.02), legend.justification = c(1, 0),
        legend.background = element_blank(), legend.title = element_text(size = 6.5))

# --- layout -------------------------------------------------------------------------------------------------------

row_ab <- (p_a + tag("A")) + (p_b + tag("B")) + plot_layout(widths = c(1, 2.05))

# C over D in the first column (same height); E and F run from the top of C to the bottom of D. C's top margin
# holds its taxon labels, and the C, E and F tags share the top line of the row
row_cf <- (p_c + tag("C")) + (p_d + tag("D")) + (p_e + tag("E")) + (p_f + tag("F")) +
  plot_layout(design = c(area(1, 1), area(2, 1), area(1, 2, 2, 2), area(1, 3, 2, 3)),
              widths = c(1, 0.6, 1.45))

fig <- row_ab / row_cf + plot_layout(heights = c(1.7, 2.6))
save_fig(fig, "fig1", height = 144)

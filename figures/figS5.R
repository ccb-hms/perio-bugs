# Figure S5. Periodontitis-associated genera from all PerioSigDB signatures and from sequencing signatures
# only (16S and shotgun).

source("common.R")

NS_GREY <- "grey82"
ANALYSES <- c("All signatures", "Sequencing only (16S, shotgun)")
GROUPS <- c("Increased in periodontitis", "Decreased in periodontitis")
STATUS <- c("FDR < 0.05", "p < 0.05", "p ≥ 0.05")

d <- load_fig("figS5_assay_genera") |>
  mutate(analysis = factor(analysis, levels = ANALYSES),
         row_dir = factor(row_dir, levels = GROUPS),
         status = factor(status, levels = STATUS),
         significant = status %in% c("FDR < 0.05", "p < 0.05"),
         # one row per genus, best p at the top of its group
         taxon = fct_reorder(taxon, row_order))
max_n <- max(d$increased, d$decreased)
p_x <- -max_n * 0.68  # left edge of the p column

bars <- d |>
  pivot_longer(c(increased, decreased), names_to = "sig_direction", values_to = "n") |>
  mutate(n = ifelse(sig_direction == "decreased", -n, n),
         fill = ifelse(significant, sig_direction, "ns"))

p <- ggplot(bars, aes(n, taxon)) +
  geom_col(aes(fill = fill, alpha = status), width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.3) +
  geom_text(data = d, aes(x = p_x, label = p_label, colour = significant), hjust = 0, size = 5.3 / .pt) +
  scale_fill_manual(values = c(increased = INC, decreased = DEC, ns = NS_GREY), breaks = c("increased", "decreased"),
                    labels = c("Reported increased", "Reported decreased"), name = NULL) +
  scale_alpha_manual(values = c("FDR < 0.05" = 1, "p < 0.05" = 0.45, "p ≥ 0.05" = 1), drop = FALSE,
                     labels = c("FDR < 0.05", "p < 0.05 only", "p ≥ 0.05"), name = NULL) +
  scale_colour_manual(values = c(`TRUE` = "grey20", `FALSE` = "grey65"), guide = "none") +
  scale_x_continuous(labels = abs, breaks = c(-25, 0, 25, 50, 75), expand = c(0, 0)) +
  scale_y_discrete(labels = \(x) taxon_md(x), expand = expansion(add = 0.6)) +
  coord_cartesian(xlim = c(-max_n * 0.72, max_n * 1.03), clip = "off") +
  facet_grid(row_dir ~ analysis, scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(order = 1),
         alpha = guide_legend(order = 2, override.aes = list(fill = c("grey20", "grey20", NS_GREY)))) +
  labs(x = "PerioSigDB signatures reporting the genus (left: decreased, right: increased)", y = NULL) +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_markdown(colour = "grey10"),
        strip.text.x = element_text(margin = margin(b = 9)),
        strip.text.y = element_text(angle = 270, hjust = 0.5, margin = margin(l = 4)),
        panel.spacing.x = unit(8, "pt"),
        panel.spacing.y = unit(6, "pt"),
        legend.box = "horizontal",
        legend.box.spacing = unit(6, "pt"))

save_fig(p, "figS5", height = 142, width = 150)

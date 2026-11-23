# Age, sex, smoking and periodontal measures of the health and periodontitis groups in each study: one small plot
# per measure, for Figure 2C. Needs common.R.

paired <- load_fig("fig1_paired")
paired_tests <- load_fig("fig1_paired_tests")

#' Each study's two group means joined by a grey line, with the medians across studies as large points. Below the
#' title: the median difference, starred where the paired test gives p < 0.05. The caption gives the numbers of
#' studies and the p values.
paired_panel <- function(feature, ylab = NULL) {
  t <- filter(paired_tests, .data$feature == !!feature)
  set.seed(1)
  f <- paired |>
    filter(.data$feature == !!feature) |>
    select(Study, group, mean) |>
    pivot_wider(names_from = group, values_from = mean) |>
    drop_na(Health, Periodontitis) |>
    mutate(jitter = runif(n(), -0.04, 0.04))
  pts <- f |> pivot_longer(c(Health, Periodontitis), names_to = "group", values_to = "mean") |>
    mutate(x = (group == "Periodontitis") + jitter)
  med <- tibble(group = c("Health", "Periodontitis"), x = 0:1, mean = c(median(f$Health), median(f$Periodontitis)))
  diff <- paste0(if (t$diff >= 0) "+" else "", signif(t$diff, 2), if (t$p < 0.05) "\\*" else "")
  title <- paste0("**", SHORT_LABELS[[feature]], "**<br>", diff)   # label bold, difference plain below
  ggplot(pts, aes(x = x, y = mean)) +
    geom_line(aes(group = Study), colour = "grey70", linewidth = 0.2, alpha = 0.7) +
    geom_point(aes(colour = group), size = 0.5, alpha = 0.6, stroke = 0) +
    geom_line(data = med, colour = "grey10", linewidth = 0.5) +
    geom_point(data = med, aes(fill = group), shape = 21, size = 2.4, colour = "white", stroke = 0.6) +
    scale_colour_manual(values = pal_group, guide = "none") +
    scale_fill_manual(values = pal_group, guide = "none") +
    scale_x_continuous(breaks = 0:1, labels = c("Health", "Perio.")) +
    scale_y_continuous(breaks = scales::breaks_extended(4)) +
    coord_cartesian(xlim = c(-0.15, 1.15)) +
    labs(x = NULL, y = ylab, title = title) +
    theme(plot.title = element_markdown(face = "plain", lineheight = 1.1), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank())
}

# short plot titles; the caption spells out PD, CAL and BOP
SHORT_LABELS <- c(age = "Age (years)", males = "Males (%)", smokers = "Smokers (%)", pd = "PD (mm)",
                  cal = "CAL (mm)", bop = "BOP (% sites)", plaque = "Plaque (% sites)",
                  supp = "Suppuration (% sites)")

#' One plot per measure, in SHORT_LABELS order, with a y title on the first of each row of `ncol`.
study_group_plots <- function(ncol = 3) {
  feats <- names(SHORT_LABELS)
  map2(feats, if_else(seq_along(feats) %% ncol == 1, "Group mean", NA_character_),
       \(f, y) paired_panel(f, if (is.na(y)) NULL else y))
}

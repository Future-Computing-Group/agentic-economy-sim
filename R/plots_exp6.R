# plots_exp6.R
# ---------------------------------------------------------------------------
# Experiment 6 figures: Mechanism Ablation
#
# Four-panel figure (2x2):
#   (a) Welfare by topology, faceted by load
#   (b) Efficiency by topology, faceted by load
#   (c) Drop rate by topology, faceted by load
#   (d) Median latency by topology, faceted by load
#
# Main figure shows naive architecture; supplementary shows hybrid or
# interaction.  Four mechanisms: random, edf, greedy_ev, market.
# Bootstrap 95% CI error bars.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
})


# Colour palette for mechanisms (greyscale-safe with shape+linetype backup)
palette_mech <- c(
  "Random"    = "#E41A1C",
  "EDF"       = "#FF7F00",
  "Greedy EV" = "#377EB8",
  "Market"    = "#4DAF4A"
)

shape_mech <- c(
  "Random"    = 16,
  "EDF"       = 17,
  "Greedy EV" = 15,
  "Market"    = 18
)

linetype_mech <- c(
  "Random"    = "solid",
  "EDF"       = "dotted",
  "Greedy EV" = "dashed",
  "Market"    = "twodash"
)


#' Prepare Exp6 raw data for plotting with bootstrap CIs.
#'
#' @param raw_df       Per-seed results from exp6_results_raw.
#' @param arch_filter  Architecture to show (default "naive").
#' @return A tibble with mean and CI columns.
exp6_prepare <- function(raw_df, arch_filter = "naive") {
  metrics <- c("median_latency", "p95_latency", "drop_rate",
               "welfare", "mean_price_volatility", "efficiency")

  df <- raw_df %>%
    filter(architecture == arch_filter)

  if ("seed" %in% names(df)) {
    df <- df %>%
      group_by(mechanism, graph_type, load_level) %>%
      summarise(
        across(
          any_of(metrics),
          list(
            mean = \(x) bootstrap_ci(x)$mean,
            lo   = \(x) bootstrap_ci(x)$lo,
            hi   = \(x) bootstrap_ci(x)$hi
          ),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
  } else {
    for (m in metrics) {
      if (m %in% names(df)) {
        df[[paste0(m, "_mean")]] <- df[[m]]
        df[[paste0(m, "_lo")]]   <- df[[m]]
        df[[paste0(m, "_hi")]]   <- df[[m]]
      }
    }
  }

  df %>%
    mutate(
      mechanism  = factor(mechanism,
                          levels = c("random", "edf", "greedy_ev", "market"),
                          labels = c("Random", "EDF", "Greedy EV", "Market")),
      graph_type = factor(graph_type, levels = c("tree", "sp", "entangled")),
      load_level = factor(load_level, levels = c("medium", "high"))
    )
}


# Shared legend guide (no title, compact 2-row layout)
exp6_guide <- guide_legend(title = NULL, nrow = 2, byrow = TRUE)


#' Panel (a): Welfare by topology with bootstrap 95% CI.
plot_exp6_welfare <- function(df) {
  ggplot(df, aes(x = graph_type, y = welfare_mean,
                 colour = mechanism, shape = mechanism,
                 linetype = mechanism, group = mechanism)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = welfare_lo, ymax = welfare_hi),
                  linewidth = 0.6, width = 0.15,
                  position = position_dodge(width = 0.3)) +
    facet_wrap(~ load_level, ncol = 2,
               labeller = labeller(
                 load_level = c(medium = "Medium", high = "High"))) +
    scale_colour_manual(values = palette_mech, guide = exp6_guide) +
    scale_shape_manual(values = shape_mech, guide = exp6_guide) +
    scale_linetype_manual(values = linetype_mech, guide = exp6_guide) +
    labs(x = "DAG topology", y = "Welfare (a.u.)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


#' Panel (b): Efficiency ratio by topology with bootstrap 95% CI.
plot_exp6_efficiency <- function(df) {
  ggplot(df, aes(x = graph_type, y = efficiency_mean,
                 colour = mechanism, shape = mechanism,
                 linetype = mechanism, group = mechanism)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = efficiency_lo, ymax = efficiency_hi),
                  linewidth = 0.6, width = 0.15,
                  position = position_dodge(width = 0.3)) +
    facet_wrap(~ load_level, ncol = 2,
               labeller = labeller(
                 load_level = c(medium = "Medium", high = "High"))) +
    scale_colour_manual(values = palette_mech, guide = exp6_guide) +
    scale_shape_manual(values = shape_mech, guide = exp6_guide) +
    scale_linetype_manual(values = linetype_mech, guide = exp6_guide) +
    labs(x = "DAG topology", y = "Efficiency") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


#' Panel (c): Drop rate by topology with bootstrap 95% CI.
plot_exp6_drop <- function(df) {
  ggplot(df, aes(x = graph_type, y = drop_rate_mean,
                 colour = mechanism, shape = mechanism,
                 linetype = mechanism, group = mechanism)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = drop_rate_lo, ymax = drop_rate_hi),
                  linewidth = 0.6, width = 0.15,
                  position = position_dodge(width = 0.3)) +
    facet_wrap(~ load_level, ncol = 2,
               labeller = labeller(
                 load_level = c(medium = "Medium", high = "High"))) +
    scale_colour_manual(values = palette_mech, guide = exp6_guide) +
    scale_shape_manual(values = shape_mech, guide = exp6_guide) +
    scale_linetype_manual(values = linetype_mech, guide = exp6_guide) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "DAG topology", y = "Drop rate (%)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


#' Panel (d): Median latency by topology with bootstrap 95% CI.
plot_exp6_latency <- function(df) {
  ggplot(df, aes(x = graph_type, y = median_latency_mean,
                 colour = mechanism, shape = mechanism,
                 linetype = mechanism, group = mechanism)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = median_latency_lo, ymax = median_latency_hi),
                  linewidth = 0.6, width = 0.15,
                  position = position_dodge(width = 0.3)) +
    facet_wrap(~ load_level, ncol = 2,
               labeller = labeller(
                 load_level = c(medium = "Medium", high = "High"))) +
    scale_colour_manual(values = palette_mech, guide = exp6_guide) +
    scale_shape_manual(values = shape_mech, guide = exp6_guide) +
    scale_linetype_manual(values = linetype_mech, guide = exp6_guide) +
    labs(x = "DAG topology", y = "Latency (ms)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


#' Combined 2x2 multi-panel figure for Exp6.
#'
#' @param raw_df       Per-seed results from exp6_results_raw.
#' @param arch_filter  Architecture to display (default "naive").
#' @return A patchwork object.
make_exp6_combined <- function(raw_df, arch_filter = "naive") {
  df <- exp6_prepare(raw_df, arch_filter = arch_filter)

  no_legend <- theme(legend.position = "none")
  top_legend <- theme(legend.position = "top",
                      legend.justification = "center",
                      legend.direction = "horizontal",
                      legend.box = "vertical")
  no_xaxis <- theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.ticks.x = element_blank())

  p_wel  <- plot_exp6_welfare(df)    + top_legend + no_xaxis
  p_eff  <- plot_exp6_efficiency(df) + no_legend  + no_xaxis
  p_drop <- plot_exp6_drop(df)       + no_legend
  p_lat  <- plot_exp6_latency(df)    + no_legend

  p_wel + p_eff + p_drop + p_lat +
    plot_layout(ncol = 2, nrow = 2)
}

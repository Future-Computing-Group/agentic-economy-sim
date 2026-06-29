# plots_exp5.R
# ---------------------------------------------------------------------------
# Experiment 5 figures: Hybrid x Governance interaction
#
# Two-panel figure:
#   (a) Price volatility by topology, faceted by architecture x policy
#   (b) Welfare by topology, faceted by architecture x policy
#
# Both panels include bootstrap 95% CI error bars.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
})


# Colour palette for architecture x policy combinations
palette_exp5 <- c(
  "naive / none"    = "#E41A1C",
  "naive / strict"  = "#FF7F00",
  "hybrid / none"   = "#377EB8",
  "hybrid / strict" = "#4DAF4A"
)

shape_exp5 <- c(
  "naive / none"    = 16,
  "naive / strict"  = 17,
  "hybrid / none"   = 15,
  "hybrid / strict" = 18
)


#' Prepare Exp5 raw data for plotting with bootstrap CIs.
#'
#' @param raw_df Per-seed results from exp5_results_raw.
#' @return A tibble with mean and CI columns, plus a combined condition label.
exp5_prepare_with_ci <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "welfare",
               "mean_price_volatility", "coverage", "efficiency")

  grouped <- raw_df %>%
    mutate(condition = paste(architecture, "/", policy)) %>%
    group_by(condition, architecture, policy, graph_type, load_level)

  # Compute bootstrap CIs per condition x topology x load
  grouped %>%
    summarise(
      across(
        all_of(metrics),
        list(
          mean = \(x) bootstrap_ci(x)$mean,
          lo   = \(x) bootstrap_ci(x)$lo,
          hi   = \(x) bootstrap_ci(x)$hi
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    mutate(
      graph_type = factor(graph_type, levels = c("tree", "sp", "entangled")),
      condition  = factor(condition, levels = names(palette_exp5))
    )
}


#' Panel (a): Price volatility by topology and condition.
plot_exp5_volatility <- function(df) {
  exp5_guide <- guide_legend(title = "Condition", nrow = 2, byrow = TRUE)
  ggplot(df, aes(x = graph_type, y = mean_price_volatility_mean,
                 colour = condition, shape = condition, group = condition)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = mean_price_volatility_lo,
                      ymax = mean_price_volatility_hi),
                  linewidth = 0.6, width = 0.15,
                  position = position_dodge(width = 0.3)) +
    scale_colour_manual(values = palette_exp5, guide = exp5_guide) +
    scale_shape_manual(values = shape_exp5, guide = exp5_guide) +
    facet_wrap(~load_level, ncol = 2) +
    labs(x = "DAG topology",
         y = expression(paste("Price vol. (", sigma, ")")),
         colour = "Condition", shape = "Condition") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


#' Panel (b): Welfare by topology and condition.
plot_exp5_welfare <- function(df) {
  ggplot(df, aes(x = graph_type, y = welfare_mean,
                 colour = condition, shape = condition, group = condition)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = welfare_lo, ymax = welfare_hi),
                  linewidth = 0.6, width = 0.15,
                  position = position_dodge(width = 0.3)) +
    scale_colour_manual(values = palette_exp5) +
    scale_shape_manual(values = shape_exp5) +
    facet_wrap(~load_level, ncol = 2) +
    labs(x = "DAG topology", y = "Welfare",
         colour = "Condition", shape = "Condition") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


#' Combined 2x1 multi-panel figure for Exp5.
#'
#' @param raw_df Per-seed results from exp5_results_raw (NOT aggregated).
#' @return A patchwork object.
make_exp5_combined <- function(raw_df) {
  df <- exp5_prepare_with_ci(raw_df)

  no_legend <- theme(legend.position = "none")
  top_legend <- theme(legend.position = "top",
                      legend.justification = "center",
                      legend.direction = "horizontal",
                      legend.box = "vertical")
  no_xaxis <- theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.ticks.x = element_blank())

  p_vol  <- plot_exp5_volatility(df) + top_legend + no_xaxis
  p_welf <- plot_exp5_welfare(df) + no_legend

  p_vol / p_welf +
    plot_layout(ncol = 1)
}

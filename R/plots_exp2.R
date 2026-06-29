# plots_exp2.R
# ---------------------------------------------------------------------------
# Experiment 2 figures -- IEEE TSC style, single-column (~3.5 in width).
#
# Four-panel combined figure with bootstrap 95% CIs:
#   (a) Latency (median with CI)
#   (b) Drop rate (with CI)
#   (c) Deadline satisfaction (with CI)
#   (d) Price volatility (with CI)
#
# Multi-topology scaling: x = N, colour/linetype/shape = topology.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
})


#' Prepare Exp2 raw data with bootstrap CIs for plotting.
exp2_prepare <- function(raw_df) {
  metrics <- c("median_latency", "p95_latency", "drop_rate",
               "mean_price_volatility", "welfare")
  # Add deadline_sat if present
  if ("deadline_sat" %in% names(raw_df)) metrics <- c(metrics, "deadline_sat")

  if ("seed" %in% names(raw_df)) {
    df <- raw_df %>%
      group_by(graph_type, N) %>%
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
    # Derive deadline_sat from drop_rate if not present
    if (!"deadline_sat_mean" %in% names(df) && "drop_rate_mean" %in% names(df)) {
      df <- df %>% mutate(
        deadline_sat_mean = 1 - drop_rate_mean,
        deadline_sat_lo   = 1 - drop_rate_hi,
        deadline_sat_hi   = 1 - drop_rate_lo
      )
    }
  } else {
    df <- raw_df
    for (m in metrics) {
      if (m %in% names(df)) {
        df[[paste0(m, "_mean")]] <- df[[m]]
        df[[paste0(m, "_lo")]]   <- df[[m]]
        df[[paste0(m, "_hi")]]   <- df[[m]]
      }
    }
    if (!"deadline_sat_mean" %in% names(df)) {
      if ("drop_rate" %in% names(df)) {
        df$deadline_sat_mean <- 1 - df$drop_rate
        df$deadline_sat_lo   <- df$deadline_sat_mean
        df$deadline_sat_hi   <- df$deadline_sat_mean
      } else if ("deadline_sat" %in% names(df)) {
        df$deadline_sat_mean <- df$deadline_sat
        df$deadline_sat_lo   <- df$deadline_sat
        df$deadline_sat_hi   <- df$deadline_sat
      }
    }
  }

  df %>%
    mutate(
      graph_type = factor(graph_type, levels = c("tree", "sp", "entangled")),
      N          = as.integer(N)
    )
}

# Shared legend guide
exp2_guide <- guide_legend(title = "Topology")

#' Panel (a): Median latency with bootstrap 95% CI.
plot_exp2_latency <- function(df) {
  ggplot(df, aes(x = N, y = median_latency_mean, colour = graph_type,
                linetype = graph_type, shape = graph_type)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = median_latency_lo, ymax = median_latency_hi),
                  linewidth = 0.6, width = 2) +
    scale_colour_manual(values = palette_topo[c("tree", "sp", "entangled")],
                        guide = exp2_guide) +
    scale_linetype_manual(
      values = c("tree" = "solid", "sp" = "dashed", "entangled" = "dotted"),
      guide = exp2_guide) +
    scale_shape_manual(values = shape_topo[c("tree", "sp", "entangled")],
                       guide = exp2_guide) +
    labs(x = "Number of agents (N)", y = "Latency (ms)") +
    theme_ieee()
}

#' Panel (b): Drop rate with bootstrap 95% CI.
plot_exp2_drop <- function(df) {
  ggplot(df, aes(x = N, y = drop_rate_mean, colour = graph_type,
                linetype = graph_type, shape = graph_type)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = drop_rate_lo, ymax = drop_rate_hi),
                  linewidth = 0.6, width = 2) +
    scale_colour_manual(values = palette_topo[c("tree", "sp", "entangled")],
                        guide = exp2_guide) +
    scale_linetype_manual(
      values = c("tree" = "solid", "sp" = "dashed", "entangled" = "dotted"),
      guide = exp2_guide) +
    scale_shape_manual(values = shape_topo[c("tree", "sp", "entangled")],
                       guide = exp2_guide) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Number of agents (N)", y = "Drop rate (%)") +
    theme_ieee()
}

#' Panel (c): Deadline satisfaction with bootstrap 95% CI.
plot_exp2_deadline_sat <- function(df) {
  ggplot(df, aes(x = N, y = deadline_sat_mean, colour = graph_type,
                linetype = graph_type, shape = graph_type)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = deadline_sat_lo, ymax = deadline_sat_hi),
                  linewidth = 0.6, width = 2) +
    scale_colour_manual(values = palette_topo[c("tree", "sp", "entangled")],
                        guide = exp2_guide) +
    scale_linetype_manual(
      values = c("tree" = "solid", "sp" = "dashed", "entangled" = "dotted"),
      guide = exp2_guide) +
    scale_shape_manual(values = shape_topo[c("tree", "sp", "entangled")],
                       guide = exp2_guide) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(x = "Number of agents (N)", y = "Deadline satisfaction (%)") +
    theme_ieee()
}

#' Panel (d): Price volatility with bootstrap 95% CI.
plot_exp2_volatility <- function(df) {
  ggplot(df, aes(x = N, y = mean_price_volatility_mean, colour = graph_type,
                linetype = graph_type, shape = graph_type)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = mean_price_volatility_lo,
                      ymax = mean_price_volatility_hi),
                  linewidth = 0.6, width = 2) +
    scale_colour_manual(values = palette_topo[c("tree", "sp", "entangled")],
                        guide = exp2_guide) +
    scale_linetype_manual(
      values = c("tree" = "solid", "sp" = "dashed", "entangled" = "dotted"),
      guide = exp2_guide) +
    scale_shape_manual(values = shape_topo[c("tree", "sp", "entangled")],
                       guide = exp2_guide) +
    labs(x = "Number of agents (N)",
         y = expression(paste("Price vol. (", sigma, ")"))) +
    theme_ieee()
}

#' Combined 2x2 multi-panel figure for Exp2.
make_exp2_combined <- function(raw_df) {
  df <- exp2_prepare(raw_df)

  no_legend <- theme(legend.position = "none")
  top_legend <- theme(legend.position = "top",
                      legend.justification = "center",
                      legend.direction = "horizontal")
  no_xaxis <- theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.ticks.x = element_blank())

  p_lat  <- plot_exp2_latency(df)      + top_legend + no_xaxis
  p_drop <- plot_exp2_drop(df)         + no_legend  + no_xaxis
  p_dsat <- plot_exp2_deadline_sat(df) + no_legend
  p_vol  <- plot_exp2_volatility(df)   + no_legend

  p_lat + p_drop + p_dsat + p_vol +
    plot_layout(ncol = 2, nrow = 2)
}

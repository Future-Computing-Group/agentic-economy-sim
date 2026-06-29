# plots_exp3.R
# ---------------------------------------------------------------------------
# Experiment 3 figures -- IEEE TSC style, single-column (~3.5 in width).
#
# Four-panel combined figure with bootstrap 95% CIs:
#   (a) Latency (median with CI)
#   (b) Drop rate (with CI)
#   (c) Service coverage (with CI)
#   (d) Price volatility (with CI)
#
# Governance policies (none/moderate/strict) under medium/high load for
# tree vs entangled topologies.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(RColorBrewer)
  library(patchwork)
  library(scales)
})


#' Prepare Exp3 raw data with bootstrap CIs for plotting.
exp3_prepare <- function(raw_df) {
  metrics <- c("median_latency", "p95_latency", "drop_rate",
               "coverage", "price_volatility_general", "welfare")

  if ("seed" %in% names(raw_df)) {
    df <- raw_df %>%
      group_by(policy, graph_type, load_level) %>%
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
    df <- raw_df
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
      policy     = factor(policy, levels = c("none", "moderate", "strict")),
      load_level = factor(load_level, levels = c("medium", "high")),
      graph_type = factor(graph_type, levels = c("tree", "entangled"))
    )
}

# Shared legend guides
exp3_guide_colour   <- guide_legend(title = "Load level", order = 1)
exp3_guide_linetype <- guide_legend(title = "Topology", order = 2)

#' Panel (a): Median latency with bootstrap 95% CI.
plot_exp3_latency <- function(df) {
  ggplot(df, aes(x = policy, y = median_latency_mean,
                colour = load_level, linetype = graph_type,
                group = interaction(load_level, graph_type))) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.20)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.20)) +
    geom_errorbar(aes(ymin = median_latency_lo, ymax = median_latency_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.20)) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp3_guide_colour) +
    scale_linetype_manual(
      values = c("tree" = "solid", "entangled" = "dashed"),
      guide = exp3_guide_linetype) +
    labs(x = "Policy", y = "Latency (ms)") +
    theme_ieee()
}

#' Panel (b): Drop rate with bootstrap 95% CI.
plot_exp3_drop <- function(df) {
  ggplot(df, aes(x = policy, y = drop_rate_mean,
                colour = load_level, linetype = graph_type,
                group = interaction(load_level, graph_type))) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.20)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.20)) +
    geom_errorbar(aes(ymin = drop_rate_lo, ymax = drop_rate_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.20)) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp3_guide_colour) +
    scale_linetype_manual(
      values = c("tree" = "solid", "entangled" = "dashed"),
      guide = exp3_guide_linetype) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Policy", y = "Drop rate (%)") +
    theme_ieee()
}

#' Panel (c): Service coverage with bootstrap 95% CI.
plot_exp3_coverage <- function(df) {
  ggplot(df, aes(x = policy, y = coverage_mean,
                colour = load_level, linetype = graph_type,
                group = interaction(load_level, graph_type))) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.20)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.20)) +
    geom_errorbar(aes(ymin = coverage_lo, ymax = coverage_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.20)) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp3_guide_colour) +
    scale_linetype_manual(
      values = c("tree" = "solid", "entangled" = "dashed"),
      guide = exp3_guide_linetype) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Policy", y = "Service coverage (%)") +
    theme_ieee()
}

#' Panel (d): Price volatility with bootstrap 95% CI.
plot_exp3_volatility <- function(df) {
  ggplot(df, aes(x = policy, y = price_volatility_general_mean,
                colour = load_level, linetype = graph_type,
                group = interaction(load_level, graph_type))) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.20)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.20)) +
    geom_errorbar(aes(ymin = price_volatility_general_lo,
                      ymax = price_volatility_general_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.20)) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp3_guide_colour) +
    scale_linetype_manual(
      values = c("tree" = "solid", "entangled" = "dashed"),
      guide = exp3_guide_linetype) +
    labs(x = "Policy",
         y = expression(paste("Price volatility (", sigma, ")"))) +
    theme_ieee()
}

#' Combined 2x2 multi-panel figure for Exp3.
make_exp3_combined <- function(raw_df) {
  df <- exp3_prepare(raw_df)

  no_legend <- theme(legend.position = "none")
  top_legend <- theme(legend.position = "top",
                      legend.justification = "center",
                      legend.direction = "horizontal",
                      legend.box = "vertical")
  no_xaxis <- theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.ticks.x = element_blank())

  p_lat  <- plot_exp3_latency(df)    + top_legend + no_xaxis
  p_drop <- plot_exp3_drop(df)       + no_legend  + no_xaxis
  p_cov  <- plot_exp3_coverage(df)   + no_legend
  p_vol  <- plot_exp3_volatility(df) + no_legend

  p_lat + p_drop + p_cov + p_vol +
    plot_layout(ncol = 2, nrow = 2)
}

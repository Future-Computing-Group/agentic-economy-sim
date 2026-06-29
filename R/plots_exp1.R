# plots_exp1.R
# ---------------------------------------------------------------------------
# Experiment 1 figures -- IEEE TSC style, single-column (~3.5 in width).
#
# Four-panel combined figure:
#   (a) Latency (median with bootstrap 95% CI)
#   (b) Drop rate (with bootstrap 95% CI)
#   (c) Utilisation index (with bootstrap 95% CI)
#   (d) Price volatility (with bootstrap 95% CI)
#
# Greyscale-safe: colour encodes load level; linetype also encodes load level.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(RColorBrewer)
  library(patchwork)
  library(scales)
})


#' Prepare Exp1 raw data with bootstrap CIs for plotting.
#'
#' @param raw_df Per-seed results (one row per seed per condition).
#'               Falls back to legacy summary format if no seed column.
#' @return Tibble with mean and CI columns per metric.
exp1_prepare <- function(raw_df) {
  metrics <- c("median_latency", "p95_latency", "drop_rate",
               "utilisation", "mean_price_volatility", "welfare")

  if ("seed" %in% names(raw_df)) {
    # Raw per-seed data: compute bootstrap CIs
    df <- raw_df %>%
      group_by(graph_type, load_level) %>%
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
      )
  } else {
    # Legacy aggregated data: no CIs available, use point estimates
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
      graph_type = factor(graph_type, levels = c("linear", "tree", "sp", "entangled")),
      load_level = factor(load_level, levels = c("low", "medium", "high"))
    )
}

# Shared legend guide
exp1_guide <- guide_legend(title = "Load level")

#' Panel (a): Median latency with bootstrap 95% CI.
plot_exp1_latency <- function(df) {
  ggplot(df, aes(x = graph_type, y = median_latency_mean, colour = load_level,
                linetype = load_level, group = load_level)) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.25)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = median_latency_lo, ymax = median_latency_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.25)) +
    scale_colour_manual(values = palette_load, guide = exp1_guide) +
    scale_linetype_manual(values = linetype_load, guide = exp1_guide) +
    labs(x = "DAG topology", y = "Latency (ms)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Panel (b): Drop rate with bootstrap 95% CI.
plot_exp1_drop <- function(df) {
  ggplot(df, aes(x = graph_type, y = drop_rate_mean, colour = load_level,
                linetype = load_level, group = load_level)) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.25)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = drop_rate_lo, ymax = drop_rate_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.25)) +
    scale_colour_manual(values = palette_load, guide = exp1_guide) +
    scale_linetype_manual(values = linetype_load, guide = exp1_guide) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "DAG topology", y = "Drop rate (%)") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Panel (c): Utilisation index with bootstrap 95% CI.
plot_exp1_util <- function(df) {
  ggplot(df, aes(x = graph_type, y = utilisation_mean, colour = load_level,
                linetype = load_level, group = load_level)) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.25)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = utilisation_lo, ymax = utilisation_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.25)) +
    scale_colour_manual(values = palette_load, guide = exp1_guide) +
    scale_linetype_manual(values = linetype_load, guide = exp1_guide) +
    labs(x = "DAG topology", y = "Utilisation") +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Panel (d): Price volatility with bootstrap 95% CI.
plot_exp1_volatility <- function(df) {
  ggplot(df, aes(x = graph_type, y = mean_price_volatility_mean,
                colour = load_level, linetype = load_level,
                group = load_level)) +
    geom_line(linewidth = 1.0, position = position_dodge(width = 0.25)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = mean_price_volatility_lo,
                      ymax = mean_price_volatility_hi),
                  linewidth = 0.6,
                  width = 0.12, position = position_dodge(width = 0.25)) +
    scale_colour_manual(values = palette_load, guide = exp1_guide) +
    scale_linetype_manual(values = linetype_load, guide = exp1_guide) +
    labs(x = "DAG topology",
         y = expression(paste("Price vol. (", sigma, ")"))) +
    theme_ieee() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Combined 2x2 multi-panel figure for Exp1.
#'
#' @param raw_df Per-seed results (or legacy summary tibble).
#' @return A patchwork object.
make_exp1_combined <- function(raw_df) {
  df <- exp1_prepare(raw_df)

  no_legend <- theme(legend.position = "none")
  top_legend <- theme(legend.position = "top",
                      legend.justification = "center",
                      legend.direction = "horizontal")
  no_xaxis <- theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.ticks.x = element_blank())

  p_lat  <- plot_exp1_latency(df)    + top_legend + no_xaxis
  p_drop <- plot_exp1_drop(df)       + no_legend  + no_xaxis
  p_util <- plot_exp1_util(df)       + no_legend
  p_vol  <- plot_exp1_volatility(df) + no_legend

  p_lat + p_drop + p_util + p_vol +
    plot_layout(ncol = 2, nrow = 2)
}

# plots_exp4.R
# ---------------------------------------------------------------------------
# Experiment 4 figures -- IEEE TSC style, single-column (~3.5 in width).
#
# Two vertical figures (2 panels each, 2 topology facets per panel):
#   Figure A: Latency + Drop rate (operational metrics)
#   Figure B: Welfare + Price volatility (economic metrics)
#
# The four cells of the architecture x smoothing factorial across SP and
# entangled topologies, medium and high load, varying N.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
})


#' Prepare Exp4 raw data with bootstrap CIs for plotting.
exp4_prepare <- function(raw_df) {
  # Both dispersion columns are prepared: the panel plots the trimmed one, the
  # supplement's per-cell table still reports the full run.
  metrics <- c("median_latency", "p95_latency", "drop_rate",
               "welfare", "mean_price_volatility",
               "mean_price_volatility_tail", "efficiency")

  if ("seed" %in% names(raw_df)) {
    df <- raw_df %>%
      group_by(architecture, graph_type, load_level, N) %>%
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
      # The legacy level keeps its place at the end so the experiments that
      # still run it (the sweep, the overhead and faithfulness studies) plot
      # through this preparer unchanged.
      architecture = factor(architecture,
                            levels = c("naive", "naive_ema", "hybrid_noema",
                                       "hybrid_ema", "hybrid"),
                            labels = c("Naive", "Naive EMA", "Slice no EMA",
                                       "Slice EMA", "Hybrid full")),
      graph_type   = factor(graph_type, levels = c("sp", "entangled")),
      load_level   = factor(load_level, levels = c("medium", "high")),
      N            = as.integer(N)
    )
}

# Linetype + shape per cell; smoothing off is solid, smoothing on is broken.
linetype_arch <- c("Naive" = "solid", "Naive EMA" = "dotted",
                   "Slice no EMA" = "longdash", "Slice EMA" = "dashed",
                   "Hybrid full" = "dotdash")
shape_arch    <- c("Naive" = 16, "Naive EMA" = 1, "Slice no EMA" = 17,
                   "Slice EMA" = 2, "Hybrid full" = 5)

# Shared legend guides (no titles to save horizontal space)
exp4_guide_colour <- guide_legend(title = NULL, order = 1)
exp4_guide_arch   <- guide_legend(title = NULL, order = 2)

#' Panel (a): Median latency with bootstrap 95% CI.
plot_exp4_latency <- function(df) {
  ggplot(df, aes(x = N, y = median_latency_mean,
                colour = load_level, linetype = architecture,
                shape = architecture,
                group = interaction(architecture, load_level))) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = median_latency_lo, ymax = median_latency_hi),
                  linewidth = 0.6, width = 2) +
    facet_wrap(~ graph_type, scales = "free_y",
               labeller = labeller(graph_type = c(sp = "SP", entangled = "Entangled"))) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp4_guide_colour) +
    scale_linetype_manual(values = linetype_arch, guide = exp4_guide_arch) +
    scale_shape_manual(values = shape_arch, guide = exp4_guide_arch) +
    labs(x = "Number of agents (N)", y = "Latency (ms)") +
    theme_ieee()
}

#' Panel (b): Drop rate with bootstrap 95% CI.
plot_exp4_drop <- function(df) {
  ggplot(df, aes(x = N, y = drop_rate_mean,
                colour = load_level, linetype = architecture,
                shape = architecture,
                group = interaction(architecture, load_level))) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = drop_rate_lo, ymax = drop_rate_hi),
                  linewidth = 0.6, width = 2) +
    facet_wrap(~ graph_type, scales = "free_y",
               labeller = labeller(graph_type = c(sp = "SP", entangled = "Entangled"))) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp4_guide_colour) +
    scale_linetype_manual(values = linetype_arch, guide = exp4_guide_arch) +
    scale_shape_manual(values = shape_arch, guide = exp4_guide_arch) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Number of agents (N)", y = "Drop rate (%)") +
    theme_ieee()
}

#' Panel (c): Welfare with bootstrap 95% CI.
plot_exp4_welfare <- function(df) {
  ggplot(df, aes(x = N, y = welfare_mean,
                colour = load_level, linetype = architecture,
                shape = architecture,
                group = interaction(architecture, load_level))) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = welfare_lo, ymax = welfare_hi),
                  linewidth = 0.6, width = 2) +
    facet_wrap(~ graph_type, scales = "free_y",
               labeller = labeller(graph_type = c(sp = "SP", entangled = "Entangled"))) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp4_guide_colour) +
    scale_linetype_manual(values = linetype_arch, guide = exp4_guide_arch) +
    scale_shape_manual(values = shape_arch, guide = exp4_guide_arch) +
    labs(x = "Number of agents (N)", y = "Welfare (a.u.)") +
    theme_ieee()
}

#' Panel (d): Price volatility with bootstrap 95% CI.
#'
#' Plotted on the burn-in-trimmed column, which is the column the factorial's
#' decomposition is taken on: a figure and a decomposition that report two
#' different dispersion measurements invite the second to be read off the first.
plot_exp4_volatility <- function(df) {
  ggplot(df, aes(x = N, y = mean_price_volatility_tail_mean,
                colour = load_level, linetype = architecture,
                shape = architecture,
                group = interaction(architecture, load_level))) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = mean_price_volatility_tail_lo,
                      ymax = mean_price_volatility_tail_hi),
                  linewidth = 0.6, width = 2) +
    facet_wrap(~ graph_type, scales = "free_y",
               labeller = labeller(graph_type = c(sp = "SP", entangled = "Entangled"))) +
    scale_colour_manual(values = palette_load[c("medium", "high")],
                        guide = exp4_guide_colour) +
    scale_linetype_manual(values = linetype_arch, guide = exp4_guide_arch) +
    scale_shape_manual(values = shape_arch, guide = exp4_guide_arch) +
    labs(x = "Number of agents (N)",
         y = expression(atop(paste("Price vol. (", sigma, ")"), "after burn-in"))) +
    theme_ieee()
}

#' Exp4 figure A (vertical): Latency + Drop rate.
make_exp4_combined_a <- function(raw_df) {
  df <- exp4_prepare(raw_df)

  top_legend <- theme(legend.position = "top",
                      legend.justification = "center",
                      legend.direction = "vertical",
                      legend.box = "horizontal",
                      legend.box.just = "center")
  no_legend <- theme(legend.position = "none")
  no_xaxis <- theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.ticks.x = element_blank())

  p_lat  <- plot_exp4_latency(df) + top_legend + no_xaxis
  p_drop <- plot_exp4_drop(df)    + no_legend

  p_lat / p_drop
}

#' Exp4 figure B (vertical): Welfare + Price volatility.
make_exp4_combined_b <- function(raw_df) {
  df <- exp4_prepare(raw_df)

  top_legend <- theme(legend.position = "top",
                      legend.justification = "center",
                      legend.direction = "vertical",
                      legend.box = "horizontal",
                      legend.box.just = "center")
  no_legend <- theme(legend.position = "none")
  no_xaxis <- theme(axis.title.x = element_blank(),
                    axis.text.x  = element_blank(),
                    axis.ticks.x = element_blank())

  p_wel <- plot_exp4_welfare(df)    + top_legend + no_xaxis
  p_vol <- plot_exp4_volatility(df) + no_legend

  p_wel / p_vol
}

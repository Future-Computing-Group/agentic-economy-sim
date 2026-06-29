# plots_tufte.R
# ---------------------------------------------------------------------------
# Tufte-style ALTERNATIVE figures for the data-driven experiment plots.
#
# These are additional, opt-in figures (new tar targets writing to fig/tufte/);
# the original plots_exp*.R / fig/exp* outputs are left untouched so the
# manuscript can switch back by reverting one \includegraphics path.
#
# Design (Edward Tufte, "Visual Display of Quantitative Information"):
#   - maximise data-ink: no panel border, no background, no vertical grid;
#     a single faint horizontal reference grid for reading values.
#   - grayscale-first: load level encoded by a sequential grey ramp
#     (light = low -> dark = high), self-documenting and print-safe;
#     linetype kept as a redundant grayscale cue.
#   - thin ink: lighter lines/points/error bars than the IEEE default.
#   - one slim shared legend (collected) instead of per-panel keys + boxes.
#   - plain (un-boxed) strip/panel labels.
# Data preparation is reused verbatim from plots_exp*.R (exp1_prepare, ...).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
})

#' Clean, data-ink-maximised ggplot2 theme (IEEE single-column).
theme_tufte_ieee <- function(base_size = 8) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.25),
      panel.border       = ggplot2::element_blank(),
      axis.line          = ggplot2::element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks         = ggplot2::element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks.length  = grid::unit(2, "pt"),
      strip.background   = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.95), hjust = 0),
      axis.title         = ggplot2::element_text(size = ggplot2::rel(0.95)),
      axis.text          = ggplot2::element_text(size = ggplot2::rel(0.82), colour = "grey25"),
      plot.title         = ggplot2::element_text(size = ggplot2::rel(1.0), face = "bold", hjust = 0),
      plot.margin        = ggplot2::margin(2, 7, 2, 2),
      legend.position    = "bottom",
      legend.key.height  = grid::unit(0.6, "lines"),
      legend.key.width   = grid::unit(1.3, "lines"),
      legend.text        = ggplot2::element_text(size = ggplot2::rel(0.82)),
      legend.title       = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.margin      = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin  = ggplot2::margin(-5, 0, 0, 0)
    )
}

# Sequential grey ramp for ordinal load level (light -> dark = low -> high).
palette_load_tufte <- c("low" = "grey72", "medium" = "grey45", "high" = "grey10")

#' Tufte-style Exp1 figure: structural ablation (topology x load), 2x2 panels.
#' Reuses exp1_prepare() from plots_exp1.R.
make_exp1_tufte <- function(raw_df) {
  df <- exp1_prepare(raw_df)
  dodge <- position_dodge(width = 0.18)

  panel <- function(ycol, locol, hicol, ylab, pct = FALSE) {
    p <- ggplot(df, aes(x = graph_type,
                        y = .data[[ycol]],
                        colour = load_level, linetype = load_level,
                        group = load_level)) +
      geom_line(linewidth = 0.6, position = dodge) +
      geom_point(size = 1.8, position = dodge) +
      geom_errorbar(aes(ymin = .data[[locol]], ymax = .data[[hicol]]),
                    width = 0, linewidth = 0.4, alpha = 0.65, position = dodge) +
      scale_colour_manual(values = palette_load_tufte, name = "Load") +
      scale_linetype_manual(values = linetype_load, name = "Load") +
      labs(x = NULL, y = ylab) +
      theme_tufte_ieee(base_size = 12) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    if (pct) p <- p + scale_y_continuous(labels = percent_format(accuracy = 1))
    p
  }

  # Volatility panel intentionally omitted: the price-volatility / market-clearing
  # story is consolidated in the headline grid (Table III). This figure carries the
  # operational consequences of structural fragility that the table does not show.
  p_lat  <- panel("median_latency_mean", "median_latency_lo", "median_latency_hi", "Latency (ms)")
  p_drop <- panel("drop_rate_mean", "drop_rate_lo", "drop_rate_hi", "Drop rate", pct = TRUE)
  p_util <- panel("utilisation_mean", "utilisation_lo", "utilisation_hi", "Utilisation")

  (p_lat + p_drop + p_util) +
    plot_layout(ncol = 3, guides = "collect") &
    theme(legend.position = "bottom")
}

# --- extra palettes for the remaining figures ---------------------------------
palette_load2_tufte  <- c("medium" = "grey50", "high" = "grey12")          # exp3, exp4 (2 loads)
palette_topo_tufte   <- c("tree" = "grey68", "sp" = "grey42", "entangled" = "grey10")
linetype_topo_tufte  <- c("tree" = "solid", "sp" = "dashed", "entangled" = "dotted")
palette_qual_tufte   <- c("#4878D0", "#EE854A", "#6ACC64", "#956CB4")       # muted; nominal 4-level
# Redundant linetype cue for the nominal 4-level colour figures (exp5/exp6),
# so they survive grayscale printing and read the same way as each other.
linetype_qual_tufte  <- c("solid", "longdash", "dotted", "dotdash")
.sigp <- expression(paste("Price vol. (", sigma[p], ")"))
.bottom <- function(p) p & ggplot2::theme(legend.position = "bottom")
# Bottom legend wrapped to two rows: for the 4-level nominal figures (exp5/exp6),
# whose titled colour+shape+linetype keys overflow a single row at column width.
.bottom2 <- function(p) {
  p & ggplot2::theme(legend.position = "bottom") &
    ggplot2::guides(colour   = ggplot2::guide_legend(nrow = 2),
                    shape    = ggplot2::guide_legend(nrow = 2),
                    linetype = ggplot2::guide_legend(nrow = 2))
}

#' Exp2 (Tufte): scaling, x = N, colour/linetype/shape = topology (grayscale).
make_exp2_tufte <- function(raw_df) {
  df <- exp2_prepare(raw_df)
  panel <- function(yc, lc, hc, ylab, pct = FALSE) {
    p <- ggplot(df, aes(N, .data[[yc]], colour = graph_type, linetype = graph_type,
                        shape = graph_type, group = graph_type)) +
      geom_line(linewidth = 0.45) + geom_point(size = 1.2) +
      geom_errorbar(aes(ymin = .data[[lc]], ymax = .data[[hc]]),
                    width = 0, linewidth = 0.3, alpha = 0.6) +
      scale_colour_manual(values = palette_topo_tufte, name = "Topology") +
      scale_linetype_manual(values = linetype_topo_tufte, name = "Topology") +
      scale_shape_manual(values = shape_topo[c("tree","sp","entangled")], name = "Topology") +
      labs(x = "Agents (N)", y = ylab) + theme_tufte_ieee()
    if (pct) p <- p + scale_y_continuous(labels = percent_format(accuracy = 1))
    p
  }
  p1 <- panel("median_latency_mean","median_latency_lo","median_latency_hi","Latency (ms)")
  p2 <- panel("drop_rate_mean","drop_rate_lo","drop_rate_hi","Drop rate", TRUE)
  p3 <- panel("deadline_sat_mean","deadline_sat_lo","deadline_sat_hi","Deadline sat.", TRUE)
  p4 <- panel("mean_price_volatility_mean","mean_price_volatility_lo","mean_price_volatility_hi", .sigp)
  .bottom((p1 + p2 + p3 + p4) + plot_layout(ncol = 2, guides = "collect"))
}

#' Exp3 (Tufte): governance, x = policy, colour = load (grayscale), linetype = topology.
make_exp3_tufte <- function(raw_df) {
  df <- exp3_prepare(raw_df); dg <- position_dodge(width = 0.18)
  panel <- function(yc, lc, hc, ylab, pct = FALSE) {
    p <- ggplot(df, aes(policy, .data[[yc]], colour = load_level, linetype = graph_type,
                        group = interaction(load_level, graph_type))) +
      geom_line(linewidth = 0.45, position = dg) + geom_point(size = 1.2, position = dg) +
      geom_errorbar(aes(ymin = .data[[lc]], ymax = .data[[hc]]),
                    width = 0, linewidth = 0.3, alpha = 0.6, position = dg) +
      scale_colour_manual(values = palette_load2_tufte, name = "Load") +
      scale_linetype_manual(values = linetype_topo_tufte[c("tree","entangled")], name = "Topology") +
      labs(x = "Policy", y = ylab) + theme_tufte_ieee()
    if (pct) p <- p + scale_y_continuous(labels = percent_format(accuracy = 1))
    p
  }
  p1 <- panel("median_latency_mean","median_latency_lo","median_latency_hi","Latency (ms)")
  p2 <- panel("drop_rate_mean","drop_rate_lo","drop_rate_hi","Drop rate", TRUE)
  p3 <- panel("coverage_mean","coverage_lo","coverage_hi","Coverage", TRUE)
  p4 <- panel("price_volatility_general_mean","price_volatility_general_lo","price_volatility_general_hi", .sigp)
  .bottom((p1 + p2 + p3 + p4) + plot_layout(ncol = 2, guides = "collect"))
}

#' Exp4 (Tufte): architecture, x = N, colour = load (grayscale), linetype/shape = arch, facet by topology.
make_exp4_tufte <- function(raw_df, which = c("a","b")) {
  which <- match.arg(which); df <- exp4_prepare(raw_df)
  panel <- function(yc, lc, hc, ylab, pct = FALSE) {
    p <- ggplot(df, aes(N, .data[[yc]], colour = load_level, linetype = architecture,
                        shape = architecture, group = interaction(architecture, load_level))) +
      geom_line(linewidth = 0.4) + geom_point(size = 1.0) +
      geom_errorbar(aes(ymin = .data[[lc]], ymax = .data[[hc]]),
                    width = 0, linewidth = 0.25, alpha = 0.55) +
      facet_wrap(~ graph_type, scales = "free_y") +
      scale_colour_manual(values = palette_load2_tufte, name = "Load") +
      scale_linetype_manual(values = linetype_arch, name = "Arch.") +
      scale_shape_manual(values = shape_arch, name = "Arch.") +
      labs(x = "Agents (N)", y = ylab) + theme_tufte_ieee()
    if (pct) p <- p + scale_y_continuous(labels = percent_format(accuracy = 1))
    p
  }
  if (which == "a") {
    p1 <- panel("median_latency_mean","median_latency_lo","median_latency_hi","Latency (ms)")
    p2 <- panel("drop_rate_mean","drop_rate_lo","drop_rate_hi","Drop rate", TRUE)
  } else {
    p1 <- panel("welfare_mean","welfare_lo","welfare_hi","Welfare (a.u.)")
    p2 <- panel("mean_price_volatility_mean","mean_price_volatility_lo","mean_price_volatility_hi", .sigp)
  }
  .bottom((p1 / p2) + plot_layout(guides = "collect"))
}

#' Exp5 (Tufte): arch x governance, x = topology, colour/shape/linetype = condition (muted), facet by load.
make_exp5_tufte <- function(raw_df) {
  df <- exp5_prepare_with_ci(raw_df); dg <- position_dodge(width = 0.25)
  lv   <- levels(df$condition)
  cols <- setNames(palette_qual_tufte[seq_along(lv)], lv)
  ltys <- setNames(linetype_qual_tufte[seq_along(lv)], lv)
  panel <- function(yc, lc, hc, ylab) {
    ggplot(df, aes(graph_type, .data[[yc]], colour = condition, shape = condition,
                   linetype = condition, group = condition)) +
      geom_line(linewidth = 0.4, position = dg) + geom_point(size = 1.1, position = dg) +
      geom_errorbar(aes(ymin = .data[[lc]], ymax = .data[[hc]]),
                    width = 0, linewidth = 0.25, alpha = 0.55, position = dg) +
      facet_wrap(~ load_level, ncol = 2) +
      scale_colour_manual(values = cols, name = "Condition") +
      scale_shape_manual(values = shape_exp5, name = "Condition") +
      scale_linetype_manual(values = ltys, name = "Condition") +
      labs(x = "DAG topology", y = ylab) + theme_tufte_ieee() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
  }
  p1 <- panel("mean_price_volatility_mean","mean_price_volatility_lo","mean_price_volatility_hi", .sigp)
  p2 <- panel("welfare_mean","welfare_lo","welfare_hi","Welfare")
  .bottom2((p1 / p2) + plot_layout(ncol = 1, guides = "collect"))
}

#' Exp6 (Tufte): mechanism ablation, x = topology, colour/shape/linetype = mechanism (muted), facet by load.
make_exp6_tufte <- function(raw_df, arch_filter = "naive") {
  df <- exp6_prepare(raw_df, arch_filter = arch_filter); dg <- position_dodge(width = 0.25)
  cols <- setNames(palette_qual_tufte[seq_along(levels(df$mechanism))], levels(df$mechanism))
  panel <- function(yc, lc, hc, ylab, pct = FALSE) {
    p <- ggplot(df, aes(graph_type, .data[[yc]], colour = mechanism, shape = mechanism,
                        linetype = mechanism, group = mechanism)) +
      geom_line(linewidth = 0.4, position = dg) + geom_point(size = 1.0, position = dg) +
      geom_errorbar(aes(ymin = .data[[lc]], ymax = .data[[hc]]),
                    width = 0, linewidth = 0.25, alpha = 0.55, position = dg) +
      facet_wrap(~ load_level, ncol = 2) +
      scale_colour_manual(values = cols, name = "Mechanism") +
      scale_shape_manual(values = shape_mech, name = "Mechanism") +
      scale_linetype_manual(values = linetype_mech, name = "Mechanism") +
      labs(x = "DAG topology", y = ylab) + theme_tufte_ieee() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    if (pct) p <- p + scale_y_continuous(labels = percent_format(accuracy = 1))
    p
  }
  p1 <- panel("welfare_mean","welfare_lo","welfare_hi","Welfare (a.u.)")
  p2 <- panel("efficiency_mean","efficiency_lo","efficiency_hi","Efficiency")
  p3 <- panel("drop_rate_mean","drop_rate_lo","drop_rate_hi","Drop rate", TRUE)
  p4 <- panel("median_latency_mean","median_latency_lo","median_latency_hi","Latency (ms)")
  .bottom2((p1 + p2 + p3 + p4) + plot_layout(ncol = 2, guides = "collect"))
}

# plot_nodelevel.R
# ---------------------------------------------------------------------------
# Node-level twins of the Tufte figures.
#
# Every figure function in plots_tufte.R reads a per-tier results target. The
# manuscript reports the node-level instances instead (T, S and X, eight nodes
# each, at two congestion levels of which the calibrated one is reported), so
# each data figure it includes needs a twin reading the node-level target of
# the same experiment. The per-tier functions and their targets are untouched:
# these are additional functions, writing to fig/node/.
#
# Shared with plots_tufte.R by call rather than by copy: theme_tufte_ieee(),
# the grey load ramps, the topology palettes (re-keyed here to the printed
# instance names) and the two-row legend helper .bottom2(). mean_ci95() from
# plot_helpers.R carries the intervals.
#
# Printed names only. The store's arm strings never reach an axis or a legend;
# the label maps below carry the manuscript's own words for the same arms.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
})

# --- printed names ---------------------------------------------------------

# The instance order is the factor order: T, S, X.
node_instance_names <- c("tree" = "T", "sp" = "S", "entangled" = "X")

# The 2x2 architecture factorial, in the words the supplement uses for it.
node_arch_names <- c("naive"        = "uncontracted",
                     "naive_ema"    = "uncontracted + EMA",
                     "hybrid_noema" = "contracted",
                     "hybrid_ema"   = "contracted + EMA")

# Architecture x governance crosses one contracted arm against one
# uncontracted one and names them by the contraction alone.
node_arch2_names <- c("naive" = "uncontracted", "hybrid_ema" = "contracted")

# The six coordinate-cap levels, by their determinant rather than their code.
node_policy_names <- c("none"             = "none",
                       "trust"            = "trust",
                       "locality"         = "jurisdiction",
                       "role"             = "role class",
                       "residency"        = "residency",
                       "residency_sliced" = "residency, sliced")

# The zero cap, where governance is a two-level factor.
node_cap2_names <- c("none" = "no cap", "locality" = "zero cap")

# The four ablation levels, then the two tuned arms.
node_mech_names <- c("random" = "random", "edf" = "EDF",
                     "greedy_ev" = "value-greedy", "market" = "market")
node_tuned_names <- c("posted_price" = "posted price (tuned)",
                      "market"       = "market (tuned)")

# --- scales, re-keyed from the per-tier palettes ---------------------------
#
# R/ is sourced in file order and plots_tufte.R follows this file, so each of
# these reads its source palette when the figure is drawn rather than when the
# file is sourced.

palette_instance_node <- function()
  setNames(unname(palette_topo_tufte),
           node_instance_names[names(palette_topo_tufte)])
linetype_instance_node <- function()
  setNames(unname(linetype_topo_tufte),
           node_instance_names[names(linetype_topo_tufte)])
shape_instance_node <- function()
  setNames(unname(shape_topo[names(node_instance_names)]),
           unname(node_instance_names))

# Six nominal levels for the mechanism figure: the four-level muted palette,
# extended in the same family, with the redundant grayscale cues to match.
palette_arm_node  <- function() c(palette_qual_tufte, "#D65F5F", "#8C613C")
linetype_arm_node <- function() c(linetype_qual_tufte, "twodash", "dashed")
shape_arm_node    <- function() 15:20

#' Label a run frame's instance column, in T / S / X order.
#'
#' @param df A node-level results frame carrying `graph_type`.
#' @return The frame with an added `instance` factor.
node_instance_col <- function(df) {
  dplyr::mutate(df, instance = factor(node_instance_names[as.character(graph_type)],
                                      levels = unname(node_instance_names)))
}

#' Label a column from a name map, keeping the map's order as factor levels.
node_label_col <- function(x, map) factor(map[as.character(x)], levels = unname(map))

#' Cell means with 95% intervals, one row per cell and metric.
#'
#' @param df      Per-seed node-level rows.
#' @param by      Grouping columns.
#' @param metrics Response columns to summarise.
#' @return A tibble carrying `<metric>_mean`, `_lo` and `_hi`.
node_plot_ci <- function(df, by, metrics) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(metrics),
                                   list(mean = \(x) mean_ci95(x)$mean,
                                        lo   = \(x) mean_ci95(x)$lo,
                                        hi   = \(x) mean_ci95(x)$hi),
                                   .names = "{.col}_{.fn}"),
                     .groups = "drop")
}

#' One panel: a response with its interval over the figure's own aesthetics.
#'
#' A response one arm does not carry (the tuned arms have no drop rate) is left
#' in the frame as a missing value and skipped by the geoms rather than filtered
#' out of it: dropping the rows would leave that panel with fewer factor levels
#' than its siblings, and a collected legend would then be drawn twice.
#'
#' @param df      A frame from node_plot_ci().
#' @param mapping The figure's plot-level aesthetics, x included.
#' @param ycol    Response stem, without the `_mean` / `_lo` / `_hi` suffix.
#' @param ylab    Axis label.
#' @param pct     Print the y axis as a percentage.
#' @param acc     Percent-label accuracy; 1 unless the spread is narrower.
#' @param facet   Column to facet over, or NULL.
#' @param dodge_w Dodge width; 0 for a continuous x.
#' @param base    Base font size.
#' @param xlab    x axis label, NULL for none.
#' @return A ggplot.
node_panel <- function(df, mapping, ycol, ylab, pct = FALSE, facet = NULL,
                       dodge_w = 0.22, base = 8, xlab = NULL, acc = 1) {
  pos <- if (dodge_w > 0) position_dodge(width = dodge_w) else position_identity()
  p <- ggplot(df, mapping) +
    geom_line(aes(y = .data[[paste0(ycol, "_mean")]]),
              linewidth = 0.45, position = pos, na.rm = TRUE) +
    geom_point(aes(y = .data[[paste0(ycol, "_mean")]]),
               size = 1.2, position = pos, na.rm = TRUE) +
    geom_errorbar(aes(ymin = .data[[paste0(ycol, "_lo")]],
                      ymax = .data[[paste0(ycol, "_hi")]]),
                  width = 0, linewidth = 0.3, alpha = 0.6, position = pos,
                  na.rm = TRUE) +
    labs(x = xlab, y = ylab) +
    theme_tufte_ieee(base_size = base)
  if (!is.null(facet)) p <- p + facet_wrap(stats::as.formula(paste("~", facet)), ncol = 2)
  if (pct) p <- p + scale_y_continuous(labels = percent_format(accuracy = acc))
  p
}

# Medium then high, the order the supplement's facets are read in.
node_load2 <- function(x) factor(as.character(x), levels = c("medium", "high"))

# ===========================================================================
# Structure: latency, drop rate and admitted volume by instance and load
# ===========================================================================

#' Node-level Exp.1 figure: the three instances at three loads.
#'
#' @param raw_df Per-seed rows from node_exp1_results_raw.
#' @return A patchwork of three panels.
make_node_exp1_tufte <- function(raw_df) {
  df <- node_plot_ci(node_instance_col(dplyr::bind_rows(raw_df)),
                     c("instance", "load_level"),
                     c("median_latency", "drop_rate", "tokens_admitted"))
  df$load_level <- factor(as.character(df$load_level),
                          levels = c("low", "medium", "high"))
  base_aes <- aes(x = instance, colour = load_level, linetype = load_level,
                  group = load_level)
  pan <- function(y, lab, pct = FALSE)
    node_panel(df, base_aes, y, lab, pct = pct, dodge_w = 0.18, base = 12) +
      scale_colour_manual(values = palette_load_tufte, name = "Load") +
      scale_linetype_manual(values = linetype_load, name = "Load")

  # Price volatility is not a panel here; the supplement's Exp.1 table carries
  # it, and the figure carries the operational consequences instead.
  .bottom((pan("median_latency", "Latency (ms)") +
           pan("drop_rate", "Drop rate", TRUE) +
           pan("tokens_admitted", "Admitted volume")) +
          plot_layout(ncol = 3, guides = "collect"))
}

# ===========================================================================
# Scaling: the population sweep
# ===========================================================================

#' Node-level Exp.2 figure: the population sweep at one load.
#'
#' The sweep's instrument is the onset of a non-zero price series, which is a
#' property of a single load: pooling the two would average one instance's
#' pre-onset zeros into the other's dispersion. The medium-load grid is the one
#' the surrounding text reads its sequence off.
#'
#' The node store carries no deadline-satisfaction column. The fourth panel
#' carries the sweep's own onset instrument instead, under its own name: the
#' fraction of rounds in which the bottleneck node binds, which is what the
#' price series leaves the reserve on.
#'
#' @param raw_df Per-seed rows from node_exp2_results_raw.
#' @param load   Load level to plot.
#' @return A patchwork of four panels.
make_node_exp2_tufte <- function(raw_df, load = "medium") {
  df <- dplyr::bind_rows(raw_df) %>%
    dplyr::filter(.data$load_level == load) %>%
    node_instance_col() %>%
    node_plot_ci(c("instance", "N"),
                 c("median_latency", "drop_rate", "binding_fraction",
                   "mean_price_volatility"))
  base_aes <- aes(x = N, colour = instance, linetype = instance,
                  shape = instance, group = instance)
  pan <- function(y, lab, pct = FALSE, acc = 1)
    node_panel(df, base_aes, y, lab, pct = pct, dodge_w = 0,
               xlab = "Agents (N)", acc = acc) +
      scale_colour_manual(values = palette_instance_node(), name = "Instance") +
      scale_linetype_manual(values = linetype_instance_node(), name = "Instance") +
      scale_shape_manual(values = shape_instance_node(), name = "Instance")

  .bottom((pan("median_latency", "Latency (ms)") +
           pan("drop_rate", "Drop rate", TRUE) +
           pan("binding_fraction", "Binding fraction", TRUE) +
           pan("mean_price_volatility", .sigp)) +
          plot_layout(ncol = 2, guides = "collect"))
}

# ===========================================================================
# Governance: the six coordinate-cap levels
# ===========================================================================

#' Node-level Exp.3 figure: the cap levels on the three instances at two loads.
#'
#' Coverage is the manuscript's own: the share of offered tasks served, which
#' is the complement of the drop rate rather than the served share of what was
#' admitted. Uniform leaf shares, the mix the exactness question is asked at;
#' no contrast in this experiment crosses mixes.
#'
#' @param raw_df Per-seed rows from node_exp3_results_raw.
#' @return A patchwork of four panels.
make_node_exp3_tufte <- function(raw_df) {
  df <- dplyr::bind_rows(raw_df) %>%
    dplyr::filter(.data$leaf_mix == "uniform") %>%
    dplyr::mutate(coverage = 1 - .data$drop_rate) %>%
    node_instance_col() %>%
    dplyr::mutate(cap = node_label_col(.data$policy, node_policy_names),
                  load_level = node_load2(.data$load_level)) %>%
    node_plot_ci(c("instance", "cap", "load_level"),
                 c("median_latency", "drop_rate", "coverage",
                   "mean_price_volatility"))
  base_aes <- aes(x = cap, colour = load_level, linetype = instance,
                  shape = instance, group = interaction(load_level, instance))
  pan <- function(y, lab, pct = FALSE, xlab = NULL)
    node_panel(df, base_aes, y, lab, pct = pct, dodge_w = 0.3, xlab = xlab) +
      scale_colour_manual(values = palette_load2_tufte, name = "Load") +
      scale_linetype_manual(values = linetype_instance_node(), name = "Instance") +
      scale_shape_manual(values = shape_instance_node(), name = "Instance") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))

  .bottom2((pan("median_latency", "Latency (ms)") +
            pan("drop_rate", "Drop rate", TRUE) +
            pan("coverage", "Coverage", TRUE, "Coordinate cap") +
            pan("mean_price_volatility", .sigp, xlab = "Coordinate cap")) +
           plot_layout(ncol = 2, guides = "collect"))
}

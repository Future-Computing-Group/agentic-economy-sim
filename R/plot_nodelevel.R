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

# The four ablation levels, then the tuned arms. The deadline-priority posted
# price is tuned too but not drawn: it tracks the arrival-order arm to within
# the plotted resolution in every cell, so it would only overprint it.
node_mech_names <- c("random" = "random", "edf" = "EDF",
                     "greedy_ev" = "value-greedy", "market" = "market")
node_tuned_names <- c("posted_price"      = "posted price, value-ranked (tuned)",
                      "posted_price_fcfs" = "posted price, arrival-order (tuned)",
                      "market"            = "market (tuned)")

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
palette_arm_node  <- function() c(palette_qual_tufte, "#D65F5F", "#DC7EC0",
                                  "#8C613C")
linetype_arm_node <- function() c(linetype_qual_tufte, "twodash", "42",
                                  "dashed")
shape_arm_node    <- function() c(15:19, 8, 20)

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

# ===========================================================================
# Architecture: the 2x2 factorial
# ===========================================================================

# The cleared series is the same post-burn-in dispersion measured before the
# reporting filter, so the two volatility panels are read against each other.
.sigp_cleared <- expression(atop(paste("Price vol. (", sigma[p], ")"),
                                 "cleared, after burn-in"))

#' Node-level Exp.4 figures: the four factorial arms.
#'
#' Panel set "a" is operational (latency, drop rate); "b" is economic (welfare,
#' and the post-burn-in price volatility of the agent-facing series beside the
#' cleared one it is a filtered copy of).
#'
#' @param raw_df Per-seed rows from node_exp4_results_raw.
#' @param which  "a" or "b".
#' @return A patchwork.
make_node_exp4_tufte <- function(raw_df, which = c("a", "b")) {
  which <- match.arg(which)
  df <- dplyr::bind_rows(raw_df) %>%
    node_instance_col() %>%
    dplyr::mutate(arm = node_label_col(.data$architecture, node_arch_names),
                  load_level = node_load2(.data$load_level)) %>%
    node_plot_ci(c("instance", "arm", "load_level"),
                 c("median_latency", "drop_rate", "welfare",
                   "mean_price_volatility_tail", "price_volatility_cleared"))
  base_aes <- aes(x = instance, colour = load_level, linetype = arm,
                  shape = arm, group = interaction(arm, load_level))
  pan <- function(y, lab, pct = FALSE)
    node_panel(df, base_aes, y, lab, pct = pct, dodge_w = 0.3) +
      scale_colour_manual(values = palette_load2_tufte, name = "Load") +
      scale_linetype_manual(values = setNames(linetype_qual_tufte,
                                              unname(node_arch_names)),
                            name = NULL) +
      scale_shape_manual(values = setNames(15:18, unname(node_arch_names)),
                         name = NULL)

  fig <- if (which == "a") {
    (pan("median_latency", "Latency (ms)") / pan("drop_rate", "Drop rate", TRUE)) +
      plot_layout(guides = "collect")
  } else {
    (pan("welfare", "Welfare (a.u.)") /
       (pan("mean_price_volatility_tail", .sigp_tail) +
          pan("price_volatility_cleared", .sigp_cleared))) +
      plot_layout(guides = "collect")
  }
  .bottom2(fig) &
    ggplot2::theme(legend.box = "vertical",
                   legend.spacing.y = ggplot2::unit(0, "pt"))
}

# ===========================================================================
# Architecture x governance
# ===========================================================================

#' Node-level Exp.5 figure: the contraction crossed with a zero cap.
#'
#' @param raw_df Per-seed rows from node_exp5_results_raw.
#' @return A patchwork of two rows, faceted by load.
make_node_exp5_tufte <- function(raw_df) {
  df <- dplyr::bind_rows(raw_df) %>%
    node_instance_col() %>%
    dplyr::mutate(
      condition = factor(
        paste(node_arch2_names[as.character(.data$architecture)],
              node_cap2_names[as.character(.data$policy)], sep = ", "),
        levels = as.vector(outer(node_arch2_names, node_cap2_names,
                                 paste, sep = ", "))),
      load_level = node_load2(.data$load_level)) %>%
    node_plot_ci(c("instance", "condition", "load_level"),
                 c("greedy_exact_incidence", "welfare"))
  base_aes <- aes(x = instance, colour = condition, shape = condition,
                  linetype = condition, group = condition)
  pan <- function(y, lab, pct = FALSE)
    node_panel(df, base_aes, y, lab, pct = pct, dodge_w = 0.25,
               facet = "load_level", xlab = "Instance") +
      scale_colour_manual(values = setNames(palette_qual_tufte,
                                            levels(df$condition)),
                          name = "Condition") +
      scale_shape_manual(values = setNames(15:18, levels(df$condition)),
                         name = "Condition") +
      scale_linetype_manual(values = setNames(linetype_qual_tufte,
                                              levels(df$condition)),
                            name = "Condition")

  .bottom2((pan("greedy_exact_incidence", "Exactness shortfall") /
              pan("welfare", "Welfare (a.u.)")) +
           plot_layout(ncol = 1, guides = "collect"))
}

# ===========================================================================
# Mechanism
# ===========================================================================

#' Node-level Exp.6 figure: the ablation levels and the tuned arms.
#'
#' The reported congestion level is the calibrated one, under the uncontracted
#' architecture. The ascending arm is retired and appears nowhere. The tuned
#' arms (the value-ranked and arrival-order posted prices and the market) come
#' from the tuned table, whose knob was chosen on seeds disjoint from the
#' ablation's; that table carries no drop rate, so those arms have no point in
#' the drop panel.
#'
#' @param raw_df   Per-seed rows from node_exp6_results_raw.
#' @param tuned_df One tuned row per cell and mechanism, node_exp6_tuned.
#' @return A patchwork of four panels, faceted by load.
make_node_exp6_tufte <- function(raw_df, tuned_df) {
  metrics <- c("welfare", "alloc_ratio_true", "drop_rate", "median_latency")
  arms <- c(unname(node_mech_names), unname(node_tuned_names))

  ablation <- dplyr::bind_rows(raw_df) %>%
    dplyr::filter(.data$architecture == "naive",
                  .data$congestion == "calibrated",
                  .data$mechanism %in% names(node_mech_names)) %>%
    node_instance_col() %>%
    dplyr::mutate(arm = node_mech_names[as.character(.data$mechanism)]) %>%
    node_plot_ci(c("instance", "arm", "load_level"), metrics)

  tuned <- dplyr::bind_rows(tuned_df) %>%
    dplyr::filter(.data$architecture == "naive",
                  .data$congestion == "calibrated",
                  .data$mechanism %in% names(node_tuned_names)) %>%
    node_instance_col() %>%
    dplyr::mutate(arm = node_tuned_names[as.character(.data$mechanism)],
                  drop_rate = NA_real_) %>%
    node_plot_ci(c("instance", "arm", "load_level"), metrics)

  df <- dplyr::bind_rows(ablation, tuned) %>%
    dplyr::mutate(arm = factor(.data$arm, levels = arms),
                  load_level = node_load2(.data$load_level))
  base_aes <- aes(x = instance, colour = arm, shape = arm, linetype = arm,
                  group = arm)
  pan <- function(y, lab, pct = FALSE)
    node_panel(df, base_aes, y, lab, pct = pct, dodge_w = 0.3,
               facet = "load_level", xlab = "Instance") +
      scale_colour_manual(values = setNames(palette_arm_node(), arms),
                          name = "Arm", drop = FALSE) +
      scale_shape_manual(values = setNames(shape_arm_node(), arms),
                         name = "Arm", drop = FALSE) +
      scale_linetype_manual(values = setNames(linetype_arm_node(), arms),
                            name = "Arm", drop = FALSE)

  fig <- (pan("welfare", "Welfare (a.u.)") +
          pan("alloc_ratio_true", "Allocative ratio") +
          pan("drop_rate", "Drop rate", TRUE) +
          pan("median_latency", "Latency (ms)")) +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(
      caption = "Calibrated congestion level, uncontracted architecture.",
      theme = ggplot2::theme(
        plot.caption = ggplot2::element_text(size = 6, colour = "grey35",
                                             hjust = 0)))
  # Four rows, so the seven arms fall in two columns: the tuned labels are too
  # long for a third column at the column width the figure is printed at.
  fig & ggplot2::theme(legend.position = "bottom") &
    ggplot2::guides(colour   = ggplot2::guide_legend(nrow = 4),
                    shape    = ggplot2::guide_legend(nrow = 4),
                    linetype = ggplot2::guide_legend(nrow = 4))
}

# calibrate_operating_point.R
# ---------------------------------------------------------------------------
# The sweep behind _targets.R's per-topology n_agents vector.
#
# The operating point is a calibration of the environment, not a result: at high
# load the market must contend (bottleneck offered load rho in [1.1, 1.7])
# without collapsing, at medium load it must still clear, and at low load it
# must be slack. The knob is the agent count; capacities and deadlines are
# fixed. This runs that sweep so the chosen counts can be re-derived rather than
# taken on trust, and it reads the criterion from rho_bottleneck() in
# sim_helpers.R, the same definition tests/testthat/test-operating-point.R pins.
#
# Measured on the naive arm only: no cross-arm quantity enters the criterion.
# Full sweep (3 topologies x 2 loads x 10 counts x 3 seeds x 100 rounds) takes
# roughly ten minutes on eight cores; narrow it with the arguments to probe one
# topology.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

#' Sweep the agent count and report the operating-point criterion per cell.
#'
#' @param graph_types DAG topologies to sweep.
#' @param n_grid      Agent counts to try.
#' @param load_levels Load regimes to measure.
#' @param seeds       Monte Carlo seeds.
#' @param n_rounds    Rounds per run.
#' @return A tibble, one row per (graph_type, load_level, N): rho, the mean and
#'   worst-seed clearing fraction, the drop rate, the across-tier mean
#'   utilisation and the mean unit cost.
operating_point_sweep <- function(graph_types = c("tree", "sp", "entangled"),
                                  n_grid = c(20L, 30L, 35L, 40L, 50L, 55L,
                                             60L, 75L, 90L, 100L),
                                  load_levels = c("medium", "high"),
                                  seeds = 1:3, n_rounds = 100L) {
  grid <- tidyr::expand_grid(graph_type = graph_types, load_level = load_levels,
                             N = n_grid, seed = seeds)
  rows <- purrr::pmap(grid, function(graph_type, load_level, N, seed) {
    res <- exp4_run_single("naive", graph_type, load_level, N = N, seed = seed,
                           n_rounds = n_rounds)
    tibble(graph_type = graph_type, load_level = load_level, N = N,
           rho = rho_bottleneck(graph_type, N, load_level),
           clearing_fraction = res$clearing_fraction, drop_rate = res$drop_rate,
           utilisation = res$utilisation, unit_cost = res$mean_unit_cost)
  })

  bind_rows(rows) %>%
    group_by(graph_type, load_level, N, rho) %>%
    summarise(clearing_min  = min(clearing_fraction),
              clearing_mean = mean(clearing_fraction),
              drop_rate     = mean(drop_rate),
              utilisation   = mean(utilisation),
              unit_cost     = mean(unit_cost),
              .groups = "drop")
}

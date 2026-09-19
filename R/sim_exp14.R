# sim_exp14.R
# ---------------------------------------------------------------------------
# Experiment 14: one-at-a-time parameter sensitivity of the volatility-reduction
# result. The reduction is produced by the hybrid integrator relative to the
# naive baseline on the SAME per-tier price-volatility metric
# (mean_price_volatility). This experiment tests whether that result is robust
# to the baseline parameter choices. This driver varies each of a set of
# existing exp4 knobs one at a time around its baseline, at a representative
# volatile condition, and reports the resulting reduction.
#
# No new market mechanism is introduced: every knob (cap_scale, integ_efficiency,
# integ_eta, lambda_l) is an existing, separately-tested parameter of
# exp4_run_single(). The reduction is computed on the fair metric only.
# ---------------------------------------------------------------------------

#' Volatility-reduction for one (topology, seed) at a given parameter override.
#'
#' Runs the naive and hybrid architectures under identical settings and returns
#' the per-tier price-volatility reduction 1 - sigma_hybrid / sigma_naive. Cells
#' where the naive baseline is itself non-volatile (sigma <= floor) are returned
#' as NA so they do not distort the headline (the reduction is only defined where
#' there is volatility to reduce -- the "volatile cells" of the main text).
exp14_reduction_one <- function(graph_type, load_level, N, seed,
                                cap_scale = 1.0,
                                integ_efficiency = 1.0,
                                integ_eta = price_eta,
                                lambda_l_default = 0.005,
                                n_rounds = 200L,
                                deadlines = c(500L, 750L, 1000L),
                                vol_floor = 0.02) {
  common <- list(
    graph_type = graph_type, load_level = load_level, N = as.integer(N),
    seed = as.integer(seed), n_rounds = n_rounds, deadlines = deadlines,
    lambda_l_default = lambda_l_default, cap_scale = cap_scale,
    integ_eta = integ_eta, integ_efficiency = integ_efficiency
  )
  naive  <- do.call(exp4_run_single, c(list(architecture = "naive"),  common))
  hybrid <- do.call(exp4_run_single, c(list(architecture = "hybrid"), common))
  s_naive  <- naive$mean_price_volatility[1]
  s_hybrid <- hybrid$mean_price_volatility[1]
  if (is.na(s_naive) || s_naive <= vol_floor) return(NA_real_)
  1 - s_hybrid / s_naive
}

#' Baseline values the sweep holds every other knob at.
#'
#' This is the headline configuration, which is what makes the other rows
#' sensitivities of a reported result rather than of an unreported one. The
#' efficiency baseline is pinned to the pipeline's constant by
#' tests/testthat/test-exp14-sensitivity.R.
exp14_baseline <- function() {
  list(cap_scale = 1.0, integ_eta = price_eta,
       lambda_l_default = 0.005, integ_efficiency = 1.0)
}


#' The levels each knob is swept over, one at a time.
exp14_sweeps <- function() {
  list(
    cap_scale        = c(0.7, 1.0, 1.3),
    integ_eta        = c(price_eta - 0.05, price_eta, price_eta + 0.05),
    lambda_l_default = c(0.0025, 0.005, 0.0075),
    # 1.0, the baseline, is the no-savings case: the integrator assumed to
    # reduce nothing, which is what every headline arm runs at. The levels
    # below it are the assumed reductions the result no longer rests on.
    integ_efficiency = c(0.65, 0.75, 0.85, 1.0)
  )
}


#' Is this (parameter, level) the cell the other parameters are held at?
exp14_is_baseline <- function(parameter, level) {
  isTRUE(all.equal(level, exp14_baseline()[[parameter]]))
}


#' The (parameter, level) grid the sensitivity sweep runs over.
#'
#' One row per cell; the pipeline branches over it, so the cells run in
#' parallel and each one is cached, invalidated and retried on its own.
exp14_sweep_grid <- function() {
  sweeps <- exp14_sweeps()
  grid <- data.frame(
    parameter = rep(names(sweeps), lengths(sweeps)),
    level     = unlist(sweeps, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  grid$is_baseline <- mapply(exp14_is_baseline, grid$parameter, grid$level,
                             USE.NAMES = FALSE)
  grid
}


#' Median volatility reduction for one (parameter, level) cell.
#'
#' Holds every other knob at baseline, runs the naive and hybrid arms over the
#' (topology, seed) cells, and summarises the volatile ones. Each run reseeds
#' inside exp4_run_single, so a cell's value does not depend on which other
#' cells ran, in what order, or in which process.
#'
#' `N` is either one agent count for every topology or, as in the pipeline, a
#' vector named by topology (the per-topology operating point, `_targets.R`).
#'
#' @return A one-row data frame: parameter, level, is_baseline, n_volatile,
#'   median_reduction, min_reduction, max_reduction.
exp14_sensitivity_row <- function(parameter, level,
                                  topologies = c("sp", "entangled"),
                                  seeds = 1:5,
                                  N = 60L,
                                  load_level = "high",
                                  n_rounds = 200L) {
  args_over <- exp14_baseline()
  stopifnot("not a sensitivity parameter" = parameter %in% names(args_over))
  args_over[[parameter]] <- level

  grid <- expand.grid(topology = topologies, seed = seeds,
                      stringsAsFactors = FALSE)
  reductions <- mapply(function(tp, sd) {
    exp14_reduction_one(
      graph_type = tp, load_level = load_level, seed = sd,
      N = if (is.null(names(N))) N else unname(N[[tp]]),
      cap_scale = args_over$cap_scale,
      integ_efficiency = args_over$integ_efficiency,
      integ_eta = args_over$integ_eta,
      lambda_l_default = args_over$lambda_l_default,
      n_rounds = n_rounds
    )
  }, grid$topology, grid$seed)
  reductions <- reductions[!is.na(reductions)]

  data.frame(
    parameter   = parameter,
    level       = level,
    is_baseline = exp14_is_baseline(parameter, level),
    n_volatile  = length(reductions),
    median_reduction = if (length(reductions)) median(reductions) else NA_real_,
    min_reduction    = if (length(reductions)) min(reductions)    else NA_real_,
    max_reduction    = if (length(reductions)) max(reductions)    else NA_real_,
    stringsAsFactors = FALSE
  )
}


#' One-at-a-time sensitivity table: every cell of the grid, in one call.
#'
#' The pipeline branches over `exp14_sweep_grid()` instead of calling this, and
#' gets the same rows; this is the serial entry point for a script or a test.
exp14_sensitivity_table <- function(topologies = c("sp", "entangled"),
                                    seeds = 1:5,
                                    N = 60L,
                                    load_level = "high",
                                    n_rounds = 200L) {
  grid <- exp14_sweep_grid()
  rows <- mapply(exp14_sensitivity_row, grid$parameter, grid$level,
                 MoreArgs = list(topologies = topologies, seeds = seeds, N = N,
                                 load_level = load_level, n_rounds = n_rounds),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

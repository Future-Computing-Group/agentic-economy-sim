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
                                integ_efficiency = NULL,
                                integ_eta = 0.15,
                                lambda_l_default = 0.005,
                                n_rounds = 200L,
                                deadlines = c(500L, 750L, 1000L),
                                vol_floor = 0.02) {
  if (is.null(integ_efficiency)) {
    integ_efficiency <- if (graph_type == "entangled") 0.85 else 0.75
  }
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

#' One-at-a-time sensitivity table.
#'
#' For each parameter and each of its sweep levels, computes the median
#' volatility reduction over the volatile (topology, seed) cells, holding every
#' other parameter at baseline. Returns a tidy data frame with one row per
#' (parameter, level).
exp14_sensitivity_table <- function(topologies = c("sp", "entangled"),
                                    seeds = 1:5,
                                    N = 60L,
                                    load_level = "high",
                                    n_rounds = 200L) {
  # Baseline values and the one-at-a-time sweep levels for each knob.
  sweeps <- list(
    cap_scale        = c(0.7, 1.0, 1.3),
    integ_eta        = c(0.10, 0.15, 0.20),
    lambda_l_default = c(0.0025, 0.005, 0.0075),
    integ_efficiency = c(0.65, 0.75, 0.85)   # spans the SP/entangled defaults
  )
  baseline <- list(cap_scale = 1.0, integ_eta = 0.15,
                   lambda_l_default = 0.005, integ_efficiency = NULL)

  grid <- expand.grid(topology = topologies, seed = seeds,
                      stringsAsFactors = FALSE)
  rows <- list()
  for (param in names(sweeps)) {
    for (lvl in sweeps[[param]]) {
      args_over <- baseline
      args_over[[param]] <- lvl
      reductions <- mapply(function(tp, sd) {
        exp14_reduction_one(
          graph_type = tp, load_level = load_level, N = N, seed = sd,
          cap_scale = args_over$cap_scale,
          integ_efficiency = args_over$integ_efficiency,
          integ_eta = args_over$integ_eta,
          lambda_l_default = args_over$lambda_l_default,
          n_rounds = n_rounds
        )
      }, grid$topology, grid$seed)
      reductions <- reductions[!is.na(reductions)]
      rows[[length(rows) + 1L]] <- data.frame(
        parameter   = param,
        level       = lvl,
        is_baseline = isTRUE(all.equal(lvl, baseline[[param]])) ||
                      (param == "integ_efficiency" && lvl == 0.75),
        n_volatile  = length(reductions),
        median_reduction = if (length(reductions)) median(reductions) else NA_real_,
        min_reduction    = if (length(reductions)) min(reductions)    else NA_real_,
        max_reduction    = if (length(reductions)) max(reductions)    else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

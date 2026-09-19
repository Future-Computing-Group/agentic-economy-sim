# sim_exp4.R
# ---------------------------------------------------------------------------
# Experiment 4: the architecture x smoothing factorial
#
# Two crossed binary factors, all four cells at efficiency 1.0 and one common
# price step, so neither factor carries the other's effect:
#   encapsulation off, smoothing off -> naive         (per-tier market, raw price)
#   encapsulation off, smoothing on  -> naive_ema     (per-tier market, EMA price)
#   encapsulation on,  smoothing off -> hybrid_noema  (integrator, beta = 0)
#   encapsulation on,  smoothing on  -> hybrid_ema    (integrator, EMA price)
#
# The legacy `hybrid` level (encapsulation plus an assumed demand reduction) is
# not a cell of the factorial; it is retained for the sensitivity sweep and for
# the experiments that reuse this driver.
#
# Varies architecture x topology (sp/entangled) x load (medium/high)
# x N (20/40/60/80).
#
# Paper reference: the headline results (subsec:ablation-results,
# subsec:exp4-scalability), detailed results in app:exp4-detailed
# (tab:exp4-results) and the architecture ablation in app:exp4-ablation
# (tab:exp4-ablation).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})


#' Initialise an integrator for the hybrid architecture.
#'
#' @param beta              EMA smoothing for slice price (0.8 = 80% previous).
#' @param efficiency_factor Demand reduction factor for internal scheduling;
#'                          1.0, the headline configuration, assumes none.
#' @param eta               Slice price step size during tatonnement.
#' @return A list representing integrator state.
integrator_init <- function(beta = 0.8,
                            efficiency_factor = 1.0, eta = price_eta) {
  list(
    beta              = beta,
    efficiency_factor = efficiency_factor,
    eta               = eta,
    slice_price       = 0.5
  )
}


#' Run a single Exp4 configuration (one seed).
#'
#' @param architecture      A cell of the factorial ("naive", "naive_ema",
#'                          "hybrid_noema", "hybrid_ema") or the legacy
#'                          "hybrid" (encapsulation plus demand reduction).
#' @param graph_type        DAG topology: "sp" or "entangled".
#' @param load_level        Load regime: "medium" or "high".
#' @param N                 Number of agents.
#' @param seed              Random seed.
#' @param n_rounds          Number of simulation rounds.
#' @param deadlines         Integer vector of possible task deadlines (ms).
#' @param alpha             Congestion sensitivity.
#' @param p                 Congestion exponent.
#' @param lambda_l_default  Per-ms latency decay rate.
#' @param salvage           Value retained after deadline miss.
#' @param iters             Tatonnement iterations per round.
#' @param eta               Price step size.
#' @param success_lr        Learning rate for success model.
#' @param integ_beta        Integrator EMA smoothing.
#' @param integ_efficiency  Integrator efficiency factor.
#' @param integ_eta         Integrator slice price step size.
#' @param alloc_out         Directory to export this run to for emulation
#'                          replay, or NULL for no export. Keep it outside the
#'                          repository: runs are data, not code.
#' @return A single-row tibble of summary metrics.
exp4_run_single <- function(architecture = c("naive", "naive_ema",
                                             "hybrid_noema", "hybrid_ema",
                                             "hybrid"),
                            graph_type = c("sp", "entangled", "agentic", "tree"),
                            load_level = c("medium", "high", "low"),
                            N = 50L, seed = 1L, n_rounds = 50L,
                            deadlines = c(500L, 750L, 1000L),
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = price_eta, success_lr = 0.3,
                            integ_beta = 0.8,
                            integ_efficiency = 1.0, integ_eta = price_eta,
                            enc_overhead_ms = 0, cap_scale = 1.0,
                            slice_inflation = 1.0, alloc_out = NULL) {
  architecture <- match.arg(architecture)
  # Encapsulation off: the agent faces the per-resource market directly.
  is_naive     <- architecture %in% c("naive", "naive_ema")
  graph_type   <- match.arg(graph_type)
  load_level   <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)
  # Parameter-sensitivity knob (Exp.14): scale per-tier capacities, admission
  # and execution alike. cap_scale < 1 saturates the market; 1 is the default.
  env <- scale_capacities(env, cap_scale)
  # Emulation replay export: the testbed runs what this run admits and decides
  # nothing itself, so the allocations, the environment they were decided in,
  # and this run's own per-task outcomes all leave from here.
  alloc_paths <- NULL
  if (!is.null(alloc_out)) {
    alloc_paths <- emul_export_open(alloc_out, load_level)
    export_env_json(env, alloc_out, n_rounds = n_rounds, seed = seed,
                    deadlines = deadlines)
  }

  agents           <- init_agents(N)
  base_latency_bid <- base_latency_for_bids(env)
  prev_util        <- NULL

  ms_res     <- init_market_state(env)
  # Every cell of the factorial runs at efficiency 1.0: an assumed demand
  # reduction is not one of the two crossed factors, and only the legacy
  # `hybrid` level still carries it. beta is the smoothing factor.
  eff_init <- if (architecture == "hybrid") integ_efficiency else 1.0
  smoothing <- if (architecture == "hybrid_noema") 0 else integ_beta
  integrator <- integrator_init(
    beta = smoothing,
    efficiency_factor = eff_init, eta = integ_eta
  )

  # Pre-allocate per-round metric vectors
  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  slicePriceV <- unitCostV <- clearV <- servedV <- numeric(n_rounds)

  for (t in seq_len(n_rounds)) {
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a) {
      generate_tasks(a, env, round = t, deadlines = deadlines)
    })
    tasks_all <- bind_tasks(tasks_list)
    n_gen     <- nrow(tasks_all)
    util_hat  <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)

    if (is_naive) {
      # -- Encapsulation off: direct multi-tier market clearing (no integrator).
      # naive_ema posts the same EMA-smoothed price the integrator posts, from
      # the same opening round, on the same allocation, which is what makes
      # smoothing a factor in its own right. The opening transient this gives
      # every smoothed arm is dropped by the trimmed metric rather than by a
      # per-arm exemption: the arms differ there by their initial price, which
      # is 0.1 a tier against a slice price seeded near its operating level.
      cleared <- clear_multitier_market(
        tasks_all, env, util_hat, base_latency_bid, ms_res,
        alpha = alpha, p = p, lambda_l_default = lambda_l_default,
        salvage = salvage, iters = iters, eta = eta,
        beta = if (architecture == "naive_ema") integ_beta else 0
      )
      allocation      <- cleared$allocation
      ms_res          <- append_price_history(cleared$market_state,
                                              cleared$market_state$prices)
      unitCostV[t]    <- cleared$clearing$unit_cost
      slicePriceV[t]  <- NA_real_

    } else {
      # -- Encapsulation on: the integrator clears a single-dimensional slice
      # market. hybrid_noema is the same integrator with its smoothing off. --
      ic <- integrator_clear(
        tasks_all, env, util_hat, base_latency_bid, ms_res$success_model,
        integrator,
        alpha = alpha, p = p, lambda_l_default = lambda_l_default,
        salvage = salvage, iters = iters, slice_inflation = slice_inflation
      )
      allocation     <- ic$allocation
      integrator     <- ic$integrator
      unitCostV[t]   <- ic$unit_cost
      slicePriceV[t] <- integrator$slice_price

      # The integrator's internal per-tier prices, from the sourcing loop inside
      # integrator_clear. They are recorded here and nowhere consumed: the
      # reported volatility is the CV of unitCostV, the per-task cost the agent
      # actually faces, which on this arm is the posted slice price. Each tier
      # is kept distinct in the record because copying one scalar slice price
      # across all three would make any series read off it an artifact of the
      # copying (σ_p ≈ 0.097 regardless of N, load or topology).
      ms_res <- append_price_history(ms_res, ic$tier_prices)
    }

    allocation <- allocation %>%
      mutate(
        task_id    = as.character(task_id),
        agent_id   = as.integer(agent_id),
        deadline   = as.numeric(deadline),
        value_base = as.numeric(value_base)
      )

    # Execute: pass efficiency_factor only for full hybrid mode so the
    # integrator's internal scheduling reduces per-task resource footprint.
    # hybrid_ema uses EMA price smoothing but no execution-time demand reduction.
    eff_factor <- if (architecture == "hybrid") integrator$efficiency_factor else NULL
    # Encapsulation overhead applies only on the integrator (hybrid) path;
    # naive has no integrator, so it incurs no protocol-translation latency.
    enc_ms     <- if (is_naive) 0 else enc_overhead_ms
    results_t  <- execute_allocation(allocation, env,
                                     efficiency_factor = eff_factor,
                                     enc_overhead_ms = enc_ms)

    if (nrow(results_t) > 0 &&
        !all(c("deadline", "value_base") %in% names(results_t))) {
      results_t <- results_t %>%
        left_join(allocation %>% select(task_id, deadline, value_base),
                  by = "task_id")
    }
    if (!is.null(alloc_paths)) {
      emul_export_round(alloc_paths, t, tasks_all, allocation, results_t)
    }
    agents <- update_trust(agents, results_t)

    # -- Record metrics --
    if (nrow(results_t) == 0) {
      medL[t] <- NA; p95L[t] <- NA
    } else {
      medL[t] <- median(results_t$latency, na.rm = TRUE)
      p95L[t] <- quantile(results_t$latency, 0.95, na.rm = TRUE, names = FALSE)
    }

    util_df   <- compute_utilisation_per_tier(env, n_gen)
    utilV[t]  <- mean(util_df$util, na.rm = TRUE)
    prev_util <- util_df

    succ <- if (nrow(results_t) == 0) 0 else sum(results_t$success, na.rm = TRUE)
    if (n_gen == 0) {
      dropV[t]  <- 0
      clearV[t] <- NA_real_
    } else {
      dropV[t]  <- 1 - succ / n_gen
      clearV[t] <- nrow(allocation) / n_gen
    }
    # Deliverability of what was admitted: NA in a round that admitted nothing.
    servedV[t] <- if (nrow(allocation) == 0) NA_real_ else succ / nrow(allocation)

    welfareV[t] <- compute_welfare(
      results_t, env, ms_res$prices,
      lambda_l_default = lambda_l_default, salvage = salvage,
      cong_cost = TRUE, cong_gamma = 0.05
    )
    orc <- oracle_pack_realised(
      tasks_all, env, util_hat, base_latency_bid, ms_res$success_model,
      alpha = alpha, p = p,
      lambda_l_default = lambda_l_default, salvage = salvage
    )
    oracleV[t] <- orc$oracle_value
    effV[t]    <- ifelse(oracleV[t] > 0, welfareV[t] / oracleV[t], NA_real_)

    ms_res <- market_update_from_results(
      ms_res, util_hat, results_t, lr = success_lr
    )
  }

  # Price volatility = dispersion of the per-task cost the agent actually pays
  # (unitCostV), measured identically for both arms. See agent_price_volatility()
  # for why this replaces the floor-pathological per-tier log-return metric.
  tibble(
    architecture          = architecture,
    graph_type            = graph_type,
    load_level            = load_level,
    N                     = as.integer(N),
    seed                  = seed,
    median_latency        = mean(medL, na.rm = TRUE),
    p95_latency           = mean(p95L, na.rm = TRUE),
    utilisation           = mean(utilV, na.rm = TRUE),
    drop_rate             = mean(dropV, na.rm = TRUE),
    # Share of GENERATED tasks the market admits. drop_rate counts a task that
    # was never admitted as a drop, so the two together separate rationing at
    # the market from missed deadlines in execution; the operating point is
    # calibrated on this one (tests/testthat/test-operating-point.R).
    clearing_fraction     = mean(clearV, na.rm = TRUE),
    # Deadline-met share of ADMITTED tasks. drop_rate is over GENERATED tasks,
    # so admitting more of them lowers it even when the extra admissions congest
    # the tiers; this column is the one that reads over-admission as the cost it
    # is, and it is what Exp.10 reports beside welfare.
    served_among_admitted = mean(servedV, na.rm = TRUE),
    welfare               = mean(welfareV, na.rm = TRUE),
    oracle_welfare        = mean(oracleV, na.rm = TRUE),
    efficiency            = mean(effV, na.rm = TRUE),
    mean_unit_cost        = mean(unitCostV, na.rm = TRUE),
    mean_slice_price      = mean(slicePriceV, na.rm = TRUE),
    mean_price_volatility = agent_price_volatility(unitCostV),
    # The same dispersion after the opening transient every arm starts with.
    # The factorial contrasts its cells on this one; the full-run column stays
    # as the figure and the per-arm tables have always reported it.
    mean_price_volatility_tail = agent_price_volatility_tail(unitCostV)
  )
}


#' Aggregate Exp4 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp4_run_single().
#' @return A tibble with one row per (architecture, graph_type, load_level, N).
exp4_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(architecture, graph_type, load_level, N) %>%
    summarise(
      across(
        c(median_latency, p95_latency, utilisation, drop_rate,
          clearing_fraction, served_among_admitted, welfare, oracle_welfare,
          efficiency, mean_unit_cost, mean_slice_price, mean_price_volatility,
          mean_price_volatility_tail),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}


#' Aggregate Exp.10 results (Prop.3 faithfulness sweep) across seeds.
#'
#' @param results_list List of single-seed tibbles from exp4_run_single().
#' @return A tibble with one row per (graph_type, slice_inflation).
exp10_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(graph_type, slice_inflation) %>%
    summarise(
      across(
        c(drop_rate, served_among_admitted, welfare, mean_price_volatility,
          median_latency),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}


#' Aggregate Exp.12 results (encapsulation overhead sweep) across seeds.
#'
#' @param results_list List of single-seed tibbles from exp4_run_single().
#' @return A tibble with one row per (graph_type, enc_overhead_ms).
exp12_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(graph_type, enc_overhead_ms) %>%
    summarise(
      across(
        c(median_latency, p95_latency, drop_rate, welfare),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

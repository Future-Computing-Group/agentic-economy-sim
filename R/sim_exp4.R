# sim_exp4.R
# ---------------------------------------------------------------------------
# Experiment 4: Naive vs hybrid (EMA only) vs hybrid (full) architecture
#
# Compares three strategies:
#   (1) Naive:      direct allocation on raw multi-tier resources
#   (2) Hybrid-EMA: integrator with EMA price smoothing but efficiency = 1.0
#   (3) Hybrid:     integrator with EMA + efficiency factor (demand reduction)
#
# Varies architecture x topology (sp/entangled) x load (medium/high)
# x N (20/40/60/80).  The EMA-only ablation isolates the architectural
# price-smoothing effect from the operational demand-reduction effect.
#
# Paper reference: Section VII-D, Table VI (Exp4), Supplementary Table VIII.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})


#' Initialise an integrator for the hybrid architecture.
#'
#' @param beta              EMA smoothing for slice price (0.8 = 80% previous).
#' @param efficiency_factor Demand reduction factor for internal scheduling.
#' @param eta               Slice price step size during tatonnement.
#' @return A list representing integrator state.
integrator_init <- function(beta = 0.8,
                            efficiency_factor = 0.8, eta = 0.15) {
  list(
    beta              = beta,
    efficiency_factor = efficiency_factor,
    eta               = eta,
    slice_price       = 0.5
  )
}


#' Run a single Exp4 configuration (one seed).
#'
#' @param architecture      "naive", "hybrid_ema", or "hybrid".
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
#' @return A single-row tibble of summary metrics.
exp4_run_single <- function(architecture = c("naive", "hybrid_ema", "hybrid"),
                            graph_type = c("sp", "entangled", "agentic", "tree"),
                            load_level = c("medium", "high", "low"),
                            N = 50L, seed = 1L, n_rounds = 50L,
                            deadlines = c(500L, 750L, 1000L),
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = 0.25, success_lr = 0.3,
                            integ_beta = 0.8,
                            integ_efficiency = 0.8, integ_eta = 0.15,
                            enc_overhead_ms = 0, cap_scale = 1.0,
                            slice_inflation = 1.0) {
  architecture <- match.arg(architecture)
  graph_type   <- match.arg(graph_type)
  load_level   <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)
  # Parameter-sensitivity knob (Exp.14): scale per-tier capacities.
  # cap_scale < 1 saturates the market; cap_scale = 1 is the default.
  if (cap_scale != 1.0) {
    env$capacities <- dplyr::mutate(env$capacities,
                                    capacity = capacity * cap_scale)
  }
  agents           <- init_agents(N)
  base_latency_bid <- base_latency_for_bids(env)
  prev_util        <- NULL

  ms_res     <- init_market_state(env)
  # For hybrid_ema ablation: use integrator's EMA price smoothing but no
  # efficiency-factor demand reduction (efficiency_factor = 1.0).
  eff_init <- if (architecture == "hybrid_ema") 1.0 else integ_efficiency
  integrator <- integrator_init(
    beta = integ_beta,
    efficiency_factor = eff_init, eta = integ_eta
  )

  # Pre-allocate per-round metric vectors
  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  slicePriceV <- unitCostV <- numeric(n_rounds)

  for (t in seq_len(n_rounds)) {
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a) {
      generate_tasks(a, env, round = t, deadlines = deadlines)
    })
    tasks_all <- bind_tasks(tasks_list)
    n_gen     <- nrow(tasks_all)
    util_hat  <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)

    if (architecture == "naive") {
      # -- Naive: direct multi-tier market clearing (no integrator) --
      cleared <- clear_multitier_market(
        tasks_all, env, util_hat, base_latency_bid, ms_res,
        alpha = alpha, p = p, lambda_l_default = lambda_l_default,
        salvage = salvage, iters = iters, eta = eta
      )
      allocation      <- cleared$allocation
      ms_res          <- append_price_history(cleared$market_state,
                                              cleared$market_state$prices)
      unitCostV[t]    <- cleared$clearing$unit_cost
      slicePriceV[t]  <- NA_real_

    } else {
      # -- Hybrid / Hybrid-EMA: integrator clears single-dimensional slice market --
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

      # Use the integrator's internal per-tier prices (computed by per-tier
      # tâtonnement in integrator_clear) for the price history that drives
      # volatility measurement. Each tier must carry its own price: copying a
      # single scalar slice price across all tiers yields σ_p ≈ 0.097 across
      # cells regardless of (N, load, topology), a by-construction artifact.
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
    enc_ms     <- if (architecture == "naive") 0 else enc_overhead_ms
    results_t  <- execute_allocation(allocation, env,
                                     efficiency_factor = eff_factor,
                                     enc_overhead_ms = enc_ms)

    if (nrow(results_t) > 0 &&
        !all(c("deadline", "value_base") %in% names(results_t))) {
      results_t <- results_t %>%
        left_join(allocation %>% select(task_id, deadline, value_base),
                  by = "task_id")
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

    if (n_gen == 0) {
      dropV[t] <- 0
    } else {
      succ     <- if (nrow(results_t) == 0) 0 else sum(results_t$success, na.rm = TRUE)
      dropV[t] <- 1 - succ / n_gen
    }

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
    welfare               = mean(welfareV, na.rm = TRUE),
    oracle_welfare        = mean(oracleV, na.rm = TRUE),
    efficiency            = mean(effV, na.rm = TRUE),
    mean_unit_cost        = mean(unitCostV, na.rm = TRUE),
    mean_slice_price      = mean(slicePriceV, na.rm = TRUE),
    mean_price_volatility = agent_price_volatility(unitCostV)
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
          welfare, oracle_welfare, efficiency, mean_unit_cost,
          mean_slice_price, mean_price_volatility),
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
        c(drop_rate, welfare, mean_price_volatility, median_latency),
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

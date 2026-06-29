# sim_exp6.R
# ---------------------------------------------------------------------------
# Experiment 6: Mechanism Ablation
#
# Compares four allocation mechanisms (random, EDF, value-greedy, market)
# across three topologies, two load levels, and two architectures.
# Isolates the contribution of price-based coordination over simpler
# allocation heuristics.
#
# Design: 4 x 3 x 2 x 2 x 10 seeds = 480 runs.
#
# Paper reference: Section VII-F (Mechanism Ablation).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x


# ===========================================================================
# Non-market allocation functions
# ===========================================================================

#' Hybrid non-market allocation via slice capacity.
#'
#' Computes the integrator's 1D slice capacity (from efficiency factor and
#' per-tier capacities), then selects the top tasks by the provided score
#' vector.  No price tatonnement or EMA smoothing.
#'
#' @param tasks_all   Tibble of tasks.
#' @param env         Environment list.
#' @param integrator  Integrator state list.
#' @param scores      Numeric score vector (same length as nrow(tasks_all)).
#' @return Allocation tibble.
clear_hybrid_nonmarket <- function(tasks_all, env, integrator, scores) {
  empty_alloc <- tibble(task_id = character(), agent_id = integer(),
                        deadline = numeric(), value_base = numeric())
  if (nrow(tasks_all) == 0) return(empty_alloc)

  cap    <- tier_capacities(env)
  bundle <- task_bundle(env)

  # Slice capacity: same calculation as integrator_clear()
  per_tier <- cap %>%
    left_join(bundle, by = "tier") %>%
    mutate(
      eff_demand = demand * integrator$efficiency_factor,
      max_tasks  = floor(capacity / pmax(eff_demand, 1e-6))
    )
  slice_capacity <- min(per_tier$max_tasks)

  # Order by descending score, take top slice_capacity with positive scores
  order_idx <- order(scores, decreasing = TRUE)
  chosen    <- list()
  k         <- 0L

  for (i in order_idx) {
    if (!is.finite(scores[i]) || scores[i] <= 0) next
    if (k >= slice_capacity) break
    k <- k + 1L
    chosen[[k]] <- tasks_all[i, c("task_id", "agent_id", "deadline", "value_base")]
  }

  if (length(chosen) == 0) empty_alloc else bind_rows(chosen)
}


# ===========================================================================
# Main simulation loop
# ===========================================================================

#' Run a single Exp6 configuration (one seed).
#'
#' @param mechanism       "random", "edf", "greedy_ev", or "market".
#' @param architecture    "naive" or "hybrid".
#' @param graph_type      DAG topology: "tree", "sp", or "entangled".
#' @param load_level      Load regime: "medium" or "high".
#' @param N               Number of agents.
#' @param seed            Random seed.
#' @param n_rounds        Number of simulation rounds.
#' @param deadlines       Integer vector of possible task deadlines (ms).
#' @param alpha           Congestion sensitivity.
#' @param p               Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate.
#' @param salvage         Value retained after deadline miss.
#' @param iters           Tatonnement iterations per round (market only).
#' @param eta             Price step size (market only).
#' @param success_lr      Learning rate for success model.
#' @param integ_beta      Integrator EMA smoothing.
#' @param integ_efficiency Integrator efficiency factor.
#' @param integ_eta       Integrator slice price step size.
#' @return A single-row tibble of summary metrics.
exp6_run_single <- function(mechanism = c("random", "edf", "greedy_ev", "market"),
                            architecture = c("naive", "hybrid"),
                            graph_type = c("tree", "sp", "entangled"),
                            load_level = c("medium", "high"),
                            N = 50L, seed = 1L, n_rounds = 50L,
                            deadlines = c(500L, 750L, 1000L),
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = 0.25, success_lr = 0.3,
                            integ_beta = 0.8,
                            integ_efficiency = 0.8, integ_eta = 0.15) {
  mechanism    <- match.arg(mechanism)
  architecture <- match.arg(architecture)
  graph_type   <- match.arg(graph_type)
  load_level   <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)

  agents           <- init_agents(N)
  base_latency_bid <- base_latency_for_bids(env)
  prev_util        <- NULL

  use_hybrid  <- (architecture == "hybrid")
  use_market  <- (mechanism == "market")

  # Market state (needed for market mechanism and success model updates)
  ms <- init_market_state(env)

  # Integrator state (only used when hybrid)
  integ <- integrator_init(beta = integ_beta,
                           efficiency_factor = integ_efficiency,
                           eta = integ_eta)

  # Pre-allocate per-round metric vectors
  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  unitCostV <- rep(NA_real_, n_rounds)   # agent-facing cost (market mechanism only)

  for (t in seq_len(n_rounds)) {
    # -- Generate tasks --
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a) {
      generate_tasks(a, env, round = t, deadlines = deadlines)
    })
    tasks_all <- bind_tasks(tasks_list)
    n_gen     <- nrow(tasks_all)
    util_hat  <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)

    # -- Allocate tasks based on mechanism x architecture --
    if (use_market) {
      # === Market mechanism (paper's tatonnement) ===
      if (!use_hybrid) {
        cleared <- clear_multitier_market(
          tasks_all, env, util_hat, base_latency_bid, ms,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters, eta = eta
        )
        alloc <- cleared$allocation
        ms    <- append_price_history(cleared$market_state,
                                      cleared$market_state$prices)
        unitCostV[t] <- cleared$clearing$unit_cost
      } else {
        ic    <- integrator_clear(
          tasks_all, env, util_hat, base_latency_bid, ms$success_model,
          integ,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters
        )
        alloc <- ic$allocation
        integ <- ic$integrator
        synth <- ms$prices
        synth$price <- integ$slice_price
        ms <- append_price_history(ms, synth)
        unitCostV[t] <- ic$unit_cost
      }

    } else {
      # === Non-market mechanisms ===
      # Compute scores based on mechanism type
      if (mechanism == "random") {
        scores <- runif(nrow(tasks_all), min = 0.01, max = 1.0)
      } else if (mechanism == "edf") {
        if (nrow(tasks_all) > 0) {
          max_dl <- max(tasks_all$deadline, na.rm = TRUE)
          scores <- (max_dl + 1) - tasks_all$deadline
        } else {
          scores <- numeric(0)
        }
      } else {
        # greedy_ev: use expected value as scores
        if (nrow(tasks_all) > 0) {
          scores <- task_expected_value(
            tasks_all, util_hat, base_latency_bid, ms$success_model,
            alpha = alpha, p = p,
            lambda_l_default = lambda_l_default, salvage = salvage
          )
        } else {
          scores <- numeric(0)
        }
      }

      if (!use_hybrid) {
        # Naive non-market: multi-tier greedy pack
        if (nrow(tasks_all) == 0) {
          alloc <- tibble(task_id = character(), agent_id = integer(),
                          deadline = numeric(), value_base = numeric())
        } else {
          alloc <- pack_tasks_greedy(tasks_all, scores, env)
        }
      } else {
        # Hybrid non-market: slice-capacity-limited pack
        alloc <- clear_hybrid_nonmarket(tasks_all, env, integ, scores)
      }
      # No price history for non-market mechanisms
    }

    # -- Execute allocation --
    allocation <- alloc %>%
      mutate(
        task_id    = as.character(task_id),
        agent_id   = as.integer(agent_id),
        deadline   = as.numeric(deadline),
        value_base = as.numeric(value_base)
      )

    eff_factor <- if (use_hybrid) integ$efficiency_factor else NULL
    results_t  <- execute_allocation(allocation, env,
                                     efficiency_factor = eff_factor)
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

    # Welfare: use zero prices for non-market mechanisms (welfare = realized
    # value minus congestion cost, no monetary transfers)
    if (use_market) {
      prices_w <- ms$prices
    } else {
      prices_w <- tier_capacities(env) %>%
        transmute(tier = tier, price = 0)
    }
    welfareV[t] <- compute_welfare(
      results_t, env, prices_w,
      lambda_l_default = lambda_l_default, salvage = salvage,
      cong_cost = TRUE, cong_gamma = 0.05
    )
    orc <- oracle_pack_realised(
      tasks_all, env, util_hat, base_latency_bid, ms$success_model,
      alpha = alpha, p = p,
      lambda_l_default = lambda_l_default, salvage = salvage
    )
    oracleV[t] <- orc$oracle_value
    effV[t]    <- ifelse(oracleV[t] > 0, welfareV[t] / oracleV[t], NA_real_)

    # Update success model from outcomes (learning is mechanism-independent)
    ms <- market_update_from_results(ms, util_hat, results_t, lr = success_lr)
  }

  # Price volatility: dispersion of the agent-facing per-task cost. Only the
  # market mechanism posts prices; non-market mechanisms have no price process,
  # so unitCostV is NA there and the metric is NA (agent_price_volatility()).
  mpv <- if (use_market) agent_price_volatility(unitCostV) else NA_real_

  tibble(
    mechanism             = mechanism,
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
    mean_price_volatility = mpv
  )
}


#' Aggregate Exp6 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp6_run_single().
#' @return A tibble with one row per (mechanism, architecture, graph_type, load_level).
exp6_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(mechanism, architecture, graph_type, load_level, N) %>%
    summarise(
      across(
        c(median_latency, p95_latency, utilisation, drop_rate,
          welfare, oracle_welfare, efficiency, mean_price_volatility),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

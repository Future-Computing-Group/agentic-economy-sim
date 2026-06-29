# sim_exp2.R
# ---------------------------------------------------------------------------
# Experiment 2: Agent scaling x topology
#
# Varies the number of agents (N = 10, 20, ..., 60) across three topologies
# (tree/sp/entangled) at medium load.  Shows that the rate of performance
# degradation with increasing agent population depends on DAG structure,
# reinforcing that topology is a first-order determinant.
#
# Paper reference: Section VII-B, Table VI (Exp2).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})


#' Run a single Exp2 configuration (one seed).
#'
#' @param N                Number of agents.
#' @param load_level       Load regime (default: "medium").
#' @param graph_type       DAG topology: "tree", "sp", or "entangled".
#' @param seed             Random seed.
#' @param n_rounds         Number of simulation rounds.
#' @param deadlines        Integer vector of possible task deadlines (ms).
#' @param alpha            Congestion sensitivity for bidding latency estimate.
#' @param p                Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate for value function.
#' @param salvage          Fraction of value retained after deadline miss.
#' @param iters            Tatonnement iterations per round.
#' @param eta              Price step size.
#' @param success_lr       Learning rate for online logistic success model.
#' @return A single-row tibble of summary metrics.
exp2_run_single <- function(N = 10L,
                            load_level = "medium",
                            graph_type = c("tree", "sp", "entangled"),
                            seed = 1L, n_rounds = 50L,
                            deadlines = c(500L, 750L, 1000L),
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = 0.25, success_lr = 0.3) {
  graph_type <- match.arg(graph_type)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)
  agents           <- init_agents(N)
  base_latency_bid <- base_latency_for_bids(env)
  prev_util        <- NULL
  market_state     <- init_market_state(env)

  # Pre-allocate per-round metric vectors
  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  unitCostV <- numeric(n_rounds)   # agent-facing per-task cost

  for (t in seq_len(n_rounds)) {
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a) {
      generate_tasks(a, env, round = t, deadlines = deadlines)
    })
    tasks_all <- bind_tasks(tasks_list)
    n_gen     <- nrow(tasks_all)
    util_hat  <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)

    # Market clearing
    cleared <- clear_multitier_market(
      tasks_all, env, util_hat, base_latency_bid, market_state,
      alpha = alpha, p = p, lambda_l_default = lambda_l_default,
      salvage = salvage, iters = iters, eta = eta
    )
    unitCostV[t] <- cleared$clearing$unit_cost
    allocation <- cleared$allocation %>%
      mutate(
        task_id    = as.character(task_id),
        agent_id   = as.integer(agent_id),
        deadline   = as.numeric(deadline),
        value_base = as.numeric(value_base)
      )
    market_state <- cleared$market_state
    market_state <- append_price_history(market_state, market_state$prices)

    # Execute and update trust
    results_t <- execute_allocation(allocation, env)
    if (nrow(results_t) > 0 &&
        !all(c("deadline", "value_base") %in% names(results_t))) {
      results_t <- results_t %>%
        left_join(allocation %>% select(task_id, deadline, value_base),
                  by = "task_id")
    }
    agents <- update_trust(agents, results_t)

    # Record metrics
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
      results_t, env, market_state$prices,
      lambda_l_default = lambda_l_default, salvage = salvage,
      cong_cost = TRUE, cong_gamma = 0.05
    )
    orc <- oracle_pack_realised(
      tasks_all, env, util_hat, base_latency_bid, market_state$success_model,
      alpha = alpha, p = p,
      lambda_l_default = lambda_l_default, salvage = salvage
    )
    oracleV[t] <- orc$oracle_value
    effV[t]    <- ifelse(oracleV[t] > 0, welfareV[t] / oracleV[t], NA_real_)

    market_state <- market_update_from_results(
      market_state, util_hat, results_t, lr = success_lr
    )
  }

  dr       <- mean(dropV, na.rm = TRUE)

  tibble(
    graph_type            = graph_type,
    load_level            = load_level,
    N                     = as.integer(N),
    seed                  = seed,
    median_latency        = mean(medL, na.rm = TRUE),
    p95_latency           = mean(p95L, na.rm = TRUE),
    utilisation           = mean(utilV, na.rm = TRUE),
    drop_rate             = dr,
    deadline_sat          = 1 - dr,
    welfare               = mean(welfareV, na.rm = TRUE),
    oracle_welfare        = mean(oracleV, na.rm = TRUE),
    efficiency            = mean(effV, na.rm = TRUE),
    mean_price_volatility = agent_price_volatility(unitCostV)
  )
}


#' Aggregate Exp2 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp2_run_single().
#' @return A tibble with one row per (graph_type, load_level, N) combination.
exp2_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(graph_type, load_level, N) %>%
    summarise(
      across(
        c(median_latency, p95_latency, utilisation, drop_rate,
          deadline_sat, welfare, oracle_welfare, efficiency,
          mean_price_volatility),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

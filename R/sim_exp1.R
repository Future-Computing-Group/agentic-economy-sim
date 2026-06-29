# sim_exp1.R
# ---------------------------------------------------------------------------
# Experiment 1: DAG topology x load level
#
# Varies DAG structure (linear/tree/sp/entangled) and load level (low/medium/
# high) under the naive market architecture.  Demonstrates that topology is a
# first-order determinant of latency, drop rate, utilisation, and price
# stability.
#
# Paper reference: Section VII-A, Table VI (Exp1).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Run a single Exp1 configuration (one seed).
#'
#' @param graph_type       DAG topology: "linear", "tree", "sp", or "entangled".
#' @param load_level       Load regime: "low", "medium", or "high".
#' @param seed             Random seed for reproducibility.
#' @param n_agents         Number of agents.
#' @param n_rounds         Number of simulation rounds.
#' @param deadlines        Integer vector of possible task deadlines (ms).
#' @param alpha            Congestion sensitivity for bidding latency estimate.
#' @param p                Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate for value function.
#' @param salvage          Fraction of value retained after deadline miss.
#' @param iters            Tatonnement iterations per round.
#' @param eta              Price step size (fraction of excess demand / capacity).
#' @param success_lr       Learning rate for online logistic success model.
#' @return A single-row tibble of summary metrics across all rounds.
exp1_run_single <- function(graph_type = c("linear", "tree", "sp", "entangled"),
                            load_level = c("low", "medium", "high"),
                            seed = 1L,
                            n_agents = 50L,
                            n_rounds = 50L,
                            deadlines = c(500L, 750L, 1000L),
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = 0.25, success_lr = 0.3) {
  graph_type <- match.arg(graph_type)
  load_level <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = n_agents,
                            graph_type = graph_type)
  agents           <- init_agents(n_agents)
  base_latency_bid <- base_latency_for_bids(env)

  prev_util    <- NULL
  market_state <- init_market_state(env)

  # Pre-allocate per-round metric vectors
  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  unitCostV <- numeric(n_rounds)

  for (t in seq_len(n_rounds)) {
    # -- Generate tasks --
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a) {
      generate_tasks(a, env, round = t, deadlines = deadlines)
    })
    tasks_all <- bind_tasks(tasks_list)
    n_gen     <- nrow(tasks_all)
    util_hat  <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)

    # -- Market clearing (tatonnement) --
    cleared <- clear_multitier_market(
      tasks_all, env, util_hat, base_latency_bid, market_state,
      alpha = alpha, p = p, lambda_l_default = lambda_l_default,
      salvage = salvage, iters = iters, eta = eta
    )
    allocation <- cleared$allocation %>%
      mutate(
        task_id    = as.character(task_id),
        agent_id   = as.integer(agent_id),
        deadline   = as.numeric(deadline),
        value_base = as.numeric(value_base)
      )
    market_state <- cleared$market_state
    market_state <- append_price_history(market_state, market_state$prices)
    unitCostV[t] <- cleared$clearing$unit_cost

    # -- Execute allocation --
    results_t <- execute_allocation(allocation, env)
    if (nrow(results_t) > 0 &&
        !all(c("deadline", "value_base") %in% names(results_t))) {
      results_t <- results_t %>%
        left_join(allocation %>% select(task_id, deadline, value_base),
                  by = "task_id")
    }

    # -- Update trust --
    agents <- update_trust(agents, results_t)

    # -- Record per-round metrics --
    if (nrow(results_t) == 0) {
      medL[t] <- NA
      p95L[t] <- NA
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

    # -- Update online success model --
    market_state <- market_update_from_results(
      market_state, util_hat, results_t, lr = success_lr
    )
  }

  # Price volatility = dispersion of the per-task cost agents pay (unitCostV),
  # a floor-insensitive agent-facing metric (see agent_price_volatility()).
  tibble(
    graph_type            = graph_type,
    load_level            = load_level,
    seed                  = seed,
    median_latency        = mean(medL, na.rm = TRUE),
    p95_latency           = mean(p95L, na.rm = TRUE),
    utilisation           = mean(utilV, na.rm = TRUE),
    drop_rate             = mean(dropV, na.rm = TRUE),
    welfare               = mean(welfareV, na.rm = TRUE),
    oracle_welfare        = mean(oracleV, na.rm = TRUE),
    efficiency            = mean(effV, na.rm = TRUE),
    mean_unit_cost        = mean(unitCostV, na.rm = TRUE),
    mean_price_volatility = agent_price_volatility(unitCostV)
  )
}


#' Aggregate Exp1 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp1_run_single().
#' @return A tibble with one row per (graph_type, load_level) combination.
exp1_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(graph_type, load_level) %>%
    summarise(
      across(
        c(median_latency, p95_latency, utilisation, drop_rate,
          welfare, oracle_welfare, efficiency, mean_unit_cost,
          mean_price_volatility),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

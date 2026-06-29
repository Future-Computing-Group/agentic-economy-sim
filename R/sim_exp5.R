# sim_exp5.R
# ---------------------------------------------------------------------------
# Experiment 5: Hybrid architecture x Governance interaction
#
# Crosses architecture (naive/hybrid) with governance (none/strict) across
# three topologies (tree/sp/entangled) and two load levels (medium/high).
# Completes the ablation matrix by testing the combination that Exp 3
# (governance without hybrid) and Exp 4 (hybrid without governance) leave
# uncovered.
#
# Design: 2 x 2 x 3 x 2 x 10 seeds = 240 runs.
#
# Paper reference: Section VII-E (Hybrid x Governance Interaction).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Run a single Exp5 configuration (one seed).
#'
#' Combines the governance capacity-split logic from Exp 3 with the
#' integrator-based hybrid architecture from Exp 4.
#'
#' @param architecture     "naive" or "hybrid".
#' @param policy           Governance policy: "none" or "strict".
#' @param graph_type       DAG topology: "tree", "sp", or "entangled".
#' @param load_level       Load regime: "medium" or "high".
#' @param N                Number of agents.
#' @param seed             Random seed.
#' @param n_rounds         Number of simulation rounds.
#' @param p_sensitive      Fraction of tasks classified as sensitive.
#' @param deadlines        Integer vector of possible task deadlines (ms).
#' @param trust_threshold_strict  Trust threshold for strict policy gate.
#' @param alpha            Congestion sensitivity.
#' @param p                Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate.
#' @param salvage          Value retained after deadline miss.
#' @param iters            Tatonnement iterations per round.
#' @param eta              Price step size.
#' @param success_lr       Learning rate for success model.
#' @param integ_beta       Integrator EMA smoothing.
#' @param integ_efficiency Integrator efficiency factor.
#' @param integ_eta        Integrator slice price step size.
#' @return A single-row tibble of summary metrics.
exp5_run_single <- function(architecture = c("naive", "hybrid"),
                            policy = c("none", "strict"),
                            graph_type = c("tree", "sp", "entangled"),
                            load_level = c("medium", "high"),
                            N = 50L, seed = 1L, n_rounds = 50L,
                            p_sensitive = 0.3,
                            deadlines = c(500L, 750L, 1000L),
                            trust_threshold_strict = 0.75,
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = 0.25, success_lr = 0.3,
                            integ_beta = 0.8,
                            integ_efficiency = 0.8, integ_eta = 0.15) {
  architecture <- match.arg(architecture)
  policy       <- match.arg(policy)
  graph_type   <- match.arg(graph_type)
  load_level   <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)

  # Governance capacity split (strict = 70/30; none = full capacity)
  use_governance <- (policy == "strict")
  if (use_governance) {
    env <- make_env_compliance_split(env, compliant_fraction = 0.7)
  }

  agents           <- init_agents(N)
  base_latency_bid <- base_latency_for_bids(env)
  prev_util        <- NULL

  # Market states: one per pool when governance is active
  ms_c <- init_market_state(env)
  ms_g <- init_market_state(env)

  # Integrator state (only used when architecture == "hybrid")
  use_hybrid <- (architecture == "hybrid")
  integ_c    <- integrator_init(beta = integ_beta,
                                efficiency_factor = integ_efficiency,
                                eta = integ_eta)
  integ_g    <- integrator_init(beta = integ_beta,
                                efficiency_factor = integ_efficiency,
                                eta = integ_eta)

  # Pre-allocate per-round metric vectors
  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  complianceV <- coverageV <- numeric(n_rounds)
  unitCostV <- numeric(n_rounds)   # agent-facing per-task cost (both arms/pools)

  for (t in seq_len(n_rounds)) {
    # -- Generate tasks --
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a) {
      generate_tasks(a, env, round = t, deadlines = deadlines)
    })
    tasks_all <- bind_tasks(tasks_list)
    n_gen     <- nrow(tasks_all)
    util_hat  <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)

    compliant_alloc_ids <- character(0)

    if (!use_governance) {
      # --- No governance: single pool (capacities already in env) ---
      if (!use_hybrid) {
        # Naive + no governance (baseline)
        cleared <- clear_multitier_market(
          tasks_all, env, util_hat, base_latency_bid, ms_g,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters, eta = eta
        )
        alloc <- cleared$allocation
        ms_g  <- append_price_history(cleared$market_state,
                                      cleared$market_state$prices)
        uc_t  <- cleared$clearing$unit_cost
      } else {
        # Hybrid + no governance: integrator clears single slice market
        ic <- integrator_clear(
          tasks_all, env, util_hat, base_latency_bid, ms_g$success_model,
          integ_g,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters
        )
        alloc   <- ic$allocation
        integ_g <- ic$integrator
        ms_g <- append_price_history(ms_g, ic$tier_prices)
        uc_t <- ic$unit_cost
      }
      complianceV[t] <- 1
      coverageV[t]   <- ifelse(n_gen == 0, 1, nrow(alloc) / n_gen)

    } else {
      # --- Strict governance: split into compliant / general pools ---
      tasks_all <- tag_task_sensitivity(tasks_all, p_sensitive = p_sensitive)
      agent_trust      <- agents %>% select(agent_id, trust)
      tasks_tagged     <- tasks_all %>% left_join(agent_trust, by = "agent_id")
      tasks_s_eligible <- tasks_tagged %>%
        filter(task_type == "sensitive", trust >= trust_threshold_strict)
      tasks_n          <- tasks_tagged %>% filter(task_type == "normal")

      if (!use_hybrid) {
        # Naive + strict governance (same as Exp 3 strict)
        cleared_c <- run_market_with_capacity(
          tasks_s_eligible, env, util_hat, base_latency_bid, ms_c,
          env$capacities_compliant,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters, eta = eta
        )
        cleared_g <- run_market_with_capacity(
          tasks_n, env, util_hat, base_latency_bid, ms_g,
          env$capacities_general,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters, eta = eta
        )
        alloc <- bind_rows(cleared_c$allocation, cleared_g$allocation)
        compliant_alloc_ids <- as.character(cleared_c$allocation$task_id)
        ms_c <- append_price_history(cleared_c$market_state,
                                     cleared_c$market_state$prices)
        ms_g <- append_price_history(cleared_g$market_state,
                                     cleared_g$market_state$prices)
        uc_t <- mean(c(cleared_c$clearing$unit_cost,
                       cleared_g$clearing$unit_cost), na.rm = TRUE)
      } else {
        # Hybrid + strict governance: integrator clears each pool
        # Compliant pool
        env_c <- env
        env_c$capacities <- env$capacities_compliant
        ic_c <- integrator_clear(
          tasks_s_eligible, env_c, util_hat, base_latency_bid,
          ms_c$success_model, integ_c,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters
        )
        integ_c <- ic_c$integrator
        # Per-tier prices from compliant-pool integrator.
        ms_c <- append_price_history(ms_c, ic_c$tier_prices)

        # General pool
        env_g <- env
        env_g$capacities <- env$capacities_general
        ic_g <- integrator_clear(
          tasks_n, env_g, util_hat, base_latency_bid,
          ms_g$success_model, integ_g,
          alpha = alpha, p = p, lambda_l_default = lambda_l_default,
          salvage = salvage, iters = iters
        )
        integ_g <- ic_g$integrator
        # Per-tier prices from general-pool integrator.
        ms_g <- append_price_history(ms_g, ic_g$tier_prices)

        alloc <- bind_rows(ic_c$allocation, ic_g$allocation)
        compliant_alloc_ids <- as.character(ic_c$allocation$task_id)
        uc_t <- mean(c(ic_c$unit_cost, ic_g$unit_cost), na.rm = TRUE)
      }
      n_alloc <- nrow(alloc)
      complianceV[t] <- ifelse(n_alloc == 0, NA_real_, 1.0)
      coverageV[t]   <- ifelse(n_gen == 0, 1, n_alloc / n_gen)
    }
    unitCostV[t] <- uc_t

    # -- Execute allocation --
    allocation <- alloc %>%
      mutate(
        task_id    = as.character(task_id),
        agent_id   = as.integer(agent_id),
        deadline   = as.numeric(deadline),
        value_base = as.numeric(value_base)
      )

    eff_factor <- if (use_hybrid) integ_g$efficiency_factor else NULL
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

    if (!use_governance) {
      util_df <- compute_utilisation_per_tier(env, n_gen)
    } else {
      n_s    <- nrow(tasks_s_eligible)
      n_n    <- nrow(tasks_n)
      util_c <- compute_utilisation_per_tier(env, n_s,
                  capacity_override = env$capacities_compliant)
      util_g <- compute_utilisation_per_tier(env, n_n,
                  capacity_override = env$capacities_general)
      util_df <- tibble(tier = util_c$tier,
                        util = (util_c$util + util_g$util) / 2)
    }
    utilV[t]  <- mean(util_df$util, na.rm = TRUE)
    prev_util <- util_df

    if (n_gen == 0) {
      dropV[t] <- 0
    } else {
      succ     <- if (nrow(results_t) == 0) 0 else sum(results_t$success, na.rm = TRUE)
      dropV[t] <- 1 - succ / n_gen
    }

    # Welfare (blend prices from both pools)
    prices_blend <- tier_capacities(env) %>%
      transmute(tier = tier, price = 0) %>%
      left_join(ms_g$prices, by = "tier", suffix = c("", "_g")) %>%
      left_join(ms_c$prices, by = "tier", suffix = c("", "_c")) %>%
      transmute(tier  = tier,
                price = 0.5 * (replace_na(price_g, 0) + replace_na(price_c, 0)))
    welfareV[t] <- compute_welfare(
      results_t, env, prices_blend,
      lambda_l_default = lambda_l_default, salvage = salvage,
      cong_cost = TRUE, cong_gamma = 0.05
    )
    orc <- oracle_pack_realised(
      tasks_all, env, util_hat, base_latency_bid, ms_g$success_model,
      alpha = alpha, p = p,
      lambda_l_default = lambda_l_default, salvage = salvage
    )
    oracleV[t] <- orc$oracle_value
    effV[t]    <- ifelse(oracleV[t] > 0, welfareV[t] / oracleV[t], NA_real_)

    # Update success models
    if (use_governance && nrow(results_t) > 0) {
      results_c <- results_t %>% filter(task_id %in% compliant_alloc_ids)
      results_g <- results_t %>% filter(!task_id %in% compliant_alloc_ids)
      ms_c <- market_update_from_results(ms_c, util_hat, results_c, lr = success_lr)
      ms_g <- market_update_from_results(ms_g, util_hat, results_g, lr = success_lr)
    } else {
      ms_g <- market_update_from_results(ms_g, util_hat, results_t, lr = success_lr)
      ms_c <- market_update_from_results(ms_c, util_hat, results_t, lr = success_lr)
    }
  }

  # Price volatility = dispersion of the agent-facing per-task cost (unitCostV),
  # combined across pools under governance (see agent_price_volatility()).
  tibble(
    architecture          = architecture,
    policy                = policy,
    graph_type            = graph_type,
    load_level            = load_level,
    N                     = as.integer(N),
    seed                  = seed,
    median_latency        = mean(medL, na.rm = TRUE),
    p95_latency           = mean(p95L, na.rm = TRUE),
    utilisation           = mean(utilV, na.rm = TRUE),
    drop_rate             = mean(dropV, na.rm = TRUE),
    compliance            = mean(complianceV, na.rm = TRUE),
    coverage              = mean(coverageV, na.rm = TRUE),
    welfare               = mean(welfareV, na.rm = TRUE),
    oracle_welfare        = mean(oracleV, na.rm = TRUE),
    efficiency            = mean(effV, na.rm = TRUE),
    mean_price_volatility = agent_price_volatility(unitCostV)
  )
}


#' Aggregate Exp5 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp5_run_single().
#' @return A tibble with one row per (architecture, policy, graph_type, load_level).
exp5_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(architecture, policy, graph_type, load_level, N) %>%
    summarise(
      across(
        c(median_latency, p95_latency, utilisation, drop_rate,
          compliance, coverage, welfare, oracle_welfare, efficiency,
          mean_price_volatility),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

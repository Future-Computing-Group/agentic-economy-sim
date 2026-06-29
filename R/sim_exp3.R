# sim_exp3.R
# ---------------------------------------------------------------------------
# Experiment 3: Governance policy effects
#
# Varies governance policy (none/moderate/strict) across two topologies
# (tree/entangled) and two load levels (medium/high).  Shows how governance
# constraints reshape the feasible allocation set, creating efficiency-
# compliance trade-offs.
#
# Policy mechanics:
#   - none:     Full capacity available to all tasks; no compliance routing.
#   - moderate: 50/50 capacity split; sensitive tasks (30%) routed to
#               compliant pool, normal tasks to general pool.
#   - strict:   70/30 capacity split + trust gate (>= 0.75) on sensitive tasks.
#
# Paper reference: Section VII-C, Table VI (Exp3).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Tag tasks as "sensitive" or "normal" based on deterministic hash.
#'
#' Uses a modulo on the task_id factor index to ensure reproducibility.
#'
#' @param tasks_all   Tibble of tasks.
#' @param p_sensitive  Fraction of tasks classified as sensitive (default: 0.3).
#' @return tasks_all with an added task_type column.
tag_task_sensitivity <- function(tasks_all, p_sensitive = 0.3) {
  if (nrow(tasks_all) == 0) {
    return(tasks_all %>% mutate(task_type = character()))
  }
  tasks_all %>%
    mutate(
      task_type = ifelse(
        (as.integer(factor(task_id)) %% 100) / 100 < p_sensitive,
        "sensitive", "normal"
      )
    )
}


#' Split environment capacities into compliant and general pools.
#'
#' @param env                Environment list.
#' @param compliant_fraction Fraction of each tier's capacity reserved for
#'                           compliant tasks (remainder goes to general pool).
#' @return Modified environment with capacities_compliant and capacities_general.
make_env_compliance_split <- function(env, compliant_fraction = 0.5) {
  env2 <- env
  cap  <- tier_capacities(env) %>%
    mutate(
      cap_compliant = capacity * compliant_fraction,
      cap_general   = capacity - cap_compliant
    )
  env2$capacities_compliant <- cap %>% transmute(tier, capacity = cap_compliant)
  env2$capacities_general   <- cap %>% transmute(tier, capacity = cap_general)
  env2
}


#' Run market clearing against a specific capacity subset.
#'
#' @param tasks       Tibble of tasks to clear.
#' @param env         Environment list.
#' @param cap_tbl     Capacity tibble to use (overrides env$capacities).
#' @param ...         Additional arguments passed to clear_multitier_market().
#' @return Result from clear_multitier_market().
run_market_with_capacity <- function(tasks, env, util_hat, base_latency,
                                     market_state, cap_tbl, ...) {
  env_tmp <- env
  env_tmp$capacities <- cap_tbl
  clear_multitier_market(tasks, env_tmp, util_hat, base_latency, market_state, ...)
}


#' Run a single Exp3 configuration (one seed).
#'
#' @param policy              Governance policy: "none", "moderate", or "strict".
#' @param graph_type          DAG topology: "tree" or "entangled".
#' @param load_level          Load regime: "medium" or "high".
#' @param N                   Number of agents.
#' @param seed                Random seed.
#' @param n_rounds            Number of simulation rounds.
#' @param p_sensitive         Fraction of tasks classified as sensitive.
#' @param deadlines           Integer vector of possible task deadlines (ms).
#' @param trust_threshold_strict  Trust threshold for strict policy gate.
#' @param alpha               Congestion sensitivity.
#' @param p                   Congestion exponent.
#' @param lambda_l_default    Per-ms latency decay rate.
#' @param salvage             Value retained after deadline miss.
#' @param iters               Tatonnement iterations per round.
#' @param eta                 Price step size.
#' @param success_lr          Learning rate for success model.
#' @return A single-row tibble of summary metrics.
exp3_run_single <- function(policy = c("none", "moderate", "strict"),
                            graph_type = c("tree", "entangled"),
                            load_level = c("medium", "high"),
                            N = 50L, seed = 1L, n_rounds = 50L,
                            p_sensitive = 0.3,
                            deadlines = c(500L, 750L, 1000L),
                            trust_threshold_strict = 0.75,
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = 0.25, success_lr = 0.3) {
  policy     <- match.arg(policy)
  graph_type <- match.arg(graph_type)
  load_level <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)

  # Capacity split: none = 1.0 (unused), moderate = 0.5, strict = 0.7
  compliant_frac <- switch(policy,
    "none"     = 1.0,
    "moderate" = 0.5,
    "strict"   = 0.7
  )
  env <- make_env_compliance_split(env, compliant_fraction = compliant_frac)

  agents           <- init_agents(N)
  base_latency_bid <- base_latency_for_bids(env)
  prev_util        <- NULL

  # Separate market states for compliant and general pools
  ms_c <- init_market_state(env)
  ms_g <- init_market_state(env)

  # Pre-allocate per-round metric vectors
  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  complianceV <- coverageV <- premiumV <- numeric(n_rounds)
  unitCostV_g <- unitCostV_c <- rep(NA_real_, n_rounds)  # agent-facing cost per pool

  for (t in seq_len(n_rounds)) {
    # -- Generate and tag tasks --
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a) {
      generate_tasks(a, env, round = t, deadlines = deadlines)
    })
    tasks_all <- bind_tasks(tasks_list) %>%
      tag_task_sensitivity(p_sensitive = p_sensitive)
    n_gen    <- nrow(tasks_all)
    util_hat <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)

    compliant_alloc_ids <- character(0)

    # -- Policy-specific routing and market clearing --
    if (policy == "none") {
      # No governance: full capacity, all tasks to a single pool
      cap_full <- tier_capacities(env)
      cleared  <- run_market_with_capacity(
        tasks_all, env, util_hat, base_latency_bid, ms_g, cap_full,
        alpha = alpha, p = p, lambda_l_default = lambda_l_default,
        salvage = salvage, iters = iters, eta = eta
      )
      alloc <- cleared$allocation
      ms_g  <- append_price_history(cleared$market_state, cleared$market_state$prices)
      complianceV[t] <- 1
      coverageV[t]   <- ifelse(n_gen == 0, 1, nrow(alloc) / n_gen)
      premiumV[t]    <- 0
      unitCostV_g[t] <- cleared$clearing$unit_cost

    } else if (policy == "moderate") {
      # 50/50 split: sensitive -> compliant pool, normal -> general pool
      tasks_s <- tasks_all %>% filter(task_type == "sensitive")
      tasks_n <- tasks_all %>% filter(task_type == "normal")

      cleared_c <- run_market_with_capacity(
        tasks_s, env, util_hat, base_latency_bid, ms_c, env$capacities_compliant,
        alpha = alpha, p = p, lambda_l_default = lambda_l_default,
        salvage = salvage, iters = iters, eta = eta
      )
      cleared_g <- run_market_with_capacity(
        tasks_n, env, util_hat, base_latency_bid, ms_g, env$capacities_general,
        alpha = alpha, p = p, lambda_l_default = lambda_l_default,
        salvage = salvage, iters = iters, eta = eta
      )

      alloc <- bind_rows(cleared_c$allocation, cleared_g$allocation)
      compliant_alloc_ids <- as.character(cleared_c$allocation$task_id)
      ms_c <- append_price_history(cleared_c$market_state, cleared_c$market_state$prices)
      ms_g <- append_price_history(cleared_g$market_state, cleared_g$market_state$prices)

      # All allocated tasks satisfy the policy by construction
      n_alloc <- nrow(alloc)
      complianceV[t] <- ifelse(n_alloc == 0, NA_real_, 1.0)
      coverageV[t]   <- ifelse(n_gen == 0, 1, n_alloc / n_gen)
      premiumV[t]    <- (cleared_c$clearing$unit_cost %||% NA_real_) -
                         (cleared_g$clearing$unit_cost %||% NA_real_)
      unitCostV_c[t] <- cleared_c$clearing$unit_cost %||% NA_real_
      unitCostV_g[t] <- cleared_g$clearing$unit_cost %||% NA_real_

    } else {
      # Strict: 70/30 split + trust gate on sensitive tasks
      agent_trust      <- agents %>% select(agent_id, trust)
      tasks_tagged     <- tasks_all %>% left_join(agent_trust, by = "agent_id")
      tasks_s_eligible <- tasks_tagged %>%
        filter(task_type == "sensitive", trust >= trust_threshold_strict)
      tasks_n          <- tasks_tagged %>% filter(task_type == "normal")

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
      ms_c <- append_price_history(cleared_c$market_state, cleared_c$market_state$prices)
      ms_g <- append_price_history(cleared_g$market_state, cleared_g$market_state$prices)

      # All allocated tasks are policy-compliant (rejected ones are not allocated)
      n_alloc <- nrow(alloc)
      complianceV[t] <- ifelse(n_alloc == 0, NA_real_, 1.0)
      coverageV[t]   <- ifelse(n_gen == 0, 1, n_alloc / n_gen)
      premiumV[t]    <- (cleared_c$clearing$unit_cost %||% NA_real_) -
                         (cleared_g$clearing$unit_cost %||% NA_real_)
      unitCostV_c[t] <- cleared_c$clearing$unit_cost %||% NA_real_
      unitCostV_g[t] <- cleared_g$clearing$unit_cost %||% NA_real_
    }

    # -- Execute allocation --
    allocation <- alloc %>%
      mutate(
        task_id    = as.character(task_id),
        agent_id   = as.integer(agent_id),
        deadline   = as.numeric(deadline),
        value_base = as.numeric(value_base)
      )
    results_t <- execute_allocation(allocation, env)
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

    # Utilisation: use split capacities under governance
    if (policy == "none") {
      util_df <- compute_utilisation_per_tier(env, n_gen)
    } else {
      n_s    <- nrow(tasks_all %>% filter(task_type == "sensitive"))
      n_n    <- nrow(tasks_all %>% filter(task_type == "normal"))
      util_c <- compute_utilisation_per_tier(env, n_s,
                  capacity_override = env$capacities_compliant)
      util_g <- compute_utilisation_per_tier(env, n_n,
                  capacity_override = env$capacities_general)
      # Weighted average utilisation across pools
      util_df <- tibble(
        tier = util_c$tier,
        util = (util_c$util + util_g$util) / 2
      )
    }
    utilV[t]  <- mean(util_df$util, na.rm = TRUE)
    prev_util <- util_df

    if (n_gen == 0) {
      dropV[t] <- 0
    } else {
      succ     <- if (nrow(results_t) == 0) 0 else sum(results_t$success, na.rm = TRUE)
      dropV[t] <- 1 - succ / n_gen
    }

    # Welfare: blend prices from both pools
    prices_blend <- tier_capacities(env) %>%
      transmute(tier = tier, price = 0) %>%
      left_join(ms_g$prices, by = "tier", suffix = c("", "_g")) %>%
      left_join(ms_c$prices, by = "tier", suffix = c("", "_c")) %>%
      transmute(
        tier  = tier,
        price = 0.5 * (replace_na(price_g, 0) + replace_na(price_c, 0))
      )
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

    # -- Update success models for each pool --
    if (policy != "none" && nrow(results_t) > 0) {
      results_c <- results_t %>% filter(task_id %in% compliant_alloc_ids)
      results_g <- results_t %>% filter(!task_id %in% compliant_alloc_ids)
      ms_c <- market_update_from_results(ms_c, util_hat, results_c, lr = success_lr)
      ms_g <- market_update_from_results(ms_g, util_hat, results_g, lr = success_lr)
    } else {
      ms_g <- market_update_from_results(ms_g, util_hat, results_t, lr = success_lr)
      ms_c <- market_update_from_results(ms_c, util_hat, results_t, lr = success_lr)
    }
  }

  # Price volatility = dispersion of the agent-facing per-task cost in each pool
  # (general / compliant), a floor-insensitive metric (see agent_price_volatility()).
  tibble(
    policy                     = policy,
    graph_type                 = graph_type,
    load_level                 = load_level,
    N                          = as.integer(N),
    seed                       = seed,
    median_latency             = mean(medL, na.rm = TRUE),
    p95_latency                = mean(p95L, na.rm = TRUE),
    utilisation                = mean(utilV, na.rm = TRUE),
    drop_rate                  = mean(dropV, na.rm = TRUE),
    compliance                 = mean(complianceV, na.rm = TRUE),
    coverage                   = mean(coverageV, na.rm = TRUE),
    compliant_premium          = mean(premiumV, na.rm = TRUE),
    welfare                    = mean(welfareV, na.rm = TRUE),
    oracle_welfare             = mean(oracleV, na.rm = TRUE),
    efficiency                 = mean(effV, na.rm = TRUE),
    price_volatility_general   = agent_price_volatility(unitCostV_g),
    price_volatility_compliant = agent_price_volatility(unitCostV_c)
  )
}


#' Aggregate Exp3 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp3_run_single().
#' @return A tibble with one row per (policy, graph_type, load_level, N).
exp3_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(policy, graph_type, load_level, N) %>%
    summarise(
      across(
        c(median_latency, p95_latency, utilisation, drop_rate,
          compliance, coverage, compliant_premium, welfare, oracle_welfare,
          efficiency, price_volatility_general, price_volatility_compliant),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

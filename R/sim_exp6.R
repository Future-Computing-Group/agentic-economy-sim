# sim_exp6.R
# ---------------------------------------------------------------------------
# Experiment 6: Mechanism Ablation
#
# Compares six allocation mechanisms (random, EDF, value-greedy, market, and
# two that are actually deployed for this problem: a static posted price at
# three markups, and a Kubernetes-style priority rank) across three topologies,
# two load levels, and two architectures. Isolates the contribution of
# price-based coordination over simpler allocation heuristics, and measures it
# against deployed practice rather than against argument.
#
# Design: (5 + 3 markups) x 3 x 2 x 2 x 10 seeds = 960 runs.
#
# Paper reference: the mechanism ablation (subsec:exp6-mechanism), detailed
# results in app:exp6-detailed (tab:exp6-naive).
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


#' Priority class of an agent's tasks (Kubernetes PriorityClass).
#'
#' In Kubernetes the class is an integer the workload owner attaches to the pod;
#' the scheduler does not derive it from the resource request and cannot see the
#' task's deadline or its business value. The faithful analogue is a per-agent
#' class, because a real cluster grants a class to a namespace or a team rather
#' than renegotiating it per pod.
#'
#' @param agent_id Integer agent identifiers.
#' @return Integer class in 0, 1, 2.
priority_class <- function(agent_id) as.integer(agent_id) %% 3L

#' Least-requested term: this task's free share of the whole tier capacity.
#'
#' Kubernetes' LeastRequestedPriority scores a node by how much of it is still
#' free once the pod is placed. The score here is one vector computed before the
#' packing, so the share is of the whole tier capacity against this one task's
#' recipe, not the running share left as the round fills: a ranking key, not a
#' ledger. Here there is one aggregate capacity per tier, so with identical
#' bundles the term is the same for every task in a round and contributes
#' nothing to the ordering. It is computed per task from the recipe matrix
#' rather than from the environment's single bundle, so it stops being constant
#' exactly when tasks stop demanding the same thing.
#'
#' @param tasks_all Tibble of tasks.
#' @param env       Environment list.
#' @return Numeric vector in [0, 1], one per task.
least_requested_term <- function(tasks_all, env) {
  A   <- task_recipes(tasks_all, env)
  cap <- tier_capacities(env)
  C   <- cap$capacity[match(colnames(A), cap$tier)]
  rowMeans(pmax(1 - sweep(A, 2, C, "/"), 0))
}

#' Kubernetes-style scheduling rank, as one numeric score.
#'
#' The three keys kube-scheduler applies, packed into a single score for the
#' shared packing kernel: priority class first, least-requested second, arrival
#' order last. The spacing keeps the keys lexicographic -- the two lower terms
#' together stay below 11, so they can only order within a class.
#'
#' @param tasks_all Tibble of tasks, in scheduling-queue order.
#' @param env       Environment list.
#' @return Numeric score vector, one per task.
k8s_rank_score <- function(tasks_all, env) {
  n <- nrow(tasks_all)
  if (n == 0) return(numeric(0))
  1000 * priority_class(tasks_all$agent_id) +
    10 * least_requested_term(tasks_all, env) +
    (1 - seq_len(n) / (n + 1))
}


# ===========================================================================
# Main simulation loop
# ===========================================================================

#' Run a single Exp6 configuration (one seed).
#'
#' @param mechanism       "random", "edf", "greedy_ev", "market",
#'                        "posted_price", or "k8s".
#' @param architecture    "naive" or "hybrid".
#' @param graph_type      DAG topology: "tree", "sp", or "entangled".
#' @param load_level      Load regime: "medium" or "high".
#' @param p_post_k        Markup over marginal cost (posted_price only).
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
exp6_run_single <- function(mechanism = c("random", "edf", "greedy_ev", "market",
                                          "posted_price", "k8s"),
                            architecture = c("naive", "hybrid"),
                            graph_type = c("tree", "sp", "entangled"),
                            load_level = c("medium", "high"),
                            p_post_k = 1,
                            N = 50L, seed = 1L, n_rounds = 50L,
                            deadlines = c(500L, 750L, 1000L),
                            alpha = 50, p = 1.2,
                            lambda_l_default = 0.005, salvage = 0.0,
                            iters = 15L, eta = price_eta, success_lr = 0.3,
                            integ_beta = 0.8,
                            integ_efficiency = 1.0, integ_eta = price_eta) {
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

  # What the operator posts: a markup over the environment's own marginal cost
  # for one task bundle, which is the floor the market arm's own price is
  # anchored at. The two arms therefore start from the same number.
  p_post <- posted_price_anchor(env, p_post_k)

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
      } else if (mechanism == "k8s") {
        # Kubernetes-style rank: priority class, then least-requested, then
        # arrival order. Value-blind and deadline-blind by construction.
        scores <- k8s_rank_score(tasks_all, env)
      } else {
        # greedy_ev and posted_price: expected value as scores. The posted price
        # differs from value-greedy in its participation screen, not in the
        # order it serves the participants in.
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

      if (mechanism == "posted_price") {
        # The price is what agents face whether or not they take it, so it is
        # recorded every round, as the market arm records the price it cleared
        # at. Below the price a task does not participate, which the slice
        # packer reads as a non-positive score.
        unitCostV[t] <- p_post
        alloc <- if (!use_hybrid) {
          posted_price_allocate(tasks_all, env, scores, p_post)
        } else {
          clear_hybrid_nonmarket(tasks_all, env, integ,
                                 ifelse(scores > p_post, scores, 0))
        }
      } else if (!use_hybrid) {
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

  # Price volatility: dispersion of the agent-facing per-task cost. The two
  # priced mechanisms have a price process; the rank schedulers do not, so
  # unitCostV stays NA there and agent_price_volatility() returns NA on it. A
  # posted price is a constant series, so its volatility is a real zero.
  mpv <- agent_price_volatility(unitCostV)

  tibble(
    mechanism             = mechanism,
    architecture          = architecture,
    graph_type            = graph_type,
    load_level            = load_level,
    p_post_k              = as.numeric(p_post_k),
    N                     = as.integer(N),
    seed                  = seed,
    median_latency        = mean(medL, na.rm = TRUE),
    p95_latency           = mean(p95L, na.rm = TRUE),
    utilisation           = mean(utilV, na.rm = TRUE),
    drop_rate             = mean(dropV, na.rm = TRUE),
    welfare               = mean(welfareV, na.rm = TRUE),
    oracle_welfare        = mean(oracleV, na.rm = TRUE),
    efficiency            = mean(effV, na.rm = TRUE),
    mean_price_volatility = mpv,
    # The price agents actually paid, so a discovered price and a posted one are
    # comparable on the same row. NA rather than the NaN mean() returns on an
    # all-missing series, which reads as a computed number in a statistics dump.
    mean_unit_cost        = if (all(is.na(unitCostV))) NA_real_
                            else mean(unitCostV, na.rm = TRUE)
  )
}


#' The mechanism grid Exp.6 branches over.
#'
#' The markup is a parameter of one arm, so it varies only inside that arm: a
#' plain crossing would run every other mechanism three times under a price it
#' never posts, and report the same run as three rows.
#'
#' @param n_seeds Monte Carlo seeds per cell.
#' @return A tibble with one row per branch.
exp6_mechanism_grid <- function(n_seeds) {
  arms <- bind_rows(
    tidyr::expand_grid(
      mechanism = c("random", "edf", "greedy_ev", "market", "k8s"),
      p_post_k  = 1
    ),
    tidyr::expand_grid(mechanism = "posted_price", p_post_k = c(1, 2, 4))
  )
  tidyr::expand_grid(
    arms,
    graph_type   = c("tree", "sp", "entangled"),
    load_level   = c("medium", "high"),
    architecture = c("naive", "hybrid"),
    seed         = seq_len(n_seeds)
  )
}


#' Aggregate Exp6 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp6_run_single().
#' @return A tibble with one row per (mechanism, p_post_k, architecture,
#'   graph_type, load_level).
exp6_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(mechanism, p_post_k, architecture, graph_type, load_level, N) %>%
    summarise(
      across(
        c(median_latency, p95_latency, utilisation, drop_rate,
          welfare, oracle_welfare, efficiency, mean_price_volatility,
          mean_unit_cost),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

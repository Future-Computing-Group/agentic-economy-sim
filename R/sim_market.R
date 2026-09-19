# sim_market.R
# ---------------------------------------------------------------------------
# Core market engine: tatonnement clearing + online success learning.
#
# Implements the multi-tier market mechanism used by all four experiments.
# Per-tier prices are adjusted iteratively (tatonnement) to balance supply
# and demand across device/edge/cloud tiers. Tasks are then packed by
# surplus (expected value minus cost) subject to per-tier capacity
# constraints.  An online logistic model learns the relationship between
# system utilisation and deadline success probability.
#
# Also provides:
#   - Welfare computation (latency-aware value minus congestion penalty)
#   - Oracle packing (upper bound on achievable welfare)
#   - Integrator slice clearing (hybrid architecture, Exp4)
#
# Paper reference: the evaluation's experimental setting (sec:simulation-setup);
# parameters in tab:sim-baseline-params.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Tatonnement price step, common to every arm.
#
# One constant rather than a per-arm default: the naive baseline used to run at
# 0.25 while the integrator ran at 0.15, so any naive-versus-integrator contrast
# was a comparison of two architectures AND two price steps at once. 0.15 is the
# integrator's published operating point, so the baseline is the arm that moves.
price_eta <- 0.15

# Opening fraction of a run that the price-stability metric drops.
#
# Every arm starts at a price its market has not cleared at, and a smoothed arm
# walks to its operating level over about log(0.05)/log(beta) rounds. That ramp
# is an initial condition rather than price instability, and it contaminates the
# arms unequally: the per-tier market is initialised at 0.1 a tier, far above
# the level it clears at once demand is priced in, while the integrator's slice
# price is seeded close to its own. Trimming the same opening fraction of every
# arm is what makes a dispersion contrast between two arms a contrast in their
# price dynamics.
burn_in_fraction <- 0.2


#' Exponential moving average on a posted price.
#'
#' One expression with two call sites: the integrator's slice price and the
#' per-resource market's posted tier prices. Sharing it is what makes "the same
#' smoothing on both architectures", which the Exp.4 factorial rests on, a
#' property of the code rather than a claim about it.
#'
#' @param previous Previously posted price(s).
#' @param cleared  Price(s) the round cleared at.
#' @param beta     Weight on the previous posting; 0 posts the cleared price.
#' @return The posted price(s).
ema_post <- function(previous, cleared, beta) {
  beta * previous + (1 - beta) * cleared
}


# ===========================================================================
# Bundle model: per-task tier demand
# ===========================================================================

#' Get per-task resource demand per tier (topology-aware demand weights).
#'
#' Uses demand_weights from the DAG if available; falls back to node counts.
#'
#' @param env Environment list.
#' @return A tibble with columns: tier, demand.
task_bundle <- function(env) {
  if (!is.null(env$demand_weights)) {
    dw <- env$demand_weights
    stopifnot(all(c("tier", "demand_weight") %in% names(dw)))
    dw %>% transmute(tier = tier, demand = as.numeric(demand_weight))
  } else {
    stopifnot("nodes_per_tier" %in% names(env))
    nt <- env$nodes_per_tier
    stopifnot(all(c("tier", "nodes") %in% names(nt)))
    nt %>% transmute(tier = tier, demand = as.numeric(nodes))
  }
}

#' Columns an allocation carries out of a packer.
#'
#' The recipe travels with the allocation when there is one: execution has to
#' charge the admitted MIX, and a packer that dropped the column would leave the
#' deliverable set indistinguishable from the advertised interface.
#'
#' @param tasks_all Tibble of tasks.
#' @return Character vector of column names present in tasks_all.
.alloc_cols <- function(tasks_all) {
  intersect(c("task_id", "agent_id", "deadline", "value_base", "recipe"),
            names(tasks_all))
}

#' Get the per-task resource recipe matrix.
#'
#' The generalisation of task_bundle from one bundle per environment to one
#' recipe per task. `env$recipes` is a named list of per-tier demand vectors and
#' `tasks_all$recipe` names one of them per task. When either is absent the
#' environment's single bundle is broadcast to every row, so every caller that
#' predates recipes is unchanged by construction.
#'
#' @param tasks_all Tibble of tasks, optionally with a `recipe` column.
#' @param env       Environment list, optionally with a `recipes` list.
#' @return An n x R numeric matrix, columns in task_bundle() tier order.
task_recipes <- function(tasks_all, env) {
  bundle <- task_bundle(env)
  n      <- nrow(tasks_all)
  A <- matrix(bundle$demand, nrow = n, ncol = nrow(bundle), byrow = TRUE,
              dimnames = list(NULL, bundle$tier))
  if (n == 0L || is.null(env$recipes) || !("recipe" %in% names(tasks_all))) return(A)

  lab     <- as.character(tasks_all$recipe)
  unknown <- setdiff(unique(lab), names(env$recipes))
  if (length(unknown) > 0) {
    stop("task_recipes: env$recipes has no entry named ",
         paste(unknown, collapse = ", "))
  }
  for (r in unique(lab)) {
    v <- env$recipes[[r]]
    stopifnot("each env$recipes entry must be named by tier" =
                all(bundle$tier %in% names(v)))
    m <- lab == r
    A[m, ] <- matrix(v[bundle$tier], nrow = sum(m), ncol = nrow(bundle),
                     byrow = TRUE)
  }
  A
}

#' Get per-tier capacities from the environment.
#'
#' @param env Environment list.
#' @return A tibble with columns: tier, capacity.
tier_capacities <- function(env) {
  stopifnot("capacities" %in% names(env))
  cap <- env$capacities
  stopifnot(all(c("tier", "capacity") %in% names(cap)))
  cap %>% transmute(tier = tier, capacity = as.numeric(capacity))
}


# ===========================================================================
# Latency and value model
# ===========================================================================

#' Estimate expected latency from current utilisation (bidding model).
#'
#' Uses a power-law congestion model: L = base + alpha * rho^p.
#' This is the latency estimate agents use for bidding; the actual execution
#' latency uses an M/M/1 queueing model (see execute_allocation in
#' sim_helpers.R).
#'
#' @param util_hat     Current estimated utilisation (scalar).
#' @param base_latency Base latency in ms (default: 50).
#' @param alpha        Congestion sensitivity (default: 50).
#' @param p            Congestion exponent (default: 1.2).
#' @param min_latency  Floor on latency estimate (default: 1 ms).
#' @param util_cap     Saturation cap on the utilisation signal (default: 1.0).
#'                     Steady-state utilisation cannot exceed capacity; offered
#'                     load above capacity is a saturated queue (bounded latency)
#'                     plus drops, not unbounded delay. Capping here keeps the
#'                     bid-time congestion signal consistent with the
#'                     execution-time M/M/1 model (which caps its rho at 0.99) so
#'                     that beyond capacity the market rations by PRICE rather
#'                     than by latency-driven value collapse.
#' @return Estimated latency in ms.
estimate_latency <- function(util_hat, base_latency = 50,
                             alpha = 50, p = 1.2, min_latency = 1,
                             util_cap = 1.0) {
  util_hat <- ifelse(is.na(util_hat), 0, util_hat)
  util_hat <- pmin(pmax(util_hat, 0), util_cap)   # saturating-queue signal
  L_hat    <- base_latency + alpha * (util_hat ^ p)
  pmax(L_hat, min_latency)
}

#' Compute latency-aware value for a single task.
#'
#' V = v_base * exp(-lambda_l * latency). If latency exceeds the task's
#' deadline, only a salvage fraction of the value is retained.
#'
#' @param task_row         Single-row data frame with value_base, deadline.
#' @param latency_ms       Estimated or realised latency in ms.
#' @param lambda_l_default Per-ms decay rate (default: 0.005).
#' @param salvage          Fraction of value kept after deadline miss (default: 0).
#' @return Scalar value.
value_latency_aware <- function(task_row, latency_ms,
                                lambda_l_default = 0.005, salvage = 0.0) {
  vb  <- as.numeric(task_row$value_base)
  dl  <- as.numeric(task_row$deadline)
  lam <- if ("lambda_l" %in% names(task_row)) {
    as.numeric(task_row$lambda_l)
  } else {
    lambda_l_default
  }
  v <- vb * exp(-lam * latency_ms)
  if (!is.na(dl) && latency_ms > dl) v <- salvage * v
  v
}


# ===========================================================================
# Online logistic success model
# ===========================================================================
# p(success | util_hat) = sigmoid(a + b * util_hat)
# Updated each round via SGD on the observed mean success rate.

#' Initialise the success model with default parameters.
#'
#' @param a Intercept (default: 2.0, encodes high base success probability).
#' @param b Slope (default: -2.0, success decreases with utilisation).
#' @return A list with components a and b.
init_success_model <- function(a = 2.0, b = -2.0) {
  list(a = as.numeric(a), b = as.numeric(b))
}

#' Predict success probability given current utilisation.
#'
#' @param model    Success model list (a, b).
#' @param util_hat Current utilisation estimate.
#' @return Scalar probability in [0, 1].
predict_success_prob <- function(model, util_hat) {
  z <- model$a + model$b * as.numeric(util_hat)
  1 / (1 + exp(-z))
}

#' Update success model with a stochastic gradient step.
#'
#' Uses the cross-entropy gradient: g = p_hat - y_observed.
#'
#' @param model        Success model list.
#' @param util_hat     Utilisation at the time of observation.
#' @param success_rate Observed mean success rate this round.
#' @param lr           Learning rate (default: 0.3).
#' @return Updated model list.
update_success_model <- function(model, util_hat, success_rate, lr = 0.3) {
  util_hat <- as.numeric(util_hat)
  y        <- pmax(0, pmin(1, as.numeric(success_rate)))
  p        <- predict_success_prob(model, util_hat)
  g        <- (p - y)
  model$a  <- model$a - lr * g
  model$b  <- model$b - lr * g * util_hat
  model
}


# ===========================================================================
# Multi-tier tatonnement clearing + greedy packing
# ===========================================================================

#' Initialise per-tier prices.
#'
#' @param env Environment list.
#' @param p0  Starting price for all tiers (default: 0.1).
#' @return A tibble with columns: tier, price.
init_tier_prices <- function(env, p0 = 0.1) {
  cap <- tier_capacities(env)
  tibble(tier = cap$tier, price = rep(as.numeric(p0), nrow(cap)))
}

#' Compute expected value for each task (latency-aware, success-discounted).
#'
#' @param tasks_all    Tibble of tasks.
#' @param util_hat     Current utilisation estimate.
#' @param base_latency Base latency for bidding estimate.
#' @param success_model Online logistic success model.
#' @param alpha        Congestion sensitivity.
#' @param p            Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate.
#' @param salvage      Salvage fraction.
#' @return Numeric vector of expected values (same length as nrow(tasks_all)).
task_expected_value <- function(tasks_all, util_hat, base_latency, success_model,
                                alpha = 200, p = 1.2,
                                lambda_l_default = 0.005, salvage = 0.0) {
  if (nrow(tasks_all) == 0) return(numeric(0))

  L_hat <- estimate_latency(util_hat, base_latency = base_latency,
                            alpha = alpha, p = p)

  v_hat <- purrr::map_dbl(seq_len(nrow(tasks_all)), function(i) {
    value_latency_aware(tasks_all[i, , drop = FALSE], latency_ms = L_hat,
                        lambda_l_default = lambda_l_default, salvage = salvage)
  })

  ps <- predict_success_prob(success_model, util_hat)
  ps * v_hat
}

#' Compute per-task surplus (expected value minus resource cost at current prices).
#'
#' @param ev_vec    Numeric vector of expected values.
#' @param bundle    Per-tier demand tibble from task_bundle().
#' @param prices_df Per-tier price tibble.
#' @return Numeric vector of surpluses.
task_surplus <- function(ev_vec, bundle, prices_df) {
  x         <- bundle %>% left_join(prices_df, by = "tier")
  unit_cost <- sum(x$demand * x$price, na.rm = TRUE)
  ev_vec - unit_cost
}

#' Greedily pack tasks by descending surplus subject to per-tier capacities.
#'
#' @param tasks_all   Tibble of tasks.
#' @param surplus_vec Numeric vector of surpluses (from task_surplus).
#' @param env         Environment list.
#' @param max_tasks   Optional upper bound on number of tasks to accept.
#' @return A tibble of accepted tasks.
pack_tasks_greedy <- function(tasks_all, surplus_vec, env, max_tasks = Inf) {
  chosen <- .greedy_pack_by(surplus_vec, tasks_all, env, max_tasks = max_tasks)
  if (length(chosen) == 0) {
    return(tibble(task_id = character(), agent_id = integer(),
                  deadline = numeric(), value_base = numeric()))
  }
  tasks_all[chosen, .alloc_cols(tasks_all)] %>%
    mutate(
      task_id    = as.character(task_id),
      agent_id   = as.integer(agent_id),
      deadline   = as.numeric(deadline),
      value_base = as.numeric(value_base)
    )
}

#' Clear the multi-tier market via tatonnement + greedy packing.
#'
#' Iteratively adjusts per-tier prices based on excess demand, then packs
#' tasks by descending surplus. Returns the allocation, surplus vector,
#' clearing info, and updated market state.
#'
#' @param tasks_all       Tibble of tasks.
#' @param env             Environment list.
#' @param util_hat        Current utilisation estimate.
#' @param base_latency    Base latency for bidding.
#' @param market_state    Market state list (prices, success_model, history).
#' @param alpha           Congestion sensitivity.
#' @param p               Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate.
#' @param salvage         Salvage fraction.
#' @param iters           Number of tatonnement iterations (default: 15).
#' @param eta             Price step size (common price step, price_eta).
#' @param price_floor     Minimum price (default: 0).
#' @param price_cap       Maximum price (default: 1000).
#' @param beta            EMA weight on the entry prices when posting the
#'                        cleared prices; 0, the default, posts them raw.
#' @return A list with: allocation, surplus, expected_value, clearing, market_state.
clear_multitier_market <- function(tasks_all, env, util_hat, base_latency,
                                   market_state,
                                   alpha = 200, p = 1.2,
                                   lambda_l_default = 0.005, salvage = 0.0,
                                   iters = 15L, eta = price_eta,
                                   price_floor = 0.0, price_cap = 1000.0,
                                   beta = 0) {
  if (is.null(market_state$prices)) {
    market_state$prices <- init_tier_prices(env)
  }
  if (is.null(market_state$success_model)) {
    market_state$success_model <- init_success_model()
  }

  prices <- market_state$prices
  cap    <- tier_capacities(env)
  bundle <- task_bundle(env)
  # Per-task recipes, and the two arithmetic shortcuts they retire. The
  # single-bundle branch is kept as DEFENSIVE insurance rather than because
  # folding it drifts: folding was measured bit-identical across a 32-entry
  # probe set covering three graph types, both clearing paths and full
  # exp4_run_single runs. It keeps a round's aggregate demand one multiply
  # rather than a column sum over identical rows, so no published result can
  # move by a rounding order under a weight that is not a dyadic rational.
  het <- !is.null(env$recipes)
  A   <- if (het) task_recipes(tasks_all, env) else NULL
  recipe_surplus <- function(ev_vec, prices_df) {
    if (!het) return(task_surplus(ev_vec, bundle, prices_df))
    p <- prices_df$price[match(bundle$tier, prices_df$tier)]
    ev_vec - rowSums(A * matrix(p, nrow(A), ncol(A), byrow = TRUE))
  }
  # Per-tier reserve / marginal-cost price anchor (env$reserve_price, default 0
  # for legacy envs). The effective floor is max(price_floor, reserve).
  reserve <- env$reserve_price %||% 0

  # Compute expected value for all tasks
  ev <- task_expected_value(
    tasks_all, util_hat, base_latency, market_state$success_model,
    alpha = alpha, p = p,
    lambda_l_default = lambda_l_default, salvage = salvage
  )

  # Tatonnement: iteratively adjust per-tier prices
  for (k in seq_len(iters)) {
    s     <- recipe_surplus(ev, prices)
    pos   <- which(s > 0)

    demand <- tibble(
      tier         = bundle$tier,
      total_demand = if (het) colSums(A[pos, , drop = FALSE])
                     else bundle$demand * length(pos)
    )

    x <- cap %>%
      left_join(demand, by = "tier") %>%
      left_join(prices, by = "tier") %>%
      mutate(
        total_demand = replace_na(total_demand, 0),
        excess       = total_demand - capacity,
        step         = eta * (excess / pmax(capacity, 1))
      )

    prices <- x %>%
      transmute(
        tier  = tier,
        price = pmin(price_cap, pmax(max(price_floor, reserve), price + step))
      )
  }

  # Final allocation: pack tasks by surplus at converged prices
  s_final    <- recipe_surplus(ev, prices)
  allocation <- pack_tasks_greedy(tasks_all, s_final, env)

  # EMA posting rule, mirroring the integrator's slice-price smoothing: the
  # pack above ran at the raw clearing prices, so beta moves the price the
  # agent is charged without moving the allocation. This is the smoothing
  # factor of the Exp.4 factorial; at the default it is the identity, which is
  # what every other caller of this function takes.
  if (beta > 0) {
    entry <- market_state$prices$price[match(prices$tier, market_state$prices$tier)]
    prices$price <- ema_post(entry, prices$price, beta)
  }

  # Clearing summary. unit_cost is deliberately the MIX-AVERAGE basket
  # sum_r bundle_r * p_r even under heterogeneous recipes: it is the
  # agent-facing cost the price-dispersion metric is computed on, and a basket
  # whose composition changed with the arm would make that metric an
  # apples-to-oranges artefact rather than a measurement.
  x         <- bundle %>% left_join(prices, by = "tier")
  unit_cost <- sum(x$demand * x$price, na.rm = TRUE)
  clearing  <- list(
    prices       = prices,
    unit_cost    = unit_cost,
    n_alloc      = nrow(allocation),
    mean_surplus = mean(s_final[s_final > 0], na.rm = TRUE)
  )

  market_state$prices <- prices
  list(
    allocation     = allocation,
    surplus        = s_final,
    expected_value = ev,
    clearing       = clearing,
    market_state   = market_state
  )
}


# ===========================================================================
# Market state management
# ===========================================================================

#' Initialise market state.
#'
#' @param env Optional environment (used to set initial prices).
#' @return A list with: prices, price_history, success_model, success_history.
init_market_state <- function(env = NULL) {
  list(
    prices          = if (!is.null(env)) init_tier_prices(env) else NULL,
    price_history   = list(),
    success_model   = init_success_model(),
    success_history = numeric(0)
  )
}

#' Append current prices to the price history.
#'
#' @param market_state Market state list.
#' @param prices_df    Current prices tibble.
#' @return Updated market state.
append_price_history <- function(market_state, prices_df) {
  market_state$price_history[[length(market_state$price_history) + 1L]] <- prices_df
  market_state
}

#' Agent-facing price volatility: dispersion of the per-task cost agents pay.
#'
#' This is THE price-volatility metric for the study.
#' It is preferred over a per-tier log-return measure, which is floor-pathological:
#' a per-tier price legitimately drops to 0 when its tier is uncongested
#' (competitive price of slack capacity), and `sd(diff(log(.)))` then spikes on the
#' 0<->positive transition. That measure was also asymmetric between arms
#' (naive per-tier prices floor at 0.0 and touch 0; the integrator's internal
#' per-tier prices floor at 0.01 and sit pinned), so the naive-vs-hybrid ratio is
#' an apples-to-oranges artefact rather than a measured stabilisation effect.
#'
#' Instead we measure the standard deviation of `unit_cost` -- the per-task cost
#' the agent actually pays, recorded identically for both arms (naive: bundle
#' cost \eqn{\sum_r demand_r \cdot price_r}; hybrid: the slice price). This is the
#' same agent-facing observable on both arms, floor-insensitive (level, not
#' log-return), and symmetric.
#'
#' @param unit_cost_series Numeric vector of per-round agent unit costs.
#' @return Scalar volatility (sd), or NA if fewer than 3 finite observations.
agent_price_volatility <- function(unit_cost_series, mean_floor = 0.01) {
  x <- unit_cost_series[is.finite(unit_cost_series)]
  if (length(x) < 3) return(NA_real_)
  m <- mean(x)
  # Scale-free coefficient of variation: removes the bundle-sum (naive) vs
  # slice-price (hybrid) scale gap so the two arms are comparable. Undefined
  # where there is no sustained price (uncongested cell, agent pays ~nothing) --
  # the volatility claim is scoped to the contended regime where a price exists.
  if (m < mean_floor) return(NA_real_)
  stats::sd(x) / m
}

#' Agent-facing price volatility after the opening rounds.
#'
#' The same metric on the same series with the first `fraction` of the rounds
#' dropped. This is the one the architecture factorial contrasts its cells on,
#' because the opening transient differs between the arms by their initial
#' price and not by their price dynamics (see `burn_in_fraction`).
#'
#' @param unit_cost_series Numeric vector of per-round agent unit costs.
#' @param fraction         Opening fraction of the run to drop.
#' @return Scalar volatility on the retained rounds, NA on fewer than 3.
agent_price_volatility_tail <- function(unit_cost_series,
                                        fraction = burn_in_fraction) {
  n <- length(unit_cost_series)
  agent_price_volatility(tail(unit_cost_series, n - floor(fraction * n)))
}

#' Update market state from round execution results.
#'
#' Updates the online logistic success model based on observed success rate.
#'
#' @param market_state Market state list.
#' @param util_hat     Current utilisation estimate.
#' @param results_t    Execution results tibble with success column.
#' @param lr           SGD learning rate (default: 0.3).
#' @return Updated market state.
market_update_from_results <- function(market_state, util_hat, results_t,
                                       lr = 0.3) {
  if (is.null(results_t) || nrow(results_t) == 0) return(market_state)
  if (!("success" %in% names(results_t))) return(market_state)

  sr <- mean(results_t$success, na.rm = TRUE)
  market_state$success_model <- update_success_model(
    market_state$success_model, util_hat, sr, lr = lr
  )
  market_state$success_history <- c(market_state$success_history, sr)
  market_state
}


# ===========================================================================
# Welfare computation
# ===========================================================================

#' Compute social welfare for a round.
#'
#' Sums latency-aware values of successful tasks, minus an optional congestion
#' penalty for over-utilised tiers.
#'
#' @param results_t        Execution results with value_base, deadline, latency, success.
#' @param env              Environment list.
#' @param prices_df        Per-tier prices (not used in welfare, but kept for interface).
#' @param lambda_l_default Per-ms latency decay rate.
#' @param salvage          Salvage fraction.
#' @param cong_cost        Whether to apply congestion penalty (default: TRUE).
#' @param cong_gamma       Congestion penalty weight (default: 0.05).
#' @return Scalar welfare value.
compute_welfare <- function(results_t, env, prices_df,
                            lambda_l_default = 0.005, salvage = 0.0,
                            cong_cost = TRUE, cong_gamma = 0.05) {
  if (is.null(results_t) || nrow(results_t) == 0) return(0)
  if (!all(c("value_base", "deadline", "latency", "success") %in% names(results_t))) {
    stop("results_t must contain value_base, deadline, latency, success")
  }

  rv <- purrr::map_dbl(seq_len(nrow(results_t)), function(i) {
    if (!isTRUE(results_t$success[i])) return(0)
    value_latency_aware(results_t[i, , drop = FALSE], results_t$latency[i],
                        lambda_l_default = lambda_l_default, salvage = salvage)
  })
  w <- sum(rv, na.rm = TRUE)

  if (!cong_cost) return(w)

  # Congestion penalty: squared excess utilisation over capacity
  n_exec  <- sum(results_t$success, na.rm = TRUE)
  if (n_exec == 0) return(0)
  util_df <- compute_utilisation_per_tier(env, n_exec)
  pen     <- sum((pmax(util_df$util - 1, 0))^2, na.rm = TRUE)
  w - cong_gamma * pen
}


# ===========================================================================
# Oracle packing (welfare upper bound)
# ===========================================================================

#' Compute the oracle welfare upper bound.
#'
#' Greedily packs tasks by expected value (not surplus) subject to capacity.
#' Gives the maximum achievable welfare under perfect information.
#'
#' @param tasks_all       Tibble of tasks.
#' @param env             Environment list.
#' @param util_hat        Current utilisation estimate.
#' @param base_latency    Base latency.
#' @param success_model   Online logistic success model.
#' @param alpha           Congestion sensitivity.
#' @param p               Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate.
#' @param salvage         Salvage fraction.
#' @return A list with: oracle_value (scalar welfare), n (tasks packed).
#' Greedily pack task indices by a descending rank vector, respecting per-tier
#' capacity. Shared kernel for the value-greedy oracle and the VCG allocator.
#'
#' Per-task demand comes from task_recipes(), so the kernel is a heuristic for a
#' multidimensional knapsack once recipes differ; exact_pack_by_value() is the
#' oracle that measures the gap. Under one bundle per environment it is the
#' welfare maximiser, which is what the DSIC story rests on.
#'
#' @param rank_vec  Numeric ranking (e.g. expected value); ties broken by order().
#' @param tasks_all Tibble of tasks (indices, and a `recipe` column if present).
#' @param env       Environment list (capacities + per-task per-tier demand).
#' @param max_tasks Optional upper bound on the number of tasks accepted.
#' @return Integer vector of chosen row indices into tasks_all (input order).
.greedy_pack_by <- function(rank_vec, tasks_all, env, max_tasks = Inf) {
  if (nrow(tasks_all) == 0) return(integer(0))

  A         <- task_recipes(tasks_all, env)
  cap       <- tier_capacities(env)
  remaining <- cap$capacity[match(colnames(A), cap$tier)]
  stopifnot("every recipe tier needs a capacity" = !anyNA(remaining))
  order_idx <- order(rank_vec, decreasing = TRUE)

  chosen <- integer(0)
  for (i in order_idx) {
    if (!is.finite(rank_vec[i]) || rank_vec[i] <= 0) next
    if (length(chosen) >= max_tasks) break

    need <- A[i, ]
    if (all(need <= remaining)) {
      remaining <- remaining - need
      chosen    <- c(chosen, i)
    }
  }
  chosen
}

#' Exact welfare-determining packer for small instances (brute force).
#'
#' Enumerates every subset of the tasks as a bit matrix, rejects the rows that
#' violate a resource capacity, and returns the best feasible row. Unlike
#' .greedy_pack_by this is an argmax, not a heuristic, so it is the only oracle
#' in this codebase that can measure the greedy gap on heterogeneous recipes --
#' where value-greedy is a multidimensional-knapsack heuristic rather than the
#' welfare maximiser.
#'
#' Tasks with non-positive or non-finite value are dropped before enumeration:
#' recipes are non-negative, so excluding such a task is always feasible and
#' never worse. The empty set is always enumerated, so the value is >= 0.
#'
#' # ponytail: O(2^n) enumeration, exact by construction and with no solver
#' # status to interpret. Above n = 16 the upgrade is an R MILP package (Rglpk,
#' # lpSolve) -- one binary per task, one capacity row per resource -- never a
#' # Python bridge, which would put an interpreter and a serialisation boundary
#' # on the critical path of every targets branch and of the test suite.
#'
#' @param v     Numeric vector of per-task values (length n).
#' @param A     n x R matrix of per-task resource recipes (a vector when R = 1).
#' @param C     Numeric vector of per-resource capacities (length R).
#' @param max_n Enumeration cap; above it the call is an error, not a slowdown.
#' @return A list with: value (scalar optimum), chosen (indices into v).
exact_pack_by_value <- function(v, A, C, max_n = 14L) {
  n <- length(v)
  if (n > max_n) {
    stop(sprintf(
      "exact_pack_by_value: %d tasks is above the enumeration cap of %d", n, max_n))
  }
  if (n == 0L) return(list(value = 0, chosen = integer(0)))

  A    <- matrix(A, nrow = n)
  keep <- which(is.finite(v) & v > 0)
  if (length(keep) == 0L) return(list(value = 0, chosen = integer(0)))

  grid   <- as.matrix(expand.grid(rep(list(c(0, 1)), length(keep))))
  totals <- grid %*% A[keep, , drop = FALSE]
  vals   <- as.vector(grid %*% v[keep])
  vals[rowSums(totals > matrix(C, nrow(totals), length(C), byrow = TRUE)) > 0] <- -Inf

  best <- which.max(vals)
  list(value = vals[best], chosen = keep[grid[best, ] == 1])
}

oracle_pack_realised <- function(tasks_all, env, util_hat, base_latency,
                                 success_model,
                                 alpha = 200, p = 1.2,
                                 lambda_l_default = 0.005, salvage = 0.0) {
  if (nrow(tasks_all) == 0) return(list(oracle_value = 0, n = 0))

  ev <- task_expected_value(
    tasks_all, util_hat, base_latency, success_model,
    alpha = alpha, p = p,
    lambda_l_default = lambda_l_default, salvage = salvage
  )

  chosen <- .greedy_pack_by(ev, tasks_all, env)
  list(oracle_value = sum(ev[chosen]), n = length(chosen))
}

#' Value-greedy welfare-maximising allocation, returning the CHOSEN SET.
#'
#' The set-returning twin of oracle_pack_realised (same packing kernel). Used by
#' vcg_allocate. Within a round, ev is allocation-independent (it depends on the
#' exogenous util_hat, not on which other tasks are allocated), so the leave-one-
#' out welfare computations VCG needs are exact.
#'
#' @param ev        Per-task expected value (from task_expected_value).
#' @param tasks_all Tibble of tasks.
#' @param env       Environment list.
#' @return Tibble: task_id, agent_id, realised_value (= ev of the chosen tasks).
greedy_alloc_set <- function(ev, tasks_all, env) {
  chosen <- .greedy_pack_by(ev, tasks_all, env)
  if (length(chosen) == 0) {
    return(tibble(task_id = character(), agent_id = integer(),
                  realised_value = numeric()))
  }
  tibble(
    task_id        = as.character(tasks_all$task_id[chosen]),
    agent_id       = as.integer(tasks_all$agent_id[chosen]),
    realised_value = as.numeric(ev[chosen])
  )
}

#' Clarke-pivot VCG allocation + payments (DSIC mechanism for Exp.7a).
#'
#' Ported from the same-author P2A sibling credible-marketplace-sim/R/sim_market.R
#' (vcg_allocate); the Clarke-pivot LOGIC is reused on P1's existing value-greedy
#' allocator and value machinery (NOT P2A's data structs). Welfare-maximising
#' greedy allocation, then per-agent externality (Clarke pivot) payments. Within
#' a round ev is exogenous (util_hat-driven), so leave-one-out welfare is exact
#' and every externality is non-negative.
#'
#' @inheritParams oracle_pack_realised
#' @return Tibble: task_id, agent_id, realised_value, vcg_payment.
vcg_allocate <- function(tasks_all, env, util_hat, base_latency, success_model,
                         alpha = 200, p = 1.2,
                         lambda_l_default = 0.005, salvage = 0.0) {
  # Hard guard, not a comment: this mechanism's DSIC property comes entirely
  # from its allocation rule being the exact welfare argmax, and that holds only
  # while every task carries the identical-bundle. With per-task recipes the rule
  # is a multidimensional-knapsack heuristic and the Clarke clamp papers over the
  # externalities it creates, so no incentive claim is available on such an
  # environment and none may be computed here by accident. A future DSIC
  # statement on heterogeneous recipes has to run this over exact_pack_by_value.
  if (!is.null(env$recipes)) {
    stop("vcg_allocate: DSIC holds on an identical-bundle environment only; ",
         "with per-task recipes the allocation rule is a greedy heuristic, ",
         "not the welfare argmax, so no incentive claim is available")
  }

  empty <- tibble(task_id = character(), agent_id = integer(),
                  realised_value = numeric(), vcg_payment = numeric())
  if (nrow(tasks_all) == 0) return(empty)

  ev <- task_expected_value(
    tasks_all, util_hat, base_latency, success_model,
    alpha = alpha, p = p,
    lambda_l_default = lambda_l_default, salvage = salvage
  )

  full <- greedy_alloc_set(ev, tasks_all, env)
  if (nrow(full) == 0) return(empty)

  W_total         <- sum(full$realised_value)
  agent_value_map <- tapply(full$realised_value, full$agent_id, sum)
  full$vcg_payment <- 0

  for (a in unique(full$agent_id)) {
    W_others_with_a    <- W_total - agent_value_map[[as.character(a)]]
    keep               <- tasks_all$agent_id != a
    set_without_a      <- greedy_alloc_set(ev[keep], tasks_all[keep, ], env)
    W_others_without_a <- sum(set_without_a$realised_value)
    externality_a      <- W_others_without_a - W_others_with_a
    # The clamp below is INERT under the exactness precondition guarded at the
    # top of this function: where the allocation rule is the welfare argmax,
    # removing an agent can only free capacity for the others, so no
    # externality is negative and pmax(0, .) never binds. Enforced rather than
    # asserted in prose, so an allocation rule that stops being the argmax
    # surfaces here instead of being absorbed into a zero payment.
    stopifnot(
      "the allocation rule is not the welfare argmax: a Clarke externality is negative" =
        all(externality_a >= -1e-12))

    am    <- full$agent_id == a
    tot_a <- sum(full$realised_value[am])
    if (tot_a > 0) {
      full$vcg_payment[am] <- pmax(0, externality_a * full$realised_value[am] / tot_a)
    }
  }

  full
}

#' The price the operator posts: a markup over one task bundle's marginal cost.
#'
#' Anchored on the environment's own reserve, so the posted-price arm and the
#' market arm start from the same number: this is exactly the slice reserve the
#' integrator floors its clearing price at. k = 1 posts marginal cost and never
#' rations on price; k > 1 rations.
#'
#' @param env Environment list.
#' @param k   Markup over marginal cost.
#' @return Scalar posted price per task bundle.
posted_price_anchor <- function(env, k = 1) {
  k * (env$reserve_price %||% 0) * sum(task_bundle(env)$demand)
}

#' Posted-price allocation: a static price instead of a discovered one.
#'
#' Ported from the same-author P2A sibling credible-marketplace-sim/R/sim_market.R
#' (posted_price_allocate); what the port carries is the participation screen and
#' the flat payment, reused on P1's existing value-greedy packer (NOT P2A's data
#' structs, and not its packing order: the sibling serves its participants in
#' arrival order, which is the weaker baseline of the two). Tasks whose expected
#' value clears the posted price participate, participants are packed by expected
#' value under per-tier capacity, and every winner pays the posted price.
#' No price discovery and no externality: this is deployed cloud on-demand
#' pricing, the contrast point for what tatonnement buys.
#'
#' @param tasks_all Tibble of tasks.
#' @param env       Environment list.
#' @param ev        Per-task expected value (from task_expected_value).
#' @param p_post    The posted price per task bundle.
#' @return The admitted tasks, with a `payment` column.
posted_price_allocate <- function(tasks_all, env, ev, p_post) {
  keep <- is.finite(ev) & ev > p_post
  pack_tasks_greedy(tasks_all[keep, ], ev[keep], env) %>%
    mutate(payment = p_post)
}


# ===========================================================================
# Integrator slice clearing (hybrid architecture)
# ===========================================================================

#' Clear the single-dimensional slice market (hybrid architecture).
#'
#' The integrator abstracts multi-tier resources into a single slice
#' resource. Agents bid for slices (1D) instead of multi-tier bundles (3D),
#' reducing complementarity.  Slice capacity = min over tiers of
#' floor(capacity / effective_demand_per_task).
#'
#' @param tasks_all       Tibble of tasks.
#' @param env             Environment list.
#' @param util_hat        Current utilisation estimate.
#' @param base_latency    Base latency for bidding.
#' @param success_model   Online logistic success model.
#' @param integrator      Integrator state list.
#' @param alpha           Congestion sensitivity.
#' @param p               Congestion exponent.
#' @param lambda_l_default Per-ms latency decay rate.
#' @param salvage         Salvage fraction.
#' @param iters           Tatonnement iterations for slice price.
#' @return A list with: allocation, integrator, unit_cost, slice_capacity.
integrator_clear <- function(tasks_all, env, util_hat, base_latency,
                             success_model, integrator,
                             alpha = 200, p = 1.2,
                             lambda_l_default = 0.005, salvage = 0.0,
                             iters = 10L, slice_inflation = 1.0) {
  empty_alloc <- tibble(task_id = character(), agent_id = integer(),
                        deadline = numeric(), value_base = numeric())

  if (nrow(tasks_all) == 0) {
    return(list(allocation = empty_alloc, integrator = integrator,
                unit_cost = 0, slice_capacity = 0L))
  }

  cap    <- tier_capacities(env)
  bundle <- task_bundle(env)

  # Slice capacity: minimum tasks that fit on any tier after efficiency scaling.
  # slice_inflation models a FAITHFULNESS VIOLATION of Prop. 3 condition (ii)
  # (Exp.10): when the integrator's scalar max-flow OVERSTATES the
  # true multi-dimensional feasibility, it advertises a larger slice capacity
  # than the tiers can serve, over-admitting tasks. slice_inflation = 1 is
  # faithful (the default); > 1 over-advertises and the surplus tasks congest
  # the queue in execute_allocation, degrading drop/welfare.
  per_tier <- cap %>%
    left_join(bundle, by = "tier") %>%
    mutate(
      eff_demand = demand * integrator$efficiency_factor,
      max_tasks  = floor(capacity / pmax(eff_demand, 1e-6))
    )
  slice_capacity <- floor(min(per_tier$max_tasks) * slice_inflation)

  ev <- task_expected_value(
    tasks_all, util_hat, base_latency, success_model,
    alpha = alpha, p = p,
    lambda_l_default = lambda_l_default, salvage = salvage
  )

  # Per-tier tâtonnement: the integrator runs an internal price-discovery
  # loop on the local marketplace from which it sources capacity, in
  # parallel with the agent-facing slice-price tâtonnement.
  #
  # Per-tier prices PERSIST across rounds in integrator state — matching
  # clear_multitier_market's behaviour (which reads ms_res$prices each
  # call and writes them back) and the sibling P2A reference design in
  # credible-marketplace-sim/R/sim_market.R. The slice price also
  # persists (agent-facing EMA smoothing is the integrator's signature
  # feature). The price floor is 0.01 (not 0) to avoid the log-floor
  # pathology that destabilises σ-on-log-returns when prices touch zero
  # (P2A uses the same floor).
  if (is.null(integrator$tier_prices)) {
    integrator$tier_prices <- init_tier_prices(env, p0 = 0.1)
  }
  tier_prices_df <- integrator$tier_prices
  # Per-tier reserve / marginal-cost anchor (env$reserve_price, default 0). The
  # per-tier floor is max(0.01, reserve); the slice price the integrator posts
  # is floored at its INPUT cost -- the reserve cost of one task's resource
  # bundle (reserve * sum of per-tier demand) -- so the two arms anchor at
  # comparable agent-facing cost levels.
  reserve        <- env$reserve_price %||% 0
  slice_reserve  <- reserve * sum(bundle$demand)
  price_floor    <- max(0.01, reserve)

  # Floor the agent-facing slice price at the integrator's reserve input cost.
  # This is a floor on the entry price, not a first-contact posting rule: where
  # integrator_init's 0.5 seed already sits above the reserve input cost it
  # never fires at all, and the posted price of the opening round is a blend
  # with that seed like any other round. What it prevents is the EMA crawling
  # up to the floor from below over ~1/(1-beta) rounds. The naive tier prices
  # do NOT start at the reserve, they start at init_tier_prices' p0, so the two
  # arms open at different distances from their operating level; the metric the
  # factorial contrasts them on drops that opening stretch from both.
  # Idempotent after round 1: the smoothed price never falls
  # back below slice_reserve, so this clamp only ever fires on the first clear.
  if (integrator$slice_price < slice_reserve) {
    integrator$slice_price <- slice_reserve
  }

  # Joint tâtonnement: slice-price (agent-facing) + per-tier prices
  # (integrator-internal). Per-tier loop uses the same η price-step and
  # excess-demand rule as clear_multitier_market for comparability.
  sp <- integrator$slice_price
  for (k in seq_len(iters)) {
    # Slice tâtonnement (unchanged)
    surplus <- ev - sp
    n_pos   <- sum(surplus > 0, na.rm = TRUE)
    excess  <- n_pos - slice_capacity
    sp      <- max(slice_reserve, sp + integrator$eta * (excess / max(slice_capacity, 1)))

    # Per-tier tâtonnement: at the current slice price, the projected
    # number of admitted tasks is n_pos (tasks with positive slice
    # surplus). Each admitted task draws per-tier demand bundle[r]
    # scaled by the integrator's internal efficiency factor. Per-tier
    # excess demand drives the per-tier price.
    tier_step <- bundle %>%
      dplyr::mutate(demand_r = n_pos * demand * integrator$efficiency_factor) %>%
      dplyr::inner_join(cap,            by = "tier") %>%
      dplyr::inner_join(tier_prices_df, by = "tier") %>%
      dplyr::mutate(
        excess_r  = demand_r - capacity,
        new_price = pmax(price_floor, price + integrator$eta *
                                    (excess_r / pmax(capacity, 1)))
      )
    tier_prices_df <- tier_step %>%
      dplyr::transmute(tier = tier, price = new_price)
  }

  # EMA smoothing on the slice price (unchanged behaviour).
  # beta = 0.8 means 80% weight on previous price, 20% on new clearing price.
  integrator$slice_price <- ema_post(integrator$slice_price, sp,
                                    integrator$beta)

  # Persist per-tier prices across rounds (matches naive clear_multitier_market).
  integrator$tier_prices <- tier_prices_df

  # Pack tasks by slice surplus
  surplus_final <- ev - sp
  order_idx     <- order(surplus_final, decreasing = TRUE)
  chosen <- list()
  k      <- 0L

  for (i in order_idx) {
    if (!is.finite(surplus_final[i]) || surplus_final[i] <= 0) next
    if (k >= slice_capacity) break
    k <- k + 1L
    chosen[[k]] <- tasks_all[i, .alloc_cols(tasks_all)]
  }

  allocation <- if (length(chosen) == 0) empty_alloc else bind_rows(chosen)

  list(
    allocation     = allocation,
    integrator     = integrator,
    # The agent pays the integrator's POSTED price, which is the EMA-smoothed
    # slice price (the integrator's stabilisation mechanism) -- not the raw
    # per-round clearing price `sp`. Recording the EMA matches what the agent
    # actually faces.
    unit_cost      = integrator$slice_price,
    slice_capacity = slice_capacity,
    tier_prices    = tier_prices_df    # per-tier prices (diagnostic)
  )
}

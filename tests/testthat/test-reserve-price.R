# Tests for the market price anchor (reserve / marginal-cost price).
# Without an anchor the clearing price is a pure congestion shadow price (= 0 when
# capacity is slack, the usual case) so the agent-facing price is intermittent and
# price-volatility is ill-formed. A per-tier reserve price p0 > 0 (real services
# price > 0 even when idle) anchors prices: clearing price = max(reserve, shadow
# price). env$reserve_price carries it; reserve = 0 recovers the unanchored model.

.rp_env <- function(reserve, gt = "sp", load = "high") {
  e <- init_environment(build_dependency_graph(gt), load_level = load,
                        n_agents = 20L, graph_type = gt)
  e$reserve_price <- reserve
  e
}
.rp_tasks <- function(n = 40L, seed = 1L) {
  set.seed(seed)
  tibble::tibble(task_id = sprintf("t%03d", seq_len(n)),
                 agent_id = sample.int(5, n, replace = TRUE),
                 # Deadlines at the pipeline's set. The old 100/150/200 ms sat below the
                 # bid-time latency ESTIMATE (critical path + alpha * util_hat^p, 222 ms on
                 # sp at util_hat = 0.5), not below the 135 ms critical path itself, so once
                 # the estimate stopped being the constant 50 every expected value here
                 # collapsed to salvage.
                 # (Three of the five tests in this file run at reserve_price = 0.1,
                 # where the fixture clears nothing either way; those are price-floor
                 # checks that never depended on the deadlines.)
                 deadline = sample(c(500L, 750L, 1000L), n, replace = TRUE),
                 value_base = runif(n, 1, 2))
}

test_that("reserve_price = 0 leaves the naive price floor at 0 (backward compat)", {
  env <- .rp_env(0); blb <- base_latency_for_bids(env)
  cl  <- clear_multitier_market(.rp_tasks(), env, 0.5, blb, init_market_state(env))
  expect_true(min(cl$market_state$prices$price) >= 0)        # may be exactly 0
})

test_that("a positive reserve anchors every naive per-tier price at >= reserve", {
  env <- .rp_env(0.1); blb <- base_latency_for_bids(env)
  cl  <- clear_multitier_market(.rp_tasks(), env, 0.5, blb, init_market_state(env))
  expect_true(all(cl$market_state$prices$price >= 0.1 - 1e-9))
})

test_that("the naive agent unit cost is strictly positive under a reserve", {
  env <- .rp_env(0.1); blb <- base_latency_for_bids(env)
  cl  <- clear_multitier_market(.rp_tasks(), env, 0.5, blb, init_market_state(env))
  expect_gt(cl$clearing$unit_cost, 0)
})

test_that("the integrator slice price is anchored positive under a reserve", {
  env <- .rp_env(0.1); blb <- base_latency_for_bids(env)
  integ <- integrator_init(beta = 0.8, efficiency_factor = 0.75, eta = 0.15)
  ic <- integrator_clear(.rp_tasks(), env, 0.5, blb, init_success_model(), integ, iters = 5L)
  expect_gt(ic$unit_cost, 0)
})

test_that("congestion raises the agent-facing cost above the reserve floor (anchor is a floor, not a cap)", {
  # The reserve is a price FLOOR, not a cap: in the functioning multi-round market
  # a contended cell drives the per-task cost above reserve * Sigma_demand. A
  # single-shot clearing with a fixed util_hat does not exercise cross-round price
  # discovery, so we test on the real simulation path (a contended entangled cell).
  # The cell must be contended AND still clearing: where demand is so far above
  # capacity that almost nothing is admitted, the realised per-task cost falls
  # back to the floor and the test would measure rationing, not congestion.
  # N = 60 is contended (utilisation ~0.86) and clearing on every seed tried.
  env_e <- init_environment(build_dependency_graph("entangled"), "medium",
                            n_agents = 60L, graph_type = "entangled")
  reserve_floor <- env_e$reserve_price * sum(task_bundle(env_e)$demand)   # 0.04 * 12
  r <- exp1_run_single("entangled", "medium", seed = 1L, n_agents = 60L, n_rounds = 60L)
  expect_gt(r$mean_unit_cost, reserve_floor + 1e-6)
})

test_that("the integrator primes its slice price at the reserve input cost (no sub-reserve EMA warm-up)", {
  # integrator_init seeds slice_price at an arbitrary 0.5; the slice tatonnement
  # floors the clearing price at the integrator's reserve INPUT cost
  # slice_reserve = reserve * sum(demand). When slice_reserve > 0.5 the EMA would
  # crawl up from 0.5 over ~1/(1-beta) rounds, a transient that would inflate the
  # price-stability metric. The integrator
  # must instead post a price >= its reserve input cost from the first round
  # (mirrors naive tier prices, which start at the reserve).
  env   <- .rp_env(0.3, gt = "entangled", load = "high")
  blb   <- base_latency_for_bids(env)
  reserve_in <- 0.3 * sum(task_bundle(env)$demand)         # slice_reserve
  integ <- integrator_init(beta = 0.8, efficiency_factor = 0.85, eta = 0.15)
  expect_lt(integ$slice_price, reserve_in)                 # precondition: init below floor
  ic <- integrator_clear(.rp_tasks(n = 60L), env, 0.5, blb,
                         init_success_model(), integ, iters = 5L)
  expect_gte(ic$unit_cost, reserve_in - 1e-9)              # posted price >= reserve cost
})

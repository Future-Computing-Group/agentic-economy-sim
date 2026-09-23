# test-node-congestion-arm.R
# ---------------------------------------------------------------------------
# The market arm whose bid-time latency is the one the execution model will
# charge.
#
# The baseline market bids against a capped power law of a path-mean
# utilisation signal, which cannot express a latency above a fraction of what
# the queue at capacity actually costs, so the price it discovers cannot carry
# the congestion the admitted set creates. This arm replaces that estimate by
# the execution model's own queue term, summed along the task's path at the
# previous round's per-node utilisation and uncapped. Everything else -- the
# success model, the tatonnement, the packer, the payment -- is the market's.
# ---------------------------------------------------------------------------

cc_env <- function(a = "tree") node_run_env(a, "high", node_agents()[[a]], "uniform", "off")

cc_alloc <- function(env, n) {
  tibble(task_id = sprintf("t%03d", seq_len(n)), agent_id = rep_len(1:9, n),
         deadline = 1000, value_base = 1.5,
         recipe = rep_len(rownames(env$anc), n))
}


test_that("the bid-time latency is the latency the execution model delivers", {
  # Not a restatement of the formula: the estimate is compared against the
  # deterministic latency execute_allocation charges the same round.
  env   <- cc_env()
  alloc <- cc_alloc(env, 60L)
  u     <- compute_utilisation_per_node(env, alloc)
  realised <- execute_allocation(alloc, env, latency_noise_cv = 0)

  est <- node_queue_latency_per_leaf(env, u)
  expect_equal(unname(est[as.character(alloc$recipe)]), realised$latency)

  # With no previous round there is nothing queued, so the estimate is the
  # zero-queue path.
  expect_equal(node_queue_latency_per_leaf(env, NULL), base_latency_per_leaf(env))
})

test_that("the estimate is uncapped where the execution model clamps", {
  # The execution model holds its utilisation at 0.99 and its queue at 500 ms
  # per node; a bid that inherited those caps would be blind to exactly the
  # congestion this arm exists to price.
  env <- cc_env()
  hot <- tibble(tier = tier_capacities(env)$tier, util = 1)
  est <- node_queue_latency_per_leaf(env, hot)
  expect_gt(min(est), 3 * 500)
  # and it is monotone in the utilisation it reads
  warm <- tibble(tier = tier_capacities(env)$tier, util = 0.5)
  expect_true(all(node_queue_latency_per_leaf(env, warm) < est))
})

test_that("the congestion-consistent arm is the market with one estimate replaced", {
  # Measured at the steep level (exec_clamp 0.99, queue_coef 2), where the
  # lagged signal's two-cycle is pinned; the default level is the calibrated one.
  one <- function(m) node_run_single("tree", "high", N = 90L, seed = 1L,
                                     n_rounds = 10L, mechanism = m,
                                     lambda_l_default = node_lambda_l(),
                                     exec_clamp = 0.99, queue_coef = 2)
  mk <- one("market")
  cc <- one("market_cc")

  expect_equal(cc$mechanism, "market_cc")
  # A price that can express marginal congestion rations more of it.
  expect_lt(cc$tokens_admitted, mk$tokens_admitted)
  # It does not ration it more SMOOTHLY: the signal is a lagged one, so a full
  # round prices the next one out entirely and an empty round prices nothing,
  # and the arm runs a two-cycle whose mean latency sits above the baseline's.
  # Measured, not assumed, and left in place: damping it would be a second
  # mechanism change on top of the one being measured.
  expect_gt(cc$median_latency, mk$median_latency)
  # Neither arm can pass the planner who knew the round.
  expect_lt(cc$welfare_over_optimum, 1)
  # The round's arrivals, the instance and both references are untouched by
  # the arm: only the bid is different.
  expect_equal(cc$ceiling_zero_queue, mk$ceiling_zero_queue)
  expect_equal(cc$optimum_ex_post, mk$optimum_ex_post)
  # It is still a discovered price: the arm posts what it cleared at.
  expect_true(is.finite(cc$mean_unit_cost))
  expect_true(is.finite(cc$mean_price_volatility))
})

test_that("the added arm is a grid level beside the market, never instead of it", {
  g <- node_exp6_mechanism_grid(n_seeds = 10L)
  expect_true("market_cc" %in% g$mechanism)
  expect_equal(sum(g$mechanism == "market_cc"), sum(g$mechanism == "market"))
  expect_setequal(unique(g$p_post_k[g$mechanism == "market_cc"]), 1)
  # The per-tier experiment does not run it.
  expect_false("market_cc" %in% exp6_mechanism_grid(n_seeds = 2L)$mechanism)
})

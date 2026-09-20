# test-node-encapsulation-responses.R
# ---------------------------------------------------------------------------
# The two responses the encapsulation factor was missing.
#
#   resid_excess: what the tatonnement still could not clear at its terminal
#     prices. It is the one response that can say a price vector does not
#     exist on an instance, and it was computed inside the loop and discarded.
#   price_volatility_cleared: the dispersion of the price the market cleared
#     at, before the smoothing filter. The agent-facing series is measured
#     after the filter, so any stability read off it is partly the filter's
#     arithmetic rather than the market's behaviour.
# ---------------------------------------------------------------------------

resp_env <- function(a = "entangled") node_run_env(a, "high", node_agents()[[a]], "uniform", "off")

resp_tasks <- function(env, n, seed = 5L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%04d", seq_len(n)), agent_id = rep_len(1:9, n),
         deadline = 1000, value_base = runif(n, 2.5, 3),
         recipe = rep_len(rownames(env$anc), n))
}

# A round the agents can afford: below their willingness to pay the market
# clears by pricing everyone out, and a residual of zero then says nothing
# about whether a clearing price exists.
resp_clear <- function(env, tasks, ...) {
  clear_multitier_market(tasks, env, util_hat = 0.1,
                         base_latency = base_latency_for_bids(env),
                         market_state = init_market_state(env),
                         lambda_l_default = node_lambda_l(), ...)
}


test_that("the market returns what it could not clear at its terminal prices", {
  env   <- resp_env()
  tasks <- resp_tasks(env, 400L)
  res   <- resp_clear(env, tasks)

  # The demand backing the terminal prices is the tasks whose surplus is
  # positive there, which is the surplus vector the call returns.
  A   <- task_recipes(tasks, env)
  cap <- tier_capacities(env)
  dem <- colSums(A[res$surplus > 0, , drop = FALSE])
  expect_equal(res$clearing$resid_excess,
               sum(pmax(unname(dem[cap$tier]) - cap$capacity, 0)) /
                 sum(cap$capacity))
  expect_gt(res$clearing$resid_excess, 0)

  # A round the capacities carry clears, and leaves nothing behind.
  expect_equal(resp_clear(env, resp_tasks(env, 8L))$clearing$resid_excess, 0)
})

test_that("the cleared price is reported beside the price the agent is charged", {
  env   <- resp_env()
  tasks <- resp_tasks(env, 400L)
  raw   <- resp_clear(env, tasks, beta = 0)
  smooth <- resp_clear(env, tasks, beta = 0.8)

  # The filter moves what the agent pays and not what the market cleared at.
  expect_equal(smooth$clearing$unit_cost_cleared, raw$clearing$unit_cost)
  expect_false(isTRUE(all.equal(smooth$clearing$unit_cost,
                                smooth$clearing$unit_cost_cleared)))
  # With no filter the two are one number.
  expect_identical(raw$clearing$unit_cost_cleared, raw$clearing$unit_cost)
})

test_that("every node row carries both responses", {
  mkt <- node_run_single("entangled", "high", N = 90L, seed = 1L, n_rounds = 12L,
                         lambda_l_default = node_lambda_l())
  expect_true(all(c("resid_excess", "price_volatility_cleared") %in% names(mkt)))
  expect_true(is.finite(mkt$resid_excess))
  expect_true(is.finite(mkt$price_volatility_cleared))
  # Without a filter the agent faces the price the market cleared at.
  expect_equal(mkt$price_volatility_cleared, mkt$mean_price_volatility_tail)

  # A rank scheduler has no price process, so there is nothing to clear.
  k8s <- node_run_single("entangled", "high", N = 90L, seed = 1L, n_rounds = 12L,
                         mechanism = "k8s", lambda_l_default = node_lambda_l())
  expect_true(is.na(k8s$resid_excess))
})

test_that("the smoothed arm's agent-facing dispersion is below the cleared one", {
  # The low-pass identity: the filter divides the dispersion of a series it
  # did not otherwise change, so reading market stability off the agent-facing
  # column alone reports the filter's arithmetic.
  ema <- node_run_single("entangled", "high", N = 90L, seed = 1L, n_rounds = 40L,
                         architecture = "naive_ema",
                         lambda_l_default = node_lambda_l())
  expect_lt(ema$mean_price_volatility_tail, ema$price_volatility_cleared)
  expect_lt(ema$mean_price_volatility_tail / ema$price_volatility_cleared, 0.8)
})

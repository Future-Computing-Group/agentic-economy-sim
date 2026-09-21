# test-node-ascending.R
# ---------------------------------------------------------------------------
# The price process the theory speaks to.
#
# The market arm walks prices in both directions by a step proportional to
# excess demand, with no line search, against a demand that is a step function
# of price: one step can carry the whole marginal cohort below zero surplus,
# and the walk then cycles or stops on a price no node's demand supports.
#
# The ascending process raises prices only on over-demanded nodes, by a small
# fixed increment, never lowers them, and stops when no node is over-demanded.
# On a gross-substitutes economy that terminates at the least price vector
# that clears. Everything else -- the kernel, the packer, the payment -- is the
# market's, and the market's own path must not move.
# ---------------------------------------------------------------------------

asc_env <- function(a = "tree") node_run_env(a, "high", node_agents()[[a]], "uniform", "off")

# One node over-demanded at the floor and every other node slack: the leaf's
# own capacity is cut to two tokens while three tasks ask for it.
asc_tight_env <- function() apply_leaf_caps(asc_env(), c(l2 = 2))

asc_tasks <- function(n, leaf = "l2", seed = 2L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%03d", seq_len(n)), agent_id = seq_len(n),
         deadline = 1000, value_base = runif(n, 2.5, 3),
         recipe = rep_len(leaf, n))
}

asc_clear <- function(env, tasks, ..., p0 = NULL, iters = 200L) {
  ms <- init_market_state(env)
  if (!is.null(p0)) ms$prices <- init_tier_prices(env, p0)
  clear_multitier_market(tasks, env, util_hat = 0.1,
                         base_latency = base_latency_for_bids(env),
                         market_state = ms, lambda_l_default = node_lambda_l(),
                         iters = iters, ...)
}

asc_prices <- function(res) setNames(res$clearing$prices$price,
                                     res$clearing$prices$tier)


test_that("the market's own price path does not move", {
  # The identity guard the option is allowed under: naming the existing rule
  # must be the same call as not naming it.
  env <- asc_env(); tk <- asc_tasks(40L, "l1")
  expect_identical(asc_clear(env, tk, price_rule = "bidirectional"),
                   asc_clear(env, tk))
})

test_that("prices rise only where demand exceeds capacity, and only upward", {
  env  <- asc_tight_env()
  res  <- asc_clear(env, asc_tasks(3L), price_rule = "ascending",
                    p0 = env$reserve_price, iters = 1L)
  p    <- asc_prices(res)
  rise <- 0.1 * env$reserve_price

  expect_equal(unname(p[["l2"]]), env$reserve_price + rise)
  # Every node the round did not over-demand is where it started.
  for (node in setdiff(names(p), "l2")) {
    expect_equal(unname(p[[node]]), env$reserve_price, info = node)
  }
  expect_true(all(p >= env$reserve_price))
})

test_that("the process stops at the first iteration with nothing over-demanded", {
  # A round the capacities carry: there is nothing to raise, so the walk
  # leaves the entry vector exactly as it found it however long the budget.
  env <- asc_env()
  p0  <- env$reserve_price
  short <- asc_clear(env, asc_tasks(3L, "l1"), price_rule = "ascending",
                     p0 = p0, iters = 1L)
  long  <- asc_clear(env, asc_tasks(3L, "l1"), price_rule = "ascending",
                     p0 = p0, iters = 500L)
  expect_equal(unname(asc_prices(short)), rep(p0, length(asc_prices(short))))
  expect_identical(long, short)
})

test_that("the ascent stops at the least increment the round supports", {
  # The defining property: one increment lower, the node was still
  # over-demanded, so the terminal price is the smallest the round admits.
  env   <- asc_tight_env()
  rise  <- 0.1 * env$reserve_price
  tasks <- asc_tasks(3L)
  done  <- asc_clear(env, tasks, price_rule = "ascending",
                     p0 = env$reserve_price, iters = 500L)
  k <- round((asc_prices(done)[["l2"]] - env$reserve_price) / rise)
  expect_gt(k, 0)

  # Stopped rather than run out of budget: a larger budget lands on the same
  # vector, and one increment short of it the node is still over-demanded.
  expect_identical(asc_clear(env, tasks, price_rule = "ascending",
                             p0 = env$reserve_price, iters = 5000L), done)
  short <- asc_clear(env, tasks, price_rule = "ascending",
                     p0 = env$reserve_price, iters = k - 1L)
  expect_gt(short$clearing$resid_excess, 0)
  expect_equal(done$clearing$resid_excess, 0)
})

test_that("a laminar round that clears at the floor passes the equilibrium check", {
  env   <- asc_env()
  tasks <- asc_tasks(6L, "l1")
  res   <- asc_clear(env, tasks, price_rule = "ascending",
                     p0 = env$reserve_price, iters = 500L)
  capv  <- setNames(tier_capacities(env)$capacity, tier_capacities(env)$tier)
  dem   <- node_round_demand(tasks, env, res$surplus)[names(capv)]
  expect_true(node_market_equilibrium(dem, capv, asc_prices(res)[names(capv)],
                                      env$reserve_price))
})


# ---- the arm ---------------------------------------------------------------

test_that("the arm is the market with the theory's price process", {
  one <- function(m) node_run_single("tree", "high", N = 90L, seed = 1L,
                                     n_rounds = 8L, mechanism = m,
                                     lambda_l_default = node_lambda_l())
  asc <- one("market_asc")
  mkt <- one("market")

  expect_equal(asc$mechanism, "market_asc")
  expect_gt(asc$tokens_admitted, 0)
  expect_true(is.finite(asc$mean_unit_cost))
  # A different price process on the same rounds: the references it is scored
  # against are untouched.
  expect_equal(asc$ceiling_zero_queue, mkt$ceiling_zero_queue)
  expect_equal(asc$optimum_ex_post, mkt$optimum_ex_post)
  expect_false(isTRUE(all.equal(asc$mean_unit_cost, mkt$mean_unit_cost)))

  # Each round is its own auction from the floor, so no round is charged a
  # price an earlier round ratcheted up to.
  expect_gte(asc$mean_unit_cost, 0)
})

test_that("the ascending level enters every round at its floor", {
  env <- asc_env()
  st  <- init_market_state(env)
  st$prices$price <- st$prices$price + 5
  moved <- node_price_process("market_asc", st, env)
  expect_equal(moved$rule, "ascending")
  expect_true(all(moved$state$prices$price == env$reserve_price))
  # and the markup travels with the floor
  expect_true(all(node_price_process("market_asc", st, env,
                                     reserve_markup = 2)$state$prices$price ==
                    2 * env$reserve_price))
  # Every other level is left alone.
  kept <- node_price_process("market", st, env)
  expect_equal(kept$rule, "bidirectional")
  expect_identical(kept$state, st)
})


# ---- where the arm runs ----------------------------------------------------

test_that("the ascending arm runs beside the market in every block that prices", {
  mech <- node_exp6_mechanism_grid(n_seeds = 10L)
  expect_equal(sum(mech$mechanism == "market_asc"),
               sum(mech$mechanism == "market"))

  for (g in list(node_convergence_grid(10L), node_determinacy_grid(10L),
                 node_shock_grid(10L))) {
    expect_setequal(g$mechanism, c("market", "market_asc"))
    expect_equal(sum(g$mechanism == "market_asc"),
                 sum(g$mechanism == "market"))
  }
  expect_equal(nrow(node_convergence_grid(10L)), 240L)
  expect_equal(nrow(node_determinacy_grid(10L)), 60L)
  expect_equal(nrow(node_shock_grid(10L)), 240L)
})

test_that("the battery's drivers take the level they run", {
  rows <- node_convergence_run("tree", seed = 1L, n_rounds = 2L, iters = 50L,
                               mechanism = "market_asc")
  expect_equal(unique(rows$mechanism), "market_asc")
  expect_equal(nrow(rows), 2L)

  det <- node_determinacy_run("tree", seed = 1L, n_rounds = 1L, iters = 50L,
                              mechanism = "market_asc")
  expect_equal(unique(det$mechanism), "market_asc")

  sh <- node_shock_run("tree", seed = 1L, n_rounds = 6L, shock_start = 3L,
                       capacity_end = 5L, mechanism = "market_asc")
  expect_equal(unique(sh$mechanism), "market_asc")
})

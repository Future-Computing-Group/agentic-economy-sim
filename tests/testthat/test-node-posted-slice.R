# test-node-posted-slice.R
# ---------------------------------------------------------------------------
# The deployment that mixes the two pricing rules: the integrator posts its
# slice price, the cloud-billing rule, while every other node's price is
# discovered by the tatonnement.
#
# Inside a compliant single-slice cluster there is no allocation problem to
# compare mechanisms on -- every exported unit loads the same internal nodes,
# so the internal split is exact -- and the factor that does exist is how the
# slice is priced to agents. The set of nodes held at a posted price is an
# argument so that a later grid can post at any nodes without a code change;
# it defaults to the node a contraction created.
# ---------------------------------------------------------------------------

slice_env <- function(a = "tree", interface = "inner") {
  node_run_env(a, "high", node_agents()[[a]], "uniform", interface)
}

slice_tasks <- function(env, n, seed = 4L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%04d", seq_len(n)), agent_id = rep_len(1:9, n),
         deadline = 1000, value_base = runif(n, 2.5, 3),
         recipe = rep_len(rownames(env$anc), n))
}

slice_clear <- function(env, tasks, state, ...) {
  clear_multitier_market(tasks, env, util_hat = 0.1,
                         base_latency = base_latency_for_bids(env),
                         market_state = state,
                         lambda_l_default = node_lambda_l(), ...)
}


test_that("the posted node holds its anchor every round while the rest are discovered", {
  env    <- slice_env()
  anchor <- 2 * env$reserve_price
  state  <- init_market_state(env)
  posted <- setNames(anchor, "J")

  # Alternating rounds: on a repeated load the tatonnement sits at one fixed
  # point and every price looks pinned, which would witness nothing.
  trace <- lapply(1:5, function(t) {
    step  <- slice_clear(env, slice_tasks(env, c(40L, 120L)[1 + t %% 2], seed = t),
                         state, posted_prices = posted)
    state <<- step$market_state
    setNames(step$clearing$prices$price, step$clearing$prices$tier)
  })
  at <- function(node) vapply(trace, function(p) unname(p[[node]]), numeric(1))

  expect_equal(at("J"), rep(anchor, 5))
  # ... and the nodes the tatonnement still prices do move.
  expect_gt(stats::sd(at("d")), 0)
  expect_true(any(at("d") != anchor))
})

test_that("posting a node at the price the market holds it at changes nothing", {
  # A round the capacities carry: the tatonnement walks every node down to its
  # reserve floor and leaves it there, so posting that same number for the
  # contracted node is the market's own price under another name.
  env   <- slice_env()
  tasks <- slice_tasks(env, 12L)
  free  <- slice_clear(env, tasks, init_market_state(env))
  cleared_J <- free$clearing$prices$price[free$clearing$prices$tier == "J"]
  expect_equal(cleared_J, env$reserve_price)

  pinned <- slice_clear(env, tasks, init_market_state(env),
                        posted_prices = setNames(cleared_J, "J"))
  expect_identical(pinned, free)
})

test_that("the mixed arm posts the slice and discovers the rest", {
  one <- function(m, k = 1) node_run_single(
    "tree", "high", N = 90L, seed = 1L, n_rounds = 8L, mechanism = m,
    p_post_k = k, architecture = "hybrid_noema",
    lambda_l_default = node_lambda_l())

  mixed  <- one("market_posted_slice", 2)
  market <- one("market")
  expect_equal(mixed$mechanism, "market_posted_slice")
  expect_gt(mixed$tokens_admitted, 0)
  # The quotient the arm clears over is laminar whatever the slice costs, so
  # the certificate and the references are the contracted market's.
  expect_true(mixed$certificate_ok)
  expect_equal(mixed$ceiling_zero_queue, market$ceiling_zero_queue)

  # Posting the slice is not the market with one price replaced at the
  # margin: it moves demand off the leaves the slice serves, so the shared
  # node the tatonnement still prices carries less pressure and clears lower.
  # Measured on this cell, the arm admits more and charges less than the
  # market it is otherwise identical to.
  expect_gt(mixed$tokens_admitted, market$tokens_admitted)
  expect_lt(mixed$mean_unit_cost, market$mean_unit_cost)
  # A different posted level is a different arm.
  expect_false(isTRUE(all.equal(mixed$tokens_admitted,
                                one("market_posted_slice", 1)$tokens_admitted)))
})

test_that("the posted set defaults to the node the contraction created", {
  expect_equal(node_posted_slice_nodes(slice_env("tree", "inner"),
                                       slice_env("tree", "off")), "J")
  # On an uncontracted region there is no slice to post, which is why the arm
  # runs on the contracted architectures alone.
  expect_length(node_posted_slice_nodes(slice_env("tree", "off"),
                                        slice_env("tree", "off")), 0L)
})

test_that("the mixed arm is a grid level on the contracted architecture only", {
  g <- node_exp6_mechanism_grid(n_seeds = 10L)
  rows <- g[g$mechanism == "market_posted_slice", ]
  expect_setequal(rows$architecture, "hybrid")
  expect_setequal(rows$p_post_k, c(1, 2))
  # Two markups, three instances, two loads, two congestion levels.
  expect_equal(nrow(rows), 2L * 3L * 2L * 2L * 10L)
  expect_false("market_posted_slice" %in% exp6_mechanism_grid(n_seeds = 2L)$mechanism)
})

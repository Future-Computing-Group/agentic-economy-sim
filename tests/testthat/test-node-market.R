# test-node-market.R
# ---------------------------------------------------------------------------
# The node-level driver: one per-round loop for every experiment, over a market
# whose resources are the service nodes of a dependency graph.
#
# The market kernel is untouched. What the driver adds is the per-token latency
# model, the exogenous bid-time signal, the exact reference off the market, and
# the leaf-share draw that matches the demand stream across supply arms.
# ---------------------------------------------------------------------------

test_that("the environment prices one node and one recipe per leaf", {
  env <- node_env("entangled", "high", 90L)
  expect_equal(env$capacities$tier,
               c("d", "e1", "e2", "e3", "l1", "l2", "l3", "l4"))
  # Admission packs against `capacities` and execution queues against the copy
  # in `per_tier`: the two carry the same vector on the same labels.
  expect_equal(env$per_tier$capacity[match(env$capacities$tier, env$per_tier$tier)],
               env$capacities$capacity)
  expect_equal(names(env$recipes), c("l1", "l2", "l3", "l4"))
  expect_equal(base_latency_for_bids(env), 70)
  expect_equal(unname(base_latency_per_leaf(env)), rep(70, 4))
})

test_that("the bottleneck load and K_c come from the environment", {
  # Matched by construction across tree and entangled and half on sp, which is
  # the conservatism of the leaf-block region and cannot also be matched.
  kc <- vapply(c("tree", "sp", "entangled"),
               function(a) node_k_c(node_env(a, "high", 90L)), numeric(1))
  expect_equal(kc, c(tree = 100, sp = 50, entangled = 100))

  # The populations put all three at one offered load.
  N <- c(tree = 90L, sp = 45L, entangled = 90L)
  rho <- vapply(names(N), function(a) {
    env <- node_env(a, "high", N[[a]])
    rho_bottleneck(a, N[[a]], "high", env = env)
  }, numeric(1))
  expect_equal(unname(rho), rep(1.35, 3))

  # The per-tier path is untouched where no environment is handed in.
  expect_equal(rho_bottleneck("tree", 90L, "high"),
               rho_bottleneck("tree", 90L, "high",
                              env = init_environment(
                                build_dependency_graph("tree"), "high", 90L, "tree")))
})

test_that("the leaf mix is matched across supply arms under one seed", {
  # The label stream is drawn after a per-round reseed, so two instances at one
  # population and one seed see the identical demand in every round: the arms
  # differ in supply and in nothing else.
  draws <- lapply(c("tree", "entangled"), function(a) {
    env <- node_env(a, "high", 90L)
    node_round_tasks(env, init_agents(90L), t = 5L, seed = 3L,
                     deadlines = c(500L, 750L, 1000L))
  })
  expect_equal(draws[[1]]$recipe, draws[[2]]$recipe)
  expect_equal(draws[[1]]$value_base, draws[[2]]$value_base)
  expect_true(all(draws[[1]]$recipe %in% c("l1", "l2", "l3", "l4")))

  # A different mix draws a different stream, and the skewed one leans on l1.
  skew <- node_round_tasks(node_env("tree", "high", 90L, leaf_mix = "skewed"),
                           init_agents(90L), t = 5L, seed = 3L,
                           deadlines = c(500L, 750L, 1000L))
  expect_gt(mean(skew$recipe == "l1"), mean(draws[[1]]$recipe == "l1"))
})

test_that("the agent-facing unit cost is the token weight times the mean leaf price", {
  # The identity that lets the price-volatility metric stay the function it is:
  # the mix-average basket clear_multitier_market records is w times the
  # demand-weighted mean of the INDUCED leaf prices.
  env   <- node_env("entangled", "high", 90L)
  tasks <- node_round_tasks(env, init_agents(90L), t = 1L, seed = 1L,
                            deadlines = c(500L, 750L, 1000L))
  res <- clear_multitier_market(tasks, env, util_hat = 0.5,
                                base_latency = base_latency_for_bids(env),
                                market_state = init_market_state(env))

  p_leaf <- induced_leaf_prices(res$clearing$prices, env$anc)
  expect_equal(res$clearing$unit_cost,
               env$spec$weight * sum(env$leaf_shares[names(p_leaf)] * p_leaf))
})

test_that("the within-round cross-leaf price dispersion is identically zero on sp", {
  # Every leaf of sp has the identical ancestor set, so every induced leaf
  # price is identical. Recorded in advance rather than discovered as a result.
  env   <- node_env("sp", "high", 45L)
  tasks <- node_round_tasks(env, init_agents(45L), t = 1L, seed = 1L,
                            deadlines = c(500L, 750L, 1000L))
  res <- clear_multitier_market(tasks, env, util_hat = 0.5,
                                base_latency = base_latency_for_bids(env),
                                market_state = init_market_state(env))
  p_leaf <- induced_leaf_prices(res$clearing$prices, env$anc)
  expect_equal(as.numeric(stats::sd(p_leaf)), 0)
  expect_equal(cross_leaf_price_cv(res$clearing$prices, env$anc), 0)
})


# ---- the driver ------------------------------------------------------------

test_that("the driver returns the per-tier columns plus the structural ones", {
  r <- node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 6L)
  expect_equal(nrow(r), 1L)
  expect_true(all(c("graph_type", "load_level", "N", "seed", "median_latency",
                    "p95_latency", "utilisation", "drop_rate",
                    "clearing_fraction", "served_among_admitted", "welfare",
                    "oracle_welfare", "efficiency", "mean_unit_cost",
                    "mean_price_volatility", "mean_price_volatility_tail",
                    "greedy_exact_ratio", "greedy_exact_incidence",
                    "greedy_exact_worst", "flow_bound_ratio",
                    "flow_bound_token_gap", "shape_refusal_fraction",
                    "price_cv_cross", "tokens_admitted", "binding_fraction",
                    "leaf_mix") %in% names(r)))
  expect_equal(r$graph_type, "tree")
  expect_equal(r$leaf_mix, "uniform")
  expect_gt(r$clearing_fraction, 0)
  expect_gt(r$tokens_admitted, 0)
})

test_that("greedy equals the exact reference on the laminar arms", {
  # A departure here is a bug and not a finding: on a laminar leaf-block family
  # under unit leaf-token demand value-greedy IS the maximiser.
  for (a in c("tree", "sp")) {
    r <- node_run_single(a, "high", N = if (a == "sp") 45L else 90L,
                         seed = 2L, n_rounds = 10L)
    expect_equal(r$greedy_exact_ratio, 1, tolerance = 1e-9, info = a)
    expect_equal(r$greedy_exact_incidence, 0, info = a)
    expect_equal(r$greedy_exact_worst, 1, tolerance = 1e-9, info = a)
  }
})

test_that("the crossing arm loses welfare to the packing rule in some rounds", {
  r <- node_run_single("entangled", "high", N = 90L, seed = 2L, n_rounds = 40L)
  expect_gt(r$greedy_exact_incidence, 0)
  expect_lt(r$greedy_exact_worst, 1)
  expect_lte(r$greedy_exact_ratio, 1)
  # The instrument is computed off the market, so the market's own rationing
  # cannot bury it: the ratio is near one while the clearing fraction is not.
  expect_gt(r$greedy_exact_ratio, 0.9)
  expect_lt(r$clearing_fraction, 0.9)
})

test_that("the bid-time value of a round is a function of the round before it", {
  # The exogeneity the separable-concave reduction needs: within a round the
  # congestion signal the agents bid against does not move with the round's own
  # admitted set, so a mutated allocation leaves the bid-time values alone.
  env   <- node_env("entangled", "high", 90L)
  agents <- init_agents(90L)
  prev  <- node_round_tasks(env, agents, t = 1L, seed = 1L,
                            deadlines = c(500L, 750L, 1000L))

  u_full <- compute_utilisation_per_node(env, prev)
  u_half <- compute_utilisation_per_node(env, prev[seq_len(nrow(prev) %/% 2), ])
  bid <- function(u) {
    tasks <- node_round_tasks(env, agents, t = 2L, seed = 1L,
                              deadlines = c(500L, 750L, 1000L))
    node_bid_inputs(env, tasks, u)$util_hat
  }
  expect_false(isTRUE(all.equal(bid(u_full), bid(u_half))))
  # ... and the signal is the leaf-wise mean of THAT vector, nothing else.
  tasks <- node_round_tasks(env, agents, t = 2L, seed = 1L,
                            deadlines = c(500L, 750L, 1000L))
  expect_equal(node_bid_inputs(env, tasks, u_full)$util_hat,
               unname(leaf_util_hat(u_full, env$anc)[tasks$recipe]))
})


# ---- the matched-control diagnostic table ----------------------------------

test_that("the diagnostic table computes every control it reports", {
  d <- node_instance_diagnostics()
  expect_setequal(d$graph_type, c("tree", "sp", "entangled"))
  expect_true(all(d$leaf_mix %in% c("uniform", "skewed")))

  u <- d[d$leaf_mix == "uniform", ]
  row <- function(a, col) u[[col]][u$graph_type == a]
  expect_equal(vapply(c("tree", "sp", "entangled"), row, numeric(1), "n_arcs"),
               c(tree = 7, sp = 15, entangled = 8))
  expect_true(all(u$n_nodes == 8) && all(u$n_leaves == 4))
  expect_true(all(u$critical_path_ms == 70))
  expect_true(all(u$max_flow == 100))
  expect_equal(vapply(c("tree", "sp", "entangled"), row, numeric(1), "k_c"),
               c(tree = 100, sp = 50, entangled = 100))
  expect_equal(vapply(c("tree", "sp", "entangled"), row, numeric(1), "N"),
               c(tree = 90, sp = 45, entangled = 90))
  # The two controls that cannot hold, reported rather than engineered away: a
  # leaf of sp has five ancestors, so its path reserve cost is higher.
  expect_equal(vapply(c("tree", "sp", "entangled"), row, numeric(1),
                      "leaf_reserve_cost"),
               c(tree = 0.24, sp = 0.40, entangled = 0.24))
  expect_equal(u$laminar[u$graph_type == "entangled"], FALSE)
  expect_equal(u$submodular[u$graph_type == "entangled"], FALSE)
  expect_true(all(u$submodular[u$graph_type != "entangled"]))
  expect_equal(u$binding_nodes[u$graph_type == "tree"], "d, e1")
  expect_equal(u$binding_nodes[u$graph_type == "sp"], "e1, e2, e3")
})

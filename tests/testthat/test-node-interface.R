# test-node-interface.R
# ---------------------------------------------------------------------------
# The integrator's interface as a real contraction of a sub-DAG: a quotient
# graph and one advertised scalar, built as a modified instance spec so that no
# kernel code moves.
#
# The market clears against the ADVERTISED region and delivery is evaluated
# against the TRUE instance. That split is the whole measurement: an interface
# that advertises a scalar its internal routing cannot honour admits a mix the
# nodes queue on, and the cost lands on the deliverable share rather than on
# the admitted one.
# ---------------------------------------------------------------------------

iface_env <- function(a, interface = "off", mix = "skewed") {
  node_run_env(a, "high", node_agents()[[a]], mix, interface)
}


test_that("a contraction is a quotient graph with one advertised node", {
  spec <- node_instance("entangled")
  q    <- contract_cluster(spec, c("e1", "e2"), "maxflow")

  expect_equal(q$nodes$node, c("d", "e3", "l1", "l2", "l3", "l4", "J"))
  expect_setequal(paste(q$edges$from, q$edges$to),
                  c("d J", "d e3", "J l1", "J l2", "J l3", "e3 l4"))
  # The quotient's own leaf-block family is laminar on every arm: that is the
  # construction invariant, not a reported result.
  expect_true(leaf_blocks_laminar(ancestor_matrix(q)))
  expect_equal(leaf_set(q), c("l1", "l2", "l3", "l4"))
  # The contracted node inherits the cluster's physical tier, so the quotient's
  # zero-queue critical path is the instance's and the arms are comparable.
  expect_equal(critical_path_ms(build_leaf_graph(q), leaf_base_ms(q)), 70)
})

test_that("the advertised scalar is the cluster's own node-split max flow", {
  # 100 tokens on the two-member clusters and 150 on the three-member one,
  # asserted against flow_rank of the cluster's own sub-network rather than
  # against a typed number.
  for (a in c("tree", "entangled")) {
    q <- contract_cluster(node_instance(a), c("e1", "e2"), "maxflow")
    expect_equal(token_capacity(q)[["J"]], 100, info = a)
  }
  q_sp <- contract_cluster(node_instance("sp"), c("e1", "e2", "e3"), "maxflow")
  expect_equal(token_capacity(q_sp)[["J"]], 150)

  # The inner-exposure scalar is the largest whose advertised region is inside
  # the true one: a leaf reachable through one cluster member alone bounds it.
  q_in <- contract_cluster(node_instance("entangled"), c("e1", "e2"), "inner")
  expect_equal(token_capacity(q_in)[["J"]], 50)
  expect_lt(token_capacity(q_in)[["J"]], token_capacity(q_sp)[["J"]])
})

test_that("the maxflow interface over-commits at the designed witness and the inner one does not", {
  spec  <- node_instance("entangled")
  anc   <- ancestor_matrix(spec)
  C     <- token_capacity(spec)
  q_mf  <- contract_cluster(spec, c("e1", "e2"), "maxflow")
  q_in  <- contract_cluster(spec, c("e1", "e2"), "inner")

  advertised <- function(q, x) {
    a <- ancestor_matrix(q)
    all(as.numeric(x[rownames(a)] %*% a) <= token_capacity(q)[colnames(a)])
  }
  true_load <- function(x) as.numeric(x[rownames(anc)] %*% anc)

  witness <- c(l1 = 75, l2 = 0, l3 = 25, l4 = 0)
  expect_true(advertised(q_mf, witness))            # the interface admits it
  expect_false(all(true_load(witness) <= C[colnames(anc)]))   # the nodes cannot
  # l1 hangs off one cluster member, so 75 tokens need 75 through a node of 50.
  expect_equal(unname(true_load(witness)[colnames(anc) == "e1"] - C[["e1"]]), 25)
  # The deliverable maximum along that ray, and the factor it falls short by.
  t_max <- 50 / 75
  expect_equal(sum(witness) * t_max, 200 / 3)
  expect_equal(sum(witness) / (sum(witness) * t_max), 1.5)

  # The inner interface refuses the same point, which is what buys exactness.
  expect_false(advertised(q_in, witness))
  # ... and its scalar is maximal: one more token at the exposed leaf fails.
  expect_true(advertised(q_in, c(l1 = 50, l2 = 0, l3 = 0, l4 = 0)))
  expect_false(advertised(q_in, c(l1 = 51, l2 = 0, l3 = 0, l4 = 0)))
})

test_that("contracting the parallel arm recovers a factor two of throughput", {
  # The leaf-block region of sp carries 50 tokens because every leaf sits under
  # every internal node; the contraction advertises the aggregate and the
  # device's own capacity is what binds. This is the conservatism of the
  # leaf-block reading, measured.
  spec <- node_instance("sp")
  anc  <- ancestor_matrix(spec)
  L    <- rownames(anc)
  full <- subset_name(L, L)
  expect_equal(leaf_rank(anc, token_capacity(spec))[[full]], 50)

  q <- contract_cluster(spec, c("e1", "e2", "e3"), "maxflow")
  qa <- ancestor_matrix(q)
  expect_equal(leaf_rank(qa, token_capacity(q))[[full]], 100)
  expect_equal(min(150, token_capacity(spec)[["d"]]), 100)

  # And the mix the advertised region admits is ROUTABLE on the true network,
  # so on this arm the contraction is exact rather than optimistic. The
  # leaf-block reading refuses it, which is the conservatism being recovered:
  # it charges every internal node for every token because every one of them
  # could carry it.
  x  <- c(l1 = 75, l2 = 0, l3 = 25, l4 = 0)
  fc <- flow_region_constraints(flow_rank(spec, anc))
  expect_true(all(fc$A %*% x[colnames(fc$A)] <= fc$C))
  expect_false(all(as.numeric(x[L] %*% anc) <=
                     token_capacity(spec)[colnames(anc)]))
})


# ---- through the driver ----------------------------------------------------

test_that("the interface levels build the environment the arm names", {
  off <- iface_env("entangled", "off")
  expect_false("J" %in% off$capacities$tier)

  inner <- iface_env("entangled", "inner")
  expect_true("J" %in% inner$capacities$tier)
  expect_equal(inner$capacities$capacity[inner$capacities$tier == "J"], 100)

  mf <- iface_env("entangled", "maxflow")
  expect_equal(mf$capacities$capacity[mf$capacities$tier == "J"], 200)
  # Both quotients keep the instance's zero-queue path, so the interface
  # factor is not a latency treatment in disguise.
  expect_equal(base_latency_for_bids(inner), 70)
  expect_equal(base_latency_for_bids(mf), 70)
})

test_that("the maxflow arm over-commits the true instance and the inner arm does not", {
  run <- function(iface) node_run_single(
    "entangled", "high", N = 90L, seed = 1L, n_rounds = 15L,
    leaf_mix = "skewed", interface = iface)

  off   <- run("off")
  inner <- run("inner")
  mf    <- run("maxflow")

  expect_equal(off$overcommitment, 0)
  expect_equal(inner$overcommitment, 0)
  expect_gt(mf$overcommitment, 0)
  # The cost lands where over-admission shows: on the deliverable share of what
  # was admitted, not on the admitted share of what was offered.
  expect_lt(mf$served_among_admitted, inner$served_among_admitted)
  # The inner arm buys exactness at a throughput cost. The uncontracted arm's
  # own gap is pinned at the uniform mix, where the crossing leaf carries a
  # quarter of the demand rather than a twentieth.
  expect_equal(inner$greedy_exact_incidence, 0)
  expect_lt(inner$tokens_admitted, off$tokens_admitted)
})

test_that("contracting the parallel arm lifts what its market can admit", {
  off <- node_run_single("sp", "high", N = 45L, seed = 1L, n_rounds = 15L,
                         leaf_mix = "skewed", interface = "off")
  mf  <- node_run_single("sp", "high", N = 45L, seed = 1L, n_rounds = 15L,
                         leaf_mix = "skewed", interface = "maxflow")
  expect_gt(mf$tokens_admitted, off$tokens_admitted)
  expect_equal(off$flow_bound_ratio, 0.5)
  expect_equal(mf$flow_bound_ratio, 1)
  # Exact on both, so the recovery is throughput and not a packing artefact.
  expect_equal(off$greedy_exact_incidence, 0)
  expect_equal(mf$greedy_exact_incidence, 0)
})


# ---- the architecture factorial --------------------------------------------

test_that("the four architecture levels cross the contraction with the smoothing", {
  expect_equal(node_architecture("naive"),
               list(interface = "off", beta = 0))
  expect_equal(node_architecture("naive_ema"),
               list(interface = "off", beta = 0.8))
  expect_equal(node_architecture("hybrid_noema"),
               list(interface = "inner", beta = 0))
  expect_equal(node_architecture("hybrid_ema"),
               list(interface = "inner", beta = 0.8))
})

test_that("smoothing moves the posted price without moving the round's allocation", {
  # The two factors are separable within a round by construction: the pack runs
  # at the raw clearing prices and the average is applied to what the agent is
  # charged. Across rounds the smoothed price is the next round's entry price,
  # so the arms do diverge, which is the dynamics the factorial contrasts.
  env   <- node_run_env("entangled", "high", 90L, "uniform", "off")
  tasks <- node_round_tasks(env, init_agents(90L), 1L, 1L, c(500L, 750L, 1000L))
  ms    <- init_market_state(env)
  b     <- node_bid_inputs(env, tasks, NULL)

  raw <- clear_multitier_market(tasks, env, b$util_hat, b$base_latency, ms,
                                beta = 0)
  sm  <- clear_multitier_market(tasks, env, b$util_hat, b$base_latency, ms,
                                beta = 0.8)
  expect_identical(raw$allocation, sm$allocation)
  expect_false(isTRUE(all.equal(raw$clearing$unit_cost, sm$clearing$unit_cost)))
})

test_that("all four architecture cells run and price", {
  out <- lapply(c("naive", "naive_ema", "hybrid_noema", "hybrid_ema"),
                function(a) node_run_single("entangled", "high", N = 90L,
                                            seed = 1L, n_rounds = 15L,
                                            architecture = a))
  out <- dplyr::bind_rows(out)
  expect_equal(out$interface, c("off", "off", "inner", "inner"))
  expect_true(all(is.finite(out$mean_price_volatility_tail)))
  expect_true(all(is.finite(out$welfare)))
  # The contraction is a real supply restriction on this arm, so the two
  # encapsulated cells admit strictly less than the two direct ones.
  expect_true(all(out$tokens_admitted[3:4] < out$tokens_admitted[1:2]))
})

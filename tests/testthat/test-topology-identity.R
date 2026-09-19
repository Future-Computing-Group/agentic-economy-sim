# The matched-topology control on the PER-TIER substrate, discharged as an
# identity rather than as a run.
#
# This file is the artefact that records why a node-level substrate exists, and
# it now records both halves of it: on the per-tier substrate two drawings
# matched on what the simulator reads are the same run, and on the node
# substrate they are not. The last test below is that second half.
#
# On the per-tier substrate the simulator reads exactly three things from a
# topology object: the per-tier
# demand profile it induces, the tier sequence along its critical path, and a
# node count that is dead whenever demand weights are present. Node identity and
# edge structure have no further effect. Two topologies matched on the first two
# therefore produce IDENTICAL trajectories under a common seed -- by
# construction, not empirically -- so running them as an experiment and
# reporting the agreement would be reporting a code identity as a finding.
#
# The two negative controls below are what keep this from being vacuous: unmatch
# either channel and the trajectories separate.

# ---- two specs, matched on what the simulator reads --------------------------

tree_weights <- function() {
  tibble::tibble(tier = c("device", "edge", "cloud"), demand_weight = c(2, 2, 2))
}

# Spec 1: the canonical tree. Critical path in1 -> pre1 -> edge_inf ->
# cloud_inf -> post, i.e. the tier sequence device, edge, edge, cloud, cloud.
spec_tree <- function(weights = tree_weights()) {
  list(
    nodes = tibble::tibble(
      node = c("in1", "in2", "pre1", "edge_inf", "cloud_inf", "post"),
      tier = c("device", "device", "edge", "edge", "cloud", "cloud")
    ),
    edges = tibble::tribble(
      ~from,       ~to,
      "in1",       "pre1",
      "in2",       "pre1",
      "pre1",      "edge_inf",
      "edge_inf",  "cloud_inf",
      "cloud_inf", "post"
    ),
    demand_weights = weights
  )
}

# Spec 2: a DIFFERENT drawing -- seven nodes rather than six, different names,
# a different edge set, an extra edge-tier leaf hanging off the source -- with
# the same critical-path tier sequence and the same demand weights.
spec_chain <- function(weights = tree_weights()) {
  list(
    nodes = tibble::tibble(
      node = c("a1", "a2", "b1", "b2", "b3", "c1", "c2"),
      tier = c("device", "device", "edge", "edge", "edge", "cloud", "cloud")
    ),
    edges = tibble::tribble(
      ~from, ~to,
      "a1",  "b1",
      "a2",  "b1",
      "a1",  "b3",
      "b1",  "b2",
      "b2",  "c1",
      "c1",  "c2"
    ),
    demand_weights = weights
  )
}

# Spec 3: matched demand weights, but the critical path visits one edge node
# instead of two, so its tier sequence differs.
spec_short <- function(weights = tree_weights()) {
  list(
    nodes = tibble::tibble(
      node = c("a1", "b1", "c1", "c2"),
      tier = c("device", "edge", "cloud", "cloud")
    ),
    edges = tibble::tribble(~from, ~to, "a1", "b1", "b1", "c1", "c1", "c2"),
    demand_weights = weights
  )
}

# A fixed loop over the three things a run is made of, assembled here rather
# than taken from an experiment runner, so the test pins the engine and not one
# experiment's bookkeeping.
run_fixed <- function(graph, seed = 4L, n_rounds = 10L, N = 12L,
                      mutate_nodes = FALSE, drop_weights = FALSE) {
  env <- init_environment(graph, "medium", n_agents = N, graph_type = "tree")
  if (drop_weights) env$demand_weights <- NULL
  if (mutate_nodes) {
    env$nodes_per_tier <- dplyr::mutate(env$nodes_per_tier, nodes = nodes * 10L + 7L)
    env$per_tier       <- dplyr::mutate(env$per_tier,       nodes = nodes * 10L + 7L)
  }
  set.seed(seed)
  agents    <- init_agents(N)
  ms        <- init_market_state(env)
  blb       <- base_latency_for_bids(env)
  prev_util <- NULL
  out       <- list()

  for (t in seq_len(n_rounds)) {
    tasks <- bind_tasks(lapply(split(agents, agents$agent_id),
                               function(a) generate_tasks(a, env, round = t)))
    util_hat <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)
    cleared  <- clear_multitier_market(tasks, env, util_hat, blb, ms)
    ms       <- cleared$market_state
    res      <- execute_allocation(cleared$allocation, env)
    out[[t]] <- tibble::tibble(
      round   = t,
      n_alloc = nrow(cleared$allocation),
      price   = paste(format(cleared$clearing$prices$price, digits = 17), collapse = "|"),
      latency = if (nrow(res) == 0) NA_real_ else mean(res$latency),
      success = if (nrow(res) == 0) NA_real_ else mean(res$success)
    )
    prev_util <- compute_utilisation_per_tier(env, nrow(tasks))
  }
  dplyr::bind_rows(out)
}

# ---- the identity -----------------------------------------------------------

test_that("the two specs really are different drawings", {
  a <- spec_tree(); b <- spec_chain()
  expect_false(identical(a$nodes, b$nodes))
  expect_false(identical(a$edges, b$edges))
  expect_false(nrow(a$nodes) == nrow(b$nodes))
  # ... matched on exactly the two channels the simulator reads.
  lat <- c(device = 5, edge = 15, cloud = 50)
  expect_equal(critical_path_ms(a, lat), critical_path_ms(b, lat))
  expect_equal(a$demand_weights, b$demand_weights)
})

test_that("matched topology specs produce identical runs under one seed", {
  # NOT asserted on the environment objects: per_tier$nodes genuinely differs
  # between the two specs, and that difference being invisible in the OUTPUT is
  # the whole point.
  expect_identical(run_fixed(spec_tree()), run_fixed(spec_chain()))
})

test_that("nodes_per_tier is inert whenever demand_weights are present", {
  # Pins the coalesce fallback as dead in every configured experiment: it is
  # reachable only from an environment that carries no demand weights, and none
  # of them do.
  expect_identical(run_fixed(spec_tree()),
                   run_fixed(spec_tree(), mutate_nodes = TRUE))
})

test_that("nodes_per_tier IS read once the demand weights are gone", {
  # The conditional half of the claim above. Drop the weights and the fallback
  # becomes live, so the same mutation now moves the run: "inert" is a statement
  # about every configured environment, not about unreachable code.
  expect_false(isTRUE(all.equal(
    run_fixed(spec_tree(), drop_weights = TRUE),
    run_fixed(spec_tree(), drop_weights = TRUE, mutate_nodes = TRUE))))
})

# ---- negative controls: unmatch one channel at a time -----------------------

test_that("unmatching the demand profile separates the runs", {
  heavier <- tibble::tibble(tier = c("device", "edge", "cloud"),
                            demand_weight = c(2, 3, 2))
  expect_false(isTRUE(all.equal(run_fixed(spec_tree()),
                                run_fixed(spec_chain(heavier)))))
})

test_that("unmatching the critical-path tier sequence separates the runs", {
  expect_false(isTRUE(all.equal(run_fixed(spec_tree()),
                                run_fixed(spec_short()))))
})


# ---- and what a node-indexed substrate reads instead ------------------------

test_that("matched drawings separate once the resources are the service nodes", {
  # The before-and-after of the whole rebuild. The tree and the crossing
  # instance share a node set, a capacity vector, a critical-path tier
  # sequence, the population at which the first node saturates, and an arrival
  # stream; they differ in one arc. On the per-tier substrate a difference of
  # that kind reaches nothing and the runs above are identical by
  # construction. Here the arc changes which leaves a node is an ancestor of,
  # so it reaches the region the market clears over, and the runs separate.
  a <- node_env("tree", "high", 90L)
  b <- node_env("entangled", "high", 90L)
  expect_equal(a$capacities, b$capacities)
  expect_equal(base_latency_for_bids(a), base_latency_for_bids(b))
  expect_equal(unname(base_latency_per_leaf(a)), unname(base_latency_per_leaf(b)))
  expect_equal(node_k_c(a), node_k_c(b))
  # The demand profile is where the arc lands, and it is a PER-NODE profile:
  # there is no three-number summary of it for two drawings to agree on.
  expect_false(isTRUE(all.equal(a$demand_weights, b$demand_weights)))

  tree <- node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 10L)
  ent  <- node_run_single("entangled", "high", N = 90L, seed = 1L, n_rounds = 10L)
  expect_false(isTRUE(all.equal(tree$welfare, ent$welfare)))
  # And the channel it separates on is the structural one, not the demand one:
  # the exact reference is reached on one arm and not on the other.
  expect_equal(tree$greedy_exact_incidence, 0)
  expect_gt(ent$greedy_exact_incidence, 0)
})

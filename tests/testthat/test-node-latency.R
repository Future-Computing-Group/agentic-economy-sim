# test-node-latency.R
# ---------------------------------------------------------------------------
# The per-token latency model: one critical path per LEAF instead of one per
# round, and a bid-time congestion signal that differs by leaf.
#
# A round-level scalar latency makes every leaf of an instance equally fast, so
# the dependency graph reaches the deadline test only through the one number
# its longest path produces. Every site below keys the quantity on the leaf a
# task is destined for, and every site keeps its scalar behaviour byte
# identical where no leaf set is there to key on.
# ---------------------------------------------------------------------------

lat_specs <- leaf_instance_specs("scale")

lat_env <- function(nm = "T") {
  spec <- lat_specs[[nm]]
  env  <- init_environment(build_leaf_graph(spec), "high", 8L, "leaf",
                           capacities   = leaf_capacities(spec),
                           base_latency = leaf_base_latency(spec))
  env$recipes <- leaf_recipes(spec, "unit")
  env$spec    <- spec
  env
}


# ---- the shared traversal --------------------------------------------------

test_that("critical_path_ms is bit-identical after the traversal refactor", {
  # The same four topologies and the same zero-queue values test-critical-path.R
  # pins, recomputed here so the refactor cannot move them quietly.
  for (g in c("linear", "tree", "sp", "entangled")) {
    graph <- build_dependency_graph(g)
    expect_equal(critical_path_ms(graph, c(device = 5, edge = 15, cloud = 50)),
                 max(.longest_path_dist(
                   graph, c(device = 5, edge = 15, cloud = 50))))
  }
  # The four zero-queue scalars the shipped topologies have always returned.
  expect_equal(
    vapply(c("linear", "tree", "sp", "entangled"),
           function(g) critical_path_ms(build_dependency_graph(g),
                                        c(device = 5, edge = 15, cloud = 50)),
           numeric(1)),
    c(linear = 125, tree = 135, sp = 135, entangled = 140))
})

test_that("critical_path_to_leaves returns the zero-queue path on every leaf", {
  for (nm in c("T", "X", "S")) {
    spec <- lat_specs[[nm]]
    d <- critical_path_to_leaves(build_leaf_graph(spec), leaf_base_ms(spec),
                                 leaf_set(spec))
    expect_equal(names(d), c("l1", "l2", "l3", "l4"))
    expect_equal(unname(d), rep(70, 4))
  }
})

test_that("a per-node queue term makes leaf latencies differ by leaf", {
  spec <- lat_specs$X
  lat  <- leaf_base_ms(spec)
  lat[["e1"]] <- lat[["e1"]] + 400                  # one congested edge node
  d <- critical_path_to_leaves(build_leaf_graph(spec), lat, leaf_set(spec))

  # l1 and l2 hang off e1; l4 hangs off e3 and is untouched.
  expect_gt(d[["l1"]], d[["l4"]])
  expect_gt(d[["l2"]], d[["l4"]])
  expect_equal(d[["l3"]], d[["l4"]])
  # l2 has two parents on X, so its path is the MAXIMUM over them, not a sum
  # and not the parent the arc list happens to name first.
  dist <- .longest_path_dist(build_leaf_graph(spec), lat)
  expect_equal(d[["l2"]], max(dist[c("e1", "e2")]) + lat[["l2"]])
})

test_that("base_latency_per_leaf is the zero-queue path of each leaf", {
  env <- lat_env("X")
  expect_equal(base_latency_per_leaf(env),
               critical_path_to_leaves(env$graph,
                                       setNames(env$per_tier$base_ms,
                                                env$per_tier$tier),
                                       leaf_set(env$spec)))
  expect_equal(unname(base_latency_per_leaf(env)), rep(70, 4))
  # The scalar sibling every per-tier driver calls is untouched.
  expect_equal(base_latency_for_bids(env), 70)
})


# ---- the bid-time congestion signal ---------------------------------------

test_that("leaf_util_hat is a mean over the leaf's own ancestors", {
  anc <- ancestor_matrix(lat_specs$X)
  prev <- tibble(tier = colnames(anc),
                 util = c(d = 0.4, e1 = 0.9, e2 = 0.2, e3 = 0.1,
                          l1 = 0.5, l2 = 0.3, l3 = 0.6, l4 = 0.2))
  u <- leaf_util_hat(prev, anc)

  # l1's ancestors on X are d, e1, l1.
  expect_equal(u[["l1"]], mean(c(0.4, 0.9, 0.5)))
  # l2's are d, e1, e2, l2: the crossing arc puts a fourth node in the mean.
  expect_equal(u[["l2"]], mean(c(0.4, 0.9, 0.2, 0.3)))
  expect_true(all(u >= 0 & u <= 1))
  # A mean, not a sum: estimate_latency saturates its signal at 1, so a path
  # sum would sit on the cap in every contended round.
  expect_lt(max(u), sum(prev$util))
  expect_equal(unname(leaf_util_hat(NULL, anc)), rep(0, 4))
})

test_that("compute_utilisation_per_node reports one row per service node", {
  env   <- lat_env("X")
  tasks <- tibble(task_id = sprintf("t%02d", 1:8), agent_id = 1L,
                  deadline = 1000, value_base = 1,
                  recipe = rep(c("l1", "l3"), each = 4))
  u <- compute_utilisation_per_node(env, tasks)

  expect_equal(u$tier, env$capacities$tier)
  # Four l1 tokens at weight 2 draw 8 units of e1's 100, and nothing of e3.
  expect_equal(u$util[u$tier == "e1"], 8 / 100)
  expect_equal(u$util[u$tier == "e3"], 0)
  expect_equal(u$util[u$tier == "d"], 16 / 200)
  expect_equal(compute_utilisation_per_node(env, tasks[0, ])$util, rep(0, 8))
})


# ---- the per-task expected value at a per-task signal ----------------------

test_that("task_expected_value takes a per-task congestion signal and latency", {
  env   <- lat_env("X")
  tasks <- tibble(task_id = c("a", "b"), agent_id = 1:2, deadline = 1000,
                  value_base = c(1.5, 1.5))
  sm    <- init_success_model()

  slow <- task_expected_value(tasks, util_hat = c(0.9, 0.1),
                              base_latency = c(200, 50), sm)
  expect_length(slow, 2)
  # Each row is exactly what the scalar call on that row alone returns.
  expect_equal(slow[1], task_expected_value(tasks[1, ], 0.9, 200, sm))
  expect_equal(slow[2], task_expected_value(tasks[2, ], 0.1, 50, sm))
  expect_lt(slow[1], slow[2])
  # And a scalar signal still returns exactly what it always returned.
  expect_equal(task_expected_value(tasks, 0.5, 70, sm),
               c(task_expected_value(tasks[1, ], 0.5, 70, sm),
                 task_expected_value(tasks[2, ], 0.5, 70, sm)))
})


# ---- execution -------------------------------------------------------------

test_that("execute_allocation is byte-identical on a per-tier environment", {
  # Captured from the deployed function before the per-leaf path existed, on
  # both branches a caller can take: the plain one and the one the integrator
  # takes with an efficiency factor and an encapsulation overhead.
  before <- readRDS(test_path("fixtures", "execute-allocation-per-tier.rds"))
  env <- init_environment(build_dependency_graph("sp"), "high", 8L, "sp")
  alloc <- tibble(task_id = sprintf("t%02d", 1:12), agent_id = rep(1:4, 3),
                  deadline = rep(c(500, 750, 1000), 4),
                  value_base = seq(1, 2, length.out = 12))

  set.seed(11L)
  expect_identical(execute_allocation(alloc, env), before$plain)
  set.seed(11L)
  expect_identical(execute_allocation(alloc, env, efficiency_factor = 0.8,
                                      enc_overhead_ms = 25),
                   before$hybrid)
})

test_that("a task's deadline is tested against its own leaf's latency", {
  # One slow leaf and three fast ones, at the base delays rather than through
  # the queue, so the contrast is the graph's and not a draw's: every task at
  # the slow leaf misses a deadline every task at a fast leaf meets.
  spec <- lat_specs$T
  slow <- leaf_base_latency(spec)
  slow$base_ms[slow$tier == "e1"] <- 900
  env  <- init_environment(build_leaf_graph(spec), "low", 8L, "leaf",
                           capacities   = leaf_capacities(spec),
                           base_latency = slow)
  env$recipes <- leaf_recipes(spec, "unit")
  env$spec    <- spec

  alloc <- tibble(task_id = sprintf("t%02d", 1:8), agent_id = 1L,
                  deadline = 500, value_base = 1,
                  recipe = rep(c("l1", "l4"), each = 4))
  set.seed(3L)
  res <- execute_allocation(alloc, env)

  by_leaf <- split(res$success, alloc$recipe)
  expect_true(all(!by_leaf$l1))          # under the congested edge node
  expect_true(all(by_leaf$l4))           # under an untouched one
  expect_gt(min(res$latency[alloc$recipe == "l1"]),
            max(res$latency[alloc$recipe == "l4"]))
})

# test-node-indexed-env.R
# ---------------------------------------------------------------------------
# The environment constructor carries its per-resource capacities and base
# latencies as arguments, so an environment can be indexed by service node
# instead of by physical tier. Two things have to hold: the default path is
# untouched, and the market kernel takes the node-indexed environment with no
# change of its own.
# ---------------------------------------------------------------------------

test_that("init_environment's default path is unchanged on every shipped graph", {
  # The comparison is against the constructor's own output captured BEFORE the
  # two arguments existed, so this fails if the defaults drift by so much as a
  # column order. Every experiment calls this function.
  before <- readRDS(test_path("fixtures", "init-environment-default-path.rds"))
  for (g in names(before)) {
    expect_identical(init_environment(build_dependency_graph(g), "medium", 8L, g),
                     before[[g]])
  }
})

test_that("the constructor takes node-indexed capacities and per_tier agrees with them", {
  spec <- leaf_instance_specs("small")$X
  env  <- init_environment(build_leaf_graph(spec), "medium", 8L, "leaf",
                           capacities   = leaf_capacities(spec),
                           base_latency = leaf_base_latency(spec))

  expect_equal(env$capacities$tier, c("d", "e1", "e2", "l1", "l2", "l3"))
  expect_equal(env$capacities$capacity, c(9, 4, 4, 5, 5, 5))
  # Admission packs against `capacities` and execution queues against the copy
  # in `per_tier`: the two must be the same vector on the same labels.
  expect_equal(env$per_tier$capacity[match(env$capacities$tier, env$per_tier$tier)],
               env$capacities$capacity)
  expect_equal(env$per_tier$base_ms[match(c("d", "e1", "l1"), env$per_tier$tier)],
               c(5, 15, 50))
  expect_equal(base_latency_for_bids(env), 70)
})

test_that("the recipe tatonnement clears one price per node on a node-indexed environment", {
  spec <- leaf_instance_specs("small")$X
  env  <- init_environment(build_leaf_graph(spec), "medium", 8L, "leaf",
                           capacities   = leaf_capacities(spec),
                           base_latency = leaf_base_latency(spec))
  env$recipes <- leaf_recipes(spec, "unit")

  set.seed(1L)
  tasks <- tibble(
    task_id    = paste0("t", 1:12),
    agent_id   = rep(1:4, each = 3),
    deadline   = 1000L,
    value_base = runif(12, 1, 2),
    recipe     = rep(c("l1", "l2", "l3"), 4)
  )
  res <- clear_multitier_market(tasks, env, util_hat = 0,
                                base_latency = base_latency_for_bids(env),
                                market_state = init_market_state(env))

  expect_equal(res$clearing$prices$tier, c("d", "e1", "e2", "l1", "l2", "l3"))
  expect_true(all(is.finite(res$clearing$prices$price)))
  expect_true(is.finite(res$clearing$unit_cost))
  expect_lte(nrow(res$allocation), nrow(tasks))
  # The admitted set is feasible on the node capacities it was packed against.
  used <- colSums(task_recipes(res$allocation, env))
  expect_true(all(used <= env$capacities$capacity[match(names(used), env$capacities$tier)]))
})

test_that("value-greedy falls below the exact packer on the crossing instance", {
  # Four tokens at the shared leaf valued 10, four at each single-parent leaf
  # valued 9. Greedy takes the four 10s, which fills both internal nodes, and
  # stops at 40; the optimum takes the eight 9s at 72. On a laminar instance no
  # such witness exists, which is what the certificate says.
  spec <- leaf_instance_specs("small")$X
  env  <- init_environment(build_leaf_graph(spec), "medium", 8L, "leaf",
                           capacities   = leaf_capacities(spec),
                           base_latency = leaf_base_latency(spec))
  env$recipes <- leaf_recipes(spec, "unit")

  tasks <- tibble(task_id  = paste0("t", 1:12),
                  agent_id = 1L,
                  deadline = 1000L,
                  value_base = 1,
                  recipe   = rep(c("l2", "l1", "l3"), each = 4))
  ev <- c(rep(10, 4), rep(9, 4), rep(9, 4))

  A <- task_recipes(tasks, env)
  C <- env$capacities$capacity[match(colnames(A), env$capacities$tier)]
  greedy <- sum(ev[.greedy_pack_by(ev, tasks, env)])
  exact  <- exact_pack_by_value(ev, A, C)

  expect_equal(greedy, 40)
  expect_equal(exact$value, 72)
  expect_setequal(tasks$recipe[exact$chosen], c("l1", "l3"))
})

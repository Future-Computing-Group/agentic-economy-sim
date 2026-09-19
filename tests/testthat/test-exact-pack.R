# Tests for exact_pack_by_value -- exhaustive welfare determination on the small
# instances the recipe experiment runs on.
#
# This is the first packer in this codebase whose optimum is not itself produced
# by the greedy kernel, so it is the only oracle that can measure the greedy
# gap. Every assertion below is either a hand-computed optimum, a capacity
# property, or a reduction check against .greedy_pack_by; none compares greedy
# against greedy.

# ---- hand-computed optima ---------------------------------------------------

test_that("exact packer reproduces a hand-computed optimum", {
  # I1: 4 tasks, 2 resources, C = (3, 3).
  #   t1 = (2,1) v 5 | t2 = (1,2) v 4 | t3 = (1,1) v 3 | t4 = (3,3) v 7
  #   feasible pairs: {1,2} = (3,3) v 9 | {1,3} = (3,2) v 8 | {2,3} = (2,3) v 7
  #   {1,2,3} = (4,4) infeasible; {4} = (3,3) v 7. Optimum: {1,2}, value 9.
  A1 <- rbind(c(2, 1), c(1, 2), c(1, 1), c(3, 3))
  r1 <- exact_pack_by_value(c(5, 4, 3, 7), A1, c(3, 3))
  expect_equal(r1$value, 9)
  expect_equal(sort(r1$chosen), c(1L, 2L))

  # I2: 3 tasks, 1 resource, C = 10.
  #   t1 = 10 units v 6 | t2 = 5 units v 4 | t3 = 5 units v 4
  #   {1} v 6 | {2,3} = 10 units v 8. Optimum: {2,3}, value 8.
  A2 <- rbind(10, 5, 5)
  r2 <- exact_pack_by_value(c(6, 4, 4), A2, 10)
  expect_equal(r2$value, 8)
  expect_equal(sort(r2$chosen), c(2L, 3L))

  # I3: 5 identical-recipe tasks (1,1), C = (4,4), values 5,4,3,2,1.
  #   At most 4 fit; the best 4 are the top 4 values. Optimum: {1,2,3,4}, 14.
  A3 <- matrix(1, nrow = 5, ncol = 2)
  r3 <- exact_pack_by_value(c(5, 4, 3, 2, 1), A3, c(4, 4))
  expect_equal(r3$value, 14)
  expect_equal(sort(r3$chosen), 1:4)
})

# ---- the number greedy cannot produce ---------------------------------------

test_that("exact packer strictly beats greedy on a designed instance", {
  # Descending value order is not optimal: the highest-value task exhausts the
  # single resource and blocks two cheaper tasks that jointly dominate it.
  v <- c(6, 4, 4)
  A <- rbind(10, 5, 5)
  C <- 10

  exact  <- exact_pack_by_value(v, A, C)
  # Greedy by descending value under the same capacity, spelled out so the
  # comparison cannot degenerate into greedy against greedy.
  remaining <- C
  greedy_v  <- 0
  for (i in order(v, decreasing = TRUE)) {
    if (all(A[i, ] <= remaining)) { remaining <- remaining - A[i, ]; greedy_v <- greedy_v + v[i] }
  }

  expect_equal(greedy_v, 6)
  expect_gt(exact$value, greedy_v)
})

# ---- capacity property ------------------------------------------------------

test_that("exact packer never violates a capacity", {
  set.seed(7L)
  for (k in seq_len(200L)) {
    n <- sample(2:12, 1L)
    R <- sample(1:3, 1L)
    A <- matrix(runif(n * R, 0.1, 3), nrow = n, ncol = R)
    C <- runif(R, 1, 6)
    v <- runif(n, -0.5, 2)
    res <- exact_pack_by_value(v, A, C)
    used <- colSums(A[res$chosen, , drop = FALSE])
    expect_true(all(used <= C + 1e-12))
    expect_gte(res$value, 0)
  }
})

# ---- reduction: identical recipes ------------------------------------------

test_that("exact equals greedy under identical recipes", {
  # Certifies, rather than assumes, the identical-bundle claim the DSIC story
  # rests on: with one bundle for every task, value-greedy IS the argmax.
  env <- init_environment(build_dependency_graph("sp"), "high",
                          n_agents = 8L, graph_type = "sp")
  env$capacities <- dplyr::mutate(env$capacities, capacity = 30)
  set.seed(3L)
  tasks <- tibble::tibble(
    task_id    = sprintf("t%02d", seq_len(10L)),
    agent_id   = sample.int(8L, 10L, replace = TRUE),
    deadline   = sample(c(500L, 750L, 1000L), 10L, replace = TRUE),
    value_base = runif(10L, 1, 2)
  )
  ev <- task_expected_value(tasks, 0.5, base_latency_for_bids(env),
                            init_success_model())

  bundle <- task_bundle(env)
  A <- matrix(bundle$demand, nrow = nrow(tasks), ncol = nrow(bundle), byrow = TRUE)
  C <- tier_capacities(env)$capacity[match(bundle$tier, tier_capacities(env)$tier)]

  greedy_value <- sum(ev[.greedy_pack_by(ev, tasks, env)])
  expect_equal(exact_pack_by_value(ev, A, C)$value, greedy_value)
})

# ---- the cap is a contract --------------------------------------------------

test_that("exact packer refuses instances above the enumeration cap", {
  big <- 16L
  A <- matrix(1, nrow = big, ncol = 1L)
  expect_error(exact_pack_by_value(rep(1, big), A, 3), "enumeration cap")
  # The boundary itself, not just a value well past it: a max_n of 14 -> 15
  # loosening has to fail something.
  expect_error(exact_pack_by_value(rep(1, 15L), matrix(1, 15L, 1L), 3),
               "enumeration cap")
  # ... and accepts the instance size the experiment is capped at.
  expect_no_error(exact_pack_by_value(rep(1, 14L), matrix(1, 14L, 1L), 3))
})

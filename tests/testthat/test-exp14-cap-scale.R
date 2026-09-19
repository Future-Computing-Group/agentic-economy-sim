# Tests for the cap_scale knob used by Exp.14 (parameter sensitivity).
# cap_scale multiplies per-tier capacities; smaller => more saturation
# (higher drop), the dominant driver of the price-volatility result. Exp.14
# probes the sensitivity of that result to the parameter choices.

test_that("cap_scale=1 is byte-identical to the default (no behaviour change)", {
  a <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 5L,
                       n_rounds = 20L)
  b <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 5L,
                       n_rounds = 20L, cap_scale = 1.0)
  expect_equal(digest::digest(a), digest::digest(b))
})

test_that("smaller cap_scale raises drop rate (more saturation)", {
  hi_cap <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 5L,
                            n_rounds = 20L, cap_scale = 1.5)$drop_rate
  lo_cap <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 5L,
                            n_rounds = 20L, cap_scale = 0.5)$drop_rate
  expect_true(lo_cap >= hi_cap - 1e-9)
})


# The environment carries capacity twice: `capacities`, which admission prices
# and packs against, and the copy joined into `per_tier`, which execution
# queues against (rho = demand / capacity, execute_allocation). A knob that
# scaled one and not the other would bind at admission only, and the sweep's
# capacity row would report half of the effect it names.

exec_rho <- function(env, n_tasks) {
  per_tier <- dplyr::left_join(env$per_tier, env$demand_weights, by = "tier")
  stats::setNames(pmin(0.99, per_tier$demand_weight * n_tasks / per_tier$capacity),
                  per_tier$tier)
}

test_that("cap_scale scales every capacity the environment carries", {
  env  <- init_environment(build_dependency_graph("sp"), "high",
                           n_agents = 20L, graph_type = "sp")
  half <- scale_capacities(env, 0.5)
  expect_equal(half$capacities$capacity, env$capacities$capacity / 2)
  expect_equal(half$per_tier$capacity, env$per_tier$capacity / 2)
  expect_equal(scale_capacities(env, 1.0), env)
})

test_that("halving capacity doubles the utilisation execution queues on", {
  env <- init_environment(build_dependency_graph("sp"), "high",
                          n_agents = 20L, graph_type = "sp")
  half <- scale_capacities(env, 0.5)
  expect_equal(exec_rho(half, 10L), 2 * exec_rho(env, 10L))

  # And the same allocation therefore lands later. Under one seed the noise
  # draw is the same on both sides, so this compares the queueing term alone.
  alloc <- tibble::tibble(
    task_id    = paste0("t", 1:10),
    agent_id   = 1:10,
    deadline   = 1000,
    value_base = 1.5
  )
  set.seed(11L); full_cap <- execute_allocation(alloc, env)
  set.seed(11L); half_cap <- execute_allocation(alloc, half)
  expect_true(all(half_cap$latency > full_cap$latency))
})

test_that("cap_scale reaches execution through exp4_run_single", {
  # The end the sweep actually runs: at half capacity the run must queue and
  # miss deadlines more than at full capacity, not merely admit fewer tasks.
  full_cap <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 7L,
                              n_rounds = 20L, cap_scale = 1.0)
  half_cap <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 7L,
                              n_rounds = 20L, cap_scale = 0.5)
  expect_gt(half_cap$median_latency, full_cap$median_latency)
  expect_lt(half_cap$served_among_admitted, full_cap$served_among_admitted)
})

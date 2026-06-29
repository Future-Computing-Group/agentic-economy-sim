# Tests for Exp.12 — integrator encapsulation overhead.
# The integrator's cross-layer abstraction adds latency (delta_enc) to the
# hybrid path; this overhead must be charged on the hybrid execution path.
# Minimal structure: one additive-latency term on the hybrid execution path,
# swept to find the break-even where hybrid stops beating naive.

test_that("execute_allocation adds enc_overhead_ms to critical-path latency", {
  env   <- init_environment(build_dependency_graph("sp"), "high",
                            n_agents = 10L, graph_type = "sp")
  alloc <- tibble::tibble(task_id = sprintf("t%d", 1:20),
                          agent_id = rep(1:5, 4),
                          deadline = 200, value_base = 1.5)
  set.seed(1); r0  <- execute_allocation(alloc, env, enc_overhead_ms = 0)
  set.seed(1); r30 <- execute_allocation(alloc, env, enc_overhead_ms = 30)
  # Same RNG → the +30 ms shows up as a +30 ms shift in mean latency.
  expect_equal(mean(r30$latency) - mean(r0$latency), 30, tolerance = 5)
})

test_that("hybrid latency rises monotonically with encapsulation overhead", {
  lat <- function(d) {
    exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 2L,
                    n_rounds = 20L, enc_overhead_ms = d)$median_latency
  }
  l0  <- lat(0); l20 <- lat(20); l50 <- lat(50)
  expect_true(l0 <= l20 + 1e-6)
  expect_true(l20 <= l50 + 1e-6)
  # A real, non-trivial overhead effect by 50 ms.
  expect_gt(l50, l0)
})

test_that("naive is unaffected by integrator overhead (no integrator)", {
  n0  <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 2L,
                         n_rounds = 15L, enc_overhead_ms = 0)$median_latency
  n50 <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 2L,
                         n_rounds = 15L, enc_overhead_ms = 50)$median_latency
  expect_equal(n0, n50, tolerance = 1e-9)
})
